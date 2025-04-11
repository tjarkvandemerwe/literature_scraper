# --- Literature Scraper with RSelenium, Caching, and HTML Download ---

# 1. Load Required Packages (Install if needed: install.packages(c("RSelenium", "rvest", "httr", "stringr", "dplyr", "urltools", "digest", "wdman", "netstat")))
library(RSelenium)
library(rvest)
library(httr)
library(stringr)
library(dplyr)
library(urltools)
library(digest)
library(wdman) # Often used by RSelenium implicitly, good to have explicitly
library(netstat) # For finding free ports

# --- Helper Function to Detect Interstitial Pages ---
is_google_interstitial_page <- function(html_page) {
  if (is.null(html_page)) return(FALSE)
  # Use tryCatch for robustness in case html_page is not valid HTML
  page_text <- tryCatch(html_page %>% html_text(trim = TRUE), error = function(e) "")
  noscript_node <- tryCatch(html_page %>% html_node("noscript"), error = function(e) NULL)
  noscript_text <- if (!is.null(noscript_node)) tryCatch(html_text(noscript_node), error = function(e) "") else ""
  
  # Combine texts safely
  combined_text <- paste(page_text, noscript_text)
  
  interstitial_patterns <- c(
    "Enable JavaScript", "not redirected within a few seconds",
    "having trouble accessing Google Search", "unusual traffic from your computer network",
    "/sorry/image", "needs JavaScript enabled", "Privacy Reminder", "Before you continue"
  )
  found_pattern <- any(sapply(interstitial_patterns, function(pattern) {
    grepl(pattern, combined_text, ignore.case = TRUE)
  }))
  return(found_pattern)
}


# --- Main Function ---
scrape_literature_selenium <- function(search_term,
                                       max_results = 20,
                                       download_dir = "literature_downloads_selenium",
                                       download_pdfs = TRUE,
                                       download_html = TRUE,
                                       refresh_search = FALSE,
                                       browser_name = "chrome", # or "firefox", "edge"
                                       # Specify path to your chromedriver if NOT handled by wdman/rsDriver
                                       # chromedriver_path = NULL, # Usually not needed
                                       wait_time_short = 5,  # Seconds to wait after navigation/clicks
                                       wait_time_long = 10, # Longer wait for results rendering
                                       politeness_delay = 3   # Delay for PDF/HTML downloads via httr
) {
  
  cat("--- Starting Literature Scraper (RSelenium Version) ---\n")
  cat("Search Term:", search_term, "\n")
  cat("Refresh Google Search:", refresh_search, "\n")
  cat("Download PDFs:", download_pdfs, "\n")
  cat("Download HTML Content:", download_html, "\n")
  cat("Browser:", browser_name, "\n")
  cat("--- IMPORTANT --- \n")
  cat("Ensure you have:\n")
  cat(" 1. Java installed and in your system PATH.\n")
  cat(" 2. The specified browser (", browser_name, ") installed.\n")
  cat(" 3. The correct WebDriver for your *exact* browser version.\n")
  cat("    - RSelenium/wdman attempts to manage this, but mismatches are common!\n")
  cat("    - Check your Chrome version: chrome://version\n")
  cat("    - If issues persist, try running: wdman::selenium(driver = 'chromedriver', check = TRUE)\n")
  cat("-----------------\n")
  
  # --- Setup Directories ---
  safe_search_term_dir <- gsub("[^a-zA-Z0-9_-]", "_", search_term)
  full_search_path <- file.path(download_dir, safe_search_term_dir)
  pdf_dir <- file.path(full_search_path, "pdfs")
  html_content_dir <- file.path(full_search_path, "html_content")
  search_cache_dir <- file.path(full_search_path, "search_cache")
  
  # Use showWarnings = FALSE as we check existence later
  dir.create(full_search_path, recursive = TRUE, showWarnings = FALSE)
  if (download_pdfs) dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)
  if (download_html) dir.create(html_content_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(search_cache_dir, recursive = TRUE, showWarnings = FALSE)
  
  
  # --- Define Cache File Path ---
  safe_cache_filename <- paste0("search_", gsub("[^a-zA-Z0-9_-]", "_", search_term), ".html")
  search_cache_file <- file.path(search_cache_dir, safe_cache_filename)
  invalid_cache_file_path <- paste0(search_cache_file, ".invalid")
  
  # --- Initialize Variables ---
  search_results_page <- NULL
  search_results_html_content <- NULL
  remDr <- NULL # Remote driver object
  rD <- NULL    # rsDriver object (to manage server)
  loaded_from_cache <- FALSE
  cache_was_invalid <- FALSE
  error_encountered <- FALSE
  rselenium_startup_error <- FALSE # Flag specific to startup phase
  
  # --- Cleanup function to ensure browser/server closes ---
  on.exit({
    cat("\n--- Cleaning up RSelenium ---\n")
    # Close browser window first
    if (!is.null(remDr) && !is.null(remDr$serverUrl)) { # Check if remDr was likely initialized
      tryCatch({
        cat("Closing browser window...\n")
        remDr$close()
      }, error = function(e) {
        cat("Error closing browser window (might have already closed or failed to start):", conditionMessage(e), "\n")
      })
    }
    # Stop Selenium server
    if (!is.null(rD) && !is.null(rD$server)) {
      tryCatch({
        cat("Stopping Selenium server...\n")
        rD$server$stop()
      }, error = function(e) {
        cat("Error stopping Selenium server:", conditionMessage(e), "\n")
        # Consider more forceful stop if needed, but be cautious
        # server_process_id <- rD$server$process # Assuming wdman structure
        # if (!is.null(server_process_id)) {
        #    try(tools::pskill(server_process_id), silent = TRUE)
        #    try(tools::pskill(server_process_id, signal = tools::SIGKILL), silent = TRUE)
        # }
      })
    }
    # Nullify objects to prevent reuse after cleanup
    remDr <<- NULL
    rD <<- NULL
    # Run garbage collection
    gc()
    cat("RSelenium cleanup attempted.\n")
  }, add = TRUE) # Add = TRUE ensures it adds to any existing on.exit calls
  
  # Rename any previously marked invalid cache
  if (file.exists(invalid_cache_file_path)) {
    new_invalid_name <- paste0(invalid_cache_file_path, ".", format(Sys.time(), "%Y%m%d%H%M%S"))
    try(file.rename(invalid_cache_file_path, new_invalid_name), silent = TRUE)
    cat("Found and renamed previously marked invalid cache file to:", new_invalid_name, "\n")
  }
  
  # --- Try loading from cache first ---
  if (!refresh_search && file.exists(search_cache_file)) {
    cat("Attempting to load Google Search results from cache:", search_cache_file, "\n")
    tryCatch({
      # Read with UTF-8 encoding, suppress warnings about incomplete final line
      search_results_html_content <- paste(readLines(search_cache_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      
      # Basic check for emptiness or minimal content
      if (nchar(trimws(search_results_html_content)) < 1000) { # Increased threshold
        stop("Cached content seems too short or potentially corrupted.")
      }
      
      # Parse the HTML
      temp_page <- read_html(search_results_html_content)
      
      # Validate if it's an interstitial page
      if (is_google_interstitial_page(temp_page)) {
        cat("WARNING: Cached page seems to be an interstitial/JS-required/consent page.\n")
        cat("Marking cache file as invalid:", invalid_cache_file_path, "\n")
        try(file.rename(search_cache_file, invalid_cache_file_path), silent = TRUE)
        # Reset variables as if cache didn't exist
        search_results_page <- NULL
        search_results_html_content <- NULL
        cache_was_invalid <- TRUE
        cat("Proceeding to fetch fresh results using RSelenium.\n")
      } else {
        # Cache seems valid
        search_results_page <- temp_page
        cat("Successfully loaded and validated search results from cache.\n")
        loaded_from_cache <- TRUE
      }
    }, error = function(e) {
      cat("Error loading or validating cached file:", search_cache_file, "\n")
      cat("Error message:", conditionMessage(e), "\n")
      cat("Cache file might be empty, corrupted, or an interstitial page.\n")
      cat("Proceeding to fetch fresh results using RSelenium.\n")
      # Ensure variables are reset
      search_results_page <<- NULL # Use <<- to assign in parent environment
      search_results_html_content <<- NULL
      # Optionally, mark cache as invalid here too if loading fails
      if (file.exists(search_cache_file)) {
        cat("Marking cache file as invalid due to loading error:", invalid_cache_file_path, "\n")
        try(file.rename(search_cache_file, invalid_cache_file_path), silent = TRUE)
        cache_was_invalid <<- TRUE
      }
    })
  } else {
    if (refresh_search) cat("Cache refresh requested. Fetching fresh results.\n")
    else cat("No valid cache file found. Fetching fresh results.\n")
  }
  
  # --- Fetch using RSelenium if not loaded from cache ---
  if (!loaded_from_cache) {
    cat("\n--- Initializing RSelenium ---\n")
    rselenium_startup_error <- FALSE # Reset flag for this attempt
    tryCatch({
      # --- Determine Driver Version ---
      browser_version <- "latest" # Default to latest
      if (browser_name == "chrome") {
        cat("Checking installed Chrome version using wdman...\n")
        local_chrome_version <- tryCatch({
          wdman::chrome()$version[1] # Get MAJOR.MINOR.BUILD.PATCH
        }, error = function(e) {
          cat("Warning: wdman::chrome() failed to get local Chrome version:", conditionMessage(e), "\n")
          cat("Will attempt using 'latest' ChromeDriver.\n")
          NULL
        })
        
        if (!is.null(local_chrome_version)) {
          # Extract major version for chromever argument if needed, or let rsDriver handle 'latest' matching
          cat("Detected Chrome version:", local_chrome_version, "\n")
          # You could extract the major version: browser_version <- sub("^(\\d+)\\..*", "\\1", local_chrome_version)
          # But 'latest' often works well with wdman managing the specific driver download.
          browser_version <- "latest" # Let rsDriver/wdman try to find the best match
          cat("Attempting to use ChromeDriver compatible with version:", local_chrome_version, "(using chromever='latest')\n")
        }
      } # Add similar checks for firefox/edge if needed using wdman::firefox(), etc.
      
      # --- Configure Browser Options (e.g., headless) ---
      eCaps <- list()
      if (browser_name == "chrome") {
        eCaps <- list(
          chromeOptions = list(
            args = c('--headless', '--disable-gpu', '--window-size=1920,1080', '--lang=en-US,en')
          )
        )
        cat("Configuring Chrome for headless operation with English language.\n")
      } else if (browser_name == "firefox") {
        eCaps <- list(
          firefoxOptions = list(
            args = c('--headless')
          )
        )
        cat("Configuring Firefox for headless operation.\n")
      }
      
      
      # --- Start Selenium Server and Browser using rsDriver ---
      cat("Starting rsDriver (this may take a moment, especially the first time)...\n")
      # Use a free port to avoid conflicts
      free_port <- netstat::free_port()
      cat("Attempting to use port:", free_port, "\n")
      
      # Set verbose=TRUE for detailed debugging output if needed
      rD_temp <- rsDriver(
        browser = browser_name,
        chromever = if(browser_name == 'chrome') browser_version else NULL, # Pass detected or 'latest'
        geckover = if(browser_name == 'firefox') 'latest' else NULL,
        # edgedriver = if(browser_name == 'edge') 'latest' else NULL, # requires wdman >= 0.2.6
        port = free_port,
        verbose = TRUE, # Set TRUE for detailed debugging
        extraCapabilities = eCaps # Apply headless/other options
      )
      
      # Assign to the main variables AFTER successful start
      rD <<- rD_temp # Assign to parent scope variable
      remDr <<- rD$client # Assign to parent scope variable
      cat("RSelenium browser session started successfully.\n")
      
      # --- Construct Google Search URL ---
      search_query_quoted <- URLencode(paste0('"', search_term, '"'), reserved = TRUE)
      google_url <- paste0("https://www.google.com/search?q=", search_query_quoted,
                           "&num=", max_results, # Request specific number of results
                           "&hl=en",        # Request English results
                           "&gl=US",        # Geo-location US (optional, can influence results)
                           "&sourceid=chrome", # Mimic browser source
                           "&pws=0",        # Personal Web Search off
                           "&nfpr=1")       # No auto-correction of search terms
      
      
      cat("Navigating browser to:", google_url, "\n")
      remDr$navigate(google_url)
      Sys.sleep(wait_time_short) # Give time for initial page load and redirects
      
      # --- Handle Cookie Consent / "Before you continue" Page ---
      cat("Attempting to handle consent/interstitial popups...\n")
      # Common selectors (these change frequently!)
      consent_selectors <- c(
        "button[aria-label='Accept all']",   # Google standard
        "button[aria-label='Alle akzeptieren']", # German
        "button[aria-label='Alles accepteren']", # Dutch
        "button[id='L2AGLb']",             # Older Google ID
        "form[action*='consent'] button",   # Button within a form likely related to consent
        "div[role='dialog'] button:nth-of-type(1)", # Sometimes the first button
        "div[role='dialog'] button:nth-of-type(2)", # Sometimes the second button
        "button[jsname='VER5K']" # Another observed jsname
      )
      consent_accepted = FALSE
      for (sel in consent_selectors) {
        if(consent_accepted) break
        tryCatch({
          consent_button <- remDr$findElement(using = 'css selector', sel)
          if (!is.null(consent_button$elementId) && consent_button$isElementDisplayed()$value) {
            cat("Found potential consent button with selector:", sel, "... Clicking.\n")
            # remDr$clickElement(consent_button$elementId) # Sometimes fails
            remDr$executeScript("arguments[0].click();", list(consent_button)) # JS click is often more reliable
            Sys.sleep(3) # Wait a bit after click for page to react
            consent_accepted = TRUE
            cat("Clicked consent button.\n")
            break # Stop trying once one is found and clicked
          }
        }, error = function(e_consent) {
          # Button not found or not displayed with this selector, try next
          # cat("Selector", sel, "not found or error:", conditionMessage(e_consent), "\n") # Debug only
        })
      }
      if (!consent_accepted) cat("Could not find or click a common consent button (might not be present, page loaded directly, or selectors need update).\n")
      
      
      # --- Wait for Results to Load Dynamically ---
      cat("Waiting", wait_time_long, "seconds for search results page to fully render...\n")
      Sys.sleep(wait_time_long) # Crucial: Let JavaScript execute and content settle
      
      # --- Get Final Page Source ---
      cat("Retrieving final page source from browser...\n")
      search_results_html_content_temp <- remDr$getPageSource()[[1]]
      
      # --- Validate and Parse ---
      if (nchar(search_results_html_content_temp) < 1000) { # Increased threshold
        stop("Retrieved page source after waiting is suspiciously short. Possible loading issue or block.")
      }
      search_results_page_temp <- read_html(search_results_html_content_temp)
      
      # Check *again* for interstitial pages after waiting (e.g., reCAPTCHA)
      if (is_google_interstitial_page(search_results_page_temp)) {
        # Try saving the problematic page source for debugging
        debug_file <- file.path(search_cache_dir, paste0("debug_interstitial_", format(Sys.time(), "%Y%m%d%H%M%S"), ".html"))
        try(writeLines(search_results_html_content_temp, debug_file, useBytes = TRUE), silent=TRUE)
        cat("WARNING: Page still appears to be an interstitial/error page after waiting.\n")
        cat("Saved potential interstitial source for debugging:", debug_file, "\n")
        stop("Retrieved page seems to be an interstitial or blocked page (e.g., CAPTCHA).")
      }
      
      # If valid, assign to main variables
      search_results_html_content <<- search_results_html_content_temp
      search_results_page <<- search_results_page_temp
      cat("Successfully retrieved and validated page source via RSelenium.\n")
      
      # --- Save Valid Content to Cache ---
      cat("Saving retrieved search results to cache:", search_cache_file, "\n")
      tryCatch({
        writeLines(search_results_html_content, search_cache_file, useBytes = TRUE)
      }, error = function(e_save) {
        cat("Warning: Could not save search results to cache file:", search_cache_file, "\n")
        cat("Error message:", conditionMessage(e_save), "\n")
      })
      
    }, error = function(e) {
      # This block catches errors during rsDriver start, navigation, or page source retrieval
      cat("--------------------------------------------------------\n")
      cat("FATAL ERROR during RSelenium setup, navigation, or page retrieval:\n")
      cat("Error message:", conditionMessage(e), "\n")
      cat("\n")
      cat("Possible Causes & Things to Check:\n")
      cat(" 1. Browser / WebDriver Version Mismatch: Ensure ChromeDriver/GeckoDriver matches your installed Chrome/Firefox version *exactly*.\n")
      cat("    - Check Chrome: chrome://version\n")
      cat("    - Try updating WebDriver: wdman::selenium(driver='chromedriver', check=TRUE)\n")
      cat(" 2. WebDriver Not Found/Permissions: Ensure WebDriver is downloadable/executable by R.\n")
      cat(" 3. Port Conflict: Another Selenium instance or process might be using the port (though free_port() should help).\n")
      cat(" 4. Firewall/Antivirus: Security software might be blocking WebDriver or browser communication.\n")
      cat(" 5. Browser Crashed or Failed to Start: Check if the browser window even appeared (if not headless).\n")
      cat(" 6. Google Blocking/CAPTCHA: Google might be detecting automated access (interstitial page error above handles some cases).\n")
      cat(" 7. Network Issues: Problems connecting to Google or WebDriver download servers.\n")
      cat(" 8. Insufficient wait times: Increase wait_time_short/wait_time_long if the connection is slow.\n")
      cat("--------------------------------------------------------\n")
      error_encountered <<- TRUE
      rselenium_startup_error <<- TRUE # Specifically flag startup/nav error
      # Ensure variables are null if RSelenium failed
      search_results_page <<- NULL
      search_results_html_content <<- NULL
      # Cleanup will be handled by on.exit
    }) # End tryCatch for RSelenium block
  } # End if(!loaded_from_cache)
  
  # --- Check if we have a valid page (either from cache or RSelenium) ---
  if (is.null(search_results_page)) {
    if (rselenium_startup_error) {
      cat("Fatal Error: RSelenium failed to initialize or retrieve a valid page. See error messages above.\n")
    } else if (cache_was_invalid) {
      cat("Fatal Error: Cache was invalid, and RSelenium fetch was either disabled or also failed.\n")
    } else if (!loaded_from_cache && !refresh_search && !file.exists(search_cache_file)) {
      cat("Fatal Error: No cache file found, and RSelenium fetch failed or didn't run.\n")
    } else {
      cat("Fatal Error: Could not obtain usable search results page content. Unknown reason.\n")
    }
    # Cleanup is handled by on.exit()
    # Return minimal failure information
    return(invisible(list(
      search_term = search_term, download_directory = full_search_path,
      cached_search_used = FALSE, cache_was_invalid = cache_was_invalid,
      downloaded_pdf_files = character(), failed_pdf_downloads = character(),
      saved_html_files = character(), failed_html_fetches = character(),
      other_links_found = character(),
      error_google_fetch = TRUE, # Signifies failure to get the page content
      error_rselenium = rselenium_startup_error # Specific RSelenium startup/nav flag
    )))
  }
  
  # --- Extract Links from Valid Page ---
  cat("\n--- Extracting Links from Search Page ---\n")
  # Selectors might need updates if Google changes layout
  # .yuRUbf tends to be the main container for organic results
  # h3 elements within .g containers were common before
  # #search is the main results area ID
  link_nodes <- search_results_page %>% html_nodes(css = ".yuRUbf a") # Primary selector
  
  if (length(link_nodes) == 0) {
    cat("Warning: Primary selector '.yuRUbf a' found no links. Trying fallback selectors...\n")
    link_nodes <- search_results_page %>% html_nodes(css = "div.g a[href^='http'], h3 a[href^='http']") # More general fallbacks
    if (length(link_nodes) == 0) {
      cat("ERROR: No result links found using primary or fallback CSS selectors.\n")
      cat("Google's page structure might have changed. Examine the cached HTML file:\n", search_cache_file, "\n")
      # Continue gracefully, but downloads will be empty
    }
  }
  
  raw_links <- link_nodes %>% html_attr("href")
  
  # Clean links (handle Google redirects, filter out internal anchors/JS links)
  cleaned_links <- character()
  for (link in raw_links) {
    if (is.na(link) || !nzchar(link)) next # Skip NA or empty strings
    if (startsWith(link, "/url?q=")) {
      # Extract the actual URL from Google's redirect
      parsed_url <- urltools::url_parse(paste0("https://www.google.com", link)) # Needs base URL for parsing
      actual_url <- parsed_url$parameter$q
      if (!is.null(actual_url) && startsWith(actual_url, "http")) {
        # Decode URL encoding
        cleaned_links <- c(cleaned_links, URLdecode(actual_url))
      }
    } else if (startsWith(link, "http")) {
      # Direct link, check it's not a Google internal link (e.g., images, related searches)
      if (!grepl("google\\.com/aclk|google\\.com/search|google\\.com/maps|google\\.com/preferences|google\\.com/advanced_search", link)) {
        cleaned_links <- c(cleaned_links, link)
      }
    }
    # Ignore other links (e.g., relative paths, javascript:void(0))
  }
  
  # Remove duplicates and very short links
  cleaned_links <- unique(cleaned_links)
  cleaned_links <- cleaned_links[nchar(cleaned_links) > 15] # Basic sanity check for URL length
  
  # Remove links ending with common non-content extensions if desired (optional)
  # cleaned_links <- cleaned_links[!str_ends(cleaned_links, "\\.(jpg|png|gif|css|js)$")]
  
  cat("Found", length(cleaned_links), "plausible unique external links after cleaning.\n")
  # print(head(cleaned_links)) # Uncomment for debugging
  
  # --- Identify PDF and Other Links ---
  # Regex looks for .pdf at the end, possibly followed by query parameters (?) or fragments (#)
  pdf_pattern <- "\\.pdf(?:$|[?#])"
  pdf_links <- cleaned_links[str_detect(tolower(cleaned_links), pdf_pattern)]
  other_links <- setdiff(cleaned_links, pdf_links)
  cat("Identified", length(pdf_links), "potential PDF links.\n")
  cat("Identified", length(other_links), "other links (potential HTML/docs/etc.).\n")
  
  
  # --- Download PDFs (Using httr for downloads) ---
  downloaded_files_list <- character()
  failed_downloads_list <- character()
  if (download_pdfs && length(pdf_links) > 0) {
    cat("\n--- Attempting PDF Downloads via httr (saving to:", pdf_dir, ") ---\n")
    # Ensure dir exists (should have been created earlier)
    if (!dir.exists(pdf_dir)) dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Use a realistic User-Agent for downloads
    ua_download <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/110.0.0.0 Safari/537.36"
    
    for (i in seq_along(pdf_links)) {
      pdf_url <- pdf_links[i]
      cat("\n[PDF", i, "/", length(pdf_links), "] Trying:", pdf_url, "\n")
      target_filepath <- NULL # Reset for each loop iteration
      
      tryCatch({
        # --- Generate Safe Filename ---
        url_path <- tryCatch(urltools::path(pdf_url), error = function(e) NA) # Get path part of URL safely
        if (is.na(url_path) || nchar(url_path) < 2) {
          # If path is missing or trivial, use hash of URL
          filename_base <- paste0("downloaded_pdf_", digest::digest(pdf_url, algo="md5"))
        } else {
          filename_base <- basename(url_path)
        }
        # Decode potential URL encoding in the filename (e.g., %20 -> space)
        filename_base_decoded <- URLdecode(filename_base)
        # Remove or replace characters unsafe for filenames
        safe_filename <- gsub("[^a-zA-Z0-9_.-]", "_", filename_base_decoded)
        # Ensure it ends with .pdf
        if (!str_ends(tolower(safe_filename), ".pdf")) {
          safe_filename <- paste0(str_remove(safe_filename, "\\.[^.]*$"), ".pdf") # Remove existing extension first
        }
        # Truncate long filenames to avoid OS limits, append hash for uniqueness
        if (nchar(safe_filename) > 150) {
          safe_filename <- paste0(substr(safe_filename, 1, 140), "_", digest::digest(pdf_url, algo="xxhash32"), ".pdf")
        }
        target_filepath <- file.path(pdf_dir, safe_filename)
        cat("  Target file:", target_filepath, "\n")
        
        # --- Check if file already exists ---
        if(file.exists(target_filepath)) {
          cat("  File already exists. Skipping download.\n")
          downloaded_files_list <- c(downloaded_files_list, target_filepath)
          next # Skip to the next URL
        }
        
        # --- Perform Download with httr::GET ---
        dl_response <- httr::GET(pdf_url,
                                 httr::user_agent(ua_download),
                                 httr::write_disk(target_filepath, overwrite = TRUE), # Overwrite temp file if exists
                                 httr::timeout(90), # Increased timeout for potentially large PDFs
                                 httr::progress()) # Show download progress
        
        # --- Check Response Status and Content-Type ---
        status <- httr::status_code(dl_response)
        content_type <- httr::headers(dl_response)$`content-type`
        content_disp <- httr::headers(dl_response)$`content-disposition` # Check for filename hint
        
        cat("  HTTP Status:", status, "\n")
        cat("  Content-Type:", if (!is.null(content_type)) content_type else "NULL", "\n")
        
        if (status >= 200 && status < 300) {
          # Check if it looks like a PDF based on Content-Type or URL extension (as fallback)
          is_pdf_content <- (!is.null(content_type) && grepl("application/pdf", content_type, ignore.case = TRUE))
          looks_like_pdf <- is_pdf_content || grepl(pdf_pattern, tolower(pdf_url))
          
          if (looks_like_pdf) {
            # Optional: Check file size if possible (httr doesn't store it directly after write_disk)
            fsize <- file.size(target_filepath)
            if (!is.na(fsize) && fsize < 1024) { # Check if file size is very small (e.g., < 1KB), could be error page
              cat("  Warning: Downloaded file is very small (", fsize, " bytes). Might be an error page instead of PDF.\n")
              # Decide whether to keep it or treat as failure. Let's keep it but warn.
              downloaded_files_list <- c(downloaded_files_list, target_filepath)
            } else {
              cat("  Success! Saved potential PDF to:", target_filepath, "\n")
              downloaded_files_list <- c(downloaded_files_list, target_filepath)
            }
          } else {
            cat("  Warning: Status OK, but Content-Type ('", if (!is.null(content_type)) content_type else "NULL", "') doesn't indicate PDF. File saved, but may not be a PDF:", target_filepath, "\n")
            # Still add to downloaded list, user can check later
            downloaded_files_list <- c(downloaded_files_list, target_filepath)
          }
        } else {
          cat("  Failed! HTTP Status indicates error:", status, "\n")
          failed_downloads_list <- c(failed_downloads_list, pdf_url)
          # Remove the potentially incomplete/empty file created by write_disk on failure
          if (!is.null(target_filepath) && file.exists(target_filepath)) {
            try(file.remove(target_filepath), silent=TRUE)
          }
        }
      }, error = function(e) {
        cat("  Error during PDF download process for:", pdf_url, "\n")
        cat("  Error message:", conditionMessage(e), "\n")
        failed_downloads_list <<- c(failed_downloads_list, pdf_url) # Use <<- inside function
        # Ensure removal of potentially partial file if error occurred after path definition
        if(!is.null(target_filepath) && file.exists(target_filepath)) {
          try(file.remove(target_filepath), silent=TRUE)
        }
      }) # End tryCatch for single PDF download
      
      # --- Politeness Delay ---
      cat("  Waiting", politeness_delay, "seconds before next request...\n")
      Sys.sleep(politeness_delay)
      
    } # End loop through PDF links
  } else if (length(pdf_links) > 0) {
    cat("\nPDF downloading is disabled (download_pdfs = FALSE).\n")
  } else {
    cat("\nNo potential PDF links found to download.\n")
  }
  
  
  # --- Download HTML Content (Using httr for downloads) ---
  saved_html_files_list <- character()
  failed_html_fetches_list <- character()
  if (download_html && length(other_links) > 0) {
    cat("\n--- Attempting HTML Content Downloads via httr (saving to:", html_content_dir, ") ---\n")
    if (!dir.exists(html_content_dir)) dir.create(html_content_dir, recursive = TRUE, showWarnings = FALSE)
    ua_download <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/110.0.0.0 Safari/537.36"
    
    for (i in seq_along(other_links)) {
      html_url <- other_links[i]
      cat("\n[HTML", i, "/", length(other_links), "] Trying:", html_url, "\n")
      
      # Use hash of URL for filename to avoid issues with complex URLs/paths
      html_filename <- paste0(digest::digest(html_url, algo = "md5"), ".html")
      html_target_filepath <- file.path(html_content_dir, html_filename)
      cat("  Target file:", html_target_filepath, "\n")
      
      # --- Check if file already exists ---
      if (file.exists(html_target_filepath)) {
        cat("  HTML file already exists. Skipping download.\n")
        saved_html_files_list <- c(saved_html_files_list, html_target_filepath)
        next # Skip to the next URL
      }
      
      tryCatch({
        # --- Perform Download with httr::GET ---
        # Don't use write_disk here, fetch content first to check type
        dl_response <- httr::GET(html_url,
                                 httr::user_agent(ua_download),
                                 httr::timeout(30)) # Shorter timeout for HTML is usually fine
        
        # --- Check Response Status and Content-Type ---
        status <- httr::status_code(dl_response)
        content_type <- httr::headers(dl_response)$`content-type`
        cat("  HTTP Status:", status, "\n")
        cat("  Content-Type:", if (!is.null(content_type)) content_type else "NULL", "\n")
        
        # Follow redirects if necessary (httr handles simple redirects automatically)
        # Check status code is successful (2xx)
        if (status >= 200 && status < 300) {
          # Check if content type indicates HTML
          is_html_content <- (!is.null(content_type) && grepl("text/html", content_type, ignore.case = TRUE))
          
          if(is_html_content) {
            # Get content as text, trying to respect encoding declared by server/meta tags
            page_content <- httr::content(dl_response, as = "text", encoding = "UTF-8") # Specify UTF-8 default
            
            # Basic check for minimal content length
            if(nchar(page_content) > 100) {
              # Save the content
              writeLines(page_content, html_target_filepath, useBytes = TRUE) # useBytes avoids encoding issues on write
              cat("  Success! Saved HTML content to:", html_target_filepath, "\n")
              saved_html_files_list <- c(saved_html_files_list, html_target_filepath)
            } else {
              cat("  Warning: Status OK and Content-Type is HTML, but content is very short. Skipping save.\n")
              # Optionally save anyway or add URL to a specific 'short content' list
              failed_html_fetches_list <- c(failed_html_fetches_list, html_url)
            }
          } else {
            cat("  Warning: Status OK, but Content-Type ('", if (!is.null(content_type)) content_type else "NULL", "') is not 'text/html'. Skipping save.\n")
            # Treat as failure for HTML download purpose
            failed_html_fetches_list <- c(failed_html_fetches_list, html_url)
          }
        } else {
          cat("  Failed! HTTP Status indicates error:", status, "\n")
          failed_html_fetches_list <- c(failed_html_fetches_list, html_url)
        }
      }, error = function(e) {
        cat("  Error during HTML download process for:", html_url, "\n")
        cat("  Error message:", conditionMessage(e), "\n")
        failed_html_fetches_list <<- c(failed_html_fetches_list, html_url) # Use <<-
      }) # End tryCatch for single HTML download
      
      # --- Politeness Delay ---
      cat("  Waiting", politeness_delay, "seconds before next request...\n")
      Sys.sleep(politeness_delay)
      
    } # End loop through other links
  } else if (length(other_links) > 0) {
    cat("\nHTML downloading is disabled (download_html = FALSE).\n")
  } else {
    cat("\nNo other (non-PDF) links found to download HTML from.\n")
  }
  
  # --- Final Summary ---
  cat("\n--- Scraper Finished ---\n")
  cat("Search Term:", search_term, "\n")
  cat("Results stored in:", full_search_path, "\n")
  if(loaded_from_cache && !cache_was_invalid) cat("Used VALID cached Google Search results from:", search_cache_file, "\n")
  else if (cache_was_invalid) cat("Original cache was INVALID. Used RSelenium to fetch Google Search results.\n")
  else cat("Used RSelenium to fetch Google Search results.\n")
  
  if(download_pdfs) cat("PDFs downloaded:", length(downloaded_files_list), "; Failed/Skipped:", length(failed_downloads_list), "\n")
  if(download_html) cat("HTML pages saved:", length(saved_html_files_list), "; Failed/Skipped:", length(failed_html_fetches_list), "\n")
  
  # Note: 'other_links_found' includes links attempted for HTML download (failed or successful)
  cat("Total unique external links processed:", length(cleaned_links), "(PDFs:", length(pdf_links), ", Others:", length(other_links), ")\n")
  
  # Cleanup is handled by on.exit()
  
  # --- Return Results List ---
  return(invisible(list(
    search_term = search_term,
    download_directory = full_search_path,
    cached_search_used = loaded_from_cache && !cache_was_invalid,
    cache_was_invalid = cache_was_invalid,
    google_search_cache_file = if(file.exists(search_cache_file)) search_cache_file else NA_character_,
    pdf_download_dir = if(download_pdfs) pdf_dir else NULL,
    html_content_dir = if(download_html) html_content_dir else NULL,
    downloaded_pdf_files = downloaded_files_list,
    failed_pdf_downloads = failed_downloads_list,
    saved_html_files = saved_html_files_list,
    failed_html_fetches = failed_html_fetches_list, # URLs that failed download or were skipped (e.g., wrong content type)
    all_pdf_links_found = pdf_links,
    all_other_links_found = other_links, # All non-PDF links identified
    error_google_fetch = !loaded_from_cache && error_encountered, # True if RSelenium fetch was attempted and failed
    error_rselenium_startup = rselenium_startup_error # Specific flag for init/nav failure
  )))
}

# --- Example Usage ---

# Define your search keyword/theme
my_keyword <- "brede wijkaanpak" # Example: "integrated neighborhood approach" in Dutch

# Run the scraper:
# First time may download WebDriver. Ensure prerequisites are met.
results_selenium <- scrape_literature_selenium(
  search_term = my_keyword,
  max_results = 15,          # Number of Google results to request
  download_dir = "my_literature_cache_selenium", # Base directory for results
  download_pdfs = TRUE,      # Attempt to download PDFs
  download_html = TRUE,      # Attempt to download HTML content of other links
  refresh_search = FALSE,    # Set TRUE to force fetching from Google, ignore cache
  browser_name = "chrome",   # Use "chrome" or "firefox"
  wait_time_short = 6,       # Initial wait after navigation
  wait_time_long = 15,       # Wait for JS rendering after consent handling
  politeness_delay = 2       # Delay between individual file downloads
)

# --- Check results ---
# Print summary info stored in the returned list
if(!is.null(results_selenium)) {
  cat("\n--- Returned Summary ---")
  cat("\nSearch Term:", results_selenium$search_term)
  cat("\nDownload Dir:", results_selenium$download_directory)
  cat("\nUsed Cache:", results_selenium$cached_search_used)
  cat("\nCache Invalid:", results_selenium$cache_was_invalid)
  cat("\nRSelenium Startup Error:", results_selenium$error_rselenium_startup)
  cat("\nPDFs Downloaded:", length(results_selenium$downloaded_pdf_files))
  cat("\nPDFs Failed:", length(results_selenium$failed_pdf_downloads))
  cat("\nHTMLs Saved:", length(results_selenium$saved_html_files))
  cat("\nHTMLs Failed/Skipped:", length(results_selenium$failed_html_fetches))
  cat("\n")
  
  # Optionally list failed downloads
  # if(length(results_selenium$failed_pdf_downloads) > 0) {
  #     cat("Failed PDF URLs:\n")
  #     print(results_selenium$failed_pdf_downloads)
  # }
  # if(length(results_selenium$failed_html_fetches) > 0) {
  #     cat("Failed/Skipped HTML URLs:\n")
  #     print(results_selenium$failed_html_fetches)
  # }
} else {
  cat("Scraping function returned NULL, indicating a major failure.\n")
}

# --- IMPORTANT TROUBLESHOOTING STEPS ---
# If you still get SessionNotCreatedException or similar RSelenium startup errors:
# 1. Manually check your Chrome/Firefox version.
# 2. Run `wdman::selenium(driver = "chromedriver", check = TRUE)` (or 'geckodriver') in the R console.
#    This forces wdman to check/download the latest matching driver. Note the version it downloads.
# 3. Try running the `rsDriver(...)` part of the script interactively with `verbose = TRUE`.
#    Look closely at the output for specific errors from Java, WebDriver, or the browser.
# 4. Ensure no other Selenium instance is running and potentially blocking the port.
# 5. Temporarily disable antivirus/firewall to rule them out.
# 6. Try running R/RStudio as an administrator (less ideal, but can solve permission issues).
# 7. Check Java installation: `Sys.getenv("JAVA_HOME")` and ensure it's correctly set up.