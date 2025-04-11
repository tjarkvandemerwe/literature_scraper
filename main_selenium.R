# --- Literature Scraper with RSelenium, Caching, and HTML Download ---

# 1. Load Required Packages (Assumes already installed)
library(RSelenium)
library(rvest)
library(httr)
library(stringr)
library(dplyr)
library(urltools)
library(digest)
# library(wdman) # Optional

# --- Helper Function to Detect Interstitial Pages (Still useful for cache validation) ---
is_google_interstitial_page <- function(html_page) {
  # ... (function definition remains the same as previous version) ...
  if (is.null(html_page)) return(FALSE)
  page_text <- tryCatch(html_page %>% html_text(trim = TRUE), error = function(e) "")
  noscript_node <- tryCatch(html_page %>% html_node("noscript"), error = function(e) NULL)
  noscript_text <- if(!is.null(noscript_node)) html_text(noscript_node) else ""
  interstitial_patterns <- c(
    "Enable JavaScript", "not redirected within a few seconds",
    "having trouble accessing Google Search", "unusual traffic from your computer network",
    "/sorry/image", "needs JavaScript enabled"
  )
  found_pattern <- any(sapply(interstitial_patterns, function(pattern) {
    grepl(pattern, page_text, ignore.case = TRUE) || grepl(pattern, noscript_text, ignore.case = TRUE)
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
                                       # Specify path to your chromedriver if not in PATH
                                       # chromedriver_path = "/path/to/your/chromedriver",
                                       extra_driver_args = list(), # e.g., list(port = 4445L)
                                       wait_time_short = 5,  # Seconds to wait after navigation/clicks
                                       wait_time_long = 10, # Longer wait if needed
                                       politeness_delay = 3   # Delay for PDF/HTML downloads via httr
) {
  
  cat("--- Starting Literature Scraper (RSelenium Version) ---\n")
  cat("Search Term:", search_term, "\n")
  cat("Refresh Google Search:", refresh_search, "\n")
  cat("Download PDFs:", download_pdfs, "\n")
  cat("Download HTML Content:", download_html, "\n")
  cat("IMPORTANT: Ensure Java, Browser (", browser_name, "), and matching WebDriver are installed.\n")
  
  # --- Setup Directories ---
  safe_search_term_dir <- gsub("[^a-zA-Z0-9_-]", "_", search_term)
  full_search_path <- file.path(download_dir, safe_search_term_dir)
  pdf_dir <- file.path(full_search_path, "pdfs")
  html_content_dir <- file.path(full_search_path, "html_content")
  search_cache_dir <- file.path(full_search_path, "search_cache")
  
  if (!dir.exists(full_search_path)) dir.create(full_search_path, recursive = TRUE, showWarnings = FALSE)
  if (download_pdfs && !dir.exists(pdf_dir)) dir.create(pdf_dir, showWarnings = FALSE)
  if (download_html && !dir.exists(html_content_dir)) dir.create(html_content_dir, showWarnings = FALSE)
  if (!dir.exists(search_cache_dir)) dir.create(search_cache_dir, showWarnings = FALSE)
  
  # --- Define Cache File Path ---
  safe_cache_filename <- paste0("search_", gsub("[^a-zA-Z0-9_-]", "_", search_term), ".html")
  search_cache_file <- file.path(search_cache_dir, safe_cache_filename)
  invalid_cache_file_path <- paste0(search_cache_file, ".invalid")
  
  # --- Initialize Variables ---
  search_results_page <- NULL
  search_results_html_content <- NULL
  remDr <- NULL # Remote driver object
  rD <- NULL    # rsDriver object
  loaded_from_cache <- FALSE
  cache_was_invalid <- FALSE
  error_encountered <- FALSE
  
  # --- Cleanup function to ensure browser/server closes ---
  on.exit({
    cat("\n--- Cleaning up RSelenium ---\n")
    if (!is.null(remDr)) {
      tryCatch({ remDr$close() }, error = function(e) { cat("Error closing browser window:", conditionMessage(e), "\n") })
    }
    if (!is.null(rD) && !is.null(rD$server)) {
      tryCatch({ rD$server$stop() }, error = function(e) { cat("Error stopping Selenium server:", conditionMessage(e), "\n") })
      # Explicitly kill the process if stop fails (can happen)
      # server_process_id <- rD$server$process # Assuming wdman structure
      # if (!is.null(server_process_id)) {
      #    try(tools::pskill(server_process_id), silent = TRUE)
      #    try(tools::pskill(server_process_id, signal = tools::SIGKILL), silent = TRUE)
      # }
    }
    cat("RSelenium cleanup attempted.\n")
  })
  
  # Rename any previously marked invalid cache
  if (file.exists(invalid_cache_file_path)) {
    try(file.rename(invalid_cache_file_path, paste0(invalid_cache_file_path, ".", Sys.Date())), silent = TRUE)
    cat("Found and renamed previously marked invalid cache file.\n")
  }
  
  # --- Try loading from cache first ---
  if (!refresh_search && file.exists(search_cache_file)) {
    cat("Attempting to load Google Search results from cache:", search_cache_file, "\n")
    tryCatch({
      search_results_html_content <- paste(readLines(search_cache_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      # Basic check for emptiness
      if (nchar(trimws(search_results_html_content)) < 500) {
        stop("Cached content seems too short.")
      }
      temp_page <- read_html(search_results_html_content)
      
      if (is_google_interstitial_page(temp_page)) {
        cat("WARNING: Cached page seems to be an interstitial/JS-required page.\n")
        cat("Marking cache file as invalid:", invalid_cache_file_path, "\n")
        try(file.rename(search_cache_file, invalid_cache_file_path), silent = TRUE)
        search_results_page <- NULL
        search_results_html_content <- NULL
        cache_was_invalid <- TRUE
        cat("Proceeding to fetch fresh results using RSelenium.\n")
      } else {
        search_results_page <- temp_page
        cat("Successfully loaded and validated search results from cache.\n")
        loaded_from_cache <- TRUE
      }
    }, error = function(e) {
      cat("Error loading or validating cached file:", search_cache_file, "\n")
      cat(conditionMessage(e), "\n")
      cat("Proceeding to fetch fresh results using RSelenium.\n")
      search_results_page <- NULL
      search_results_html_content <- NULL
      # Don't set refresh_search = TRUE, let the next block handle the fetch
    })
  }
  
  # --- Fetch using RSelenium if not loaded from cache ---
  if (!loaded_from_cache) {
    cat("\n--- Initializing RSelenium ---\n")
    tryCatch({
      # --- Start Selenium Server and Browser ---
      # Check if chromedriver path is provided
      # driver_opts <- list()
      # if (exists("chromedriver_path") && !is.null(chromedriver_path) && file.exists(chromedriver_path)) {
      #     # This method is less common now, usually handled by wdman or within rsDriver
      #     # driver_opts[['phantomjs.binary.path']] = chromedriver_path # Example for phantomjs, adjust for chrome
      #     # For Chrome, setting via chromeOptions might be needed if not in PATH
      #     # Setting system property (might work)
      #     # Sys.setenv(CHROMEDRIVER_PATH = chromedriver_path)
      #     cat("Using provided chromedriver path:", chromedriver_path, "\n") # Informational
      # }
      
      # Using rsDriver (recommended) - it often handles driver download/management
      # Specify chromever = 'latest' or your specific installed Chrome version e.g., '114.0.5735.90'
      # Check chrome://version in your browser
      # If wdman is installed, it will use it.
      # Add extra_driver_args for port conflicts etc.
      browser_version <- NULL
      if (browser_name == "chrome") {
        browser_version <- tryCatch({
          wdman::chrome()$version[1] # Get installed chrome version
        }, error = function(e) "latest") # Fallback if wdman fails
        cat("Attempting to use ChromeDriver version:", browser_version, "\n")
      } # Add similar checks for firefox/edge if needed
      
      
      # Add headless option for Chrome if desired
      eCaps <- list(
        chromeOptions = list(
          args = c('--headless', '--disable-gpu', '--window-size=1920,1080', '--lang=en-US')
        )
      )
      # For Firefox:
      # eCaps <- list(
      #   firefoxOptions = list(
      #     args = c('--headless')
      #   )
      # )
      
      rD <- rsDriver(
        browser = browser_name,
        chromever = browser_version, # Let rsDriver find the appropriate version
        # geckover = "latest", # For Firefox
        # iedrver = NULL, # For IE
        # phantomver = NULL, # For PhantomJS (deprecated)
        port = netstat::free_port(), # Use a random free port
        verbose = FALSE,
        # extraCapabilities = eCaps # Uncomment to run headless
      )
      
      remDr <- rD$client
      cat("RSelenium browser session started.\n")
      
      # --- Construct Google Search URL ---
      search_query_quoted <- URLencode(paste0('"', search_term, '"'), reserved = TRUE)
      google_url <- paste0("https://www.google.com/search?q=", search_query_quoted,
                           "&num=", max_results,
                           "&hl=en", # Request English results
                           "&sourceid=chrome") # Appear like Chrome
      
      cat("Navigating browser to:", google_url, "\n")
      remDr$navigate(google_url)
      Sys.sleep(wait_time_short) # Give time for initial load
      
      # --- Handle Cookie Consent (Example - SELECTORS WILL CHANGE) ---
      cat("Attempting to handle cookie consent popup...\n")
      cookie_selectors <- c(
        "button[aria-label='Accept all']",
        "button[aria-label='Alle akzeptieren']", # German
        "button[id='L2AGLb']", # Common ID observed previously
        "div[role='dialog'] button:last-of-type" # Generic: last button in a dialog
      )
      accepted_cookie = FALSE
      for (sel in cookie_selectors) {
        tryCatch({
          cookie_button <- remDr$findElement(using = 'css selector', sel)
          if (!is.null(cookie_button$elementId)) {
            cat("Found potential cookie button with selector:", sel, "... Clicking.\n")
            remDr$clickElement(cookie_button$elementId)
            Sys.sleep(2) # Wait a bit after click
            accepted_cookie = TRUE
            break # Stop trying once one is found and clicked
          }
        }, error = function(e) {
          # Button not found with this selector, try next
        })
      }
      if (!accepted_cookie) cat("Could not find or click a common cookie consent button (might not be present or selectors changed).\n")
      
      # --- Wait for Results to Load ---
      cat("Waiting", wait_time_long, "seconds for page to fully load results...\n")
      Sys.sleep(wait_time_long) # Crucial: Let JavaScript render the page
      
      # --- Get Page Source ---
      cat("Retrieving page source from browser...\n")
      search_results_html_content <- remDr$getPageSource()[[1]]
      
      # --- Validate and Parse ---
      if (nchar(search_results_html_content) < 500) {
        stop("Retrieved page source is suspiciously short.")
      }
      search_results_page <- read_html(search_results_html_content)
      
      # Optional: Check again for interstitial pattern, though less likely with RSelenium
      if (is_google_interstitial_page(search_results_page)) {
        cat("WARNING: Even with RSelenium, the retrieved page might be an interstitial/error page.\n")
        # Decide how to handle this - maybe try reloading? For now, treat as failure.
        stop("Retrieved page seems to be an interstitial page.")
      }
      
      cat("Successfully retrieved page source via RSelenium.\n")
      
      # --- Save Valid Content to Cache ---
      cat("Saving retrieved search results to cache:", search_cache_file, "\n")
      tryCatch({
        writeLines(search_results_html_content, search_cache_file, useBytes = TRUE)
      }, error = function(e_save) {
        cat("Warning: Could not save search results to cache file:", search_cache_file, "\n")
        cat(conditionMessage(e_save), "\n")
      })
      
    }, error = function(e) {
      cat("ERROR during RSelenium setup or navigation:\n")
      cat(conditionMessage(e), "\n")
      error_encountered <<- TRUE # Use <<- to modify variable in parent scope
      search_results_page <<- NULL
      search_results_html_content <<- NULL
    }) # End tryCatch for RSelenium block
  } # End if(!loaded_from_cache)
  
  # --- Check if we have a valid page (either from cache or RSelenium) ---
  if (is.null(search_results_page) || error_encountered) {
    cat("Fatal Error: Could not obtain usable search results page content via cache or RSelenium.\n")
    # Cleanup is handled by on.exit()
    return(list(
      search_term = search_term, download_directory = full_search_path,
      cached_search_used = loaded_from_cache && !cache_was_invalid,
      cache_was_invalid = cache_was_invalid,
      downloaded_pdf_files = character(), failed_pdf_downloads = character(),
      saved_html_files = character(), failed_html_fetches = character(),
      other_links_found = character(),
      error_google_fetch = !loaded_from_cache, # Fetch was attempted if not loaded from cache
      error_rselenium = error_encountered # Specific flag for RSelenium phase error
    ))
  }
  
  # --- Extract Links from Valid Page ---
  cat("\n--- Extracting Links from Valid Search Page ---\n")
  # ... (Link extraction and cleaning logic remains the same) ...
  link_nodes <- search_results_page %>% html_nodes(css = ".yuRUbf a, #search .g a, h3.r a")
  if (length(link_nodes) == 0) {
    cat("WARNING: No standard result links found using selectors.\n")
  }
  raw_links <- link_nodes %>% html_attr("href")
  cleaned_links <- character()
  # ... (loop for cleaning links) ...
  for (link in raw_links) {
    if (is.na(link)) next
    if (startsWith(link, "/url?q=")) {
      parsed_url <- urltools::url_parse(paste0("http://google.com", link))
      actual_url <- parsed_url$parameter$q
      if (!is.null(actual_url)) {
        cleaned_links <- c(cleaned_links, URLdecode(actual_url))
      }
    } else if (startsWith(link, "http")) {
      cleaned_links <- c(cleaned_links, link)
    }
  }
  cleaned_links <- unique(cleaned_links)
  cleaned_links <- cleaned_links[nchar(cleaned_links) > 10]
  cat("Found", length(cleaned_links), "cleaned unique links.\n")
  
  # --- Identify PDF and Other Links ---
  pdf_links <- cleaned_links[str_detect(cleaned_links, "\\.pdf$|\\.pdf[?#]")]
  other_links <- setdiff(cleaned_links, pdf_links)
  cat("Identified", length(pdf_links), "potential PDF links.\n")
  cat("Identified", length(other_links), "other links.\n")
  
  
  # --- Download PDFs (Using httr for downloads) ---
  downloaded_files_list <- character()
  failed_downloads_list <- character()
  if (download_pdfs && length(pdf_links) > 0) {
    cat("\n--- Attempting PDF Downloads via httr (saving to:", pdf_dir, ") ---\n")
    if (!dir.exists(pdf_dir)) dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)
    ua_download <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/110.0.0.0 Safari/537.36" # UA for downloads
    for (i in seq_along(pdf_links)) {
      pdf_url <- pdf_links[i]
      cat("\n[PDF", i, "/", length(pdf_links), "] Trying:", pdf_url, "\n")
      target_filepath <- NULL
      tryCatch({
        # ... (filename generation logic remains the same) ...
        url_path <- urltools::path(pdf_url)
        if (is.na(url_path) || nchar(url_path) < 2) {
          filename_base <- paste0("downloaded_pdf_", digest::digest(pdf_url, algo="md5"))
        } else {
          filename_base <- basename(url_path)
        }
        filename_base_decoded <- URLdecode(filename_base)
        safe_filename <- gsub("[^a-zA-Z0-9_.-]", "_", filename_base_decoded)
        if (!str_ends(tolower(safe_filename), ".pdf")) {
          safe_filename <- paste0(safe_filename, ".pdf")
        }
        if (nchar(safe_filename) > 150) {
          safe_filename <- paste0(substr(safe_filename, 1, 140), "_", digest::digest(pdf_url, algo="xxhash32"), ".pdf")
        }
        target_filepath <- file.path(pdf_dir, safe_filename)
        
        if(file.exists(target_filepath)) {
          cat("  File already exists:", target_filepath, "... Skipping.\n")
          downloaded_files_list <- c(downloaded_files_list, target_filepath)
          next
        }
        # Use httr GET for the download
        dl_response <- httr::GET(pdf_url,
                                 httr::user_agent(ua_download), # Use a standard UA
                                 httr::write_disk(target_filepath, overwrite = TRUE),
                                 httr::timeout(60))
        
        if (httr::status_code(dl_response) >= 200 && httr::status_code(dl_response) < 300) {
          content_type <- httr::headers(dl_response)$`content-type`
          if (!is.null(content_type) && grepl("application/pdf", content_type, ignore.case = TRUE)) {
            cat("  Success! Saved PDF to:", target_filepath, "\n")
            downloaded_files_list <- c(downloaded_files_list, target_filepath)
          } else {
            cat("  Warning: Status OK, but Content-Type is '", content_type %||% "NULL", "'. Still saved:", target_filepath, "\n")
            downloaded_files_list <- c(downloaded_files_list, target_filepath)
          }
        } else {
          cat("  Failed! HTTP Status:", httr::status_code(dl_response), "\n")
          failed_downloads_list <- c(failed_downloads_list, pdf_url)
          if (file.exists(target_filepath)) try(file.remove(target_filepath), silent=TRUE)
        }
      }, error = function(e) {
        cat("  Error downloading or saving PDF:", pdf_url, "\n")
        cat("  ", conditionMessage(e), "\n")
        failed_downloads_list <- c(failed_downloads_list, pdf_url)
        if(!is.null(target_filepath) && file.exists(target_filepath)) {
          try(file.remove(target_filepath), silent=TRUE)
        }
      })
      Sys.sleep(politeness_delay) # Delay between httr download requests
    }
  } else if (length(pdf_links) > 0) {
    cat("\nPDF downloading is disabled (download_pdfs = FALSE).\n")
  } else {
    cat("\nNo potential PDF links found.\n")
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
      html_filename <- paste0(digest::digest(html_url, algo = "md5"), ".html")
      html_target_filepath <- file.path(html_content_dir, html_filename)
      
      if (file.exists(html_target_filepath)) {
        cat("  HTML file already exists:", html_target_filepath, "... Skipping.\n")
        saved_html_files_list <- c(saved_html_files_list, html_target_filepath)
        next
      }
      tryCatch({
        dl_response <- httr::GET(html_url,
                                 httr::user_agent(ua_download),
                                 httr::timeout(30))
        
        if (httr::status_code(dl_response) >= 200 && httr::status_code(dl_response) < 400) {
          content_type <- httr::headers(dl_response)$`content-type`
          if (!is.null(content_type) && grepl("text/html", content_type, ignore.case = TRUE)) {
            page_content <- httr::content(dl_response, "text", encoding = "UTF-8")
            writeLines(page_content, html_target_filepath, useBytes = TRUE)
            cat("  Success! Saved HTML to:", html_target_filepath, "\n")
            saved_html_files_list <- c(saved_html_files_list, html_target_filepath)
          } else {
            cat("  Warning: Status OK, but Content-Type is '", content_type %||% "NULL", "' (not text/html). Skipping save.\n")
          }
        } else {
          cat("  Failed! HTTP Status:", httr::status_code(dl_response), "\n")
          failed_html_fetches_list <- c(failed_html_fetches_list, html_url)
        }
      }, error = function(e) {
        cat("  Error fetching HTML content:", html_url, "\n")
        cat("  ", conditionMessage(e), "\n")
        failed_html_fetches_list <- c(failed_html_fetches_list, html_url)
      })
      Sys.sleep(politeness_delay) # Delay between httr download requests
    }
  } else if (length(other_links) > 0) {
    cat("\nHTML downloading is disabled (download_html = FALSE).\n")
  } else {
    cat("\nNo other (non-PDF) links found to download HTML from.\n")
  }
  
  # --- Final Summary ---
  cat("\n--- Scraper Finished ---\n")
  # ... (Summary printing remains similar) ...
  cat("Search Term:", search_term, "\n")
  cat("Results stored in:", full_search_path, "\n")
  if(loaded_from_cache) cat("Used cached Google Search results.\n") else cat("Used RSelenium to fetch Google Search results.\n")
  if(cache_was_invalid) cat("Original cache file was invalid and marked/renamed.\n")
  if(download_pdfs) cat("PDFs downloaded:", length(downloaded_files_list), "; Failed:", length(failed_downloads_list), "\n")
  if(download_html) cat("HTML pages saved:", length(saved_html_files_list), "; Failed:", length(failed_html_fetches_list), "\n")
  cat("Other links found (not downloaded as PDF/HTML):", length(setdiff(other_links, failed_html_fetches_list)), "\n")
  
  # Cleanup is handled by on.exit()
  
  return(invisible(list(
    # ... (Return list structure remains similar) ...
    search_term = search_term,
    download_directory = full_search_path,
    cached_search_used = loaded_from_cache && !cache_was_invalid,
    cache_was_invalid = cache_was_invalid,
    google_search_cache_file = search_cache_file,
    pdf_download_dir = if(download_pdfs) pdf_dir else NULL,
    html_content_dir = if(download_html) html_content_dir else NULL,
    downloaded_pdf_files = downloaded_files_list,
    failed_pdf_downloads = failed_downloads_list,
    saved_html_files = saved_html_files_list,
    failed_html_fetches = failed_html_fetches_list,
    other_links_found = other_links,
    error_google_fetch = !loaded_from_cache && error_encountered, # True if RSelenium fetch failed
    error_rselenium = error_encountered # Specific flag
  )))
}

# --- Example Usage ---

# Define your search keyword/theme
my_keyword <- "brede wijkaanpak"

# Run the scraper:
# Make sure prerequisites (Java, Chrome/Firefox, WebDriver) are met!
# The first run might take longer as RSelenium starts up.
results_selenium <- scrape_literature_selenium(
  search_term = my_keyword,
  max_results = 15,
  download_dir = "my_literature_cache_selenium",
  download_pdfs = TRUE,
  download_html = TRUE,
  refresh_search = FALSE, # Use cache if available and valid
  browser_name = "chrome", # Or "firefox"
  # chromedriver_path = "path/to/your/driver", # Specify if needed
  wait_time_short = 7,     # Increase if needed
  wait_time_long = 12,    # Increase if page loads slowly
  politeness_delay = 3
)

# Check results
# print(results_selenium$error_rselenium)
# print(results_selenium$downloaded_pdf_files)