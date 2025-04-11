# --- Literature Scraper with Caching, HTML Download, and Interstitial Page Detection ---

# 1. Install and Load Required Packages
# install.packages(c("rvest", "httr", "stringr", "dplyr", "urltools", "digest"))
library(rvest)
library(httr)
library(stringr)
library(dplyr)
library(urltools) # Useful for URL parsing and cleaning
library(digest)   # For creating safe filenames from URLs

# --- Helper Function to Detect Interstitial Pages ---
is_google_interstitial_page <- function(html_page) {
  if (is.null(html_page)) return(FALSE) # Can't check a null object
  
  # Check for specific text content often found in these pages
  page_text <- html_page %>% html_text(trim = TRUE)
  
  # Check within <noscript> tag more specifically
  noscript_node <- html_page %>% html_node("noscript")
  noscript_text <- if(!is.null(noscript_node)) html_text(noscript_node) else ""
  
  # Check for common phrases/patterns
  interstitial_patterns <- c(
    "Enable JavaScript",
    "not redirected within a few seconds",
    "having trouble accessing Google Search", # From the #yvlrue div
    "unusual traffic from your computer network",
    "/sorry/image", # Often part of CAPTCHA pages
    "needs JavaScript enabled"
  )
  
  found_pattern <- any(sapply(interstitial_patterns, function(pattern) {
    grepl(pattern, page_text, ignore.case = TRUE) || grepl(pattern, noscript_text, ignore.case = TRUE)
  }))
  
  # Also check if the main result selectors are completely absent (as a secondary check)
  # Be cautious with this, as a real search might have zero results, but combined
  # with the text patterns it's a stronger signal.
  # link_nodes_check <- html_page %>% html_nodes(css = ".yuRUbf a, #search .g a, h3.r a")
  # zero_results_present <- length(link_nodes_check) == 0
  
  # Primarily rely on text patterns
  return(found_pattern)
}


# --- Main Function ---
scrape_literature <- function(search_term,
                              max_results = 20,
                              download_dir = "literature_downloads",
                              download_pdfs = TRUE,
                              download_html = TRUE,
                              refresh_search = FALSE,
                              politeness_delay = 2) {
  
  cat("--- Starting Literature Scraper ---\n")
  # ... (rest of initial setup messages) ...
  cat("Refresh Google Search:", refresh_search, "\n")
  cat("Download PDFs:", download_pdfs, "\n")
  cat("Download HTML Content:", download_html, "\n")
  # ... (rest of initial setup messages) ...
  
  # --- Setup Directories ---
  # ... (Directory setup code remains the same) ...
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
  invalid_cache_file_path <- paste0(search_cache_file, ".invalid") # Path for marking invalid cache
  
  # --- Fetch or Load Google Search Page ---
  search_results_page <- NULL
  search_results_html_content <- NULL
  fetched_from_google <- FALSE
  loaded_from_cache <- FALSE
  cache_was_invalid <- FALSE
  
  # Rename any previously marked invalid cache to avoid confusion
  if (file.exists(invalid_cache_file_path)) {
    file.rename(invalid_cache_file_path, paste0(invalid_cache_file_path, ".", Sys.Date())) # Add date suffix
    cat("Found and renamed previously marked invalid cache file.\n")
  }
  
  # Try loading from cache first if refresh_search is FALSE
  if (!refresh_search && file.exists(search_cache_file)) {
    cat("Attempting to load Google Search results from cache:", search_cache_file, "\n")
    tryCatch({
      search_results_html_content <- paste(readLines(search_cache_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      temp_page <- read_html(search_results_html_content)
      
      if (is_google_interstitial_page(temp_page)) {
        cat("WARNING: Cached page seems to be an interstitial/JS-required page, not actual results.\n")
        cat("Marking cache file as invalid:", invalid_cache_file_path, "\n")
        # Rename the invalid cache file instead of deleting
        file.rename(search_cache_file, invalid_cache_file_path)
        search_results_page <- NULL # Ensure page is null
        search_results_html_content <- NULL
        cache_was_invalid <- TRUE
        # Force a fetch attempt now, even though refresh_search was FALSE
        cat("Proceeding to fetch fresh results from Google.\n")
        refresh_search <- TRUE # Temporarily override for this run
      } else {
        search_results_page <- temp_page
        cat("Successfully loaded and validated search results from cache.\n")
        loaded_from_cache <- TRUE
      }
    }, error = function(e) {
      cat("Error loading or validating cached file:", search_cache_file, "\n")
      cat(conditionMessage(e), "\n")
      cat("Proceeding to fetch fresh results from Google.\n")
      search_results_page <- NULL
      search_results_html_content <- NULL
      refresh_search <- TRUE # Force fetch if loading failed
    })
  }
  
  # If not loaded from cache (or cache was invalid, or refresh_search is TRUE), fetch from Google
  if (is.null(search_results_page) && !loaded_from_cache) {
    # ... (message printing) ...
    
    # Construct Google Search URL with more parameters
    search_query_encoded <- URLencode(search_term, reserved = TRUE) # Use search term directly for oq
    search_query_quoted <- URLencode(paste0('"', search_term, '"'), reserved = TRUE) # Quoted for main q=
    google_url <- paste0(
      "https://www.google.com/search?q=", search_query_quoted,
      "&oq=", search_query_encoded,   # Add original query
      "&sourceid=chrome",             # Add sourceid
      "&ie=UTF-8",                    # Keep encoding
      "&num=", max_results,
      "&hl=en"                        # Keep language
      # Optionally add rlz, but be cautious: "&rlz=1C1GCEB_enNL1051NL1051"
    )
    
    cat("Searching Google:", google_url, "\n")
    
    # Perform Search and Get HTML with more headers
    ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36" # Example of a slightly newer UA
    request_headers <- httr::add_headers(
      `User-Agent` = ua,
      `Accept-Language` = "en-US,en;q=0.9", # Common Accept-Language
      `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7", # Common Accept header
      `Upgrade-Insecure-Requests` = "1", # Often sent by browsers
      `Sec-Fetch-Dest` = "document",
      `Sec-Fetch-Mode` = "navigate",
      `Sec-Fetch-Site` = "same-origin", # Or "none" if simulating typing into address bar
      `Sec-Fetch-User` = "?1"
      # DNT = "1" # Do Not Track - optional
    )
    
    response <- NULL
    fetched_from_google <- TRUE
    
    tryCatch({
      response <- httr::GET(
        google_url,
        request_headers, # Use the defined headers
        httr::timeout(25) # Slightly longer timeout maybe
      )
      httr::stop_for_status(response) # Check for HTTP errors (4xx, 5xx)
      
      temp_html_content <- httr::content(response, "text", encoding = "UTF-8")
      temp_page <- read_html(temp_html_content)
      
      # *** CHECK FOR INTERSTITIAL PAGE AFTER FETCH *** (Same check as before)
      if (is_google_interstitial_page(temp_page)) {
        cat("ERROR: Fetched page from Google appears to be an interstitial/JS-required page.\n")
        cat("This likely means Google detected automated access despite added headers.\n")
        cat("Try increasing politeness_delay significantly (e.g., 10+ seconds) or use other methods.\n")
        search_results_page <- NULL
        search_results_html_content <- NULL
        cat("Invalid page content will not be saved to cache.\n")
      } else {
        # Page seems valid, proceed
        search_results_html_content <- temp_html_content
        search_results_page <- temp_page
        cat("Successfully fetched and validated search results page.\n")
        
        # Save the VALID fetched content to cache
        cat("Saving fetched Google Search results to cache:", search_cache_file, "\n")
        tryCatch({
          writeLines(search_results_html_content, search_cache_file, useBytes = TRUE)
        }, error = function(e_save) {
          cat("Warning: Could not save search results to cache file:", search_cache_file, "\n")
          cat(conditionMessage(e_save), "\n")
        })
      }
      
      Sys.sleep(politeness_delay) # Be polite after fetching search page
      
    }, error = function(e) {
      cat("Error fetching or parsing Google Search results:\n")
      cat(conditionMessage(e), "\n")
      cat("This might be due to Google blocking, network issues, or changed HTML structure.\n")
      search_results_page <- NULL
      search_results_html_content <- NULL
      # Return minimal info if Google search failed
      return(list(search_term = search_term,
                  download_directory = full_search_path,
                  cached_search_used = FALSE,
                  downloaded_files = character(0),
                  failed_downloads = character(0),
                  saved_html_files = character(0),
                  failed_html_fetches = character(0),
                  other_links = character(0),
                  error_google_fetch = TRUE,
                  error_interstitial_page = FALSE)) # Not specifically interstitial if fetch itself failed
    })
  }
  
  # If still null after attempting fetch/load, something went wrong (could be interstitial or other fetch error)
  if (is.null(search_results_page)) {
    cat("Fatal Error: Could not obtain usable search results page content.\n")
    return(list(search_term = search_term,
                download_directory = full_search_path,
                cached_search_used = loaded_from_cache,
                downloaded_files = character(0),
                failed_downloads = character(0),
                saved_html_files = character(0),
                failed_html_fetches = character(0),
                other_links = character(0),
                error_google_fetch = fetched_from_google, # TRUE if we tried to fetch
                error_interstitial_page = fetched_from_google # Assume interstitial if fetch was attempted but page is null
    ))
  }
  
  # --- Extract Links (Proceed only if page is valid) ---
  cat("\n--- Extracting Links from Valid Search Page ---\n")
  link_nodes <- search_results_page %>%
    html_nodes(css = ".yuRUbf a, #search .g a, h3.r a") # Add more selectors if needed
  
  if (length(link_nodes) == 0) {
    cat("WARNING: No standard result links found using selectors (.yuRUbf a, #search .g a, h3.r a).\n")
    cat("Google's page structure might have changed, or the search truly yielded no results.\n")
    # Consider adding more checks or different selectors if this happens frequently with valid pages
  }
  
  raw_links <- link_nodes %>% html_attr("href")
  cat("Found", length(raw_links), "potential raw links in search results.\n")
  
  # --- Clean and Filter Links (Same as before) ---
  # ... (Link cleaning code remains the same) ...
  cleaned_links <- character()
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
  # ... (Link identification code remains the same) ...
  pdf_links <- cleaned_links[str_detect(cleaned_links, "\\.pdf$|\\.pdf[?#]")]
  other_links <- setdiff(cleaned_links, pdf_links)
  cat("Identified", length(pdf_links), "potential PDF links.\n")
  cat("Identified", length(other_links), "other links.\n")
  
  # --- Download PDFs (if requested) ---
  # ... (PDF download code remains the same) ...
  downloaded_files_list <- character()
  failed_downloads_list <- character()
  if (download_pdfs && length(pdf_links) > 0) {
    cat("\n--- Attempting PDF Downloads (saving to:", pdf_dir, ") ---\n")
    if (!dir.exists(pdf_dir)) dir.create(pdf_dir, recursive = TRUE, showWarnings = FALSE)
    # ... (rest of the PDF download loop)
    for (i in seq_along(pdf_links)) {
      # ... (inside loop code unchanged) ...
    }
  } else if (length(pdf_links) > 0) {
    cat("\nPDF downloading is disabled (download_pdfs = FALSE).\n")
  } else {
    cat("\nNo potential PDF links found.\n")
  }
  
  
  # --- Download HTML Content (if requested) ---
  # ... (HTML download code remains the same) ...
  saved_html_files_list <- character()
  failed_html_fetches_list <- character()
  if (download_html && length(other_links) > 0) {
    cat("\n--- Attempting HTML Content Downloads (saving to:", html_content_dir, ") ---\n")
    if (!dir.exists(html_content_dir)) dir.create(html_content_dir, recursive = TRUE, showWarnings = FALSE)
    # ... (rest of the HTML download loop)
    for (i in seq_along(other_links)) {
      # ... (inside loop code unchanged) ...
    }
  } else if (length(other_links) > 0) {
    cat("\nHTML downloading is disabled (download_html = FALSE).\n")
  } else {
    cat("\nNo other (non-PDF) links found to download HTML from.\n")
  }
  
  
  # --- Final Summary ---
  cat("\n--- Scraper Finished ---\n")
  cat("Search Term:", search_term, "\n")
  cat("Results stored in:", full_search_path, "\n")
  if(loaded_from_cache) cat("Used cached Google Search results.\n") else if(fetched_from_google) cat("Fetched fresh Google Search results.\n")
  if(cache_was_invalid) cat("Original cache file was invalid and marked/renamed.\n")
  if(download_pdfs) cat("PDFs downloaded:", length(downloaded_files_list), "; Failed:", length(failed_downloads_list), "\n")
  if(download_html) cat("HTML pages saved:", length(saved_html_files_list), "; Failed:", length(failed_html_fetches_list), "\n")
  cat("Other links found (not downloaded as PDF/HTML):", length(setdiff(other_links, failed_html_fetches_list)), "\n")
  
  
  # Return results
  return(invisible(list(
    search_term = search_term,
    download_directory = full_search_path,
    cached_search_used = loaded_from_cache,
    cache_was_invalid = cache_was_invalid,
    google_search_cache_file = search_cache_file,
    pdf_download_dir = if(download_pdfs) pdf_dir else NULL,
    html_content_dir = if(download_html) html_content_dir else NULL,
    downloaded_pdf_files = downloaded_files_list,
    failed_pdf_downloads = failed_downloads_list,
    saved_html_files = saved_html_files_list,
    failed_html_fetches = failed_html_fetches_list,
    other_links_found = other_links,
    error_google_fetch = fetched_from_google && is.null(search_results_page), # True if fetch was attempted but failed OR got interstitial
    error_interstitial_page = fetched_from_google && is.null(search_results_page) && !loaded_from_cache # More specific error
  )))
}

# --- Example Usage ---

# Define your search keyword/theme
my_keyword <- "brede wijkaanpak" # Using your example keyword

# Run the scraper:
# Set refresh_search = TRUE if you know the cache is bad or want fresh results
results1 <- scrape_literature(
  search_term = my_keyword,
  max_results = 15,
  download_dir = "my_literature_cache_v2",
  download_pdfs = TRUE,
  download_html = TRUE,
  refresh_search = FALSE, # Start with FALSE to test cache first
  politeness_delay = 4    # Maybe increase delay slightly
)

# If the first run failed due to interstitial page from cache:
# 1. The script should have marked the cache as invalid.
# 2. It would have attempted a fresh fetch in that same run.
# 3. If the fresh fetch ALSO failed (e.g., got another interstitial page),
#    you might need to wait, change IP, or increase delay significantly.

# Example of forcing a refresh if you suspect cache issues:
# results_refresh <- scrape_literature(
#   search_term = my_keyword,
#   max_results = 15,
#   download_dir = "my_literature_cache_v2",
#   download_pdfs = TRUE,
#   download_html = TRUE,
#   refresh_search = TRUE, # Force fetch
#   politeness_delay = 5
# )

# Check results, especially the error flags
# print(results1$error_google_fetch)
# print(results1$error_interstitial_page)
# print(results1$cache_was_invalid)