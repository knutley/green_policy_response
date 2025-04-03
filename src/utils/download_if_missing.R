#' Download a required file if missing
#'
#' Checks if a file exists locally. If not, downloads it automatically from
#' a URL listed in required_files.csv. Displays progress as
#' "XXX MB / YYY MB" while downloading.
#'
#' @param filename The expected filename (e.g. "Corp_HouseOfCommons_V2.rds")
#' @param dest_dir The folder to download into (e.g. "data/raw/")
#' @return Full path to the file
#' @export

download_if_missing <- function(filename, dest_dir = "data/raw") {
  # We'll use the curl package for chunked downloads and progress updates
  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("'curl' package is required. Install it via install.packages('curl').")
  }

  full_path <- file.path(dest_dir, filename)
  if (file.exists(full_path)) {
    return(full_path)
  }

  metadata_path <- "data/required_files.csv"
  if (!file.exists(metadata_path)) {
    stop("Required metadata file not found at ", metadata_path)
  }

  # Read CSV and ensure it contains filename,url
  urls <- data.table::fread(metadata_path)
  if (!all(c("filename", "url") %in% names(urls))) {
    stop("CSV must contain columns: filename,url")
  }
  
  # Trim whitespace from filenames to ensure an exact match
  urls[, filename := trimws(filename)]
  filename <- trimws(filename)
  
  # Find the row where urls$filename exactly matches our requested filename
  # Use a different approach to matching that's more explicit
  match_idx <- which(urls$filename == filename)
  if (length(match_idx) == 0) {
    stop("No URL found in required_files.csv for: ", filename)
  }
  match <- urls[match_idx[1],]
  
  # Debug: print the matched row
  message("DEBUG: Found metadata - filename: '", match$filename,
          "', URL: '", match$url,
          "', size: '", if("size" %in% names(match)) match$size else "unknown", "'")
  
  url  <- match$url
  # e.g. "2.3 GB", "727 MB", or maybe something else
  size <- if ("size" %in% names(match)) match$size else ""

  message("File '", filename, "' is missing.")
  message("Automatically downloading (~", size, ") from:\n", url)

  # Create destination directory if needed
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  # Extend timeout for large files (2 hours)
  old_timeout <- getOption("timeout")
  options(timeout = 7200)
  on.exit(options(timeout = old_timeout), add = TRUE)

  # Temporary file to avoid partial/corrupt final files on failure
  tmp_file <- tempfile(tmpdir = dest_dir, fileext = ".download")

  # Download, showing MB progress
  .download_with_progress(url, tmp_file, size)

  # Rename to final destination once the download completes
  file.rename(tmp_file, full_path)

  message("Downloaded to ", full_path)
  return(full_path)
}

# ---- Helper Functions -------------------------------------------------------

# Attempt to parse "2.3 GB" or "727 MB" into bytes. If parsing fails, returns 0.
.parse_size_to_bytes <- function(size_str) {
  # We'll do a simple regex to capture a number and "GB" or "MB".
  # E.g. "2.3 GB" -> c("2.3", "GB")
  m <- regexpr("([0-9.]+)\\s*(GB|MB)", size_str, ignore.case = TRUE, perl = TRUE)
  if (m == -1) return(0)

  # Extract the matched substring (e.g. "2.3 GB")
  match_str <- regmatches(size_str, m)
  # Now split into number and unit
  captures <- regmatches(match_str, regexpr("([0-9.]+)\\s*(GB|MB)", match_str, ignore.case=TRUE))
  if (length(captures) == 0) return(0)

  # separate the numeric part and unit part
  # e.g. captures might be "2.3 GB" => sub-captures are in regmatches?
  numeric_part <- sub("([0-9.]+)\\s*(GB|MB)", "\\1", captures, ignore.case = TRUE)
  unit_part    <- sub("([0-9.]+)\\s*(GB|MB)", "\\2", captures, ignore.case = TRUE)

  val <- as.numeric(numeric_part)
  if (is.na(val)) return(0)

  # Convert to bytes
  unit_part <- toupper(unit_part)
  if (unit_part == "GB") {
    return(val * 1e9)
  } else if (unit_part == "MB") {
    return(val * 1e6)
  }
  return(0)
}

# Attempt a HEAD request for Content-Length. If not available, returns 0.
.get_content_length <- function(url) {
  h <- curl::new_handle()
  curl::handle_setopt(h, nobody = TRUE)  # HEAD request
  info <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)
  if (is.null(info)) {
    # HEAD request failed, fallback to 0
    return(0)
  }
  # Look for a "content-length" header
  hdrs <- info$headers
  cl_name <- tolower(names(hdrs)) == "content-length"
  if (any(cl_name)) {
    val <- as.numeric(hdrs[cl_name])
    if (!is.na(val)) return(val)
  }
  return(0)
}

# Download a file in chunks, displaying a progress message:
# "XXX MB / YYY MB" if the total is known, otherwise "XXX MB downloaded".
.download_with_progress <- function(url, destfile, fallback_size_str, chunk_size = 64 * 1024) {
  # Attempt HEAD to get content length from server
  total_len <- .get_content_length(url)

  # If HEAD gave us no content-length, parse the CSV fallback (e.g. "2.3 GB")
  if (total_len == 0 && nzchar(fallback_size_str)) {
    total_len <- .parse_size_to_bytes(fallback_size_str)
  }

  # Open read and write connections
  con_in  <- curl::curl(url, "rb")
  on.exit(close(con_in), add = TRUE)

  con_out <- file(destfile, "wb")
  on.exit(close(con_out), add = TRUE)

  downloaded <- 0
  repeat {
    # Read up to 'chunk_size' bytes
    buf <- readBin(con_in, raw(), n = chunk_size)
    if (length(buf) == 0) {
      # no more data
      break
    }
    # Write to our destination file
    writeBin(buf, con_out)
    downloaded <- downloaded + length(buf)

    # Convert to MB
    mb_downloaded <- downloaded / 1e6
    mb_total      <- total_len / 1e6

    # If we know total_len, show "XXX MB / YYY MB", else just show downloaded
    if (total_len > 0) {
      cat(sprintf("\r%.1f MB / ~%.1f MB", mb_downloaded, mb_total))
    } else {
      cat(sprintf("\r%.1f MB downloaded", mb_downloaded))
    }
    flush.console()
  }
  cat("\n")  # final newline after loop finishes
}