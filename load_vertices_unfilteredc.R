load_all_vertices_unfiltered <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  lines <- gsub("\t", " ", lines)

  v_lines <- grep("^v\\s", lines, value = TRUE)
  if (length(v_lines) == 0) return(matrix(NA, nrow = 0, ncol = 3))

  raw_vertices <- t(sapply(v_lines, function(line) {
    # Strip only the 'v' and preserve minus signs
    clean_line <- sub("^v\\s+", "", line)
    parts <- strsplit(trimws(clean_line), "\\s+")[[1]]
    vals <- suppressWarnings(as.numeric(parts[1:3]))
    if (length(vals) < 3 || any(is.na(vals))) {
      return(c(NA, NA, NA))
    } else {
      return(vals)
    }
  }))
  
  return(raw_vertices)
}
