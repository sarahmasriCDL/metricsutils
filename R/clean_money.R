#' Detect currency codes from text
#'
#' Identifies common currency indicators within a character vector and returns
#' standardized three-letter currency codes.
#'
#' This function searches for patterns such as "USD", "US$", "CAD", "C$", "EUR",
#' and "€" within each element of the input and maps them to ISO-like currency codes.
#'
#' @param x A character vector containing currency values or text.
#'
#' @return A character vector of the same length as `x`, containing detected
#' currency codes ("USD", "CAD", "EUR") or `NA` if no currency is identified.
#'
#' @examples
#' detect_currency(c("USD 100", "C$200", "€300", "400"))
#'
#' @export
detect_currency <- function(x) {
  x <- toupper(as.character(x))
  
  dplyr::case_when(
    stringr::str_detect(x, "USD|US\\$|\\bUS\\b") ~ "USD",
    stringr::str_detect(x, "CAD|C\\$|\\bCA\\b") ~ "CAD",
    stringr::str_detect(x, "EUR|€") ~ "EUR",
    TRUE ~ NA_character_
  )
}




#' Parse numeric amounts from messy text
#'
#' Extracts numeric values from character strings and applies multipliers for
#' shorthand notations such as "k" (thousands) and "m" (millions), as well as
#' full words like "thousand" and "million".
#'
#' The function removes commas, trims whitespace, and detects suffixes or words
#' to scale values appropriately.
#'
#' @param x A character vector containing numeric values with possible text,
#' formatting, or shorthand notation.
#'
#' @return A numeric vector representing the parsed values. Returns `NA` where
#' no numeric value can be extracted.
#'
#' @examples
#' parse_amount(c("1,200", "2.5k", "3M", "1.2 million", "7 thousand"))
#'
#' @export
parse_amount <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- stringr::str_replace_all(x, ",", "")
  
  detect_multiplier <- function(z) {
    dplyr::case_when(
      stringr::str_detect(z, "million|\\bm\\b|[0-9]m\\b") ~ 1e6,
      stringr::str_detect(z, "thousand|\\bk\\b|[0-9]k\\b") ~ 1e3,
      TRUE ~ 1
    )
  }
  
  parse_single <- function(x_single, default_multiplier = 1) {
    multiplier <- detect_multiplier(x_single)
    
    if (multiplier == 1) {
      multiplier <- default_multiplier
    }
    
    num <- stringr::str_extract(x_single, "[0-9]+\\.?[0-9]*")
    as.numeric(num) * multiplier
  }
  
  purrr::map_dbl(x, function(val) {
    if (is.na(val) || stringr::str_trim(val) == "") {
      return(NA_real_)
    }
    
    is_range <- stringr::str_detect(val, "[0-9].*-[^\\(]*[0-9]")
    
    if (is_range) {
      default_multiplier <- detect_multiplier(val)
      parts <- unlist(stringr::str_split(val, "-"))
      parts <- stringr::str_trim(parts)
      nums <- purrr::map_dbl(parts, parse_single, default_multiplier = default_multiplier)
      return(mean(nums, na.rm = TRUE))
    }
    
    parse_single(val)
  })
}



#' Extract outer monetary text
#'
#' Removes trailing parenthetical notes from a string, returning only the
#' outer portion of the text. This is useful when a total amount is provided
#' followed by a breakdown in parentheses.
#'
#' @param x A character vector containing monetary text.
#'
#' @return A character vector with any trailing parentheses and their contents removed.
#'
#' @examples
#' extract_outer_text("$1.75M CAD ($750k closed, $1M committed)")
#'
#' @export
extract_outer_text <- function(x) {
  stringr::str_trim(stringr::str_replace(x, "\\s*\\(.*\\)$", ""))
}



#' Detect presence of an outer amount
#'
#' Determines whether a string contains a numeric value outside of any
#' trailing parentheses. This is used to decide whether a total amount
#' is explicitly provided.
#'
#' @param x A character vector containing monetary text.
#'
#' @return A logical vector indicating whether each element contains
#' a numeric value outside parentheses.
#'
#' @examples
#' has_outer_amount("$1.75M CAD ($750k closed)")
#' has_outer_amount("($750k closed, $1M committed)")
#'
#' @export
has_outer_amount <- function(x) {
  has_parentheses <- stringr::str_detect(x, "\\(.*\\)")
  outer <- extract_outer_text(x)
  has_number_outer <- stringr::str_detect(outer, "[0-9]")
  
  has_parentheses & has_number_outer
}



#' Extract monetary amount chunks from text
#'
#' Identifies and extracts all substrings within a character vector that
#' resemble monetary values, including optional currency symbols, numeric
#' values, and magnitude indicators such as "k", "m", "thousand", or "million".
#'
#' @param x A character vector containing monetary text.
#'
#' @return A list of character vectors, where each element contains the
#' extracted monetary substrings for the corresponding input element.
#'
#' @examples
#' extract_amount_chunks("$750k CAD closed, $1M CAD committed")
#'
#' @export
extract_amount_chunks <- function(x) {
  stringr::str_extract_all(
    x,
    "(?i)(usd|cad|eur|us\\$|c\\$|\\$|€)?\\s*[0-9][0-9,]*(?:\\.[0-9]+)?\\s*(?:k|m|thousand|million)?\\s*(?:cad|usd|eur)?"
  )
}



#' Clean and standardize complex monetary values
#'
#' Converts messy monetary strings into numeric values in a target currency.
#' Handles currency detection, numeric parsing, shorthand multipliers
#' (e.g., "k", "M", "thousand", "million"), and conditional aggregation of
#' multiple amounts.
#'
#' The function follows a hierarchical parsing strategy:
#' - If an outer total amount is present (outside parentheses), it is used directly.
#' - If no outer total is present, multiple detected amounts are parsed and summed.
#' - Currency is detected from the text where possible, otherwise a fallback
#'   `currency` vector is used.
#' - All values are converted to the target currency using the provided rates.
#'
#' @param x A character vector containing monetary values.
#' @param currency Optional character vector specifying currency codes for each
#' element of `x`. Used when currency is not embedded in the text.
#' @param to Target currency code. Defaults to "CAD".
#' @param rates Named numeric vector of exchange rates relative to a base currency.
#' Must include all currencies present in `x` or `currency`.
#'
#' @return A numeric vector of monetary values converted to the target currency.
#'
#' @examples
#' x <- c(
#'   "$1.75M CAD ($750k CAD closed, $1M CAD committed)",
#'   "$250k USD (committed)",
#'   "$750k CAD closed, $1M CAD committed"
#' )
#' clean_money(x)
#'
#' @export
clean_money <- function(x, currency = NULL, to = "CAD",
                        rates = c(CAD = 1, USD = 1.37, EUR = 1.48)) {
  
  if (is.null(currency)) {
    currency <- rep(NA_character_, length(x))
  }
  
  clean_one <- function(value, fallback_currency) {
    value <- as.character(value)
    
    if (is.na(value) || stringr::str_trim(value) == "") {
      return(NA_real_)
    }
    
    if (has_outer_amount(value)) {
      text_to_parse <- extract_outer_text(value)
      amount <- parse_amount(text_to_parse)
      
      curr <- dplyr::coalesce(
        detect_currency(text_to_parse),
        toupper(as.character(fallback_currency)),
        "CAD"
      )
      
      return(amount * unname(rates[curr]) / unname(rates[to]))
    }
    
    if (stringr::str_detect(value, "[0-9][^,]*-[^,]*[0-9]")) {
      amount <- parse_amount(value)
      
      curr <- dplyr::coalesce(
        detect_currency(value),
        toupper(as.character(fallback_currency)),
        "CAD"
      )
      
      return(amount * unname(rates[curr]) / unname(rates[to]))
    }
    
    chunks <- unlist(extract_amount_chunks(value))
    chunks <- chunks[stringr::str_detect(chunks, "[0-9]")]
    
    if (length(chunks) == 0) {
      return(NA_real_)
    }
    
    currencies <- dplyr::coalesce(
      detect_currency(chunks),
      toupper(as.character(fallback_currency)),
      "CAD"
    )
    
    if (length(unique(currencies)) > 1) {
      warning("Multiple currencies detected in one cell: ", value)
    }
    
    amounts <- parse_amount(chunks)
    converted <- amounts * unname(rates[currencies]) / unname(rates[to])
    
    sum(converted, na.rm = TRUE)
  }
  
  purrr::map2_dbl(x, currency, clean_one)
}