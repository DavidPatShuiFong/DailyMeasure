## 'helper' functions for calculation

calc_age <- function(birthDate, refDate = Sys.Date()) {
  # Calculate age at a given reference date
  # Create an interval between the date of birth and the enrollment date;
  # note that arguments can be vectors, so needto use mapply

  period <- mapply(function(x, y)
    (length(seq.Date(min(x, y), max(x, y), by = "year")) - 1 ) *
      ifelse(y > x, 1, -1),
    # note that seq.Date can't handle 'negative' periods
    birthDate, refDate)

  return(period)
}

calc_age_months <- function(birthDate, refDate = Sys.Date()) {
  # Calculate age at a given reference date, in months
  # Create an interval between the date of birth and the enrollment date;
  # note that arguments can be vectors, so need to use mapply

  period <- mapply(function(x, y)
    (length(seq.Date(min(x, y), max(x, y), by = "month")) - 1) *
      ifelse(y > x, 1, -1),
    # note that seq.Date can't handle 'negative' periods
    birthDate, refDate)

  return(period)
}

hrmin <- function(t) {
  # converts seconds to a 'time' starting from midnight
  # t : value in seconds
  # returns 24-hour time of form '14:15' (hh:mm)

  format(as.POSIXct('1900-1-1') + t, '%H:%M')
}

# code for encoding/decoding. not 'very' secret
# requires libraries jsonlite (provides base64enc partly for obfuscation)
# and sodium

#' Simple encoder
#'
#' Simple encode of text strings, will output a text string.
#' Uses sodium library and base64_enc/dec from jsonlite. Has some defaults, but
#' will also take command-line arguments or read from environment
#'
#' @param msg the text to encode
#' @param key the cipher, which can be set manually, otherwise will read from env
#' @param nonce a non-secret unique data value used to randomize the cipher
#'
#' @return - the encrypted text
simple_encode <- function (msg, key = NULL, nonce = NULL) {
  if (is.null(nonce)) {
    # non-secret unique data 'nonce' used to randomize the cipher
    nonce <- sodium::hex2bin("89:63:73:bc:dc:eb:98:14:59:ce:17:4f:6e:0a:75:15:83:0c:36:00:f2:6e:f7:07")
    # the 24 bytes of hexadecimal digits created by paste0(random(24), collapse = ":")
  }
  if (is.null(key)) {
    if (nchar(Sys.getenv("DailyMeasure_Value2"))>0) {
      # if not set then the number of characters will be zero
      key <- Sys.getenv("DailyMeasure_value2")
      # this can be set in .Renviron
    } else {
      key <- "noncenonce"
    }
  }
  key <- sodium::hash(charToRaw(key))
  return(jsonlite::base64_enc(
    sodium::data_encrypt(charToRaw(msg), key, nonce)))
}

#' Simple decoder
#'
#' Simple decoder of text strings, will output a text string.
#' Uses sodium library and base64_enc/dec from jsonlite. Has some defaults, but
#' will also take command-line arguments or read from environment.
#' Companion function to simple_encode
#'
#' @param msg the text to decode
#' @param key the cipher, which can be set manually, otherwise will read from env
#' @param nonce a non-secret unique data value used to randomize the cipher
#'
#' @return - the encrypted text
simple_decode <- function(msg, key = NULL, nonce = NULL) {
  if (is.null(nonce)) {
    # non-secret unique data 'nonce' used to randomize the cipher
    nonce <- sodium::hex2bin("89:63:73:bc:dc:eb:98:14:59:ce:17:4f:6e:0a:75:15:83:0c:36:00:f2:6e:f7:07")
    # the 24 bytes of hexadecimal digits created by paste0(random(24), collapse = ":")
  }
  if (is.null(key)) {
    if (nchar(Sys.getenv("DailyMeasure_Value2"))>0) {
      # if not set then the number of characters will be zero
      key <- Sys.getenv("DailyMeasure_value2")
      # this can be set in .Renviron
      # or with Sys.setenv(DailyMeasure_value2="password")
    } else {
      key <- "noncenonce"
    }
  }
  key <- sodium::hash(charToRaw(key))
  return(rawToChar(sodium::data_decrypt(
    jsonlite::base64_dec(msg),key, nonce)
  ))
}

#' Simple tagger
#'
#' Simple tagger of text strings, will output a text string.
#' Uses sodium library and base64enc. Has some defaults, but
#' will also take command-line arguments or read from environment.
#'
#' @param msg the text to decode
#' @param key the cipher, which can be set manually, otherwise will read from env
#'
#' @return - the hash
simple_tag <- function(msg, key = NULL) {
  if (is.null(key)) {
    if (nchar(Sys.getenv("DailyMeasure_Value3"))>0) {
      # if not set then the number of characters will be zero
      key <- Sys.getenv("DailyMeasure_value3")
      # this can be set in .Renviron
      # or with Sys.setenv(DailyMeasure_value2="password")
    } else {
      key <- "noncenonce"
    }
  }
  key <- sodium::hash(charToRaw(key))
  msg <- serialize(msg, NULL)
  tag <- jsonlite::base64_enc(sodium::data_tag(msg, key))

  return(tag)
}

#' Simple tag comparison
#'
#' Simple tagger of text strings, will output a text string.
#' Uses sodium library and base64enc. Has some defaults, but
#' will also take command-line arguments or read from environment.
#'
#' @param msg the text to check
#' @param tag the tagged message (base64 encoded)
#' @param key the cipher, which can be set manually, otherwise will read from env
#'
#' @return - TRUE if matching, FALSE otherwise
simple_tag_compare <- function(msg, tag, key = NULL) {
  if (is.null(key)) {
    if (nchar(Sys.getenv("DailyMeasure_Value3"))>0) {
      # if not set then the number of characters will be zero
      key <- Sys.getenv("DailyMeasure_value3")
      # this can be set in .Renviron
      # or with Sys.setenv(DailyMeasure_value2="password")
    } else {
      key <- "noncenonce"
    }
  }
  key <- sodium::hash(charToRaw(key))
  msg <- serialize(msg, NULL)
  newtag <- sodium::data_tag(msg, key)
  oldtag <- jsonlite::base64_dec(tag)

  if (newtag == oldtag) {
    result = TRUE
  } else {
    result = FALSE
  }

  return(newtag == oldtag)
}

#' setPassword
#'
#' sets password for user
#' Adjusts both the 'in-memory' data (UserConfig)
#' and the UserConfig database
#'
#' @param newpassword the new password
#' @param UserConfig reactive, the User Configuration table
#' @param LoggedInUser reactive, the current user
#' @param config_db R6 object, access to the user configuration database
#'
#' @return nothing
#'
setPassword <- function(newpassword, UserConfig, LoggedInUser, config_db) {
  # set the password for the user

  newpassword <- simple_tag(newpassword)
  # tagging (hash) defined in calculation_definitions

  newUserConfig <-
    UserConfig() %>%
    mutate(Password =
             replace(Password,
                     LoggedInUser()$Fullname == Fullname,
                     newpassword))
  UserConfig(newUserConfig) # replace password with empty string

  query <- "UPDATE Users SET Password = ? WHERE id = ?"
  # write to configuration database
  data_for_sql <- list(newpassword, LoggedInUser()$id[[1]])

  config_db$dbSendQuery(query, data_for_sql)
  # if the connection is a pool, can't send write query (a statement) directly
  # so use the object's method
}
