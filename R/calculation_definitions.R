## 'helper' functions for calculation

calc_age <- function(birthDate, refDate = Sys.Date()) {
	# Calculate age at a given reference date
	# Create an interval between the date of birth and the enrollment date;
	# intervals are specific to the two dates. Periods give the actual length
	# of time between those dates, so convert to period and extract the year.
	# written by 'mmparker' https://gist.github.com/mmparker/7254445

  period <- lubridate::as.period(lubridate::interval(birthDate, refDate),
                                 unit = "year")
  period$year
}

calc_age_months <- function(birthDate, refDate = Sys.Date()) {
	# Calculate age at a given reference date, in months
	# Create an interval between the date of birth and the enrollment date;
	# intervals are specific to the two dates. Periods give the actual length
	# of time between those dates, so convert to period and extract the month.
	# based on code written by 'mmparker' https://gist.github.com/mmparker/7254445

  period <- lubridate::as.period(lubridate::interval(birthDate, refDate),
                                 unit = "month")
	period$month
}

hrmin <- function(t) {
	# converts seconds to a 'time' starting from midnight
	# t : value in seconds
	# returns 24-hour time of form '14:15' (hh:mm)
	td <- lubridate::seconds_to_period(t)
	sprintf('%02d:%02d', td@hour, td@minute)
}

# code for encoding/decoding. not 'very' secret
# requires libraries base64enc (partly for obfuscation)
# and sodium

#' Simple encoder
#'
#' Simple encode of text strings, will output a text string.
#' Uses sodium library and base64enc. Has some defaults, but
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
	return(base64enc::base64encode(
		sodium::data_encrypt(charToRaw(msg), key, nonce)))
}

#' Simple decoder
#'
#' Simple decoder of text strings, will output a text string.
#' Uses sodium library and base64enc. Has some defaults, but
#' will also take command-line arguments or read from environment.
#' Componaion function to simple_encode
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
		base64enc::base64decode(msg),key, nonce)
	))
}

