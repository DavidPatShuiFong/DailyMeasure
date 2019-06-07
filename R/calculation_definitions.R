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
