% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module_Configuration_users.R
\name{userconfig_datatable}
\alias{userconfig_datatable}
\title{user configuration module - server function}
\usage{
userconfig_datatable(input, output, session, dM)
}
\arguments{
\item{input}{as required by Shiny modules}

\item{output}{as required by Shiny modules}

\item{session}{as required by Shiny modules}

\item{dM}{dMeasure R6 object. includes $UserConfigR,
$UserRestrictions, $location.list, $location_listR
and links to EMR and configuration db}
}
\value{
count - increments with each GUI edit of user configuration database
}
\description{
user configuration module - server function
}
