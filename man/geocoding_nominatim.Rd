% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/System-geocoding.R
\name{geocoding_nominatim}
\alias{geocoding_nominatim}
\title{Geocoding data using Nominatim}
\usage{
geocoding_nominatim(address, threshold = 0.5)
}
\arguments{
\item{address}{address is free-form but it is best form your address like {street}, {city}, {county}, {state}, {postalcode}}

\item{threshold}{importance threshold to identify invalid geocoded observation}
}
\value{
address, lat, lng, city, county, zipcode, status, importance
}
\description{
Geocoding data using Nominatim
}
