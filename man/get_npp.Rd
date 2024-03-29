% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.R
\name{get_npp}
\alias{get_npp}
\title{Get Net Annual NPP (CONUS)}
\usage{
get_npp(
  aoi,
  method = "ld_NPP",
  param = NULL,
  stat = "median",
  cloud_mask = TRUE,
  startDate = "1986-01-01",
  endDate = "2019-01-01",
  mask = FALSE,
  m.low = NULL,
  m.high = NULL,
  c.low = 1,
  c.high = 12
)
}
\arguments{
\item{aoi}{A sf object indicating the extent of the geom.}

\item{method}{A \code{character} indicating what method to use, e.g. 'ld_NPP', 'modis_NPP', 'terra', 'aqua'.}

\item{param}{A \code{character} indicating what band to visualize, e.g. 'annualNPP', 'Npp', etc.}

\item{stat}{A \code{character} indicating what to reduce the imageCollection by, e.g. 'median' (default), 'mean',  'max', 'min', 'sum', 'stdDev', 'first'.}

\item{cloud_mask}{\code{logical} whether to mask out certain cloud artifacts. TRUE (default).}

\item{startDate}{\code{character} format date, e.g. "2018-10-23"}

\item{endDate}{\code{character} format date, e.g. "2018-10-23"}

\item{mask}{\code{logical} whether to mask out certain ranges}

\item{m.low}{\code{numeric} low value for mask, e.g. greater than 'm.low'}

\item{m.high}{\code{numeric} high value for mask, e.g. less than 'm.high'}

\item{c.low}{\code{numeric} lower month value for calendar range}

\item{c.high}{\code{numeric} higher month value for calendar range}
}
\value{
A list of Earth Engine Objects and arguments.
}
\description{
This function gets Net Primary Productivity products for selected time frames. Cloud masking will be at 50\% if TRUE; however, for 'ld_NPP' 255 will be masked out.
}
\details{
The methods currently available:
\itemize{
\item  \strong{ld_NPP}: UMT/NTSG/v2/LANDSAT/NPP; 1986-01-01 - 2019-01-01.
\item  \strong{modis_NPP}: UMT/NTSG/v2/MODIS/NPP; 2001-01-01 - 2019-01-01.
\item \strong{terra}: MODIS/006/MOD17A3HGF; 2001-01-01 - 2019-01-01.
\item \strong{aqua}: MODIS/006/MYD17A3HGF; 2002-01-01 - 2019-01-01.
}
}
\examples{
\dontrun{
# Load Libraries

library(rgee)
ee_Initialize()
library(exploreRGEE)

# Bring in data
huc <- exploreRGEE::huc

npp <- get_npp(huc, method = 'ld_NPP', param = 'annualNPP', startDate = '2014-01-01',
                  endDate = '2018-12-31')


}
}
