
<!-- README.md is generated from README.Rmd. Please edit that file -->

# exploreRGEE

<!-- badges: start -->

<!-- badges: end -->

The goal of exploreRGEE is to explore Google Earth Engine (GEE) in the
Rstudio IDE. This package uses rgee and other spatial packages (sf,
leaflet) to explore Google Earth Engine collections in a exploratory
data analysis manner relatively quickly while using R. This package is
meant to be exploratory but also provides ways to get GEE pixel data
that can be used in resource management type analysis. The advantage of
doing this in R is that some people are not familiar with JavaScript or
Python but versed in R and this provides easy access to commonly used
workflows in resource management.

## Installation

Development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("joshualerickson/exploreRGEE")

# strongly suggest
remotes::install_github("mikejohnson51/AOI")
```

## Important Note

The backbone of this package relies on
[rgee](https://github.com/r-spatial/rgee) and a GEE account (it’s free),
so for more information go to the rgee
[website](https://github.com/r-spatial/rgee) or the GEE
[website](https://developers.google.com/earth-engine/) for more details
on how to get set-up and also tons of functionality using the
[rgee](https://github.com/r-spatial/rgee) package.

## Example

This is a basic example which shows you how to solve a common problem:
visualizing net annual NPP data and creating a time series.

``` r
library(exploreRGEE)
library(rgee)
library(AOI)

#this initializes the API
ee_Initialize()
#> -- rgee 1.0.6 ---------------------------------------------------- earthengine-api 0.1.235 -- 
#>  v email: not_defined
#>  v Initializing Google Earth Engine: v Initializing Google Earth Engine:  DONE!
#>  v Earth Engine user: users/joshualerickson 
#> ---------------------------------------------------------------------------------------------
```

Let’s look at Northwestern Montana and the median NPP over a 30 year
span (1986 to 2016). To do this, we’ll call the `viz_NPP()`. This
function has numerous paramaters which we’ll go over in this workflow.
It’s important to note that the workflow below can be applied to other
functions as well: `viz_PRISM(), viz_Landsat(), viz_Sent2(),
viz_Terrain()`.

``` r

lincoln_county <- AOI::aoi_get(state = "Montana", county = "Lincoln")

viz_NPP(lincoln_county, startDate = '1986-01-01', endDate = '2016-01-01')
```

<br>

Also, as a default six stats will be available as **below**.  
<br>
<img src="D:/R_folder/R/Random/exploreRGEE/npp_print.png" width="20%" />
<img src="D:/R_folder/R/Random/exploreRGEE/npp.png" width="100%" />

As you can see our median is 5,215 of annual \(\frac{kg*C}{m^2}\). We
can then mask out all the values below this threshold to see where these
areas are at. This involves using the arguments `window, w.low, w.high`
as below.

``` r

viz_NPP(lincoln_county, startDate = '1986-01-01', endDate = '2016-01-01', window = TRUE, w.low = 5215, w.high = 8356)
```

<img src="D:/R_folder/R/Random/exploreRGEE/npp2.png" width="100%" />

Ok, now that we’ve got an idea of the area let’s see what it would look
like if we reduced this area of interest by watershed. To do that we’ll
use the `rr_NPP()` function. This function uses the USGS Watershed
Boundary Dataset of Basins to reduce regions (wb) or a provided sf
object to common statistics, e.g. mean, max, min, median, stdDev and
sum. By default, it provides a leaflet map with popups of the
statistics, colored numerically by mean value and prints out the first
10 of the sf object. If you want to save as an **sf** object just use
the left `<-` or right `->` operator.

``` r
rr_NPP(lincoln_county, wb = "HUC08", startDate = '1986-01-01', endDate = '2016-01-01')
```

<br>
<img src="D:/R_folder/R/Random/exploreRGEE/npp3.png" width="100%" />

Finally, maybe we want to see how these regions have looked over time
and not as static statistics? Well, we can use the `band_NPP()` function
to do this. This function takes each year of each region and saves it as
a data.frame with a side effect plot. You also have the option to save
the plot, which is good if you want to compare different time series.
Again, we are using the `wb` argument but if you wanted to provide a sf
object (‘POINT’,‘POLYGON’, etc) then go on ahead just **remember** to
use the `id` argument so that it knows what to give each reduction to.
**CAUTION** this is an expensive function and can take awhile (reason
why we are only showing ‘2000’ to ‘2019’) if you are trying to get a
time series for let’s say landsat or daily PRISM or 30 year NPP. In
future developments I’ll add the `bestEffort = TRUE' argument
and`maxPixels`as well but for now just up the scale like below or just
split your query into chunks and`rbind\` after processing like below.

``` r
npp1 <- band_NPP(lincoln_county, wb = "HUC08", startDate = '2010-01-01', endDate = '2019-01-01', scale = 800)
npp2 <- band_NPP(lincoln_county, wb = "HUC08", startDate = '1999-01-01', endDate = '2009-01-01', scale = 800)
npp3 <- band_NPP(lincoln_county, wb = "HUC08", startDate = '1990-01-01', endDate = '1998-01-01', scale = 800)
npp4 <- band_NPP(lincoln_county, wb = "HUC08", startDate = '1986-01-01', endDate = '1989-01-01', scale = 800)

npp_ts <- rbind(npp1, npp2, npp3, npp4)

npp_ts %>% ggplot(aes(Date, value, color = ID)) +
      geom_line() +
      geom_smooth(alpha = 0.3) +
      geom_point() +
      theme_bw() +
      labs(title = paste0("Annnual NPP (Kg*C/m^2*yr)", " values for date range: "),
           subtitle = paste0("Years: 1986 - 2019"),
           y = paste0("Annnual NPP (Kg*C/m^2)", ' values')) +
      facet_wrap(~ID)
```

<br>
<img src="D:/R_folder/R/Random/exploreRGEE/npp4.png" width="100%" />
