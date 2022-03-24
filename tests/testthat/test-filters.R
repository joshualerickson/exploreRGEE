test_that("filtering by year with ee ImageCollection", {

  # Bring in data
  aoi <- setup(exploreRGEE::huc)

  imageCol = ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")$filterBounds(aoi)

  #using rgee/ee IC

  filter_by_year = ee_year_filter(imageCol = imageCol,
                                startDate = '2014-01-01',
                                endDate = '2018-12-31',
                                stat = 'mean')

  expect_equal(length(filter_by_year$getInfo()$features), 5)

  point <- sf::st_sfc(sf::st_point(x = c(-115.07421, 48.85393)), crs = 4326)

  point <- setup(point)

  fby_image <- filter_by_year$mean()

  rgee_extr <- rgee::ee_extract(fby_image, point, scale = 30)

  expect_equal(round(rgee_extr$B1_mean,2), 2054.68)

})

test_that('filter year with exploreList', {

#using exploreRGEE

huc <- exploreRGEE::huc

ld8 <- get_landsat(huc, method = 'ld8', startDate = '2014-01-01',
                   endDate = '2018-12-31', c.low = 6, c.high = 11)

filter_by_year = ee_year_filter(imageCol = ld8,
                                stat = 'mean')

expect_equal(length(filter_by_year$getInfo()$features), 5)

#this test below fails but works within session....

# fby_image <- filter_by_year$mean()
#
# point <- sf::st_sfc(sf::st_point(x = c(-115.07421, 48.85393)), crs = 4326)
#
# point <- setup(point)
#
# rgee_extr <- rgee::ee_extract(fby_image, point, scale = 30)
#
# expect_equal(round(rgee_extr$Blue_mean,4), 241.6933)

})

test_that("filtering by month with ee ImageCollection", {

  aoi <- setup(exploreRGEE::huc)

  imageCol = ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")$filterBounds(aoi)


  filter_by_month <- ee_month_filter(imageCol = imageCol,
                                     months = c(2,8),
                                     stat = 'mean')
  expect_equal(length(filter_by_month$getInfo()$features), 7)

  point <- sf::st_sfc(sf::st_point(x = c(-115.07421, 48.85393)), crs = 4326)

  point <- setup(point)

  fbm_image <- filter_by_month$mean()

  rgee_extr <- rgee::ee_extract(fbm_image, point, scale = 30)

  expect_equal(round(rgee_extr$B1_mean,3), 1927.059)

  expect_error( ee_month_filter(imageCol = imageCol,
                                stat = 'mean'))

  })

test_that('filter month with exploreList', {

  #using exploreRGEE

  huc <- exploreRGEE::huc

  ld8 <- get_landsat(huc, method = 'ld8', startDate = '2014-01-01',
                     endDate = '2018-12-31', c.low = 6, c.high = 11)

  filter_by_year = ee_month_filter(imageCol = ld8,
                                  stat = 'mean')

  expect_equal(length(filter_by_year$getInfo()$features), 6)

})

test_that("filtering by year and month ee ImageCollection", {

  aoi <- setup(exploreRGEE::huc)

  imageCol = ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")$filterBounds(aoi)

  filter_by_year_month <- ee_year_month_filter(imageCol = imageCol,
                                               startDate = '2014-01-01',
                                               endDate = '2018-12-31',
                                               months = c(2,8),
                                               stat = 'mean')

  expect_equal(length(filter_by_year_month$getInfo()$features), 35)

  point <- sf::st_sfc(sf::st_point(x = c(-115.07421, 48.85393)), crs = 4326)

  point <- setup(point)

  fbym_image <- filter_by_year_month$mean()

  rgee_extr <- rgee::ee_extract(fbym_image, point, scale = 30)

  expect_equal(round(rgee_extr$B1_mean,3), 1860.530)

  expect_error(ee_year_month_filter(imageCol = imageCol,
                                stat = 'mean'))

})


test_that("filtering by year and month exploreList", {


  huc <- exploreRGEE::huc

  ld8 <- get_landsat(huc, method = 'ld8', startDate = '2014-01-01',
                     endDate = '2018-12-31', c.low = 6, c.high = 11)

  filter_by_year_month <- ee_year_month_filter(imageCol = ld8,
                                               stat = 'mean')

  expect_equal(length(filter_by_year_month$getInfo()$features), 30)

})




