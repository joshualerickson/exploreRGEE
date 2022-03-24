
test_that("timeseries with ee ImageCollection", {

  aoi <- exploreRGEE:::setup(exploreRGEE::huc)

  imageCol = ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")$filterBounds(aoi)

  #yearly
  # 3 years, 12 bands, 2 polygons/features

  ld8_band <- ee_timeseries(imageCol, scale = 250,
                            geom = huc,
                            startDate = '2016-01-01',
                            endDate = '2018-12-31',
                            temporal = 'yearly',
                            months = c(5,9))

  expect_equal(nrow(ld8_band), 72)

  expect_equal(ld8_band[1,]$date, as.Date('2016-01-01'))

  #monthly
  # 5 months, 12 bands, 2 polygons/features

  ld8_band <- ee_timeseries(imageCol, scale = 250, geom = huc, startDate = '2016-01-01',
                            endDate = '2018-12-31', temporal = 'monthly',months = c(5,9))

  expect_equal(nrow(ld8_band), 120)

  expect_equal(ld8_band[1,]$date, as.Date('0001-05-01'))

  #year_month
  # 2 months, 3 years, 12 bands, 2 polygons/features
  ld8_band <- ee_timeseries(imageCol, scale = 250, geom = huc, startDate = '2016-01-01',
                            endDate = '2018-12-31', temporal = 'year_month',months = c(8,9))

  expect_equal(nrow(ld8_band), 144)

  expect_equal(ld8_band[1,]$date, as.Date('2016-08-01'))

  expect_equal(ld8_band[1,]$parameter, 'B10_mean')

  #all
  # all data at 2 months, 1 years, 12 bands, 2 polygons/features
  ld8_band <- ee_timeseries(imageCol, scale = 250,
                            geom = huc, startDate = '2018-01-01',
                            endDate = '2018-12-31', temporal = 'all',months = c(8,9))

  expect_equal(nrow(ld8_band), 4170)

  expect_error(ld8_band[1,]$date, NA)

  expect_equal(ld8_band[1,]$parameter, 'B10')

})

test_that("timeseries with ee exploreList", {

  #yearly
  huc <- exploreRGEE::huc

  ld8 <- get_landsat(huc, method = 'ld8', startDate = '2014-01-01',
                     endDate = '2018-12-31', c.low = 6, c.high = 11)

  ld8_band <- ee_timeseries(ld8, scale = 250, temporal = 'yearly')

  expect_equal(nrow(ld8_band), 100)

  expect_equal(ld8_band[1,]$date, as.Date('2014-01-01'))

  expect_equal(ld8_band[1,]$parameter, 'Blue_mean')

  #monthly

  ld8_band <- ee_timeseries(ld8, scale = 250, temporal = 'monthly')

  expect_equal(nrow(ld8_band), 120)

  expect_equal(ld8_band[1,]$date, as.Date('0001-06-01'))

  expect_equal(ld8_band[1,]$parameter, 'Blue_mean')

  #year_month
  ld8 <- get_landsat(huc, method = 'ld8', startDate = '2017-01-01',
                     endDate = '2018-12-31', c.low = 6, c.high = 11)

  ld8_band <- ee_timeseries(ld8, scale = 250, temporal = 'year_month')

  expect_equal(nrow(ld8_band), 220)

  expect_equal(ld8_band[1,]$date, as.Date('2017-06-01'))

  expect_equal(ld8_band[1,]$parameter, 'Blue_mean')


  #all
  ld8 <- get_landsat(huc, method = 'ld8', startDate = '2018-01-01',
                     endDate = '2018-12-31', c.low = 10, c.high = 11)

  ld8_band <- ee_timeseries(ld8, scale = 250, temporal = 'all')

  expect_equal(nrow(ld8_band), 60)

  expect_equal(ld8_band[1,]$date, as.Date('2018-10-09'))

  expect_equal(ld8_band[1,]$parameter, 'Blue')
})
