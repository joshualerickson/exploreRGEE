test_that("Checking SWIR2 and SWIR1 for Year Month", {

  huc <- exploreRGEE::huc

  ts <- get_landsat(huc, method = 'ts', startDate = '2019-01-01', endDate = '2020-12-31', c.low = 1, c.high = 12)

  ts_band <- ts %>% band(band = 'SWIR2', scale = 250, ggplot = F, temporal = 'year_month')

  expect_equal(nrow(ts_band), 48)

  expect_equal(ts_band[1,]$parameter, 'SWIR2_median')

  ts_band <- ts %>% band(band = 'SWIR1', scale = 250, ggplot = F, temporal = 'year_month')

  expect_equal(nrow(ts_band), 48)

  expect_equal(ts_band[1,]$parameter, 'SWIR1_median')

  ts <- get_landsat(huc, method = 'ts', startDate = '2019-01-01', endDate = '2020-12-31', c.low = 1, c.high = 12)

  ts_band <- ts %>% band(band = 'SWIR2', temporal_stat = 'mean', scale = 250, ggplot = F, temporal = 'year_month')

  expect_equal(ts_band[1,]$parameter, 'SWIR2_mean')

  ts_band <- ts %>% band(band = 'SWIR2', temporal_stat = 'mean', scale = 250, ggplot = F, temporal = 'year_month')

  expect_equal(ts_band[1,]$parameter, 'SWIR2_mean')

  ts_band <- ts %>% band(band = 'SWIR2', temporal_stat = 'sd', scale = 250, ggplot = F, temporal = 'year_month')

  expect_equal(ts_band[1,]$parameter, 'SWIR2_stdDev')

  ts_band <- ts %>% band(band = 'SWIR1', temporal_stat = 'sd', scale = 250, ggplot = F, temporal = 'year_month')

  expect_equal(ts_band[1,]$parameter, 'SWIR1_stdDev')


})
