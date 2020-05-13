context("AnalyseRecentLine")

test_that("significantStatus", {
  d <- GenFakeDataAnalysis()
  res <- QuasipoissonTrainPredictData(
    datasetTrain = d,
    datasetPredict = d,
    isDaily = T
  )
  res <- DetermineStatus(res)
  significantByThreshold <- res[n > n_baseline_thresholdu0]
  significantByConfidenceIntervals <- res[n_status != "normal"]
  testthat::expect_equal(significantByThreshold, significantByConfidenceIntervals)
})
