library(varnastats)
context("Tests of function rmnegcorr")

testmatrix <- data.frame(
  c1 = c(5,0,0,0,1,1), 
  c2 = c(5,0,0,0,1,1), 
  c3 = c(5,1,7,0,0,2),
  c4 = c(5,6,7,0,0,0),
  c5 = c(5,3,2,0,0,3),
  c6 = c(0,6,1,0,0,0),
  c7 = c(0,1,1,1,0,0)
)
testmatrix <- varnastats::booleanize(testmatrix)
testcorrmatrix <- corrmat(testmatrix2, "chi2", chi2limit = 0.2, dim = 1)

test_that(
  "the output of rmnegcorr is a data.frame", 
  {
    expect_equal(
      is.data.frame(rmnegcorr(testmatrix, testmatrix, dim = 1, niv = 0.1)), 
      TRUE
    )
  }
)

test_that(
  "the output of rmnegcorr is a data.frame with the correct width and heigth", 
  {
    expect_equal(
      ncol(rmnegcorr(testmatrix, testmatrix, dim = 1, niv = 0.1)), 
      ncol(testmatrix2)
    )
    expect_equal(
      nrow(newcorrtable(rmnegcorr(testmatrix, testmatrix, dim = 1, niv = 0.1))), 
      ncol(testmatrix2)
    )
  }
)

test_that(
  "the different methods of corrmat are calculated correctly", 
  {
    expect_equal(
      corrmat(testmatrix2, "chi2", chi2limit = 0.2)[5,3], 
      1
    )
    expect_equal(
      round(corrmat(testmatrix2, "phi", chi2limit = 0.2)[5,3], 3), 
      0.625
    )
    expect_equal(
      round(corrmat(testmatrix2, "cc", chi2limit = 0.2)[5,3], 3), 
      0.53
    )
    expect_equal(
      round(corrmat(testmatrix2, "lambda", chi2limit = 0.2)[5,3], 3), 
      1
    )
  }
)

test_that(
  "the removal of negative relations in corrmat works",
  {
    expect_equal(
      corrmat(testmatrix2, "chi2", chi2limit = 0.2)[1,7], 
      1
    )
    expect_equal(
      corrmat(testmatrix2, "chi2", chi2limit = 0.2, rmnegniv = 0.1)[1,7], 
      0
    )
  }
)