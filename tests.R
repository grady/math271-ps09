test_that("<5>Solution file exists and knits",{
  expect_true(file.exists("solution.Rmd"))
  suppressWarnings(expect_error(knitr::knit("solution.Rmd", quiet = TRUE, envir=globalenv()), NA))
})

test_that("<1> vole_trapping imported", {
  expect_error({
    expect_s3_class(vole_trapping, "data.frame")
    expect_equal(dim(vole_trapping), c(56,4))  
  }, NA)
})

test_that("<1> vole_main model", {
  expect_error({
    expect_s3_class(vole_main, "lm")
    expect_equal(coef(vole_main), c("(Intercept)" = 15.4640264081856, 
                                    veg = 0.259068927923567, 
                                    soilmoist = 9.10029798390719))
  }, NA)
})

test_that("<1> vole_inter model", {
  expect_error({
    expect_s3_class(vole_inter, "lm")
    expect_equal(coef(vole_inter), c("(Intercept)" = 18.0027311211252,
                                     veg = 0.205380308616687,
                                     soilmoist = 4.92951146603061,
                                     "veg:soilmoist" = 0.0891136507318685 ))
  }, NA)
})


test_that("<1> mtl_main model", {
  expect_error({
    expect_s3_class(mtl_main, "lm")
    expect_equal(coef(mtl_main), c("(Intercept)" = 2.42318453357595, 
                                   age = 0.00434968529341886, 
                                   sitting = -0.0209757289688585))
  }, NA)
})


test_that("<1> mtl_inter model", {
  expect_error({
    expect_s3_class(mtl_inter, "lm")
    expect_equal(coef(mtl_inter),c("(Intercept)" = 3.25644016726678, 
                                   age = -0.00910611578654424, 
                                   sitting = -0.143325195783933, 
                                   "age:sitting" = 0.00200014792612012 ) )
  }, NA)
})
