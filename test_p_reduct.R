dat3 = cbind.data.frame( PR = sort( signif( runif(11,0,100), 3), decreasing = T)
                         , year = 2020:2030 )

dat2 = cbind.data.frame( value = sort( signif( runif(11,0,100), 3), decreasing = T)
                         , year = 2020:2030 )

dat1 = cbind.data.frame( PR = sort( signif( runif(11,0,100), 3), decreasing = T)
                         , years = 2020:2030 )


p_reduct = function(dat3 , baseyear, print = F ){
  #' calculates the percent reduction based on a baseyear
  #' @param baseyear a year against which to make comparisons
  #' @param dat a dataset with columns "PR" and "year"
  
  stopifnot(baseyear%in%dat3$year)
  
  stopifnot("PR"%in%colnames(dat3))

  stopifnot("year"%in%colnames(dat3))
  
    res = dat3 %>% 
    arrange(year) %>%  
    mutate(redu = ( PR-PR[year == baseyear]) / PR[year == baseyear]*100)
  
  if(print) print(res)
  
}

p_reduct( dat3, baseyear = 2020, print = T)

#####################################################################################################
library("testthat")

test_that("Test p_reduct: not included baseyear in the data",{
  expect_error(p_reduct(dat3, 2019))
})

test_that("Test p_reduct: not included PR column in the data",{
  expect_error(p_reduct(dat2, 2020))
})

test_that("Test p_reduct: not included year column in the data",{
  expect_error(p_reduct(dat1, 2020))
})

