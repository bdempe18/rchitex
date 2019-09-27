test_that("Cohesion test", {
  library(rchitex)
  data("freeny")
  mod <- lm(y ~ lag.quarterly.revenue + price.index + income.level + market.potential, data = freeny)
  mod2 <- lm(y ~ lag.quarterly.revenue + price.index + income.level, data = freeny)
  d <- list('price.index' = 'Price index', 'income.level' = 'Income level', 'market.potential' = 'Market potential','lag.quarterly.revenue' = 'Lagged rev')

  #build(mod, mod2,silent=F, rse=T, md=TRUE, dep_names = d)
})
