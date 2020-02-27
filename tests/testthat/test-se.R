context('Testing RSE and SE S3')
test_that("ensures that number of obs correctly calculated for RSE and adj.SE", {
	data1 <- data.frame(y = rnorm(10, 10, 3), x = rnorm(10, 15, 2))
	data2 <- data.frame(y = rnorm(100, 10, 3), x = rnorm(100, 15, 2))
	data3 <- data.frame(y = sample(c(0,1), 20, replace=TRUE),
			    x = rnorm(20, 15, 3))

	mod1 <- lm(y ~ x, data = data1)
	mod2 <- lm(y ~ x, data = data2)
	mod3 <- glm(y ~ x, data = data3, family = "binomial")

	# overwriting nobs method should not interfere with stats S3 object
	expect_equal(nobs(mod1), nrow(data1))
	expect_equal(nobs(mod3), nrow(data3))

	# RSE object should correctly count number of obs
	expect_equal(nobs(rse(mod1)), nobs(mod1))
	expect_equal(nobs(rse(mod2)), nobs(mod2))
	expect_equal(nobs(rse(mod3)), nobs(mod3))

	# adj.se with func object should correctly count number of obs
	func <- function(x) x + 2
	expect_equal(nobs(adj_se(mod1, func)), nobs(mod1))
	expect_equal(nobs(adj_se(mod2, func)), nobs(mod2))
	expect_equal(nobs(adj_se(mod3, func)), nobs(mod3))

	# adj.se with list of se should correctl count number of obs
	se1 <- rep(8, nrow(data1))
	se2 <- rep(7, nrow(data2))
	se3 <- rep(6, nrow(data3))

	expect_equal(nobs(adj_se(mod1, se1)), nobs(mod1))
	expect_equal(nobs(adj_se(mod2, se2)), nobs(mod2))
	expect_equal(nobs(adj_se(mod3, se3)), nobs(mod3))
})


test_that("ensures that primative functions of RSE and adj.se object do not throw error", {
	data1 <- data.frame(y = rnorm(50, 20, 5), x = rnorm(50, 10, 7),
	                    random_y = sample(c(0,1), 5, replace=TRUE))
	func <- function(x) runif(1)
	se <- rep(5, nrow(data1))
	mod1 <- lm(y ~ x, data=data1)
	mod2 <- glm(random_y ~ x, data=data1, family="binomial")

	# primative rse functions should not throw errors
	expect_error(summary(rse(mod1)), NA)
	expect_error(summary(rse(mod2)), NA)

	expect_error(print(rse(mod1)), NA)
	expect_error(print(rse(mod2)), NA)

	# primative adj.se with func should not throw errors
	expect_error(summary(adj_se(mod1, func)), NA)
	expect_error(summary(adj_se(mod2, func)), NA)

	expect_error(print(adj_se(mod1, func)), NA)
	expect_error(print(adj_se(mod2, func)), NA)

	# primative adj.se with se list should not throw errors
	expect_error(summary(adj_se(mod1, se)), NA)
	expect_error(summary(adj_se(mod2, se)), NA)

	expect_error(print(adj_se(mod1, se)), NA)
	expect_error(print(adj_se(mod2, se)), NA)
})
