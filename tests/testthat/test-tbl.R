context('tests text output of build and describe methods')
data('freeny')

width_check <- function(tbl) {
	# returns TRUE if all length of rows on inputted string are equal
	# delimited by newline
  atomic_tbl <- tbl$text
	rows <- strsplit(atomic_tbl, '\n')
	row_widths <-unlist(lapply(rows, nchar))
	length(unique(row_widths[row_widths != 0])) == 1 # TRUE if all elements are equal
}

test_that('ensures that every row of the table has same width', {
	random_ys <- sample(c(1, 0), nrow(freeny), replace=T)
	freeny2 <- cbind(random_ys, freeny)
	mod1 <- lm(y ~ income.level, data = freeny)
	mod2 <- lm(y ~ lag.quarterly.revenue + income.level + market.potential +
		   price.index, data = freeny)
	mod3 <- glm(random_ys ~ market.potential + price.index, data = freeny2,
		    family = 'binomial')

	# Checks build function
	expect_true(width_check(build(mod1, silent=T)))
	expect_true(width_check(build(mod2, silent=T)))
	expect_true(width_check(build(mod3, silent=T)))

	# Checks describe function
	expect_true(width_check(describe(freeny, silent=T)))
	expect_true(width_check(describe(freeny2, silent=T)))
})
