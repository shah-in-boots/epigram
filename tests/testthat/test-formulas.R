# Basics ----

test_that("fmls can be initialized and formatted", {

	# Empty class
	expect_length(fmls(), 0)

})

test_that("fmls-fmls can be combined", {

	# Vec ptype2 and cast testing
	f1 <- output ~ input + modifier
	f2 <- output ~ .x(input) + modifier
	f3 <- output ~ .x(input) + log(modifier) + log(variable) + another

	# Simple
	x <- fmls(f1)
	y <- fmls(f2)
	expect_s3_class(vec_ptype2(x, y), "fmls")

	# `input` is a plain predictor in one family and the exposure of the
	# other; both definitions survive the combine, each formula still
	# reading its own family's definition
	expect_no_message(f <- vec_c(x, y))
	expect_length(f, 3)
	expect_equal(nrow(f), 2)
	expect_equal(
		with(vec_data(key_terms(f)), role[term == "input"]),
		c("predictor", "exposure")
	)

	# The rows stay distinct: each formula's membership cell points at its
	# own definition of `input` by ordinal within the term table
	expect_equal(vec_data(f)$input, c(1L, 2L))

	# Flipped, the exposure definition is declared (and so listed) first
	expect_no_message(f <- vec_c(y, x))
	expect_equal(
		with(vec_data(key_terms(f)), role[term == "input"]),
		c("exposure", "predictor")
	)
	expect_length(key_terms(f), 4)

	# No conflict, no message
	expect_no_message(f <- c(fmls(a ~ b), fmls(x ~ y)))
	expect_equal(nrow(f), 2)

	# More complex version
	x <- fmls(f1)
	y <- fmls(f3)
	f <- vec_c(x, y) # "weaker" version is first, richer is second

	expect_length(f, 6)
	expect_equal(nrow(f), 2)
	expect_s3_class(f, "fmls")
	expect_true("exposure" %in% vec_data(key_terms(f))$role)

	f <- vec_c(y, x) # Flip the order so the richer formula is first
	expect_equal(sum(f[1, ] >= 1, na.rm = TRUE), 5)
	expect_equal(sum(f[2, ] >= 1, na.rm = TRUE), 3)
	expect_s3_class(f, "fmls")
	expect_true("exposure" %in% vec_data(key_terms(f))$role)

	# Print output
	expect_output(print(format(x)), "output|input|modifier")

})

test_that("fmls and formulas can be interchanged", {

	# Forward: fmls to a list of base formulas
	f <- fmls(mpg ~ wt + hp + .s(am))
	fl <- formula(f)
	expect_type(fl, "list")
	expect_s3_class(fl[[1]], "formula")
	expect_equal(deparse1(fl[[1]]), "mpg ~ wt + hp")

	# Backward: base formulas to fmls
	f2 <- fmls(fl[[1]])
	expect_s3_class(f2, "fmls")
	expect_equal(deparse1(formula(f2)[[1]]), "mpg ~ wt + hp")

	# Casting also round-trips
	f3 <- vec_cast(mpg ~ wt + hp, fmls())
	expect_s3_class(f3, "fmls")
	expect_equal(as.character(f3), "mpg ~ wt + hp")

	# Expanded families give one formula per row
	f4 <- fmls(mpg ~ wt + hp, pattern = "parallel")
	fl4 <- formula(f4)
	expect_length(fl4, 2)
	expect_equal(
		sort(sapply(fl4, deparse1)),
		c("mpg ~ hp", "mpg ~ wt")
	)

})

test_that("tm can convert to fmls objects", {

	t <- tm(.o(good) ~ .x(bad) + ugly)
	f <- fmls(t)
	expect_equal(f, fmls(t))

})

test_that("fmls can be coerced to character class", {

	# Characters
	x <- fmls(witch ~ wicked + west)
	expect_type(vec_c(x, "test"), "character")
})


test_that("patterns can be included into formula", {

	f1 <- fmls(witch ~ wicked + west, pattern = "parallel")
	expect_equal(nrow(f1), 2)

	f2 <- fmls(witch ~ wicked + west + green, pattern = "sequential")
	expect_equal(nrow(f2), 3)

	f3 <- fmls(witch ~ .x(wicked) + west + green, pattern = "sequential")
	expect_equal(nrow(f3), 3)

	# Printing for sequential works; the stratum holds a matrix column of its
	# own alongside the four modeling terms
	x <- mpg ~ wt + hp + cyl + .s(am)
	pattern <- "sequential"
	f <- fmls(x, pattern = pattern)
	expect_equal(nrow(f), 3)
	expect_length(f, 5)

})

test_that("interaction terms can be included", {

	x <- witch ~ wicked + green + west + wicked:west
	expect_equal(as.character(fmls(x)), deparse1(x))

	expect_message(f <- fmls(witch ~ .x(wicked) + green + .i(west)))
	expect_equal(formula(f, env = .GlobalEnv)[[1]], x, ignore_attr = TRUE)

	## By Patterns

	# Parallel
	# Should produce only one formulas as green and witch:green are needed
	# This is a grouping issue from `tm()` function
	x <- wicked ~ .x(witch) + .i(green)
	f <- fmls(x, pattern = "parallel")
	expect_equal(nrow(f), 1)

	# Sequential should produce 3 formulas in this case?
	x <- wicked ~ .x(witch) + west + .i(green)
	f <- fmls(x, pattern = "sequential")
	expect_equal(nrow(f), 3)

	# Sequential should produce 3 formulas in this case as well
	x <- wicked ~ .x(witch) + .i(west) + green
	f <- fmls(x, pattern = "sequential")
	expect_equal(nrow(f), 3)
	f2 <- wicked ~ witch + west + witch:west # Green should not yet have appeared
	expect_equal(unname(as.character(f[2, ])), deparse1(f2))
})

test_that("strata terms can be kept within the formula appropriately", {

	x <- tm(mpg ~ wt + hp + .s(am))
	f <- fmls(x)
	expect_length(key_terms(f), 4)

	x <- mpg ~ wt + hp + .s(am)
	f <- fmls(x)
	expect_length(key_terms(f), 4)
	# The stratum is a member of the formula matrix like any other term (this
	# is what scopes it to its own family when families combine)
	expect_equal(ncol(f), 4)
	expect_equal(nrow(f), 1)

})

test_that("strata stay scoped to the family that declared them", {

	# A stratum from one family must not leak into a combined family that
	# never asked for it (`fit()` would silently stratify the second family)
	f <- suppressMessages(
		c(fmls(mpg ~ .x(wt) + .s(am)), fmls(qsec ~ .x(hp)))
	)

	tms <- formulas_to_terms(f)
	strataOf <- lapply(tms, function(t) {
		with(vec_proxy(t), term[role == "strata"])
	})
	expect_equal(strataOf[[1]], "am")
	expect_length(strataOf[[2]], 0)

	# And the fitting plan agrees: only the mpg formula expands by stratum
	plan <- plan_fit(f, data = mtcars)
	expect_equal(
		plan$strata_variable[plan$formula_index == 1],
		c("am", "am")
	)
	expect_true(all(is.na(plan$strata_variable[plan$formula_index == 2])))

})


# Special terms ----

test_that("terms with multiple roles can be converted to formulas", {

	x <- tm(green + white ~ .x(wicked) + .x(good) + witch + fairy + magic + .m(west))
	f <- fmls(x)
	expect_equal(nrow(f), 10)

	f <- fmls(green ~ .x(wicked) + witch + west, pattern = "sequential")
	expect_equal(nrow(f), 3)

	f <- fmls(green ~ .x(wicked) + witch + west, pattern = "direct")
	expect_equal(nrow(f), 1)

	f <-
		fmls(witch ~ .x(wicked) + .x(good) + .m(magic) + west + north + green,
				 pattern = "sequential")
	expect_equal(nrow(f), 10)
})


test_that("printed formulas annotate meta terms with their runes", {

	withr::local_options(recast.color = FALSE)

	f <- fmls(mpg ~ .x(wt) + hp + .s(am))
	expect_match(format(f), "mpg ~ wt + hp + .s(am)", fixed = TRUE)

	# Random effects annotate the same way
	r <- fmls(mpg ~ .x(wt) + .r(cyl))
	expect_match(format(r), ".r(cyl)", fixed = TRUE)

	# A stratum stays with its own family after combining
	g <- fmls(mpg ~ .x(hp))
	both <- c(f, g)
	fmt <- format(both)
	expect_match(fmt[1], ".s(am)", fixed = TRUE)
	expect_false(grepl(".s(am)", fmt[2], fixed = TRUE))

})


test_that("combining formulas works appropriately", {

	# Two expossures but the same outcome and same adjustment set
  f1 <- fmls(mpg ~ .x(wt) + hp + cyl, pattern = "sequential")
  f2 <- fmls(mpg ~ .x(disp) + hp + cyl, pattern = "sequential")
	# A mediation analysis with similar variables
	f3 <- fmls(mpg ~ .x(wt) + .m(hp) + cyl)

	# Roles differ across the families (`hp` adjusts f1 and f2, mediates f3),
	# but that is not a conflict: each family keeps its own definition
	expect_no_message({f <- c(f1, f2, f3)})

	# Both roles are maintained for the shared term, in family order
	expect_equal({
		x <- f |> tm() |> describe("role")
		x$hp
	}, c("predictor", "mediator"))

	# And each formula still reads its own family's definition: the
	# mediation triad regenerates intact from the combined object
	fmt <- format(f)
	expect_true("hp ~ wt" %in% fmt)

})
