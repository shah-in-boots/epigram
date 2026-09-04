# Meeting the data ----

test_that("set_data stamps type, distribution, and levels onto terms", {

	t <- tm(mpg ~ wt + .s(am) + factor(cyl))
	t <- set_data(t, mtcars)
	d <- vec_data(t)

	expect_equal(d$type[d$term == "wt"], "continuous")
	expect_equal(d$distribution[d$term == "wt"], "continuous")

	# Binary and categorical are different definitions: a numeric 0/1 column
	# is binary (one quantitative coefficient), not categorical
	expect_equal(d$type[d$term == "am"], "continuous")
	expect_equal(d$distribution[d$term == "am"], "binary")

	# Strata still become data-aware: a stratum splits by observed values
	# whatever the column's type
	expect_equal(describe(t, "level")$am, c("0", "1"))

	# Transformed terms classify from their underlying variable
	expect_equal(d$type[d$term == "factor(cyl)"], "categorical")
	expect_equal(describe(t, "level")$`factor(cyl)`, c("4", "6", "8"))

	# Terms with no matching column are left untouched
	t2 <- set_data(tm(mpg ~ unicorn), mtcars)
	expect_true(is.na(vec_data(t2)$type[2]))

})

test_that("set_data flows through a fmls object", {

	f <- fmls(mpg ~ .x(wt) + .s(am))
	f <- set_data(f, mtcars)
	tmTab <- attr(f, "termTable")
	expect_equal(tmTab$level[[which(tmTab$term == "am")]], c("0", "1"))

})

# Fluent verbs ----

test_that("strata can be added and removed fluently", {

	f <- fmls(mpg ~ .x(wt) + hp)

	f2 <- add_strata(f, am)
	tmTab <- attr(f2, "termTable")
	expect_equal(tmTab$role[tmTab$term == "am"], "strata")
	expect_equal(tmTab$side[tmTab$term == "am"], "meta")

	# The strata guides fitting: one model per level
	m <- fit(f2, .fn = lm, data = mtcars, raw = FALSE)
	expect_length(m, 2)

	# Existing covariates can be promoted to strata, and the family says so:
	# every adjustment set changes when a covariate becomes a stratum, which
	# a caption written earlier does not know
	expect_message(f3 <- add_strata(f, hp), "`hp` was a predictor")
	expect_equal(attr(f3, "termTable")$role[2:3], c("exposure", "strata"))
	# A stratum already in place is not announced again
	expect_no_message(add_strata(f3, hp))

	# A variable holding the name is spliced in with `!!`, a vector with `!!!`;
	# a bare `v` would be taken as a term called `v`
	v <- "am"
	expect_equal(add_strata(f, !!v), add_strata(f, am))
	vs <- c("am", "vs")
	expect_equal(add_strata(f, !!!vs), add_strata(f, am, vs))

	# And removed again
	f4 <- remove_strata(f2, am)
	expect_false("strata" %in% attr(f4, "termTable")$role)
	f5 <- remove_strata(f2)
	expect_false("strata" %in% attr(f5, "termTable")$role)

})

test_that("terms can be added and removed fluently", {

	f <- fmls(mpg ~ .x(wt) + hp)

	f2 <- add_terms(f, cyl, disp)
	expect_true(all(c("cyl", "disp") %in% attr(f2, "termTable")$term))
	expect_equal(
		unname(as.character(f2)),
		"mpg ~ wt + hp + cyl + disp"
	)

	f3 <- remove_terms(f2, hp, disp)
	expect_equal(unname(as.character(f3)), "mpg ~ wt + cyl")

	# Verbs compose in a pipe
	f4 <-
		fmls(mpg ~ .x(wt)) |>
		add_terms(hp) |>
		add_strata(am)
	expect_equal(nrow(attr(f4, "termTable")), 4)

})

test_that("outcomes can be swapped", {

	f <- fmls(mpg ~ .x(wt) + hp)

	# Bare name works when there is a single outcome
	f2 <- swap_outcome(f, qsec)
	expect_equal(unname(as.character(f2)), "qsec ~ wt + hp")

	# Formula form targets a specific outcome
	f3 <- swap_outcome(f, mpg ~ disp)
	expect_equal(unname(as.character(f3)), "disp ~ wt + hp")

	# Guard rails
	expect_error(swap_outcome(f, cyl ~ disp), regexp = "not an outcome")
	expect_error(swap_outcome(f, hp), regexp = "already a term")

})

test_that("subset instructions ride along with the family", {

	f <-
		fmls(mpg ~ .x(wt) + hp) |>
		subset_data(am == 1)

	inst <- attr(f, "instructions")
	expect_length(inst$subsets, 1)

	# Instructions survive combination
	g <- suppressMessages(c(f, fmls(mpg ~ cyl)))
	expect_length(attr(g, "instructions")$subsets, 1)

})

# The deck ----

test_that("printing a fmls shows the deck", {

	f <-
		fmls(mpg ~ .x(wt) + hp + .s(am)) |>
		set_data(mtcars) |>
		subset_data(cyl > 4)

	out <- capture.output(print(f))
	expect_match(out[1], "<fmls: 1 formula>")

	# Roles are visible in the formula coloring and through `describe()`, so
	# the deck only carries what the formulas cannot show: strata, random
	# effects, and subsets
	expect_no_match(out, "outcome: mpg")
	expect_no_match(out, "exposure: wt")
	expect_match(out, "strata: am \\(2 levels\\)", all = FALSE)
	expect_match(out, "subsets: cyl > 4", all = FALSE)

	expect_equal(describe(f, "role")$wt, "exposure")

})

# Random effects through the formula layer ----

test_that("random-effect terms compose and print sensibly", {

	f <- fmls(mpg ~ .x(wt) + .r(cyl))
	expect_equal(deparse1(formula(f)[[1]]), "mpg ~ wt + (1 | cyl)")

	out <- capture.output(print(f))
	expect_match(out, "random: cyl", all = FALSE)

})
