test_that("`fmls` objects can be fitted", {

	# Reference models
	data("mtcars")
	m0 <- lm(mpg ~ wt + hp, data = mtcars)

	# Original models
	object <- fmls(mpg ~ wt + hp)
	args <- list(model = TRUE)
	data <- mtcars
	m1 <- fit(object, .fn = lm, data = mtcars, raw = TRUE)[[1]]
	expect_equal(class(m0), class(m1))
	expect_equal(m0, m1, ignore_attr = TRUE) # Data argument should be the same name

	# <mdl> subtypes
	m1 <- fit(object, .fn = lm, data = mtcars, raw = FALSE)
	expect_s3_class(m1, "mdl")
	expect_length(m1, 1)

	# Stratified, should have two models
	object <- fmls(mpg ~ wt + hp + .s(am))
	m2 <- fit(object, .fn = lm, data = mtcars, raw = TRUE)
	expect_length(m2, 2)
	expect_s3_class(m2[[1]], "lm")

	# Should also keep strata term information when "tidied" into <mdl> object
	m2 <- fit(object, .fn = lm, data = mtcars, raw = FALSE)
	expect_s3_class(m2, "mdl")
	expect_equal(field(m2, "dataArgs")[[1]]$strataVariable, "am")
	expect_equal(field(m2, "dataArgs")[[1]]$dataName, "mtcars")
	# Strata come in level order (sorted, or a factor's own), not in order of
	# first appearance: `mtcars$am` starts with a 1, and the table's row bands
	# should still read 0 then 1
	expect_equal(field(m2, "dataArgs")[[1]]$strataLevel, 0)
	expect_equal(field(m2, "dataArgs")[[2]]$strataLevel, 1)


})

test_that("sequential/lengthy formulas can be fitted", {

	object <- fmls(mpg ~ wt + hp + cyl + .s(am), pattern = "sequential")
	m <- fit(object, .fn = lm, data = mtcars, raw = FALSE)
	expect_length(m, 6)

})


test_that("complex terms can be fit", {

	library(survival) # Using lung data
	f <- Surv(time, status) ~ ph.karno + cluster(sex)
	m0 <- coxph(Surv(time, status) ~ ph.karno + cluster(sex), data = lung)
	object <- fmls(f)
	m1 <- fit(object, .fn = coxph, data = lung, raw = TRUE)[[1]]

	# When fitting an object, the data term name should be retained
	expect_equal(m0, m1, ignore_attr = TRUE)

})

# The plan ----

test_that("the fitting plan is inspectable before anything runs", {

	f <-
		fmls(mpg + hp ~ .x(wt) + .s(am), pattern = "direct") |>
		subset_data(cyl > 4)

	# Two outcomes x two strata levels x one subset
	p <- plan_fit(f, data = mtcars)
	expect_s3_class(p, "tbl_df")
	expect_equal(nrow(p), 4)
	expect_named(
		p,
		c("formula_index", "formula_call", "formula",
			"strata_variable", "strata_level", "subset", "subset_expr")
	)

	# Without data, stratum levels stay unresolved
	p2 <- plan_fit(f)
	expect_equal(nrow(p2), 2)
	expect_true(all(is.na(unlist(p2$strata_level))))

})

test_that("a stratum missing from the data is an error, not a silent empty plan", {

	f <- fmls(mpg ~ .x(wt) + hp) |> add_strata(not_a_column)

	# The zero-level expansion used to erase the formula's models silently
	expect_error(plan_fit(f, data = mtcars), "not_a_column")
	expect_error(fit(f, .fn = lm, data = mtcars, raw = FALSE), "not_a_column")

	# Without data the plan still forms, levels unresolved
	expect_equal(nrow(plan_fit(f)), 1)

})

# The modeling approach resolves by name ----

test_that("argument order and .fn forms do not matter", {

	object <- fmls(mpg ~ wt + hp)

	# Named arguments in any order (the old parser read `.fn` by position)
	m1 <- fit(object, data = mtcars, .fn = lm, raw = TRUE)[[1]]
	expect_s3_class(m1, "lm")

	# A string names the function just as well
	m2 <- fit(object, .fn = "lm", data = mtcars, raw = TRUE)[[1]]
	expect_equal(coef(m1), coef(m2))

	# Unresolvable approaches say so
	expect_error(
		fit(object, .fn = 42, data = mtcars),
		regexp = "fitting function"
	)

})

test_that("parsnip model specifications serve as the modeling approach", {

	skip_if_not_installed("parsnip")

	object <- fmls(mpg ~ .x(wt) + hp)
	spec <- parsnip::linear_reg()

	# Raw returns the underlying engine fit
	m1 <- fit(object, .fn = spec, data = mtcars, raw = TRUE)[[1]]
	expect_s3_class(m1, "lm")
	expect_equal(coef(m1), coef(lm(mpg ~ wt + hp, data = mtcars)))

	# The mdl path carries the engine's identity forward
	m2 <- fit(object, .fn = spec, data = mtcars, raw = FALSE)
	expect_s3_class(m2, "mdl")
	expect_equal(unlist(field(m2, "modelCall")), "lm")
	mt <- model_table(engine = m2)
	expect_true(mt$fit_status[1])

})

# Failures are soft ----

test_that("one failed model does not sink the fleet", {

	# Two outcomes: one fits, one refers to a missing column
	object <- fmls(mpg + unicorn ~ .x(wt), pattern = "direct")

	# Raw mode warns and keeps the error in place
	expect_warning(
		ml <- fit(object, .fn = lm, data = mtcars, raw = TRUE),
		regexp = "failed to fit"
	)
	expect_length(ml, 2)
	expect_s3_class(ml[[1]], "lm")
	expect_s3_class(ml[[2]], "error")

	# The mdl path records the failure with its message
	m <- fit(object, .fn = lm, data = mtcars, raw = FALSE)
	expect_length(m, 2)
	mt <- model_table(mixed_luck = m)
	expect_equal(mt$fit_status, c(TRUE, FALSE))

	# Failed fits fall out of flattening rather than corrupting it
	flat <- flatten_models(mt)
	expect_true(all(flat$outcome == "mpg"))

})

# Subsets ride the plan ----

test_that("subset instructions produce one fit per subset", {

	f <-
		fmls(mpg ~ .x(wt) + hp) |>
		subset_data(am == 1, cyl > 4)

	m <- fit(f, .fn = lm, data = mtcars, raw = FALSE)
	expect_length(m, 2)

	si <- field(m, "summaryInfo")
	expect_equal(si[[1]]$nobs, sum(mtcars$am == 1))
	expect_equal(si[[2]]$nobs, sum(mtcars$cyl > 4))

	# Provenance lands in the model table
	mt <- model_table(subsets = m)
	expect_equal(mt$subset, c("am == 1", "cyl > 4"))

})

# Mixed models through the random-effects role ----

test_that("random-effect terms fit through lme4", {

	skip_if_not_installed("lme4")
	skip_if_not_installed("broom.mixed")

	f <- fmls(mpg ~ .x(wt) + .r(cyl))
	m <- fit(f, .fn = lme4::lmer, data = mtcars, raw = FALSE)

	expect_s3_class(m, "mdl")
	expect_equal(unlist(field(m, "modelCall")), "lmerMod")

	# Fixed effects flow into the parameter table
	pe <- field(m, "parameterEstimates")[[1]]
	expect_true(all(c("(Intercept)", "wt") %in% pe$term))

	mt <- model_table(mixed = m)
	expect_true(mt$fit_status[1])

})

# Model families keep their own degrees of freedom ----

test_that("degrees of freedom follow each model family's accounting", {

	x <- lm(mpg ~ wt + hp, data = mtcars)
	m <- mdl(x)
	expect_equal(
		field(m, "summaryInfo")[[1]]$degrees_freedom,
		df.residual(x)
	)

})

# The grammar's default: fit() returns mdl vectors ----

test_that("fit() defaults to raw = FALSE, returning a `mdl` vector", {

	object <- fmls(mpg ~ .x(wt) + hp)
	m <- fit(object, .fn = lm, data = mtcars)

	expect_s3_class(m, "mdl")
	expect_identical(m, fit(object, .fn = lm, data = mtcars, raw = FALSE))

})

test_that("an inline `data` expression records a stable content-derived id", {

	object <- fmls(mpg ~ .x(wt))
	expect_message(
		m <- fit(object, .fn = lm, data = subset(mtcars, am == 1)),
		"record the dataset as `data_"
	)
	mt <- model_table(m)
	expect_match(mt$data_id, "^data_[0-9a-f]{8}$")

	# The identical frame gets the identical id at attach time, so they meet
	expect_message(
		mt <- attach_data(mt, subset(mtcars, am == 1)),
		"attaching it as `data_"
	)
	expect_equal(names(attr(mt, "dataList")), mt$data_id)

})

test_that("engine-native strata() conditions within one model at fit", {

	skip_if_not_installed("survival")
	lung <- survival::lung
	lung$sex <- factor(lung$sex)

	m <- fit(
		fmls(Surv(time, status) ~ .x(age) + survival::strata(sex)),
		.fn = survival::coxph, data = lung
	)
	mt <- model_table(m, data = lung)

	# One model, not one per sex level: conditioning, not a data split
	expect_equal(nrow(mt), 1)
	expect_true(is.na(mt$strata))
	expect_match(mt$formula_call, "strata(sex)", fixed = TRUE)
	expect_true(mt$fit_status)

	# The engine absorbed the conditioning term; only `age` has a coefficient
	expect_equal(mt$model_parameters[[1]]$term, "age")

})

test_that("warnings raised while fitting are recorded on the model, not lost", {
	# Complete separation: `glm()` warns that fitted probabilities are 0 or 1
	d <- data.frame(y = c(0, 0, 0, 1, 1, 1), z = 1:6)
	# Nothing leaks: the fit's own warnings and the dozens `confint()` raises
	# while profiling the separated likelihood are all recorded, not emitted
	expect_no_warning(
		m <- fit(fmls(y ~ .x(z)), .fn = glm, family = binomial(), data = d)
	)
	mt <- model_table(m, data = d)
	w <- model_diagnostics(mt)
	expect_equal(nrow(w), 1)
	expect_match(w$warnings[[1]], "fitted probabilities|did not converge", all = FALSE)
	# Repeats collapse to one line with a count
	expect_true(any(grepl("\\(x[0-9]+\\)$", w$warnings[[1]])))
	expect_output(print(mt), "fit with warnings")
	# Convergence is not estimability: the fit is `fitted`, and the separation
	# shows as a coefficient whose interval the profile could not bound and a
	# standard error in the thousands, with no threshold to invent
	expect_equal(w$status, "fitted")
	expect_equal(w$unbounded, 1L)
	expect_gt(w$max_std_error, 1000)
	# A clean fit records none of it
	clean <- model_diagnostics(model_table(fit(fmls(mpg ~ .x(wt)), .fn = lm, data = mtcars)))
	expect_length(clean$warnings[[1]], 0)
	expect_equal(clean$unbounded, 0L)
	# The raw path re-emits them, since a bare fit has nowhere to keep them
	raw <- testthat::capture_warnings(
		fit(fmls(y ~ .x(z)), .fn = glm, family = binomial(), data = d, raw = TRUE)
	)
	expect_true(any(grepl("fitted probabilities|did not converge", raw)))
})
