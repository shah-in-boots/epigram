test_that('interaction estimates can be made', {
	# Survival model
	library(survival)

	# Since sex is a two level structure, interaction must happen at both
	# levels. `lung$sex` is coded 1/2, which the within-level arithmetic does
	# not fit (it assumes a factor or 0/1), so the modifier is a factor here
	d <- lung
	d$sex <- factor(d$sex)
	x <-
		fmls(Surv(time, status) ~ .x(age) + ph.karno + .i(sex),
				 pattern = 'sequential') |>
		fit(.fn = coxph, data = d, raw = FALSE) |>
		suppressMessages()

	mt <- model_table(int_sex = x, data = d)
	expect_s3_class(mt, 'mdl_tbl')
	expect_equal(nrow(mt), 3)
	expect_error(estimate_interaction(mt), regexp = "single row")

	object <- dplyr::filter(mt, interaction == 'sex')
	expect_equal(nrow(object), 1)
	expect_error(
	  estimate_interaction(object, exposure = "ph.karno"),
	  regexp = "exposure"
	)
	expect_error(
	  estimate_interaction(object, exposure = "age", interaction = "ph.karno"),
	  regexp = "interaction"
	)

	i <- estimate_interaction(
	  object,
	  exposure = "age",
	  interaction = "sex",
	  conf_level = 0.95
	)

	expect_length(i, 8)
	expect_equal(nrow(i), 2)
	expect_named(i, c("estimate", "conf_low", "conf_high", "p_value",
										"statistic", "df", "nobs", "level"))
	# One interaction coefficient: its own test, one degree of freedom
	expect_equal(unique(i$df), 1L)

})

test_that("interaction generalizes to categorical levels, against
					 hand-computed references (M6.9)", {

	d <- mtcars
	d$cyl <- factor(d$cyl)

	mt <-
		fmls(mpg ~ .x(hp) + .i(cyl)) |>
		fit(.fn = lm, data = d, raw = FALSE) |>
		model_table(data = d) |>
		suppressMessages()
	object <- dplyr::filter(mt, interaction == "cyl")

	i <- estimate_interaction(object, exposure = "hp", interaction = "cyl")
	expect_equal(nrow(i), 3)
	expect_equal(i$level, c("4", "6", "8"))

	# Per-level effects and intervals from the variance-covariance matrix, by
	# name (Figueiras et al. 1998), against a hand fit
	ref <- stats::lm(mpg ~ hp * cyl, data = d)
	b <- stats::coef(ref)
	V <- stats::vcov(ref)
	crit <- stats::qt(0.975, df = stats::df.residual(ref))

	expect_equal(i$estimate[1], unname(b["hp"]))
	expect_equal(i$conf_low[1], unname(b["hp"] - crit * sqrt(V["hp", "hp"])))
	for (k in c("6", "8")) {
		key <- paste0("hp:cyl", k)
		at <- match(k, i$level)
		est <- unname(b["hp"] + b[key])
		se <- sqrt(V["hp", "hp"] + V[key, key] + 2 * V["hp", key])
		expect_equal(i$estimate[at], est)
		expect_equal(i$conf_low[at], est - crit * se)
		expect_equal(i$conf_high[at], est + crit * se)
	}

	# The across-levels p-value is the joint Wald test of the interaction
	# coefficients, repeated on every row, with the statistic and its degrees
	# of freedom beside it so a table can say "joint Wald, 2 df"
	keys <- c("hp:cyl6", "hp:cyl8")
	stat <- drop(t(b[keys]) %*% solve(V[keys, keys]) %*% b[keys])
	expect_equal(unique(i$p_value),
							 stats::pchisq(stat, df = 2, lower.tail = FALSE))
	expect_equal(unique(i$statistic), stat)
	expect_equal(unique(i$df), 2L)

	# Per-level observation counts come from the attached data
	expect_equal(i$nobs, unname(as.integer(table(d$cyl))))
})

test_that("a categorical exposure derives one row-set per exposure contrast", {

	d <- mtcars
	d$cyl <- factor(d$cyl)

	mt <-
		fmls(mpg ~ .x(cyl) + .i(am)) |>
		fit(.fn = lm, data = d, raw = FALSE) |>
		model_table(data = d) |>
		suppressMessages()
	object <- dplyr::filter(mt, interaction == "am")

	i <- estimate_interaction(object, exposure = "cyl", interaction = "am")

	# Two non-reference exposure levels x two interaction levels, with the
	# exposure contrast named on every row
	expect_named(
		i,
		c("estimate", "conf_low", "conf_high", "p_value", "statistic", "df",
			"nobs", "level", "exposure_level")
	)
	expect_equal(nrow(i), 4)
	expect_equal(i$exposure_level, c("6", "6", "8", "8"))
	expect_equal(i$level, c("0", "1", "0", "1"))

	# Against a hand fit: within am = 0 the effect of level k is its own
	# coefficient; within am = 1 it adds the interaction coefficient, with the
	# covariance in the variance (Figueiras et al. 1998)
	ref <- stats::lm(mpg ~ cyl * am, data = d)
	b <- stats::coef(ref)
	V <- stats::vcov(ref)
	crit <- stats::qt(0.975, df = stats::df.residual(ref))
	for (k in c("6", "8")) {
		expKey <- paste0("cyl", k)
		intKey <- paste0("cyl", k, ":am")
		at0 <- which(i$exposure_level == k & i$level == "0")
		at1 <- which(i$exposure_level == k & i$level == "1")
		expect_equal(i$estimate[at0], unname(b[expKey]))
		expect_equal(i$estimate[at1], unname(b[expKey] + b[intKey]))
		se1 <- sqrt(V[expKey, expKey] + V[intKey, intKey] +
									2 * V[expKey, intKey])
		expect_equal(i$conf_low[at1], unname(b[expKey] + b[intKey]) - crit * se1)
	}

	# The across-levels p-value is the joint Wald test over every interaction
	# coefficient the exposure's contrasts carry
	keys <- c("cyl6:am", "cyl8:am")
	stat <- drop(t(b[keys]) %*% solve(V[keys, keys]) %*% b[keys])
	expect_equal(unique(i$p_value),
							 stats::pchisq(stat, df = 2, lower.tail = FALSE))

})

test_that("a numeric modifier is refused, since there are no levels to condition on", {
	d <- mtcars
	# `hp` takes many values. Every level matched the same bare `wt:hp` key,
	# so the joint test's matrix was rank one and `solve()` died in LAPACK
	mt <- fmls(mpg ~ .x(wt) + .i(hp)) |>
		fit(.fn = lm, data = d) |>
		model_table(data = d) |>
		suppressMessages()
	expect_error(
		estimate_interaction(mt, exposure = "wt", interaction = "hp"),
		"`hp` is numeric"
	)
	# Coded 1/2 the level-2 effect is `b_exp + 2 b_int`, not `b_exp + b_int`,
	# so a two-valued numeric modifier is accepted only as 0/1
	d$g <- ifelse(d$am == 1, 2, 1)
	mt2 <- fmls(mpg ~ .x(wt) + .i(g)) |>
		fit(.fn = lm, data = d) |>
		model_table(data = d) |>
		suppressMessages()
	expect_error(
		estimate_interaction(mt2, exposure = "wt", interaction = "g"),
		"`g` is numeric"
	)
	# 0/1 is the model's own contrast and passes
	mt3 <- fmls(mpg ~ .x(wt) + .i(am)) |>
		fit(.fn = lm, data = d) |>
		model_table(data = d) |>
		suppressMessages()
	i <- estimate_interaction(mt3, exposure = "wt", interaction = "am")
	ref <- stats::coef(stats::lm(mpg ~ wt * am, data = d))
	expect_equal(i$estimate, unname(c(ref["wt"], ref["wt"] + ref["wt:am"])))
})

test_that("interaction terms match by identity, never by substring", {

	# `am` must not match `gam`, the adversarial-naming rule of M6.2
	d <- mtcars
	d$gam <- rev(d$am)

	mt <-
		fmls(mpg ~ .x(hp) + .i(gam)) |>
		fit(.fn = lm, data = d, raw = FALSE) |>
		model_table(data = d) |>
		suppressMessages()
	object <- dplyr::filter(mt, interaction == "gam")

	# The model interacts on `gam`; asking for `am` is an identity miss
	expect_error(
		estimate_interaction(object, exposure = "hp", interaction = "am"),
		"interaction"
	)

	# And the true term resolves through its exact keys
	i <- estimate_interaction(object, exposure = "hp", interaction = "gam")
	ref <- stats::lm(mpg ~ hp * gam, data = d)
	expect_equal(i$estimate[1], unname(stats::coef(ref)["hp"]))
	expect_equal(
		i$estimate[2],
		unname(stats::coef(ref)["hp"] + stats::coef(ref)["hp:gam"])
	)
})

test_that("an aliased interaction coefficient leaves the joint test rather than voiding it", {
	d <- mtcars
	d$cyl <- factor(d$cyl)
	d$gear <- factor(d$gear)
	# No 8-cylinder car has 4 gears, so `cyl8:gear4` cannot be estimated and
	# `lm()` returns it as `NA`; the joint test over the remaining product
	# terms should still be a number
	stopifnot(sum(d$cyl == "8" & d$gear == "4") == 0)
	mt <- fmls(mpg ~ .x(cyl) + .i(gear)) |>
		fit(.fn = lm, data = d) |>
		model_table(data = d)
	est <- estimate_interaction(mt, exposure = "cyl", interaction = "gear")
	expect_false(anyNA(est$p_value))
	expect_true(is.na(est$estimate[est$exposure_level == "8" & est$level == "4"]))
	expect_false(anyNA(est$estimate[est$exposure_level == "6"]))
})

test_that("an ordered modifier is refused with polynomial contrasts named as the cause", {
	d <- mtcars
	d$cyl <- factor(d$cyl, ordered = TRUE)
	# Under the default contrasts the coefficients are `cyl.L` and `cyl.Q`,
	# so no `hp:cyl6` key exists; the refusal should say why, not just that
	mt <- fmls(mpg ~ .x(hp) + .i(cyl)) |>
		fit(.fn = lm, data = d) |>
		model_table(data = d)
	expect_error(
		estimate_interaction(mt, exposure = "hp", interaction = "cyl"),
		"ordered factor.*factor\\(cyl, ordered = FALSE\\)"
	)
})

test_that("an empty cell at the modifier's reference level is NA, not another cell's effect", {
	# Exposure level `c` never occurs with modifier level `r`, the reference.
	# `lm()` aliases the last product term (`xc:mt`) and the `xc` main effect
	# then measures c vs a within `t`, not within `r`; reading it as the
	# reference-level effect would report a real number under the wrong label
	d <- expand.grid(x = c("a", "b", "c"), m = c("r", "s", "t"), rep = 1:6,
									 stringsAsFactors = TRUE)
	d <- d[!(d$x == "c" & d$m == "r"), ]
	set.seed(11)
	d$y <- stats::rnorm(nrow(d)) + as.integer(d$x) * as.integer(d$m)
	mt <- fmls(y ~ .x(x) + .i(m)) |>
		fit(.fn = lm, data = d) |>
		model_table(data = d)
	est <- estimate_interaction(mt, exposure = "x", interaction = "m")
	pick <- function(e, l) est$estimate[est$exposure_level == e & est$level == l]
	expect_true(is.na(pick("c", "r")))   # the empty cell
	expect_true(is.na(pick("c", "t")))   # the aliased product term
	expect_false(is.na(pick("c", "s")))  # still identified
	expect_false(anyNA(est$estimate[est$exposure_level == "b"]))
	expect_false(anyNA(est$p_value))
})

test_that("counts within a stratified model come from its own stratum", {
	d <- mtcars
	d$cyl <- factor(d$cyl)
	d$am <- factor(d$am)
	mt <- fmls(mpg ~ .x(hp) + .i(cyl) + .s(am)) |>
		fit(.fn = lm, data = d) |>
		model_table(data = d)
	# Every stratum used to report the whole frame's 11 / 7 / 14
	manual <- mt[mt$level == "1", ]
	est <- estimate_interaction(manual, exposure = "hp", interaction = "cyl")
	expect_equal(est$nobs, unname(as.integer(table(d$cyl[d$am == "1"]))))
})
