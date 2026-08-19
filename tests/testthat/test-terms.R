# Term vector implementation ----

test_that("`tm` objects can be generated and printed", {
	# Basic generation
	expect_length(new_tm(), 0)
	expect_s3_class(new_tm(), "tm")
	expect_output(print(new_tm()), "<term\\[0\\]>")
	expect_length(tm(), 0)
	expect_s3_class(tm(), "tm")
	expect_output(print(tm()), "<term\\[0\\]>")
	expect_equal(tm(), tm(tm()))


	if (isTRUE(requireNamespace("tibble", quietly = TRUE))) {
		tibble::tibble(tm()) |>
			print() |>
			expect_output("<tm>")
	}
})

test_that("formula formatting passes color options to terms", {

	old <- options(
		cli.num_colors = 256,
		recast.color = TRUE
	)
	on.exit(options(old), add = TRUE)

	f <- fmls(mpg ~ .x(wt) + hp)
	# These must dispatch through base's generics. S7 methods registered by the
	# table specification must not create package-local generic bindings.
	noColor <- base::format(f, color = FALSE)
	colored <- base::format(f, color = TRUE)

	expect_equal(noColor, "mpg ~ wt + hp")
	expect_equal(cli::ansi_strip(colored), "mpg ~ wt + hp")
	expect_false(identical(colored, cli::ansi_strip(colored)))
	expect_output(base::print(f, color = FALSE), "<fmls: 1 formula>", fixed = TRUE)

})

test_that("new `tm` can be made from character/atomic components", {

	ty <- tm(
		x = "Y",
		role = "outcome",
		label = "Dependent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "continuous"
	)

	tx <- tm(
		"X",
		role = "exposure",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "continuous"
	)

	tm <- tm(
		"M",
		role = "mediator",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "normal",
		type = "continuous"
	)

	tc <- tm(
		"C",
		role = "confounder",
		label = "Confounder",
		description = "Artificially created",
		distribution = "ordinal",
		type = "categorical"
	)

	tp <- tm(
		"P",
		role = "predictor",
		label = "Independent Variable",
		description = "Artificially created",
		distribution = "ordinal",
		type = "categorical"
	)

	ts <- tm(
		"S",
		role = "strata",
		label = "Stratification Variable",
		description = "Levels for data set",
		distribution = "binary",
		type = "categorical",
	)

	ti <- tm(
		"I",
		role = "interaction",
		label = "Interaction Variable",
		description = "Interaction for the exposure variable",
		distribution = "binary",
		type = "categorical",
	)

	t <- c(ty, tx, tm, tp, tc, ts, ti)

	expect_length(t, 7)
	expect_true(is_tm(t))
	expect_length(tm(character()), 0)

})

test_that("terms can be generated from a formula", {

	# Formula stump
	x <- ~ input
	t <- tm(x)
	expect_length(t, 1)

	# Simple formulas
	f1 <- output ~ input + modifier
	f2 <- output ~ .x(input) + modifier
	f3 <- output ~ .x(input) + log(modifier) + log(variable) + another
	expect_equal(lhs(f1), lhs(f2))
	expect_match(rhs(f2), ".x", all = FALSE)

	group = type = distribution = description = transformation = formula()
	rl <- input ~ "exposure"
	lb <- list(output ~ "The Final Outcome", input ~ "The First Mover")
	allArgs <-
		list(
			role = rl,
			label = lb,
			group = group,
			type = type,
			distribution = distribution,
			description = description,
			transformation = transformation
		)

	tms <- tm(f1, role = rl, label = lb)
	d <- vec_data(tms)
	expect_equal(d$role[d$term == "input"], "exposure")

	tms <- tm(f3, role = rl, label = lb)
	expect_s3_class(tms, "tm")
	expect_length(tms, 5)
	expect_equal(vec_data(tms)$label[1], "The Final Outcome")
	expect_length(tm(formula()), 0)

})

test_that("term coercion works", {

	f <- fmls(.o(wicked) ~ .x(witch) + west)
	t <- tm(f)
	expect_s3_class(t, "tm")

})

# Special terms ----

test_that("mediation terms will be assessed correctly", {

	x <- .o(output) ~ .x(input) + .m(mediator) + random
	t <- tm(x)
	expect_equal(describe(t, "role")$mediator, "mediator")

	x <- output ~ input + .m(mediator) + random
	expect_warning(t <- tm(x))
	expect_equal(describe(t, "role")$mediator, "predictor")

})

test_that("term groups can be established", {

	x <- witch ~ glinda + wicked + west
	role = label = type = distribution = description = transformation = formula()
	group <- list(wicked ~ 1, west ~ 1)
	allArgs <-
		list(
			role = role,
			label = label,
			group = group,
			type = type,
			distribution = distribution,
			description = description,
			transformation = transformation
		)

	# Group by term implementation external to formula
	t <- tm(x, group = group)
	d <- vec_data(t)
	expect_equal(d$group[d$term == "wicked"], 1)

	### Group term implementation using shortcuts

	# Expect .g to become .g0, and .g1 to be g1
	x1 <- witch ~ west + .g(green) + .g(wicked)
	x2 <- witch ~ west + .g(green) + .g1(wicked)
	t1 <- tm(x1)
	t2 <- tm(x2)
	expect_equal(describe(t1, "group")$green, describe(t2, "group")$green)
	expect_equal(describe(t2, "group")$wicked, 1)

	# Expect appropriate group levels
	x1 <- witch ~ west + .g1(green) + .g1(wicked)
	x2 <- witch ~ west + .g1(green) + .g2(wicked)
	t1 <- tm(x1)
	t2 <- tm(x2)
	expect_equal(describe(t1, "group")$green, describe(t2, "group")$green)
	expect_equal(describe(t2, "group")$wicked, 2)
})

test_that("interaction terms are appropriately made", {

	# From formula
	f <- witch ~ green + wicked*west
	expect_length(tm(f), 5)

	# Expanded formula
	x <- witch ~ wicked + west + wicked:west + green
	t <- tm(x)
	expect_length(t, 5)
	expect_equal(describe(t, "role")$'wicked:west', "interaction")

	# Role-based interaction
	x <- witch ~ .x(wicked) + green + .i(west)
	role = label = group = type = distribution = description = transformation =
		formula()
	expect_message(t <- tm(x))
	expect_length(t, 5)

	# Interaction role must be given to the correct term
	x <- wicked ~ .x(witch) + .i(green)
	tmTab <- vec_proxy(tm(x))
	expect_equal(tmTab$role[tmTab$term == "green"], "interaction")
	expect_equal(tmTab$role[tmTab$term == "witch:green"], "interaction")

	# Multiple interactions
	# Two messages about two interaction terms
	expect_message(expect_message(t <- tm(mpg ~ .x(hp) + .i(am) + .i(vs))))
	expect_length(t, 6)

	# Multiple exposures with interaction
	# Two messages for two exposures
	x <- mpg ~ .x(hp) + .x(cyl) + .i(am)
	expect_message(expect_message(t <- tm(x)))
	expect_length(t, 6)

	# Most importantly, an interaction term has a grouping
	# Groups must "go" together in the sequence of terms
	x <- wicked ~ .x(witch) + .i(west) + green
	t <- tm(x)
	expect_equal(vec_data(t)$term,
									c("wicked", "witch", "west", "witch:west", "green"))



})

# Term helpers ----

test_that("terms can be found and updated and attributes can be found", {

	object <- tm(output ~ input + .c(modifier))
	t <- filter(object, role == "outcome")
	expect_length(t, 1)
	expect_equal(describe(t, "role")[[1]], "outcome")

	dots <- list(
		role = input ~ "exposure",
		label = list(output ~ "The Final Outcome", input ~ "The First Mover")
	)

	x <- update(
		object,
		role = input ~ "exposure",
		label = list(output ~ "The Final Outcome", input ~ "The First Mover")
	)

	y <- update(object, dots)

	expect_equal(x, y)
	expect_equal(vec_data(x)$role, c("outcome", "exposure", "confounder"))
})

test_that("engine-native strata() passes through as a conditioning term", {

	# `survival::strata()` conditions *within* one model — a different
	# mechanism from `.s()`, which splits the data into separate fits — so
	# the call rides the formula whole, traced in `transformation`
	t <- tm(Surv(time, status) ~ x + strata(sex))
	proxy <- vctrs::vec_proxy(t)
	row <- proxy[proxy$term == "strata(sex)", ]
	expect_equal(nrow(row), 1)
	expect_equal(row$side, "right")
	expect_equal(row$transformation, "strata")
	expect_false("strata" %in% proxy$role)

	# Joint conditioning stays one term; the namespaced form works the same
	t2 <- tm(y ~ x + strata(a, b))
	expect_true("strata(a, b)" %in% vctrs::vec_proxy(t2)$term)
	t3 <- tm(y ~ x + survival::strata(g))
	expect_true("survival::strata(g)" %in% vctrs::vec_proxy(t3)$term)

	# The fitted formula keeps the conditioning term for the engine, and no
	# data split is planned — unlike a `.s()` stratum
	f <- fmls(y ~ .x(x) + strata(g))
	d <- data.frame(y = 1:4, x = 4:1, g = c("a", "a", "b", "b"))
	fp <- plan_fit(f, data = d)
	expect_match(fp$formula_call, "strata(g)", fixed = TRUE)
	expect_true(all(is.na(fp$strata_variable)))

	fs <- fmls(y ~ .x(x) + .s(g))
	fps <- plan_fit(fs, data = d)
	expect_false(any(grepl("strata", fps$formula_call)))
	expect_equal(unique(fps$strata_variable), "g")

})


# Round trips: formula -> tm -> formula must be lossless ----

test_that("plain formulas round-trip through `tm` losslessly", {

	fl <- list(
		mpg ~ wt,
		mpg ~ wt + hp,
		witch ~ green + wicked:west,
		witch ~ wicked + west + wicked:west + green
	)

	for (f in fl) {
		t <- tm(f)
		expect_equal(deparse1(stats::formula(t)), deparse1(f))
	}

})

test_that("role shortcuts reconstruct to their explicit formula", {

	# Roles annotate terms; reconstruction drops the runes but keeps structure
	t <- tm(output ~ .x(input) + log(modifier) + another)
	expect_equal(
		deparse1(stats::formula(t)),
		"output ~ input + log(modifier) + another"
	)

	# Interaction shortcuts expand to the explicit product
	expect_message(t <- tm(witch ~ .x(wicked) + .i(west)))
	expect_equal(
		deparse1(stats::formula(t)),
		"witch ~ wicked + west + wicked:west"
	)

})

test_that("opaque calls survive the round trip", {

	f <- survival::Surv(time, status) ~ ph.karno + cluster(sex)
	t <- tm(survival::Surv(time, status) ~ ph.karno + cluster(sex))
	expect_true("cluster(sex)" %in% vec_data(t)$term)

})

test_that("meta terms leave the fitting formula but stay on the term", {

	# Strata guide fitting; they are not covariates
	t <- tm(mpg ~ wt + .s(am))
	expect_equal(deparse1(stats::formula(t)), "mpg ~ wt")
	expect_equal(describe(t, "role")$am, "strata")

	# Random effects render in hierarchical form
	t <- tm(mpg ~ .x(wt) + .r(id))
	expect_equal(deparse1(stats::formula(t)), "mpg ~ wt + (1 | id)")
	expect_equal(describe(t, "role")$id, "random")
	expect_equal(describe(t, "side")$id, "meta")

})

# M1 additions to the term layer ----

test_that("group tiers accept multiple digits", {

	t <- tm(witch ~ west + .g10(green) + .g10(wicked))
	expect_equal(describe(t, "group")$green, 10)
	expect_equal(describe(t, "group")$wicked, 10)

})

test_that("roles wrap transformations without confusion", {

	# Nested runes: role outside, transformation inside
	t <- tm(output ~ .x(log(input)) + modifier)
	d <- vec_data(t)
	expect_equal(d$role[d$term == "log(input)"], "exposure")
	expect_equal(d$transformation[d$term == "log(input)"], "log")

	# The transformation vocabulary extends beyond log
	t <- tm(output ~ sqrt(input) + factor(group))
	d <- vec_data(t)
	expect_equal(d$transformation[d$term == "sqrt(input)"], "sqrt")
	expect_equal(d$transformation[d$term == "factor(group)"], "factor")

})

test_that("terms carry levels once known", {

	t <- tm("am", role = "strata", level = c("0", "1"))
	expect_equal(describe(t, "level")$am, c("0", "1"))

	# Updating levels through `update()`
	t <- tm(mpg ~ wt + .s(am))
	t2 <- update(t, level = am ~ c("automatic", "manual"))
	expect_equal(describe(t2, "level")$am, c("automatic", "manual"))

})
