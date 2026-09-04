test_that("model tables can be initialized/created", {

	# Empty builds should fail with direction
	expect_error(mdl_tbl(), regexp = "something to tabulate")
	expect_error(model_table(), regexp = "something to tabulate")
	expect_error(new_model_table())

})

test_that("construction validates its inputs (issue #46)", {

	# Raw fitted models are turned away with directions
	rawFit <- lm(mpg ~ wt, mtcars)
	expect_error(model_table(rawFit), regexp = "raw = FALSE")
	expect_error(model_table(list(rawFit)), regexp = "`lm` object")

	# Every construction is validated against the invariant columns, then the
	# derived family columns are stamped on after them
	x <- fit(fmls(mpg ~ wt), .fn = lm, data = mtcars, raw = FALSE)
	m <- model_table(x)
	expect_named(m, c(model_table_columns(), "family", "pattern", "relation"))
	expect_error(
		new_model_table(list(id = "a")),
		regexp = "invariant column"
	)

})

test_that("model constructors work for initialization", {

	x <- fit(fmls(mpg ~ wt + hp + .s(am)), .fn = lm, data = mtcars, raw = FALSE)
	expect_length(x, 2)

	# Will only handle first model
	m <- construct_table_from_models(x)
	expect_s3_class(m, "mdl_tbl")
	expect_length(m, 16)
	expect_equal(nrow(m), 1) # Only one strata level at a time

	# Provenance columns are type-stable
	expect_type(m$name, "character")
	expect_type(m$level, "character")
	expect_type(m$data_id, "character")
	expect_type(m$number, "integer")
	expect_equal(m$number, 2) # wt + hp; the stratum splits the data instead

})

test_that("can handle list of models appropriately", {

	x <-
		fit(fmls(mpg ~ wt + hp + .s(am)),
				.fn = lm,
				data = mtcars,
				raw = FALSE)
	y <-
		fit(
			fmls(am ~ disp + cyl),
			.fn = glm,
			family = "binomial",
			data = mtcars,
			raw = FALSE
		)

	# Test if unnamed list of multiple objects
	dots <- list(x, y)

	z <- model_table(dots)
	expect_s3_class(z, "mdl_tbl")
	expect_output(print(z), "<model_table>")
	expect_equal(nrow(z), 3)
	expect_length(z, 19)
	expect_length(attr(z, "termTable")$term, 7)
	expect_length(unique(attr(z, "termTable")$term), 6)
	expect_length(attr(z, "formulaMatrix"), 6)

	# The formula matrix stays parallel to the table rows
	expect_equal(nrow(attr(z, "formulaMatrix")), nrow(z))

	# Test if single unnamed object
	dots <- list(y)
	z <- model_table(dots)
	expect_true(is.na(z$name))

	# Test if single named object
	dots <- list(single = x)
	z <- model_table(dots)
	expect_true(unique(z$name) == "single")

	# Test if multiple named objects
	dots <- list(linear = x, log = y)
	z <- model_table(dots)
	expect_equal(unique(z$name), c("linear", "log"))

	# Test for mixed naming of list of objects
	dots <- list(x, log = y)
	z <- model_table(dots)
	expect_equal(unique(z$name), c(NA, "log"))

})

test_that("formulas can be input into a model table", {

	f <- mpg ~ wt + hp + am
	x <- fmls(f, pattern = "sequential")
	m <- construct_table_from_formulas(list(x))
	expect_s3_class(m, "mdl_tbl")
	expect_length(m, 16)
	expect_equal(nrow(m), 3)

	# Unfit formulas have list columns awaiting their fits, not logical holes
	expect_type(m$model_parameters, "list")
	expect_type(m$model_summary, "list")
	expect_false(any(m$fit_status))

	# The public constructor takes formulas directly, named or not
	m2 <- model_table(x)
	expect_s3_class(m2, "mdl_tbl")
	expect_equal(nrow(m2), 3)
	m3 <- model_table(unfit = x)
	expect_equal(unique(m3$name), "unfit")

})

test_that("tables from different datasets combine intact (issue #26)", {

	skip_if_not_installed("survival")
	library(survival)

	m1 <-
		fmls(mpg ~ wt + hp + .s(am)) |>
		fit(.fn = lm, data = mtcars, raw = FALSE) |>
		model_table(data = mtcars)

	m2 <-
		fmls(Surv(time, status) ~ ph.karno) |>
		fit(.fn = coxph, data = lung, raw = FALSE) |>
		model_table(data = lung)

	# `model_table()` itself combines tables
	m3 <- model_table(m1, m2)
	expect_s3_class(m3, "mdl_tbl")
	expect_equal(nrow(m3), 3)

	# The data list survives the combination and the cast (issue #26)
	expect_setequal(names(attr(m3, "dataList")), c("mtcars", "lung"))
	m4 <- vec_cast(m2, m1)
	expect_setequal(names(attr(m4, "dataList")), c("mtcars", "lung"))
	expect_equal(nrow(attr(m4, "formulaMatrix")), nrow(m4))

	# `vec_c()`/`vec_rbind()` behave the same way
	m5 <- vec_c(m1, m2)
	expect_setequal(names(attr(m5, "dataList")), c("mtcars", "lung"))

	# Formula matrix rows stay parallel to models, with no NA holes
	fmMat <- attr(m3, "formulaMatrix")
	expect_equal(nrow(fmMat), nrow(m3))
	expect_false(anyNA(fmMat))
	expect_true("ph.karno" %in% names(fmMat))

	# Both tables' terms are present, deduplicated
	tmTab <- attr(m3, "termTable")
	expect_true(all(c("mpg", "wt", "am", "Surv(time, status)") %in% tmTab$term))
	key <- paste(tmTab$term, tmTab$role, tmTab$side)
	expect_false(any(duplicated(key)))

})

test_that("combining term tables keeps the first definition", {

	x <- data.frame(term = c("a", "b"), role = c("outcome", "predictor"),
									side = c("left", "right"), label = c("First", NA))
	y <- data.frame(term = c("a", "c"), role = c("outcome", "predictor"),
									side = c("left", "right"), label = c("Second", NA))

	merged <- merge_term_tables(x, y)
	expect_equal(nrow(merged), 3)
	expect_equal(merged$label[merged$term == "a"], "First")

})

test_that("dplyr compatibility", {

	m1 <-
		fit(fmls(mpg ~ wt + hp + .s(am)),
				.fn = lm,
				data = mtcars,
				raw = FALSE)
	m2 <-
		fit(
			fmls(vs ~ .x(mpg)),
			.fn = glm,
			family = "binomial",
			data = mtcars,
			raw = FALSE
		)

	# MPG is an exposure and an outcome in different formulas
	x <- model_table(m1, m2)
	expect_equal(model_table(list(m1, m2)), x)

	# Row subsets downscale every attribute to the remaining models; the
	# stratum `am` holds a formula-matrix column like any other term
	y <- x[1:2, ]
	a <- attributes(y)
	expect_length(a$formulaMatrix, 4)
	expect_equal(nrow(a$formulaMatrix), 2)
	expect_length(a$termTable$term, 4)
	expect_false("vs" %in% a$termTable$term)

	# The dplyr reconstruction path agrees with `[`
	expect_equal(attributes(dplyr_reconstruct(y, x)), a)

	# Would want attributes to downscale with less information present
	f <- fmls(mpg ~ wt + hp + cyl + .s(am), pattern = "sequential")
	m <- fit(f, .fn = lm, data = mtcars, raw = FALSE)
	x <- model_table(m)

	y <- filter(x, formula_call == "mpg ~ wt")
	a <- attributes(y)
	expect_length(a$termTable$term, 3) # mpg wt (strata = am)

})

test_that("dplyr verbs preserve or downgrade the class deliberately (issue #23)", {

	x <-
		fmls(mpg ~ wt + hp + cyl + .s(am), pattern = "sequential") |>
		fit(.fn = lm, data = mtcars, raw = FALSE) |>
		model_table(data = mtcars)

	# filter: keeps the class, prunes the attributes
	y <- dplyr::filter(x, formula_call == "mpg ~ wt")
	expect_s3_class(y, "mdl_tbl")
	expect_equal(nrow(attr(y, "formulaMatrix")), nrow(y))
	expect_false("cyl" %in% names(attr(y, "formulaMatrix")))

	# mutate: added columns are welcome
	y <- dplyr::mutate(x, note = "sensitivity")
	expect_s3_class(y, "mdl_tbl")
	expect_true("note" %in% names(y))
	expect_equal(nrow(attr(y, "formulaMatrix")), nrow(y))

	# arrange: rows and formula matrix move together
	y <- dplyr::arrange(x, dplyr::desc(formula_call))
	expect_s3_class(y, "mdl_tbl")
	fmMat <- attr(y, "formulaMatrix")
	expect_equal(
		fmMat$cyl == 1,
		grepl("cyl", y$formula_call)
	)

	# select: keeping the invariant columns keeps the class
	y <- dplyr::select(x, dplyr::everything())
	expect_s3_class(y, "mdl_tbl")

	# select: dropping invariant columns downgrades with a message
	expect_message(
		y <- dplyr::select(x, outcome, exposure),
		regexp = "invariant"
	)
	expect_false(inherits(y, "mdl_tbl"))
	expect_s3_class(y, "data.frame")

	# slice/base subsetting reconcile the same way
	y <- x[3:4, ]
	expect_s3_class(y, "mdl_tbl")
	expect_equal(nrow(attr(y, "formulaMatrix")), 2)

})

test_that("bind_rows works within a table and downgrades loudly across tables", {

	m1 <-
		fmls(mpg ~ wt + hp, pattern = "sequential") |>
		fit(.fn = lm, data = mtcars, raw = FALSE) |>
		model_table(data = mtcars)
	m2 <-
		fmls(hp ~ cyl) |>
		fit(.fn = lm, data = mtcars, raw = FALSE) |>
		model_table()

	# Rows the first table already knows about reconcile fine
	y <- dplyr::bind_rows(m1, m1[2, ])
	expect_s3_class(y, "mdl_tbl")
	expect_equal(nrow(y), 3)
	expect_equal(nrow(attr(y, "formulaMatrix")), 3)

	# Unrelated tables cannot be reconciled by bind_rows (it strips the
	# attributes); the user is pointed to `model_table()`
	expect_message(
		y <- dplyr::bind_rows(m1, m2),
		regexp = "model_table\\(x, y\\)"
	)
	expect_false(inherits(y, "mdl_tbl"))

	# ... and `model_table()` does reconcile them
	y <- model_table(m1, m2)
	expect_s3_class(y, "mdl_tbl")
	expect_equal(nrow(attr(y, "formulaMatrix")), nrow(y))

})

test_that("attributes of models will adjust appropriately", {

	# Sequential/stratified models
	m1 <-
		fmls(mpg ~ wt + hp + cyl + .s(am), pattern = "sequential") |>
		fit(.fn = lm, data = mtcars, raw = FALSE) |>
		model_table()
	expect_length(m1, 19)
	expect_equal(nrow(m1), 6)
	# Four modeling terms plus the ridden-along stratum column
	expect_length(attr(m1, "formulaMatrix"), 5)
	expect_equal(nrow(attr(m1, "termTable")), 5)

	m2 <-
		fmls(wt ~ mpg + cyl, pattern = "parallel") |>
		fit(.fn = lm, data = mtcars, raw = FALSE) |>
		model_table()

	# Combining tables
	m3 <- vec_c(m1, m2)
	expect_s3_class(m3, "mdl_tbl")
	expect_equal(nrow(m3), 8)
	expect_length(attr(m3, "formulaMatrix"), 5)
	expect_equal(nrow(attr(m3, "termTable")), 7)

	# Filtering tables prunes terms to the roles the rows still claim,
	# including strata (the old M5 TODO)
	m4 <- filter(m3, outcome == "wt")
	expect_s3_class(m4, "mdl_tbl")
	expect_equal(nrow(m4), 2)
	expect_length(attr(m4, "formulaMatrix"), 3)
	expect_length(attr(m4, "termTable")$term, 3)
	expect_false("am" %in% attr(m4, "termTable")$term)
	expect_false("mpg" %in%
							 	with(attr(m4, "termTable"), term[role == "outcome"]))

})

test_that("data can be attached to a model table", {

	# Should be able to attach data secondarily
	x <-
		fmls(mpg ~ wt + hp + cyl + .s(am), pattern = "sequential") |>
		fit(.fn = lm, data = mtcars, raw = FALSE) |>
		model_table()

	y <- attach_data(x, data = mtcars)
	# Attaching a dataset no model references says so
	expect_message(
		z <- attach_data(y, data = lung),
		regexp = "attached for later use"
	)
	dat <- attr(z, "dataList")
	expect_length(dat, 2)
	expect_named(dat, c("mtcars", "lung"))

	# Re-attaching under the same name says so as well
	expect_message(
		z2 <- attach_data(z, data = mtcars),
		regexp = "Replacing"
	)
	expect_length(attr(z2, "dataList"), 2)

	# Model should also take data directly
	x <-
		fmls(mpg ~ wt + hp + cyl + .s(am), pattern = "sequential") |>
		fit(.fn = lm, data = mtcars, raw = FALSE)

	m <- model_table(x, data = mtcars)
	dat <- attr(m, "dataList")
	expect_named(dat, "mtcars")
	expect_length(dat, 1)

	# A deliberately attached dataset survives filtering; unreferenced
	# leftovers of removed models do not
	skip_if_not_installed("survival")
	withData <- suppressMessages(attach_data(m, data = lung))
	filtered <- filter(withData, formula_call == "mpg ~ wt")
	expect_named(attr(filtered, "dataList"), c("mtcars", "lung"))

})

test_that("table can be simplified or flattened", {

	library(survival) # Using lung data
	f <- Surv(time, status) ~ ph.karno + meal.cal + cluster(sex)
	object <- fmls(f, pattern = 'sequential')
	m <- fit(object, .fn = coxph, data = lung, raw = FALSE)
	x <- model_table(m)

	# Cox models are inferred onto the ratio scale, with a message
	expect_message(
		y <- flatten_models(x),
		regexp = "ratio scale"
	)

	expect_s3_class(y, "data.frame")
	expect_equal(min(y$number), 1)
	expect_equal(max(y$number), 3)
	expect_type(y$var_cov[[1]], 'double')
	expect_true(inherits(y$var_cov[[1]], 'matrix'))
	expect_true(all(y$exponentiated))

	# The inference matches explicit exponentiation
	raw <- flatten_models(x, exponentiate = FALSE)
	expect_false(any(raw$exponentiated))
	expect_equal(y$estimate, exp(raw$estimate))

	# Now for multiple tables and flatten_model selections
	m1 <-
	  fmls(am ~ wt) |>
	  fit(.fn = glm, family = "binomial", data = mtcars, raw = FALSE)

	m2 <-
	  fmls(am ~ wt) |>
	  fit(.fn = glm, family = "binomial", data = mtcars, raw = FALSE)

	mt <- model_table(log = m1, exp = m2)
	fm <- flatten_models(mt, exponentiate = TRUE, which = "exp")

	expect_equal(exp(fm$estimate[2]), fm$estimate[4])

})

test_that("flatten_models infers exponentiation from the model family", {

	logistic <-
		fmls(am ~ wt) |>
		fit(.fn = glm, family = "binomial", data = mtcars, raw = FALSE)
	linear <-
		fmls(mpg ~ wt) |>
		fit(.fn = lm, data = mtcars, raw = FALSE)

	mixed <- model_table(logit = logistic, lin = linear)

	expect_message(
		fl <- flatten_models(mixed),
		regexp = "glm\\(logit\\)"
	)

	# Binomial glm exponentiates; lm does not
	expect_true(all(fl$exponentiated[fl$name == "logit"]))
	expect_false(any(fl$exponentiated[fl$name == "lin"]))

	raw <- flatten_models(mixed, exponentiate = FALSE)
	expect_equal(
		fl$estimate[fl$name == "logit"],
		exp(raw$estimate[raw$name == "logit"])
	)
	expect_equal(
		fl$estimate[fl$name == "lin"],
		raw$estimate[raw$name == "lin"]
	)

	# Unfit rows are dropped with a message that says where to look
	unfitted <- model_table(fmls(mpg ~ wt))
	both <- model_table(mixed, unfitted)
	msgs <- capture_messages(fl2 <- flatten_models(both, exponentiate = FALSE))
	expect_true(any(grepl("unfit formula", msgs)))
	expect_equal(nrow(fl2), nrow(raw))

})

test_that("model table can be filtered", {

	# Two messages here
	expect_message(expect_message(
		f <- vec_c(
			fmls(hp + mpg ~ .x(wt) + .i(am) + cyl),
			fmls(hp + mpg ~ .x(wt) + .i(vs) + cyl)
		)
	))
	m <- fit(f, .fn = lm, data = mtcars, raw = FALSE)
	object <- model_table(linear = m, data = mtcars)

	# Filtering example
	x <- object
	to <- object[object$outcome == "hp", ]
	expect_length(to$outcome, 2)

	# Filtering
	obj <-
		object |>
		dplyr::filter(outcome == "hp")

	expect_length(obj$outcome, 2)
})

test_that("filtering keeps the term definitions the remaining rows point at", {
	d <- mtcars
	d$cyl <- factor(d$cyl)
	d$am <- factor(d$am)
	# `am` is the first family's modifier and the second's exposure, so the
	# combined term table defines it twice and the second family's cells
	# point at the second definition. Filtering to that family used to keep
	# the wrong definition and leave the ordinal pointing past the table
	mt <- c(
		fmls(mpg ~ .x(hp) + .i(am) + cyl),
		fmls(mpg ~ .x(am) + .i(cyl) + hp)
	) |>
		fit(.fn = lm, data = d) |>
		model_table(data = d)
	sub <- dplyr::filter(mt, exposure == "am")
	proxy <- vctrs::vec_proxy(term_table(sub))
	expect_equal(proxy$role[proxy$term == "am"], "exposure")
	expect_equal(proxy$role[proxy$term == "cyl"], "interaction")
	expect_equal(proxy$role[proxy$term == "hp"], "predictor")
	# And the formulas still resolve, which is what `mdl_gt()` needs
	expect_equal(
		unname(as.character(formula(model_table_formulas(sub)))),
		"mpg ~ am + cyl + am:cyl + hp"
	)
})
