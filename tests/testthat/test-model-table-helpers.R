# The print and summary methods are the front door of the model table: they
# should say what was fit, what failed, what is waiting, and what to do next.

test_that("printing a model table reports the state of the fleet", {

	withr::local_options(recast.color = FALSE)

	x <-
		fmls(mpg ~ wt + hp + .s(am), pattern = "sequential") |>
		fit(.fn = lm, data = mtcars, raw = FALSE) |>
		model_table(data = mtcars)

	out <- capture.output(print(x))
	expect_true(any(grepl("<model_table> 4 models", out)))
	expect_true(any(grepl("4 fitted", out)))
	expect_true(any(grepl("mtcars \\[attached\\]", out)))
	expect_true(any(grepl("strata: am", out)))
	expect_true(any(grepl("summary\\(\\)", out)))

	# Stratum provenance shows up per row
	expect_true(any(grepl("am=0", out)))
	expect_true(any(grepl("am=1", out)))

	# The n argument truncates with a pointer to the rest
	out <- capture.output(print(x, n = 2))
	expect_true(any(grepl("2 more models", out)))

})

test_that("printing distinguishes fitted, failed, and unfit models", {

	withr::local_options(recast.color = FALSE)

	fitted <-
		fmls(mpg ~ wt) |>
		fit(.fn = lm, data = mtcars, raw = FALSE)
	failed <-
		fmls(mpg ~ not_a_column) |>
		fit(.fn = lm, data = mtcars, raw = FALSE) |>
		suppressWarnings()
	pending <- fmls(mpg ~ hp)

	x <- model_table(fitted, failed, pending)
	out <- capture.output(print(x))

	expect_true(any(grepl("1 fitted", out)))
	expect_true(any(grepl("1 failed", out)))
	expect_true(any(grepl("1 unfit", out)))
	expect_true(any(grepl("model_failures\\(\\)", out)))
	expect_true(any(grepl("await `fit\\(\\)`", out)))

	# Unattached data is called out
	expect_true(any(grepl("mtcars \\[not attached\\]", out)))

	# An emptied table explains how to begin
	e <- filter(x, outcome == "zzz")
	out <- capture.output(print(e))
	expect_true(any(grepl("No models yet", out)))

})

test_that("summary maps the fleet and explains failures", {

	withr::local_options(recast.color = FALSE)

	fitted <-
		fmls(mpg ~ wt + .s(am)) |>
		fit(.fn = lm, data = mtcars, raw = FALSE)
	failed <-
		fmls(mpg ~ not_a_column) |>
		fit(.fn = lm, data = mtcars, raw = FALSE) |>
		suppressWarnings()

	x <- model_table(main = fitted, broken = failed, data = mtcars)

	out <- capture.output(overview <- summary(x))
	expect_true(any(grepl("<model_table> summary", out)))
	expect_true(any(grepl("terms \\|", out)))
	expect_true(any(grepl("not_a_column", out))) # the failure line

	# The grouped overview comes back invisibly for further use
	expect_s3_class(overview, "tbl_df")
	expect_true(all(c("data", "model", "outcome", "models", "fitted") %in%
									names(overview)))
	expect_equal(sum(overview$models), nrow(x))

})

test_that("model_failures returns the attempted-and-errored models", {

	fitted <-
		fmls(mpg ~ wt) |>
		fit(.fn = lm, data = mtcars, raw = FALSE)
	failed <-
		fmls(mpg ~ not_a_column) |>
		fit(.fn = lm, data = mtcars, raw = FALSE) |>
		suppressWarnings()
	pending <- fmls(mpg ~ hp)

	x <- model_table(ok = fitted, broken = failed, pending)

	fails <- model_failures(x)
	expect_s3_class(fails, "tbl_df")
	expect_equal(nrow(fails), 1) # unfit formulas are not failures
	expect_equal(fails$name, "broken")
	expect_match(fails$error, "not_a_column")

	# A clean table reports no failures
	clean <- model_table(fitted)
	expect_equal(nrow(model_failures(clean)), 0)

})

test_that("accessors expose the table attributes without attr() calls", {

	f <- fmls(mpg ~ .x(wt) + hp + .s(am))
	x <-
		f |>
		fit(.fn = lm, data = mtcars, raw = FALSE) |>
		model_table(data = mtcars)

	# Terms come back as a tm vector with their roles
	tms <- term_table(x)
	expect_s3_class(tms, "tm")
	roles <- vec_proxy(tms)$role
	expect_true("exposure" %in% roles)
	expect_true("strata" %in% roles)

	# The same accessor reads a formula family
	expect_s3_class(term_table(f), "tm")

	# The formula matrix is the model-by-term membership grid
	fmMat <- formula_matrix(x)
	expect_s3_class(fmMat, "tbl_df")
	expect_equal(nrow(fmMat), nrow(x))
	expect_true(all(c("mpg", "wt", "hp") %in% names(fmMat)))
	expect_s3_class(formula_matrix(f), "tbl_df")

	# Data recall by name (the frame attaches whole), with a helpful error
	# otherwise
	expect_named(model_data(x), "mtcars")
	expect_identical(model_data(x, "mtcars"), mtcars)
	expect_error(model_data(x, "lung"), regexp = "attached: mtcars")

})

test_that("mixed models flow through the flattening inference", {

	skip_if_not_installed("lme4")
	skip_if_not_installed("broom.mixed")

	x <-
		fmls(mpg ~ .x(wt) + .r(cyl)) |>
		fit(.fn = lme4::lmer, data = mtcars, raw = FALSE) |>
		model_table()

	# A gaussian mixed model stays on the linear scale, silently
	expect_silent(fl <- flatten_models(x))
	expect_false(any(fl$exponentiated))

})

test_that("attach_data() aliases the same frame arriving under another name", {

	d <- mtcars
	mt <- fit(fmls(mpg ~ .x(wt) + hp), .fn = lm, data = d) |>
		model_table()

	renamed <- d
	expect_message(
		mt2 <- attach_data(mt, renamed),
		"attaching it as `d`"
	)
	expect_equal(names(attr(mt2, "dataList")), "d")

	# An explicit `name` is always honored as given
	expect_message(
		mt3 <- attach_data(mt, renamed, name = "renamed"),
		"attached for later use"
	)
	expect_equal(names(attr(mt3, "dataList")), "renamed")

	# A frame missing the models' variables is not aliased
	unrelated <- data.frame(a = 1:3)
	expect_message(
		mt4 <- attach_data(mt, unrelated),
		"attached for later use"
	)
	expect_equal(names(attr(mt4, "dataList")), "unrelated")

})

test_that("attach_data() keeps the whole frame for later reach", {

	# Columns no current formula names stay available — a follow-up column
	# for add_events(), a variable for the next family of models
	d <- mtcars
	m <- fit(fmls(mpg ~ .x(wt) + hp + .s(am)), .fn = lm, data = d)
	mt <- attach_data(model_table(m), d)
	expect_identical(model_data(mt, "d"), d)

})

# Paring ---------------------------------------------------------------

pare_table <- function() {
	d <- mtcars
	suppressMessages(
		c(
			fmls(mpg ~ .x(wt) + hp + cyl, pattern = "sequential"),
			fmls(mpg ~ .x(disp) + hp + cyl, pattern = "sequential"),
			fmls(qsec ~ .x(am))
		) |>
			fit(.fn = lm, data = d, raw = FALSE) |>
			model_table(data = d)
	)
}

test_that("keep/drop verbs pare by causal role, bare names or strings", {

	mt <- pare_table()

	expect_message(kept <- keep_outcomes(mt, mpg), "Kept 6 of 7")
	expect_s3_class(kept, "mdl_tbl")
	expect_equal(unique(kept$outcome), "mpg")

	# Strings reach the same place; drop is the exact complement
	viaString <- suppressMessages(keep_outcomes(mt, "mpg"))
	expect_equal(kept$id, viaString$id)
	dropped <- suppressMessages(drop_outcomes(mt, qsec))
	expect_equal(kept$id, dropped$id)

	# Several terms combine as OR; verbs chain as AND
	both <- suppressMessages(keep_exposures(mt, wt, disp))
	expect_equal(nrow(both), 6L)
	one <- suppressMessages(mt |> keep_outcomes(mpg) |> keep_exposures(disp))
	expect_equal(unique(one$exposure), "disp")
	expect_equal(nrow(suppressMessages(drop_exposures(mt, wt, disp, am))), 0L)

	# A typo errors with what is available, never a silent empty keep --
	# and an empty call teaches the vocabulary
	expect_error(keep_outcomes(mt, mpgg), "Available outcome")
	expect_error(drop_exposures(mt, wt2), "Available exposure")
	expect_error(keep_outcomes(mt), "This table holds")
})

test_that("keep_families() pares by the identified family structure", {

	mt <- pare_table()

	# The table always carries family/pattern/relation, so every selector reads
	# straight off the rows
	related <- suppressMessages(keep_families(mt, relation = "varied exposures"))
	expect_equal(unique(related$outcome), "mpg")
	expect_equal(nrow(related), 6L)
	ladders <- suppressMessages(keep_families(mt, pattern = "sequential"))
	expect_equal(nrow(ladders), 6L)

	# family ids work directly, no prior stamp; an empty call still errors
	expect_error(keep_families(mt), "needs a family id")
	fam2 <- suppressMessages(keep_families(mt, 2))
	expect_equal(unique(fam2$exposure), "disp")

	# And the pared result passes the mdl_gt gate
	expect_true(S7::S7_inherits(mdl_gt(suppressMessages(
		keep_families(mt, relation = "varied exposures")
	)), mdl_gt))
})

test_that("restrict_to() narrows the population and the dataset", {

	d <- mtcars
	mt <- suppressMessages(
		fmls(mpg ~ .x(wt) + hp) |>
			add_strata(am) |>
			fit(.fn = lm, data = d, raw = FALSE) |>
			model_table(data = d)
	)

	byLevel <- suppressMessages(restrict_to(mt, level = 1))
	expect_equal(unique(byLevel$level), "1")
	byStrata <- suppressMessages(restrict_to(mt, strata = am))
	expect_equal(nrow(byStrata), nrow(mt))
	byData <- suppressMessages(restrict_to(mt, data = d))
	expect_equal(nrow(byData), nrow(mt))

	expect_error(restrict_to(mt), "population dimension")
	expect_error(restrict_to(mt, level = 9), "Available level")
})

test_that("adjusting_for() and excluding() pare by the adjustment terms", {

	mt <- pare_table()

	# adjusting_for keeps models whose set carries every named covariate
	adj <- suppressMessages(adjusting_for(mt, hp))
	expect_equal(nrow(adj), 4L)
	full <- suppressMessages(adjusting_for(mt, hp, cyl))
	expect_equal(nrow(full), 2L)
	expect_true(all(grepl("cyl", full$formula_call)))
	expect_error(adjusting_for(mt), "adjustment_sets")
	expect_error(adjusting_for(mt, gear), "Available adjustment covariate")

	# excluding keeps models whose formulas avoid the terms, whatever the role
	noCyl <- suppressMessages(excluding(mt, cyl))
	expect_false(any(grepl("cyl", noCyl$formula_call)))
	noWt <- suppressMessages(excluding(mt, wt))
	expect_false("wt" %in% noWt$exposure) # exposures count too
	expect_error(excluding(mt, nothing), "Available term")

	# The verbs chain into a gate-ready analysis
	ready <- suppressMessages(
		mt |> keep_outcomes(mpg) |> adjusting_for(hp)
	)
	expect_true(S7::S7_inherits(mdl_gt(ready), mdl_gt))
})

test_that("adjustment_sets() shows the rungs select_adjustment() picks from", {

	mt <- pare_table()
	rungs <- adjustment_sets(mt)

	expect_s3_class(rungs, "tbl_df")
	expect_equal(rungs$adjustment, 1:3)
	expect_equal(rungs$covariates[1], "(unadjusted)")
	expect_equal(rungs$adds[2], "+ hp")
	expect_equal(rungs$adds[3], "+ cyl")
	# Both mpg ladders share every rung; the stray direct model shares the
	# unadjusted rung
	expect_equal(rungs$models, c(3L, 2L, 2L))

	# The mdl_gt method reads the specification's fitted models
	spec <- mdl_gt(suppressMessages(keep_outcomes(mt, mpg)))
	expect_equal(adjustment_sets(spec)$adjustment, 1:3)
})
