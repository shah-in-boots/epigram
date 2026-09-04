test_that("coefficient effects have stable semantic identity and reference anchors", {
	effects <- regression_mdl_tbl() |>
		mdl_gt() |>
		inspect_mdl_gt("effects")

	# The effect frame is the contract every later stage reads: each semantic
	# dimension with its label beside it, the statistics, and the sort keys
	expect_named(effects, c(
		"effect_id", "model", "source", "outcome", "outcome_label", "term",
		"term_label", "contrast", "contrast_label", "adjustment",
		"adjustment_label", "modifier", "modifier_label", "modifier_level",
		"modifier_level_label", "stratum", "stratum_label", "stratum_level",
		"stratum_level_label", "subset", "dataset", "family", "is_reference",
		"estimate", "conf_low", "conf_high", "p_value", "nobs", "exponentiated",
		"outcome_order", "term_order", "contrast_order", "adjustment_order",
		"modifier_order", "modifier_level_order", "stratum_order", "model_order"
	))
	expect_equal(nrow(effects), 6)
	expect_equal(sum(effects$is_reference), 2)
	expect_false(anyNA(effects$model))
	expect_setequal(unique(effects$contrast), c("4", "6", "8"))
	expect_equal(unique(effects$term), "cyl")
	expect_equal(unique(effects$outcome), "mpg")
})

test_that("multiple outcomes and wide categorical terms share one effect schema", {
	multi <- multi_outcome_mdl_tbl() |>
		mdl_gt() |>
		inspect_mdl_gt("effects")
	expect_setequal(unique(multi$outcome), c("mpg", "qsec"))
	expect_setequal(unique(multi$contrast), c("4", "6", "8"))

	wide <- wide_categorical_mdl_tbl() |>
		mdl_gt() |>
		inspect_mdl_gt("effects")
	expect_setequal(unique(wide$term), c("cyl", "am"))
	expect_setequal(unique(wide$contrast), c("4", "6", "8", "0", "1"))
})

test_that("the built-in group registry declares its semantic contract", {
	registry <- recast:::mdl_gt_group_registry()
	expect_named(registry, recast:::mdl_gt_group_ids())
	for (entry in registry) {
		expect_named(entry, c(
			"required_measures", "grain", "supported_axes", "supported_views",
			"default_format", "renderer", "default_axis", "order", "materialize"
		))
		expect_true(entry$default_axis %in% entry$supported_axes)
		expect_type(entry$materialize, "closure")
	}
})

test_that("atomic measures are independent of presentation", {
	base <- mdl_gt(regression_mdl_tbl())
	wide <- base |>
		add_estimates() |>
		add_n()
	body <- wide |>
		place_cells(effect, axis = "body")

	expect_equal(inspect_mdl_gt(wide, "effects"), inspect_mdl_gt(body, "effects"))
	expect_equal(inspect_mdl_gt(wide, "measures"), inspect_mdl_gt(body, "measures"))
	m <- inspect_mdl_gt(wide, "measures")
	expect_setequal(unique(m$statistic),
		c("estimate", "conf_low", "conf_high", "p_value", "n"))
	expect_equal(unique(m$grain[m$statistic == "n"]), "model")
})

test_that("interaction models produce conditional effects and group measures", {
	spec <- interaction_mdl_tbl() |>
		mdl_gt() |>
		add_interaction() |>
		add_n() |>
		add_estimates()
	b <- inspect_mdl_gt(spec, "all")

	expect_equal(unique(b$effects$term), "hp")
	expect_equal(unique(b$effects$modifier), "cyl")
	expect_equal(b$effects$modifier_level, c("4", "6", "8"))
	expect_equal(nrow(b$conditions[b$conditions$condition_kind == "modifier", ]), 3)
	p <- b$measures[b$measures$statistic == "p_value", ]
	expect_equal(nrow(p), 1)
	expect_equal(p$grain, "modifier_group")
	n <- b$measures[b$measures$statistic == "n", ]
	expect_equal(n$grain, rep("condition", 3))

	ref <- estimate_interaction(
		interaction_mdl_tbl(), exposure = "hp", interaction = "cyl"
	)
	expect_equal(b$effects$estimate, ref$estimate)
	expect_equal(b$effects$conf_low, ref$conf_low)
	expect_equal(b$effects$conf_high, ref$conf_high)
})

test_that("all declared modifiers are discovered from formula membership", {
	d <- table_fixture_data()
	mt <- fmls(mpg ~ .x(hp) + .i(cyl) + .i(am)) |>
		fit(.fn = lm, data = d) |>
		model_table(data = d)
	effects <- mt |>
		mdl_gt() |>
		add_interaction() |>
		inspect_mdl_gt("effects")
	expect_setequal(unique(effects$modifier), c("cyl", "am"))
})

test_that("strata and modifiers coexist as normalized conditions", {
	spec <- interaction_mdl_tbl(stratified = TRUE) |>
		mdl_gt() |>
		add_interaction() |>
		add_estimates() |>
		modify_layout(
			rows = c("stratum", "stratum_level", "modifier", "modifier_level"),
			columns = c("term", "contrast")
		)
	b <- inspect_mdl_gt(spec, "all")
	expect_setequal(unique(b$conditions$condition_kind), c("stratum", "modifier"))
	expect_setequal(unique(b$effects$stratum_level), c("0", "1"))
	expect_setequal(unique(b$effects$modifier_level), c("4", "6", "8"))
	expect_equal(nrow(b$effects), 6)
})

test_that("event, rate, and rate-difference measures retain their grains", {
	skip_if_not_installed("survival")
	spec <- survival_mdl_tbl() |>
		mdl_gt() |>
		add_events() |>
		add_rate_difference() |>
		add_estimates(columns = list(beta ~ "HR", conf ~ "95% CI"))
	m <- inspect_mdl_gt(spec, "measures")

	expect_setequal(
		unique(m$statistic[m$group_id %in% c("events", "rate")]),
		c("events", "rate")
	)
	expect_true(all(is.na(m$adjustment[m$group_id %in% c("events", "rate")])))
	expect_equal(unique(m$grain[m$group_id == "rate_difference"]), "term")

	d <- survival::lung
	d$sex <- factor(d$sex, levels = 1:2, labels = c("Male", "Female"))
	py <- survival::pyears(
		survival::Surv(time, status) ~ sex, data = d, scale = 365.25
	)
	events <- m[m$statistic == "events", ]
	expect_equal(events$value, as.numeric(py$event))
})

test_that("a modifier belongs only to models whose formula crosses it with the exposure", {
	d <- table_fixture_data()
	# Each family names the other's modifier as a plain covariate. The term
	# table is table-wide, so attributing modifiers by role and membership
	# alone gave both modifiers to both models and sent
	# `estimate_interaction()` after an `hp:am` coefficient the first model
	# never had
	mt <- c(
		fmls(mpg ~ .x(hp) + .i(cyl) + am),
		fmls(mpg ~ .x(hp) + .i(am) + cyl)
	) |>
		fit(.fn = lm, data = d) |>
		model_table(data = d)
	effects <- mt |>
		mdl_gt() |>
		add_interaction() |>
		inspect_mdl_gt("effects")
	modifiersPerModel <- vapply(
		split(effects$modifier, effects$model),
		function(m) length(unique(m)),
		integer(1)
	)
	expect_equal(unname(modifiersPerModel), c(1L, 1L))
	expect_setequal(unique(effects$modifier), c("cyl", "am"))
})

test_that("a selected adjustment rung without the interaction term is refused, not dropped", {
	d <- table_fixture_data()
	# Rung 1 is the bare `mpg ~ hp`; the modifier only enters at rung 2, so
	# an "Unadjusted" label on rung 1 used to vanish from the table silently
	mt <- fmls(mpg ~ .x(hp) + .i(cyl) + wt, pattern = "sequential") |>
		fit(.fn = lm, data = d) |>
		model_table(data = d)
	spec <- mt |>
		mdl_gt() |>
		add_interaction() |>
		select_adjustment(1 ~ "Unadjusted", 3 ~ "Adjusted")
	expect_error(
		inspect_mdl_gt(spec, "effects"),
		"Adjustment set\\(s\\) 1 \\(`Unadjusted`\\) carry no interaction term"
	)
	# A bare mesa over the same ladder sets the rung aside with a message and
	# lays out the rest: exploration should not stop on the crude rung
	expect_message(
		effectsBare <- mt |> mdl_gt() |> add_interaction() |> inspect_mdl_gt("effects"),
		"Setting aside 1 model"
	)
	expect_setequal(unique(effectsBare$adjustment), c(2L, 3L))
	# The rungs that carry the modifier still lay out
	effects <- mt |>
		mdl_gt() |>
		add_interaction() |>
		select_adjustment(2 ~ "Crude", 3 ~ "Adjusted") |>
		inspect_mdl_gt("effects")
	expect_setequal(unique(effects$adjustment_label), c("Crude", "Adjusted"))
})

test_that("a bare interaction mesa over a ladder takes the rungs as row bands by itself", {
	d <- table_fixture_data()
	mt <- fmls(mpg ~ .x(hp) + .i(cyl) + wt, pattern = "sequential") |>
		fit(.fn = lm, data = d) |>
		model_table(data = d)
	# Two rungs, no layout declared: the preset alone maps modifier and
	# contrast, so the rungs would collide on one coordinate. They become the
	# outer bands; the single modifier, constant on every row, leaves the stub
	# for the stubhead so a row reads "4", not "cyl › 4"
	spec <- mt |>
		mdl_gt() |>
		add_interaction() |>
		select_adjustment(2 ~ "Crude", 3 ~ "Adjusted") |>
		add_estimates()
	b <- inspect_mdl_gt(spec, "all")
	expect_equal(b$layout$rows, c("adjustment", "modifier", "modifier_level"))
	expect_equal(b$layout$collapsed_rows, "modifier")
	expect_equal(b$layout$stubhead, "cyl")
	body <- b$cells[b$cells$group_id == "effect", ]
	expect_setequal(unique(body$row_group), c("Crude", "Adjusted"))
	expect_setequal(unique(body$row_label), c("4", "6", "8"))
	# And the bands render, the P floating over each
	expect_s3_class(as_gt(spec), "gt_tbl")
	# A layout the user declared without the dimension stays strict
	expect_error(
		spec |>
			modify_layout(rows = c("modifier", "modifier_level"),
										columns = "contrast") |>
			inspect_mdl_gt("cells"),
		"same coordinate"
	)
})

test_that("a categorical exposure's reference level is a contrast column of the interaction table", {
	d <- table_fixture_data()
	mt <- fmls(mpg ~ .x(cyl) + .i(am)) |>
		fit(.fn = lm, data = d) |>
		model_table(data = d)
	b <- mt |>
		mdl_gt() |>
		add_interaction() |>
		add_estimates() |>
		modify_style(reference_text = "Ref.") |>
		inspect_mdl_gt("all")
	# One reference row per modifier level, first among the contrasts
	refs <- b$effects[b$effects$is_reference, ]
	expect_equal(refs$contrast, c("4", "4"))
	expect_setequal(refs$modifier_level, c("0", "1"))
	expect_true(all(is.na(refs$estimate)))
	expect_equal(b$effects$contrast[order(b$effects$contrast_order)][1], "4")
	cells <- b$cells[b$cells$group_id == "effect" & b$cells$type == "reference", ]
	expect_equal(nrow(cells), 2)
})

test_that("N leads and P trails within each adjustment spanner", {
	d <- table_fixture_data()
	mt <- c(
		fmls(mpg ~ .x(cyl) + .i(am)),
		fmls(mpg ~ .x(cyl) + .i(am) + hp)
	) |>
		fit(.fn = lm, data = d) |>
		model_table(data = d)
	cells <- mt |>
		mdl_gt() |>
		add_interaction() |>
		add_n() |>
		add_estimates() |>
		modify_layout(rows = c("modifier", "modifier_level"),
									columns = c("adjustment", "contrast")) |>
		inspect_mdl_gt("cells")
	cols <- unique(cells[order(cells$column_order), c("column_label", "spanner")])
	first <- cols[startsWith(cols$spanner, "Model 1"), ]
	expect_equal(first$column_label[1], "N")
	expect_equal(first$column_label[nrow(first)], "P value")
	# Standalone too: with no column dimension of its own, P still trails the
	# odds ratios it tests and N still leads them
	single <- mt |>
		dplyr::filter(number == 3) |>
		mdl_gt() |>
		add_interaction() |>
		add_n() |>
		add_estimates() |>
		inspect_mdl_gt("cells")
	labels <- unique(single$column_label[order(single$column_order)])
	expect_equal(labels[1], "N")
	expect_equal(labels[length(labels)], "P value")
})

test_that("a bare interaction mesa over strata takes the strata as row bands", {
	d <- table_fixture_data()
	mt <- fmls(mpg ~ .x(hp) + .i(cyl) + .s(am)) |>
		fit(.fn = lm, data = d) |>
		model_table(data = d)
	b <- mt |>
		mdl_gt() |>
		add_interaction() |>
		add_estimates() |>
		inspect_mdl_gt("all")
	expect_equal(b$layout$rows,
							 c("stratum", "stratum_level", "modifier", "modifier_level"))
	expect_setequal(unique(b$cells$row_group[b$cells$group_id == "effect"]),
									c("am", "am"))
})

test_that("modify_labels() reaches the stratum and its levels", {
	d <- mtcars
	d$am <- factor(d$am)
	mt <- fmls(mpg ~ .x(wt) + .s(am)) |>
		fit(.fn = lm, data = d) |>
		model_table(data = d) |>
		suppressMessages()
	# The label used to be stored and never read: the stratum band rendered
	# the bare column name whatever `modify_labels()` was told
	b <- mt |>
		mdl_gt() |>
		modify_labels(am ~ "Transmission") |>
		inspect_mdl_gt("all")
	stratum <- b$conditions[b$conditions$condition_kind == "stratum", ]
	expect_equal(unique(stratum$label), "Transmission")
	expect_equal(unique(b$cells$row_group[b$cells$group_id == "effect"]),
							 "Transmission")
	# A vector relabels the levels in order, and a bare level name relabels
	# that level wherever it sits
	levels <- mt |>
		mdl_gt() |>
		modify_labels(am ~ c("Automatic", "Manual")) |>
		inspect_mdl_gt("conditions")
	levels <- levels[levels$condition_kind == "stratum", ]
	expect_equal(levels$level_label[match(c("0", "1"), levels$level)],
							 c("Automatic", "Manual"))
	one <- mt |>
		mdl_gt() |>
		modify_labels(`1` ~ "Manual") |>
		inspect_mdl_gt("conditions")
	expect_equal(unique(one$level_label[one$level == "1"]), "Manual")
})

test_that("an estimate whose interval could not be bounded is blank, not a magnitude", {
	# Complete separation: the profile likelihood never finds the upper limit
	# of `z`, so `confint()` returns `NA` there while the estimate is a finite
	# number in the tens (an odds ratio in the billions once exponentiated)
	d <- data.frame(y = c(0, 0, 0, 1, 1, 1), z = 1:6)
	mt <- fit(fmls(y ~ .x(z)), .fn = glm, family = binomial(), data = d) |>
		model_table(data = d)
	spec <- mdl_gt(mt) |> add_estimates(view = "separate")
	eff <- inspect_mdl_gt(spec, "effects")
	expect_true(is.na(eff$estimate))
	expect_true(is.na(eff$conf_low))
	# In the separate view the estimate column used to carry the number alone
	cells <- inspect_mdl_gt(spec, "cells")
	expect_true(all(vapply(cells$value, function(v) all(is.na(unlist(v))),
												 logical(1))))
	# An engine with no interval at all keeps its estimates: every bound `NA`
	# is the absence of a method, not a failed profile
	plain <- fit(fmls(mpg ~ .x(wt)), .fn = lm, data = mtcars) |>
		model_table(data = mtcars)
	plain$model_parameters[[1]]$conf_low <- NA_real_
	plain$model_parameters[[1]]$conf_high <- NA_real_
	expect_false(anyNA(inspect_mdl_gt(mdl_gt(plain), "effects")$estimate))
})
