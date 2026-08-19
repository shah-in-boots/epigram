test_that("coefficient effects have stable semantic identity and reference anchors", {
	effects <- regression_mdl_tbl() |>
		mdl_gt() |>
		inspect_mdl_gt("effects")

	expect_named(effects, recast:::effect_frame_fields())
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
