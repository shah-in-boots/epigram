test_that("as_gt renders a normal gt table from semantic cells", {
	g <- regression_mdl_tbl() |>
		mdl_gt() |>
		add_estimates() |>
		add_n() |>
		as_gt()
	expect_s3_class(g, "gt_tbl")
	expect_true(all(c("Model 1", "Model 2") %in% g[["_data"]]$row_label))
	html <- gt::as_raw_html(g)
	expect_match(html, "Estimate (95% CI)", fixed = TRUE)
	expect_match(html, ">N<")
})

test_that("forest axes are renderer footers, not semantic effects", {
	spec <- regression_mdl_tbl() |>
		mdl_gt() |>
		add_estimates(columns = list(beta ~ "B", conf ~ "CI")) |>
		add_forest(axis = list(title = "Difference"))
	cells <- inspect_mdl_gt(spec, "cells")
	expect_false(any(vapply(cells$row_path, function(x) ".axis" %in% x,
		logical(1))))
	expect_true(any(cells$renderer == "forest"))
	expect_true(all(cells$type[cells$group_id == "forest"] %in%
		c("plot", "reference")))

	g <- as_gt(spec)
	expect_s3_class(g, "gt_tbl")
	expect_equal(tail(g[["_data"]]$row_label, 1), "")
})

test_that("forest and text groups present identical estimate measures", {
	groups <- regression_mdl_tbl() |>
		mdl_gt() |>
		add_estimates(columns = list(beta ~ "B", conf ~ "CI")) |>
		add_forest() |>
		inspect_mdl_gt("groups")
	text <- groups[groups$group_id == "effect", ]
	forest <- groups[groups$group_id == "forest", ]
	forest <- forest[match(text$effect_id, forest$effect_id), ]
	for (stat in c("estimate", "conf_low", "conf_high")) {
		expect_equal(
			vapply(text$value, `[[`, numeric(1), stat),
			vapply(forest$value, `[[`, numeric(1), stat)
		)
	}
})

test_that("group-scoped p-values render within each nested condition band", {
	spec <- interaction_mdl_tbl(stratified = TRUE) |>
		mdl_gt() |>
		add_interaction() |>
		add_estimates() |>
		modify_layout(
			rows = c("stratum", "stratum_level", "modifier", "modifier_level"),
			columns = c("term", "contrast")
		)
	g <- as_gt(spec)
	pColumn <- grep("^column_.*703a70$", names(g[["_data"]]), value = TRUE)
	expect_length(pColumn, 1)
	expect_equal(length(unique(g[["_data"]][[pColumn]])), 2)
	styles <- g[["_styles"]]
	expect_true(nrow(styles) > 0)
})

test_that("journal styling is built in and gt remains the escape hatch", {
	spec <- regression_mdl_tbl() |>
		mdl_gt() |>
		modify_style(
			theme = "journal", widths = list(effect = 160),
			align = list(effect = "right"), reference_text = "Reference"
		)
	g <- as_gt(spec)
	expect_s3_class(g, "gt_tbl")
	expect_equal(g[["_data"]][[3]][1], "Reference")

	finished <- g |>
		gt::tab_header(title = "Journal-ready model table") |>
		gt::tab_source_note("Final bespoke edits remain ordinary gt calls")
	expect_s3_class(finished, "gt_tbl")
	expect_match(gt::as_raw_html(finished), "Journal-ready model table")
})

test_that("the survival presentation composes statistic rows and effect body", {
	skip_if_not_installed("survival")
	g <- survival_mdl_tbl() |>
		mdl_gt() |>
		modify_layout(preset = "levels") |>
		add_events() |>
		add_rate_difference() |>
		add_estimates(columns = list(beta ~ "HR", conf ~ "95% CI")) |>
		as_gt()
	dat <- g[["_data"]]
	expect_equal(dat$row_label[1:2],
		c("Events", "Rate per 100 person-years"))
	expect_true(any(dat$row_label == "Model 1"))
	expect_match(gt::as_raw_html(g), "Rate difference")
})

test_that("a non-finite interval renders as the missing text, not as a magnitude", {
	# A separated logistic fit exponentiates to an interval reaching `Inf`;
	# the estimate beside it is a finite but meaningless number
	value <- list(estimate = 4857318.63, conf_low = 0, conf_high = Inf)
	format <- list(fmt = "number", digits = 2,
								 pattern = "{estimate} ({conf_low}, {conf_high})")
	expect_equal(format_cell(value, format, "—"), "—")
	# A finite interval is untouched
	value$conf_high <- 12.5
	expect_equal(format_cell(value, format, "—"), "4857318.63 (0.00, 12.50)")
})
