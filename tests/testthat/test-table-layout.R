test_that("the cell frame carries semantic ids and unencoded paths", {
	cells <- regression_mdl_tbl() |>
		mdl_gt() |>
		add_estimates() |>
		inspect_mdl_gt("cells")

	expect_named(cells, c(
		"cell_id", "effect_id", "group_id", "subgroup", "scope", "scope_id",
		"row_id", "row_group", "row_label", "row_path", "row_path_key",
		"row_order", "row_kind", "row_indent", "column_id", "column_label",
		"column_path", "column_order", "spanner", "renderer", "type", "value",
		"format"
	))
	expect_type(cells$row_path, "list")
	expect_type(cells$column_path, "list")
	expect_false(any(cells$row_label == ".axis"))
	expect_true(all(grepl("^cell_", cells$cell_id)))
})

test_that("adjustment, levels, and interaction are declarative placements", {
	adjustment <- regression_mdl_tbl() |>
		mdl_gt() |>
		add_n() |>
		add_estimates() |>
		inspect_mdl_gt("all")
	expect_equal(adjustment$layout$rows, c("outcome", "adjustment"))
	expect_equal(adjustment$layout$columns, c("term", "contrast"))
	expect_true(all(adjustment$cells$group_id == "n" |
		adjustment$cells$column_label != ""))

	skip_if_not_installed("survival")
	levels <- survival_mdl_tbl() |>
		mdl_gt() |>
		modify_layout(preset = "levels") |>
		add_events() |>
		add_estimates(columns = list(beta ~ "HR", conf ~ "95% CI")) |>
		inspect_mdl_gt("all")
	expect_equal(levels$layout$placements$effect$axis, "body")
	expect_equal(levels$layout$placements$events$axis, "rows")
	expect_true(all(levels$cells$row_label[levels$cells$group_id == "events"] ==
		"Events"))

	interaction <- interaction_mdl_tbl() |>
		mdl_gt() |>
		add_interaction() |>
		add_estimates() |>
		inspect_mdl_gt("all")
	expect_equal(interaction$layout$rows, c("modifier", "modifier_level"))
	expect_equal(unique(interaction$cells$scope[interaction$cells$group_id == "p"]),
		"group")
})

test_that("cell groups move without changing effects or measures", {
	base <- regression_mdl_tbl() |>
		mdl_gt() |>
		add_estimates() |>
		add_n()
	moved <- base |>
		place_cells(effect, axis = "body") |>
		place_cells(n, axis = "rows", .before = "body")

	expect_equal(inspect_mdl_gt(base, "effects"), inspect_mdl_gt(moved, "effects"))
	expect_equal(inspect_mdl_gt(base, "measures"), inspect_mdl_gt(moved, "measures"))
	expect_false(identical(inspect_mdl_gt(base, "cells"),
		inspect_mdl_gt(moved, "cells")))
	n_rows <- inspect_mdl_gt(moved, "cells")$row_label[
		inspect_mdl_gt(moved, "cells")$group_id == "n"]
	expect_true(all(endsWith(n_rows, "N")))
})

test_that("unplaced varying dimensions fail with an actionable diagnostic", {
	spec <- interaction_mdl_tbl(stratified = TRUE) |>
		mdl_gt() |>
		add_interaction() |>
		add_estimates()
	# A declared layout that leaves the varying stratum unplaced is refused
	# with the dimension named; the bare mesa places it as the outer band
	# itself (see test-table-build.R)
	declared <- spec |>
		modify_layout(rows = c("modifier", "modifier_level"),
									columns = c("term", "contrast"))
	expect_error(inspect_mdl_gt(declared, "cells"), "stratum_level")
	expect_no_error(inspect_mdl_gt(spec, "cells"))

	placed <- spec |>
		modify_layout(
			rows = c("stratum", "stratum_level", "modifier", "modifier_level"),
			columns = c("term", "contrast")
		)
	expect_no_error(inspect_mdl_gt(placed, "cells"))
})

test_that("body coordinate collisions name the responsible effects", {
	spec <- regression_mdl_tbl() |>
		mdl_gt() |>
		add_estimates() |>
		add_forest() |>
		place_cells(effect, forest, axis = "body")
	expect_error(inspect_mdl_gt(spec, "cells"),
		"same coordinate.*effects `effect_", perl = TRUE)
})

test_that("group-scoped cells retain the complete semantic band id", {
	spec <- interaction_mdl_tbl(stratified = TRUE) |>
		mdl_gt() |>
		add_interaction() |>
		add_estimates() |>
		modify_layout(
			rows = c("stratum", "stratum_level", "modifier", "modifier_level"),
			columns = c("term", "contrast")
		)
	p <- inspect_mdl_gt(spec, "cells") |>
		dplyr::filter(group_id == "p")
	expect_equal(nrow(p), 2)
	expect_true(all(grepl("stratum_level", p$scope_id, fixed = TRUE)))
	expect_true(all(grepl("modifier=cyl", p$scope_id, fixed = TRUE)))
})
