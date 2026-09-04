# Compiling semantic effects and cell groups into table cells -----------------
#
# Presets are declarative defaults. Every built-in group is independently
# placed on rows, columns, or directly in the semantic body, and one compiler
# handles adjustment, levels, interaction, strata, and their combinations.

#' Declarative preset definitions
#' @keywords internal
#' @noRd
mdl_gt_preset <- function(preset) {
	presets <- list(
		adjustment = list(
			rows = c("outcome", "adjustment"),
			columns = c("term", "contrast"),
			placements = list()
		),
		levels = list(
			rows = c("outcome", "adjustment"),
			columns = c("term", "contrast"),
			placements = list(
				effect = list(axis = "body"),
				events = list(axis = "rows", before = "body"),
				rate = list(axis = "rows", before = "body", after = "events"),
				rate_difference = list(axis = "columns", row_anchor = "rate")
			)
		),
		interaction = list(
			rows = c("modifier", "modifier_level"),
			columns = "contrast",
			placements = list()
		)
	)
	out <- presets[[preset]]
	if (is.null(out)) stop("Unknown layout preset `", preset, "`.", call. = FALSE)
	out
}

#' Resolve axes and group placements without touching measures
#' @keywords internal
#' @noRd
resolve_mdl_gt_layout <- function(x, effects, groups) {
	preset <- mdl_gt_preset(x@layout$preset)
	rows <- first_of(x@layout$rows, preset$rows)
	columns <- first_of(x@layout$columns, preset$columns)
	# A preset maps the dimensions its shape is about. Laid bare over an
	# adjustment ladder or a set of strata it does not map, the rungs or
	# strata would collide on one coordinate, so an undeclared layout takes
	# them as the outer row bands. Rows, not columns: a table twice as wide
	# does not fit a portrait page, and print is where these tables end up.
	# A layout the user declared stays strict and errors at compile with the
	# dimensions named
	varies <- function(d) length(unique(stats::na.omit(effects[[d]]))) > 1
	if (!isTRUE(x@layout$declared)) {
		outer <- character()
		if (!"adjustment" %in% c(rows, columns) && varies("adjustment")) {
			outer <- c(outer, "adjustment")
		}
		if (!"stratum_level" %in% c(rows, columns) && varies("stratum_level")) {
			outer <- c(outer, "stratum", "stratum_level")
		}
		rows <- c(outer, rows)
	}
	# An inner row dimension constant across every effect says the same thing
	# on every row (`NDI quartile › Least Deprivation`); it leaves the stub
	# and names the stubhead instead. The outermost dimension keeps its band
	collapsed <- rows[-1][!vapply(rows[-1], varies, logical(1))]
	collapsed <- collapsed[vapply(collapsed, function(d) {
		v <- dimension_value(effects[1, , drop = FALSE], d)
		!is.na(v) && nzchar(v)
	}, logical(1))]
	stubhead <- paste(vapply(collapsed, function(d) {
		dimension_label(effects[1, , drop = FALSE], d)
	}, character(1)), collapse = " / ")
	active <- unique(groups$group_id)
	registry <- mdl_gt_group_registry()
	placements <- lapply(active, function(id) {
		base <- list(axis = registry[[id]]$default_axis, before = NULL,
			after = NULL, row_anchor = NULL)
		if (!is.null(preset$placements[[id]])) {
			base <- utils::modifyList(base, preset$placements[[id]])
		}
		if (!is.null(x@layout$placements[[id]])) {
			base <- utils::modifyList(base, x@layout$placements[[id]])
		}
		base
	})
	names(placements) <- active
	rowOrder <- resolve_placement_order(active, placements, "rows", registry)
	columnOrder <- resolve_placement_order(active, placements, "columns", registry)
	list(
		preset = x@layout$preset, rows = rows, columns = columns,
		collapsed_rows = collapsed, stubhead = stubhead,
		placements = placements, row_group_order = rowOrder,
		column_group_order = columnOrder,
		group_labels = vapply(active, function(id) {
			label <- unique(groups$group_label[groups$group_id == id])
			if (length(label)) label[1] else group_display_label(id)
		}, character(1))
	)
}

#' Resolve explicit before/after constraints deterministically
#' @keywords internal
#' @noRd
resolve_placement_order <- function(active, placements, axis, registry) {
	ids <- active[vapply(active, function(id) placements[[id]]$axis == axis,
		logical(1))]
	nodes <- unique(c(ids, "body"))
	baseRank <- vapply(nodes, function(id) {
		if (id == "body") 25 else registry[[id]]$order
	}, numeric(1))
	order <- nodes[order(baseRank, nodes)]
	constraints <- list()
	for (id in ids) {
		p <- placements[[id]]
		if (!is.null(p$before) && p$before %in% nodes) {
			constraints[[length(constraints) + 1L]] <- c(id, p$before)
		}
		if (!is.null(p$after) && p$after %in% nodes) {
			constraints[[length(constraints) + 1L]] <- c(p$after, id)
		}
	}
	for (iteration in seq_len(max(1L, length(nodes)^2L))) {
		changed <- FALSE
		for (edge in constraints) {
			a <- match(edge[1], order)
			b <- match(edge[2], order)
			if (a > b) {
				order <- append(order[-a], edge[1], after = b - 1L)
				changed <- TRUE
			}
		}
		if (!changed) break
		if (iteration == length(nodes)^2L) {
			stop("Cell-group placement constraints contain a cycle on the `",
					 axis, "` axis.", call. = FALSE)
		}
	}
	stats::setNames(seq_along(order), order)
}

#' Compile all built-in group cells through one semantic projection
#' @keywords internal
#' @noRd
compile_mdl_gt_cells <- function(x, effects, groups, layout) {
	projected <- lapply(seq_len(nrow(groups)), function(i) {
		project_group_cell(groups[i, , drop = FALSE], effects, layout)
	})
	projected <- dplyr::bind_rows(projected)

	# Reference status is semantic effect metadata, never inferred from a key.
	ref <- effects$is_reference[match(projected$effect_id, effects$effect_id)]
	ref[is.na(ref)] <- FALSE
	projected$type[ref & projected$group_id %in% c("effect", "p", "forest")] <-
		"reference"

	rows <- unique(projected[
		projected$scope == "row",
		c("row_id", "row_group", "row_label", "row_path_key", "row_path",
			"row_track", "row_indent", "row_kind", "row_sort")
	])
	rows <- order_descriptor_frame(rows, "row_sort")
	rows$row_order <- seq_len(nrow(rows))

	cols <- unique(projected[c(
		"column_id", "column_label", "column_path_key", "column_path",
		"spanner", "column_sort"
	)])
	cols <- order_descriptor_frame(cols, "column_sort")
	cols$column_order <- seq_len(nrow(cols))

	projected$row_order <- rows$row_order[match(projected$row_id, rows$row_id)]
	projected$column_order <- cols$column_order[
		match(projected$column_id, cols$column_id)
	]
	projected <- validate_projected_coordinates(projected, effects, layout)
	projected <- projected[order(
		ifelse(projected$scope == "group", Inf, projected$row_order),
		projected$column_order, projected$scope
	), , drop = FALSE]
	projected$cell_id <- sprintf("cell_%06d", seq_len(nrow(projected)))

	fields <- c(
		"cell_id", "effect_id", "group_id", "subgroup", "scope", "scope_id",
		"row_id", "row_group", "row_label", "row_path", "row_path_key",
		"row_order", "row_kind",
		"row_indent", "column_id", "column_label", "column_path", "column_order",
		"spanner", "renderer", "type", "value", "format"
	)
	tibble::as_tibble(projected[fields])
}

#' Map one group value to semantic row and column paths
#' @keywords internal
#' @noRd
project_group_cell <- function(cell, effects, layout) {
	id <- cell$group_id
	p <- layout$placements[[id]]
	rowParts <- semantic_path(cell, layout$rows, effects,
		skip = layout$collapsed_rows)
	colParts <- semantic_path(cell, layout$columns, effects)
	rowTrack <- "body"
	if (!is.null(p$row_anchor)) {
		rowParts <- append_group_path(rowParts, p$row_anchor,
			first_of(layout$group_labels[[p$row_anchor]],
				group_display_label(p$row_anchor, p$row_anchor)))
		rowTrack <- p$row_anchor
	} else if (p$axis == "rows") {
		rowParts <- append_group_path(rowParts, id, cell$group_label,
			cell$subgroup)
		rowTrack <- id
	}
	if (p$axis == "columns") {
		colParts <- append_group_path(colParts, id, cell$group_label,
			cell$subgroup)
	}
	if (!length(colParts$keys)) {
		colParts <- append_group_path(colParts, ".value", "")
	}
	if (!length(rowParts$keys)) {
		rowParts <- append_group_path(rowParts, ".row", "")
	}

	# A measure that stops at an outer row dimension is group-scoped. The
	# renderer floats it over the concrete leaf rows in that band.
	rowMissing <- vapply(layout$rows, function(d) {
		v <- dimension_value(cell, d)
		is.na(v) || !nzchar(v)
	}, logical(1))
	scope <- if (is.null(p$row_anchor) && p$axis != "rows" &&
			any(rowMissing) && any(!rowMissing)) "group" else "row"
	rowId <- if (scope == "row") path_id(rowParts$keys, "row") else NA_character_
	rowGroup <- if (length(rowParts$labels) > 1) rowParts$labels[1] else ""
	rowLabel <- if (length(rowParts$labels) > 1) {
		paste(rowParts$labels[-1], collapse = " \u203a ")
	} else rowParts$labels[1]
	if (scope == "group") {
		rowGroup <- if (length(rowParts$labels)) rowParts$labels[1] else ""
		rowLabel <- NA_character_
	}
	scopeId <- if (scope == "group") paste(rowParts$keys, collapse = "\r") else
		path_id(rowParts$keys, "scope")
	colLabel <- utils::tail(colParts$labels, 1)
	spanner <- if (length(colParts$labels) > 1) {
		paste(colParts$labels[-length(colParts$labels)], collapse = spanner_sep)
	} else NA_character_

	data.frame(
		effect_id = cell$effect_id, group_id = id, subgroup = cell$subgroup,
		scope = scope, scope_id = scopeId, row_id = rowId,
		row_group = rowGroup, row_label = rowLabel,
		row_path_key = paste(rowParts$keys, collapse = "\r"),
		row_path = I(list(rowParts$labels)), row_track = rowTrack,
		row_indent = max(0L, length(rowParts$labels) - 2L), row_kind = "body",
		row_sort = I(list(row_sort_key(cell, layout, rowTrack, effects))),
		column_id = path_id(colParts$keys, "column"), column_label = colLabel,
		column_path_key = paste(colParts$keys, collapse = "\r"),
		column_path = I(list(colParts$labels)), spanner = spanner,
		column_sort = I(list(column_sort_key(cell, layout, id, effects))),
		renderer = cell$renderer, type = cell$type, value = I(cell$value),
		format = I(cell$format), stringsAsFactors = FALSE
	)
}

#' @keywords internal
#' @noRd
semantic_path <- function(cell, dimensions, effects, skip = character()) {
	keys <- labels <- character()
	for (dimension in dimensions) {
		value <- dimension_value(cell, dimension)
		if (is.na(value) || !nzchar(value)) next
		keys <- c(keys, paste0(dimension, "=", value))
		# A collapsed dimension keeps its key -- the band id stays complete --
		# and only drops out of the displayed path
		if (dimension %in% skip) next
		labels <- c(labels, dimension_label(cell, dimension))
	}
	list(keys = keys, labels = labels)
}

#' @keywords internal
#' @noRd
append_group_path <- function(path, id, label, subgroup = id) {
	path$keys <- c(path$keys, paste0("group=", id, ":", subgroup))
	path$labels <- c(path$labels, na_to_blank(label))
	path
}

#' @keywords internal
#' @noRd
dimension_value <- function(cell, dimension) {
	column <- switch(dimension,
		outcome = "outcome", term = "term", contrast = "contrast",
		adjustment = "adjustment", modifier = "modifier",
		modifier_level = "modifier_level", stratum = "stratum",
		stratum_level = "stratum_level", subset = "subset", dataset = "dataset",
		model = "model"
	)
	value <- cell[[column]]
	if (!length(value) || is.na(value)) NA_character_ else as.character(value)
}

#' @keywords internal
#' @noRd
dimension_label <- function(cell, dimension) {
	column <- switch(dimension,
		outcome = "outcome_label", term = "term_label",
		contrast = "contrast_label", adjustment = "adjustment_label",
		modifier = "modifier_label", modifier_level = "modifier_level_label",
		stratum = "stratum_label", stratum_level = "stratum_level_label",
		subset = "subset", dataset = "dataset", model = "model"
	)
	value <- cell[[column]]
	if (!length(value) || is.na(value)) "" else as.character(value)
}

#' @keywords internal
#' @noRd
path_id <- function(keys, prefix) {
	paste0(prefix, "_", vapply(keys, function(x) {
		paste(sprintf("%02x", utf8ToInt(x)), collapse = "")
	}, character(1)) |> paste(collapse = "_"))
}

#' Numeric order of one semantic value, using effect-frame first appearance
#' @keywords internal
#' @noRd
dimension_order <- function(cell, dimension, effects) {
	value <- dimension_value(cell, dimension)
	if (is.na(value) || !nzchar(value)) return(Inf)
	column <- switch(dimension,
		outcome = "outcome", term = "term", contrast = "contrast",
		adjustment = "adjustment", modifier = "modifier",
		modifier_level = "modifier_level", stratum = "stratum",
		stratum_level = "stratum_level", subset = "subset", dataset = "dataset",
		model = "model"
	)
	match(value, unique(na_to_blank(effects[[column]])))
}

#' @keywords internal
#' @noRd
row_sort_key <- function(cell, layout, track, effects) {
	trackOrder <- unname(layout$row_group_order[track])
	if (!length(trackOrder) || is.na(trackOrder)) {
		trackOrder <- unname(layout$row_group_order["body"])
	}
	orders <- vapply(layout$rows, function(d) dimension_order(cell, d, effects),
		numeric(1))
	# The outer row dimension remains the primary band; the track then decides
	# whether statistic rows precede/follow semantic body rows within it.
	if (length(orders)) c(orders[1], trackOrder, orders[-1]) else trackOrder
}

#' @keywords internal
#' @noRd
column_sort_key <- function(cell, layout, group, effects) {
	orders <- vapply(layout$columns, function(d) dimension_order(cell, d, effects),
		numeric(1))
	groupOrder <- unname(layout$column_group_order[group])
	if (!length(groupOrder) || is.na(groupOrder)) {
		groupOrder <- unname(layout$column_group_order["body"])
	}
	# Standalone coarse groups (N, a floating P) carry no column dimension and
	# sit before or after the body's columns by their placement order; every
	# standalone group used to sort ahead of the body, so P for interaction
	# led the odds ratios it tested. Groups carrying semantic columns sort
	# within those dimensions
	bodyOrder <- unname(layout$column_group_order["body"])
	if (!length(orders) || all(is.infinite(orders))) {
		return(c(if (groupOrder < bodyOrder) 0 else 200, groupOrder,
			rep(Inf, length(orders))))
	}
	# A group that carries some column dimensions but not the inner ones -- N
	# under an adjustment spanner, with no contrast -- sits at the near edge of
	# the dimensions it lacks: before the body's columns when its placement is
	# before the body, after them otherwise. Sorting its missing dimension as
	# `Inf` put N after every contrast column regardless of placement
	orders[is.infinite(orders)] <- if (groupOrder < bodyOrder) -Inf else Inf
	c(100, orders, groupOrder)
}

#' @keywords internal
#' @noRd
order_descriptor_frame <- function(data, sortColumn) {
	if (!nrow(data)) return(data)
	keys <- data[[sortColumn]]
	width <- max(lengths(keys))
	matrix <- do.call(rbind, lapply(keys, function(x) c(x, rep(Inf, width - length(x)))))
	ord <- do.call(order, as.data.frame(matrix))
	data[ord, , drop = FALSE]
}

#' Detect coordinates that lost a varying semantic dimension
#' @keywords internal
#' @noRd
validate_projected_coordinates <- function(cells, effects, layout) {
	key <- paste(
		ifelse(cells$scope == "group", cells$scope_id, cells$row_id),
		cells$column_id, cells$scope, sep = "\r"
	)
	dup <- duplicated(key) | duplicated(key, fromLast = TRUE)
	if (!any(dup)) return(cells)
	# Identical repeated data-derived cells may arrive through several models;
	# collapse them only when their values and presentation agree exactly.
	groups <- split(which(dup), key[dup])
	drop <- integer()
	bad <- list()
	for (idx in groups) {
		signature <- vapply(idx, function(i) paste0(
			cells$group_id[i], "\r", cells$subgroup[i], "\r",
			paste(unlist(cells$value[[i]]), collapse = "\r")
		), character(1))
		if (length(unique(signature)) == 1) {
			drop <- c(drop, idx[-1])
		} else bad[[length(bad) + 1L]] <- idx
	}
	if (length(bad)) {
		idx <- bad[[1]]
		effectIds <- unique(stats::na.omit(cells$effect_id[idx]))
		candidate <- setdiff(mdl_gt_dimensions(), c(layout$rows, layout$columns))
		varying <- candidate[vapply(candidate, function(d) {
			vals <- vapply(idx, function(i) {
				e <- effects[match(cells$effect_id[i], effects$effect_id), , drop = FALSE]
				if (!nrow(e)) return("")
				dimension_value(e, d)
			}, character(1))
			length(unique(vals)) > 1
		}, logical(1))]
		stop(
			"The layout maps different cell values to the same coordinate for `",
			cells$group_id[idx[1]], "`",
			if (length(effectIds)) paste0(" (effects `",
				paste(effectIds, collapse = "`, `"), "`)") else "", ".",
			if (length(varying)) paste0(
				" Place one of these varying dimensions on an axis: ",
				paste0("`", varying, "`", collapse = ", "), "."
			) else " Narrow the selected models or move the cell group.",
			call. = FALSE
		)
	}
	if (length(drop)) cells <- cells[-unique(drop), , drop = FALSE]
	cells
}

#' @keywords internal
#' @noRd
group_display_label <- function(id, fallback = id) {
	switch(id,
		effect = "Estimate", p = "P value", n = "N", events = "Events",
		rate = "Rate", rate_difference = "Rate difference", forest = "",
		fallback
	)
}

# Delimiter for the renderer's compatibility spanner path. The cell frame also
# carries the unencoded `column_path` list-column as the authoritative form.
spanner_sep <- "///"
