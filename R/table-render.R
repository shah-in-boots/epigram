# Rendering the <mdl_gt> cell frame -------------------------------------------
#
# `as_gt()` builds the semantic pipeline and renders its mechanical cell frame.
# This is the only file that emits `{gt}` layout calls. Forest axes are render
# metadata and never masquerade as semantic rows.

#' Render a `<mdl_gt>` specification to a `{gt}` table
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' `as_gt()` compiles a [mdl_gt()] specification through effects, atomic
#' measures, cell groups, layout, and a mechanical cell frame, then emits a
#' normal `{gt}` table. On a bare specification it renders a minimal default:
#' each displayed term's estimate and 95% confidence interval.
#'
#' @param x A `<mdl_gt>` specification (from [mdl_gt()])
#' @param ... Passed to methods
#'
#' @return A `gt_tbl` object.
#'
#' @seealso [inspect_mdl_gt()], [mdl_gt()]
#'
#' @include table-spec.R
#' @export
as_gt <- S7::new_generic("as_gt", "x")

# The method registers against the `mdl_gt` *class object*, which lives in
# table-spec.R -- hence the `@include` above, so collation defines the class
# before this file loads. The method is registered at load by
# `S7::methods_register()` (see zzz.R); it needs no NAMESPACE entry, so no
# `@export`.
method(as_gt, mdl_gt) <- function(x, ...) {
	built <- build_mdl_gt(x)
	render_cell_frame(built@cells, x, built@layout)
}

# The render stage -------------------------------------------------------------

#' Render a cell frame to a `{gt}` table
#'
#' The *render* stage, and the only place in the package that emits `{gt}`
#' layout calls. It is mechanical over the frame: format each cell through its
#' recipe, pivot wide (`row_group` the group column, `row_key` the stub),
#' then spanners (innermost first, so nested paths stack), column labels, stub
#' indentation, alignment, and the style layer (accents, then the baseline
#' theme). Two mechanisms extend the mechanical pipeline and live only here --
#' the rowspan emulation for group-scoped cells and the drawing of
#' `type = "plot"` cells -- so the frame itself stays plain data.
#' @keywords internal
#' @noRd
render_cell_frame <- function(frame, spec, layout = NULL) {

	missing_text <- first_of(spec@style$missing_text, "")
	rows <- unique(frame[
		frame$scope == "row",
		c("row_id", "row_group", "row_label", "row_path_key", "row_order",
			"row_indent", "row_kind")
	])
	rows <- rows[order(rows$row_order), , drop = FALSE]
	cols <- unique(frame[c(
		"column_id", "column_label", "column_order", "spanner"
	)])
	cols <- cols[order(cols$column_order), , drop = FALSE]

	# Plot axes are renderer metadata. Add a render-only footer row to the wide
	# data; the semantic cell frame itself remains free of magic `.axis` rows.
	plotColumns <- unique(frame$column_id[frame$renderer == "forest"])
	axisAt <- integer()
	if (length(plotColumns)) {
		rows <- dplyr::bind_rows(rows, data.frame(
			row_id = "render::forest-axis", row_group = "", row_label = "",
			row_path_key = "render::forest-axis",
			row_order = max(rows$row_order, 0) + 1L, row_indent = 0L,
			row_kind = "footer", stringsAsFactors = FALSE
		))
		axisAt <- nrow(rows)
	}

	# Pivot: one formatted text column per column key, missing cells filled
	# with the missing text
	wide <- tibble::tibble(
		row_group = na_to_blank(rows$row_group),
		row_label = rows$row_label
	)
	rowScoped <- frame[frame$scope == "row", , drop = FALSE]
	rowScoped$cell <- vapply(seq_len(nrow(rowScoped)), function(i) {
		if (rowScoped$type[i] == "reference") {
			first_of(spec@style$reference_text, missing_text)
		} else {
			format_cell(rowScoped$value[[i]], rowScoped$format[[i]], missing_text)
		}
	}, character(1))
	for (key in cols$column_id) {
		colCells <- rowScoped[rowScoped$column_id == key, , drop = FALSE]
		filled <- rep(missing_text, nrow(rows))
		at <- match(colCells$row_id, rows$row_id)
		filled[at[!is.na(at)]] <- colCells$cell[!is.na(at)]
		wide[[key]] <- filled
	}

	# Group-scoped cells: `{gt}` has no rowspan for body cells, so the value is
	# written into each row of its group here and all but one copy is masked
	# after the table is built (see `apply_group_scoped()`)
	groupScoped <- frame[frame$scope == "group", , drop = FALSE]
	gsPlacements <- list()
	for (i in seq_len(nrow(groupScoped))) {
		g <- groupScoped$row_group[i]
		key <- groupScoped$column_id[i]
		at <- which(startsWith(rows$row_path_key, groupScoped$scope_id[i]) &
			rows$row_kind == "body")
		if (length(at) == 0) next
		wide[[key]][at] <- format_cell(
			groupScoped$value[[i]], groupScoped$format[[i]], missing_text
		)
		# Vertically centered on the band: the middle row when the group has an
		# odd number of rows; with an even count, the row above the seam,
		# bottom-aligned so the value floats on it. Print formats that drop
		# vertical alignment (typst, Word via Quarto's table conversion) show
		# the value on that row's baseline instead; a first-row placement was
		# tried for them and rejected -- the float is the intended reading
		even <- length(at) %% 2 == 0
		visible <- at[ceiling(length(at) / 2)]
		gsPlacements[[length(gsPlacements) + 1]] <- list(
			column = key, visible = visible, masked = setdiff(at, visible),
			v_align = if (even) "bottom" else "middle"
		)
	}

	gtbl <- gt::gt(wide, rowname_col = "row_label", groupname_col = "row_group")
	# A row dimension constant across the table names the stub instead of
	# repeating on every row (see `resolve_mdl_gt_layout()`)
	stubhead <- if (is.null(layout)) "" else first_of(layout$stubhead, "")
	if (nzchar(stubhead)) {
		gtbl <- gt::tab_stubhead(gtbl, label = stubhead)
	}

	# Spanners, innermost first so nested paths stack upward; a spanner covers
	# each maximal run of consecutive columns sharing its path prefix
	paths <- strsplit(na_to_blank(cols$spanner), spanner_sep, fixed = TRUE)
	for (depth in rev(seq_len(max(c(lengths(paths), 1))))) {
		prefix <- vapply(paths, function(p) {
			if (length(p) >= depth) {
				paste(p[seq_len(depth)], collapse = spanner_sep)
			} else {
				NA_character_
			}
		}, character(1))
		runs <- rle(na_to_blank(prefix))
		ends <- cumsum(runs$lengths)
		starts <- ends - runs$lengths + 1
		for (r in seq_along(runs$values)) {
			if (!nzchar(runs$values[r])) next
			span <- seq(starts[r], ends[r])
			gtbl <- gt::tab_spanner(
				gtbl,
				label = paths[[span[1]]][depth],
				columns = dplyr::all_of(cols$column_id[span]),
				id = paste0("sp", depth, "::", cols$column_id[span[1]])
			)
		}
	}

	# Column labels from the frame, blank when suppressed
	labels <- stats::setNames(
		as.list(na_to_blank(cols$column_label)), cols$column_id
	)
	gtbl <- gt::cols_label(gtbl, .list = labels)

	for (level in sort(unique(rows$row_indent[rows$row_indent > 0]))) {
		at <- which(rows$row_indent == level & rows$row_kind == "body")
		if (length(at)) {
			gtbl <- gt::tab_stub_indent(gtbl, rows = !!at, indent = min(level, 5L))
		}
	}

	gtbl <- apply_group_scoped(gtbl, gsPlacements)
	gtbl <- render_registered_columns(gtbl, frame, rows, axisAt)
	gtbl <- apply_accents(gtbl, frame, rows, spec@style$accents)

	# Vertical padding: `modify_style(padding =)` wins; a table with plot
	# columns defaults to the dense zero-padding canvas they need
	padding <- spec@style$padding
	if (is.null(padding) && any(frame$type == "plot")) {
		padding <- 0
	}
	if (!is.null(padding)) {
		gtbl <- gt::opt_vertical_padding(gtbl, scale = padding)
	}

	gtbl <- apply_mdl_gt_theme(gtbl, spec@style$theme)
	gtbl <- gtbl |>
		gt::cols_align(align = "center", columns = dplyr::all_of(cols$column_id)) |>
		gt::opt_align_table_header("left") |>
		gt::tab_style(
			style = gt::cell_text(align = "left"),
			locations = gt::cells_stub()
		)
	apply_group_widths_and_alignment(gtbl, frame, spec@style)
}

#' Built-in visual renderer dispatch
#'
#' The main renderer owns table structure; registered visual renderers transform
#' only the cells that declare their renderer id. Adding a visual presentation
#' therefore does not create a second table realization path.
#' @keywords internal
#' @noRd
render_registered_columns <- function(gtbl, frame, rows, axisAt) {
	registry <- list(forest = render_plot_columns)
	active <- setdiff(unique(frame$renderer), "text")
	unknown <- setdiff(active, names(registry))
	if (length(unknown)) {
		stop("No built-in renderer is registered for: ",
			paste0("`", unknown, "`", collapse = ", "), ".", call. = FALSE)
	}
	for (id in active) {
		cells <- frame[frame$renderer == id, , drop = FALSE]
		gtbl <- registry[[id]](gtbl, cells, rows, axisAt)
	}
	gtbl
}

#' Format one cell through its recipe
#'
#' The single formatting authority of the renderer. Each statistic in the
#' cell's value formats by the recipe's `fmt` (`"number"` to `digits`;
#' `"count"` whole; `"p"` three decimals with `<0.001` below) and substitutes
#' into the merge `pattern` (`"{estimate} ({conf_low}, {conf_high})"`). A
#' missing statistic inside a parenthetical drops the parenthetical; missing
#' anywhere else -- including everywhere, as in a reference cell -- yields the
#' missing text.
#' @keywords internal
#' @noRd
format_cell <- function(value, format, missing_text) {

	if (!is.list(value)) {
		value <- list(value)
	}
	if (length(value) == 0 || all(vapply(value, function(v) {
		length(v) == 0 || is.na(v)
	}, logical(1)))) {
		return(missing_text)
	}

	# A non-finite statistic is a separated fit, not a number: an odds ratio
	# whose interval runs to `Inf` because a cell of the cross-classification
	# held no events. Printing `4857318.63 (0.00, Inf)` reports a plausible-
	# looking magnitude that means nothing, so the whole cell is missing
	nonFinite <- vapply(value, function(v) {
		is.numeric(v) && length(v) == 1 && !is.na(v) && !is.finite(v)
	}, logical(1))
	if (any(nonFinite)) {
		return(missing_text)
	}

	fmt <- first_of(format$fmt, "number")
	digits <- first_of(format$digits, 2L)
	pattern <- first_of(
		format$pattern,
		paste0("{", names(value)[1], "}")
	)

	na <- "NA"
	out <- pattern
	for (nm in names(value)) {
		v <- value[[nm]]
		text <-
			if (length(v) == 0 || is.na(v)) {
				na
			} else if (is.character(v)) {
				v
			} else {
				switch(
					fmt,
					p = if (v < 0.001) "<0.001" else {
						formatC(v, format = "f", digits = 3)
					},
					count = formatC(v, format = "f", digits = 0),
					formatC(v, format = "f", digits = digits)
				)
			}
		out <- gsub(paste0("{", nm, "}"), text, out, fixed = TRUE)
	}

	# A parenthetical missing any of its statistics drops whole (the interval
	# of an estimate without one); a missing statistic outside parentheses
	# empties the cell
	out <- gsub("\\s*\\([^()]*NA[^()]*\\)", "", out)
	if (grepl(na, out, fixed = TRUE) || !nzchar(trimws(out))) {
		return(missing_text)
	}
	trimws(out)
}

#' Emulate a body-cell rowspan for the group-scoped cells
#'
#' `{gt}` has no rowspan for body cells, so a group-scoped cell -- a statistic
#' belonging to a whole row-group band, like the interaction p-value floating
#' between its level rows -- renders by the duplicate-and-mask device the old
#' forest table used, now documented in this one place: the value is written
#' into every row of the group (done at pivot), exactly one copy stays
#' visible, vertically centered on the band (with an even row count, the row
#' above the seam, bottom-aligned, which is what makes it float on the seam),
#' and the duplicates are blanked by a text transform — a content substitution
#' rather than a white-text style, so the mask holds on dark themes and on
#' every output format (LaTeX, RTF, Word). The mask is a render artifact and
#' never appears in the cell frame; if `{gt}` ever grows body rowspans, only
#' this function changes.
#' @keywords internal
#' @noRd
apply_group_scoped <- function(gtbl, placements) {

	for (placement in placements) {
		# The row positions are injected (`!!`): `cells_body(rows = )` is
		# data-masked, so a bare symbol could collide with a column name
		gtbl <- gt::tab_style(
			gtbl,
			style = gt::cell_text(align = "center", v_align = placement$v_align),
			locations = gt::cells_body(
				columns = dplyr::all_of(placement$column),
				rows = !!placement$visible
			)
		)
		if (length(placement$masked) > 0) {
			gtbl <- gt::text_transform(
				gtbl,
				locations = gt::cells_body(
					columns = dplyr::all_of(placement$column),
					rows = !!placement$masked
				),
				fn = function(x) rep("", length(x))
			)
		}
	}
	gtbl
}

#' Draw the `type = "plot"` cells of a frame
#'
#' A plot cell's value is plain numbers (`estimate`, `conf_low`, `conf_high`);
#' the drawing happens here, necessarily, because the x-scale is a property of
#' the *column* -- limits, intercept, breaks, and log-versus-linear resolved
#' across all of its cells, with the block's `axis` options (on the cells'
#' format recipe) overriding the guesses. Each cell renders through
#' [gt::text_transform()]. A render-only footer takes the bottom axis strip;
#' it is never part of the semantic cell frame.
#' @keywords internal
#' @noRd
render_plot_columns <- function(gtbl, frame, rows, axisAt = integer()) {

	plotKeys <- unique(frame$column_id[frame$renderer == "forest"])
	if (length(plotKeys) == 0) {
		return(gtbl)
	}

	for (key in plotKeys) {
		cells <- frame[
			frame$column_id == key & frame$renderer == "forest" &
				frame$scope == "row",
			, drop = FALSE
		]
		if (nrow(cells) == 0) next
		options <- cells$format[[1]]$axis
		scale <- resolve_plot_scale(cells$value, options)
		width <- first_of(cells$format[[1]]$width, 100)

		at <- match(cells$row_id, rows$row_id)
		values <- cells$value[order(at)]
		gtbl <- gt::text_transform(
			gtbl,
			locations = gt::cells_body(
				columns = dplyr::all_of(key), rows = !!sort(at)
			),
			fn = function(x) {
				vapply(seq_along(x), function(i) {
					plot_image(draw_forest_cell(values[[i]], scale), width, 30)
				}, character(1))
			}
		)

		if (length(axisAt) > 0) {
			# A titled strip needs room for the title line under the tick labels
			axisHeight <- if (is.null(scale$title)) 22 else 34
			gtbl <- gt::text_transform(
				gtbl,
				locations = gt::cells_body(
					columns = dplyr::all_of(key), rows = !!axisAt
				),
				fn = function(x) {
					vapply(seq_along(x), function(i) {
						plot_image(draw_forest_axis(scale), width, axisHeight)
					}, character(1))
				}
			)
		}
	}

	# The footer is renderer machinery, not a semantic row.
	if (length(axisAt) > 0) {
		gtbl <- gt::text_transform(
			gtbl,
			locations = gt::cells_stub(rows = !!axisAt),
			fn = function(x) rep("", length(x))
		)
	}

	# The dense look the plots need to read as one continuous canvas enters as
	# *defaults* to the style layer: zero vertical padding unless
	# `modify_style(padding =)` says otherwise (set by the caller), and a
	# borderless body. The body goes borderless as a whole — the journal
	# booktabs look: top rule, header rule, bottom rule, nothing inside —
	# rather than per plot cell: under `border-collapse: collapse` a hidden
	# border wins every shared edge, so per-cell hiding punched gaps in the
	# header and bottom rules while the other columns kept their row lines
	gtbl <- gt::tab_options(
		gtbl,
		table_body.hlines.style = "none",
		row_group.border.top.style = "none",
		row_group.border.bottom.style = "none",
		stub.border.style = "none"
	)
	gtbl
}

#' Resolve a plot column's shared x-scale across all of its cells
#'
#' The guesses -- limits padded past the widest interval, the intercept at the
#' log/linear null, breaks from the limits -- each yield to the block's `axis`
#' options (`limits`, `intercept`, `breaks`, `log`). The optional `title`
#' rides along untouched: it is a property of the column's shared axis, so
#' this resolved scale is where the axis strip finds it.
#' @keywords internal
#' @noRd
resolve_plot_scale <- function(values, options = NULL) {

	nums <- unlist(lapply(values, function(v) {
		c(v$estimate, v$conf_low, v$conf_high)
	}))
	nums <- nums[!is.na(nums)]
	log <- isTRUE(options$log)
	if (!length(nums)) {
		nums <- if (!is.null(options$limits)) options$limits else
			if (log) c(0.5, 2) else c(-1, 1)
	}

	limits <- options$limits
	if (is.null(limits)) {
		span <- range(nums)
		pad <- diff(span) * 0.05
		if (pad == 0) pad <- abs(span[1]) * 0.05 + 0.5
		limits <- span + c(-pad, pad)
		if (log) limits[1] <- max(limits[1], min(nums) * 0.9)
	}
	intercept <- first_of(options$intercept, if (log) 1 else 0)
	breaks <- options$breaks
	if (is.null(breaks)) {
		breaks <-
			if (log) {
				scales::log_breaks()(limits)
			} else {
				scales::extended_breaks(n = 4)(limits)
			}
		breaks <- breaks[breaks >= limits[1] & breaks <= limits[2]]
	}

	list(
		limits = limits, intercept = intercept, breaks = breaks, log = log,
		title = options$title, left = options$left, right = options$right
	)
}

#' Render a plot cell at its displayed size, as an inline image
#'
#' [gt::ggplot_image()] draws every plot on a fixed 5-inch canvas and squashes
#' it to the displayed height, which shrinks each mark ~17-fold -- the point
#' and interval come out sub-pixel, effectively invisible. The cells here
#' render at their true size, so the drawn weights survive to the page and
#' every cell in a column shares identical panel geometry -- what makes the
#' column read as one forest plot.
#'
#' Cells draw as vector SVG (each its own base64 `data:` document, so glyph
#' ids cannot collide across cells), sized in `em` units so they scale with
#' the text they sit beside instead of holding a fixed pixel geometry. A
#' build without cairo falls back to the fixed-size PNG.
#' @keywords internal
#' @noRd
plot_image <- function(plot, width, height) {

	if (isTRUE(capabilities("cairo")[[1]])) {
		file <- tempfile(fileext = ".svg")
		on.exit(unlink(file))
		ggplot2::ggsave(
			file, plot = plot, device = grDevices::svg, bg = "transparent",
			width = width / 96, height = height / 96
		)
		tag <- as.character(gt::local_image(file, height = height))
		# The fixed pixel size becomes em units (16 px = 1 em, the CSS root
		# default). Both dimensions are pinned: the svg device rounds its canvas
		# to whole points, which skews each image's intrinsic aspect ratio by a
		# different amount — left to the browser, the axis strip would render a
		# different width than the cells above it
		return(sub(
			"height:[0-9.]+px;",
			sprintf(
				"width:%sem;height:%sem;",
				format(round(width / 16, 3)), format(round(height / 16, 3))
			),
			tag
		))
	}

	file <- tempfile(fileext = ".png")
	on.exit(unlink(file))
	ggplot2::ggsave(
		file, plot = plot, device = "png", bg = "transparent",
		width = width / 96, height = height / 96, dpi = 192
	)
	gt::local_image(file, height = height)
}

#' One forest cell: the point estimate and its interval on the column's scale
#' @keywords internal
#' @noRd
draw_forest_cell <- function(value, scale) {

	d <- data.frame(
		estimate = value$estimate,
		conf_low = first_of(value$conf_low, NA_real_),
		conf_high = first_of(value$conf_high, NA_real_)
	)
	showIntercept <- scale$intercept >= scale$limits[1] &&
		scale$intercept <= scale$limits[2]
	ggplot2::ggplot(d, ggplot2::aes(x = estimate, y = 0)) +
		(if (showIntercept) {
			ggplot2::geom_vline(
				xintercept = scale$intercept, linetype = "dashed", linewidth = 0.3
			)
		}) +
		ggplot2::geom_errorbar(
			ggplot2::aes(xmin = conf_low, xmax = conf_high),
			width = 0.7, linewidth = 0.5, orientation = "y", na.rm = TRUE
		) +
		ggplot2::geom_point(size = 1.6, na.rm = TRUE) +
		forest_x_scale(scale) +
		# The y-scale is pinned so the interval's end caps keep a fixed
		# proportion of the cell height (a degenerate one-value y range would
		# otherwise clip them to full-height bars)
		ggplot2::scale_y_continuous(limits = c(-1, 1)) +
		ggplot2::theme_void() +
		ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
}

#' The render-only bottom axis strip of a forest column
#'
#' The strip continues the cells' reference line down to the axis, so the
#' column reads as one panel rather than rows with a ruler pasted under
#' them; an axis `title` draws beneath the tick labels.
#' @keywords internal
#' @noRd
draw_forest_axis <- function(scale) {

	showIntercept <- scale$intercept >= scale$limits[1] &&
		scale$intercept <= scale$limits[2]
	ggplot2::ggplot(
		data.frame(x = scale$limits, y = 0), ggplot2::aes(x = x, y = y)
	) +
		ggplot2::geom_blank() +
		(if (showIntercept) {
			ggplot2::geom_vline(
				xintercept = scale$intercept, linetype = "dashed", linewidth = 0.3
			)
		}) +
		forest_x_scale(scale, breaks = scale$breaks) +
		(if (!is.null(scale$title)) ggplot2::labs(x = scale$title)) +
		ggplot2::theme_void() +
		ggplot2::theme(
			axis.line.x = ggplot2::element_line(linewidth = 0.3),
			axis.ticks.x = ggplot2::element_line(linewidth = 0.3),
			axis.ticks.length.x = ggplot2::unit(2, "pt"),
			axis.text.x = ggplot2::element_text(size = 7),
			axis.title.x = if (is.null(scale$title)) {
				ggplot2::element_blank()
			} else {
				ggplot2::element_text(size = 7, margin = ggplot2::margin(t = 2))
			},
			plot.margin = ggplot2::margin(0, 0, 0, 0)
		)
}

#' The shared x-scale of a forest column, as a ggplot scale
#' @keywords internal
#' @noRd
forest_x_scale <- function(scale, breaks = NULL) {
	# Values past the limits squish onto the edge rather than dropping the
	# whole errorbar (the default `oob` turns any out-of-range bound to NA)
	args <- list(limits = scale$limits, oob = scales::oob_squish)
	if (!is.null(breaks)) {
		args$breaks <- breaks
	}
	if (scale$log) {
		args$transform <- "log"
	}
	do.call(ggplot2::scale_x_continuous, args)
}

#' Apply the `modify_style()` accents
#'
#' The generalization of the old `tbl_beta()` accents (which recognized only a
#' `p <` criterion and hard-coded bold): each accent's criterion may compare
#' any displayed statistic, and its instruction may be bold, italic, and/or a
#' text color. A criterion evaluates once per term-level context within each
#' row -- the cells sharing a column-key base, so an estimate and the p-value
#' it belongs to are judged (and accented) together -- against every statistic
#' those cells carry.
#' @keywords internal
#' @noRd
apply_accents <- function(gtbl, frame, rows, accents) {

	if (length(accents) == 0) {
		return(gtbl)
	}

	cells <- frame[
		frame$scope == "row" & frame$type %in% c("numeric", "reference"),
		, drop = FALSE
	]
	context <- ifelse(
		!is.na(cells$effect_id), cells$effect_id,
		paste(cells$row_id, cells$group_id, sep = "\r")
	)
	groups <- split(
		seq_len(nrow(cells)),
		paste(cells$row_id, context, sep = "\r")
	)

	for (idx in groups) {
		stats <- list()
		for (i in idx) {
			value <- cells$value[[i]]
			if (cells$group_id[i] == "rate_difference") {
				value <- list(rate_difference = value$estimate)
			}
			if (cells$group_id[i] == "n" && !"n" %in% names(value)) {
				value <- list(n = unname(value[[1]]))
			}
			stats <- utils::modifyList(stats, value[!vapply(value, function(v) {
				length(v) == 0 || is.na(v)
			}, logical(1))])
		}
		# The registry's aliases: `beta` for the estimate, `p` for the p-value —
		# whichever alias a cell's value carries, propagate it onto its siblings
		for (s in Filter(function(s) isTRUE(s$accentable), table_statistics())) {
			present <- Filter(function(a) !is.null(stats[[a]]), s$aliases)
			if (length(present) > 0) {
				val <- stats[[present[[1]]]]
				for (a in s$aliases) stats[[a]] <- val
			}
		}

		for (accent in accents) {
			hit <- tryCatch(
				isTRUE(eval(accent$criterion, envir = stats)),
				error = function(e) FALSE
			)
			if (!hit) next
			# Translate the instruction into a `gt::cell_text()` style
			args <- list()
			if ("bold" %in% accent$instruction) args$weight <- "bold"
			if ("italic" %in% accent$instruction) args$style <- "italic"
			color <- setdiff(accent$instruction, c("bold", "italic"))
			if (length(color) > 0) args$color <- color[1]
			style <- do.call(gt::cell_text, args)
			for (i in idx) {
				at <- match(cells$row_id[i], rows$row_id)
				if (is.na(at)) next
				gtbl <- gt::tab_style(
					gtbl,
					style = style,
					locations = gt::cells_body(
						columns = dplyr::all_of(cells$column_id[i]), rows = !!at
					)
				)
			}
		}
	}
	gtbl
}

#' Apply one built-in HTML-first table theme
#' @keywords internal
#' @noRd
apply_mdl_gt_theme <- function(gtbl, theme = "journal") {
	theme <- first_of(theme, "journal")
	if (theme == "plain") return(gtbl)
	if (theme == "compact") {
		return(gtbl |>
			gt::opt_vertical_padding(scale = 0.5) |>
			gt::tab_options(table_body.hlines.style = "none"))
	}
	gtbl |>
		gt::tab_options(
			table_body.hlines.style = "none",
			row_group.border.top.style = "none",
			row_group.border.bottom.style = "none",
			stub.border.style = "none",
			column_labels.border.bottom.style = "solid",
			column_labels.border.bottom.width = gt::px(1),
			table_body.border.bottom.style = "solid",
			table_body.border.bottom.width = gt::px(1)
		)
}

#' Cell-group width/alignment overrides resolved against semantic frame ids
#' @keywords internal
#' @noRd
apply_group_widths_and_alignment <- function(gtbl, frame, style) {
	for (id in names(style$widths)) {
		columns <- unique(frame$column_id[frame$group_id == id])
		if (!length(columns)) next
		width <- style$widths[[id]]
		if (is.numeric(width) && length(width) == 1 && !is.na(width)) {
			widthFormula <- rlang::new_formula(
				rlang::expr(dplyr::all_of(!!columns)),
				rlang::expr(gt::px(!!width))
			)
			gtbl <- gt::cols_width(gtbl, .list = list(widthFormula))
		}
	}
	for (id in names(style$align)) {
		columns <- unique(frame$column_id[frame$group_id == id])
		value <- style$align[[id]]
		if (!length(columns) || !is.character(value) || length(value) != 1 ||
				!value %in% c("left", "center", "right")) next
		gtbl <- gt::cols_align(gtbl, align = value,
			columns = dplyr::all_of(columns))
	}
	gtbl
}

#' The first non-`NULL` argument (the renderer's fallback chains)
#' @keywords internal
#' @noRd
first_of <- function(...) {
	for (x in list(...)) {
		if (!is.null(x)) return(x)
	}
	NULL
}
