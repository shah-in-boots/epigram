# Class ----------------------------------------------------------------------

#' Model tables
#'
#' @description
#'
#' `r lifecycle::badge('experimental')`
#'
#' `model_table()` creates a `mdl_tbl` object — the notebook of models. It
#' collects `mdl` vectors (fitted models from [fit()] with `raw = FALSE`),
#' `fmls` objects (formulas not yet fit), and other `mdl_tbl` objects into a
#' single data frame where each row is one model: its formula, its causal
#' roles, its fitting context (data, strata, subsets), and its results.
#' `mdl_tbl()` is a documented alias; the class itself is named `mdl_tbl`.
#'
#' The table is the working surface of an analysis. Its print method
#' summarizes what has been fit, what failed, and what is still waiting;
#' [summary()] maps the fleet; [flatten_models()] pulls out estimates;
#' [model_diagnostics()] explains failures and shows which fits converged
#' without being estimable. See [model_table_helpers] for the full set.
#'
#' @details
#'
#' Along with the row-per-model data, three scalar attributes carry the
#' context needed to reconstruct any model in the table:
#'
#' 1. A **formula matrix** ([formula_matrix()]) with one row per model and one
#' column per term, marking which terms each model's formula contains.
#'
#' 1. A **term table** ([term_table()]) describing each term's causal role,
#' label, and other metadata.
#'
#' 1. A **data list** ([model_data()]) holding datasets attached via
#' [attach_data()] or the `data` argument, so models can be diagnosed or
#' re-examined later.
#'
#' These attributes are reconciled automatically when tables are combined,
#' filtered, or otherwise manipulated with `dplyr` verbs.
#'
#' @section Invariant columns:
#'
#' Every `mdl_tbl` carries these columns; they may be reordered by row but not
#' removed or renamed. Dropping any of them (e.g. through [dplyr::select()])
#' returns a plain `data.frame` with a message.
#'
#' * `id` — hash identifying the model (links rows to the formula matrix,
#'   whose rows stay parallel to the table's)
#' * `data_id` — name of the dataset the model was fit on
#' * `name` — the label the object was given when added to the table
#' * `model_call` — the fitting function (e.g. `lm`, `coxph`); `NA` until fit
#' * `formula_call` — the model formula as text
#' * `number` — number of right-hand-side terms (the "adjustment degree")
#' * `outcome`, `exposure`, `mediator`, `interaction` — terms by causal role
#' * `strata`, `level` — the stratifying term and this model's stratum
#' * `subset` — the [subset_data()] instruction the model was fit under
#' * `model_parameters` — parameter-level estimates (a list of data frames)
#' * `model_summary` — model-level statistics (a list)
#' * `fit_status` — `TRUE` if the model fit; `FALSE` if it failed or has not
#'   been fit yet
#'
#' @section Family columns:
#'
#' Every `mdl_tbl` also carries three columns read from the formulas' causal
#' roles, describing how its models group into analyses:
#'
#' * `family` — integer id grouping the rows that belong to one analysis (an
#'   adjustment ladder, a mediation triad, ...), numbered by order of first
#'   appearance
#' * `pattern` — the family's shape: `sequential`, `parallel`, `mediation`, or
#'   `direct`
#' * `relation` — how families relate across their boundaries (`varied
#'   exposures`, `varied outcomes`), or `NA` for a family that stands alone
#'
#' These are derived, not supplied: they are recomputed automatically whenever
#' the table is built or reshaped (filtering to one exposure can dissolve a
#' `varied exposures` relation; paring renumbers the ids from 1), so they
#' always describe the rows currently present. [keep_families()] pares by
#' them.
#'
#' @section Combining tables:
#'
#' Model tables combine through `model_table(x, y)` (or `vctrs::vec_rbind()`);
#' formula matrices, term tables, and data lists are merged and deduplicated,
#' with the first (left-most) definition of a term kept. [dplyr::bind_rows()]
#' only works when the rows already belong to the first table (it strips
#' attributes before they can be reconciled); combining unrelated tables with
#' it returns a plain `data.frame` with a message pointing back to
#' `model_table()`.
#'
#' @param ... `mdl` or `fmls` objects to tabulate (named arguments become the
#'   `name` column), or `mdl_tbl` objects to combine. A single bare `list()`
#'   of such objects is also accepted. Raw fitted models (e.g. an `lm`
#'   object) are not: refit with `fit(..., raw = FALSE)` or wrap with [mdl()].
#'
#' @param data A `data.frame` used by the models, attached under the name it
#'   was passed as (see [attach_data()])
#'
#' @param x A `mdl_tbl` object
#'
#' @return A `mdl_tbl` object: a `tibble` where each row describes one model,
#'   with the formula matrix, term table, and data list carried as attributes.
#'
#' @name model_table
#' @importFrom tibble tibble new_tibble
#' @export
model_table <- function(..., data = NULL) {

	# Call
	mc <- match.call()

	dots <- rlang::list2(...)
	if (length(dots) == 0) {
		stop(
			"`model_table()` needs something to tabulate: ",
			"`mdl` objects from `fit(..., raw = FALSE)`, `fmls` objects, ",
			"or other `mdl_tbl` objects.",
			call. = FALSE
		)
	}

	# A single, unnamed bare list is a container for its elements
	if (length(dots) == 1 &&
			is.null(names(dots)) &&
			is.list(dots[[1]]) &&
			!is.object(dots[[1]])) {
		dots <- dots[[1]]
	}

	# Guard the gate: raw fits and other strangers are turned away with
	# directions (issue #46)
	for (i in seq_along(dots)) {
		el <- dots[[i]]
		if (!inherits(el, c("mdl", "fmls", "mdl_tbl"))) {
			stop(
				"`model_table()` accepts `mdl`, `fmls`, and `mdl_tbl` objects, ",
				"but was given a `", class(el)[1], "` object. ",
				"Refit with `fit(..., raw = FALSE)` or wrap a fitted model ",
				"with `mdl()` first.",
				call. = FALSE
			)
		}
	}

	# Model Table Lists...
	mtl <- vector("list", length(dots))

	for (i in seq_along(dots)) {
		if (inherits(dots[[i]], "mdl")) {
			mtl[[i]] <- construct_table_from_models(dots[i])
		} else if (inherits(dots[[i]], "fmls")) {
			mtl[[i]] <- construct_table_from_formulas(dots[i])
		} else if (inherits(dots[[i]], "mdl_tbl")) {
			mtl[[i]] <- dots[[i]]
		}
	}

	# Convert into a single table
	mdTab <- do.call(vec_rbind, mtl)

	# Once it comes back as a new class, we need to add data if its available
	if (!is.null(data)) {
		nm <-
			if (is.symbol(mc$data)) {
				deparse1(mc$data)
			} else {
				data_content_name(data)
			}
		mdTab <- attach_data(mdTab, data = data, name = nm)
	}

	# Stamp the family identification on as ordinary columns. The combined table
	# now carries the full, row-parallel formula matrix, so ids number across
	# every model at once; any later reshape refreshes them (see
	# `stamp_families()`)
	mdTab <- stamp_families(mdTab)

	# Return new class
	mdTab
}

#' @rdname model_table
#' @export
mdl_tbl <- model_table

#' @rdname model_table
#' @export
is_model_table <- function(x) {
	inherits(x, "mdl_tbl")
}

#' @keywords internal
#' @noRd
methods::setOldClass(c("mdl_tbl", "vctrs_vctr"))

#' @export
vec_ptype_full.mdl_tbl <- function(x, ...) {
	"model_table"
}

#' @export
vec_ptype_abbr.mdl_tbl <- function(x, ...) {
	"mdl_tbl"
}

# Constructors -----------------------------------------------------------------

#' The role's term for each term list of a table under construction, `NA`
#' when a model has none; the shared role pull of both constructors
#' @keywords internal
#' @noRd
role_term <- function(termList, wanted) {
	vapply(termList, function(.x) {
		tms <- as.character(.x)[vec_proxy(.x)$role == wanted]
		if (length(tms) == 0) NA_character_ else tms[1]
	}, character(1))
}

#' The one recorded interaction term per model (component terms like `a:b`
#' are skipped; several declared terms keep the first, with a message)
#' @keywords internal
#' @noRd
interaction_term <- function(termList) {
	vapply(termList, function(.x) {
		tms <- as.character(.x)[vec_proxy(.x)$role == "interaction"]
		tms <- tms[!grepl(":", tms)]
		if (length(tms) == 0) {
			return(NA_character_)
		}
		if (length(tms) > 1) {
			message(
				"A model carries multiple interaction terms (`",
				paste0(tms, collapse = "`, `"),
				"`); only the first is mirrored in the scalar `interaction` column. ",
				"Formula metadata retains every modifier for `mdl_gt()` discovery."
			)
		}
		tms[1]
	}, character(1))
}

#' Restructure models to fit within a model table
#' Passes information to `new_model_table()` for initialization
#' @param x Single-element, possibly-named list holding a `mdl` vector
#' @keywords internal
construct_table_from_models <- function(x, ...) {

	# Meta components of the models
	obj <- x[[1]] # Removes it from its list
	nm <-
		if (is.null(names(x)) || is.na(names(x)) || names(x) == "") {
			NA_character_
		} else {
			names(x)
		}
	n <- length(obj)
	rid <- sapply(obj, rlang::hash)

	# Components of the model fields
	mc <- unlist(field(obj, "modelCall"))
	ma <- field(obj, "modelArgs")
	pe <- field(obj, "parameterEstimates")
	si <- field(obj, "summaryInfo")

	# A model that recorded an error did not fit
	fits <- sapply(si, function(.x) {
		is.null(.x$error) || all(is.na(.x$error))
	})

	# Get terms and formulas
	mf <- field(obj, "modelFormula")
	tl <- lapply(mf, key_terms)
	fl <- unname(sapply(mf, as.character, USE.NAMES = FALSE))
	num <- formula_term_count(fl)

	# Terms must be combined into a term table for later look up
	tmTab <-
		do.call(what = rbind, args = lapply(tl, vec_data)) |>
		unique()

	# The formula matrix rows, parallel to the models, each re-pointed from
	# its own family's term table at the combined one (models fit from
	# different families may define a shared term at different ordinals)
	fmMat <-
		do.call(what = rbind, args = lapply(mf, function(f) {
			remap_definition_ordinals(
				vec_data(vec_proxy(f)), from = attr(f, "termTable"), to = tmTab
			)
		})) |>
		vec_data()

	out <- role_term(tl, "outcome")
	exp <- role_term(tl, "exposure")
	med <- role_term(tl, "mediator")
	int <- interaction_term(tl)

	# Get all data names, strata variables, and subsets back
	da <- field(obj, "dataArgs")
	did <- sapply(da, function(.x) {
		.x$dataName
	})
	sta <- sapply(da, function(.x) {
		.x$strataVariable
	})
	# Stratum levels keep their provenance as text so tables from different
	# datasets (numeric vs factor strata) can combine
	slvl <- sapply(da, function(.x) {
		if (is.null(.x$strataLevel) || all(is.na(.x$strataLevel))) {
			NA_character_
		} else {
			as.character(.x$strataLevel)
		}
	})
	sub <- sapply(da, function(.x) {
		if (is.null(.x$subsetName) || length(.x$subsetName) == 0) {
			NA_character_
		} else {
			as.character(.x$subsetName)
		}
	})

	# Initialize a new list
	res <- df_list(
		id = rid,
		data_id = did,
		name = nm,
		model_call = mc,
		formula_call = fl,
		number = num,
		outcome = out,
		exposure = exp,
		mediator = med,
		interaction = int,
		strata = sta,
		level = slvl,
		subset = sub,
		model_parameters = pe,
		model_summary = si,
		fit_status = fits
	)

	# Return
	new_model_table(res,
									formulaMatrix = fmMat,
									termTable = tmTab,
									dataList = list())

}

#' Restructure formulas to fit within a model table
#' @param x Single-element, possibly-named list holding a `fmls` object
#' @keywords internal
construct_table_from_formulas <- function(x, ...) {

	# Meta components of the models
	obj <- x[[1]] # Removes it from its list
	nm <-
		if (is.null(names(x)) || is.na(names(x)) || names(x) == "") {
			NA_character_
		} else {
			names(x)
		}
	n <- nrow(obj)
	rid <- apply(obj, MARGIN = 1, rlang::hash) # Since formulas are matrices
	fits <- rep(FALSE, n)

	# Get terms and formulas
	mf <- obj
	tl <- formulas_to_terms(mf)
	fl <- as.character(stats::formula(mf))
	num <- formula_term_count(fl)
	fmMat <- vec_data(mf)

	# Terms must be combined into a term table for later look up
	tmTab <-
		do.call(what = rbind, args = lapply(tl, vec_data)) |>
		unique()

	out <- role_term(tl, "outcome")
	exp <- role_term(tl, "exposure")
	med <- role_term(tl, "mediator")
	int <- interaction_term(tl)

	# Data will be empty because this is just a formula
	# Strata may be present, but levels cannot be
	sta <- role_term(tl, "strata")

	# Initialize a new list
	res <- df_list(
		id = rid,
		data_id = NA_character_,
		name = nm,
		model_call = NA_character_,
		formula_call = fl,
		number = num,
		outcome = out,
		exposure = exp,
		mediator = med,
		interaction = int,
		strata = sta,
		level = NA_character_,
		subset = NA_character_,
		model_parameters = vector("list", n),
		model_summary = vector("list", n),
		fit_status = fits
	)

	# Return
	new_model_table(res,
									formulaMatrix = fmMat,
									termTable = tmTab,
									dataList = list())
}

#' Count the right-hand-side terms of formulas given as text
#' @keywords internal
#' @noRd
formula_term_count <- function(formulaCalls) {
	vapply(formulaCalls, function(.f) {
		tryCatch(
			length(labels(stats::terms(stats::formula(.f)))),
			error = function(e) NA_integer_
		)
	}, integer(1), USE.NAMES = FALSE)
}

#' The invariant columns of a model table
#' @keywords internal
#' @noRd
model_table_columns <- function() {
	c(
		"id",
		"data_id",
		"name",
		"model_call",
		"formula_call",
		"number",
		"outcome",
		"exposure",
		"mediator",
		"interaction",
		"strata",
		"level",
		"subset",
		"model_parameters",
		"model_summary",
		"fit_status"
	)
}

#' @keywords internal
new_model_table <- function(x = list(),
														formulaMatrix = data.frame(),
														termTable = data.frame(),
														dataList = list(),
														...) {

	# Invariant rules:
	#		Can add and remove rows (each row is essentially a model)
	#		Rows can be re-ordered
	#		Invariant columns (see `model_table_columns()`) cannot be removed,
	#		renamed, or re-ordered; new columns may be added after them

	if (length(x) == 0) {
		stop(
			"No data was available to be coerced to a `mdl_tbl` object.",
			call. = FALSE
		)
	}

	validate_class(formulaMatrix, "data.frame")
	validate_class(termTable, "data.frame")
	validate_class(dataList, "list")

	out <- tibble::new_tibble(
		x,
		formulaMatrix = formulaMatrix,
		termTable = termTable,
		dataList = dataList,
		class = "mdl_tbl"
	)

	# Validate on every construction: the invariant columns must be present
	# with their expected types (see the Invariant columns section of
	# [model_table])
	missingCols <- setdiff(model_table_columns(), names(out))
	if (length(missingCols) > 0) {
		stop(
			"A `mdl_tbl` requires the invariant column(s) `",
			paste(missingCols, collapse = "`, `"),
			"` (see `?model_table`).",
			call. = FALSE
		)
	}
	if (!is.logical(out$fit_status)) {
		stop("The `fit_status` column of a `mdl_tbl` must be logical.",
				 call. = FALSE)
	}
	if (!is.list(out$model_parameters) || !is.list(out$model_summary)) {
		stop(
			"The `model_parameters` and `model_summary` columns of a `mdl_tbl` ",
			"must be list columns.",
			call. = FALSE
		)
	}

	out
}

# Attribute reconciliation -----------------------------------------------------

#' Merge term tables, keeping the first definition of each term/role pair
#' @keywords internal
#' @noRd
merge_term_tables <- function(x, y) {

	combined <- rbind(x, y)
	if (nrow(combined) == 0) {
		return(combined)
	}

	# Left-most wins, consistent with how `fmls` objects combine (issue #42);
	# the same term may hold different roles (e.g. outcome here, exposure there)
	key <- paste(combined$term, combined$role, combined$side, sep = "\r")
	combined[!duplicated(key), , drop = FALSE]
}

#' Merge formula matrices row-wise, filling absent terms with zero membership
#' @keywords internal
#' @noRd
merge_formula_matrices <- function(x, y) {
	dplyr::bind_rows(x, y) |>
		{
			\(.x) replace(.x, is.na(.x), 0)
		}()
}

#' Merge data lists by name, keeping the first copy of a dataset
#' @keywords internal
#' @noRd
merge_data_lists <- function(x, y) {
	combined <- c(x, y)
	combined[!duplicated(names(combined))]
}

#' Does the object carry attributes that parallel its rows?
#' @keywords internal
#' @noRd
has_parallel_attributes <- function(x) {
	fmMat <- attr(x, "formulaMatrix")
	tmTab <- attr(x, "termTable")
	is.data.frame(fmMat) && is.data.frame(tmTab) && nrow(fmMat) == nrow(x)
}

#' @importFrom dplyr dplyr_reconstruct
#' @export
dplyr_reconstruct.mdl_tbl <- function(data, template) {
  model_table_reconstruct(x = data, to = template)
}

#' @importFrom dplyr dplyr_row_slice
#' @export
dplyr_row_slice.mdl_tbl <- function(data, i, ...) {
	model_table_reconstruct(vec_slice(data, i), data)
}

#' @export
`[.mdl_tbl` <- function(x, i, j, ..., drop = FALSE) {
	out <- NextMethod()
	if (!is.data.frame(out)) {
		return(out)
	}
	model_table_reconstruct(out, x)
}

#' @keywords internal
model_table_reconstruct <- function(x, to) {
	# All invariant columns must survive the operation; added columns are fine
	if (all(model_table_columns() %in% names(x))) {
		df_reconstruct(x, to)
	} else {
		lost <- setdiff(model_table_columns(), names(x))
		message(
			"Column(s) `",
			paste(lost, collapse = "`, `"),
			"` are invariant to a `mdl_tbl`; ",
			"removing them returns a plain `data.frame`."
		)
		as.data.frame(x)
	}

}

#' Rebuild the scalar attributes of a model table around the rows of `x`
#'
#' The formula matrix keeps only `x`'s rows (in `x`'s order), the term table
#' keeps only terms those rows still use in the roles they still hold, and
#' the data list keeps datasets those rows reference (plus any dataset that
#' was attached deliberately without being referenced).
#' @keywords internal
df_reconstruct <- function(x, to) {

	# The attribute source is the template when it covers all of `x`'s models;
	# otherwise `x`'s own attributes (e.g. after `vec_rbind()` merged tables)
	src <- NULL
	if (has_parallel_attributes(to) && all(x$id %in% to$id)) {
		src <- to
	} else if (has_parallel_attributes(x)) {
		src <- x
	}

	if (is.null(src)) {
		message(
			"These rows reach beyond the original `mdl_tbl`, so its attributes ",
			"cannot be reconciled; returning a plain `data.frame`. ",
			"Combine model tables with `model_table(x, y)` instead."
		)
		return(as.data.frame(x))
	}

	# Formula matrix: x's models, in x's row order, dropping terms no model uses
	idx <- match(x$id, src$id)
	fmMat <- attr(src, "formulaMatrix")[idx, , drop = FALSE]
	fmMat <- fmMat[, colSums(fmMat, na.rm = TRUE) > 0, drop = FALSE]
	rownames(fmMat) <- NULL

	# Term table: keep exactly the definitions the remaining rows point at.
	# Each matrix cell is an ordinal naming which definition of its term that
	# formula reads (`resolve_formula_row()`), so a term the table defines
	# twice -- `race` as one family's exposure and another's modifier -- keeps
	# only the definition some remaining row still uses. Pruning by role and
	# membership alone kept the other family's definition when the bare term
	# was merely a covariate here, and dropped a definition an ordinal still
	# pointed at, which surfaced as `subscript out of bounds` deep in
	# `mdl_gt()`. Random effects are recorded in the matrix like any other
	# term, so they follow the same rule
	tmTab <- attr(src, "termTable")
	referenced <- rep(FALSE, nrow(tmTab))
	for (term in names(fmMat)) {
		definitions <- which(tmTab$term == term)
		cells <- fmMat[[term]]
		ordinals <- unique(cells[!is.na(cells) & cells >= 1])
		referenced[definitions[ordinals[ordinals <= length(definitions)]]] <- TRUE
	}
	newTab <- tmTab[referenced, , drop = FALSE]
	rownames(newTab) <- NULL
	fmMat <- remap_definition_ordinals(fmMat, from = tmTab, to = newTab)

	# Datasets: those x's rows reference, plus any attached without being
	# referenced by the source at all (a deliberate `attach_data()`)
	datLs <- attr(src, "dataList")
	referenced <- unique(src$data_id)
	keepData <-
		names(datLs) %in% unique(x$data_id) | !names(datLs) %in% referenced
	newDat <- datLs[keepData]

	# Rebuild x with the template's class and the reconciled attributes
	attrs <- attributes(to)
	attrs$names <- names(x)
	attrs$row.names <- .row_names_info(x, type = 0L)
	attrs$formulaMatrix <- fmMat
	attrs$termTable <- newTab
	attrs$dataList <- newDat
	attributes(x) <- attrs

	# Any reshape can unearth or dissolve a family (a subset renumbers, a
	# varied-exposures relation vanishes when narrowed to one exposure), so the
	# identification is recomputed against the rows that remain
	stamp_families(x)

}

# Casting and coercion ---------------------------------------------------------

# SELF

#' @keywords internal
mdl_tbl_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {

  # Create a temporary/new structure of the table
	mdTab <- tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)

	# The prototype's attributes hold both tables' rows so that, when
	# `vec_rbind()`/`vec_c()` restores the combined rows to this prototype, the
	# formula matrix stays parallel to the table rows. Each side's cells are
	# re-pointed at the merged term table first: a cell is an ordinal naming
	# which definition of its term the formula reads, and the merge may put
	# the other table's definition of a shared term ahead of this one's
	termTable <- merge_term_tables(attr(x, "termTable"), attr(y, "termTable"))
  new_model_table(
  	x = as.list(mdTab),
  	formulaMatrix = merge_formula_matrices(
  		remap_definition_ordinals(
  			attr(x, "formulaMatrix"), from = attr(x, "termTable"), to = termTable
  		),
  		remap_definition_ordinals(
  			attr(y, "formulaMatrix"), from = attr(y, "termTable"), to = termTable
  		)
  	),
  	termTable = termTable,
  	dataList = merge_data_lists(
  		attr(x, "dataList"),
  		attr(y, "dataList")
  	)
  )

}

#' @keywords internal
mdl_tbl_cast <- function(x, to, ..., x_arg = "", to_arg = "") {

  # Create a temporary/new structure of the table
  mdTab <- tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)

	# The cast keeps x's own formula rows (parity with x's models), widened to
	# the union of terms; the term table and data list gain `to`'s context
	# (issue #26: the data list must survive the cast)
	termTable <- merge_term_tables(attr(x, "termTable"), attr(to, "termTable"))
	xFm <- remap_definition_ordinals(
		attr(x, "formulaMatrix"), from = attr(x, "termTable"), to = termTable
	)
	toFm <- attr(to, "formulaMatrix")
	for (col in setdiff(names(toFm), names(xFm))) {
		xFm[[col]] <- rep(0, nrow(xFm))
	}

	new_model_table(
		x = as.list(mdTab),
		formulaMatrix = xFm,
		termTable = termTable,
		dataList = merge_data_lists(
			attr(x, "dataList"),
			attr(to, "dataList")
		)
	)

}

#' @export
vec_ptype2.mdl_tbl.mdl_tbl <- function(x, y, ...) {
	mdl_tbl_ptype2(x, y, ...)
}

#' @export
vec_cast.mdl_tbl.mdl_tbl <- function(x, to, ...) {
	mdl_tbl_cast(x, to, ...)
}
