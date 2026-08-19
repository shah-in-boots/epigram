# Building the <mdl_gt> semantic pipeline -------------------------------------
#
# This is the single realization path. A specification becomes selected
# models, semantic effects, normalized conditions, atomic measures, and then
# built-in presentation groups. Layout compilation lives in table-layout.R;
# rendering knows only the resulting cell frame.

#' The resolved build behind one `<mdl_gt>`
#' @keywords internal
#' @noRd
mdl_gt_build <- S7::new_class(
	"mdl_gt_build",
	package = "recast",
	properties = list(
		selected = S7::class_data.frame,
		effects = S7::class_data.frame,
		conditions = S7::class_data.frame,
		measures = S7::class_data.frame,
		groups = S7::class_data.frame,
		layout = S7::class_list,
		cells = S7::class_data.frame
	)
)

#' Inspect a model-table presentation before rendering
#'
#' `inspect_mdl_gt()` exposes each plain-data stage of the effect-and-
#' presentation pipeline. It is the troubleshooting counterpart to [as_gt()].
#'
#' @param x A `<mdl_gt>` specification.
#' @param stage One of `"effects"`, `"conditions"`, `"measures"`, `"groups"`,
#'   `"cells"`, or `"all"`.
#' @return A tibble for a single stage, or a named list for `"all"`.
#' @export
inspect_mdl_gt <- function(x, stage = c(
	"effects", "conditions", "measures", "groups", "cells", "all"
)) {
	validate_class(x, "mdl_gt")
	stage <- match.arg(stage)
	b <- build_mdl_gt(x)
	if (stage == "all") {
		return(list(
			selected = b@selected, effects = b@effects,
			conditions = b@conditions, measures = b@measures,
			groups = b@groups, layout = b@layout, cells = b@cells
		))
	}
	switch(stage,
		effects = b@effects,
		conditions = b@conditions,
		measures = b@measures,
		groups = b@groups,
		cells = b@cells
	)
}

#' Build every semantic stage once
#' @keywords internal
#' @noRd
build_mdl_gt <- function(x) {
	validate_class(x, "mdl_gt")
	sel <- resolve_mdl_gt_selection(x)
	effects <- realize_mdl_gt_effects(x, sel)
	conditions <- effect_conditions(effects)
	measures <- realize_mdl_gt_measures(x, effects)
	groups <- materialize_mdl_gt_groups(x, effects, measures)
	layout <- resolve_mdl_gt_layout(x, effects, groups)
	cells <- compile_mdl_gt_cells(x, effects, groups, layout)
	mdl_gt_build(
		selected = tibble::as_tibble(sel$models),
		effects = effects,
		conditions = conditions,
		measures = measures,
		groups = groups,
		layout = layout,
		cells = cells
	)
}

# Selection ------------------------------------------------------------------

#' @keywords internal
#' @noRd
selection_input <- function(x) {
	if (is.null(x)) return(list())
	labeled_formulas_to_named_list(x)
}

#' @keywords internal
#' @noRd
adjustment_set_index <- function(x) {
	fam <- identify_families(model_table_formulas(x))
	if (!nrow(fam)) return(integer())
	sig <- vapply(fam$covariates, paste, character(1), collapse = "\r")
	first <- !duplicated(sig)
	sizes <- lengths(fam$covariates)[first]
	rungs <- sig[first][order(sizes, seq_along(sizes))]
	match(sig, rungs)
}

#' Resolve requested terms with their model/data metadata
#' @keywords internal
#' @noRd
resolve_term_metadata <- function(x, requested) {
	datLs <- attr(x, "dataList")
	referenced <- unique(stats::na.omit(x$data_id))
	hit <- referenced[referenced %in% names(datLs)]
	tmTab <- vec_restore(attr(x, "termTable"), to = tm())
	seen <- character()
	for (data in datLs[c(hit, setdiff(names(datLs), hit))]) {
		newCols <- setdiff(names(data), seen)
		if (length(newCols)) {
			tmTab <- set_data(tmTab, data[newCols])
			seen <- c(seen, newCols)
		}
	}
	proxy <- vec_proxy(tmTab)
	empty <- tibble::tibble(
		variable = character(), role = character(), label = character(),
		type = character(), distribution = character(), levels = list(),
		reference = character(), keys = list()
	)
	if (!length(requested)) return(empty)
	unknown <- setdiff(names(requested), proxy$term)
	if (length(unknown)) {
		stop(
			"No term matches ", paste0("`", unknown, "`", collapse = ", "),
			". Available terms: ",
			paste0("`", unique(proxy$term), "`", collapse = ", "), ".",
			call. = FALSE
		)
	}
	rows <- lapply(names(requested), function(v) {
		row <- proxy[proxy$term == v, , drop = FALSE][1, , drop = FALSE]
		lvls <- row$level[[1]]
		if (is.null(lvls)) lvls <- character()
		label <- requested[[v]]
		label <- if (length(label) && !is.na(label[1]) && nzchar(label[1]) &&
				!identical(as.character(label[1]), v)) {
			as.character(label[1])
		} else if (!is.na(row$label) && nzchar(row$label)) {
			row$label
		} else v
		keys <- if (length(lvls) > 1) {
			unique(c(v, paste0(v, lvls[-1])))
		} else v
		tibble::tibble(
			variable = v, role = as.character(row$role), label = label,
			type = as.character(row$type),
			distribution = as.character(row$distribution), levels = list(lvls),
			reference = if (length(lvls)) lvls[1] else NA_character_,
			keys = list(keys)
		)
	})
	dplyr::bind_rows(rows)
}

#' @keywords internal
#' @noRd
match_term_keys <- function(keys, metadata) {
	if (!nrow(metadata)) return(rep(NA_character_, length(keys)))
	lookup <- character()
	for (i in seq_len(nrow(metadata))) {
		lookup[metadata$keys[[i]]] <- metadata$variable[i]
	}
	unname(lookup[keys])
}

#' @keywords internal
#' @noRd
mdl_gt_display_terms <- function(mt, selection, models) {
	if (!is.null(selection$terms)) {
		return(resolve_term_metadata(mt, selection_input(selection$terms)))
	}
	exposures <- unique(stats::na.omit(models$exposure))
	if (length(exposures)) {
		return(resolve_term_metadata(mt, stats::setNames(as.list(exposures), exposures)))
	}
	proxy <- vec_proxy(term_table(mt))
	keep <- proxy$role != "outcome"
	if ("side" %in% names(proxy)) keep <- keep & proxy$side != "meta"
	vars <- unique(proxy$term[keep])
	resolve_term_metadata(mt, stats::setNames(as.list(vars), vars))
}

#' @keywords internal
#' @noRd
resolve_mdl_gt_selection <- function(x) {
	mt <- x@mdl_tbl
	adjLabels <- selection_input(x@selection$adjustment)
	adj <- adjustment_set_index(mt)
	keep <- rep(TRUE, nrow(mt))
	if (length(adjLabels)) {
		wanted <- suppressWarnings(as.integer(names(adjLabels)))
		if (anyNA(wanted)) {
			stop("Adjustment selections must be numbered adjustment sets.",
					 call. = FALSE)
		}
		bad <- setdiff(wanted, sort(unique(adj)))
		if (length(bad)) {
			stop("No adjustment set numbered ", paste(bad, collapse = ", "),
					 " is available; `adjustment_sets()` shows the mapping.",
					 call. = FALSE)
		}
		keep <- adj %in% wanted
	}
	models <- mt[keep, , drop = FALSE]
	if (!nrow(models)) {
		stop("No models on the mesa match the current selection.", call. = FALSE)
	}
	terms <- mdl_gt_display_terms(mt, x@selection, models)
	if (!nrow(terms) && !isTRUE(x@effects$interaction)) {
		stop("Nothing to display: choose a term with `select_terms()`.",
				 call. = FALSE)
	}
	list(
		models = models,
		adjustment_index = adj[keep],
		terms = terms,
		adjustment_labels = adjLabels
	)
}

# Effects --------------------------------------------------------------------

#' @keywords internal
#' @noRd
realize_mdl_gt_effects <- function(x, selection) {
	effects <- if (isTRUE(x@effects$interaction)) {
		realize_interaction_effects(x, selection)
	} else {
		realize_coefficient_effects(x, selection)
	}
	if (!nrow(effects)) {
		stop("The selected models produced no displayable effects.", call. = FALSE)
	}
	effects <- apply_effect_labels(effects, x@labels$relabels)
	effects <- effects[order(
		effects$outcome_order, effects$stratum_order, effects$adjustment_order,
		effects$term_order, effects$modifier_order, effects$modifier_level_order,
		effects$contrast_order, effects$model_order
	), , drop = FALSE]
	effects$effect_id <- sprintf("effect_%05d", seq_len(nrow(effects)))
	tibble::as_tibble(effects[effect_frame_fields()])
}

#' @keywords internal
#' @noRd
effect_frame_fields <- function() {
	c(
		"effect_id", "model", "source", "outcome", "outcome_label", "term",
		"term_label", "contrast", "contrast_label", "adjustment",
		"adjustment_label", "modifier", "modifier_label", "modifier_level",
		"modifier_level_label", "stratum", "stratum_level", "subset", "dataset",
		"family", "is_reference", "estimate", "conf_low", "conf_high",
		"p_value", "nobs", "exponentiated", "outcome_order", "term_order",
		"contrast_order", "adjustment_order", "modifier_order",
		"modifier_level_order", "stratum_order", "model_order"
	)
}

#' @keywords internal
#' @noRd
realize_coefficient_effects <- function(x, selection) {
	models <- selection$models
	terms <- selection$terms
	config <- x@groups$effect
	exponentiate <- if (is.null(config)) NULL else config$exponentiate
	flat <- suppressMessages(flatten_models(models, exponentiate = exponentiate))
	for (nm in c("estimate", "conf_low", "conf_high", "p_value")) {
		if (!nm %in% names(flat)) flat[[nm]] <- NA_real_
	}
	if (!"nobs" %in% names(flat)) flat$nobs <- NA_real_
	if (!"exponentiated" %in% names(flat)) flat$exponentiated <- FALSE
	flat$variable <- match_term_keys(flat$term, terms)
	flat <- flat[!is.na(flat$variable), , drop = FALSE]
	if (!nrow(flat)) {
		stop("The selected terms have no estimates among the selected models.",
				 call. = FALSE)
	}
	meta <- tibble::tibble(
		variable = terms$variable, term_label = terms$label,
		levels = terms$levels, reference = terms$reference,
		categorical = lengths(terms$levels) > 1,
		term_order = seq_len(nrow(terms))
	)
	dec <- dplyr::left_join(flat, meta, by = "variable")
	dec$contrast <- vapply(seq_len(nrow(dec)), function(i) {
		lvls <- dec$levels[[i]]
		if (length(lvls) <= 1) return(NA_character_)
		hit <- lvls[-1][paste0(dec$variable[i], lvls[-1]) == dec$term[i]]
		if (length(hit)) hit[1] else NA_character_
	}, character(1))
	dec$is_reference <- FALSE

	catRows <- dec[dec$categorical & !is.na(dec$categorical), , drop = FALSE]
	if (nrow(catRows)) {
		keys <- intersect(c(
			"id", "formula_call", "model_call", "outcome", "exposure",
			"data_id", "strata", "level", "subset", "variable",
			"term_label", "reference", "categorical", "nobs", "exponentiated",
			"term_order"
		), names(catRows))
		refs <- dplyr::distinct(catRows[keys])
		refs$term <- paste0(refs$variable, "__reference")
		refs$contrast <- refs$reference
		refs$is_reference <- TRUE
		refs$estimate <- refs$conf_low <- refs$conf_high <- refs$p_value <- NA_real_
		dec <- dplyr::bind_rows(dec, refs)
	}

	modelKeys <- model_identity_key(
		models$data_id, models$model_call, models$outcome, models$exposure,
		models$strata, models$level, models$subset, models$formula_call
	)
	decKeys <- model_identity_key(
		dec$data_id, dec$model_call, dec$outcome, dec$exposure,
		dec$strata, dec$level, dec$subset, dec$formula_call
	)
	modelIndex <- match(decKeys, modelKeys)
	adj <- selection$adjustment_index[modelIndex]
	modelMeta <- as.data.frame(models)[modelIndex, , drop = FALSE]
	adjLabel <- adjustment_labels(adj, selection$adjustment_labels)
	contrastOrder <- vapply(seq_len(nrow(dec)), function(i) {
		lvls <- dec$levels[[i]]
		if (length(lvls) <= 1 || is.na(dec$contrast[i])) 1L else
			match(dec$contrast[i], lvls)
	}, integer(1))
	modelOrder <- modelIndex
	data.frame(
		model = as.character(models$id[modelIndex]), source = "coefficient",
		outcome = dec$outcome, outcome_label = dec$outcome,
		term = dec$variable, term_label = dec$term_label,
		contrast = dec$contrast, contrast_label = dec$contrast,
		adjustment = as.integer(adj), adjustment_label = adjLabel,
		modifier = NA_character_, modifier_label = NA_character_,
		modifier_level = NA_character_, modifier_level_label = NA_character_,
		stratum = modelMeta$strata, stratum_level = modelMeta$level,
		subset = modelMeta$subset, dataset = modelMeta$data_id,
		family = modelMeta$family, is_reference = dec$is_reference,
		estimate = dec$estimate, conf_low = dec$conf_low,
		conf_high = dec$conf_high, p_value = dec$p_value, nobs = dec$nobs,
		exponentiated = dec$exponentiated,
		outcome_order = match(dec$outcome, unique(models$outcome)),
		term_order = dec$term_order, contrast_order = contrastOrder,
		adjustment_order = as.integer(adj), modifier_order = 1L,
		modifier_level_order = 1L,
		stratum_order = match(na_to_blank(modelMeta$level),
			unique(na_to_blank(models$level))),
		model_order = modelOrder,
		stringsAsFactors = FALSE
	)
}

#' Every modifier declared by each selected model
#' @keywords internal
#' @noRd
model_modifier_terms <- function(models) {
	proxy <- vec_proxy(term_table(models))
	mods <- unique(proxy$term[proxy$role == "interaction" &
		!grepl(":", proxy$term, fixed = TRUE)])
	fm <- formula_matrix(models)
	lapply(seq_len(nrow(models)), function(i) {
		candidates <- mods[mods %in% names(fm)]
		hit <- candidates[vapply(candidates, function(m) fm[[m]][i] >= 1,
			logical(1))]
		if (!length(hit) && !is.na(models$interaction[i])) models$interaction[i] else hit
	})
}

#' @keywords internal
#' @noRd
realize_interaction_effects <- function(x, selection) {
	models <- selection$models
	modsByModel <- model_modifier_terms(models)
	keep <- lengths(modsByModel) > 0
	models <- models[keep, , drop = FALSE]
	modsByModel <- modsByModel[keep]
	adj <- selection$adjustment_index[keep]
	if (!nrow(models)) {
		stop("`add_interaction()` needs models with declared modifier terms.",
				 call. = FALSE)
	}
	requested <- if (is.null(x@selection$terms)) character() else
		names(selection_input(x@selection$terms))
	if (length(requested)) {
		keep <- models$exposure %in% requested
		models <- models[keep, , drop = FALSE]
		modsByModel <- modsByModel[keep]
		adj <- adj[keep]
	}
	if (!nrow(models)) stop("No selected exposure has interaction effects.", call. = FALSE)

	vars <- unique(c(models$exposure, unlist(modsByModel)))
	meta <- resolve_term_metadata(x@mdl_tbl, stats::setNames(as.list(vars), vars))
	effectConfig <- x@groups$effect
	exponentiate <- if (is.null(effectConfig)) NULL else effectConfig$exponentiate
	flat <- suppressMessages(flatten_models(models, exponentiate = exponentiate))
	modelKeys <- model_identity_key(
		models$data_id, models$model_call, models$outcome, models$exposure,
		models$strata, models$level, models$subset, models$formula_call
	)
	flatKeys <- model_identity_key(
		flat$data_id, flat$model_call, flat$outcome, flat$exposure,
		flat$strata, flat$level, flat$subset, flat$formula_call
	)
	expFlag <- vapply(seq_len(nrow(models)), function(i) {
		flag <- unique(flat$exponentiated[flatKeys == modelKeys[i]])
		isTRUE(flag[1])
	}, logical(1))
	rows <- list()
	for (i in seq_len(nrow(models))) {
		model <- models[i, , drop = FALSE]
		for (j in seq_along(modsByModel[[i]])) {
			modifier <- modsByModel[[i]][j]
			est <- estimate_interaction(
				model, exposure = model$exposure, interaction = modifier,
				conf_level = x@effects$conf_level
			)
			if (isTRUE(expFlag[i])) {
				est$estimate <- exp(est$estimate)
				est$conf_low <- exp(est$conf_low)
				est$conf_high <- exp(est$conf_high)
			}
			contrast <- if ("exposure_level" %in% names(est)) {
				as.character(est$exposure_level)
			} else rep(NA_character_, nrow(est))
			termLabel <- unname(meta$label[match(model$exposure, meta$variable)])
			modifierLabel <- unname(meta$label[match(modifier, meta$variable)])
			rows[[length(rows) + 1L]] <- data.frame(
				model = unname(as.character(model$id)), source = "interaction",
				outcome = model$outcome, outcome_label = model$outcome,
				term = model$exposure, term_label = termLabel,
				contrast = contrast, contrast_label = contrast,
				adjustment = as.integer(adj[i]),
				adjustment_label = adjustment_labels(adj[i], selection$adjustment_labels),
				modifier = modifier, modifier_label = modifierLabel,
				modifier_level = as.character(est$level),
				modifier_level_label = as.character(est$level),
				stratum = model$strata, stratum_level = model$level,
				subset = model$subset, dataset = model$data_id,
				family = model$family, is_reference = FALSE,
				estimate = est$estimate, conf_low = est$conf_low,
				conf_high = est$conf_high, p_value = est$p_value,
				nobs = est$nobs, exponentiated = isTRUE(expFlag[i]),
				outcome_order = match(model$outcome, unique(models$outcome)),
				term_order = match(model$exposure, unique(models$exposure)),
				contrast_order = ifelse(is.na(contrast), 1L,
					match(contrast, unique(contrast))),
				adjustment_order = as.integer(adj[i]), modifier_order = j,
				modifier_level_order = seq_len(nrow(est)),
				stratum_order = match(na_to_blank(model$level),
					unique(na_to_blank(models$level))),
				model_order = i, row.names = NULL, stringsAsFactors = FALSE
			)
		}
	}
	dplyr::bind_rows(rows)
}

#' @keywords internal
#' @noRd
adjustment_labels <- function(index, labels) {
	vapply(as.character(index), function(k) {
		label <- labels[[k]]
		if (is.null(label)) paste0("Model ", k) else as.character(label)
	}, character(1))
}

#' Relabel every semantic dimension before measures/layout
#' @keywords internal
#' @noRd
apply_effect_labels <- function(effects, relabels) {
	for (nm in names(relabels)) {
		value <- relabels[[nm]]
		if (nm %in% effects$outcome && length(value) == 1) {
			effects$outcome_label[effects$outcome == nm] <- as.character(value)
		}
		if (nm %in% effects$term) {
			if (length(value) == 1) {
				effects$term_label[effects$term == nm] <- as.character(value)
			} else {
				levels <- unique(stats::na.omit(effects$contrast[effects$term == nm]))
				for (i in seq_along(levels)) if (i <= length(value)) {
					effects$contrast_label[effects$term == nm &
						effects$contrast == levels[i]] <- as.character(value[i])
				}
			}
		}
		if (nm %in% effects$modifier) {
			if (length(value) == 1) {
				effects$modifier_label[effects$modifier == nm] <- as.character(value)
			} else {
				levels <- unique(stats::na.omit(
					effects$modifier_level[effects$modifier == nm]
				))
				for (i in seq_along(levels)) if (i <= length(value)) {
					effects$modifier_level_label[effects$modifier == nm &
						effects$modifier_level == levels[i]] <- as.character(value[i])
				}
			}
		}
		# A bare value can relabel either kind of level.
		effects$contrast_label[!is.na(effects$contrast) & effects$contrast == nm] <-
			as.character(value[1])
		effects$modifier_level_label[!is.na(effects$modifier_level) &
			effects$modifier_level == nm] <- as.character(value[1])
	}
	effects
}

#' @keywords internal
#' @noRd
effect_conditions <- function(effects) {
	rows <- list()
	add <- function(kind, variable, level, label, level_label, order) {
		hit <- !is.na(variable) & nzchar(variable)
		if (!any(hit)) return(NULL)
		tibble::tibble(
			effect_id = effects$effect_id[hit], condition_kind = kind,
			variable = variable[hit], level = level[hit], label = label[hit],
			level_label = level_label[hit], condition_order = order[hit]
		)
	}
	rows[[1]] <- add("stratum", effects$stratum, effects$stratum_level,
		effects$stratum, effects$stratum_level, effects$stratum_order)
	rows[[2]] <- add("modifier", effects$modifier, effects$modifier_level,
		effects$modifier_label, effects$modifier_level_label,
		effects$modifier_level_order)
	subsetHit <- !is.na(effects$subset) & nzchar(effects$subset)
	if (any(subsetHit)) {
		rows[[3]] <- tibble::tibble(
			effect_id = effects$effect_id[subsetHit], condition_kind = "subset",
			variable = "subset", level = effects$subset[subsetHit], label = "Subset",
			level_label = effects$subset[subsetHit], condition_order = 1L
		)
	}
	dplyr::bind_rows(rows)
}

# Measures -------------------------------------------------------------------

#' @keywords internal
#' @noRd
measure_dimensions <- function() {
	c(
		"model", "outcome", "outcome_label", "term", "term_label", "contrast",
		"contrast_label", "adjustment", "adjustment_label", "modifier",
		"modifier_label", "modifier_level", "modifier_level_label", "stratum",
		"stratum_level", "subset", "dataset"
	)
}

#' @keywords internal
#' @noRd
new_measure_rows <- function(base, statistic, value, group, grain,
		effect_id = base$effect_id) {
	out <- base[measure_dimensions()]
	out$measure_id <- NA_character_
	out$effect_id <- effect_id
	out$group_id <- group
	out$statistic <- statistic
	out$grain <- grain
	out$value <- as.numeric(value)
	out[c("measure_id", "effect_id", measure_dimensions(), "group_id",
		"statistic", "grain", "value")]
}

#' @keywords internal
#' @noRd
realize_mdl_gt_measures <- function(x, effects) {
	parts <- list(
		new_measure_rows(effects, "estimate", effects$estimate, "effect", "effect"),
		new_measure_rows(effects, "conf_low", effects$conf_low, "effect", "effect"),
		new_measure_rows(effects, "conf_high", effects$conf_high, "effect", "effect")
	)
	if ("p" %in% names(x@groups)) {
		if (any(effects$source == "interaction")) {
			key <- semantic_key(effects, c(
				"model", "outcome", "term", "adjustment", "modifier", "stratum",
				"stratum_level", "subset", "dataset"
			))
			p <- effects[!duplicated(key), , drop = FALSE]
			p$contrast <- p$contrast_label <- p$modifier_level <-
				p$modifier_level_label <- NA_character_
			parts[[length(parts) + 1L]] <- new_measure_rows(
				p, "p_value", p$p_value, "p", "modifier_group"
			)
		} else {
			parts[[length(parts) + 1L]] <- new_measure_rows(
				effects, "p_value", effects$p_value, "p", "effect"
			)
		}
	}
	if ("n" %in% names(x@groups)) {
		if (any(effects$source == "interaction")) {
			key <- semantic_key(effects, c(
				"model", "outcome", "adjustment", "modifier", "modifier_level",
				"stratum", "stratum_level", "subset", "dataset"
			))
			n <- effects[!duplicated(key), , drop = FALSE]
			n$term <- n$term_label <- n$contrast <- n$contrast_label <- NA_character_
			parts[[length(parts) + 1L]] <- new_measure_rows(
				n, "n", n$nobs, "n", "condition"
			)
		} else {
			key <- semantic_key(effects, c(
				"model", "outcome", "adjustment", "stratum", "stratum_level",
				"subset", "dataset"
			))
			n <- effects[!duplicated(key), , drop = FALSE]
			n$term <- n$term_label <- n$contrast <- n$contrast_label <- NA_character_
			parts[[length(parts) + 1L]] <- new_measure_rows(
				n, "n", n$nobs, "n", "model"
			)
		}
	}
	if (any(c("events", "rate", "rate_difference") %in% names(x@groups))) {
		parts <- c(parts, realize_data_measures(x, effects))
	}
	measures <- dplyr::bind_rows(parts)
	measures$measure_id <- sprintf("measure_%06d", seq_len(nrow(measures)))
	tibble::as_tibble(measures)
}

#' Data-derived event, rate, and rate-difference measures
#' @keywords internal
#' @noRd
realize_data_measures <- function(x, effects) {
	if (!all(c("events", "rate") %in% names(x@groups))) {
		stop("Event and rate cell groups travel together; call `add_events()`.",
				 call. = FALSE)
	}
	if ("rate_difference" %in% names(x@groups) &&
			!all(c("events", "rate") %in% names(x@groups))) {
		stop("`add_rate_difference()` requires `add_events()`.", call. = FALSE)
	}
	if (!requireNamespace("survival", quietly = TRUE)) {
		stop("Event-rate measures require the {survival} package.", call. = FALSE)
	}
	config <- x@groups$events
	datLs <- attr(x@mdl_tbl, "dataList")
	contextDims <- c(
		"dataset", "outcome", "term", "modifier", "modifier_level", "stratum",
		"stratum_level", "subset"
	)
	contexts <- effects[!duplicated(semantic_key(effects, contextDims)), , drop = FALSE]
	parts <- list()
	for (i in seq_len(nrow(contexts))) {
		ctx <- contexts[i, , drop = FALSE]
		dat <- datLs[[ctx$dataset]]
		if (is.null(dat)) {
			stop("No attached dataset matches `", ctx$dataset,
					 "`; use `attach_data()`.", call. = FALSE)
		}
		dat <- slice_effect_data(dat, ctx)
		if (!ctx$term %in% names(dat) || !is.factor(dat[[ctx$term]]) ||
				nlevels(dat[[ctx$term]]) < 2) {
			stop("Event-rate measures require categorical focal term `", ctx$term,
					 "` in the attached data.", call. = FALSE)
		}
		if (!config$followup %in% names(dat)) {
			stop("Follow-up column `", config$followup, "` is absent from `",
					 ctx$dataset, "`.", call. = FALSE)
		}
		surv <- parse_surv_outcome(ctx$outcome)
		event <- if (ctx$outcome %in% names(dat)) ctx$outcome else
			if (!is.null(surv) && surv$event %in% names(dat)) surv$event else NULL
		if (is.null(event)) {
			stop("Outcome `", ctx$outcome, "` does not resolve to an event column.",
					 call. = FALSE)
		}
		py <- survival::pyears(
			survival::Surv(dat[[config$followup]], dat[[event]]) ~ dat[[ctx$term]],
			scale = config$scale
		)
		levels <- levels(dat[[ctx$term]])
		events <- as.numeric(py$event)
		personTime <- as.numeric(py$pyears) / config$person_years
		rate <- events / personTime
		hit <- semantic_context_match(effects, ctx, contextDims)
		base <- effects[hit, , drop = FALSE]
		# Data statistics are invariant to model adjustment. Keep one semantic
		# anchor per term contrast/condition and deliberately erase model grain so
		# a levels layout creates statistic rows rather than repeated model rows.
		base <- base[!duplicated(semantic_key(base, c(
			"outcome", "term", "contrast", "modifier", "modifier_level",
			"stratum", "stratum_level", "subset", "dataset"
		))), , drop = FALSE]
		base$model <- NA_character_
		base$adjustment <- NA_integer_
		base$adjustment_label <- NA_character_
		base$effect_id <- NA_character_
		at <- match(base$contrast, levels)
		parts[[length(parts) + 1L]] <- new_measure_rows(
			base, "events", events[at], "events", "effect"
		)
		parts[[length(parts) + 1L]] <- new_measure_rows(
			base, "rate", rate[at], "rate", "effect"
		)
		if ("rate_difference" %in% names(x@groups)) {
			if (length(levels) != 2) {
				stop("Rate differences require exactly two levels of `", ctx$term,
						 "`.", call. = FALSE)
			}
			conf <- x@groups$rate_difference$conf_level
			z <- stats::qnorm(1 - (1 - conf) / 2)
			est <- rate[2] - rate[1]
			se <- sqrt(events[2] / personTime[2]^2 +
				events[1] / personTime[1]^2)
			rd <- base[1, , drop = FALSE]
			rd$contrast <- rd$contrast_label <- NA_character_
			parts[[length(parts) + 1L]] <- new_measure_rows(
				rd, "rate_difference", est, "rate_difference", "term",
				effect_id = NA_character_
			)
			parts[[length(parts) + 1L]] <- new_measure_rows(
				rd, "rate_difference_low", est - z * se, "rate_difference", "term",
				effect_id = NA_character_
			)
			parts[[length(parts) + 1L]] <- new_measure_rows(
				rd, "rate_difference_high", est + z * se, "rate_difference", "term",
				effect_id = NA_character_
			)
		}
	}
	parts
}

#' @keywords internal
#' @noRd
slice_effect_data <- function(data, context) {
	keep <- rep(TRUE, nrow(data))
	if (!is.na(context$stratum) && context$stratum %in% names(data) &&
			!is.na(context$stratum_level)) {
		keep <- keep & as.character(data[[context$stratum]]) == context$stratum_level
	}
	if (!is.na(context$modifier) && context$modifier %in% names(data) &&
			!is.na(context$modifier_level)) {
		keep <- keep & as.character(data[[context$modifier]]) == context$modifier_level
	}
	if (!is.na(context$subset) && nzchar(context$subset)) {
		expr <- tryCatch(str2lang(context$subset), error = function(e) NULL)
		if (!is.null(expr)) {
			sub <- tryCatch(eval(expr, envir = data, enclos = parent.frame()),
				error = function(e) NULL)
			if (is.logical(sub) && length(sub) == nrow(data)) keep <- keep & sub
		}
	}
	data[keep %in% TRUE, , drop = FALSE]
}

# Groups ---------------------------------------------------------------------

#' Materialize built-in presentation groups from atomic measures
#' @keywords internal
#' @noRd
materialize_mdl_gt_groups <- function(x, effects, measures) {
	knownLabels <- c("beta", "conf", mdl_gt_group_ids())
	unknownLabels <- setdiff(names(x@labels$columns), knownLabels)
	if (length(unknownLabels)) {
		stop("`modify_labels(columns = )` names unknown cell groups/statistics: ",
				 paste0("`", unknownLabels, "`", collapse = ", "), ".",
				 call. = FALSE)
	}
	registry <- mdl_gt_group_registry()
	active <- mdl_gt_group_ids()[mdl_gt_group_ids() %in% names(x@groups)]
	parts <- lapply(active, function(id) {
		registry[[id]]$materialize(x, effects, measures, x@groups[[id]])
	})
	groups <- dplyr::bind_rows(parts)
	if (!nrow(groups)) stop("No active cell group has values to display.", call. = FALSE)
	groups$group_cell_id <- sprintf("group_cell_%06d", seq_len(nrow(groups)))
	groups <- groups[c(
		"group_cell_id", "effect_id", measure_dimensions(), "group_id", "subgroup",
		"group_label", "grain", "renderer", "type", "value", "format"
	)]
	tibble::as_tibble(groups)
}

#' Built-in cell-group registry
#' @keywords internal
#' @noRd
mdl_gt_group_registry <- function() {
	list(
		effect = list(
			required_measures = c("estimate", "conf_low", "conf_high"),
			grain = "effect", supported_axes = c("columns", "rows", "body"),
			supported_views = c("merged", "separate"),
			default_format = "estimate_with_interval", renderer = "text",
			default_axis = "columns", order = 30L,
			materialize = materialize_effect_group),
		p = list(
			required_measures = "p_value",
			grain = c("effect", "modifier_group"),
			supported_axes = c("columns", "rows", "body"),
			supported_views = "scalar", default_format = "p_value",
			renderer = "text", default_axis = "columns", order = 40L,
			materialize = materialize_scalar_group),
		n = list(
			required_measures = "n", grain = c("model", "condition"),
			supported_axes = c("columns", "rows", "body"),
			supported_views = "scalar", default_format = "count",
			renderer = "text", default_axis = "columns", order = 10L,
			materialize = materialize_scalar_group),
		events = list(
			required_measures = "events", grain = "effect",
			supported_axes = c("columns", "rows", "body"),
			supported_views = "scalar", default_format = "count",
			renderer = "text", default_axis = "columns", order = 15L,
			materialize = materialize_scalar_group),
		rate = list(
			required_measures = "rate", grain = "effect",
			supported_axes = c("columns", "rows", "body"),
			supported_views = "scalar", default_format = "number",
			renderer = "text", default_axis = "columns", order = 20L,
			materialize = materialize_scalar_group),
		rate_difference = list(
			required_measures = c(
				"rate_difference", "rate_difference_low", "rate_difference_high"
			), grain = "term",
			supported_axes = c("columns", "rows", "body"),
			supported_views = "merged", default_format = "estimate_with_interval",
			renderer = "text", default_axis = "columns", order = 50L,
			materialize = materialize_rate_difference_group),
		forest = list(
			required_measures = c("estimate", "conf_low", "conf_high"),
			grain = "effect", supported_axes = c("columns", "rows", "body"),
			supported_views = "forest", default_format = "forest_axis",
			renderer = "forest", default_axis = "columns", order = 35L,
			materialize = materialize_forest_group)
	)
}

#' @keywords internal
#' @noRd
group_base <- function(measures, statistic) {
	measures[measures$statistic %in% statistic, , drop = FALSE]
}

#' @keywords internal
#' @noRd
materialize_effect_group <- function(x, effects, measures, config) {
	m <- group_base(measures, c("estimate", "conf_low", "conf_high"))
	wide <- atomic_measures_wide(m, c("estimate", "conf_low", "conf_high"))
	digits <- first_of(config$digits, x@style$digits, 2L)
	estimateLabel <- group_label_override(x, "beta", config$labels$estimate)
	confidenceLabel <- group_label_override(x, "conf", config$labels$confidence)
	if (config$view == "separate") {
		parts <- list()
		if (isTRUE(config$show_estimate)) {
			parts[[length(parts) + 1L]] <- group_rows(
				wide, "effect", "estimate", estimateLabel, "effect",
				"text", "numeric", lapply(wide$estimate, function(v) list(estimate = v)),
				list(fmt = "number", digits = digits, pattern = "{estimate}")
			)
		}
		if (isTRUE(config$show_confidence)) {
			parts[[length(parts) + 1L]] <- group_rows(
				wide, "effect", "confidence", confidenceLabel, "effect",
				"text", "numeric", lapply(seq_len(nrow(wide)), function(i) list(
					conf_low = wide$conf_low[i], conf_high = wide$conf_high[i]
				)), list(fmt = "number", digits = digits,
					pattern = "({conf_low}, {conf_high})")
			)
		}
		return(dplyr::bind_rows(parts))
	}
	pattern <- if (isTRUE(config$show_estimate) && isTRUE(config$show_confidence)) {
		"{estimate} ({conf_low}, {conf_high})"
	} else if (isTRUE(config$show_estimate)) "{estimate}" else
		"({conf_low}, {conf_high})"
	label <- if (isTRUE(config$show_estimate) && isTRUE(config$show_confidence)) {
		paste0(estimateLabel, " (", confidenceLabel, ")")
	} else if (isTRUE(config$show_estimate)) estimateLabel else confidenceLabel
	group_rows(
		wide, "effect", "effect", label, "effect", "text", "numeric",
		lapply(seq_len(nrow(wide)), function(i) list(
			estimate = wide$estimate[i], conf_low = wide$conf_low[i],
			conf_high = wide$conf_high[i]
		)), list(fmt = "number", digits = digits, pattern = pattern)
	)
}

#' @keywords internal
#' @noRd
materialize_scalar_group <- function(x, effects, measures, config) {
	stat <- switch(config$id, p = "p_value", n = "n", events = "events", rate = "rate")
	m <- group_base(measures, stat)
	if (!nrow(m)) return(empty_group_frame())
	format <- switch(config$id,
		p = list(fmt = "p", digits = 3L, pattern = "{p_value}"),
		n = list(fmt = "count", digits = 0L, pattern = "{n}"),
		events = list(fmt = "count", digits = 0L, pattern = "{events}"),
		rate = list(fmt = "number", digits = config$digits, pattern = "{rate}")
	)
	group_rows(
		m, config$id, config$id,
		group_label_override(x, config$id, config$label),
		m$grain, "text", "numeric",
		lapply(m$value, function(v) stats::setNames(list(v), stat)), format
	)
}

#' @keywords internal
#' @noRd
materialize_rate_difference_group <- function(x, effects, measures, config) {
	m <- group_base(measures, c(
		"rate_difference", "rate_difference_low", "rate_difference_high"
	))
	wide <- atomic_measures_wide(m, c(
		"rate_difference", "rate_difference_low", "rate_difference_high"
	))
	digits <- first_of(x@groups$rate$digits, x@style$digits, 1L)
	group_rows(
		wide, "rate_difference", "rate_difference",
		group_label_override(x, "rate_difference", config$label),
		"term", "text",
		"numeric", lapply(seq_len(nrow(wide)), function(i) list(
			estimate = wide$rate_difference[i],
			conf_low = wide$rate_difference_low[i],
			conf_high = wide$rate_difference_high[i]
		)), list(fmt = "number", digits = digits,
			pattern = "{estimate} ({conf_low}, {conf_high})")
	)
}

#' @keywords internal
#' @noRd
materialize_forest_group <- function(x, effects, measures, config) {
	if (!"effect" %in% names(x@groups) ||
			!isTRUE(x@groups$effect$show_estimate) ||
			!isTRUE(x@groups$effect$show_confidence)) {
		stop("`add_forest()` requires estimate and confidence measures in `add_estimates()`.",
				 call. = FALSE)
	}
	m <- group_base(measures, c("estimate", "conf_low", "conf_high"))
	wide <- atomic_measures_wide(m, c("estimate", "conf_low", "conf_high"))
	values <- lapply(seq_len(nrow(wide)), function(i) {
		if (isTRUE(config$invert)) {
			list(estimate = 1 / wide$estimate[i], conf_low = 1 / wide$conf_high[i],
				conf_high = 1 / wide$conf_low[i])
		} else list(estimate = wide$estimate[i], conf_low = wide$conf_low[i],
			conf_high = wide$conf_high[i])
	})
	group_rows(
		wide, "forest", "forest", group_label_override(x, "forest", config$label),
		"effect", "forest", "plot",
		values, list(fmt = "number", digits = first_of(x@style$digits, 2L),
			pattern = NULL, axis = config$axis, width = config$width)
	)
}

#' @keywords internal
#' @noRd
atomic_measures_wide <- function(measures, statistics) {
	if (!nrow(measures)) return(measures)
	idCols <- c("effect_id", measure_dimensions(), "grain")
	base <- unique(measures[idCols])
	for (stat in statistics) {
		m <- measures[measures$statistic == stat, c(idCols, "value"), drop = FALSE]
		names(m)[names(m) == "value"] <- stat
		base <- dplyr::left_join(base, m, by = idCols)
	}
	base
}

#' @keywords internal
#' @noRd
group_rows <- function(base, id, subgroup, label, grain, renderer, type,
		values, format) {
	out <- base[c("effect_id", measure_dimensions())]
	out$group_id <- id
	out$subgroup <- subgroup
	out$group_label <- label
	out$grain <- if (length(grain) == 1) rep(grain, nrow(out)) else grain
	out$renderer <- renderer
	out$type <- type
	out$value <- values
	out$format <- rep(list(format), nrow(out))
	out
}

#' @keywords internal
#' @noRd
empty_group_frame <- function() {
	out <- data.frame(effect_id = character(), stringsAsFactors = FALSE)
	for (nm in measure_dimensions()) out[[nm]] <- character()
	out$adjustment <- integer()
	out$value <- I(list())
	out$format <- I(list())
	out
}

#' @keywords internal
#' @noRd
group_label_override <- function(x, id, default) {
	value <- x@labels$columns[[id]]
	if (is.null(value)) default else as.character(value)
}

# Utilities ------------------------------------------------------------------

#' Stable identity shared by model rows and flattened parameter rows
#' @keywords internal
#' @noRd
model_identity_key <- function(data_id, model_call, outcome, exposure,
		strata, level, subset, formula_call) {
	na <- function(x) ifelse(is.na(x), ".NA", as.character(x))
	paste(
		na(data_id), na(model_call), na(outcome), na(exposure), na(strata),
		na(level), na(subset), na(formula_call), sep = "\r"
	)
}

#' @keywords internal
#' @noRd
semantic_key <- function(data, fields) {
	if (!length(fields)) return(rep("", nrow(data)))
	do.call(paste, c(lapply(data[fields], na_to_blank), sep = "\r"))
}

#' @keywords internal
#' @noRd
semantic_context_match <- function(data, context, fields) {
	wanted <- semantic_key(context, fields)[1]
	semantic_key(data, fields) == wanted
}

#' @keywords internal
#' @noRd
na_to_blank <- function(x) ifelse(is.na(x), "", as.character(x))

#' Parse a `Surv()` outcome into time and event arguments
#' @keywords internal
#' @noRd
parse_surv_outcome <- function(outcome) {
	expr <- tryCatch(str2lang(outcome), error = function(e) NULL)
	if (!is.call(expr) || !deparse1(expr[[1]]) %in% c("Surv", "survival::Surv")) {
		return(NULL)
	}
	args <- as.list(expr)[-1]
	event <- if (!is.null(args[["event"]])) args[["event"]] else args[[length(args)]]
	time <- if (!is.null(args[["time"]])) args[["time"]] else args[[1]]
	list(time = deparse1(time), event = deparse1(event))
}
