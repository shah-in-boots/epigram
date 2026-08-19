# nocov start

utils::globalVariables(c(
	"conf_high",
	"conf_low",
	"data",
	"estimate",
	"exposure",
	"ggplots",
	"level",
	"mask",
	"number",
	"outcome",
	"p_value",
	"term",
	"x",
	"y"
))

.onLoad <- function(libname, pkgname) {
	if (!exists("possible_tidy")) {
		possible_tidy <-
			purrr::possibly(my_tidy, otherwise = NA, quiet = FALSE)
	}
	if (!exists("possible_glance")) {
		possible_glance <-
			purrr::possibly(my_glance, otherwise = NA, quiet = FALSE)
	}

	# S7 methods registered on generics owned elsewhere -- base's `print()` and
	# `format()`, and any S7 generic another package might import -- are wired up
	# at load time here. Methods on recast's own S7 generics self-register, so this
	# is belt-and-suspenders; it is also the one line every S7-using package
	# needs and the safest habit. See `vignette("s7")`.
	S7::methods_register()

}

# nocov end
