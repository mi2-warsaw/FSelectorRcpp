context("Lintr")

linters <- lintr::default_linters
linters$camel_case_linter <- NULL
linters$multiple_dots_linter <- NULL
#lintr::expect_lint_free(linters = linters)
