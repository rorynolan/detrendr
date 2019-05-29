library(future)
plan(multiprocess)

rhub_fedora <- future(rhub::check_on_fedora())
rhub_san <- future(rhub::check_with_sanitizers())
rhub_cran <- future(rhub::check_for_cran())

