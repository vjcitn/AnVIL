## Check that the API calls used by AnVIL are consistent with the API
## in the YAML. Requires manual investigation of any removed or
## updated_args_in_use functions.
##
## Use functionality in R/api.R:.api_test_write() to record the
## current interface; .api_test_check() to compare the current and
## previously recorded versions.

test_that("Interfaces are current", {
    skip_if(!GCPtools::gcloud_exists())

    service_status <- .api_test_check(Terra(), "Terra")
    expect_identical(service_status$removed_in_use, character())
    expect_identical(service_status$updated_in_use, character())

    service_status <- .api_test_check(Rawls(), "Rawls")
    expect_identical(service_status$removed_in_use, character())
    expect_identical(service_status$updated_in_use, character())

    service_status <- .api_test_check(Leonardo(), "Leonardo")
    expect_identical(service_status$removed_in_use, character())
    expect_identical(service_status$updated_in_use, character())
})
