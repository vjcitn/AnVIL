.gcloud_do <-
    function(...)
{
    .gcloud_sdk_do("gcloud", c(...))
}

#' @name gcloud-defunct
#'
#' @aliases gcloud
#'
#' @title DEFUNCT - gcloud command line utility interface
#'
#' @description `r lifecycle::badge("deprecated")`\cr
#' These functions invoke the `gcloud` command line utility. See \link{gsutil}
#'   for details on how `gcloud` is located.
NULL

gcloud_runs <- function() {
    tryCatch({
        .gcloud_do("version")[1L] |>
            startsWith("Google Cloud SDK")
    }, error = function(...) FALSE)
}

#' @rdname gcloud-defunct
#'
#' @description `gcloud_exists()` tests whether the `gcloud()` command
#'     can be found on this system. After finding the binary location,
#'     it runs `gcloud version` to identify potentially misconfigured
#'     installations. See 'Details' section of `gsutil` for where the
#'     application is searched.
#'
#' @return `gcloud_exists()` returns `TRUE` when the `gcloud`
#'     application can be found, FALSE otherwise.
#'
#' @examples
#' library(AnVILGCP)
#' gcloud_exists()
#'
#' @export
gcloud_exists <-
    function()
{
    lifeCycle(
        newpackage = "AnVILGCP",
        package = "AnVIL",
        cycle = "defunct",
        title = "gcloud"
    )
    result <- tryCatch({
        .gcloud_sdk_find_binary("gcloud")
    }, error = function(...) "")
    nzchar(result) && gcloud_runs()
}

#' @importFrom utils tail
.gcloud_get_value_check <-
    function(result, function_name)
{
    value <- tail(result, 1L)
    if (identical(value, "(unset)")) {
        message <- paste0(
            "'", function_name, "()' returned '(unset)'; this may indicate ",
            "that the gcloud active configuration is incorrect. Try ",
            "`gcloud auth application-default login` at the command line"
        )
        warning(paste(strwrap(message), collapse = "\n"))
    }
    value
}

#' @rdname gcloud-defunct
#'
#' @description `gcloud_account()`: report the current gcloud account
#'     via `gcloud config get-value account`.
#'
#' @param account character(1) Google account (e.g., `user@gmail.com`)
#'     to use for authentication.
#'
#' @return `gcloud_account()` returns a `character(1)` vector
#'     containing the active gcloud account, typically a gmail email
#'     address.
#'
#' @examples
#' if (gcloud_exists())
#'     gcloud_account()
#'
#' @export
gcloud_account <- function(account = NULL) {
    stopifnot(isScalarCharacter_or_NULL(account))
    lifeCycle(
        newpackage = "AnVILGCP",
        package = "AnVIL",
        cycle = "defunct",
        title = "gcloud"
    )
    if (!is.null(account))
        .gcloud_do("config", "set", "account", account)
    result <- .gcloud_do("config", "get-value", "account")
    .gcloud_get_value_check(result, "gcloud_account")
}

#' @rdname gcloud-defunct
#'
#' @description `gcloud_project()`: report the current gcloud project
#'     via `gcloud config get-value project`.
#'
#' @param project character(1) billing project name.
#'
#' @return `gcloud_project()` returns a `character(1)` vector
#'     containing the active gcloud project.
#'
#' @export
gcloud_project <- function(project = NULL) {
    stopifnot(
        isScalarCharacter_or_NULL(project)
    )
    lifeCycle(
        newpackage = "AnVILGCP",
        package = "AnVIL",
        cycle = "defunct",
        title = "gcloud"
    )
    if (!is.null(project))
        .gcloud_do("config", "set", "project", project)
    result <- .gcloud_do("config", "get-value", "project")
    ## returns two lines when `CLOUDSDK_ACTIVE_CONFIG_NAME=`
    ## envirionment variable is set
    .gcloud_get_value_check(result, "gcloud_account")
}

#' @rdname gcloud-defunct
#'
#' @description `gcloud_help()`: queries `gcloud` for help for a
#'     command or sub-comand via `gcloud help ...`.
#'
#' @param ... Additional arguments appended to gcloud commands.
#'
#' @return `gcloud_help()` returns an unquoted `character()` vector
#'     representing the text of the help manual page returned by
#'     `gcloud help ...`.
#'
#' @examples
#' if (gcloud_exists())
#'     gcloud_help()
#'
#' @export
gcloud_help <- function(...) {
    lifeCycle(
        newpackage = "AnVILGCP",
        package = "AnVIL",
        cycle = "defunct",
        title = "gcloud"
    )
    .gcloud_sdk_result(.gcloud_do("help", ...))
}

#' @rdname gcloud-defunct
#'
#' @description `gcloud_cmd()` allows arbitrary `gcloud` command
#'     execution via `gcloud ...`. Use pre-defined functions in
#'     preference to this.
#'
#' @param cmd `character(1)` representing a command used to evaluate
#'     `gcloud cmd ...`.
#'
#' @return `gcloud_cmd()` returns a `character()` vector representing
#'     the text of the output of `gcloud cmd ...`
#'
#' @export
gcloud_cmd <- function(cmd, ...) {
    lifeCycle(
        newpackage = "AnVILGCP",
        package = "AnVIL",
        cycle = "defunct",
        title = "gcloud"
    )
    .gcloud_do(cmd, ...)
}

#' @rdname gcloud-defunct
#'
#' @description `gcloud_storage()` allows arbitrary `gcloud storage` command
#'   execution via `gcloud storage ...`. Typically used for bucket management
#'   commands such as `rm` and `cp`.
#'
#' @export
gcloud_storage <- function(cmd, ...) {
    lifeCycle(
        newpackage = "AnVILGCP",
        package = "AnVIL",
        cycle = "defunct",
        title = "gcloud"
    )
    .gcloud_do("storage", cmd, ...)
}

#' @rdname gcloud-defunct
#'
#' @description `gcloud_storage_buckets()` provides an interface to the
#'  `gcloud storage buckets` command. This command can be used to create a new
#'   bucket via `gcloud storage buckets create ...`.
#'
#' @param bucket_cmd `character(1)` representing a buckets command typically
#'   used to create a new bucket. It can also be used to
#'   `add-iam-policy-binding` or `remove-iam-policy-binding` to a bucket.
#'
#' @param bucket `character(1)` representing a unique bucket name to be created
#'   or modified.
#'
#' @importFrom BiocBaseUtils isScalarCharacter
#' @export
gcloud_storage_buckets <- function(bucket_cmd = "create", bucket, ...) {
    stopifnot(
        isScalarCharacter(bucket_cmd), isScalarCharacter(bucket)
    )
    lifeCycle(
        newpackage = "AnVILGCP",
        package = "AnVIL",
        cycle = "defunct",
        title = "gcloud"
    )
    gcloud_storage("buckets", bucket_cmd, bucket, ...)
}
