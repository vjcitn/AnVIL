# setwd("~/bioc/AnVIL")
file_loc <- "inst/service/dockstore/openapi.yaml"

download.file(
    url = "https://dockstore.org/api/openapi.yaml",
    destfile = file_loc
)

docklines <- readLines("R/Dockstore.R")

verline <- grep(
    pattern = ".DOCKSTORE_API_REFERENCE_VERSION <-",
    x = docklines,
    fixed = TRUE,
    value = TRUE
)
oldver <- unlist(strsplit(verline, "\""))[[2]]
newver <- yaml::read_yaml(file_loc)[[c("info", "version")]]
updatedlines <- gsub("\"[0-9a-f]{32}\"", dQuote(newver, FALSE), docklines)

convert <- FALSE
if (convert) {
    oldwd <- setwd("inst/service/dockstore")
    on.exit(setwd(oldwd))
    system2(
        command = "api-spec-converter",
        args = "-f openapi_3 -t swagger_2 openapi.yaml > api.yaml",
        stdout = TRUE
    )
}

## success -- updated API files and MD5
if (!identical(oldver, newver)) {
    writeLines(updatedlines, con = file("R/Dockstore.R"))
    quit(status = 0)
} else {
    ## failure -- API the same
    quit(status = 1)
}
