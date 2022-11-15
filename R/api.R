## https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html

library(httr)


rawsugar_api <- function(path) {
    url <- modify_url("http://dev-rawsugar.parkerici.org/api", path = path)
    GET(url)
}

## bypass security for now
rawsugar_api <- function(path) {
    url <- modify_url("http://localhost:1888/", path = path)
    resp <- GET(url)
    if (http_type(resp) != "application/transit+json") {
        stop("API did not return transit+json", call. = FALSE)
    }
    untransit(jsonlite::fromJSON(content(resp, "text"), simplifyVector = FALSE))
}

# resp <- rawsugar_api('/api/projects')
