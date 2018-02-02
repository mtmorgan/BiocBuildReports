#' @importFrom utils relist
#' @export
cleandeps <- function(x) {
    x <- strsplit(x, ",")
    x <- relist(trimws(unlist(x)), x)
    x <- vapply(x, paste, character(1), collapse=", ")
    x[x == "NA"] <- NA
    x
}

#' @importFrom tools package_dependencies
.pkg_deps <- function(packages, db, ..., recursive) {
    deps <- package_dependencies(
        packages, db, ..., reverse = TRUE, recursive = recursive
    )
    is_CRAN <- db[startsWith(db[,"Repository"], "https://cran"), "Package"]
    tibble(
        package = rep(names(deps), lengths(deps)),
        has_rev_dep = unlist(deps),
        dep_is_CRAN = unlist(deps) %in% is_CRAN
    )
}

globalVariables(c("package", "has_rev_dep"))

#' @importFrom dplyr mutate
#' @export
pkg_deps <- function(packages, db, ...) {
    recursive <- .pkg_deps(packages, db, ..., recursive = TRUE)
    direct <- .pkg_deps(packages, db, ..., recursive = FALSE)
    mutate(recursive, type = {
        x <- paste(package, has_rev_dep, sep="\r")
        table <- paste(direct$package, direct$has_rev_dep, sep="\r")
        type <- ifelse(x %in% table, "direct", "recursive")
        factor(type, levels=c("recursive", "direct"))
    })
}
