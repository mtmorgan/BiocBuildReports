.pkg_url <- function(url, package, host) {
    paste0(url, package, "/", host, "-buildsrc.html")
}

.as_commit <- function(x) {
    x <- as.character(x)
    as.POSIXct(iconv(x, to="ASCII", sub = " "))
}

#' @importFrom httr GET content
#' @importFrom xml2 xml_find_first xml_find_all
#' @importFrom tibble tibble
#' @importFrom stats setNames
#' @export
report <- function(release = "3.7", host = "malbec2") {
    url <- paste0(
        "https://bioconductor.org/checkResults/", release, "/bioc-LATEST/"
    )
    flds <- c("install", "buildsrc", "checksrc")

    report <- content(GET(url))

    metadata <- list(
        host = host,
        release = release,
        snapshot_date = .as_commit(xml_find_first(
            report, "//span[@class='svn_info']/text()"
        ))
    )

    classes <- setNames(paste("status", host, flds), flds)
    lights <- paste("status", host)

    ## "//a allows for strike-out /b/s/a/text() or not /b/a/text()
    path <- "//td/b//a/text()"
    package <- as.character(xml_find_all(report, path))

    path <- "//table[@class='svn_info']/tr[2]//span/text()"
    commit <- .as_commit(xml_find_all(report, path))

    status <- lapply(classes, function(class) {
        path <- sprintf("//td[@class='%s']//text()", class)
        values <- as.character(xml_find_all(report, path))
        trimws(gsub(" +", " ", iconv(values, to = "ASCII", sub = " ")))
    })
    status$buildsrc <- factor(
        status$buildsrc,
        levels=c("ERROR", "TIMEOUT", "WARNINGS", "OK")
    )
    status$checksrc <- factor(
        status$checksrc,
        levels=c("skipped", "ERROR", "TIMEOUT", "WARNINGS", "OK")
    )

    ## lights
    path <- sprintf("//td[@class='%s'][img or .]", lights)
    values <- as.character(xml_find_all(report, path))
    img <- grepl("<img", values)
    values[img] <- sub('.*title="([^\"]+)".*', "\\1", values[img])
    values[!img] <- "skipped"
    ## 
    lightre <- "([A-Z]+)[:,] (.*)"
    light <- list(
        push = factor(
            sub(lightre, "\\1", values),
            levels = c("skipped", "NO", "UNNEEDED", "YES")
        ),
        txt = sub(lightre, "\\2", values)
    )

    result <- do.call(
        tibble,
        c(
            list(package = package, commit = commit),
            status, light,
            list(url = .pkg_url(url, package, host))
        )
    )
    attr(result, "metadata") <- metadata
    class(result) <- c("report", class(result))
    result
}

#' @export
metadata <- function(x)
    attr(x, "metadata")

#' @export
snapshot_date <- function(x)
    metadata(x)$snapshot_date

#' @export
save_report <- function(rpt, dir = ".") {
    file <- file.path(
        dir,
        paste0("rpt_", format(snapshot_date(rpt), "%F"), ".rds")
    )
    stopifnot(!file.exists(file))
    saveRDS(rpt, file = file)
}

#' @importFrom lubridate today
#' @export
load_report <- function(dir = ".", when = format(Sys.time(), "%F")) {
    file <- file.path(dir, paste0("rpt_", when, ".rds"))
    readRDS(file)
}

.print_header <- function(x) {
    cat(
        "release: ", metadata(x)$release, "\n",
        "host: ", metadata(x)$host, "\n",
        "snapshot_date(): ", format(snapshot_date(x), "%F"),
            " (", weekdays(snapshot_date(x)), ")\n",
        sep=""
    )
}

#' @exportMethod print
print.report <- function(x, ...) {
    .print_header(x)
    NextMethod(x)
}

globalVariables(c("buildsrc", "checksrc", "push"))

#' @importFrom dplyr group_by summarize n
#' @exportMethod summary
summary.report <- function(object, ...) {
    object <- group_by(object, buildsrc, checksrc, push)
    .print_header(object)
    summarize(object, n=n())
}

globalVariables(c("commit", "url", "install"))

#' @importFrom lubridate days
#' @importFrom dplyr filter arrange select
#' @importFrom magrittr "%>%"
#' @export
filter_recent <- function(rpt, days_ago = days(1)) {
    filter(rpt, commit > snapshot_date(rpt) - days_ago) %>%
        select(-commit, -url, -install) %>%
        arrange(buildsrc, checksrc, push)
}
