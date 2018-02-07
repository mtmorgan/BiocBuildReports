library(lubridate)
library(tidyverse)
devtools::load_all()

rpt <- report()
summary(rpt)
filter_recent(rpt) %>% print(n=nrow(.))
(versions <- filter(rpt, push %in% "NO"))
save_report(rpt, "./reports")

VIEWS <- "https://bioconductor.org/packages/devel/bioc/VIEWS"
git <- "https://git.bioconductor.org/packages"

versions$git <- sprintf("%s/%s.git", git, versions$package)
versions$local <- vapply(versions$git, function(git) {
    dir <- tempdir()
    destination <- file.path(dir, sub(".git", "", basename(git)))
    if (file.exists(destination)) {
        message("reusing cloned repository for '", basename(destination), "'")
    } else system2("git", c("-C", dir, "clone", "--depth 1", git))
    destination
}, character(1))
versions$repo <- vapply(versions$local, function(repo) {
    description <- file.path(sub(".git$", "", repo), "DESCRIPTION")
    trimws(read.dcf(description, "Version"))
}, character(1))

pkgs <- local({
    views <- tempfile()
    download.file(VIEWS, views)
    tbl <- as.tibble(read.dcf(views, c("Package", "Version", "Maintainer")))
    names(tbl) <- c("package", "published", "maintainer")
    tbl
})
versions <- left_join(versions, pkgs)

versions %>% select(package, published, repo)
noquote(paste(versions$maintainer, collapse=", "))

left_join(filter(rpt, push == "YES"), revdeps) %>%
    mutate(
        commit = NULL, install = NULL, buildsrc = NULL, checksrc = NULL,
        push = NULL, txt = NULL, url = NULL
    ) %>%
    arrange(desc(recursive.required))

left_join(filter(rpt, commit > snapshot_date(rpt) - days(1)), revdeps) %>%
    mutate(
        commit = NULL, install = NULL, buildsrc = NULL, checksrc = NULL,
        push = NULL, txt = NULL, url = NULL
    ) %>%
    arrange(desc(recursive.required))

left_join(
    filter(rpt, commit == "2018-01-26") %>% select(package),
    filter(deps, !dep_is_CRAN) # , kind == "required")
) %>% select(has_rev_dep) %>% filter(!is.na(has_rev_dep)) %>% unique


## packages modified since the release
library(lubridate)
since_release <- filter(rpt, commit > "2017-10-31")

group_by(since_release, buildsrc, checksrc, push) %>% summarize(n = n())

mutate(
    since_release,
    days_ago = days(snapshot_date(rpt) - commit)
) %>% select(package, commit, days_ago)

mutate(since_release, wday = wday(commit, label=TRUE)) %>% group_by(wday) %>%
    summarize(n = n())
