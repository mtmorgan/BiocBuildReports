devtools::load_all()
library(tidyverse)
library(tools)

repos <- c("BioCsoft", "CRAN")
bioc_repos <- BiocInstaller::biocinstallRepos()[["BioCsoft"]]

pkgs <- available.packages(repos=BiocInstaller::biocinstallRepos()[repos])
all <- as.tibble(pkgs)

## clean
flds <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
all[flds] <- lapply(all[flds], cleandeps)

bioc <- filter(all, startsWith(all$Repository, bioc_repos))

deps_all <- pkg_deps(bioc$Package, pkgs, flds)
deps_required <- pkg_deps(bioc$Package, pkgs, head(flds, 3))

deps <- mutate(deps_all, kind = {
    x <- paste(package, has_rev_dep, sep="\r")
    table <- paste(deps_required$package, deps_required$has_rev_dep, sep="\r")
    kind <- ifelse(x %in% table, "required", "all")
    factor(kind, levels=c("all", "required"))
})
saveRDS(deps, file.path("~/a/revdeps", paste0("deps_", Sys.Date(), ".rds")))

## DEMO

## A. Types of reverse depedencies
packages <- "ShortRead"
revdeps <- character()

flds <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
iter <- package_dependencies(packages, pkgs, flds, FALSE, reverse = TRUE)
lengths(iter)[lengths(iter) > 0]
iter <- unlist(iter, use.names=FALSE)

packages <- setdiff(iter, revdeps)
revdeps <- union(revdeps, iter)

## B. How many?

group_by(deps, type, kind) %>% summarize(n = n())

revdeps <- group_by(deps, package, type, kind) %>% summarize(n = n()) %>%
    ungroup() %>%
    mutate(type.kind = paste0(type, ".", kind), type = NULL, kind = NULL) %>%
    spread(type.kind, n, 0L) %>% arrange(desc(recursive.required))
revdeps

## C. revdeps of pushed packages

left_join(filter(rpt, push == "YES"), revdeps) %>%
    select(package, starts_with("direct"), starts_with("recursive")) %>%
    arrange(desc(recursive.required))

## D. commits since release

## packages modified since the release
since_release <- filter(rpt, commit > "2017-10-31")

group_by(since_release, buildsrc, checksrc, push) %>% summarize(n = n())

fig <- ggplot(arrange(since_release, commit), aes(commit, seq_along(commit))) +
    geom_point()

fig <- plotly(arrange(since_release, commit), aes(commit, seq_along(commit))) +
    geom_point()


mutate(
    since_release,
    time_ago = snapshot_date(since_release) - commit
) %>% select(package, commit, time_ago)

mutate(since_release, wday = wday(commit, label=TRUE)) %>% group_by(wday) %>%
    summarize(n = n())
