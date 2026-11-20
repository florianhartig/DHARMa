# For each package in Imports of DHARMa/DESCRIPTION: which packages (other than
# base and recommended ones, counting the Import itself) are installed only
# because of this Import? A large count for an Import that DHARMa uses in few
# places is a candidate for removal (e.g. gap, #566).
# Uses the current CRAN versions; run from the repository root.

db <- available.packages(repos = "https://cloud.r-project.org")
imports <- read.dcf("DHARMa/DESCRIPTION", fields = "Imports")
imports <- trimws(sub("\\(.*", "", strsplit(imports, ",")[[1]]))

baseRec <- unlist(tools::standard_package_names())
deps <- function(p) {
  d <- tools::package_dependencies(p, db = db, recursive = TRUE,
                                   which = c("Depends", "Imports", "LinkingTo"))
  setdiff(unique(c(p, unlist(d))), baseRec)
}

onlyVia <- sapply(imports, function(i) {
  setdiff(deps(i), deps(setdiff(imports, i)))
}, simplify = FALSE)

data.frame(import = imports,
           n = lengths(onlyVia),
           onlyVia = sapply(onlyVia, paste, collapse = ", "),
           row.names = NULL)
