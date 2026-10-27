root <- normalizePath(file.path(getwd(),"../.."))
source(file.path(root,"R/bootstrap.R")); tm_load(root)
fixture <- function() readRDS(file.path(root,"tests/fixtures/sim-aggregates.rds"))
