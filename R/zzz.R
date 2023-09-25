.onLoad <- function(libname, pkgname){

  path <- system.file(package = "VAEAnalysis")
  VAE_builder <- reticulate::import_from_path(module = "VAE_builder", path = path)

  path <- system.file(package = "VAEAnalysis")
  utils_VAE <- reticulate::import_from_path(module = "utils_VAE", path = path)

  load_model <<- utils_VAE$load_model

  string_db <<- NULL
}
