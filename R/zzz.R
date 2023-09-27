# .onLoad <- function(libname, pkgname){
#
#   encoder_path <- system.file("extdata", "encoder.h5", package = "VAEanalysis")
#   decoder_path <- system.file("extdata", "decoder.h5", package = "VAEanalysis")
#   vae_path <- system.file("extdata", "VAE.h5", package = "VAEanalysis")
#
#   encoder <<- keras::load_model_hdf5(encoder_path)
#   decoder <<- keras::load_model_hdf5(decoder_path)
#   vae <<- keras::load_model_hdf5(encoder_path)
#
#
#
#
# }
