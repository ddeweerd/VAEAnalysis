#' @import keras
#' @import dplyr
#' @import magrittr
#' @import tensorflow

#'@export
get_condition_latent_vector <- function(patient_counts, control_counts, encoder){
  colMeans(get_latent_space(patient_counts, mod)) - colMeans(get_latent_space(control_counts, encoder))
}
#'@export
get_latent_space <- function(counts, encoder){
  encoder %>% predict(t(counts))
}
#' @export
get_module_genes <- function(rank_vector, max_rank){
  genenames[rank_vector <= max_rank]
}
