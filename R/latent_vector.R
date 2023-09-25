#'@export
get_condition_latent_vector <- function(patient_counts, control_counts, mod){
  colMeans(get_latent_space(patient_counts, mod)) - colMeans(get_latent_space(control_counts, mod))
}
#'@export
get_latent_space <- function(counts, mod){
  mod$mumodel %>% predict(t(counts))
}
#' @export
get_module_genes <- function(rank_vector, max_rank){
  genenames[rank_vector <= max_rank]
}
