#' Latent vector
#' @export
get_gene_ranks <- function(latent_vector, boost_factor, decoder, n_comparison){
  decoded_vector <- decode_lv(latent_vector, boost_factor, decoder)

  random_preds <- get_random_profiles(n_comparison, decoder)

  sapply(1:length(decoded_vector), check_gene_rank, decoded_vector, random_preds) %>%
    apply(., 2, function(x) which(x == 1)) %>%
    magrittr::set_names(genenames)
}
#' @export
decode_lv <- function(latent_vector, boost_factor, decoder){
  #Sets the correct data type (matrix) and amplifies signal
  latent_vector <- matrix(latent_vector*(boost_factor), 1)

  decoded_signal <- decoder %>% predict(latent_vector)
  decoded_signal[1,]
}

#' @export
get_random_profiles <- function(n_comparison, decoder){
  random_lsp <- matrix(rnorm(64 * n_comparison), n_comparison)
  decoder %>% predict(random_lsp) %>%
    t()
}

check_gene_rank <- function(inde, decoded_vector, random_preds){
  order(c(decoded_vector[inde], random_preds[inde, ]), decreasing = T)
}
#' @export
cosine_distance <- function(lv1, lv2){
  1 - sum(lv1 * lv2) / (sqrt(sum(lv1^2)) * sqrt(sum(lv2^2)))
}

#' @export
euclidian_distance <- function(lv1, lv2){
  sqrt(sum((lv1 - lv2)^2))
}

#' @export
get_d_bar_cosine <- function(patient_counts, control_counts, encoder){
  apply(get_latent_space(patient_counts, encoder), 1, function(y)
    apply(get_latent_space(control_counts, encoder), 1, function(x) cosine_distance(x,y))) %>%
    mean()
}
#' @export
get_d_bar_control_cosine <- function(control_counts, encoder){
  lsp <- get_latent_space(control_counts, encoder)
  dist_mat <- sapply(X = 1:nrow(lsp), FUN = function(y) sapply(X = 1:nrow(lsp), FUN = function(x)cosine_distance(lsp[x, ], lsp[y, ])))
  mean(dist_mat[lower.tri(dist_mat)])
}

#' @export
get_d_bar_euclid <- function(patient_counts, control_counts, encoder){
  apply(get_latent_space(patient_counts, encoder), 1, function(y)
    apply(get_latent_space(control_counts, encoder), 1, function(x) euclidian_distance(x,y))) %>%
    mean()
}
#' @export
get_d_bar_control_euclid <- function(control_counts, encoder){
  lsp <- get_latent_space(control_counts, encoder)
  dist_mat <- sapply(X = 1:nrow(lsp), FUN = function(y) sapply(X = 1:nrow(lsp), FUN = function(x)euclidian_distance(lsp[x, ], lsp[y, ])))
  mean(dist_mat[lower.tri(dist_mat)])
}
