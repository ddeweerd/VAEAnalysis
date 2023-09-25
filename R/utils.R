#' @import keras
#' @import dplyr
#' @import reticulate
#' @import RSQLite
#' @import magrittr
#' @import tensorflow
#' @import STRINGdb
#' @import igraph

#' @export
load_model <- function(filepath){
  load_model(filepath)
}

#' @export
set_string_db <- function(version = "11.5", score_threshold = 700, network_type = "full", input_directory = ""){
  string_db <<- STRINGdb$new(version = version,
                            species = 9606,
                            score_threshold = score_threshold,
                            input_directory = input_directory)
}

create_empty_ppi_result <- function(){
  list(1, 0, 0) %>%
    set_names(c("enrichment", "edges", "lambda"))
}

#' @export
generate_ppi_enrichment_data <- function(cutoffs, sd_val, mod){

  lsp <- rnorm(n = 64, mean = 0, sd = sd_val)

  decoded_vec <- decode_lv(lsp, 1, mod)

  proteins <-   proteinnames[decoded_vec > cutoffs]
  n_proteins <- length(proteins)

  enri <- get_modularity(proteins) %>%
    unlist ()

  list("lv" = lsp,
       enrichment = enri[2],
       n_proteins = n_proteins)
}
