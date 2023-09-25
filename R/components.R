#' @export
get_components <- function(genes){

  if(is.null(string_db)){
    stop("string_db has not been set")
  }

  subnet <- string_db$get_subnetwork(proteinnames[genenames %in% genes])
  compon <- igraph::components(subnet)

  comp1 <- names(which(compon[[1]] == which.max(compon$csize)))
  genenames[proteinnames %in% comp1]

}

get_subnetwork <- function(genes){
  string_db$get_subnetwork(proteinnames[genenames %in% genes])
}

#' @export
get_components_proteins <- function(proteins){

  if(is.null(string_db)){
    stop("string_db has not been set")
  }

  subnet <- string_db$get_subnetwork(proteins)
  compon <- igraph::components(subnet)

  comp1 <- names(which(compon[[1]] == which.max(compon$csize)))
  genenames[proteinnames %in% comp1]
}

#'@export
get_modularity <- function(proteins){
  if(purrr::is_null(proteins)){
    create_empty_ppi_result()
  }
  if(length(proteins) == 0){
    create_empty_ppi_result()
  }else{
    string_db$ppi_enrichment(proteins)
  }
}
