#' @export
build_autoencoder <- function(input_size = 48, layer1 = 24, layer2 = 12, layer3 = 24 ){

  encoder_input <- layer_input(shape = c(input_size))
  encoder_output <- encoder_input %>%
    layer_dense(units = layer1, activation = 'relu') %>%
    layer_dense(units = layer2, activation = 'relu')


  encoder <- keras_model(encoder_input, encoder_output)

  decoder_input <- layer_input(shape = c(layer2))
  decoder_output <- decoder_input %>%
    layer_dense(units = layer3, activation = 'relu') %>%
    layer_dense(units = input_size)

  decoder <- keras_model(decoder_input, decoder_output)

  autoencoder_output <- encoder_input %>%
    encoder %>%
    decoder

  autoencoder <- keras_model(encoder_input, autoencoder_output)

  autoencoder %>% compile(
    optimizer = 'adam',
    loss = 'mean_squared_error'
  )
}

#'@export
slice_history <- function(history, n_epochs){

  epoch_length <- history$params$epochs

  history$params$epochs <-  n_epochs

  history$metrics$loss <- history$metrics$loss[(epoch_length - (n_epochs - 1)):epoch_length]
  history$metrics$val_loss <- history$metrics$val_loss[(epoch_length - (n_epochs - 1)):epoch_length]

  history
}
