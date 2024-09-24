#' @export
build_autoencoder <- function(input_size = 48, layer1 = 24, layer2 = 12, layer3 = 24) {

  # Encoder
  encoder_input <- layer_input(shape = c(input_size))
  encoder_output <- encoder_input %>%
    layer_dense(units = layer1, activation = 'relu') %>%
    layer_dense(units = layer2, activation = 'relu')

  encoder <- keras_model(inputs = encoder_input, outputs = encoder_output)

  # Decoder
  decoder_input <- layer_input(shape = c(layer2))
  decoder_output <- decoder_input %>%
    layer_dense(units = layer3, activation = 'relu') %>%
    layer_dense(units = input_size)

  decoder <- keras_model(inputs = decoder_input, outputs = decoder_output)

  # Autoencoder: Connect the encoder and decoder
  autoencoder_output <- encoder_output %>%
    decoder

  autoencoder <- keras_model(inputs = encoder_input, outputs = autoencoder_output)
  print(class(autoencoder))  # Should be "keras.engine.functional.Functional" or "keras.models.Model"

  # Compile the autoencoder
  autoencoder %>% compile(
    optimizer = 'adam',
    loss = 'mean_squared_error'
  )

  return(autoencoder)  # Return the compiled autoencoder model
}


#'@export
slice_history <- function(history, n_epochs){

  epoch_length <- history$params$epochs

  history$params$epochs <-  n_epochs

  history$metrics$loss <- history$metrics$loss[(epoch_length - (n_epochs - 1)):epoch_length]
  history$metrics$val_loss <- history$metrics$val_loss[(epoch_length - (n_epochs - 1)):epoch_length]

  history
}


#'@export
extract_mu_log_var <- function(model, input_data) {

  mu_layer <- model$get_layer(name = "mu")
  log_var_layer <- model$get_layer(name = "log_var")

  auxiliary_model <- keras_model(inputs = model$input, outputs = list(mu_layer$output, log_var_layer$output))


  predictions <- auxiliary_model %>% predict(input_data)

  mu_values <- predictions[[1]]
  log_var_values <- predictions[[2]]

  list(mu = colMeans(mu_values), log_var = colMeans(log_var_values))
}


sample_latent_point <- function(mu, log_var, n_samples) {

  sigma <- exp(log_var / 2)


  sampled_points <- matrix(0, n_samples, length(mu))

  for (i in 1:n_samples) {

    epsilon <- rnorm(length(mu))


    sampled_points[i,] <- mu + sigma * epsilon
  }
  sampled_points

}

#'@export
generate_data <- function(vae_decoder, mu, log_var, n_samples) {


  sampled_points <- sample_latent_point(mu, log_var, n_samples)


  generated_data <- vae_decoder %>% predict(sampled_points)

generated_data
}
