# Installare e caricare i pacchetti necessari
#install.packages("keras")
library(keras)

# Generare dati di esempio con buchi
set.seed(123)
time_series <- sin(1:100) + rnorm(100, 0, 0.1)
time_series[sample(1:100, 20)] <- NA  # Aggiungere buchi nei dati

# Preparare i dati per il modello LSTM
library(zoo)
time_series_filled <- na.approx(time_series)  # Imputazione iniziale con interpolazione lineare
time_series_matrix <- matrix(time_series_filled, ncol = 1)

# Creare sequenze di input e output
create_sequences <- function(data, seq_length) {
  x <- array(NA, dim = c(length(data) - seq_length, seq_length, 1))
  y <- array(NA, dim = c(length(data) - seq_length, 1))
  for (i in 1:(length(data) - seq_length)) {
    x[i,,1] <- data[i:(i + seq_length - 1)]
    y[i,1] <- data[i + seq_length]
  }
  list(x = x, y = y)
}

seq_length <- 10
sequences <- create_sequences(time_series_matrix, seq_length)

# Definire il modello LSTM
model <- keras_model_sequential() %>%
  layer_input(shape = c(seq_length, 1)) %>%
  layer_lstm(units = 50) %>%
  layer_dense(units = 1)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = 'adam'
)

# Addestrare il modello
model %>% fit(
  sequences$x, sequences$y,
  epochs = 50,
  batch_size = 16,
  validation_split = 0.2
)

# Prevedere i valori mancanti
predicted <- model %>% predict(sequences$x)

# Riempire i buchi nei dati originali
time_series_imputed <- time_series
time_series_imputed[is.na(time_series)] <- predicted[is.na(time_series)]

# Visualizzare i risultati
plot(time_series, type = 'l', col = 'red', main = 'Serie Temporale con Buchi Imputati')
lines(time_series_imputed, col = 'blue')
legend('topright', legend = c('Originale', 'Imputato'), col = c('red', 'blue'), lty = 1)