# load libraries: ----------------------------------

library(dplyr)
library(data.table)
library(modeldata)
library(torch)
library(torchvision)
library(torchaudio)
library(topicmodels.etm)
library(innsight)
library(luz)
library(tok)
library(hfhub)

# start: ----------------------------------

# x <- torch_tensor(cars$speed)
# y <- torch_tensor(cars$dist)
x <- torch_tensor(seq(0.001, 0.900, 0.001))
y <- 2 * x + 1

train_dataset <- tensor_dataset(
  x,
  y
)

nnet <- nn_module(
  initialize = function() {
    self$nnet <- nn_sequential(
      nn_linear(1, 1)
    )
  },
  forward = function(x) {
    x |>
      self$nnet()
  }
)

fit <- nnet |>
  setup(
    loss = nn_mse_loss(),
    optimizer = optim_sgd
  ) |>
  set_opt_hparams(
    lr = 0.01
  ) |>
  set_hparams() |>
  fit(
    train_dataset,
    epochs = 1e3,
    valid_data = y
  )

fit |>
  predict(
    torch_tensor(10)
  )
