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
x <- torch_tensor(seq(0.001, 1.000, 0.001), dtype = torch_float32())
y <- 2 * x + 1

train_sample <- sample(1:1e3, 700)
valid_sample <- setdiff(1:1e3, train_sample)

train_dataset <- tensor_dataset(
  x = x[train_sample],
  y = y[train_sample]
)

valid_dataset <- tensor_dataset(
  x = x[valid_sample],
  y = y[valid_sample]
)

train_dataset[1]
valid_dataset[1]

train_dataloader <- dataloader(train_dataset, batch_size = 1)
valid_dataloader <- dataloader(train_dataset, batch_size = 1)

nnet <- nn_module(
  initialize = function(d_in, d_out) {
    self$nnet <- nn_sequential(
      nn_linear(d_in, d_out)
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
  set_hparams(
    d_in = 1,
    d_out = 1
  ) |>
  fit(
    train_dataloader,
    epochs = 1e3,
    valid_data = valid_dataloader,
    callback = luz_callback_early_stopping(
      patience = 10
    )
  )

fit |>
  predict(
    torch_tensor(0.002)
  )

y[2]
# section: ----------------------------------

convnet <- nn_module(
  "convnet",
  initialize = function() {
    # nn_conv2d(in_channels, out_channels, kernel_size)
    self$conv1 <- nn_conv2d(1, 16, 3)
    self$conv2 <- nn_conv2d(16, 32, 3)
    self$conv3 <- nn_conv2d(32, 64, 3)

    self$output <- nn_linear(2304, 3)
  },
  forward = function(x) {
    x |>
      self$conv1() |>
      nnf_relu() |>
      nnf_max_pool2d(2) |>
      self$conv2() |>
      nnf_relu() |>
      nnf_max_pool2d(2) |>
      self$conv3() |>
      nnf_relu() |>
      nnf_max_pool2d(2) |>
      torch_flatten(start_dim = 2) |>
      self$output()
  }
)

model <- convnet()
img <- torch_randn(1, 1, 64, 64)
model(img)
