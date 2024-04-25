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

convnet <- nn_module(
  "convnet",
  initialize = function() {
    # nn_conv2d(in_channels, out_channels, kernel_size, stride)
    self$conv1 <- nn_conv2d(1, 32, 3, 1)
    self$conv2 <- nn_conv2d(32, 64, 3, 2)
    self$conv3 <- nn_conv2d(64, 128, 3, 1)
    self$conv4 <- nn_conv2d(128, 256, 3, 2)
    self$conv5 <- nn_conv2d(256, 10, 3, 2)

    self$bn1 <- nn_batch_norm2d(32)
    self$bn2 <- nn_batch_norm2d(64)
    self$bn3 <- nn_batch_norm2d(128)
    self$bn4 <- nn_batch_norm2d(256)
  },
  forward = function(x) {
    x |>
      self$conv1() |>
      nn_relu() |>
      self$bn1() |>
      self$conv2() |>
      nn_relu() |>
      self$bn2() |>
      self$conv3() |>
      nn_relu() |>
      self$bn3() |>
      self$conv4() |>
      nn_relu() |>
      self$bn4() |>
      self$conv5() |>
      torch_squeeze()
  }
)

# learning rate finder: ----------------------------------

dir <- "torch-datasets"

train_ds <- mnist_dataset(
  dir,
  download = FALSE,
  transform = \(x) {
    x |>
      torchvision::transform_to_tensor() |>
      transform_random_affine(
        degrees = c(-45, 45), translate = c(0.1, 0.1)
      )
  }
)

train_dl <- dataloader(
  train_ds,
  batch_size = 128,
  shuffle = TRUE
)

valid_ds <- mnist_dataset(
  dir,
  train = FALSE,
  transform = torchvision::transform_to_tensor
)

valid_dl <- dataloader(
  valid_ds,
  batch_size = 128
)

convnet <- nn_module(
  "convnet",
  initialize = function() {
    # nn_conv2d(in_channels, out_channels, kernel_size, stride)
    self$conv1 <- nn_conv2d(1, 32, 3, 1)
    self$conv2 <- nn_conv2d(32, 64, 3, 2)
    self$conv3 <- nn_conv2d(64, 128, 3, 1)
    self$conv4 <- nn_conv2d(128, 256, 3, 2)
    self$conv5 <- nn_conv2d(256, 10, 3, 2)

    self$bn1 <- nn_batch_norm2d(32)
    self$bn2 <- nn_batch_norm2d(64)
    self$bn3 <- nn_batch_norm2d(128)
    self$bn4 <- nn_batch_norm2d(256)
  },
  forward = function(x) {
    x |>
      self$conv1() |>
      nnf_relu() |>
      self$bn1() |>
      self$conv2() |>
      nnf_relu() |>
      self$bn2() |>
      self$conv3() |>
      nnf_relu() |>
      self$bn3() |>
      self$conv4() |>
      nnf_relu() |>
      self$bn4() |>
      self$conv5() |>
      torch_squeeze()
  }
)

model <- convnet |>
  setup(
    loss = nn_cross_entropy_loss(),
    optimizer = optim_adam,
    metrics = list(luz_metric_accuracy())
  )

rates_and_losses <- model |>
  lr_finder(
    train_dl,
    start_lr = 0.0001,
    end_lr = 0.3
  )

rates_and_losses |>
  plot()

num_epochs <- 5

# learning rate scheduler: ----------------------------------

num_epochs <- 5

# the model has already been setup(), we continue from there
fitted <- model |>
  fit(train_dl,
    epochs = num_epochs,
    valid_data = valid_dl,
    callbacks = list(
      luz_callback_lr_scheduler(
        lr_one_cycle,
        max_lr = 0.01,
        epochs = num_epochs,
        steps_per_epoch = length(train_dl),
        call_on = "on_batch_end"
      )
    )
  )

# transfer learning: ----------------------------------

resnet <- model_resnet18(pretrained = TRUE)

resnet$conv1
resnet$layer1
resnet$layer4
resnet$fc

resnet$fc <- resnet$fc$in_features |>
  nn_linear(10)

resnet$fc

convnet <- nn_module(
  initialize = function() {
    self$model <- model_resnet18(pretrained = TRUE)
    for (par in self$parameters) {
      par$requires_grad_(FALSE)
    }
    self$model$fc <- nn_linear(self$model$fc$in_features, 10)
  },
  forward = function(x) {
    self$model(x)
  }
)


train_ds <- mnist_dataset(
  dir,
  download = TRUE,
  transform = \(x) {
    expand <- function(x) {
      x$expand(c(3, 28, 28))
    }
    x |>
      torchvision::transform_to_tensor() |>
      expand() |>
      transform_random_affine(
        degrees = c(-45, 45), translate = c(0.1, 0.1)
      )
  }
)

train_dl <- dataloader(
  train_ds,
  batch_size = 128,
  shuffle = TRUE
)

valid_ds <- mnist_dataset(
  dir,
  train = FALSE,
  transform = \(x){
    expand <- function(x) {
      x$expand(c(3, 28, 28))
    }
    x |>
      torchvision::transform_to_tensor() |>
      expand()
  }
)

valid_dl <- dataloader(valid_ds, batch_size = 128)

model <- convnet |>
  setup(
    loss = nn_cross_entropy_loss(),
    optimizer = optim_adam,
    metrics = list(luz_metric_accuracy())
  ) |>
  fit(
    train_dl,
    epochs = 5,
    valid_data = valid_dl
  )
