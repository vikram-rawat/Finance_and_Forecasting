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

dir <- "/torch-datasets"

valid_ds <- mnist_dataset(
  dir,
  download = TRUE,
  train = FALSE,
  transform = transform_to_tensor
)

valid_dl <- dataloader(valid_ds, batch_size = 128)

# a convenient way to obtain individual images without
# manual iteration
test_images <- coro::collect(
  valid_dl, 1
)[[1]]$x[1:32, 1, , ] |>
  as.array()


par(mfrow = c(4, 8), mar = rep(0, 4), mai = rep(0, 4))

test_images |>
  purrr::array_tree(1) |>
  purrr::map(as.raster) |>
  purrr::iwalk(~ {
    plot(.x)
  })

# transform or augment images: ----------------------------------
train_ds <- mnist_dataset(
  dir,
  download = TRUE,
  transform = \(x) {
    x |>
      torchvision::transform_to_tensor() |>
      # flip horizontally, with a probability of 0.5
      torchvision::transform_random_horizontal_flip(p = 0.5) |>
      # flip vertically, with a probability of 0.5
      torchvision::transform_random_vertical_flip(p = 0.5) |>
      # (1) rotate to the left or the right,
      #     up to respective angles of 45 degrees
      # (2) translate vertically or horizontally,
      #     not exceeding 10% of total image width/height
      torchvision::transform_random_affine(
        degrees = c(-45, 45),
        translate = c(0.1, 0.1)
      )
  }
)

train_dl <- dataloader(
  train_ds,
  batch_size = 128,
  shuffle = TRUE
)

train_images <- coro::collect(
  train_dl, 1
)[[1]]$x[1:32, 1, , ] |>
  as.array()

par(mfrow = c(4, 8), mar = rep(0, 4), mai = rep(0, 4))
train_images |>
  purrr::array_tree(1) |>
  purrr::map(as.raster) |>
  purrr::iwalk(~ {
    plot(.x)
  })

# tensor_image_display(train_ds[1]$x)
# tensor_image_display(train_images)
# tensor_image_browse(train_ds[1]$x)
# tensor_image_browse(train_images)

# less transformation: ----------------------------------
train_ds <- mnist_dataset(
  dir,
  download = FALSE,
  transform = \(x) {
    x |>
      torchvision::transform_to_tensor() |>
      torchvision::transform_random_affine(
        degrees = c(-45, 45), translate = c(0.1, 0.1)
      )
  }
)

train_dl <- dataloader(train_ds,
  batch_size = 128,
  shuffle = TRUE
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
  },
  forward = function(x) {
    x |>
      self$conv1() |>
      nnf_relu() |>
      self$conv2() |>
      nnf_relu() |>
      self$conv3() |>
      nnf_relu() |>
      self$conv4() |>
      nnf_relu() |>
      self$conv5() |>
      torch_squeeze()
  }
)

fitted <- convnet |>
  setup(
    loss = nn_cross_entropy_loss(),
    optimizer = optim_adam,
    metrics = list(
      luz_metric_accuracy()
    )
  ) |>
  fit(
    train_dl,
    epochs = 5,
    valid_data = valid_dl
  )

fitted

# mixup: ----------------------------------

first_batch <- valid_dl |>
  coro::collect(1) |>
  _[[1]]

mixed <- nnf_mixup(
  x = first_batch$x,
  y = first_batch$y,
  weight = torch_tensor(
    rep(0.9, 128)
  )
)

mixed <- nnf_mixup(
  x = first_batch$x,
  y = first_batch$y,
  weight = torch_tensor(rep(0.7, 128))
)

mixed <- nnf_mixup(
  x = first_batch$x,
  y = first_batch$y,
  weight = torch_tensor(rep(0.5, 128))
)

# redefine the training set to not use augmentation
train_ds <- mnist_dataset(
  dir,
  download = TRUE,
  transform = transform_to_tensor
)

train_dl <- dataloader(train_ds,
  batch_size = 128,
  shuffle = TRUE
)

fitted <- convnet |>
  setup(
    loss = nn_mixup_loss(torch::nn_cross_entropy_loss()),
    optimizer = optim_adam
  ) |>
  fit(
    train_dl,
    epochs = 5,
    valid_data = valid_dl,
    callbacks = list(luz_callback_mixup())
  )

# dropout: ----------------------------------

convnet <- nn_module(
  "convnet",
  initialize = function() {
    # nn_conv2d(in_channels, out_channels, kernel_size, stride)
    self$conv1 <- nn_conv2d(1, 32, 3, 1)
    self$conv2 <- nn_conv2d(32, 64, 3, 2)
    self$conv3 <- nn_conv2d(64, 128, 3, 1)
    self$conv4 <- nn_conv2d(128, 256, 3, 2)
    self$conv5 <- nn_conv2d(256, 10, 3, 2)

    self$drop1 <- nn_dropout(p = 0.2)
    self$drop2 <- nn_dropout(p = 0.2)
  },
  forward = function(x) {
    x |>
      self$conv1() |>
      nnf_relu() |>
      self$conv2() |>
      nnf_relu() |>
      self$drop1() |>
      self$conv3() |>
      nnf_relu() |>
      self$conv4() |>
      nnf_relu() |>
      self$drop2() |>
      self$conv5() |>
      torch_squeeze()
  }
)

# regularization: ----------------------------------
# regularization in DL is nothing but weight decay
fitted <- convnet |>
  setup(
    loss = nn_cross_entropy_loss(),
    optimizer = optim_adam,
    metrics = list(luz_metric_accuracy())
  ) |>
  set_opt_hparams(
    weight_decay = 0.00001
  ) |>
  fit(
    train_dl,
    epochs = 5,
    valid_data = valid_dl
  )

# early stopping: ----------------------------------

fitted <- convnet |>
  setup(
    loss = nn_cross_entropy_loss(),
    optimizer = optim_adam,
    metrics = list(luz_metric_accuracy())
  ) |>
  fit(train_dl,
    epochs = 5,
    valid_data = valid_dl,
    callbacks = list(
      luz_callback_early_stopping()
    )
  )
