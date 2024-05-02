# load libraries: ----------------------------------

library(dplyr)
library(data.table)
library(modeldata)
library(torch)
library(torchdatasets)
library(torchvision)
library(torchaudio)
library(topicmodels.etm)
library(innsight)
library(luz)
library(tok)
library(hfhub)

# start: ----------------------------------

model <- model_mobilenet_v2(pretrained = TRUE)
model

model$features
model$features[2]$`0`$conv
model$features[2]$`0`$conv[1]$`0`

# encoder: ----------------------------------
encoder <- nn_module(
  initialize = function() {
    model <- model_mobilenet_v2(pretrained = TRUE)
    self$stages <- nn_module_list(list(
      nn_identity(),
      model$features[1:2],
      model$features[3:4],
      model$features[5:7],
      model$features[8:14],
      model$features[15:18]
    ))
    for (par in self$parameters) {
      par$requires_grad_(FALSE)
    }
  },
  forward = function(x) {
    features <- list()
    for (i in 1:length(self$stages)) {
      x <- self$stages[[i]](x)
      features[[length(features) + 1]] <- x
    }
    features
  }
)

sample <- torch_randn(1, 3, 224, 224)
sample_features <- encoder()(sample)
purrr::map(sample_features, purrr::compose(dim, as.array))

# decoder: ----------------------------------
decoder_block <- nn_module(
  initialize = function(in_channels,
                        skip_channels,
                        out_channels) {
    self$upsample <- nn_conv_transpose2d(
      in_channels = in_channels,
      out_channels = out_channels,
      kernel_size = 2,
      stride = 2
    )
    self$activation <- nn_relu()
    self$conv <- nn_conv2d(
      in_channels = out_channels + skip_channels,
      out_channels = out_channels,
      kernel_size = 3,
      padding = "same"
    )
  },
  forward = function(x, skip) {
    x <- x |>
      self$upsample() |>
      self$activation()
    input <- torch_cat(list(x, skip), dim = 2)
    input |>
      self$conv() |>
      self$activation()
  }
)

# example of upsampling in u-net: ----------------------------------

img <- torch_randn(1, 1, 5, 5)

conv <- nn_conv2d(
  in_channels = 1,
  out_channels = 1,
  kernel_size = 3,
  stride = 2,
  padding = 1
)

convolved <- conv(img)
convolved

transposed_conv <- nn_conv_transpose2d(
  in_channels = 1,
  out_channels = 1,
  kernel_size = 3,
  stride = 2,
  padding = 1
)

upsampled <- transposed_conv(convolved)
upsampled

# example of decoder: ----------------------------------

first_decoder_block <- decoder_block(
  in_channels = 320,
  skip_channels = 96,
  out_channels = 256
)

first_decoder_block(
  sample_features[[6]],
  sample_features[[5]]
) |>
  dim()

# decoder: ----------------------------------

decoder <- nn_module(
  initialize = function(
      decoder_channels = c(256, 128, 64, 32, 16),
      encoder_channels = c(16, 24, 32, 96, 320)) {
    encoder_channels <- rev(encoder_channels)
    skip_channels <- c(encoder_channels[-1], 3)
    in_channels <- c(encoder_channels[1], decoder_channels)

    depth <- length(encoder_channels)

    self$blocks <- nn_module_list()
    for (i in seq_len(depth)) {
      self$blocks$append(decoder_block(
        in_channels = in_channels[i],
        skip_channels = skip_channels[i],
        out_channels = decoder_channels[i]
      ))
    }
  },
  forward = function(features) {
    features <- rev(features)
    x <- features[[1]]
    for (i in seq_along(self$blocks)) {
      x <- self$blocks[[i]](x, features[[i + 1]])
    }
    x
  }
)

# top level module: ----------------------------------
model <- nn_module(
  initialize = \() {
    self$encoder <- encoder()
    self$decoder <- decoder()
    self$output <- nn_conv2d(
      in_channels = 16,
      out_channels = 3,
      kernel_size = 3,
      padding = "same"
    )
  },
  forward = \(x) {
    x |>
      self$encoder() |>
      self$decoder() |>
      self$output()
  }
)

# dogs and cats: ----------------------------------

dir <- "torch-datasets"

ds <- oxford_pet_dataset(
  root = dir,
  download = FALSE
  # download = TRUE
)

pet_dataset <- torch::dataset(
  inherit = oxford_pet_dataset,
  initialize = function(...,
                        size,
                        normalize = TRUE,
                        augmentation = NULL) {
    self$augmentation <- augmentation
    input_transform <- function(x) {
      x <- x %>%
        transform_to_tensor() %>%
        transform_resize(size)
      if (normalize) {
        x <- x %>%
          transform_normalize(
            mean = c(0.485, 0.456, 0.406),
            std = c(0.229, 0.224, 0.225)
          )
      }
      x
    }
    target_transform <- function(x) {
      x <- torch_tensor(x, dtype = torch_long())
      x <- x[newaxis, ..]
      # interpolation = 0 makes sure we
      # still end up with integer classes
      x <- transform_resize(x, size, interpolation = 0)
      x[1, ..]
    }
    super$initialize(
      ...,
      transform = input_transform,
      target_transform = target_transform
    )
  },
  .getitem = function(i) {
    item <- super$.getitem(i)
    if (!is.null(self$augmentation)) {
      self$augmentation(item)
    } else {
      list(x = item$x, y = item$y)
    }
  }
)

augmentation <- function(item) {
  vflip <- runif(1) > 0.5

  x <- item$x
  y <- item$y

  if (vflip) {
    x <- transform_vflip(x)
    y <- transform_vflip(y)
  }

  list(x = x, y = y)
}


angle <- runif(1, -12, 12)
x <- transform_rotate(x, angle)

# same effect as interpolation = 0, above
y <- transform_rotate(y, angle, resample = 0)
