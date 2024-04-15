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

palmerpenguins::penguins |>
  str()

# dataset(): ----------------------------------

penguins_dataset <- dataset(
  name = "penguins_dataset()",
  initialize = function(df) {
    df <- na.omit(df)
    self$x <- as.matrix(df[, 3:6]) |> torch_tensor()
    self$y <- torch_tensor(
      as.numeric(df$species)
    )$to(torch_long())
  },
  .getitem = function(i) {
    list(x = self$x[i, ], y = self$y[i])
  },
  .length = function() {
    dim(self$x)[[1]]
  }
)

ds <- penguins_dataset(palmerpenguins::penguins)

length(ds)
ds[1]
ds[2]
ds[3]

# tensor_dataset: ----------------------------------

three <- tensor_dataset(
  torch_randn(10),
  torch_randn(10),
  torch_randn(10)
)

three[1]
three[2]
three[3]

penguins <- na.omit(palmerpenguins::penguins)
ds <- tensor_dataset(
  torch_tensor(
    as.matrix(penguins[, 3:6])
  ),
  torch_tensor(
    as.numeric(penguins$species),
    dtype = torch_long()
  )
)

ds[1]
ds[2]
ds[3]

# pre defined datasets: ----------------------------------

dir <- "torch-datasets"

ds <- mnist_dataset(
  root = dir,
  train = TRUE, # default
  download = TRUE,
  transform = function(x) {
    x |>
      torch_tensor()
  }
)

first <- ds[1]

cat("Image shape: ", first$x$shape, " Label: ", first$y, "\n")

# dataloaders: ----------------------------------

dl <- dataloader(ds, batch_size = 32, shuffle = TRUE)

length(dl)

dl[1] # doesn't work on dataloader

first_batch <- dl |>
  dataloader_make_iter() |>
  dataloader_next()

dim(first_batch$x)
dim(first_batch$y)

# original image was 28 * 28 in previous datasets you will get 1 * 28 * 28
# while with dataloader you will get batch size * index * data
# which will be like 32 * 1 * 28 * 28
