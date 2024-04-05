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

layer <- nn_linear(in_features = 5, out_features = 16)

layer$weight
layer$bias

x <- torch_randn(50, 5)

output <- x |>
  layer()

output$size()

output$grad_fn

loss <- output$mean()
loss$backward()

layer$weight$grad

# mlp: ----------------------------------

mlp <- nn_sequential(
  nn_linear(10, 32),
  nn_relu(),
  nn_linear(32, 64),
  nn_relu(),
  nn_linear(64, 1)
)

torch_randn(5, 10) |>
  mlp()

# modules: ----------------------------------
my_linear <- nn_module(
  initialize = function(in_features, out_features) {
    self$w <- nn_parameter(
      torch_randn(
        in_features,
        out_features
      )
    )

    self$b <- nn_parameter(torch_zeros(out_features))
  },
  forward = function(input) {
    self$w |>
      input$mm() + self$b
  }
)

layer <- my_linear(7, 1)

layer
