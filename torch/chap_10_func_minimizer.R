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

# these are non neuton approaches for finding the minimum
# start: ----------------------------------

a <- 1
b <- 5

rosenbrock <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  (a - x1)^2 + b * (x2 - x1^2)^2
}

x <- torch_tensor(c(-1, 1), requires_grad = TRUE)

# value <- rosenbrock(x)

# value$backward()

# opt <- optim_lbfgs(x)

# value <- rosenbrock(x)

# opt$zero_grad()
# value$backward()

# opt$step()

# calc_loss <- function() {
#   optimizer$zero_grad()
#   value <- rosenbrock(x_star)
#   value$backward()
#   value
# }

# num_iterations <- 2
# for (i in 1:num_iterations) {
#   optimizer$step(calc_loss)
# }

num_iterations <- 2

x <- torch_tensor(c(-1, 1), requires_grad = TRUE)

optimizer <- optim_lbfgs(x)

calc_loss <- function() {
  optimizer$zero_grad()

  value <- rosenbrock(x)
  cat("Value is: ", as.numeric(value), "\n")

  value$backward()
  value
}

for (i in 1:num_iterations) {
  cat("\nIteration: ", i, "\n")
  optimizer$step(calc_loss)
}

x

# line search: ----------------------------------

num_iterations <- 2

x <- torch_tensor(c(-1, 1), requires_grad = TRUE)

optimizer <- optim_lbfgs(x, line_search_fn = "strong_wolfe")

calc_loss <- function() {
  optimizer$zero_grad()

  value <- rosenbrock(x)
  cat("Value is: ", as.numeric(value), "\n")

  value$backward()
  value
}

for (i in 1:num_iterations) {
  cat("\nIteration: ", i, "\n")
  optimizer$step(calc_loss)
}
