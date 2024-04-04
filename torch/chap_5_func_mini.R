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

a <- 1
b <- 5

rosenbrock <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  (a - x1)^2 + b * (x2 - x1^2)^2
} # minima of this function should be 1,1

rosenbrock(5:6)

# lr <- 0.01
# num_iterations <- 1000
# x <- torch_tensor(c(-1, 1), requires_grad = TRUE)
# attention: this is not the correct procedure yet!
# for (i in 1:num_iterations) {
#   # call function, passing in current parameter value
#   value <- rosenbrock(x)

#   # compute gradient of value w.r.t. parameter
#   value$backward()

#   # manually update parameter, subtracting a fraction
#   # of the gradient
#   # this is not quite correct yet!
#   x$sub_(lr * x$grad)
# }


num_iterations <- 1e4
lr <- 0.01
x <- torch_tensor(c(-1, 1), requires_grad = TRUE)

for (i in 1:num_iterations) {
  if (i %% 100 == 0) cat("Iteration: ", i, "\n")

  value <- rosenbrock(x)
  if (i %% 100 == 0) {
    cat("Value is: ", as.numeric(value), "\n")
  }

  value$backward()
  if (i %% 100 == 0) {
    cat("Gradient is: ", as.matrix(x$grad), "\n")
  }

  with_no_grad({
    x$sub_(lr * x$grad)
    x$grad$zero_()
  })
}

x
