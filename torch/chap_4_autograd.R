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

x1 <- torch_tensor(2, requires_grad = TRUE)
x2 <- torch_tensor(2, requires_grad = TRUE)

x3 <- x1$square()
x5 <- x3 * 0.2

x4 <- x2$square()
x6 <- x4 * 0.2

x7 <- x5 + x6 - 5
x7

x7$requires_grad

x7$backward()

x1$grad
x2$grad
x3$grad


x3 <- x1$square()
x3$retain_grad()

x5 <- x3 * 0.2
x5$retain_grad()

x4 <- x2$square()
x4$retain_grad()

x6 <- x4 * 0.2
x6$retain_grad()

x7 <- x5 + x6 - 5
x7$backward()

x6$grad
x5$grad
x4$grad
x3$grad
x2$grad
x1$grad


x3$grad_fn
x4$grad_fn
x5$grad_fn
x6$grad_fn

# section: ----------------------------------

x <- torch_tensor(1, requires_grad = TRUE)
y <- x^3 + 1
y$backward()
x$grad

x <- torch_tensor(1, requires_grad = TRUE)
y <- torch_exp(x)
y$backward()
x$grad


x <- torch_tensor(2, requires_grad = TRUE)
y <- torch_log(x)
y$backward()
x$grad
