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

# section: ----------------------------------

t1 <- torch_tensor(1:6)

t1
t1$dtype
t1$device
t1$shape

t2 <- t1$to(dtype = torch_int())

t2$dtype
t2$device

t3 <- t1$view(c(2, 3))
t3 <- t1$view(c(3, 2))

torch_tensor(1:6, dtype = torch_float())

torch_tensor(matrix(1:9, ncol = 3, byrow = TRUE))

torch_tensor(array(1:24, dim = c(4, 3, 2)))

array(1:24, dim = c(4, 3, 2))

torch_rand(3, 3) ## uniform distr
torch_randn(3, 3) ## normal distr
torch_zeros(2, 5)
torch_ones(2, 5)
torch_eye(5)
torch_diag(c(1, 2, 3, 4, 5))

# torch with dataset: ----------------------------------
JohnsonJohnson
class(JohnsonJohnson)
plot(JohnsonJohnson)

jjts <- torch_tensor(JohnsonJohnson)

as.numeric(jjts)
unclass(JohnsonJohnson)

all.equal(
  as.numeric(jjts),
  unclass(JohnsonJohnson),
  tolerance = 0.1
)

glimpse(Orange)

torch_tensor(Orange)
factor_typ <- factor(c("a", "b", "c"), ordered = TRUE)
torch_tensor(factor_typ)

Orange_mat <- Orange |>
  mutate(Tree = as.numeric(Tree)) |>
  as.matrix()

Orange_mat |>
  torch_tensor() |>
  print(n = 5)

okc <- fread("data/okc.csv")
okc_text <- fread("data/okc_text.csv")
