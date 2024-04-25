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
# okc_text <- fread("data/okc_text.csv")

okc

okc$date |>
  torch_tensor() |>
  print(n = 7)

okc$location |>
  torch_tensor() |>
  print(n = 7)

okc$location |>
  factor() |>
  as.numeric() |>
  torch_tensor() |>
  print(n = 7)

c(1:3, NA, 5) |>
  torch_tensor()

torch_nanquantile(torch_tensor(c(1, NA, 3)), q = 0.5)

# opetions on tensors: ----------------------------------

t1 <- torch_tensor(1:5)
t2 <- torch_tensor(2:6)

all.equal(
  t1 + t2,
  torch_add(t1, t2)
)

all.equal(
  torch_add(t1, t2),
  t1$add(t2)
)

t1$add_(t2)


t1 <- torch_tensor(1:3)
t2 <- torch_tensor(4:6)
t1$dot(t2)

t1$t()$dot(t2)

t3 <- torch_tensor(matrix(1:12, ncol = 3, byrow = TRUE))
t3$matmul(t1)

torch_multiply(t3, t1)

# summary: ----------------------------------
m <- outer(1:3, 1:6)
# 1:10 %o% 1:10
sum(m)
apply(m, 1, sum)
apply(m, 2, sum)

t <- torch_outer(
  torch_tensor(1:3),
  torch_tensor(1:6)
)
t$sum()
t$sum(dim = 1)
t$sum(dim = 2)

t <- torch_randn(4, 3, 2)
t

t$sum(dim = c(1, 2))
t$sum(dim = 2)

t <- torch_tensor(matrix(1:9, ncol = 3, byrow = TRUE))
t[1, ]
t[1, , drop = FALSE]

t <- torch_rand(3, 3, 3)
t[1:2, 2:3, c(1, 3)]

t <- torch_tensor(matrix(1:4, ncol = 2, byrow = TRUE))
t[-1, -1]

t <- torch_tensor(matrix(1:20, ncol = 10, byrow = TRUE))
t[, 1:8:2]

t[1, ..]


t1 <- torch_randn(2, 2)
t2 <- torch_randn(2, 2, 2)
t3 <- torch_randn(2, 2, 2, 2)
t1[1, ..]
t2[1, ..]
t3[1, ..]
t3[1, .., 2]

# reshaping: ----------------------------------

t <- torch_zeros(24)
print(t, n = 3)

t2 <- t$view(c(2, 12))
t2

t$storage()$data_ptr()
t2$storage()$data_ptr()

t <- torch_tensor(matrix(1:15, nrow = 3, byrow = TRUE))
t

t$stride()

t2 <- t$view(c(5, 3))
t2

t2$stride()

t3 <- t$t()
t3
t$storage()$data_ptr()
t3$storage()$data_ptr()

t3$stride()

t <- torch_randn(3)
t

t$unsqueeze(1)

t <- torch_randn(3, 3)
t2 <- t$t()
t2$view(9)


t <- torch_randn(3, 3)
t2 <- t$t()$reshape(9)

t$storage()$data_ptr()
t2$storage()$data_ptr()

# broadcasting: ----------------------------------

t1 <- torch_randn(3, 5)
t1 * 0.5

m <- matrix(1:15, ncol = 5, byrow = TRUE)
m2 <- matrix(1:5, ncol = 5, byrow = TRUE)

m + m2

m3 <- 1:5

m + m3


t <- torch_tensor(m)
t2 <- torch_tensor(m2)

t$shape
t2$shape

t$add(t2)

t3 <- torch_tensor(m3)

t3$shape

t$add(t3)
