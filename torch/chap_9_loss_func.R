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

nnf_mse_loss(torch_ones(2, 2), torch_zeros(2, 2) + 0.1)
loss_func <- nn_mse_loss()
loss_func(torch_ones(2, 2), torch_zeros(2, 2) + 0.1)


target <- torch_tensor(c(1, 0, 0, 1, 1))
unnormalized_estimate <- torch_tensor(c(3, 2.7, -1.2, 7.7, 1.9))
probability_estimate <- nnf_sigmoid(unnormalized_estimate)

nnf_binary_cross_entropy_with_logits(
  unnormalized_estimate,
  target
)

nnf_binary_cross_entropy(
  probability_estimate,
  target
)

# multi class data: ----------------------------------
target <- torch_tensor(
  c(2, 1, 3, 1, 3),
  dtype = torch_long()
)

rbind(
  c(1.2, 7.7, -1),
  c(1.2, -2.1, -1),
  c(0.2, -0.7, 2.5),
  c(0, -0.3, -1),
  c(1.2, 0.1, 3.2)
) |>
  class()

unnormalized_estimate <- rbind(
  c(1.2, 7.7, -1),
  c(1.2, -2.1, -1),
  c(0.2, -0.7, 2.5),
  c(0, -0.3, -1),
  c(1.2, 0.1, 3.2)
) |>
  torch_tensor()

probability_estimate <- nnf_softmax(
  unnormalized_estimate,
  dim = 2
)

logprob_estimate <- nnf_log_softmax(
  unnormalized_estimate,
  dim = 2
)

nnf_cross_entropy(
  unnormalized_estimate,
  target
)

nnf_nll_loss(
  logprob_estimate,
  target
)

# binary multi class: ----------------------------------

target <- torch_tensor(c(1, 0, 0, 1, 1))
unnormalized_estimate <- torch_tensor(c(3, 2.7, -1.2, 7.7, 1.9))
probability_estimate <- nnf_sigmoid(unnormalized_estimate)
nnf_binary_cross_entropy(probability_estimate, target)

empty_matrix <- matrix(rep(0, 10), ncol = 2)
for (i in 1:length(probability_estimate)) {
  value <- probability_estimate[i] |>
    as.numeric()

  curr_value <- c(1 - value, value)

  empty_matrix[i, ] <- curr_value
}

# logits
multiclass_probability <- empty_matrix |>
  torch_tensor()

target <- target + 1

nnf_nll_loss(
  torch_log(multiclass_probability),
  target$to(dtype = torch_long())
)
