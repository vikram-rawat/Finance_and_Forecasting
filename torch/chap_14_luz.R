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

# input dimensionality (number of input features)
d_in <- 3
# number of observations in training set
n <- 1000

x <- torch_randn(n, d_in)
coefs <- c(0.2, -1.3, -0.5)
y <- x$matmul(coefs)$unsqueeze(2) + torch_randn(n, 1)

ds <- tensor_dataset(x, y)

dl <- dataloader(ds, batch_size = 100, shuffle = TRUE)

d_hidden <- 32
d_out <- 1
net <- nn_module(
  initialize = function(d_in, d_hidden, d_out) {
    self$net <- nn_sequential(
      nn_linear(d_in, d_hidden),
      nn_relu(),
      nn_linear(d_hidden, d_out)
    )
  },
  forward = function(x) {
    self$net(x)
  }
)

# use luz: ----------------------------------
fitted <- net |>
  setup(
    loss = nn_mse_loss(),
    optimizer = optim_adam
  ) |>
  set_hparams(
    d_in = d_in,
    d_out = d_out,
    d_hidden = d_hidden
  ) |>
  fit(
    dl, # you can use a dataloader
    epochs = 200
  )

fitted <- net |>
  setup(
    loss = nn_mse_loss(),
    optimizer = optim_adam
  ) |>
  set_hparams(
    d_in = d_in,
    d_hidden = d_hidden,
    d_out = d_out
  ) |>
  fit(
    ds, # or a dataset
    epochs = 200
  )

fitted <- net |>
  setup(
    loss = nn_mse_loss(),
    optimizer = optim_adam
  ) |>
  set_hparams(
    d_in = d_in,
    d_hidden = d_hidden,
    d_out = d_out
  ) |>
  fit(
    list(x, y), # raw torch tensors
    epochs = 200
  )


fitted <- net |>
  setup(
    loss = nn_mse_loss(),
    optimizer = optim_adam
  ) |>
  set_hparams(
    d_in = d_in,
    d_hidden = d_hidden,
    d_out = d_out
  ) |>
  fit(
    list( # numeric R objects that can be converted into torch tensors
      as.matrix(x),
      as.matrix(y)
    ),
    epochs = 200
  )

# train test split: ----------------------------------
train_ids <- sample(
  1:length(ds),
  size = 0.6 * length(ds)
)

valid_ids <- sample(
  setdiff(1:length(ds), train_ids),
  size = 0.2 * length(ds)
)

test_ids <- setdiff(
  1:length(ds),
  union(train_ids, valid_ids)
)

train_ds <- dataset_subset(ds, indices = train_ids)
valid_ds <- dataset_subset(ds, indices = valid_ids)
test_ds <- dataset_subset(ds, indices = test_ids)

train_dl <- dataloader(
  train_ds,
  batch_size = 100,
  shuffle = TRUE
)
valid_dl <- dataloader(valid_ds, batch_size = 100)
test_dl <- dataloader(test_ds, batch_size = 100)

fitted <- net |>
  setup(
    loss = nn_mse_loss(),
    optimizer = optim_adam,
    metrics = list(
      luz_metric_mae()
    )
  ) |>
  set_hparams(
    d_in = d_in,
    d_out = d_out,
    d_hidden = d_hidden
  ) |>
  fit(
    train_dl,
    epochs = 200,
    valid_data = valid_dl
  )

fitted |>
  predict(test_dl)

fitted |>
  evaluate(test_dl)

# callback hooks from luz: ----------------------------------

fitted <- net |>
  setup(
    loss = nn_mse_loss(),
    optimizer = optim_adam,
    metrics = list(luz_metric_mae())
  ) |>
  set_hparams(
    d_in = d_in,
    d_hidden = d_hidden,
    d_out = d_out
  ) |>
  fit(
    train_dl,
    epochs = 200,
    valid_data = valid_dl,
    callbacks = list(
      luz_callback_model_checkpoint(
        path = "torch-datasets/models/",
        save_best_only = TRUE
      ),
      luz_callback_early_stopping(
        patience = 10
      )
    )
  )

fitted |>
  predict(test_dl)

# full luz model: ----------------------------------

# input dimensionality (number of input features)
d_in <- 3
# number of observations in training set
n <- 1000

x <- torch_randn(n, d_in)
coefs <- c(0.2, -1.3, -0.5)
y <- x$matmul(coefs)$unsqueeze(2) + torch_randn(n, 1)

ds <- tensor_dataset(x, y)

dl <- dataloader(ds, batch_size = 100, shuffle = TRUE)

train_ids <- sample(seq_along(ds), size = 0.6 * length(ds))
valid_ids <- sample(setdiff(
  seq_along(ds),
  train_ids
), size = 0.2 * length(ds))
test_ids <- setdiff(seq_along(ds), union(train_ids, valid_ids))

train_ds <- dataset_subset(ds, indices = train_ids)
valid_ds <- dataset_subset(ds, indices = valid_ids)
test_ds <- dataset_subset(ds, indices = test_ids)

train_dl <- dataloader(train_ds,
  batch_size = 100,
  shuffle = TRUE
)
valid_dl <- dataloader(valid_ds, batch_size = 100)
test_dl <- dataloader(test_ds, batch_size = 100)

# dimensionality of hidden layer
d_hidden <- 32
# output dimensionality (number of predicted features)
d_out <- 1

net <- nn_module(
  initialize = function(d_in, d_hidden, d_out) {
    self$net <- nn_sequential(
      nn_linear(d_in, d_hidden),
      nn_relu(),
      nn_linear(d_hidden, d_out)
    )
  },
  forward = function(x) {
    self$net(x)
  }
)

fitted <- net |>
  setup(
    loss = nn_mse_loss(),
    optimizer = optim_adam
  ) |>
  set_hparams(
    d_in = d_in,
    d_hidden = d_hidden,
    d_out = d_out
  ) |>
  fit(
    train_dl,
    epochs = 200,
    valid_data = valid_dl
  )

# without luz: ----------------------------------
device <- torch_device(
  if (cuda_is_available()) {
    "cuda"
  } else {
    "cpu"
  }
)

model <- net(d_in = d_in, d_hidden = d_hidden, d_out = d_out)
model <- model$to(device = device)
optimizer <- optim_adam(model$parameters)

train_batch <- function(b) {
  optimizer$zero_grad()
  output <- model(b[[1]]$to(device = device))
  target <- b[[2]]$to(device = device)

  loss <- nnf_mse_loss(output, target)
  loss$backward()
  optimizer$step()

  loss$item()
}

valid_batch <- function(b) {
  output <- model(b[[1]]$to(device = device))
  target <- b[[2]]$to(device = device)

  loss <- nnf_mse_loss(output, target)
  loss$item()
}

num_epochs <- 200

for (epoch in 1:num_epochs) {
  model$train()
  train_loss <- c()

  # use coro::loop() for stability and performance
  coro::loop(for (b in train_dl) {
    loss <- train_batch(b)
    train_loss <- c(train_loss, loss)
  })

  cat(sprintf(
    "\nEpoch %d, training: loss: %3.5f \n",
    epoch, mean(train_loss)
  ))

  model$eval()
  valid_loss <- c()

  # disable gradient tracking to reduce memory usage
  with_no_grad({
    coro::loop(for (b in valid_dl) {
      loss <- valid_batch(b)
      valid_loss <- c(valid_loss, loss)
    })
  })

  cat(sprintf(
    "\nEpoch %d, validation: loss: %3.5f \n",
    epoch, mean(valid_loss)
  ))
}
