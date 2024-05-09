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
library(ggplot2)

# start: ----------------------------------

uci <- "https://archive.ics.uci.edu"
ds_path <- "ml/machine-learning-databases/heart-disease"
ds_file <- "processed.cleveland.data"

# download.file(
#   file.path(uci, ds_path, ds_file),
#   destfile = "torch-datasets/tabular-heart.csv"
# )

heart_df <- fread(
  "torch-datasets/tabular-heart.csv",
  header = FALSE,
  col.names = c(
    "age",
    # 1 = male; 0 = female
    "sex",
    # chest pain type
    # (1 = typical angina, 2 = atypical angina,
    #  3 = non-anginal pain, 4 = asymptomatic)
    "pain_type",
    # in mm Hg on admission
    "resting_blood_pressure",
    # serum cholesterol in mg/dl
    "chol",
    # > 120 mg/dl, true (1) or false (0)
    "fasting_blood_sugar",
    # 0 = normal, 1 = ST-T wave abnormality
    # (T wave inversions and/or ST elevation
    # or depression of > 0.05 mV),
    # 2 = probable or definite left ventricular
    # hypertrophy by Estes' criteria
    "rest_ecg",
    # during exercise
    "max_heart_rate",
    # exercise induced angina (1 = yes, 0 = no),
    "ex_induced_angina",
    # ST depression induced by exercise relative to rest
    "old_peak",
    # slope of the peak exercise ST segment
    # (1 = upsloping, 2 = flat, 3 = downsloping)
    "slope",
    # number of major vessels (0-3) colored by fluoroscopy
    "ca",
    # 3 = normal; 6 = fixed defect; 7 = reversible defect
    "thal",
    # 1-4 = yes; 0 = no
    "heart_disease"
  )
)

which(is.na(heart_df), arr.ind = TRUE)

heart_df |>
  group_by(thal) |>
  summarise(n())

heart_df |>
  group_by(ca) |>
  summarise(n())

nnf_one_hot(
  torch_tensor(
    heart_df$slope,
    dtype = torch_long()
  )
) |>
  print(n = 5)


heart_dataset <- dataset(
  initialize = function(df) {
    self$x_cat <- self$get_categorical(df)
    self$x_num <- self$get_numerical(df)
    self$y <- self$get_target(df)
  },
  .getitem = function(i) {
    x_cat <- self$x_cat[i, ]
    x_num <- self$x_num[i, ]
    y <- self$y[i]
    list(x = list(x_cat, x_num), y = y)
  },
  .length = function() {
    length(self$y)
  },
  get_target = function(df) {
    heart_disease <- fifelse(df$heart_disease > 0, 1, 0)
    heart_disease
  },
  get_numerical = function(df) {
    leave_col <- c(
      "heart_disease",
      "pain_type",
      "rest_ecg",
      "slope",
      "ca",
      "thal"
    )
    df[
      ,
      .SD,
      .SDcols = -leave_col
    ] |>
      mutate(
        across(
          .cols = names(x = setdiff(names(df), leave_col)),
          .fns = scale
        )
      ) |>
      as.matrix()
  },
  get_categorical = function(df) {
    df$ca <- fifelse(is.na(df$ca), 999, df$ca)
    df$thal <- fifelse(is.na(df$thal), 999, df$thal)
    df |>
      dplyr::select(
        pain_type, rest_ecg, slope, ca, thal
      ) |>
      mutate(
        across(
          .cols = c("pain_type", "rest_ecg", "slope", "ca", "thal"),
          .fns = \(x) as.integer(as.factor(x))
        )
      ) |>
      as.matrix()
  }
)

ds <- heart_dataset(heart_df)

ds[1]

train_indices <- sample(
  x = 1:nrow(heart_df),
  size = floor(0.8 * nrow(heart_df))
)

valid_indices <- setdiff(
  x = 1:nrow(heart_df),
  y = train_indices
)

train_ds <- dataset_subset(ds, train_indices)
train_dl <- train_ds |>
  dataloader(
    batch_size = 256,
    shuffle = TRUE
  )

valid_ds <- dataset_subset(ds, valid_indices)
valid_dl <- valid_ds |>
  dataloader(
    batch_size = 256,
    shuffle = FALSE
  )

# embeddings: ----------------------------------

one <- torch_tensor(c(1.555, 0.21, -3.33, 0.0007, 0.07))
two <- torch_tensor(c(0.33, -0.03, -2.177, 1.1, 0.0005))
three <- torch_tensor(c(-0.33, 2.99, 1.77, 1.08, 3.001))

nnf_cosine_similarity(
  torch_ones(2),
  torch_ones(2) * 2.5,
  dim = 1
)

nnf_cosine_similarity(
  torch_ones(2),
  torch_ones(2) * -2.5,
  dim = 1
)

nnf_cosine_similarity(
  torch_tensor(c(1, 0)),
  torch_tensor(c(0, 1)),
  dim = 1
)

nnf_cosine_similarity(one, two, dim = 1)
nnf_cosine_similarity(one, three, dim = 1)
nnf_cosine_similarity(two, three, dim = 1)

module <- nn_embedding(
  num_embeddings = 3,
  embedding_dim = 5
)

module$weight

module(ds[1]$x[[1]][3])

# model Training: ----------------------------------

embedding_module <- nn_module(
  initialize = function(cardinalities, embedding_dim) {
    self$embeddings <- nn_module_list(
      lapply(
        cardinalities,
        function(x) {
          nn_embedding(
            num_embeddings = x, embedding_dim = embedding_dim
          )
        }
      )
    )
  },
  forward = function(x) {
    embedded <- vector(
      mode = "list",
      length = length(self$embeddings)
    )
    for (i in seq_along(self$embeddings)) {
      embedded[[i]] <- self$embeddings[[i]](x[, i])
    }
    torch_cat(embedded, dim = 2)
  }
)

model <- nn_module(
  initialize = function(cardinalities,
                        num_numerical,
                        embedding_dim,
                        fc1_dim,
                        fc2_dim) {
    self$embedder <- embedding_module(
      cardinalities,
      embedding_dim
    )
    self$fc1 <- nn_linear(
      embedding_dim * length(cardinalities) + num_numerical,
      fc1_dim
    )
    self$drop1 <- nn_dropout(p = 0.7)
    self$fc2 <- nn_linear(fc1_dim, fc2_dim)
    self$drop2 <- nn_dropout(p = 0.7)
    self$output <- nn_linear(fc2_dim, 1)
  },
  forward = function(x) {
    embedded <- self$embedder(x[[1]])
    all <- torch_cat(list(embedded, x[[2]]), dim = 2)
    score <- all |>
      self$fc1() |>
      nnf_relu() |>
      self$drop1() |>
      self$fc2() |>
      nnf_relu() |>
      self$drop2() |>
      self$output()

    score[, 1]
  }
)


# cardinalities of categorical features
cardinalities <- heart_df |>
  select(pain_type, rest_ecg, slope, ca, thal) |>
  mutate(
    across(
      .cols = c("pain_type", "rest_ecg", "slope", "ca", "thal"),
      .fns = as.factor
    )
  ) |>
  summarise(across(.fns = nlevels))

# cardinalities of categorical features,
# adjusted for presence of NAs in ca and thal
cardinalities <- cardinalities + c(0, 0, 0, 1, 1)

# number of numerical features
num_numerical <- ncol(heart_df) - length(cardinalities) - 1

embedding_dim <- 7

fc1_dim <- 32
fc2_dim <- 32


fitted <- model |>
  setup(
    optimizer = optim_adam,
    loss = nn_bce_with_logits_loss(),
    metrics = luz_metric_binary_accuracy_with_logits()
  ) |>
  set_hparams(
    cardinalities = cardinalities,
    num_numerical = num_numerical,
    embedding_dim = embedding_dim,
    fc1_dim = fc1_dim, fc2_dim
  ) |>
  set_opt_hparams(lr = 0.001) |>
  fit(train_dl,
    epochs = 200,
    valid_data = valid_dl,
    callbacks = list(
      luz_callback_early_stopping(patience = 10)
    ),
    verbose = TRUE
  )

fitted |>
  plot()

# generated embeddings: ----------------------------------

embedding_weights <- vector(mode = "list")

for (i in seq_along(fitted$model$embedder$embeddings)) {
  embedding_weights[[i]] <- fitted$
    model$
    embedder$
    embeddings[[i]]$
    parameters$
    weight$
    to(device = "cpu")
}

slope_weights <- embedding_weights[[3]]
slope_weights

pca <- prcomp(slope_weights, center = TRUE, scale = TRUE)
pca

(pca$sdev^2 / sum(pca$sdev^2)) |> round(2)

biplot(pca)

pca$x

slopes <- c("up", "flat", "down")

pca$x[, 1:2] |>
  as.data.frame() |>
  mutate(class = slopes) |>
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = class)) +
  coord_cartesian(
    xlim = c(-2.5, 2.5),
    ylim = c(-2.5, 2.5)
  ) +
  theme(aspect.ratio = 1) +
  theme_classic()
