library(here)
library(readr)
library(tibble)
library(dplyr)

NFL_norm <- as.matrix(read_csv(here("datasets", "NFLNorm.csv")))
MLB_norm <- as.matrix(read_csv(here("datasets", "MLBNorm.csv")))
NHL_norm <- as.matrix(read_csv(here("datasets", "NHLNorm.csv")))
NBA_norm <- as.matrix(read_csv(here("datasets", "NBANorm.csv")))

NFL_norm <- apply(NFL_norm, 2, as.numeric)
MLB_norm <- apply(MLB_norm, 2, as.numeric)
NHL_norm <- apply(NHL_norm, 2, as.numeric)
NBA_norm <- apply(NBA_norm, 2, as.numeric)

make_absorbing_4_to_6 <- function(P) {
  # Check that P is a square matrix
  if (!is.matrix(P) || nrow(P) != ncol(P)) {
    stop("Input must be a square matrix")
  }
  
  n <- nrow(P)
  
  # Define absorbing states (states 4 to 6, or up to n if n < 6)
  absorbing_states <- intersect(4:6, 1:n)
  
  # For each absorbing state, set its row to all zeros and put 1 on the diagonal
  for (s in absorbing_states) {
    P[s, ] <- 0
    P[s, s] <- 1
  }
  
  return(P)
}


orig_matrices <- list(NFL_norm, MLB_norm, NHL_norm, NBA_norm)

new_matrices_func <- function(matrices) {
  new_matrices <- list()
  for (matrix in matrices) {
    print(dim(matrix))
    result <- make_absorbing_4_to_6(matrix)
    new_matrices[[length(new_matrices) + 1]] <- result
  }
  return(new_matrices)
}

end_matrices <- new_matrices_func(orig_matrices)

to_power <- function(matrices) {
  vals <- list()
  for (matrix in matrices) {
    matrix_k = matrix
    for (i in 1:6){
      matrix_k = matrix_k %*% matrix
    }
    vals <- c(vals, sum(matrix_k[1, 4:6]))
  }
  return(vals)
}

after_6_years = to_power(end_matrices)

library(expm)
library(ggplot2)
library(tidyr)
library(dplyr)


compute_row_sums <- function(mat) {
  sapply(1:10, function(x) {
    powered <- mat %^% x
    sum(powered[1, 4:6])
  })
}

# Compute row sums for each matrix
row_sums_list <- lapply(end_matrices, compute_row_sums)

# Combine into a data frame
df <- data.frame(
  Power = 1:10,
  NFL = row_sums_list[[1]],
  MLB = row_sums_list[[2]],
  NHL = row_sums_list[[3]],
  NBA = row_sums_list[[4]]
)

# Reshape to long format for ggplot
df_long <- pivot_longer(df, cols = -Power, names_to = "Matrix", values_to = "RowSum")
# Reorder factor levels so legend shows NFL first
df_long$Matrix <- factor(df_long$Matrix, levels = c("NFL", "MLB", "NHL", "NBA"))

# Plot
comp_plot <- ggplot(df_long, aes(x = Power, y = RowSum, color = Matrix)) +
  geom_line(size = 1.2) +
  geom_point() +
  theme_minimal() +
  labs(title = "% Chance of Going from Bottom Tier to Having an Above Average Team within X Years", x = "Years", y = "% Chance")

ggsave(here("plots", "comparison_plot.png"), 
       plot=comp_plot,
       bg = "white",
       width = 10,
       height = 6,)

df_edited = subset(df, select=-Power)
# Convert row names into a column named "Years"
df_edited <- rownames_to_column(df_edited, var = "Years")
df_rounded <- df_edited %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

write_csv(df_rounded, here("datasets", "comparedmatrix.csv"))