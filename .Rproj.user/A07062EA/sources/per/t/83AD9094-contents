options("max.print" = 2000)
library(here)
library(readr)
library("Lahman")
library(dplyr)

get_standings <- function(year) {
  spec_df <- Teams %>% filter(yearID == year)
  spec_df <- spec_df[,c("franchID", "W", "L")]
  spec_df$WinPCT <- spec_df$"W" / (spec_df$"W" + spec_df$"L")
  spec_df$teamID <- spec_df$franchID
  
  return(spec_df[,c("teamID", "WinPCT")])
}

sort_bucket <- function(inverse_rank, numTeams) {
  result <- inverse_rank / numTeams
  if (result <= (1/6)) {
    return(1)
  } else if (result <= (2/6)) {
    return(2)
  } else if (result <= (3/6)) {
    return(3)
  } else if (result <= (4/6)) {
    return(4)
  } else if (result <= (5/6)) {
    return(5)
  } else {
    return(6)
  }
}

sortTeams <- function(df) {
  df$WinPCT <- as.numeric(df$WinPCT)
  df_sorted <- df[order(df$WinPCT), ]
  return(df_sorted)
}   

run_process <- function(prev_data, next_data, team_id, matrix) {
  
  next_teams_list <- next_data$teamID
  if (team_id %in% next_teams_list) {
    
    prev_teams_list <- prev_data$teamID

    prev_team_idx <- which(prev_teams_list == team_id)
    
    prev_win_bucket <- sort_bucket(prev_team_idx, length(prev_teams_list))
    
    next_team_idx <- which(next_teams_list == team_id)
    next_win_bucket <- sort_bucket(next_team_idx, length(next_teams_list))
    
    matrix[prev_win_bucket, next_win_bucket] <-
      matrix[prev_win_bucket, next_win_bucket] + 1
    
  }
  return(matrix)
  
}

total_interations = 0
year_count = 0
counts_matrix <- matrix(0, nrow = 6, ncol = 6)

for (year in 1969:2012) {
  if (year_count == 0) {
    prev_standings <- get_standings(year)
    next_standings <- get_standings(year + 1)
  } else {
    prev_standings = next_standings
    next_standings <- get_standings(year + 1)

  }
  Sys.sleep(1)
  
  year_count = year_count + 1
  print(year)
  
  prev_order <- sortTeams(prev_standings)
  next_order <- sortTeams(next_standings)
  
  team_nums = prev_standings$teamID
  
  for (team_num in team_nums) {
    if (total_interations == 0) {
      changing_matrix <- run_process(prev_order, next_order, team_num, counts_matrix)
    } else {
      changing_matrix <- run_process(prev_order, next_order, team_num, changing_matrix)
    }
    total_interations = total_interations + 1
  }
  
}

row_sums = rowSums(changing_matrix)

header_names <- c("Worst 5 Teams", "Teams 25-21",
                  "Teams 20-16", "Teams 15-11", 
                  "Teams 10-6", "Top 5 Teams")

normalized_matrix <- sweep(changing_matrix, 1, row_sums, FUN = "/")
rowSums(normalized_matrix)

titled_matrix <- normalized_matrix

colnames(titled_matrix) <- header_names
rownames(titled_matrix) <- header_names

matrix_k = normalized_matrix
for (i in 1:3){
  matrix_k = matrix_k %*% normalized_matrix
}

write_csv(as.data.frame(normalized_matrix), here("datasets", "MLBNorm.csv"), col_names = TRUE)


