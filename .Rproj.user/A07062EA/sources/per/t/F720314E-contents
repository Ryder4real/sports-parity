library(here)
library(readr)
library(dplyr)

# https://www.kaggle.com/datasets/datascientist97/hockey-teams-data-2024

standings <- read_csv(here("datasets", "NHL_Scraped_data.csv"))

get_standings <- function(year) {
  
  year_df <- standings %>% filter(Year == year)

  year_df$`OT Losses`[is.na(year_df$`OT Losses`)] <- 0
  
  year_df$teamID <- year_df$`Team Name`
  year_df$points <- 2 * year_df$Wins + 1 * year_df$`OT Losses`

  if ("Minnesota North Stars" %in% year_df$teamID) {
    idx <-  which(year_df$teamID == "Minnesota North Stars")
    year_df[idx, "teamID"] <- "Dallas Stars"
  }
  if ("Quebec Nordiques" %in% year_df$teamID) {
    idx <-  which(year_df$teamID == "Quebec Nordiques")
    year_df[idx, "teamID"] <- "Colorado Avalanche"
  }
  if ("Winnipeg Jets" %in% year_df$teamID) {
    idx <-  which(year_df$teamID == "Winnipeg Jets")
    year_df[idx, "teamID"] <- "Phoenix Coyotes"
  }
  
  if ("Hartford Whalers" %in% year_df$teamID) {
    idx <-  which(year_df$teamID == "Hartford Whalers")
    year_df[idx, "teamID"] <- "Carolina Hurricanes"
  }
  if ("Mighty Ducks of Anaheim" %in% year_df$teamID) {
    idx <-  which(year_df$teamID == "Mighty Ducks of Anaheim")
    year_df[idx, "teamID"] <- "Anaheim Ducks"
  }
  
  return(year_df[,c("teamID", "points")])
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
  df$points <- as.numeric(df$points)
  df_sorted <- df[order(df$points), ]
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
  else {
    print(team_id)
  }
  return(matrix)
  
}

total_interations = 0
year_count = 0
counts_matrix <- matrix(0, nrow = 6, ncol = 6)

for (year in 1990:2008) {
  if (year != 2004) {
    if (year_count == 0) {
      prev_standings <- get_standings(year)
      next_standings <- get_standings(year + 1)
    } else {
      prev_standings = next_standings
      if (year != 2003) {
        next_standings <- get_standings(year + 1)
      } else {
        next_standings <- get_standings(year + 2)
      }
        
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
}

row_sums = rowSums(changing_matrix)

header_names <- c("Bottom 1/6 of Teams", "Tier 5",
                  "Tier 4", "Tier 3", 
                  "Tier 2", "Top 1/6 of Teams")

normalized_matrix <- sweep(changing_matrix, 1, row_sums, FUN = "/")
rowSums(normalized_matrix)

titled_matrix <- normalized_matrix


colnames(titled_matrix) <- header_names
rownames(titled_matrix) <- header_names


matrix_k = normalized_matrix
for (i in 1:3){
  matrix_k = matrix_k %*% normalized_matrix
}

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

absorbed <- make_absorbing_4_to_6(normalized_matrix)

matrix_k = absorbed
for (i in 1:5){
  matrix_k = matrix_k %*% absorbed
}


write_csv(as.data.frame(normalized_matrix), here("datasets", "NHLNorm.csv"), col_names = TRUE)





