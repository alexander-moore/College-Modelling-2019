load("~/math343_s19_college_modeling/data/colleges.Rda")
df <- colleges

#############
# Count NA in variables
isna <- apply(df, 2, is.na)

tots <- apply(isna, 2, sum)

tots

#############
df$room_board <- as.numeric(df$room_board)
df$dorm_capacity <- as.numeric(df$dorm_capacity)
df$in_state_tuition <- as.numeric(df$in_state_tuition)
df$out_state_tuition <- as.numeric(df$out_state_tuition)


#############
# Impute missing numeric values by median of present cases for that feature

median_impute <- function(df) {
  
  for (i in 4:dim(df)[[2]]) {
    df[[i]] <- as.numeric(df[[i]])
    df[[i]][is.na(df[[i]])] <- median(df[[i]][!is.na(df[[i]])])
  }
  
  df
  
}

df <- median_impute(df)

df$school_id <- as.character(df$school_id)

df$control <- as.factor(df$control)
df$carnegie <- as.factor(df$carnegie)

df$in_state_tuition <- as.numeric(df$in_state_tuition)
df$out_state_tuition <- as.numeric(df$out_state_tuition)

isna <- apply(df, 2, is.na)

tots <- apply(isna, 2, sum)

tots

write.csv(df, file = "~/math343_s19_college_modeling/data/imputed_colleges.Rda")

