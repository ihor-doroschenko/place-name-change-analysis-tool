############################   OVERALL DISTANCE DEFINITIONS MODULE   ############################
#---------------------------------------------------------------------------------------#
# Author: Ihor Dorosneko
#---------------------------------------------------------------------------------------#

# -overall distance calculation function- #
measure_overall_distance <- function(dataframe) {
  results <- data.frame(matrix(nrow = nrow(dataframe), ncol = ncol(dataframe)))
  for(row in 1:nrow(dataframe)) {
    for (col in 2:ncol(dataframe)) {
      results[row,col] <- stringdist(dataframe[row,col], dataframe[row,1], method = "lv")
    }
  }
  colnames(results) <- colnames(dataframe)
  results$`places$City` <- NULL
  return(results)
}

# -overall distance combine function- #
overall <- function(citynames, dataframe, year) {
  overall_dataframe <- cbind(citynames, dataframe)
  overall_dataframe <- measure_overall_distance(overall_dataframe)
  overall_dataframe_means <- data.frame(year = round(rowMeans(overall_dataframe, na.rm = TRUE), digits=2))
  colnames(overall_dataframe_means) <- paste(year, "overall")
  return(overall_dataframe_means)
}

