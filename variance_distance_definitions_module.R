############################  VARIANCE DISTANCE DEFINITIONS MODULE  ############################
#---------------------------------------------------------------------------------------#
# Author: Ihor Dorosneko
#---------------------------------------------------------------------------------------#

# -variance distance calculation function- #
measure_variance_distance <- function(dataframe, method) {
  maintable <- data.frame(matrix(nrow = nrow(dataframe), ncol = 1))
  for (number in 1:ncol(dataframe)) {
    results <- data.frame(matrix(nrow = nrow(dataframe), ncol = ncol(dataframe)))
    for(row in 1:nrow(dataframe)) {
      for (col in 1:ncol(dataframe)) {
        results[row,col] <- stringdist(dataframe[row,col], dataframe[row,number], method = method)
      }
    }
    results[,number] <- NULL
    maintable <- cbind(maintable, results)
  }
  maintable[,1] <- NULL
  return(maintable)
}

# -variance distance combine function- #
variance <- function(citynames, dataframe, year) {
  dataframe_variance <- measure_variance_distance(dataframe, "lv")
  dataframe_variance_means <- data.frame(round(rowMeans(dataframe_variance[,-1], na.rm = TRUE), digits=2))
  dataframe_variance_means[is.nan(dataframe_variance_means)] <- 0
  colnames(dataframe_variance_means) <- paste(year, "variance")
  return(dataframe_variance_means)
}

