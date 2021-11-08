############################   GENERAL DEFINITIONS MODULE   ############################
#---------------------------------------------------------------------------------------#
# Author: Ihor Dorosneko
#---------------------------------------------------------------------------------------#

# -getting amount of sources- #
give_period <- function(dataframe, number) {
  columnNames <- grep(str_interp("X${number}."), names(dataframe), value=FALSE)
  return(columnNames[!is.na(columnNames)])
}

# -assigning and correcting (based on QGIS) the coordinates- #
get_cities_and_coordinates <- function(dataframe) {
  register_google(key = "AIzaSyBoKC1GIjr6aX7YJ1IfMnWkkCA8ZT_QyF0")
  Cities_with_coords <- data.frame(matrix(nrow = nrow(dataframe), ncol = 1))
  colnames(Cities_with_coords) <- "City"
  Cities_with_coords$City <- as.character(dataframe$City)
  Cities_with_coords <- mutate_geocode(Cities_with_coords, City)
  Cities_with_coords[which(grepl("Puck", Cities_with_coords$City)),c(2,3)] <- c(18.40720, 54.71711)
  Cities_with_coords[which(grepl("Biskupiec", Cities_with_coords$City)),c(2,3)] <- c(19.34865, 53.50048)
  Cities_with_coords[which(Cities_with_coords$City == "Szymbark"), c(2,3)] <- c(19.485872, 53.647170)
  Cities_with_coords[which(Cities_with_coords$City == "Osiek"), c(2,3)] <- c(18.489352, 53.722284)
  Cities_with_coords[which(Cities_with_coords$City == "Jasieniec"), c(2,3)] <- c(18.51555556, 53.685)
  Cities_with_coords[which(Cities_with_coords$City == "Stara Kiszewa"), c(2,3)] <- c(18.169290, 53.990190)
  Cities_with_coords[which(Cities_with_coords$City == "Pokrzywno"), c(2,3)] <- c(18.848120, 53.438747)
  return(Cities_with_coords)
}

# -exporting as shape-file function- #
export_as_shape_file <- function(dataframe, string) {
  coordinates(dataframe) = ~lon+lat
  proj4string(dataframe)<- CRS("+proj=longlat +datum=WGS84")
  writeOGR(dataframe, dsn=".", 
           layer=string, driver="ESRI Shapefile", 
           layer_options = "ENCODING=UTF-8", overwrite_layer=TRUE)
}

# -checking NaN- #
is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}

# -combine with coordinates function- #
combine_with_coordinates <- function(dataframe, locations) {
  timespans <- c("1", "2", "3", "4", "5", "6")
  dataframe_with_locations <- cbind(dataframe, locations)
  colnames(dataframe_with_locations) <- c("City", timespans, "lon", "lat")
  return(dataframe_with_locations)
}

# -calculate distance meta-function- #
calculate_distances <- function(placeNames, dataframe, function_to_calculate) {
  timespans <- c("1", "2", "3", "4", "5", "6")
  final_dataframe <- placeNames
  for (i in 1:length(timespans)) {
    subsetted_dataframe <- data.frame(dataframe[,give_period(dataframe, i)])
    calculated_dataframe <- function_to_calculate(placeNames, subsetted_dataframe, timespans[i])
    final_dataframe <- cbind(final_dataframe, calculated_dataframe)
  }
  return(final_dataframe)
}

