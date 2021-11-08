############################   MAIN MODULE   ############################
#---------------------------------------------------------------------------------------#
# Author: Ihor Dorosneko
# Data: place_name_database.csv
#---------------------------------------------------------------------------------------#

### ---DATA IMPORT--- ###
# -setting the working directory- #
currentPath <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(currentPath)
# -loading the packages- #
install.packages("pacman")
pacman::p_load(ggplot2, stringdist, stringr, ggmap, rgdal, writexl)
# -loading and pre-processing the data- #
places <- read.csv("place_name_database_to_process.csv", sep="\t", encoding = "UTF-8", na.strings=c(""," ", "NA"))
places <- data.frame(lapply(places, as.character), stringsAsFactors=FALSE)

### ---DATA PROCESSING--- ###
# -assigning the spatial coordinates- #
Cities_with_coords <- get_cities_and_coordinates(places)
# -calculating the distances- #
calculated_overall_distances <- calculate_distances(places$City, places, overall)
calculated_variance_distances <- calculate_distances(places$City, places, variance)
# -combining with the coordinates- #
overall_distances_located <- combine_with_coordinates(calculated_overall_distances, Cities_with_coords[,c(2,3)])
variance_distances_located <- combine_with_coordinates(calculated_variance_distances, Cities_with_coords[,c(2,3)])

### ---DATA EXPORT--- ###
# -exporting as shape-file- #
export_as_shape_file(overall_distances_located, "overall_distances_located")
export_as_shape_file(variance_distances_located, "variance_distances_located")
# -exporting as excel-file- #
write_xlsx(overall_distances_located,path = "C:/MyDirectory/Masterarbeit/Data/results_overall_neu.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)
write_xlsx(variance_distances_located,path = "C:/MyDirectory/Masterarbeit/Data/results_variance_neu.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)

### ---ADDITIONAL CALCULATIONS--- ###
# -Source coverage calculating- #
absoluteAmount <- count_source_coverage(places, 6)
counted <- cbind(absoluteAmount, lon = Cities_with_coords$lon, lat = Cities_with_coords$lat)
export_as_shape_file(counted, "counted")
write_xlsx(counted,path = "C:/MyDirectory/Masterarbeit/Data/counted.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)
# -Calculating total amount of names- #
sum(as.vector(apply(places, 2, function(x) length(which(!is.na(x))))))

