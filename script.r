## Defining methods
# Generate lables for pie charts
get_labels <- function(col) {
  labels <- paste(names(table(col)), "\n","n=", table(col), sep="")
  return(labels)
}

# Used for handling comma seperated decimals (like "0,5")
get_ceil <- function(x) {
  return (as.integer(ceiling(as.double(gsub(",",".",x)))))
}

## Setting the work directory 
# WATNING: if you are using windows you can face problems setting the path (this was tested only on linux)
library(rstudioapi) 
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
# Verifying the work directory
getwd()

## Reading and Preparing data (the na values where kept as they are, it's up to you to handle them as you see best)
data <- read.csv("data.csv",sep = ",")
# Redefining column names
new_column_names <- c("submitted_date","weekly_freq","last_taken","last_thrown","average_taken","average_thrown","weekly_consumption_out","sorting_waste","sign_seen","sign_influence","physical_form","fried","sauce","eat_with","sport_hours","status","cloud_cover","wind_speed","tempreature","precipetation","sign","gender","age","meal")
colnames(data) <- new_column_names
# Deleting the "sign_influence" column 
data <- data[,-10]

library(plyr)
# Remapping values
data$sorting_waste <- mapvalues(data$sorting_waste,from=c("Jamais","Moins d'une fois sur deux","Plus d'une fois sur deux", "Toujours"),to=c("0","0-0.5","0.5-1","1"))
data$age <- mapvalues(data$age,from=c("Moins de 30 ans","Entre 30 et 40","Entre 40 et 50", "Plus de 50 ans"),to=c("<30","30-40","40-50",">50"))
# Converting values that contain comma decimal seperators (can't be interpreted) to integer
# using the get_ciel method defined earlier
data$last_thrown <- sapply(data$last_thrown,FUN = get_ceil)
data$average_taken <- sapply(data$average_taken,FUN = get_ceil)
data$average_thrown <- sapply(data$average_thrown,FUN = get_ceil)
data$weekly_consumption_out <- sapply(data$weekly_consumption_out,FUN = get_ceil)
data$physical_form <- sapply(data$physical_form,FUN = get_ceil)
data$sport_hours <- sapply(data$sport_hours,FUN = get_ceil)

## Plotting data
# Again na values where not handled, handle them as you wish 
barplot(table(data$weekly_freq))
barplot(table(data$last_taken))
barplot(table(data$last_thrown))
barplot(table(data$average_taken))
barplot(table(data$average_thrown))
barplot(table(data$weekly_consumption_out))

pie(table(data$sorting_waste),labels = get_labels(data$sorting_waste))
pie(table(data$sign_seen),labels = get_labels(data$sign_seen))
pie(table(data$physical_form),labels = get_labels(data$physical_form))
pie(table(data$fried),labels = get_labels(data$fried))
pie(table(data$sauce),labels = get_labels(data$sauce))

barplot(table(data$wind_speed))
barplot(table(data$sport_hours))

pie(table(data$status),labels = get_labels(data$status))
pie(table(data$cloud_cover),labels = get_labels(data$cloud_cover))

barplot(table(data$wind_speed))
barplot(table(data$tempreature))
barplot(table(data$precipetation))

pie(table(data$sign),labels = get_labels(data$sign))
pie(table(data$gender),labels = get_labels(data$gender))
pie(table(data$age),labels = get_labels(data$age))
pie(table(data$meal),labels = get_labels(data$meal))
