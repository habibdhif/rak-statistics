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
attach(data)

# Redefining column names
new_column_names <- c("submitted_date","weekly_freq","last_taken","last_thrown","average_taken","average_thrown","weekly_consumption_out","sorting_waste","sign_seen","sign_influence","physical_form","fried","sauce","eat_with","sport_hours","status","cloud_cover","wind_speed","temperature","precipetation","sign","gender","age","meal")
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
# Discrete quantitative variables
barplot(table(data$weekly_freq), main = "barplot RAK weekly frequency")
barplot(table(data$last_taken), main = "barplot nb BREAD last taken")
barplot(table(data$last_thrown), main = "barplot nb BREAD last thrown")
barplot(table(data$average_taken), main = "barplot nb BREAD avg taken")
barplot(table(data$average_thrown), main = "barplot nb BREAD avg thrown")
barplot(table(data$weekly_consumption_out), main = "barplot nb BREAD weekly consumption outside rak")

# Ordinal quantitative variables
barplot(table(data$physical_form), main = "barplot PHYSICAL form")

# Nominal qualitative variables
pie(table(data$sorting_waste),labels = get_labels(data$sorting_waste))
pie(table(data$sign_seen),labels = get_labels(data$sign_seen))
pie(table(data$fried),labels = get_labels(data$fried))
pie(table(data$sauce),labels = get_labels(data$sauce))
pie(table(data$status),labels = get_labels(data$status))
pie(table(data$cloud_cover),labels = get_labels(data$cloud_cover))
pie(table(data$sign),labels = get_labels(data$sign))
pie(table(data$gender),labels = get_labels(data$gender))
pie(table(data$age),labels = get_labels(data$age))
pie(table(data$meal),labels = get_labels(data$meal))

# Continuous quantitative variables (good to have histograms for those)
barplot(table(data$wind_speed))
hist(data$wind_speed)
barplot(table(data$sport_hours))
hist(data$sport_hours)
barplot(table(data$temperature))
hist(data$temperature)
barplot(table(data$precipetation))
hist(data$precipetation)


# Summaries for quantitative data : position & dispersion indicators
summary(data$weekly_freq)
summary(data$last_taken)
summary(data$last_thrown)
summary(data$average_taken)
summary(data$average_thrown)
summary(data$weekly_consumption_out)
summary(data$physical_form)
summary(data$eat_with)
summary(data$sport_hours)
summary(data$wind_speed)
summary(data$sport_hours)
summary(data$temperature)
summary(data$precipetation)

# skewness : if <0 <=> skewed towards the left, if > 0, skewed towards the right
library("e1071")
skewness(data$weekly_freq, na.rm = T)
skewness(data$last_taken, na.rm = T)
skewness(data$last_thrown, na.rm = T)
skewness(data$average_taken, na.rm = T)
skewness(data$average_thrown, na.rm = T)
skewness(data$weekly_consumption_out, na.rm = T)
skewness(data$physical_form, na.rm = T)
skewness(data$eat_with, na.rm = T)
skewness(data$sport_hours, na.rm = T)
skewness(data$wind_speed, na.rm = T)
skewness(data$sport_hours, na.rm = T)
skewness(data$temperature, na.rm = T)
skewness(data$precipetation, na.rm = T)
