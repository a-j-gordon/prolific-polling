# Code to analyse trends in polling data across time
# Requires user to have all existing poll excel files (available on OSF) within the same file as this code
rm(list = ls())

########################### Read in/install required packages ###########################
packages <- c("readxl", "ggplot2", "reshape2","dplyr") #Specifiy required packages
# Check and install packages
for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}

########################### Set global variables ###########################
home.dir <- "/Users/andrewgordon/Documents/Poll/all.data"
setwd(home.dir)
sheet_name <- "PartyUpcoming" # Specify the sheet that contains the data you want to look at - you can get the sheet names from any of the excel files (note: only long-term polling data/questiosn should be used) 
demo.of.interest <- "Female" # Specify the demographic level that you want to analyse

########################### Read in the data ###########################

# List all Excel files in the directory
excel_files <- list.files(home.dir, pattern = "\\.xlsx$", full.names = TRUE)

# Function to read data from specified sheet and create a dataframe
read_specified_sheet <- function(file_path, sheet_name) {
  # Extract the filename without extension
  file_name <- tools::file_path_sans_ext(basename(file_path))
  # Read the specified sheet
  sheet_data <- readxl::read_excel(file_path, sheet = sheet_name)
  # Return a list containing the dataframe and filename
  list(data = sheet_data, file_name = file_name)
}

# Apply the function to each Excel file
results <- lapply(excel_files, read_specified_sheet, sheet_name = sheet_name)

# Access dataframes and filenames from the results
for (result in results) {
  # Access dataframe
  df <- result$data
  # Access filename
  file_name <- result$file_name
  
  # Remove spaces in file name
  file_name <- gsub(" ", "_", file_name)
  
  # Remove the unwanted first column
  df <- df[, !colnames(df) %in% "Demographic Factors"] 
  
  # Melt df to make it ready for graphing
  df <- reshape2::melt(df)
  
  # Name the dataframe
  assign(file_name, df)  
}

# Combine all dataframes into one df for graphing
final_df <- bind_rows(mget(ls(pattern = "^Prolific_Poll_")), .id = "Date")

# remove Prolific poll part of name and rename Demographic column
final_df$Date <- gsub("Prolific_Poll_", "", final_df$Date)
colnames(final_df)[colnames(final_df) == "Demographic Level"] <- "Demographic"

# Factorise columns
final_df$Demographic <- as.factor(final_df$Demographic)
# final_df$Date <- as.factor(final_df$Date)


########################### Create plot(s) ###########################
# Set which demographic subgroup you are interested in (default is National)
# To be updated to compare different demographic subgroups

# create dataframe that only includes demographic variable(s) of interest
if(length(demo.of.interest)>1){
  graphing.data <- subset(final_df,Demographic == demo.of.interest[1] | Demographic == demo.of.interest[2])  # extend with further logicals to add more options (must be same length as demo.of.interest)
} else {
    graphing.data <- subset(final_df,Demographic == demo.of.interest)
    }

# Edit the date information so that it will be ordered correctly in final graph
vec <- unique(graphing.data$Date) # find unique values
split_vec <- strsplit(vec, "\\.") # split into components
middle_nums <- sapply(split_vec, function(x) as.numeric(x[2])) # extracting middle component - first number to sort by
first_nums <- sapply(split_vec, function(x) as.numeric(x[1])) # extract first component - second number to sort by
sorted_dates <- vec[order(middle_nums, first_nums)] # sort based on middle number and then first number for final date order

# implement ordering in main df
graphing.data$Date <- factor(graphing.data$Date , levels = sorted_dates)


# plotting
ggplot(graphing.data, aes(x = Date, y = value, group = variable)) +
  geom_line(aes(color = variable)) +
  geom_point(aes(color = variable), size = 3) +
  labs(x = "Date", y = "Percent", color = "Response") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) 
