# Set the working directory and verify it
#setwd("C:/Users/gauri/Downloads/NSSO68.csv")
#getwd()

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA","glue")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("C:/Users/gauri/Downloads/NSSO68.csv")
# Display the first few rows of the data
head(data)

# Filtering for GUJ
df <- data %>%
  filter(state_1 == "GUJ")

# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Sub-setting the data
gujnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Check for missing values in the subset
cat("Missing Values in Subset:\n")
print(colSums(is.na(gujnew)))

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
gujnew$Meals_At_Home <- impute_with_mean(gujnew$Meals_At_Home)

# Check for missing values after imputation
cat("Missing Values After Imputation:\n")
print(colSums(is.na(gujnew)))

#checking for outliers
#box plot
boxplot(gujnew$wheatos_q)

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c("wheatos_q")
for (col in outlier_columns) {
  gujnew <- remove_outliers(gujnew, col)
}

#after removing outliers
boxplot(gujnew$wheatos_q)

# Summarize consumption
gujnew$total_consumption <- rowSums(gujnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top and bottom consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- gujnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top 3 Consuming Districts:\n")
print(head(district_summary, 3))
cat("Bottom 3 Consuming Districts:\n")
print(tail(district_summary, 3))

cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors , get codes from appendix of NSSO 68th ROund Data
district_mapping <- c("1" = "Ahmedabad", "2"="Amreli","3"="Anand","4"="Aravalli","5"="Banaskantha",
                      "6"="Bharuch","7"="Bhavnagar","8"="Botad","9"="Chota Udaipur","10"="Dahid","11"="Dang",
                      "12"="Dwarka","13"="Gandhinagar","14"="Gor Somnath","15"="Jamnagar","16"="Junagadh",
                      "17"="Kutch","18"="Kheda","19"="Mahisagar","20"="Mehsana","21"="Morbi","22"="Narmada")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

gujnew$District <- as.character(gujnew$District)
gujnew$Sector <- as.character(gujnew$Sector)
gujnew$District <- ifelse(gujnew$District %in% names(district_mapping), district_mapping[gujnew$District],
                          gujnew$District)
gujnew$Sector <- ifelse(gujnew$Sector %in% names(sector_mapping), sector_mapping[gujnew$Sector], gujnew$Sector)


# Test for differences in mean consumption between urban and rural
rural <- gujnew %>%
  filter(Sector == "RURAL") %>%
  select(total_consumption)

urban <- gujnew %>%
  filter(Sector == "URBAN") %>%
  select(total_consumption)

mean_rural <- mean(rural$total_consumption)
mean_urban <- mean(urban$total_consumption)

# Perform z-test
z_test_result <- z.test(rural, urban, alternative = "two.sided", mu = 0, sigma.x = 2.56, sigma.y = 2.34, conf.level = 0.95)

# Generate output based on p-value
if (z_test_result$p.value < 0.05) {
  cat(glue::glue("P value is < 0.05 i.e. {round(z_test_result$p.value,5)}, Therefore we reject the null hypothesis.\n"))
  cat(glue::glue("There is a difference between mean consumptions of urban and rural.\n"))
  cat(glue::glue("The mean consumption in Rural areas is {mean_rural} and in Urban areas its {mean_urban}\n"))
} else {
  cat(glue::glue("P value is >= 0.05 i.e. {round(z_test_result$p.value,5)}, Therefore we fail to reject the null hypothesis.\n"))
  cat(glue::glue("There is no significant difference between mean consumptions of urban and rural.\n"))
  cat(glue::glue("The mean consumption in Rural area is {mean_rural} and in Urban area its {mean_urban}\n"))
}
