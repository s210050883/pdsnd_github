library(ggplot2)

ny <- read.csv('new_york_city.csv')
wash <- read.csv('washington.csv')
chi <- read.csv('chicago.csv')

head(ny)
head(wash)
head(chi)

# Define a function to check for missing values in a data frame and return a vector of the results
check_missing_values <- function(city_data) {
  # Check whether any missing values are present in the data frame
  any_missing <- any(is.na(city_data))

  # Calculate the total number of missing values in the data frame
  total_missing <- sum(is.na(city_data))

  # Calculate the number of missing values in each column of the data frame
  column_missing <- colSums(is.na(city_data))

  # Return a vector of the results
  return(c(any_missing, total_missing, column_missing))
}

# Call the check_missing_values function for each city's data frame
for (city in list(ny, wash, chi)) {
  check_missing_values(city)
}

# Define a function to calculate the mode of a vector
get_mode <- function(x) {
  # Get the unique values in the vector
  U.F <- unique(x)
  # Calculate the frequency of each unique value
  freq <- tabulate(match(x, U.F))
  # Return the unique value with the highest frequency (the mode)
  U.F[which.max(freq)]
}

# Call the get_mode function for each city's "Start.Time" and print the results
for (city in list(ny, wash, chi)) {
  get_mode(city$Start.Time)
}

# Define a function to create a bar chart of trip counts by month using ggplot2
create_trip_count_bar_chart <- function(city_data) {
  # Convert the Start.Time variable to a date format
  city_data$Start.Time <- as.Date(city_data$Start.Time)

  # Create a bar chart of trip counts by month using ggplot2
  ggplot(aes(format(Start.Time, "%Y-%m")), data = na.omit(city_data)) +
    geom_bar(width = 0.6, color = 'black', fill = 'pink') +
    ggtitle('Most Common Month Bar Charts') +
    labs(x = 'First 6 Months of 2017', y = 'Trips Count')
}

# Call the create_trip_count_bar_chart function for each city's data and create a bar chart of trip counts by month
for (city in list(ny, wash, chi)) {
  create_trip_count_bar_chart(city)
}

"Based on the generated plots, it is evident that the month of June (06/2017) recorded the highest number
of trips in all three cities. This observation aligns with the fact that more people tend to ride bicycles
during the summer season compared to other seasons."

#Summary For All Datasets
summary(chi['Start.Time'])
print('----------------------------------------------------------------------------')
summary(ny['Start.Time'])
print('----------------------------------------------------------------------------')
summary(wash['Start.Time'])

# Define a function to slice the first 3 rows of a data frame
slice_head <- function(city_data){
  head(city_data, 3)
}

# Call the slice_head function for the 'ny' data frame
slice_head(ny)

# Define a function to get the dimensions of a data frame with a specified column
get_dimensions <- function(city_data, column_name){
  return(dim(city_data[column_name]))
}

# Call the get_dimensions function for the 'ny' data frame with the 'Trip.Duration' column included
get_dimensions(ny, 'Trip.Duration')

# Define a function to create a box plot of trip duration by user type with custom colors
Total.T.D.Users <- function(DS){
  ggplot(aes("User.Type", `Trip.Duration`, fill = User.Type, color =User.Type), data = subset(DS, User.Type != "")) +
    geom_boxplot() +
    scale_y_discrete(limits = seq(0, 3000, 250)) +
    ggtitle('Total travel time for users in different cities') +
    labs(x = 'Users Type', y = 'Trip Duration') +
    scale_fill_manual(values = c("pink", "orange")) + # Set fill colors
    scale_color_manual(values = c("black", "black")) # Set outline colors
}

# Call the Total.T.D.Users function for each city's data and create a box plot of trip duration by user type with custom colors
for (city in list(ny, wash, chi)) {
  Total.T.D.Users(city)
}

"It can be observed that the number of customers in trip duration is significantly higher than
the number of subscribers in all three states. This indicates that customers are more
enthusiastic and likely to take longer trips compared to subscribers."
