
library(ggplot2)

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

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
check_missing_values(chi)
check_missing_values(ny)
check_missing_values(wash)

# Define a function to calculate the mode of a vector
Uniq.Func <- function(DS){
  # Get the unique values in the vector
  U.F <- unique(DS)
  # Calculate the frequency of each unique value
  freq <- tabulate(match(DS, U.F))
  # Return the unique value with the highest frequency (the mode)
  U.F[which.max(freq)]
}

# Call the Uniq.Func function for each city's data for the starting months and print the results
Uniq.Func(chi)
Uniq.Func(ny)
Uniq.Func(wash)

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
create_trip_count_bar_chart(chi)
create_trip_count_bar_chart(ny)
create_trip_count_bar_chart(wash)

"Based on the generated plots, it is evident that the month of June (06/2017) recorded the highest number
of trips in all three cities. This observation aligns with the fact that more people tend to ride bicycles 
during the summer season compared to other seasons."

#Summary For All Datasets
summary(chi['Start.Time'])
print('----------------------------------------------------------------------------')
summary(ny['Start.Time'])
print('----------------------------------------------------------------------------')
summary(wash['Start.Time'])

# Print the first 3 rows of the 'ny' data frame
slice_head <- function(city_data){
  head(city_data, 3)
}
slice_head(ny)

# Get the dimensions of the 'ny' data frame with the 'Trip.Duration' column included
get_dimensions <- function(city_data){
  dim(city_data['Trip.Duration'])
}
get_dimensions(ny)

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
Total.T.D.Users(ny)
Total.T.D.Users(chi)
Total.T.D.Users(wash)

"It can be observed that the number of customers in trip duration is significantly higher than 
the number of subscribers in all three states. This indicates that customers are more 
enthusiastic and likely to take longer trips compared to subscribers."

# Display summary statistics of trip duration by user type for each city's data
by(ny$Trip.Duration, ny$User.Type, summary)
print("_________________________________________________________")
by(chi$Trip.Duration, chi$User.Type, summary)
print("_________________________________________________________")
by(wash$Trip.Duration, wash$User.Type, summary)

"It is worth noting that the clients in the three states are arranged in descending order as follows: 
1) Washonton, 2) New York, 3) Chicago"

"And in terms of subscribers:
1) New York, 2) Washonton, 3) Chicago."

# Define a function to calculate the mean trip duration for each user type using the by() function
calculate_mean_duration_by_user_type <- function(city_data) {
  by(city_data$Trip.Duration, city_data$User.Type, mean)
}

# Call the calculate_mean_duration_by_user_type function for each city's data frame and print a separator line between the results
cat("Mean trip duration by user type:\n")
print(calculate_mean_duration_by_user_type(ny))
cat("_________________________________________________________\n")
print(calculate_mean_duration_by_user_type(chi))
cat("_________________________________________________________\n")
print(calculate_mean_duration_by_user_type(wash))

# Define a function to create a bar plot of user type counts
user_type_plot <- function(data) {
  ggplot(data = subset(data, User.Type != ""), aes(x = User.Type)) +
    geom_bar(color = 'black', fill = 'pink') +
    ggtitle('Counts of Each User Type') +
    labs(x = 'User Type', y = 'Count of User Type')
}

# Call the user_type_plot function for each city's data and create a bar plot of user type counts
user_type_plot(ny)
user_type_plot(chi)
user_type_plot(wash)

"It can be observed that all three states have a higher number of subscribers compared to customers." 

# Print a summary of the 'User.Type' column in the 'ny' data frame
summary(ny['User.Type'])

# Print a separator line
print('--------------')

# Print a summary of the 'User.Type' column in the 'chi' data frame
summary(chi['User.Type'])

# Print a separator line
print('--------------')

# Print a summary of the 'User.Type' column in the 'wash' data frame
summary(wash['User.Type'])

# Define a function to create a bar plot of birth year counts
birth_year_plot <- function(data) {
  ggplot(data = na.omit(data), aes(x = Birth.Year)) +
    geom_bar(color = 'black', fill = 'pink') +
    scale_x_continuous(breaks = seq(0, 2000, 3)) +
    coord_cartesian(xlim = c(1940, 2000)) +
    ggtitle('Earliest and Most Common Year of Birth') +
    labs(x = 'Birth Year', y = 'Count')
}

# Call the birth_year_plot function for each city's data and create a bar plot of birth year counts
birth_year_plot(ny)
birth_year_plot(chi)

"Based on the generated plots, we can observe that the New York State has its earliest, 
most recent and most frequent birth years between 1985 and 1990, with 1989 being the year with the highest count. 
On the other hand, the most common birth year in Chicago is between 1986 and 1992, 
but the most frequent age is the same as that of New York, which is 1989."

# Summarize the 'Birth.Year' column in the 'chi' data frame
summary(chi$Birth.Year)

# Print a separator line
cat("------------\n")

# Summarize the 'Birth.Year' column in the 'ny' data frame
summary(ny$Birth.Year)

system('python -m nbconvert Explore_bikeshare_data.ipynb')
