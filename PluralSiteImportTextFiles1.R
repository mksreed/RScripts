setwd("C:/Users/msreed/Documents/Personal/RSpace/PluralSiteImportingFormattedTextFiles")
dir()
#***********************************************header=FALSE by default.
employee_df <- read.table("employee data.txt")
employee_df
employee_df <- read.table("employee data.txt", header = TRUE)
employee_df
# Display structure of employee_df, notice by default our strings/characters come in as factors
str(employee_df)
# Use argument (stringsAsFactors = FALSE) if we want
employee_df <- read.table("employee data.txt", header= TRUE, stringsAsFactors = FALSE)
# Display structure of employee_df, notice now our strings/characters come in as strings/characters
str(employee_df)
# Use argument (skip = 2) to skip the first two lines of the txt file
employee_df <- read.table("employee data skip lines.txt", header = TRUE, stringsAsFactors = FALSE, skip = 2)
employee_df
# Use argument (sep = '/') when data is separated by '/'
employee_df <- read.table("employee data slash.txt", header = TRUE, stringsAsFactors = FALSE, sep = '/')
employee_df
# Make use of arguments "skip" and "nrows" in combination to help read specific subsets of data
employee_df <- read.table("employee data certain lines.txt", header = FALSE,
                          stringsAsFactors = FALSE, skip = 2, nrows = 3, 
                          col.names = c("ID", "Name", "Department", "Salary"))
employee_df
#***************************************************in CSV Header=TRUE is default.
employee_df <- read.csv("employee data.csv")
employee_df
employee_df <- read.csv("employee data.csv", stringsAsFactors = FALSE)
employee_df
str(employee_df)
# Use argument "colClass" to define specific data types for columns
employee_df <- read.csv("employee data.csv", colClasses = c("integer", "character", "character", "numeric"))
employee_df
str(employee_df)
# Use argument "colClass" and asssign column to NULL to skip specific columns
employee_df <- read.csv("employee data.csv", colClasses = c("NULL", "character", "NULL", "numeric"))
employee_df
# In some cases we might have a csv file with a number of missing values (blank,NA,X,none, etc.)
employee_df <- read.csv("employee data missing values.csv", stringsAsFactors = FALSE)
employee_df
# Use argument "na.strings" will set any specific values to be set to NA
employee_df <- read.csv("employee data missing values.csv", stringsAsFactors = FALSE, 
                        na.strings = c("X", "none", "empty"))
employee_df
#********************************************************delimited text
# Use read.table() to create a dataframe from employee data text file
employee_df <- read.delim("employee data tab delimited.txt")
employee_df
# Use the which.max() function to search for the index of the max value of some vector
highest_salary_index <- which.max(employee_df$Salary)
print(highest_salary_index)
# Index our dataframe to print out data of employee with highest salary
highest_salary_data <- employee_df[highest_salary_index,]
print(highest_salary_data)
# Use the which.min() function to search for the index of the min value of some vector
lowest_salary_index <- which.min(employee_df$Salary)
print(lowest_salary_index)
lowest_salary_data <- employee_df[lowest_salary_index,]
print(lowest_salary_data)
#*****************************************************JSON
# Install the rjson package required to read JSON files.
install.packages("rjson")
# Load the package required to read JSON files.
library("rjson")

# Create simple JSON data
simple_json <- '{"First Name":"Justin", "Last Name":"Flett", "Birth Year":1988, "Age":30}'

# Convert JSON data to a R list
simple_list <- fromJSON(simple_json)

# Print out structure of list converted from JSON data
str(simple_list)

# Import the JSON file using fromJSON
employee_data <- fromJSON(file = "employee data.json")

# Display the result.
str(employee_data)

# Convert this list into a matrix or dataframe if needed. rbind() gives us a matrix.
emploee_data_matrix <- do.call(rbind, employee_data)
class(emploee_data_matrix)
emploee_data_matrix

# Convert the matrix to data frame if needed.
emploee_data_df <- as.data.frame(emploee_data_matrix)
class(emploee_data_df)
emploee_data_df

# In the same way, we can pass a URL directly if that URL contains JSON data

# Import the JSON file using fromJSON
motor_racing_data <- fromJSON(file = 'http://ergast.com/api/f1/2004/1/results.json')

# Display the result.
str(motor_racing_data)
#*****************************************XML file
library(XML)
# Import XML file as an R dataframe or list as needed
employee_data_df <- xmlToDataFrame("employee data.xml")
employee_data_list <- xmlToList("employee data.xml")
employee_data_df
#Display same data as a list instead of a dataframe
print(employee_data_list)
# Load the `Rcurl` library if needed to help get URL with getURL() function
library(RCurl)
# In the same way, we can pass a URL directly if that URL contains XML data
# Here we use getURL from RCurl
# Import the XML URL using xmlToDataFrame
url <- getURL("https://www.w3schools.com/xml/plant_catalog.xml")
data_df <- xmlToDataFrame(url)
data_list <- xmlToList(url)
data_df
str(data_list)
