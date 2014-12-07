# This code compares 3 functions to read a "large" csv file with a condition
# ideas on how to do this came from:
# http://stackoverflow.com/questions/23197243/how-do-i-read-only-lines-that-fulfil-a-condition-from-a-csv-into-r

# Calculating the approx memory requirements:
# The dataset has 2,075,259 rows and 9 columns, let's say all input is numeric
# 2,075,259x9x8 = 149418648 bytes = 142.4967 MB


# function getAndPrepareData1 reads the whole file using read.table
# and applies the condition on the loaded dataset
# advantages: 
# - no new packages need to be installed
# - can specify column classes and NA strings
# disadvantages:
# - need to load the whole dataset and this may kill the RAM of the machine
# - it is the slowest method (40 sec)

# function getAndPrepareData2 reads the whole file using fread
# and applies the condition on the loaded dataset
# advantages: 
# - it is the fastest method (3 sec)
# disadvantages:
# - data.table package needs to be installed
# - cannot specify column classes and NA strings at the same time, 
# for some reason this doesn't seem to work well, in principle it should
# - need to load the whole dataset and this may kill the RAM of the machine

# function getAndPrepareData3 reads only the data we want
# it uses an sql query to apply the condition before storing the data into a variable
# advantages:
# - it is the most elegant method, it only loads the data we want
# - for really large datasets it is the only way to go, cannot load them all into 
# memory
# disadvantages:
# - sqldf package needs to be installed
# - cannot specify NA values
# - it leaves an open connection to the temporary database, and ensures a warning 
# when closing it
# It is not the fastest method (25 sec)

getAndPrepareData1 <- function(filename = "./household_power_consumption.txt") {
      temp <- read.table(filename, 
                         header = T, 
                         sep = ";", 
                         nrows = -1L, 
                         na.strings = "?",
                         colClasses = c("character","character","numeric",
                                        "numeric","numeric","numeric",
                                        "numeric","numeric","numeric"))
      date1  <- "1/2/2007"
      date2  <- "2/2/2007"
      cond <- temp$Date == date1 | temp$Date == date2
      data <- temp[cond,]
      b <- paste(data$Date,data$Time)
      c <- strptime(b, format = "%d/%m/%Y %H:%M:%S")
      data <- transform(data, datetime = c)
      return(data)
}

getAndPrepareData2 <- function(filename = "./household_power_consumption.txt") {
      temp <- data.table::fread(filename, 
                                header = T, 
                                sep = ";", 
                                nrows = -1L, 
                                na.strings = "?", 
                                data.table = F,
                                colClasses = c("character","character","character",
                                               "character","character","character",
                                               "character","character","character"))
      date1  <- "1/2/2007"
      date2  <- "2/2/2007"
      cond <- temp$Date == date1 | temp$Date == date2
      data <- temp[cond,]
      for (i in 3:9){
            data[,i] <- as.numeric(data[,i])
      }
      b <- paste(data$Date,data$Time)
      c <- strptime(b, format = "%d/%m/%Y %H:%M:%S")
      data <- transform(data, datetime = c)
      return(data)
}

getAndPrepareData3 <- function(filename = "./household_power_consumption.txt") {
      data <- sqldf::read.csv.sql(file = filename,
                                   sql = "SELECT * from file WHERE Date = '1/2/2007' OR Date = '2/2/2007'",
                                   header = T, 
                                   sep = ";", 
                                   colClasses = c("character","character","numeric",
                                                  "numeric","numeric","numeric",
                                                  "numeric","numeric","numeric"), 
                                   eol = "\n")
      sqldf()
      b <- paste(data$Date,data$Time)
      c <- strptime(b, format = "%d/%m/%Y %H:%M:%S")
      data <- transform(data, datetime = c)
      return(data)
}

# > system.time(getAndPrepareData1())
# user  system elapsed 
# 40.450   0.674  41.767 

# > system.time(getAndPrepareData2())
# Read 2075259 rows and 9 (of 9) columns from 0.124 GB file in 00:00:03
# user  system elapsed 
# 3.355   0.139   3.971 

# > system.time(getAndPrepareData3())
# user  system elapsed 
# 25.149   0.355  25.575 

# We have a clear winner