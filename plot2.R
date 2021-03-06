# program to create a plot from the "household_power_consumption.txt" file
# to solve the Course Project 1 of the Exploratory Data Analysis course
# to run it needs the "data.table" package installed
# for more info about data loading methods please look at: LargeDatasetInputMethods.R

# Calculating the approx memory requirements:
# The dataset has 2,075,259 rows and 9 columns, let's say all input is numeric
# 2,075,259x9x8 = 149418648 bytes = 142.4967 MB

# function getAndPrepareData2 reads the whole file using fread
# and applies the condition on the loaded dataset
# advantages: 
# - it is the fastest method (3 sec)
# disadvantages:
# - data.table package needs to be installed
# - cannot specify column classes and NA strings at the same time, 
# for some reason this doesn't seem to work well, in principle it should
# - need to load the whole dataset and this may kill the RAM of the machine

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

# Load the data
data <- getAndPrepareData2()

# Make the plot
if (!file.exists("plots")) dir.create(file.path("./", "plots"))

png(filename = "./plots/plot2.png", width = 480, height = 480, units = "px")
with(data, plot(datetime,Global_active_power, ylab = "Global Active Power (kilowatts)", 
     type = "l", xlab = ""))
dev.off()
print("Plot 2 is in folder 'plots' stored as 'plot2.png'")