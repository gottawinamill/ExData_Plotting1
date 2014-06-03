## JV - 2014-06-03
## plot 2 - function to create a time series plot of Global Active Power
##          usage over the course of a number of days
##          resultant plot is outputted as png to pwd

plot2 <- function(){

	# create a class of type customDate to use when importing data
	# this class allows us to convert from character to date via setAs fn
	setClass("customDate")
	setAs("character", "customDate", function(from) as.Date(from, format="%d/%m/%Y"))

	# for plot 2 we just need the date, time and global active power cols 
	# so strip out everything else using colClasses to reduce memory
	DT <- read.table("household_power_consumption.txt", 
				colClasses=c("customDate", "character", "numeric", rep("NULL",6)),
				sep=';', header=TRUE, na.strings="?")  
	
	sub <- subset(DT, Date >= "2007-02-01" & Date <= "2007-02-02")
	
	DT <- NULL # free memory

	#create a datetime col - this is used for the x axis on timeseries plot
	sub$DateTime <- as.POSIXct(paste(sub$Date, sub$Time), format="%Y-%m-%d %H:%M:%S")
	
	# create the plot
	png(file="plot2.png", width=480, height=480)
	with(sub, plot(DateTime, Global_active_power, ylab="Global Active Power (kilowatts)",
				xlab="", type="l"))
	dev.off()	
} 