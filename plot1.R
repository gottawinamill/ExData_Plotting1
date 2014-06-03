## JV - 2014-06-03
## plot1 - simple function to generate png of frequency of Global Active Power

plot1 <- function(){

	# create a class of type customDate to use when importing data
	# this class allows us to convert from character to date via setAs fn
	setClass("customDate")
	setAs("character", "customDate", function(from) as.Date(from, format="%d/%m/%Y"))

	# for plot 1 we just need the date and global active power cols 
	# so strip out everything else using colClasses to reduce memory
	DT <- read.table("household_power_consumption.txt", 
				colClasses=c("customDate", "NULL", "numeric", rep("NULL",6)),
				sep=';', header=TRUE, na.strings="?")  
	
	sub <- subset(DT, Date >= "2007-02-01" & Date <= "2007-02-02")

	DT <- NULL # free memory
	
	# create the plot
	png(file="plot1.png", width=480, height=480)
	hist(sub$Global_active_power, main="Global Active Power", xlab="Global Active Power (kilowatts)", 
		col="Red")
	dev.off()
	
}