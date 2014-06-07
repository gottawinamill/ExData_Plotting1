## JV - 2014-06-07
## plot 3 - function to create a time series plot of Energy Submetering by type
##          usage over the course of a number of days
##          resultant plot is outputted as png to pwd

plot3 <- function(){

	# create a class of type customDate to use when importing data
	# this class allows us to convert from character to date via setAs fn
	# which in trun will make both import and susetting more efficient
	setClass("customDate")
	setAs("character", "customDate", function(from) as.Date(from, format="%d/%m/%Y"))

	# for plot 3 we just need the date, time and sub_metering 1,2,3 cols 
	# so strip out everything else using colClasses to reduce memory
	DT <- read.table("hpc.txt", 
				colClasses=c("customDate", "character", rep("NULL",4), rep("numeric",3)),
				sep=';', header=TRUE, na.strings="?")  
	
	sub <- subset(DT, Date >= "2007-02-01" & Date <= "2007-02-02")
	
	DT <- NULL # free memory

	#create a datetime col - this is used for the x axis on timeseries plot
	sub$DateTime <- as.POSIXct(paste(sub$Date, sub$Time), format="%Y-%m-%d %H:%M:%S")
	
	# create the plot
	png(file="plot3.png", width=480, height=480)
	with(sub, plot(DateTime, Sub_metering_1, ylab="Energy sub metering",
				xlab="", type="l"))
	with(sub, lines(DateTime, Sub_metering_2, col="red"))
	with(sub, lines(DateTime, Sub_metering_3, col="blue"))
	legend("topright", lty=1,lwd=1, col=c("black", "red", "blue"), 
		legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
	dev.off()	
} 