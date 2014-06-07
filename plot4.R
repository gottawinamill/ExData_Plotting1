## JV - 2014-06-07
## plot 4 - function to create 4 plots in one graph
##          organised in a 2x2 matrix

plot4 <- function(){

	# create a class of type customDate to use when importing data
	# this class allows us to convert from character to date via setAs fn
	# which in trun will make both import and susetting more efficient
	setClass("customDate")
	setAs("character", "customDate", function(from) as.Date(from, format="%d/%m/%Y"))

	# import required fields, exclude others to reduce memory and increase speed
	DT <- read.table("household_power_consumption.txt", 
				colClasses=c("customDate", "character",rep("numeric",3),
							"NULL", rep("numeric",3)),
				sep=';', header=TRUE, na.strings="?")  
	
	sub <- subset(DT, Date >= "2007-02-01" & Date <= "2007-02-02")
	
	DT <- NULL # free memory

	#create a datetime col - this is used for the x axis on timeseries plot
	sub$DateTime <- as.POSIXct(paste(sub$Date, sub$Time), format="%Y-%m-%d %H:%M:%S")
	
	# create the plot
	png(file="plot4.png", width=480, height=480)
	
	par(mfrow=c(2,2))
	plot(sub$DateTime, sub$Global_active_power, ylab="Global Active Power", xlab="", type="l")
	plot(sub$DateTime, sub$Voltage, ylab="Voltage",xlab="datetime", type="l")
	
	createSubMeter(sub) # use a helper fn to create submeter plot for readability
	plot(sub$DateTime, sub$Global_reactive_power, ylab="Global_reactive_power",xlab="datetime", type="l")

	dev.off()	
} 


# helper function to create submeter combined plots 
createSubMeter <- function(data = data.frame()){
	x <- with(data, plot(DateTime, Sub_metering_1, ylab="Energy sub metering",
				xlab="", type="l"))
	     with(data, lines(DateTime, Sub_metering_2, col="red"))
	     with(data, lines(DateTime, Sub_metering_3, col="blue"))
	     legend("topright",bty="n", lty=1,lwd=1, col=c("black", "red", "blue"), 
		      legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
	return(x)

}