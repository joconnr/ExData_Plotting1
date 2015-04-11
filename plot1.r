plot1 <- function(directory, file="household_power_consumption.txt") {

	## Prepare path and read file 
	pathStart <- paste(getwd(),directory,sep='/')
	fullPath <- paste(pathStart,file,sep='/')
	fullList <- read.table(fullPath, header=T, sep=";")  
	## subset non-?'s
      filteredList <- subset(fullList, (fullList$Global_active_power!="?"&fullList$Date!="?"&!is.na(fullList$Sub_metering_3)))
	## subset february 1-2
      filtList <- subset(filteredList, as.Date(filteredList$Date)=="1/2/2007"|as.Date(filteredList$Date)=="2/2/2007")
      
	## Set file
      png(file="plot1.png", width=480, height=480)
 
	## Histogram of Global active power
      with(filtList, hist(as.numeric(filtList$Global_active_power), col="red", ylab="Frequency", xlab="Global Active Power (kilowatts)", main="Global Active Power"))
	dev.off()
}