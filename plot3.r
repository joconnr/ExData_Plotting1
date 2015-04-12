plot3 <- function(directory, file="household_power_consumption.txt") {

	## Prepare path and read file 
	pathStart <- paste(getwd(),directory,sep='/')
	fullPath <- paste(pathStart,file,sep='/')
	fullList <- read.table(fullPath, header=T, sep=";")
	## subset non-?'s
      filteredList <- subset(fullList, (fullList$Sub_metering_1!="?"&fullList$Sub_metering_1!="?"&!is.na(fullList$Sub_metering_3)))
	## subset february 1-2
      filtList <- subset(filteredList, as.Date(filteredList$Date)=="1/2/2007"|as.Date(filteredList$Date)=="2/2/2007")
	
      ## Set file
	png(file="plot3.png", width=480, height=480)

      ## Plot Sub_metering_1
	plot(filtList$Sub_metering_1, type="l", col="black", axes=F, ylab="Energy sub metering", xlab="")
      
	## Plot Sub_metering_2 & 3
	lines(filtList$Sub_metering_2, type="l", col="red")
	lines(filtList$Sub_metering_3, type="l", col="blue")
	
	## Set the labels
	axis(1, at=c(20,1500,2920), labels=c("Thu", "Fri", "Sat"), xpd=T, cex=0.8)       
	axis(2, cex.axis=0.8)          
	box()
      
	## Legend for sub metering types
	legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black","red","blue"), lty=1);
	dev.off()

}