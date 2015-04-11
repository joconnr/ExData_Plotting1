plot4 <- function(directory, file="household_power_consumption.txt") {

	## Prepare path and read file 
	pathStart <- paste(getwd(),directory,sep='/')
	fullPath <- paste(pathStart,file,sep='/')
	fullList <- read.table(fullPath, header=T, sep=";")
	## subset non-?'s
	filteredList <- subset(fullList, (fullList$Global_active_power!="?"&fullList$Voltage!="?"&fullList$Sub_metering_1!="?"&fullList$Sub_metering_1!="?"&!is.na(fullList$Sub_metering_3)))
	## subset february 1-2
	filtList <- subset(filteredList, as.Date(filteredList$Date)=="1/2/2007"|as.Date(filteredList$Date)=="2/2/2007")
	
	## Set file
	png(file="plot4.png", width=480, height=480)      
      par(mfrow=c(2,2))
      
      ## Multi-plot
	with(filtList, {
	      plot(Global_active_power, type="l", axes=FALSE, ylab="Global Active Power", xlab="")
	      ## Set the labels
	      axis(1, at=c(500,1500,2500), labels=c("Thu", "Fri", "Sat"), xpd=T, cex=0.8)       
	      axis(2, cex.axis=0.8)          
	      box()    
            
            plot(Voltage, type="l", axes=FALSE, ylab="Voltage", xlab="datetime")
	      ## Set the labels
	      axis(1, at=c(500,1500,2500), labels=c("Thu", "Fri", "Sat"), xpd=T, cex=0.8)       
	      axis(2, cex.axis=0.8)          
	      box()
            
            plot(Sub_metering_1, type="l", col="black", axes=F, ylab="Energy sub metering", xlab="")
	      ## Set the labels
	      axis(1, at=c(500,1500,2500), labels=c("Thu", "Fri", "Sat"), xpd=T, cex=0.8)       
	      axis(2, cex.axis=0.8)          
	      box()    
	      ## Plot Sub_metering_2 & 3
	      lines(Sub_metering_2, type="l", col="red")
	      lines(Sub_metering_3, type="l", col="blue")	      
	      ## Legend for sub metering types
	      legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black","red","blue"), bty="n", lty=1);
	                
	      plot(Global_reactive_power, type="l", axes=FALSE, ylab="Global_reactive_power", xlab="datetime")
	      ## Set the labels
	      axis(1, at=c(500,1500,2500), labels=c("Thu", "Fri", "Sat"), xpd=T, cex=0.8)       
	      axis(2, cex.axis=0.8)          
	      box()
	})
	dev.off()
}