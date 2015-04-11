plot2 <- function(directory, file="household_power_consumption.txt") {
      
      ## Prepare path and read file 
      pathStart <- paste(getwd(),directory,sep='/')
      fullPath <- paste(pathStart,file,sep='/')
      fullList <- read.table(fullPath, header=T, sep=";")
      ## subset non-?'s
      filteredList <- subset(fullList, (fullList$Global_active_power!="?"&fullList$Date!="?"&!is.na(fullList$Sub_metering_3)))
      ## subset february 1-2
      filtList <- subset(filteredList, as.Date(filteredList$Date)=="1/2/2007"|as.Date(filteredList$Date)=="2/2/2007")
      
      ## Set file
      png(file="plot2.png", width=480, height=480)
      
      ## Plot Global active power	 
      plot(filtList$Global_active_power, type="l", axes=FALSE, ylab="Global Active Power (kilowatts)", xlab="")            
      ## Set the labels
      axis(1, at=c(500,1500,2500), labels=c("Thu", "Fri", "Sat"), xpd=T, cex=0.8)       
      axis(2, cex.axis=0.8)          
      box()
      
      dev.off()
}