#set english as locale
Sys.setlocale(category = "LC_ALL", locale = "english")

#' this function read the data set and parse it to data frame format
#' @param fileName the name of file that contains the data set, as default 'household_power_consumption'
#' @param from the date form start the read, as default '2007-02-01'
#' @param to the date to end the read '2007-02-02
read_data <- function(fileName ="household_power_consumption", from = "2007-02-01", to ="2007-02-02"){
  
  library(lubridate)
  
  print("Reading file")
  data <- read.table(paste0(fileName,".txt"), sep = ";", header = TRUE,
             colClasses = c("character","character","double","double","double","double","double","double","double"),
             na.strings = c("?"),  stringsAsFactors = FALSE)
  
  print("Formatting date and time")
  data$Date = parse_date_time(data$Date, orders = c("ymd", "dmy", "mdy"))
  data$Time = as.POSIXct(paste(data$Date, data$Time), format="%Y-%m-%d %H:%M:%S")
  
  print("Subsetting, by default: from the dates 2007-02-01 and 2007-02-02")
  df <- data[data$Date >= as.Date(from) & data$Date <= as.Date(to),]
  
  print("Finished")
  
  return(df)
}

#' this function draw the plot1
#' @param data the data set 
#' @param fileName the name for the png file, as default 'plot1'
#' @param saveAsPng if is TRUE save the plot on a png file else only show the plot, as default FALSE
#' @param width the height of the png file, as default 480px
#' @param height the width of the png file, as default 480px
drawPlot1 <- function(data, fileName="plot1", saveAsPng = FALSE, width = 480, height = 480){
  
  if(saveAsPng){
    png(paste0(fileName,".png"), width, height)
  }
  
  hist(data$Global_active_power,col="green", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
  
  if(saveAsPng){
    dev.off()
  }
}
#' this function draw the plot2
#' @param data the data set 
#' @param fileName the name for the png file, as default 'plot2'
#' @param saveAsPng if is TRUE save the plot on a png file else only show the plot, as default FALSE
#' @param width the height of the png file, as default 480px
#' @param height the width of the png file, as default 480px
drawPlot2 <- function(data,  fileName="plot2", saveAsPng = FALSE, width = 480, height = 480){
  if(saveAsPng){
    png(paste0(fileName,".png"), width, height)
  }
  plot(data$Time, data$Global_active_power, type = "l", ylab = "Global Active Power (kilowatts)", xlab = "")

  if(saveAsPng){
    dev.off()
  }
}
#' this function draw the plot3
#' @param data the data set 
#' @param fileName the name for the png file, as default 'plot3'
#' @param saveAsPng if is TRUE save the plot on a png file else only show the plot, as default FALSE
#' @param width the height of the png file, as default 480px
#' @param height the width of the png file, as default 480px
drawPlot3 <- function(data, fileName="plot3", saveAsPng = FALSE, width = 480, height = 480){
  if(saveAsPng){
    png(paste0(fileName,".png"), width, height)
  }
  plot(data$Time, data$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering") 
  lines(data$Time, data$Sub_metering_2, type = "l",col="2") 
  lines(data$Time, data$Sub_metering_3, type = "l",col="3") 
  legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=1:3, lty=1, cex=0.8) 
  
  if(saveAsPng){
    dev.off()
  }
}
#' this function draw the plot4
#' @param data the data set 
#' @param fileName the name for the png file, as default 'plot4'
#' @param saveAsPng if is TRUE save the plot on a png file else only show the plot, as default FALSE
#' @param width the height of the png file, as default 480px
#' @param height the width of the png file, as default 480px
drawPlot4 <- function(data, fileName="plot4", saveAsPng = FALSE, width = 480, height = 480){
  
  if(saveAsPng){
    png(paste0(fileName,".png"), width, height)
  }
  
  par(mfrow = c(2,2), mar = c(4, 4, 2, 1))
  drawPlot2(data)
  plot(data$Time, data$Voltage, type = "l", ylab = "Voltage", xlab = "datetime")
  drawPlot3(data)
  plot(data$Time, data$Global_reactive_power, type = "l", ylab = "Global Reactive Power", xlab = "datetime")
  
  if(saveAsPng){
    dev.off()
  }
}
