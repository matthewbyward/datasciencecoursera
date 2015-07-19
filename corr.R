corr <- function(directory, threshold = 0) {
        
        totalCorrs <- numeric(0)
        
        nobsData <- complete(directory)
        nobsData <- nobsData[nobsData$nobs > threshold, ]
        
        for(id in nobsData$id) {
                
                if (id < 10) {
                        monitorCSV <- paste("00", id, sep = "")
                } else if ((id > 9) && (id < 100) ){
                        monitorCSV <- paste("0", id, sep = "")
                } else {
                        monitorCSV <- paste(id, sep = "")
                }
                
                filename <- paste(getwd(), '/', directory, '/', monitorCSV, '.csv', sep = "")
                data <- read.csv(filename, header = TRUE)
           
                totalCorrs <- c(totalCorrs, cor(data$sulfate, data$nitrate, use = "pairwise.complete.obs"))
        }
        
        totalCorrs
}