pollutantmean <- function(directory, pollutant, id = 1:332) {
        totalSum <- 0
        totalElements <- 0
        
        for(i in id) {
                if (i < 10) {
                        i <- paste("00", i, sep = "")
                } else if ((i > 9) && (i < 100) ){
                        i <- paste("0", i, sep = "")
                }
                
                filename <- paste(getwd(), '/', directory, '/', i, '.csv', sep = "")
                data <- read.csv(filename, header = TRUE)
                
                dataPoints <- nrow(na.omit(data[pollutant]))
                
                if (dataPoints > 0) {
                        totalElements <- totalElements + dataPoints
                        totalSum <- totalSum + sum(data[pollutant], na.rm = TRUE)
                }
        }
        
        totalSum / totalElements
}