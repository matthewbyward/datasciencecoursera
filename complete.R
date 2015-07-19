complete <- function(directory, id = 1:332) {
        ids <- vector("numeric", length = length(id))
        cases <- vector("numeric", length = length(id))
        for(i in 1:length(id)) {
                
                ids[i] <- id[i]
                
                if (id[i] < 10) {
                        monitorCSV <- paste("00", id[i], sep = "")
                } else if ((id[i] > 9) && (id[i] < 100) ){
                        monitorCSV <- paste("0", id[i], sep = "")
                } else {
                        monitorCSV <- paste(id[i], sep = "")
                }
                
                filename <- paste(getwd(), '/', directory, '/', monitorCSV, '.csv', sep = "")
                data <- read.csv(filename, header = TRUE)
                
                cases[i] <- sum(complete.cases(data))
        }
        
        data.frame(id = ids, nobs = cases)
}