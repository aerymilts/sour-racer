pollutantmean <- function(folderdirectory, pollutant, id=1:332){
        id<-as.character(id)
        monitormeandata <- vector()
        for(i in seq_along(id)){
                if(nchar(id[[i]]) == 1){
                        id[[i]] <- paste("00", id[[i]], sep = "")
                } 
                else if(nchar(id[[i]]) == 2){
                        id[[i]] <- paste("0", id[[i]], sep = "")
                }
                else{
                }
                directory <-paste("./", folderdirectory, "/", id[[i]], ".csv", sep = "")
                data <- read.csv(directory)
                monitormeandata <- c(monitormeandata, data[[deparse(substitute(pollutant))]])
        }
        mean(monitormeandata, na.rm = TRUE)
}

complete <- function(folderdirectory, id = 1:332){
        idc<-id
        id<-vector()
        nobs<-vector()
        for(i in seq_along(idc)){
                if(nchar(idc[[i]]) == 1){
                        idc[[i]] <- paste("00", idc[[i]], sep = "")
                } 
                else if(nchar(idc[[i]]) == 2){
                        idc[[i]] <- paste("0", idc[[i]], sep = "")
                }
                else{
                }
                directory <-paste("./", folderdirectory, "/", idc[[i]], ".csv", sep = "")
                data <- read.csv(directory)
                sulfate<-table(!is.na(data$sulfate))[2] 
                nitrate<-table(!is.na(data$nitrate))[2]
                if(is.na(sulfate) | is.na(nitrate)){
                        id[i] = as.numeric(idc)[i]
                        nobs[i] = 0
                }
                else if(sulfate>nitrate){
                        id[i] = as.numeric(idc)[i]
                        nobs[i] = sum(nitrate)
                        
                }
                else{
                        id[i] = as.numeric(idc)[i]
                        nobs[i] = sum(sulfate)
                }
        }
        #print(sum(nitrate))
        #print(sum(sulfate))
        #print(as.numeric(idc))
        #print(nobs)
        cframe<-data.frame(id, nobs)
        return(cframe)
        #print(sulfate)
        #print(nitrate)
        #print(cframe)
}

pollutantnobs<-function(folderdirectory, id = 1:332){
        list.files(folderdirectory, full.names = TRUE)[id]
        
}

corr<-function(folderdirectory, threshold = 0){
        filelist<-list.files(folderdirectory, full.names = TRUE)
        corvec<-as.numeric(vector())
       
         for(i in seq_along(filelist)){
                
                df<-read.csv(filelist[i])
                thresholdcases<-sum(complete.cases(df))>threshold
                
                if (thresholdcases == TRUE){
                        datacomplete<-df[complete.cases(df), ]
                        corvec<-c(corvec, cor(datacomplete$sulfate, datacomplete$nitrate))
                } 
        }
        return(corvec)
}