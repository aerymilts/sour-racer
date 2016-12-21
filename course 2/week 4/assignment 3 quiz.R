outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcomedata[, 11] <- as.numeric(outcomedata[, 11])

library(dplyr)

best <- function(state, outcome){
        outcomes <- data.frame("heart attack" = 3, "heart failure" = 4, "pneumonia"= 5, check.names = FALSE)
        besthospital <- select(outcomedata, c(2, 7, 11, 17, 23))
        besthospital[, 4] <- suppressWarnings(as.numeric(besthospital[, 4]))
        besthospital[, 5] <- suppressWarnings(as.numeric(besthospital[, 5]))
        if(!state %in% besthospital[, 2] | !outcome %in% names(outcomes)){
                if(!state %in% besthospital[, 2]){
                        stop("invalid state")
                } else if(!outcome %in% names(outcomes)){
                        stop("invalid outcome")
                } else{
                        stop("Something is not right.")
                }
        }
        besthospital.s <- filter(besthospital, besthospital[ ,2] == state)
        bestoutcome <- min(besthospital.s[outcomes[[outcome]]], na.rm = TRUE)
        bestind <- which(besthospital.s[outcomes[[outcome]]] == bestoutcome)
        bestlist <- besthospital.s[, 1][bestind]
        return(sort(bestlist)[1])
}

rankhospital <- function(state, outcome, num = 'best'){
        outcomes <- data.frame("heart attack" = 3, "heart failure" = 4, "pneumonia"= 5, check.names = FALSE)
        nums <- c("best", "worst")
        besthospital <- select(outcomedata, c(2, 7, 11, 17, 23))
        besthospital[, 4] <- suppressWarnings(as.numeric(besthospital[, 4]))
        besthospital[, 5] <- suppressWarnings(as.numeric(besthospital[, 5]))
        if(!is.numeric(num) | !outcome %in% names(outcomes) | !state %in% besthospital[, 2]){
                if(!state %in% besthospital[, 2]){
                        stop("invalid state")
                } else if(!outcome %in% names(outcomes)){
                        stop("invalid outcome")
                } else if(!num %in% nums){
                        stop("invalid nums")
                }
        }
        besthospital.s <- filter(besthospital, besthospital[ ,2] == state)
        colnames(besthospital.s) <- 
                c("Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")
        if(is.numeric(num)){
                rankedoutcome <- arrange_(besthospital.s, as.name(outcome), "Hospital.Name")[num, 1]
        } else if(num == 'best'){
                rankedoutcome <- arrange_(besthospital.s, as.name(outcome), "Hospital.Name")[[1, 1]]
        } else if(num == 'worst'){
                besthospital.s.f <- filter(besthospital.s, complete.cases(besthospital.s))
                rankedoutcome <- arrange_(besthospital.s.f, as.name(outcome), "Hospital.Name")
                rankedoutcome <- rankedoutcome[nrow(besthospital.s.f):1, ][[1, 1]]
        }
        return(rankedoutcome)
}


rankall <- function(outcome, num = 'best'){
        outcomes <- data.frame("heart attack" = 3, "heart failure" = 4, "pneumonia"= 5, check.names = FALSE)
        nums <- c("best", "worst")
        besthospital <- select(outcomedata, c(2, 7, 11, 17, 23))
        besthospital[, 4] <- suppressWarnings(as.numeric(besthospital[, 4]))
        besthospital[, 5] <- suppressWarnings(as.numeric(besthospital[, 5]))
        #check for valid inputs
        if(!is.numeric(num) | !outcome %in% names(outcomes)){
                if(!outcome %in% names(outcomes)){
                        stop("invalid outcome")
                } else if(!num %in% nums){
                        stop("invalid nums")
                }
        }
        #stratify based on state
        besthospital.g <- group_by(besthospital, State)
        colnames(besthospital.g) <- 
                c("Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")
        #arrange by outcome, then hospital; mutate and generate rankings based on outcome stratified by state
        if(is.numeric(num)){
                rankedoutcome <- arrange_(besthospital.g, as.name(outcome), "Hospital.Name") %>% 
                        mutate(rank = row_number()) %>%
                        filter(rank == num)
        } else if(num == 'best'){
                #filter by min outcome stratified by state, returns entire rows
                rankedoutcome <- filter_(besthospital.g, 
                lazyeval::interp("var == min(var, na.rm = TRUE)", var = as.name(outcome)))
        } else if(num == 'worst'){
                #filter by max outcome stratified by state, returns entire rows
                rankedoutcome <- filter_(besthospital.g, 
                                         lazyeval::interp("var == max(var, na.rm = TRUE)", var = as.name(outcome)))
        }
        #remove dplyr tags, arrange according to state and select related columns only
        rankedoutcome <- as.data.frame(rankedoutcome) %>%
        arrange(State) %>%
        select_("Hospital.Name", "State", as.name(outcome))
        #finding the missing states to be added back since dplyr removes them
        state.vector <- unique(besthospital.g$State)
        missing <- !unique(besthospital.g$State) %in% rankedoutcome$State
        missing <- subset(state.vector, missing)
        df <- as.data.frame(matrix(ncol = 3, nrow = length(missing)))
        df[[2]] <- missing
        colnames(df) <- names(rankedoutcome)
        #bind data.frames vertically, arrange by state and change names
        rankedoutcome <- rbind(rankedoutcome, df) %>% arrange(State)
        colnames(rankedoutcome) <- c("hospital", "state")
        return(rankedoutcome[,-3])
}