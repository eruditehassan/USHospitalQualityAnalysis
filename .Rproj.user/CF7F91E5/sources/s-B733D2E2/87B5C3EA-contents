rankall <- function(outcome, num = "best"){
    ## Reading the data
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Extracting relevant columns
    df <- df[,c(2,7,11,17,23)]
    names(df) <- c("hospital","state","heart attack","heart failure","pneumonia")
    ## Further narrowing down the columns
    df <- df[,c("hospital","state",outcome)]
    df[,3] <- as.numeric(df[,3])
    df <- df[complete.cases(df),]
    ## Ordering by value of outcome and hospital name
    df <- df[order(df[,outcome],df$hospital),]
    ## Creating a sorted vector of all unique states
    un_states <- sort(unique(df$state))
    ## Looping over all the states and finding hospital for specified rank
    hos <- vector()
    state <- vector()
    for (st in un_states){
        s <- sapply(df,grep,pattern=st)
        temp <- df[s$state,]
        
        if (num == "best"){
            num = 1
        }
        else if (num == "worst"){
            num <- length(temp$hospital)
        }
        if (num > length(temp$hospital)){
            hos <- c(hos,NA)
            state <- c(state,st)
        }
        else {
            hos <- c(hos,temp[,1][[num]])
            state <- c(state,st)
        }
    }
    output <- data.frame(hos,state)
    names(output) <- c("hospital","state")
    output
}