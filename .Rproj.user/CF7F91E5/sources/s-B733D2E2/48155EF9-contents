rankhospital <- function(state,outcome,num="best"){
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    # Error checking
    all_states <- unique(df$State)
    if (!is.element(state,all_states)){
        stop("invalid state")
    }
    if (!is.element(outcome,c("heart attack","heart failure","pneumonia"))){
        stop("invalid outcome")
    }
    ## Extracting relevant columns
    df <- df[,c(2,7,11,17,23)]
    names(df) <- c("hospital","state","heart attack","heart failure","pneumonia")
    ## Further narrowing down the columns
    df <- df[,c("hospital","state",outcome)]
    df[,3] <- as.numeric(df[,3])
    df <- df[complete.cases(df),]
    ## State search variable
    s <- sapply(df,grep,pattern=state)
    df <- df[s$state,]
    df <- df[order(df[,outcome],df$hospital),]
    if (num == "best"){
        return (df[,1][[1]])
    }
    else if (num == "worst"){
        return (df[,1][[length(df$hospital)]])
    }
    else if (num > length(df$hospital)){
        return (NA)
    }
    df[,1][[num]]
}