## This function will take input of the standard US Hospital compare website data
## specifically "Outcome of care Measures" and creates a histogram of 30-day morality rate

moralityRate <- function(dataset){
    ## Extracting the required column
    rate <- dataset[,11]
    ## Converting to numeric values
    rate <- as.numeric(rate)
    hist(rate,xlab = "Morality Rate", ylab = "Frequency", 
              main = "30-day Morality Rate for Heard Attacks")
}