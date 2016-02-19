

"%p%" <- function(left, right){ # Remember to add arguments!
  paste(left, right)
}




f <- function(x) {
      g <- function(y) {
        y + z  
      }
  z <- 4 
  x + g(x)
}

z<-10 ## this dont mean sheeeit
f(3) 


x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}


h <- function(x, y = NULL, d = 3L) {
  z <- cbind(x, d)
  if(!is.null(y))
    z <- z + y
  else
    z <- z + f
  g <- x + y / z
  if(d == 3L)
    return(g)
  g <- g + 10
  g
}

## created a directory function to auto set to specdata folder
directory <- function(x) {
  x=proj_dir
  setwd(x)
}

# 1

pollutantmean <- function (directory = "C:/Users/dromotsk/Dropbox/LEARNING/rProgramming/specdata", pollutant, id = 1:332) {
  files_list <- list.files(directory, full.names=TRUE)

  full_data <- data.frame()
  for (i in id) {
    #loops and binds data
    # id <- c(list.files(directory))
    full_data <- rbind(full_data, read.csv(files_list[i])) 
 
        }
  
#   data_subset <- full_data[which(full_data[, "ID"] == i), ]
  mean(full_data[ , pollutant], na.rm = TRUE )
  
  }
  
  
# 2

complete <- function (directory , id = 1:332) {
  files_list <- list.files(directory, full.names=TRUE)
  full_data <- data.frame()

    for (i in id) {
    # read file and get completed cases
    # add to the data frame
      read <- read.csv(files_list[i])
      nobs <- sum(complete.cases(read))
      build <- data.frame(i,nobs)
      full_data <- rbind(full_data, build)
    }
      colnames(full_data) <- c("id","nobs")
      full_data
  }
  

 # 3

corr <- function (directory, threshold = 0) {
  files_list <- list.files(directory, full.names=TRUE)
 
  corr_num <- numeric(0)
  
  comp_total <- complete("specdata")

  comp_filter <- comp_total[comp_total$nobs > threshold, ]
  
  
  
  # reusing #2
  for (i in 332) {
    # read file and get completed cases
    # add to the data frame
    read <- read.csv(files_list[i])
    
    if(sum(complete.cases(read)) > threshold ) {
      nobs <- sum(complete.cases(read))
      build <- data.frame(i,read[,2],read[,3],nobs)
      full_data <- rbind(full_data, build)
      }
  }
  colnames(full_data) <- c("id","sulfate", "nitrate", "nobs")
  
  corr(full_data[,2],full_data[,3])
  
}
  
  
  
  
  
    directory <- function(x) {
      proj_dir <- "C:/Users/dromotsk/Dropbox/LEARNING/rProgramming/specdata"
      x=proj_dir
    setwd(x)
  }
  
  #pollutant formula - either sulfate or nitrate
  pollutant <- c("sulfate", "nitrate")
  
  # rbind all files
  
    
  #ID formula
  id <- c(list.files(proj_dir))
  # str(file)
  # function (description = "", open = "", blocking = TRUE, encoding = getOption("encoding"),
  #          raw = FALSE)
  
    
  # return mean
  sum(pollutant[!is.na(pollutant)]) / length(pollutant[!is.na(pollutant)])


## combine files
  full_data <- data.frame()
  for (i in 1:332) {
    full_data <- rbind (full_data, read.csv(id[i]))
    
  }
  str(full_data)
