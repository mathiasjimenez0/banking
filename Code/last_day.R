last_day <- function(x){
  
  d <- NULL
  for (i in 1:NROW(x)){
    
    if (x[i] %in% c(1,3,5,7,8,10,12)){
      d[i] <- 31
    } else if (x[i] == 2){
      d[i] <- 28
    } else {
      d[i] <- 30
    }
    # 
    # d <- rbind(d,aux)
    # 
  }
  
  return(d)
  
}