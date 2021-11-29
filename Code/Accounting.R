rm(list = ls())
setwd("D:/Dropbox/Angela-Mathias")
# source("./Code/Source.R") # source to install packages if needed
load("./Dataset/R/Individual_bs.Rdata") # load individual balance sheets

# Create new dataframe with errors

df_acc <- df_all # df_all has all individual balance sheets from 1890 to 1910 in one dataframe

# prepare new df

N <- length(unique(df_acc$IDNO))
Time <- length(unique(df_acc$Date))
acc <- data.frame(Date = rep(NA,N*Time), Bank = rep(NA,N*Time), Assets = rep(NA,N*Time),
                         Total_a = rep(NA,N*Time), D_a = rep(NA,N*Time),
                         Liab = rep(NA,N*Time), Total_l = rep(NA,N*Time), D_l = rep(NA,N*Time),
                         stringsAsFactors=F)

# loop to fill df

for (i in 1:(N*Time)){
  
  x <- df_acc[i,]
  bank <- df_acc$IDNO[i]
  date <- df_acc$Date[i]
  
  a <- subset(x,select =  c("Specie","DNs","DepsDGsec","Rwysec","Domdebs","Provothdebs","CallC"
                  ,"CuloansC","LoansDG","LoansPG","Overdue","Realestate","Mortgages"
                  ,"Premises","Othera","Nchqobks","IBduefrCsec","IBdepCass",
                  "IBduefrEW","IBduefrUK","CallOSC","Secloansx","CuloansOSC",
                  "IBdepCassx","Munloans","Corploans","Call","Culoans","Overduenotesu",
                  "Overdueu","Overdues", "IBdepCassa"))

  ta <- df_acc[i,]$Totalass
  
  a <- rowSums(as.data.frame(a),na.rm = T)
  diff_a <- a - ta
  
  l <- select(x,c("Kpaid","Rest","Notes","DueDGnet","DuePG","DdC","NdC","Otherliab"
                  ,"IBduetoCsec","IBdepCliab","IBduetoEW","IBduetoUK","DOSC",
                  "DueDGd","DueDGn","DueDGsec","DuePGd","DuePGn","Dd","Nd", "IBdepCliaba", "IBdepCliabx"))
  
  tl <- rowSums(data.frame(l$Kpaid,l$Rest,df_acc[i,]$Totalliab)) 
    
  l <- rowSums(as.data.frame(l),na.rm = T)
  
  diff_l <- l - tl
  
  acc[i,1] <- date  
  acc[i,2:8] <- c(bank,a,ta,diff_a,l,tl,diff_l)

}

acc <- acc[which(!is.na(acc$Date)),] # drop banks-date empty pairs
acc$Date <- as.Date(acc$Date)

saveRDS(acc,"./errors.rds")


