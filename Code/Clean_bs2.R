# New, not from Angela, so they don't come with IDNO ##### 


df_50s_newbr <- NULL
filename <- paste0("./Dataset/balance_sheets/NB early.xlsx")

for (t in c(1857,1859,1860,1862,1864,1866,1867)){
  
  df_aux <- read_excel(filename, sheet = as.character(t), skip = 1)
  
  if (t <= 1859){
    
    df_aux[,-which(colnames(df_aux)  == "Bank")] <- df_aux[,-which(colnames(df_aux) == "Bank")]*4
 
  }

  df_aux <- df_aux %>% select(-netpi)
  
  df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_50s$`1857_NB`) - NCOL(df_aux), nrow = NROW(df_aux))))
  colnames(df_aux) <- ref_50s$`1857_NB`
  df_aux$Div <- df_aux$Div/df_aux$Kpaid*100
  
  df_aux$Date <- rep(as.Date(paste0(t,"-12-31")),nrow(df_aux))
  
  
  
  df_50s_newbr <- rbind(df_50s_newbr,df_aux)
  
  
}




df_50s <- NULL

for (t in c(1857,1859,1860,1862,1864,1866,1867:1871)){
  
  if (t <= 1860){
    
    filename <- paste0("./Dataset/balance_sheets/March ",t,".xlsx") 
    df_aux <- read_excel(filename, sheet = 1, skip = 4)
    
    df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_50s$`1857`) - NCOL(df_aux), nrow = NROW(df_aux))))
    colnames(df_aux) <- ref_50s$`1857`
    
    df_aux$Date <- rep(as.Date(paste0(t,"-03-31")),nrow(df_aux))
    
  } else if (t %in% c(1868,1869,1871)){
    
    filename <- paste0("./Dataset/balance_sheets/April ",t,".xlsx") 
    df_aux <- read_excel(filename, sheet = 1, skip = 1)
    
    df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_50s$`1862`) - NCOL(df_aux), nrow = NROW(df_aux))))
    colnames(df_aux) <- ref_50s$`1862`
    
    df_aux$Date <- rep(as.Date(paste0(t,"-04-30")),nrow(df_aux))
    
    
  } else {
    
    filename <- paste0("./Dataset/balance_sheets/March ",t,".xlsx") 
    df_aux <- read_excel(filename, sheet = 1, skip = 4)
    
    df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_50s$`1862`) - NCOL(df_aux), nrow = NROW(df_aux))))
    colnames(df_aux) <- ref_50s$`1862`  
    
    df_aux$Date <- rep(as.Date(paste0(t,"-03-31")),nrow(df_aux))
    
  }
  
  
  df_aux <- df_aux[which(!str_detect(df_aux$Bank,"TARIO")),]
  df_aux <- df_aux[which(!str_detect(df_aux$Bank,"QUEBEC")),]
  df_aux <- df_aux[which(!str_detect(df_aux$Bank,"NOVA")),]
  df_aux <- df_aux[which(!str_detect(df_aux$Bank,"NEW")),]
  df_aux <- df_aux[which(!str_detect(df_aux$Bank,"COLUMBIA")),]
  df_aux <- df_aux[which(!str_detect(df_aux$Bank,"ISLAND")),]
  df_aux <- df_aux[which(!str_detect(df_aux$Bank,"TOTAL")),]
  df_aux <- df_aux[which(!str_detect(df_aux$Bank,"Total")),]
  df_aux <- df_aux[which(!str_detect(df_aux$Bank,"total")),]
  
  
  df_50s <- rbind(df_50s,df_aux)
  
  
}




df_70s <- NULL

for (t in c(1874,1876,1878,1880,1881)){
    
  if (t <= 1880){
    
    filename <- paste0("./Dataset/balance_sheets/March ",t,".xlsx") 
    df_aux <- read_excel(filename, sheet = 1, skip = 2)
    
    df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_70s$`1874`) - NCOL(df_aux), nrow = NROW(df_aux))))
    colnames(df_aux) <- ref_70s$`1874`
    
    df_aux$Date <- rep(as.Date(paste0(t,"-03-31")),nrow(df_aux))
    
  } else{
    
    filename <- paste0("./Dataset/balance_sheets/April ",t,".xlsx") 
    df_aux <- read_excel(filename, sheet = 1, skip = 2)
    
    df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_70s$`1881`) - NCOL(df_aux), nrow = NROW(df_aux))))
    colnames(df_aux) <- ref_70s$`1881`
    
    df_aux$Date <- rep(as.Date(paste0(t,"-04-30")),nrow(df_aux))
    
  }
  
    
    df_aux <- df_aux[which(!str_detect(df_aux$Bank,"TARIO")),]
    df_aux <- df_aux[which(!str_detect(df_aux$Bank,"QUEBEC")),]
    df_aux <- df_aux[which(!str_detect(df_aux$Bank,"NOVA")),]
    df_aux <- df_aux[which(!str_detect(df_aux$Bank,"NEW")),]
    df_aux <- df_aux[which(!str_detect(df_aux$Bank,"COLUMBIA")),]
    df_aux <- df_aux[which(!str_detect(df_aux$Bank,"ISLAND")),]
    df_aux <- df_aux[which(!str_detect(df_aux$Bank,"TOTAL")),]
    df_aux <- df_aux[which(!str_detect(df_aux$Bank,"Total")),]
    df_aux <- df_aux[which(!str_detect(df_aux$Bank,"total")),]
    df_aux <- df_aux[which(!str_detect(df_aux$Bank,"RECAPITULATION")),]
    
    df_70s <- rbind(df_70s,df_aux)
    
  
}


df_80s <- NULL
filename <- "./Dataset/balance_sheets/Balance Sheet - 1882-1889.xlsx"

for (t in 1882:1889){
  
  df_aux <- read_excel(filename, sheet = as.character(t))
  
  if (t <= 1883){
  
    df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_70s$`1881`) - NCOL(df_aux), nrow = NROW(df_aux))))
    colnames(df_aux) <- ref_70s$`1881`
    
  } else{
    
    df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_70s$`1884`) - NCOL(df_aux), nrow = NROW(df_aux))))
    colnames(df_aux) <- ref_70s$`1884`
    
  }
  

  df_aux$Date <- rep(as.Date(paste0(t,"-03-31")),nrow(df_aux))
  
  df_80s <- rbind(df_80s,df_aux)
  
}


df_11s <- NULL
filename <- "./Dataset/balance_sheets/Balance Sheet - 1911-1916.xlsx"

for (t in c(1911,1912,1914,1915,1916)){
  
  df_aux <- read_excel(filename, sheet = paste0("BANK BALANCE SHEET - March ",t))
  
  if (t <= 1913){
    
    df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_09$`1911`) - NCOL(df_aux), nrow = NROW(df_aux))))
    colnames(df_aux) <- ref_09$`1911`
    
  } else{
    
    df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_13$`1913_07`) - NCOL(df_aux), nrow = NROW(df_aux))))
    colnames(df_aux) <- ref_13$`1913_07`
    
  }
  
  
  df_aux$Date <- rep(as.Date(paste0(t,"-03-31")),nrow(df_aux))
  
  df_11s <- rbind(df_11s,df_aux)
  
}



df_00s <- NULL

for (t in c(1899:1906,1909:1910,1913)){
  
  if (t == 1899){
    
    
    filename <- paste0("./Dataset/balance_sheets/Balance Sheet - ",1900,".xlsx") 
    df_aux <- read_excel(filename, sheet = 1, skip = 1)
    
    df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_00s$`1899`) - NCOL(df_aux), nrow = NROW(df_aux))))
    colnames(df_aux) <- ref_00s$`1899`
    
    df_aux <- df_aux[which(!str_detect(df_aux$Bank,"TARIO")),]
    df_aux <- df_aux[which(!str_detect(df_aux$Bank,"QUEBEC")),]
    df_aux <- df_aux[which(!str_detect(df_aux$Bank,"NOVA")),]
    df_aux <- df_aux[which(!str_detect(df_aux$Bank,"NEW")),]
    df_aux <- df_aux[which(!str_detect(df_aux$Bank,"COLUMBIA")),]
    df_aux <- df_aux[which(!str_detect(df_aux$Bank,"ISLAND")),]
    
    df_aux$Date <- rep(as.Date(paste(t,12,last_day(12), sep = "-")),nrow(df_aux))
    
    df_00s <- rbind(df_00s,df_aux)
    
  } else if (t == 1900){
    
    
    for (i in 2:7){
      
      filename <- paste0("./Dataset/balance_sheets/Balance Sheet - ",1900,".xlsx")
      df_aux <- read_excel(filename, sheet = i, skip = 1)
      df_aux <- df_aux[-c(which(df_aux[[1]] %in% c("Grand Total","Total")):NROW(df_aux)),]
      
      df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_00s$`1899`) - NCOL(df_aux), nrow = NROW(df_aux))))
      colnames(df_aux) <- ref_00s$`1899`
      
      df_aux <- df_aux[which(!str_detect(df_aux$Bank,"TARIO")),]
      df_aux <- df_aux[which(!str_detect(df_aux$Bank,"QUEBEC")),]
      df_aux <- df_aux[which(!str_detect(df_aux$Bank,"NOVA")),]
      df_aux <- df_aux[which(!str_detect(df_aux$Bank,"NEW")),]
      df_aux <- df_aux[which(!str_detect(df_aux$Bank,"COLUMBIA")),]
      df_aux <- df_aux[which(!str_detect(df_aux$Bank,"ISLAND")),]
      
      df_aux$Date <- rep(as.Date(paste(t,i-1,last_day(i-1), sep = "-")),nrow(df_aux))
      
      df_00s <- rbind(df_00s,df_aux)
      
    }  
    
    for (i in 8:13){
      
      filename <- paste0("./Dataset/balance_sheets/Balance Sheet - ",1900,".xlsx")
      df_aux <- read_excel(filename, sheet = i, skip = 1)
      df_aux <- df_aux[-c(which(df_aux[[1]] %in% c("Grand Total","Total")):NROW(df_aux)),]
      
      df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_00s$`1900_07`) - NCOL(df_aux), nrow = NROW(df_aux))))
      colnames(df_aux) <- ref_00s$`1900_07`
      
      df_aux <- df_aux[which(!str_detect(df_aux$Bank,"TARIO")),]
      df_aux <- df_aux[which(!str_detect(df_aux$Bank,"QUEBEC")),]
      df_aux <- df_aux[which(!str_detect(df_aux$Bank,"NOVA")),]
      df_aux <- df_aux[which(!str_detect(df_aux$Bank,"NEW")),]
      df_aux <- df_aux[which(!str_detect(df_aux$Bank,"COLUMBIA")),]
      df_aux <- df_aux[which(!str_detect(df_aux$Bank,"ISLAND")),]
      
      df_aux$Date <- rep(as.Date(paste(t,i-1,last_day(i-1), sep = "-")),nrow(df_aux))
      
      df_00s <- rbind(df_00s,df_aux)
      
    }  
    
  } else{
    
    
    for (i in 1:12){
      
      if (t %in% c(1905,1906,1910,1913) & i == 12){
        next
      } else {
        
        filename <- paste0("./Dataset/balance_sheets/Balance Sheet - ",t,".xlsx")
        if (t %in% c(1906)){
          df_aux <- read_excel(filename, sheet = i)  
        } else if (t %in% c(1913) & i %in% 7:11){
          df_aux <- read_excel(filename, sheet = i, skip = 2)
        } else{
          df_aux <- read_excel(filename, sheet = i, skip = 1)
        }
        
        df_aux <- df_aux[-c(which(df_aux[[1]] %in% c("Grand Total","Total")):NROW(df_aux)),]
        
        if (t %in% c(1909,1910)){
          df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_09$`1909`) - NCOL(df_aux), nrow = NROW(df_aux))))
          colnames(df_aux) <- ref_09$`1909`      
        } else if (t == 1906 & i %in% 1:7){
          df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_06$`1906_1`) - NCOL(df_aux), nrow = NROW(df_aux))))
          colnames(df_aux) <- ref_06$`1906_1`
        } else if (t == 1906 & i %in% 8){
          df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_06$`1906_2`) - NCOL(df_aux), nrow = NROW(df_aux))))
          colnames(df_aux) <- ref_06$`1906_2`
        } else if (t == 1906 & i %in% 9:11){
          df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_06$`1906_3`) - NCOL(df_aux), nrow = NROW(df_aux))))
          colnames(df_aux) <- ref_06$`1906_3`
        } else if (t == 1913 & i %in% 1:6){
          df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_09$`1911`) - NCOL(df_aux), nrow = NROW(df_aux))))
          colnames(df_aux) <- ref_09$`1911`
        } else if (t == 1913 & i %in% 7:11){
          df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_13$`1913_07`) - NCOL(df_aux), nrow = NROW(df_aux))))
          colnames(df_aux) <- ref_13$`1913_07`
        } else{
          df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = NROW(ref_00s$`1900_07`) - NCOL(df_aux), nrow = NROW(df_aux))))
          colnames(df_aux) <- ref_00s$`1900_07`  
        }
        
        df_aux <- df_aux[which(!str_detect(df_aux$Bank,"TARIO")),]
        df_aux <- df_aux[which(!str_detect(df_aux$Bank,"QUEBEC")),]
        df_aux <- df_aux[which(!str_detect(df_aux$Bank,"NOVA")),]
        df_aux <- df_aux[which(!str_detect(df_aux$Bank,"NEW")),]
        df_aux <- df_aux[which(!str_detect(df_aux$Bank,"COLUMBIA")),]
        df_aux <- df_aux[which(!str_detect(df_aux$Bank,"ISLAND")),]
        
        df_aux$Date <- rep(as.Date(paste(t,i,last_day(i), sep = "-")),nrow(df_aux))
        
        df_00s <- rbind(df_00s,df_aux)
        
        
        }
    }
  }
}


df_aux <- rbind(df_50s_newbr,df_50s,df_70s,df_80s,df_00s,df_11s)

df_aux <- df_aux[-which(df_aux[[1]] %in% c("Grand Total","Total")),]

df_aux[df_aux %in% c("NIL","Nil")] <- NA
df_aux[,-which(colnames(df_aux) %in% c("Bank","Date"))] <- lapply(df_aux[,-which(colnames(df_aux) %in% c("Bank","Date"))], function(x) {
  as.numeric(as.character(x))
}) # Transform all columns but Bank and Date into numeric
df_aux$Bank <- stri_trans_general(df_aux$Bank ,"Latin-ASCII")



df_aux$Bank <- gsub("\\.","",df_aux$Bank)
df_aux$Bank <- gsub("\\*","",df_aux$Bank)
df_aux$Bank <- gsub("-"," ",df_aux$Bank)
df_aux$Bank <- gsub("  "," ",df_aux$Bank)
df_aux$Bank <- gsub("Bk","Bank",df_aux$Bank)
df_aux$Bank <- gsub("Bank","Bank",df_aux$Bank)
df_aux$Bank <- gsub("Canda","Canada",df_aux$Bank)
df_aux$Bank <- gsub("Batik","Bank",df_aux$Bank)
df_aux$Bank <- gsub("Co ","Company",df_aux$Bank)
df_aux$Bank <- gsub("Co$","Company",df_aux$Bank)
df_aux$Bank <- gsub("B N A","BNA",df_aux$Bank)
df_aux$Bank <- gsub("P E I","PEI",df_aux$Bank)
df_aux$Bank <- gsub("Prince Edward Island","PEI",df_aux$Bank)
df_aux$Bank <- gsub("BN America","BNA",df_aux$Bank)
df_aux$Bank <- gsub("BNA","British North America",df_aux$Bank)
df_aux$Bank <- gsub("N America","North America",df_aux$Bank)
df_aux$Bank <- gsub("American","America",df_aux$Bank)
df_aux$Bank <- gsub("Amrerica","America",df_aux$Bank)
df_aux$Bank <- gsub("Mechanics'","Mechanics",df_aux$Bank)
df_aux$Bank <- gsub("Mechanic's","Mechanics",df_aux$Bank)
df_aux$Bank <- gsub("Molson's","Molsons",df_aux$Bank)
df_aux$Bank <- gsub("LC","Lower Canada",df_aux$Bank)
df_aux$Bank[str_detect(df_aux$Bank,"Provincial")] <- "Banque Provinciale du Canada"
df_aux$Bank[which(df_aux$Bank == "People's Bank")] <- "People's Bank of New Brunswick"
df_aux$Bank[which(df_aux$Bank == "People's Bank")] <- "People's Bank of New Brunswick"
df_aux$Bank[which(df_aux$Bank == "Standard Bank")] <- "Standard Bank of Canada"
df_aux$Bank[which(df_aux$Bank == "La Banque d'Hochelaga")] <- "Banque d'Hochelaga" 
df_aux$Bank[which(df_aux$Bank == "La Banque d'Hochelaga")] <- "Banque d'Hochelaga"
df_aux$Bank[which(df_aux$Bank == "The Summerside Bank")] <- "Summerside Bank"
df_aux$Bank[which(df_aux$Bank == "The Merchants Bank of PEI")] <- "Merchants Bank of PEI"
df_aux$Bank <- gsub("Maritime Bank.*$","Maritime Bank of Canada",df_aux$Bank)
df_aux$Bank <- gsub("Canadian Bank of Commerce.*$","Canadian Bank of Commerce",df_aux$Bank)
df_aux$Bank[which(df_aux$Bank == "Northern Crown Bank")] <- "Northern Crown"
df_aux$Bank[which(df_aux$Bank == "Traders Bank of Canada")] <- "Trader's Bank of Canada"
df_aux$Bank[which(df_aux$Bank == "Imperial Bank")] <- "Imperial Bank of Canada"
df_aux$Bank[which(df_aux$Bank == "Commercial Bank Windsor")] <- "Commercial Bank of Windsor"
df_aux$Bank[which(df_aux$Bank == "Exchange Bank Yarmouth")] <- "Exchange Bank of Yarmouth"
df_aux$Bank <- gsub("Coompany","Company",df_aux$Bank)
df_aux$Bank <- gsub("Stephens","Stephen's",df_aux$Bank)
# df_aux$Bank[which(df_aux$Bank == "Union Bank")] <- "Union Bank of Canada"
df_aux$Bank <- gsub("Merchants'","Merchants",df_aux$Bank)
df_aux$Bank <- gsub("Merchant's","Merchants",df_aux$Bank)
df_aux$Bank[which(df_aux$Bank == "East Townships Bank")] <- "Eastern Townships Bank"
df_aux$Bank[which(df_aux$Bank == "Merch Bank of Halifax")] <- "Merchants Bank of Halifax"
df_aux$Bank[which(df_aux$Bank == "Metropolitian Bank")] <- "Metropolitan Bank"
df_aux$Bank[which(df_aux$Bank == "Sovereigh Bank of Canada")] <- "Sovereign Bank of Canada"
df_aux$Bank[which(df_aux$Bank == "Stand Bank of Canada")] <- "Standard Bank of Canada"
df_aux$Bank[which(df_aux$Bank == "British Columbia")] <- "Bank of British Columbia"
df_aux$Bank[which(df_aux$Bank == "City Bank of Montreal")] <- "City Bank"
df_aux$Bank[which(df_aux$Bank == "Commercial Bank of Ca")] <- "Commercial Bank of Canada"
df_aux$Bank[which(df_aux$Bank == "Commercial Bank")] <- "Commercial Bank of Canada"
df_aux$Bank[which(df_aux$Bank == "Eastern Townships B")] <- "Eastern Townships Bank"
df_aux$Bank[which(df_aux$Bank == "Union Bank of Lower Canada")] <- "Union Bank of Canada"

# Union Bank of Halifax and Union Bank of PEI also appear sometimes as Union Bank. I changed manually names so that Union Bank only refers to Union Bank of Canada.
# Therefore, whenever I add more balance sheets I need to change this manually as well.
df_aux$Bank[which(df_aux$Bank == "Union Bank")] <- "Union Bank of Canada" 

df_aux$Bank[which(df_aux$Bank == "Merchants Bank")] <- "Merchants Bank of Canada"
# df_aux$Bank[which(df_aux$Bank == "Metropolitan Bank" & year(df_aux$Date) <= 1976)] <- "Metropolitan Bank of Montreal"
df_aux$Bank[which(df_aux$Bank == "Merchants Bank of Halifax")] <- "Royal Bank of Canada"


df_aux$IDNO <- var_lab_in$IDNO[amatch(tolower(df_aux$Bank),tolower(var_lab_in$Bank),maxDist = 0.1)]
df_aux <- df_aux %>% filter(!(month(Date) == 3 & year(Date) >= 1890))


df_all1 <- df_aux


# From Angela, so they already have IDNO ###### 




df_17s <- NULL

for (t in c(17:25)){ 
  # for (t in c(17:25,32,33)){
  
  filename <- paste0("./Dataset/balance_sheets/rmar",t,".xlsx") 
  df_aux <- read_excel(filename, sheet = 1)
    
  df_aux <- rename(df_aux,"Specie" = "Sp")
  n <- NROW(df_aux)
  c <- colnames(df_aux)
  c2 <- ref_70s$`1884`[-which((ref_70s$`1884` %in% c) | (ref_70s$`1884` == "Bank"))]
  df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = length(c2), nrow = n)))
  
  colnames(df_aux) <- c(c,c2)  
  df_aux$Date <- rep(as.Date(paste0(19,t,"-03-31")),nrow(df_aux))
  
  df_17s <- rbind(df_17s,df_aux)
  
}



df_90s <- NULL

for (t in c(1890:1899)){
  
  filename <- paste0("./Dataset/balance_sheets/rmar",t,"cc.xlsx") 
  df_aux <- read_excel(filename, sheet = 1)
  
  n <- NROW(df_aux)
  c <- colnames(df_aux)
  c2 <- ref_70s$`1884`[-which((ref_70s$`1884` %in% c) | (ref_70s$`1884` == "Bank"))]
  df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = length(c2), nrow = n)))
  colnames(df_aux) <- c(c,c2)  
  
  df_aux$Date <- rep(as.Date(paste0(t,"-03-31")),nrow(df_aux))

  df_90s <- rbind(df_90s,df_aux)
  
}

# 22 is Central Bank now which existed between 1883 and 1887. 
# Angela assigned 22 to Merchants Halifax which in fact is the same Bank as 49
df_90s$IDNO[which(df_90s$IDNO == 22)] <- 49 


df_0708 <- NULL
i <- 1

for (m in month.abb){
  for (t in 1907:1908){
    
    filename <- paste0("./Dataset/balance_sheets/r",tolower(m),t,"cc.xlsx") 
    df_aux <- read_excel(filename, sheet = 1)
    
    n <- NROW(df_aux)
    c <- colnames(df_aux)
    c2 <- ref_70s$`1884`[-which((ref_70s$`1884` %in% c) | (ref_70s$`1884` == "Bank"))]
    df_aux <- cbind(df_aux,as.data.frame(matrix(NA, ncol = length(c2), nrow = n)))
    colnames(df_aux) <- c(c,c2)  
    
    
    df_aux$Date <- rep(as.Date(paste(t,i,last_day(i), sep = "-")),nrow(df_aux))
    
    df_0708 <- rbind(df_0708,df_aux) 
  
  }
  i <- i + 1
}

# The other Metro existed between 1867 and 1876 only
df_0708$IDNO[which(df_0708$IDNO == 51)] <- 32 



df_aux2 <- rbind(df_17s,df_90s,df_0708)
df_aux2 <- df_aux2[,-which(colnames(df_aux2) == "Rowno")]

var_aux <- var_lab_in %>% filter(!(Bank %in% c("Merchants Bank of Halifax")))
df_aux2 <- left_join(df_aux2,var_aux[,c(2,3)],by = c("IDNO"))
df_aux2 <- rename(df_aux2, "Bank" = "Bank")

df_aux2[df_aux2 %in% c("NIL","Nil")] <- NA
df_aux2[,-which(colnames(df_aux2) %in% c("Bank","Date"))] <- lapply(df_aux2[,-which(colnames(df_aux2) %in% c("Bank","Date"))], function(x) {
  as.numeric(as.character(x))
}) # Transform all columns but Bank and Date into numeric

df_all2 <- df_aux2



# Merge ####
df_all <- rbind(df_all1,df_all2)
df_all <- df_all[,order(colnames(df_all))]
df_all$year <- year(df_all$Date)
df_all$month <- month(df_all$Date)

df_all <- left_join(df_all,var_lab_in[,which(colnames(var_lab_in) %in% c("Bank","bank"))], by = "Bank")
df_all <- df_all %>% select(-Bank)

df_all <- df_all %>% filter(!is.na(Totalass) & !is.na(IDNO))
df_all <- df_all %>% filter(!is.na(Totalass) & !is.na(IDNO))
df_all <- df_all %>% filter(!is.na(Totalass) & !is.na(IDNO))
df_all <- df_all %>% replace(is.na(.), 0) %>% mutate(Provothdebs = Provothdebs + Secloansx)
df_all <- df_all %>% replace(is.na(.), 0) %>% mutate(Provothdebs = Provothdebs + Secloansx)
df_all <- df_all %>% replace(is.na(.), 0) %>% mutate(Provothdebs = Provothdebs + Secloansx)
df_all$Secloansx <- 0

# Clean turnover ####

turnover$t_Date <- NA
for (i in 1:NROW(turnover)){
  
  if (turnover$turnover[i] %in% c(1.1,0.1,0.2,0.3,0.4,0.5)){
    
    turnover$t_Date[i] <- as.Date(paste(turnover$year[i],turnover$month[i],last_day(turnover$month[i]), sep = "-"))
    
  }
  
}
turnover$t_Date <- as.Date(turnover$t_Date)

# Entry and exit definition ####

gap_after <- 365/5
gap_before <- 365

for (i in unique(df_all$IDNO)){
  
  aux <- turnover %>% filter(IDNO == i)
  aux2 <- df_all %>% filter(IDNO == i)
  events <- c(1.1,0.1,0.2,0.3,0.4,0.5) %in% aux$turnover
  l <- sum(events)
  
  if (l == 2){
    
    for (t in aux2$Date){
      
      if (t > aux$t_Date[which(aux$turnover == 1.1)] &
          t < aux$t_Date[which(aux$turnover %in% c(0.1,0.2,0.3,0.4,0.5))] - gap_before){
        
        df_all$turnover[which(df_all$IDNO == i & df_all$Date == t)] <- 1
        
        
      } else if (t < aux$t_Date[which(aux$turnover == 1.1)] |
                 t > aux$t_Date[which(aux$turnover %in% c(0.1,0.2,0.3,0.4,0.5))] + gap_after){
        
        df_all$turnover[which(df_all$IDNO == i & df_all$Date == t)] <- 0
        
      } else{
        
        df_all$turnover[which(df_all$IDNO == i & df_all$Date == t)] <- aux$turnover[which(aux$turnover %in% c(0.1,0.2,0.3,0.4,0.5))]
        
      } 
    }
    
    
  } else if (l == 0){
    
    
    df_all$turnover[which(df_all$IDNO == i)] <- 1
    
  } else if (l == 1 & events[1] == 1){
    
    
    for (t in aux2$Date){
      
      if (t > aux$t_Date[which(aux$turnover == 1.1)]){
        
        df_all$turnover[which(df_all$IDNO == i & df_all$Date == t)] <- 1
        
        
      } else if (t < aux$t_Date[which(aux$turnover == 1.1)]){
        
        df_all$turnover[which(df_all$IDNO == i & df_all$Date == t)] <- 0
        
      }
    }
    
    
  } else if (l == 1 & events[1] == 0){
    
    
    for (t in aux2$Date){
      
      if (t > aux$t_Date[which(aux$turnover %in% c(0.1,0.2,0.3,0.4,0.5))] + gap_after){
        
        df_all$turnover[which(df_all$IDNO == i & df_all$Date == t)] <- 0
        
        
      } else if (t < aux$t_Date[which(aux$turnover %in% c(0.1,0.2,0.3,0.4,0.5))] - gap_before){
        
        df_all$turnover[which(df_all$IDNO == i & df_all$Date == t)] <- 1
        
      } else{
        
        df_all$turnover[which(df_all$IDNO == i & df_all$Date == t)] <- aux$turnover[which(aux$turnover %in% c(0.1,0.2,0.3,0.4,0.5))]
        
      }
    }
  }
  
}

# check accounting A = L to find errors ####

df_check <- df_all %>% replace(is.na(.), 0) %>% mutate(za = rowSums(across(var_lab_ia$assets)),zl = rowSums(across(var_lab_il$liabilities)))
df_check <- df_check  %>% mutate(zad = Totalass - za, zadp = (Totalass - za)/Totalass, zld = Totalliab - zl, zldp = (Totalliab - zl)/Totalliab)
df_check$d <- ifelse(abs(df_check$zadp) > 0.05 | abs(df_check$zldp) > 0.05, 1, 0)
df_check <- df_check %>% filter(d == 1) %>%  select(Date,bank,za,zad,zadp,zl,zld,zldp,d)
df_check$zadp <- format(df_check$zadp, digits = 2)
df_check$zldp <- format(df_check$zldp, digits = 2)

