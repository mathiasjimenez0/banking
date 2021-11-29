rm(list = ls())

setwd("D:/Dropbox/Angela-Mathias")

source("./Code/Source.R")
source("./Code/Load.R")

# LSE ####

df <- readRDS("./Dataset/LSE.rds") 
df$Month[which(as.numeric(df$Month) < 10)] <- paste0(0,df$Month[which(as.numeric(df$Month) < 10)])
df$Date <- ymd( paste(df$Year, df$Month, "15", sep="-") )
df$SIC[which(df$Type == "Government Bond")] <- 1
df <- df[which(df$Date <= "1909-03-15" & df$Date >= "1900-03-15"),]
df <- df[which(df$ISO %in% c("CAN","USA","GBR")),]
df <- df[-which((is.na(df$PriceMonthOpen) & is.na(df$PriceMonthLate)) | (is.na(df$AmntUnredeemed) & is.na(df$CapitalNumShare.AmntOutstanding))),]
df <- df[which(df$Type %in% c("Corporate Bond","Government Bond")),]

  cy <- c("CAN","USA","GBR")
  type <- c(4011,4110,3620,3310,1)
  
  df_cp <- data.frame("Date" = unique(df$Date), "CAN_r1" = NA, "CAN_r2" = NA,"CAN_i1" = NA, "CAN_i2" = NA, "CAN_g" = NA)
  df_co <- data.frame("Date" = unique(df$Date), "CAN_r1o" = NA, "CAN_r2o" = NA,"CAN_i1o" = NA, "CAN_i2o" = NA, "CAN_go" = NA)
  
  df_up <- data.frame("Date" = unique(df$Date), "USA_r1" = NA, "USA_r2" = NA,"USA_i1" = NA, "USA_i2" = NA, "USA_g" = NA)
  df_uo <- data.frame("Date" = unique(df$Date), "USA_r1o" = NA, "USA_r2o" = NA,"USA_i1o" = NA, "USA_i2o" = NA, "USA_go" = NA)
  
  df_gp <- data.frame("Date" = unique(df$Date), "GBR_r1" = NA, "GBR_r2" = NA,"GBR_i1" = NA, "GBR_i2" = NA, "GBR_g" = NA)
  df_go <- data.frame("Date" = unique(df$Date), "GBR_r1o" = NA, "GBR_r2o" = NA,"GBR_i1o" = NA, "GBR_i2o" = NA, "GBR_go" = NA)
  
  
  for (i in cy){
    n = 1
    
    for (j in type){
    
      n = n + 1
  
      df_aux <- df[which(df$ISO == i & df$SIC == j) , colnames(df) %in% c("Date","PriceMonthOpen","PriceMonthLate","AmntUnredeemed","CapitalNumShare.AmntOutstanding")]
      if (nrow(df_aux) == 0){next}
      df_aux$out <- df_aux$AmntUnredeemed
      df_aux$out[which(is.na(df_aux$out))] <- df_aux$CapitalNumShare.AmntOutstanding[which(is.na(df_aux$out))]
      df_aux$p <- rowMeans(df_aux[,which(colnames(df_aux) %in% c("PriceMonthOpen","PriceMonthLate"))] ,na.rm = T) 
      df_aux$pout <- df_aux$p*df_aux$out
      
      if (i == "CAN"){
        df_co[which(df_co$Date %in% c(df_aux$Date)) , n] <- aggregate(df_aux$out,by = list(df_aux$Date), sum)[,2]
        df_cp[which(df_cp$Date %in% c(df_aux$Date)) , n] <- aggregate(df_aux$pout,by =list(df_aux$Date), sum)[,2]
        df_cp[which(df_cp$Date %in% c(df_aux$Date)) , n] <- df_cp[which(df_cp$Date %in% c(df_aux$Date)) , n]/df_co[which(df_co$Date %in% c(df_aux$Date)) , n]
  
      } else if(i == "USA"){
        df_uo[which(df_uo$Date %in% c(df_aux$Date)) , n] <- aggregate(df_aux$out,by = list(df_aux$Date), sum)[,2]
        df_up[which(df_up$Date %in% c(df_aux$Date)) , n] <- aggregate(df_aux$pout,by =list(df_aux$Date), sum)[,2]
        df_up[which(df_up$Date %in% c(df_aux$Date)) , n] <- df_up[which(df_up$Date %in% c(df_aux$Date)) , n]/df_uo[which(df_uo$Date %in% c(df_aux$Date)) , n]
  
      } else if(i == "GBR"){
        df_go[which(df_go$Date %in% c(df_aux$Date)) , n] <- aggregate(df_aux$out,by = list(df_aux$Date), sum)[,2]
        df_gp[which(df_gp$Date %in% c(df_aux$Date)) , n] <- aggregate(df_aux$pout,by =list(df_aux$Date), sum)[,2]
        df_gp[which(df_gp$Date %in% c(df_aux$Date)) , n] <- df_gp[which(df_gp$Date %in% c(df_aux$Date)) , n]/df_go[which(df_go$Date %in% c(df_aux$Date)) , n]
      }
      
    }
  } 




# Securities




df_sec2 <- df_sr[,which(colnames(df_sr) %in% c("Date","IDNO","Domdebs","Rwysec","Secloansx"))]
df_sec <- df_sr[,which(colnames(df_sr) %in% c("Date","IDNO","Domdebs","Rwysec","Secloansx"))]
df_sec[is.na(df_sec)] <- 0 

df_sec_plot <-
  df_sec %>%
  # filter(IDNO %in% c(36,7,57,21,35,55,13,54))
filter(IDNO %in% c(1,30,29,49,38,5,30,10))

for (i in unique(df_sec_plot$IDNO)){
  
  df_sec_plot$Domdebs[which(df_sec_plot$IDNO == i & df_sec_plot$Domdebs > 0)] <-  
    df_sec_plot$Domdebs[which(df_sec_plot$IDNO == i & df_sec_plot$Domdebs > 0)]/df_sec_plot$Domdebs[which(df_sec_plot$IDNO == i & df_sec_plot$Domdebs > 0)][1]
  
  df_sec_plot$Rwysec[which(df_sec_plot$IDNO == i & df_sec_plot$Rwysec > 0)] <-  
    df_sec_plot$Rwysec[which(df_sec_plot$IDNO == i & df_sec_plot$Rwysec > 0)]/df_sec_plot$Rwysec[which(df_sec_plot$IDNO == i & df_sec_plot$Rwysec > 0)][1]
  
  df_sec_plot$Secloansx[which(df_sec_plot$IDNO == i & df_sec_plot$Secloansx > 0)] <- 
    df_sec_plot$Secloansx[which(df_sec_plot$IDNO == i & df_sec_plot$Secloansx > 0)]/df_sec_plot$Secloansx[which(df_sec_plot$IDNO == i & df_sec_plot$Secloansx > 0)][1]

}


plot <- ggplot(data = df_sec_plot, aes(x=Date, y=Rwysec, group=as.factor(IDNO), color= as.factor(IDNO))) +
  # scale_color_brewer(palette = "Paired") +
  ylab("Domdebs") +
  labs(color = "IDNO") +
  geom_line()
print(plot)







# Profits ####



df_sr$z <- df_sr$Totalass - df_sr$Totalliab
df_sr$z[which(df_sr$z < 0 & df_sr$IDNO != 36)] <- -df_sr$z[which(df_sr$z < 0 & df_sr$IDNO != 36)]
df_sr$z[which(df_sr$Date == "1908-10-31" & df_sr$IDNO == 36)] <- -df_sr$z[which(df_sr$Date == "1908-10-31" & df_sr$IDNO == 36)]
df_sr$Rest[which(df_sr$z < 0 & df_sr$IDNO == 1)] <- 11000000
df_sr$z <- df_sr$z - df_sr$Kpaid
df_sr$zz <- df_sr$Rest/df_sr$Kpaid
# df_sr_plot$z[which(df_sr_plot$z == -551049)] <- -235493


# Large


df_sr_plot <-
  df_sr %>%
  # filter(IDNO %in% c(26,30,29,49,38,5,30,10))
filter(IDNO %in% c(1))

filename <- "./Output/bom.pdf"
pdf(filename,12,5)

plot <- ggplot(data = df_sr_plot, aes(x=Date, y=z, group=as.factor(IDNO), color= as.factor(IDNO))) +
  scale_color_brewer(palette = "Paired") +
  ylab("A - L - K") +
  labs(color = "IDNO")+
  geom_line()

print(plot)
dev.off()


# Small


df_sr_plot <-
  df_sr %>%
  filter(IDNO %in% c(36,7,57,21,35,55,13,54))

filename <- "./Output/profits_small.pdf"
pdf(filename,12,5)

plot <- ggplot(data = df_sr_plot, aes(x=Date, y=z, group=as.factor(IDNO), color= as.factor(IDNO))) +
  scale_color_brewer(palette = "Paired") +
  ylab("A - L - K") +
  labs(color = "IDNO")+
  geom_line()


print(plot)
dev.off()


# Failures

df_sr_plot <-
  df_sr %>%
  filter(IDNO %in% c(16,13,50,36,37,7))

filename <- "./Output/profits_small.pdf"
pdf(filename,12,5)

plot <- ggplot(data = df_sr_plot, aes(x=Date, y=z, group=as.factor(IDNO), color= as.factor(IDNO))) +
  scale_color_brewer(palette = "Paired") +
  ylab("A - L - K") +
  labs(color = "IDNO")+
  geom_line()


print(plot)
dev.off()


# All

df_all$z <- df_all$Totalass - df_all$Totalliab
df_all$z[which(df_all$z < 0 & df_all$IDNO != 36)] <- -df_all$z[which(df_all$z < 0 & df_all$IDNO != 36)]
df_all$z[which(df_all$Date == "1908-10-31" & df_all$IDNO == 36)] <- -df_all$z[which(df_all$Date == "1908-10-31" & df_all$IDNO == 36)]

df_all$Rest[which(df_all$z < 0 & df_all$IDNO == 1)] <- 11000000
df_all$z[which(df_all$z == -551049)] <- -235493
df_all$Kpaid[which(df_all$z == -970819)] <- 3000000
df_all$Kpaid[which(df_all$z == -920108)] <- 3000000

df_all$z <- df_all$z - df_all$Kpaid

df_all_plot <-
  df_all %>%
  filter(IDNO %in% c(53))

filename <- "./Output/profits_fail.pdf"
pdf(filename,12,5)

plot <- ggplot(data = df_all_plot, aes(x=Date, y=z, group=as.factor(IDNO), color= as.factor(IDNO))) +
  scale_color_brewer(palette = "Paired") +
  ylab("A - L - K") +
  labs(color = "IDNO")+
  geom_line()


print(plot)
dev.off()


# Data on Matrix  ####

df_sec <- df_sr[,which(colnames(df_sr) %in% c("Date","IDNO","Domdebs","Provothdebs","Rwysec","Secloansx"))]

dates <- unique(df_sec$Date)
T <- length(dates)
banks <- unique(df_sec$IDNO)
N <- length(banks)
sec_list <- list("Domdebs","Provothdebs","Rwysec","Secloansx")
K <- length(sec_list)

for (i in 1:K){
  assign(sec_list[[i]],matrix(data = NA,N,T,dimnames = list(banks,dates))) 
}


for (i in 1:N){
  for (t in 1:T){
    if (i %in% df_sec$IDNO[which(df_sec$Date == dates[t])]){
      Domdebs[i,t] <- df_sec$Domdebs[which(df_sec$IDNO == i & df_sec$Date == dates[t])] 
    }
  }
}

