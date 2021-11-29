# Construct date column as date class. ####

Time <- length(df$month)

df$month<-capitalize(df$month)
df$month<-match(df$month,month.abb)
df$month<- as.numeric(df$month)
df$date <- vector(length = Time)
for (idt in (1:Time)) {
          
          if (as.numeric(df$month[idt]) < 10){
                    df$month[idt] <- paste0("0",df$month[idt],"28")
          } else {df$month[idt] <- paste0(df$month[idt],"28")}
          
          df$date[idt] <- paste0(df$Year[idt],df$month[idt])
          
}

df$date <- as.Date(ymd(df$date))
df <- df[,-c(1,2,3,4)]
df_dom$date <- as.Date(ymd(df$date))
df_dom <- df_dom[,-c(1,2)]
df_pi$date <- as.Date(ymd(df$date))
df_pi <- df_pi[,-c(1,2)]

# numeric column

df$loansbanksfromcanadasec <- as.numeric(df$loansbanksfromcanadasec)
df$loansbanksfromcanadasec[which(is.na(df$loansbanksfromcanadasec))] <- 0


# Construct consistent 'Total Liabilities'. Since Oct 1923 it includes capital+reserve. ####

from <- which(df$date >= "1923-10-28")

df$totalliabilities[from] <- df$totalliabilities[from] - df$capital[from] - df$reserve[from]

rm(from,idt,Time)

# # Construct consistent variables for public securities (See Curtis for details). ####
# 
# a <- df$publicsecnotdom1[which(complete.cases(df$publicsecnotdom1))]
# a_end <- a[NROW(a)]
# b <- df$publicsecnotdom2[which(complete.cases(df$publicsecnotdom2))]
# b_start <- b[1]
# prop_prov <- 1- b_start/a_end
# 
# 
# # Change NA to 0's to do math with df
# 
# df$domandprovsec[which(is.na(df$domandprovsec) == 1)] <- 0
# df$publicsecnotdom1[which(is.na(df$publicsecnotdom1) == 1)] <- 0
# df$publicsecnotdom2[which(is.na(df$publicsecnotdom2) == 1)] <- 0
# 
# # Do math
# 
# df$domandprovsec_new<- df$domandprovsec + df$publicsecnotdom1*prop_prov
# df$publicsecnotdom1 <- df$publicsecnotdom1*(1-prop_prov)
# df$publicsecnotdom_new <- df$publicsecnotdom1 + df$publicsecnotdom2
# 
# # Go back to NA's
# 
# df$domandprovsec[which(df$domandprovsec == 0)] <- NA
# df$publicsecnotdom1[which(df$publicsecnotdom1 == 0)] <- NA
# df$publicsecnotdom2[which(df$publicsecnotdom2 == 0)] <- NA
# df$domandprovsec_new[which(df$domandprovsec_new == 0)] <- NA
# df$publicsecnotdom_new[which(df$publicsecnotdom_new == 0)] <- NA
# 
# 
# df<-df[,-which(names(df) %in% c("publicsecnotdom1","publicsecnotdom2","domandprovsec"))]
# 
# # Substract Provincial notes from Current Gold and Subsidiary Coin serie ####
# 
# a <- df$currgold[which(df$date == "1870-07-28")] 
# b <- df$currgold[which(df$date == "1871-07-28")]
# prop <- b/a
# df$currgold[which(df$date <= "1871-06-28")] <- df$currgold[which(df$date <= "1871-06-28")]*prop
# 
# 
# # Keep only new reference names ####
# 
# 
# var_lab <- var_lab[which(var_lab$varname %in% names(df)),]
# var_lab_asset <- var_lab_asset[which(var_lab_asset$varname %in% names(df)),]
# var_lab_liab <- var_lab_liab[which(var_lab_liab$varname %in% names(df)),]
# 

# # Construct Reserve serie from 1883-04 back assuming equal proportion to capital ####
# 
# 
# df$id <- seq.int(nrow(df))
# df_aux <- df[which(df$date <= "1895-05-28" & df$date >= "1888-05-28"),]
# y <- df_aux$reserve/df_aux$capital
# reg <- lm(y ~ 0 + df_aux$id,data = df_aux)
# y2 <- df$reserve/df$capital
# c <- y2[which(df$date == "1883-07-28")] - reg$coefficients*df$id[which(df$date == "1883-07-28")]
# s <- which(df$date <= "1883-06-28")
# y2[s] <- reg$coefficients*df$id[s] + c
# # df$reserve[s] <- y2[s]*df$capital[s]
# 
# 
# # Before 1900, the series related to foreign business where reported in net. Construct the gross series 
# # for balances due from other banks elsewhere than Canada and the UK (call loans in the NYSE) ####
# 
# a <- df$callloansnotcanada[which(df$date == "1900-07-28")]
# b <- df$oloansdiscnotcanada[which(df$date == "1900-07-28")]
# c <- df$duefrombanksuk[which(df$date == "1900-07-28")]
# d <- df$duefrombanksnotcanuk[which(df$date == "1900-07-28")]
# 
# prop <- a/(a+b)
# 
# e <- data.frame(df$duefrombanksuk,df$duefrombanksnotcanuk)
# e <- rowSums(e,na.rm = T)
# e_1 <- prop*e 
# e_2 <- (1-prop)*e 
# 
# f_1 <- e_1[which(df$date == "1900-07-28")]
# f_2 <- e_2[which(df$date == "1900-07-28")]
# 
# prop_1 <- a/(f_1)
# 
# #df$callloansnotcanada[which(df$date <= "1900-06-28")] <- prop_1*e_1[which(df$date <= "1900-06-28")]
# 
# prop_2 <- b/(f_2)
# 
# #df$oloansdiscnotcanada[which(df$date <= "1900-06-28")] <- prop_2*e_2[which(df$date <= "1900-06-28")]
# 
# 
# #df$totalassets[which(df$date <= "1900-07-28")] <- df$totalassets[which(df$date <= "1900-07-28")] + 
# #  rowSums(data.frame((prop_1-1)*e_1[which(df$date <= "1900-07-28")],
# #                     (prop_2-1)*e_2[which(df$date <= "1900-07-28")]),na.rm = T)
# 
# 
# df$duetobanksuk[which(df$date == "1900-07-28")] <- 5517 # Typo in raw data
# a <- df$depnotcanada[(df$date == "1900-07-28")]
# e <- data.frame(df$duetobanksuk,df$duetobanksnotcanuk)
# e <- rowSums(e,na.rm = T)
# b <- e[(df$date == "1900-07-28")]
# 
# prop <- a/b
# #df$depnotcanada[which(df$date <= "1900-06-28")] <- prop*e[which(df$date <= "1900-06-28")]
# 
# 

# Sort columns alphabetically. ####

# df     <- df[,sort(names(df))]
# df_dom <- df_dom[,sort(names(df_dom))]
# df_pi  <- df_pi[,sort(names(df_pi))]
# var_lab <- var_lab[order(var_lab$varname),]
# var_lab_asset <- var_lab_asset[order(var_lab_asset$varname),]
# var_lab_liab <- var_lab_liab[order(var_lab_liab$varname),]
# var_lab_bankv <- var_lab_bankv[order(var_lab_bankv$varname),]
