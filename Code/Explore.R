# COLLAPSE-ALL COMMAND: ALT+O (TO SEE SECTION TITLES) ####
# ___________________________ 



#   START EXPLORING


# ___________________________
# Subset df ####

df <- df[which(df$date <= "1914-06-28" & df$date >= "1871-08-28"),]
df_pi <- df_pi[which(df_pi$date <= "1914-06-28" & df_pi$date >= "1871-08-28"),]

# Return On Assets (ROA) ####

ROA     <- (df$reserve)/df$totalassets

between <- which(!is.na(ROA) & df$date >= "1871-07-28") # Only plot for dates which ROA is not NA
series  <- ROA[between]
dates   <- df$date[between] 

filename <- paste0("./Output/ROA.pdf")
pdf(filename,8,5)


par(lwd=1.4,cex=1,cex.lab=0.8)
tsplot <- zoo(series,dates)
plot(tsplot,lwd=2.5,main="",xaxt = "n",yaxt = "n",
      xlab="",ylab="",ylim = c(min(series),max(series,na.rm = T)))

axis_1 <-seq(dates[1],dates[length(dates)],"year")
axis(1,axis_1 ,format(axis_1,"%Y"), cex.axis = 0.7,xlim=c(dates[1],dates[length(dates)]))
axis(2, at=pretty(series), lab=pretty(series), las=TRUE)
grid (NA,NULL, lty = 4)


dev.off()

rm(series,dates,tsplot,between,axis_1) # Remove variables


# Reserve ratio ####

dd <- rowSums(data.frame(df$deppubdemcanada,
                         df$deppublicfixcanada
                         ,df$domdepdem
                         ,df$provgovdepdem
                         ,df$loansbankscanadasec
                         ,df$loansdeptobanksunsec
                         ,df$duetobanksdailyex
                         ,df$depandduetobankscanada
                         ,df$duetobankscanada),na.rm = T)

n <- df$notes
m1 <- rowSums(data.frame(dd,n),na.rm = T)
RR     <- (df$currgold + df$domnotes)/m1
RR_g     <- (df$currgold)/m1

between <- which(!is.na(RR) & df$date >= "1871-07-28") # Only plot for dates which ROE is not NA
series  <- RR[between]
series2 <- RR_g[between]
dates   <- df$date[between] 

filename <- paste0("./Output/RR.pdf")
pdf(filename,8,5)


par(lwd=2.5,cex=1,cex.lab=0.8)

tsplot <- zoo(series,dates)
#tsplot2 <- zoo(series2,dates)
plot(tsplot,lwd=2.5,main="",xaxt = "n",yaxt = "n",
     xlab="",ylab="",ylim = c(min(rbind(series)),max(rbind(series),na.rm = T)),col = "gold")
#lines(tsplot2,lwd=1, col = "blue")
#legend("bottomleft",legend = c("Gold + Dominion Notes","Gold"),col = c("gold"),lty = c(1,1),cex = 0.6)

axis_1 <-seq(dates[1],dates[length(dates)],"year")
axis(1,axis_1 ,format(axis_1,"%Y"), cex.axis = 1,xlim=c(dates[1],dates[length(dates)]))
axis(2, at=pretty(rbind(series)), lab=pretty(rbind(series)), las=TRUE)
grid (NA,NULL, lty = 4)

# print(tsplot)

dev.off()



rm(series,dates,tsplot,between,axis_1) # Remove variables



# Leverage ratio ####

tot_equity <- rowSums(data.frame(df$capital,df$reserve),na.rm = T) 

diff <- und_profits - tot_equity

LR     <- (df$totalassets)/(tot_equity)

#LR     <- (df$deppubdemcanada + df$notes)/(tot_equity)



between <- which(!is.na(LR)) # Only plot for dates which ROE is not NA
series  <- LR[between]
dates   <- df$date[between] 



filename <- paste0("./Output/LR.pdf")
pdf(filename,8,5)


  par(lwd=1.4,cex=1,cex.lab=0.8)
  tsplot <- zoo(series,dates)
  plot(tsplot,lwd=2.5,main="",xaxt = "n",yaxt = "n",
       xlab="",ylab="",ylim = c(min(series),max(series,na.rm = T)))
  
  axis_1 <-seq(dates[1],dates[length(dates)],"year")
  axis(1,axis_1 ,format(axis_1,"%Y"), cex.axis = 0.7,xlim=c(dates[1],dates[length(dates)]))
  axis(2, at=pretty(series), lab=pretty(series), las=TRUE)
  grid (NA,NULL, lty = 4)


dev.off()

rm(series,dates,tsplot,between,axis_1) # Remove variables


# Asset porfolio ####

# Construct desired series

d <- which(df$date >= "1871-07-28")

df1 <- data.frame(df$currgold,df$domnotes)
df1 <- data.frame(df$date, Value = rowSums(df1,na.rm = T), groupnum = "Reserves") 
df2 <- data.frame(df$mortages,df$bankprem,df$realestatenotprem)
df2 <- data.frame(df$date,Value = rowSums(df2,na.rm = T), groupnum = "Mortgages and real estate") 
df3 <- data.frame(df$date,Value = df$totalsec, groupnum = "Securities") 
# duefrombanksnotcanul and duefrombankuk is the net business abroad. I assume liabilities were 0 and only assets were call loans
#df$duefrombanksnotcanuk[which(df$date >= "1900-07-28")] <- NA
#df$duefrombanksuk[which(df$date >= "1900-07-28")] <- NA
df4 <- data.frame(df$totalloanscanada,df$callloansnotcanada,df$oloansdiscnotcanada,
                  df$duefrombanksnotcanuk,df$duefrombanksuk,
                  df$loansbankscanadasec,df$loansdeptobankssec,df$loansdeptobanksunsec,
                  df$depandduefrombankscanada,
                  df$depminfin,df$loansdir) 
df4 <- data.frame(df$date,Value = rowSums(df4,na.rm = T), groupnum = "Loans") 
df5 <- data.frame(df$noteschecksonbanks,df$notesonbanks,df$chequesonbanks,df$duefrombanks,
                  df$duefrombanksdailyex) 
df5 <- data.frame(df$date,Value = rowSums(df5,na.rm = T), groupnum = "Notes and checks on other banks") 


# note for me: if you include these then diff gets very high so probably are included already in totalloanscanada
#df6 <- data.frame(df$loansdisadvsharescoll,df$loansdiscadvassetcoll)
#df6 <- data.frame(df$loansdiscadvcorp,df$loansdiscadvocorp,df$loansdiscadvmunicipal) 
# note for me: did not include foreigncurr, sharesloanscontcomp because these are all NA's from 1871 to 1914.

# Construct complement category "Others" (total - sum of desired series)

diff <- (df$totalassets - 
          df1$Value - 
          df2$Value -
          df3$Value -
          df4$Value - 
          df5$Value)

df6 <- data.frame(df$date,Value = diff, groupnum = "Others") 
df6_t <- smooth(df6$Value[!is.na(df6$Value)]) # smooth serie
df6$Value[!is.na(df6$Value)] <- df6_t
df6$Value[which(df6$Value < 0)] <- 0

# Merge all data frames and subset using 'd'

df_temp <- rbind(df4[d,],df1[d,],df2[d,],df5[d,],df3[d,],df6[d,])

# Plot asset composition (desired + "Others")


df_temp <- data.frame(df$callloanscanada[b], 
           df$otherloansdisccanada[b], df$loansgovcanada[b], df$loansprovgov[b], df$loansmunicip[b])

#df1 <- data.frame(df$date,Value = df$callloanscanada, groupnum = "call")
#df2 <- data.frame(df$date,Value = df$otherloansdisccanada, groupnum = "oloans")
#df3 <- data.frame(df$date,Value = df$loansgovcanada, groupnum = "gov")
#df4 <- data.frame(df$date,Value = df$loansprovgov, groupnum = "prov")
#df5 <- data.frame(df$date,Value = df$loansmunicip, groupnum = "muni")

#df_temp <- rbind(df1[b,],df2[b,],df3[b,],df4[b,],df5[b,])

filename <- "./Output/asset_composition.pdf"
pdf(filename,12,5)


cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
acomp_plot <- ggplot(df_temp, aes(x=df.date,y=Value,group=groupnum,fill = groupnum))     + 
           geom_area(position="fill")                   +    
           theme_minimal()                                   +
           theme(legend.justification = c(0, 0)
                 ,legend.position=c(0.1, 0.4)
                 ,legend.background = element_rect(linetype = "blank")
                 ,legend.text = element_text(size = 12)
                 ,legend.key.size = unit(0.55, "cm")
                 ,axis.text.x = element_text(size=14)
                 ,axis.text.y = element_text(size=12)
                 ,axis.text.y.right = element_text())   +
           theme(axis.title.x=element_blank()
                 ,axis.title.y =element_blank())        +
           scale_fill_manual(values = cbPalette
                 ,name = "")                            +
           scale_x_date(breaks = date_breaks("1 year")
                 ,labels=date_format("%Y"))             +
           scale_y_continuous(breaks = round(seq(0,1,by = 0.1),1)) 

print(acomp_plot)

dev.off()

# Remove variables

rm(df1,df2,df3,df4,df5,df6,df_temp,d1,filename,diff,acomp_plot)

# Loan portfolio ####

# Desired variables (n = 14)

loans <- data.frame(df$callloanscanada,df$callloansnotcanada
                  ,df$otherloansdisccanada,df$oloansdiscnotcanada
                  ,df$loansgovcanada,df$loansprovgov,df$loansmunicip
                  ,df$loansdiscadvassetcoll,df$loansdisadvsharescoll
                  ,df$loansdiscadvcorp,df$loansdiscadvocorp
                  ,df$loansdiscadvmunicipal, df$duefrombanksnotcanuk
                  ,df$duefrombanksuk,df$loansbankscanadasec
                  ,df$loansdeptobankssec,df$loansdeptobanksunsec,
                  df$depminfin,df$loansdir) 

# Check if the asset category "Loans" from section asset portfolio is consistent with 'loans' from this section

loans_temp <- data.frame(df$date,Value = rowSums(loans,na.rm = T)) 

loans_temp_2 <- data.frame(df$totalloanscanada,df$callloansnotcanada,df$oloansdiscnotcanada,df$duefrombanksnotcanuk,df$duefrombanksuk,
                           df$loansbankscanadasec,df$loansdeptobankssec,df$loansdeptobanksunsec,
                           df$depminfin,df$loansdir)  # Construct category "Loans"

loans_temp_2 <- data.frame(df$date,Value = rowSums(loans_temp_2,na.rm = T))

diff <- loans_temp_2$Value - loans_temp$Value

plot(df$date,diff)


# Construct desired groups

df1 <- rowSums(data.frame(df$otherloansdisccanada
#                          ,df$oloansdiscnotcanada
                          ,df$loansdiscadvmunicipal
                          ,df$loansmunicip
                          ,df$loansdiscadvcorp
                          ,df$loansdiscadvocorp,df$loansbankscanadasec
                          ,df$loansdeptobankssec,df$loansdeptobanksunsec)
               ,na.rm = T)

df1 <- data.frame(df$date, Value = df1, groupname = "Loans to individuals, corporations, municipalities and banks in Canada") 

df2 <- rowSums(data.frame(df$callloanscanada
                          ,df$loansdisadvsharescoll
                          ,df$loansdiscadvassetcoll
                          ,df$depminfin,df$loansdir)
               ,na.rm = T)

df2 <- data.frame(df$date, Value = df2, groupname = "Call loans") 



# Start df3

df3 <- data.frame(df$date, Value = df$oloansdiscnotcanada , groupname = "Loans to individuals, corporations and municipalities elsewhere")

# Before 1900 df3 was included in df1
# Therefore, construct df3 backwards and accommodate df1 assuming constant proportion

a <- df1$Value[which(df1$df.date == "1900-07-28")]
b <- df3$Value[which(df1$df.date == "1900-07-28")]
prop <- b/a

df3$Value[which(df3$df.date <= "1900-07-28")] <- df1$Value[which(df1$df.date <= "1900-07-28")]*prop
df1$Value[which(df1$df.date <= "1900-07-28")] <- df1$Value[which(df1$df.date <= "1900-07-28")]*(1-prop)

# End df3


# Start df4

# duefrombanksnotcanul and duefrombankuk is the net business abroad. I assume liabilities were 0 and only assets were call loans

df_temp_1 <- df$duefrombanksnotcanuk
df_temp_1[which(df$date >= "1900-07-28")] <- NA
df_temp_2 <- df$duefrombanksuk
df_temp_2[which(df$date >= "1900-07-28")] <- NA

df4 <- rowSums(data.frame(df$callloansnotcanada
                          ,df_temp_1
                          ,df_temp_2)
               ,na.rm = T)

df4 <- data.frame(df$date, Value = df4, groupname = "Call loans at US")

# End df4

df5 <- rowSums(data.frame(df$loansgovcanada
                          ,df$loansprovgov)
               ,na.rm = T)

df5 <- data.frame(df$date, Value = df5, groupname = "Dominion and Provincial loans")


# Save new variables in df

df$loansprivatecan_new <- df1$Value
df$callloanscanada_new <- df2$Value
df$loansprivatenotcan_new <- df3$Value
df$callloansnotcanada_new <- df4$Value
df$domprovloans_new <- df5$Value
# df$loansdepotherbanks_new <- df6$Value


# Plot loan portfolio

d <- which(df$date >= "1873-07-28")
df_temp <- rbind(df1[d,],df2[d,],df3[d,],df4[d,],df5[d,])

filename <- "./Output/loan_composition.pdf"
pdf(filename,12,5)

cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
lport_plot <- ggplot(df_temp, aes(x=df.date,y=Value,group=groupname,fill = groupname))     + 
          geom_area(position="fill")                   +    
          theme_minimal()                                   +
          theme(legend.justification = c(0, 0)
                ,legend.position=c(0.1, 0.4)
                ,legend.background = element_rect(linetype = "blank")
                ,legend.text = element_text(size = 12)
                ,legend.key.size = unit(0.55, "cm")
                ,axis.text.x = element_text(size=14)
                ,axis.text.y = element_text(size=12)
                ,axis.text.y.right = element_text())   +
          theme(axis.title.x=element_blank()
                ,axis.title.y =element_blank())        +
          scale_fill_manual(values = cbPalette
                            ,name = "")                            +
          scale_x_date(breaks = date_breaks("5 year")
                       ,labels=date_format("%Y"))             +
          scale_y_continuous(breaks = round(seq(0,1,by = 0.1),1)) 

print(lport_plot)

dev.off()


# Assets model ####

g <- data.frame(df$currgold,df$domnotes)
g <- data.frame(df$date, Value = rowSums(g,na.rm = T), groupname = "Reserves") 

# Start df4

f <- rowSums(data.frame(df$callloansnotcanada
                          ,df$duefrombanksnotcanuk
                          ,df$duefrombanksuk)
               ,na.rm = T)

f <- data.frame(df$date, Value = f, groupname = "Call loans at US")

# End df4

call <- rowSums(data.frame(df$callloanscanada
                          ,df$loansdisadvsharescoll
                          ,df$loansdiscadvassetcoll)
               ,na.rm = T)

call <- data.frame(df$date, Value = call, groupname = "Call loans") 




b <- data.frame(df$totalloanscanada
                ,df$oloansdiscnotcanada
                ,df$loansbankscanadasec
                ,df$loansdeptobankssec
                ,df$loansdeptobanksunsec
                ,df$mortages,df$bankprem
                ,df$realestatenotprem
                ,df$totalsec) 

b <- data.frame(df$date,Value = rowSums(b,na.rm = T), groupname = "Loans, securities and real estate") 
#b$Value <- b$Value - call$Value USE IF ALSO INCLUDE CALL LOANS


cl <- data.frame(df$noteschecksonbanks,df$notesonbanks,df$chequesonbanks,df$duefrombanks,
                  df$duefrombanksdailyex) 
cl <- data.frame(df$date,Value = rowSums(cl,na.rm = T), groupname = "Notes and checks on other banks") 



d <- which(df$date >= "1873-07-28")
df_temp <- rbind(g[d,],b[d,],f[d,],cl[d,],dd[d,],td[d,],e[d,],n[d,])


filename <- "./Output/Asset paper.pdf"
pdf(filename,12,5)

cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#EE79A7","#DD79A7")
lport_plot <- ggplot(df_temp, aes(x=df.date,y=Value,group=groupname,fill = groupname))     + 
  geom_area(position="fill")                   +    
  theme_minimal()                                   +
  theme(legend.justification = c(0, 0)
        ,legend.position=c(0.1, 0.4)
        ,legend.background = element_rect(linetype = "blank")
        ,legend.text = element_text(size = 12)
        ,legend.key.size = unit(0.55, "cm")
        ,axis.text.x = element_text(size=14)
        ,axis.text.y = element_text(size=12)
        ,axis.text.y.right = element_text())   +
  theme(axis.title.x=element_blank()
        ,axis.title.y =element_blank())        +
  scale_fill_manual(values = cbPalette
                    ,name = "")                            +
  scale_x_date(breaks = date_breaks("5 year")
               ,labels=date_format("%Y"))             +
  scale_y_continuous(breaks = round(seq(0,1,by = 0.1),1)) 

print(lport_plot)

dev.off()



# Liability composition ####

# Construct desired series


billspayable
letterscredit


d <- which(df$date >= "1871-07-28")

df1 <- data.frame(df$provgovdepdem,df$provdepfix,df$duetoprov,df$domdepdem,df$domdepfix,df$duetodomafterded)
df1 <- data.frame(df$date, Value = rowSums(df1,na.rm = T), groupnum = "Provincial and Dominion Government deposits") 
df3 <- data.frame(df$loansfromdepbybankscanadasec
          ,df$loansfromdepbybankscanadaunsec
          ,df$duetobanksdailyex
          ,df$depandduetobankscanada
          ,df$duetobankscanada)

df3 <- data.frame(df$date,Value = rowSums(df3,na.rm = T), groupnum = "Debts to other Canadian banks") 

df4 <- data.frame(df$capital,df$reserve)
df4 <- data.frame(df$date,Value = rowSums(df4,na.rm = T), groupnum = "Equity") 
df5 <- data.frame(df$depnotcanada,df$duetobanksnotcanuk,df$duetobanksuk,df$billspay,df$letterscredit)
df5 <- data.frame(df$date,Value = rowSums(df5,na.rm = T), groupnum = "Deposits from foreign banks or in foreign branches") 
df6 <- data.frame(df$date,Value = df$deppublicfixcanada, groupnum = "Time deposits from the public in Canada")
df7 <- data.frame(df$date,Value = df$deppubdemcanada, groupnum = "Demand deposits from the public in Canada")
df8 <- data.frame(df$date,Value = df$notes, groupnum = "Notes outstanding") 

# Note for me: did not include advfinact, billspay,depforsecdomcontracts, lettcred, letterscredit cause they are not relevant


# Construct complement category "Others" (total - sum of desired series)

total <- rowSums(data.frame(df$totalliabilities,df$reserve,df$capital))

diff <- (total - 
                   df1$Value - 
                   df3$Value - 
                   df4$Value - 
                   df5$Value -
                   df6$Value -
                   df7$Value -
                   df8$Value)

df9 <- data.frame(df$date,Value = diff, groupnum = "Others") 
df9$Value[which(df9$Value < 0)] <- 0

# Merge all data frames and subset using 'd'

df_temp <- rbind(df1[d,],df3[d,],df4[d,],df5[d,],df6[d,],df7[d,],df8[d,],df9[d,])

# Plot liability composition (desired + "Others")

filename <- "./Output/liability_composition.pdf"
pdf(filename,12,5)

cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999", "#E69F00")
acomp_plot <- ggplot(df_temp, aes(x=df.date,y=Value,group=groupnum,fill = groupnum))     + 
          geom_area(position="fill")                   +    
          theme_minimal()                                   +
          theme(legend.justification = c(0, 0)
                ,legend.position=c(0.55,0.2)
                ,legend.background = element_rect(linetype = "blank")
                ,legend.text = element_text(size = 12)
                ,legend.key.size = unit(0.55, "cm")
                ,axis.text.x = element_text(size=14)
                ,axis.text.y = element_text(size=12)
                ,axis.text.y.right = element_text())   +
          theme(axis.title.x=element_blank()
                ,axis.title.y =element_blank())        +
          scale_fill_manual(values = cbPalette
                            ,name = "")                            +
          scale_x_date(breaks = date_breaks("5 year")
                       ,labels=date_format("%Y"))             +
          scale_y_continuous(breaks = round(seq(0,1,by = 0.1),1)) 

print(acomp_plot)

dev.off()

# Remove variables

rm(df1,df2,df3,df4,df5,df6,df_temp,d1,filename,diff,acomp_plot)


# Liability model ####




d <- which(df$date >= "1871-07-28")
df3 <- data.frame(df$loansbankscanadasec
                  ,df$loansfromdepbybankscanadasec
                  ,df$loansfromdepbybankscanadaunsec
                  ,df$duetobanksdailyex
                  ,df$depandduetobankscanada
                  ,df$duetobankscanada)
df3 <- rowSums(df3,na.rm = T)

e <- data.frame(df$capital,df$reserve)
e <- data.frame(df$date,Value = rowSums(e,na.rm = T), groupname = "Equity") 
td <- rowSums(data.frame(df$depnotcanada,df$duetobanksnotcanuk,df$duetobanksuk,df$deppublicfixcanada,df$provdepfix,df$duetoprov,df$domdepfix,df$duetodomafterded),na.rm = T)
td <- data.frame(df$date,Value =  td, groupname = "Time deposits")
dd <- rowSums(data.frame(df$deppubdemcanada,df$domdepdem,df$provgovdepdem,df3),na.rm = T)
dd <- data.frame(df$date,Value = dd, groupname = "Demand deposits")
n <- data.frame(df$date,Value = df$notes, groupname = "Notes outstanding") 


df_temp <- rbind(dd[d,],td[d,],e[d,],n[d,])

# Plot liability composition (desired + "Others")

filename <- "./Output/Liability paper.pdf"
pdf(filename,12,5)

cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999", "#E69F00")
acomp_plot <- ggplot(df_temp, aes(x=df.date,y=Value,group=groupname,fill = groupnum))     + 
  geom_area(position="fill")                   +    
  theme_minimal()                                   +
  theme(legend.justification = c(0, 0)
        ,legend.position=c(0.55,0.2)
        ,legend.background = element_rect(linetype = "blank")
        ,legend.text = element_text(size = 12)
        ,legend.key.size = unit(0.55, "cm")
        ,axis.text.x = element_text(size=14)
        ,axis.text.y = element_text(size=12)
        ,axis.text.y.right = element_text())   +
  theme(axis.title.x=element_blank()
        ,axis.title.y =element_blank())        +
  scale_fill_manual(values = cbPalette
                    ,name = "")                            +
  scale_x_date(breaks = date_breaks("5 year")
               ,labels=date_format("%Y"))             +
  scale_y_continuous(breaks = round(seq(0,1,by = 0.1),1)) 

print(acomp_plot)

dev.off()




# Deposit-currency ratio ####

totd <- data.frame(df$totaldeppubcanada,df$depandduetobankscanada
                     ,df$depnotcanada,df$duetoprov,df$duetodomafterded)
totd <- apply(totd, 1, FUN=function(x) sum(x,na.rm = T))




between <- which(!is.na(totd) & df$date >= "1871-07-28")
dcr <- (totd[between])/(df$notes[between])

series  <- dcr
dates   <- df$date[between] 



filename <- paste0("./Output/dcr.pdf")
pdf(filename,8,5)


par(lwd=1.4,cex=1,cex.lab=0.8)
tsplot <- zoo(series,dates)
plot(tsplot,lwd=2.5,main="",xaxt = "n",yaxt = "n",
     xlab="",ylab="",ylim = c(min(series),max(series,na.rm = T)))

axis_1 <-seq(dates[1],dates[length(dates)],"year")
axis(1,axis_1 ,format(axis_1,"%Y"), cex.axis = 0.7,xlim=c(dates[1],dates[length(dates)]))
axis(2, at=pretty(series), lab=pretty(series), las=TRUE)
grid (NA,NULL, lty = 4)

print(tsplot)

dev.off()

# Construct 1st difference of dcr

d_dcr <- matrix(NA,nrow = length(between) - 1,ncol = 1)
for (i in between[2]:length(between)) {
          d_dcr[i] <- dcr[i] - dcr[i-1]
}

# Plot dcr ACF and PACF 

filename <- paste0("./Output/dcr_ACF.pdf")
pdf(filename,8,5)
par(lwd=1.4,cex=1,cex.lab=1)
Acf(d_dcr,type = "correlation",main = "")
dev.off()

filename <- paste0("./Output/dcr_PACF.pdf")
pdf(filename,8,5)
par(lwd=1.4,cex=1,cex.lab=1)
Pacf(d_dcr,main = "",ylab = "PACF")
dev.off()

# Remove variables

rm(series,dates,tsplot,between) 

# Dividend distribution ####


# Look for NA's or 0's in Div variable

NA_90 <- df_90[which(df_90$Div == 0 | is.na(df_90$Div)),]
NA_00 <- df_00[which(df_00$Div == 0 | is.na(df_00$Div)),]
NA_05 <- df_05[which(df_05$Div == 0 | is.na(df_05$Div)),]
NA_10 <- df_10[which(df_10$Div == 0 | is.na(df_10$Div)),]


# Drop banks which's balance sheet do not make sense

df_d90 <- data.frame(value = df_90$Div[-which(df_90$IDNO %in% c(44,21))],
                     group = "1890") 
df_d00 <- data.frame(value = df_00$Div[-which(df_00$IDNO %in% c(6,27))],
                     group = "1900")
df_d05 <- data.frame(value = df_05$Div[-which(df_05$IDNO %in% c(52))],
                     group = "1905")
df_d10 <- data.frame(value = df_10$Div,
                     group = "1910")

# Plot

df_d <- rbind(df_d90,df_d00,df_d05,df_d10)

cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2")

# filename <- paste0("./Output/Dividend_dist.pdf")
# pdf(filename,8,5)

ggplot(df_d, aes(x = value)) +
          geom_histogram(bins=20, fill = cbPalette[1]) +
          scale_x_continuous(name = "Dividend Rate",
                             breaks = seq(0, 16, 2),
                             limits=c(-1, 16)) +
          scale_y_continuous(name = "Frecuency") +
          ggtitle("") +
          theme_bw() +
          facet_grid(group ~ . ) +
          geom_vline(data=filter(df_d, group=="1890"), aes(xintercept= mean(df_d90$value)), colour=cbPalette[2],lwd = 3) +
          geom_vline(data=filter(df_d, group=="1900"), aes(xintercept=mean(df_d00$value)), colour=cbPalette[2],lwd = 3) +
          geom_vline(data=filter(df_d, group=="1905"), aes(xintercept=mean(df_d05$value)), colour=cbPalette[2],lwd = 3) +
          geom_vline(data=filter(df_d, group=="1910"), aes(xintercept=mean(df_d10$value)), colour=cbPalette[2],lwd = 3) 

# dev.off()


# Plot total asset distribution conditional on dividend rate

df_d90 <- df_90[-which(df_90$IDNO %in% c(44,21)),]
df_d00 <- df_00[-which(df_00$IDNO %in% c(6,27)),]
df_d05 <- df_05[-which(df_05$IDNO %in% c(52)),]
df_d10 <- df_10


reg1 <- data.frame("div" = df_d90$Div,"tass" = df_d90$Totalass)
reg2 <- data.frame("div" =df_d00$Div,"tass" = df_d00$Totalass)
reg3 <- data.frame("div" =df_d05$Div,"tass" = df_d05$Totalass)
reg4 <- data.frame("div" =df_d10$Div,"tass" = df_d10$Totalass)


reg_t <- rbind(reg1,reg2,reg3,reg4)

df1 <- reg_t[which(reg_t$div %in% c(0,1,2,3,4)),]
df1$group <- "0-4"
df2 <- reg_t[which(reg_t$div %in% c(5,6,7)),]
df2$group <- "5-7"
df3 <- reg_t[which(reg_t$div %in% c(8,9,10)),]
df3$group <- "8-10"
df4 <- reg_t[which(reg_t$div >= 11),]
df4$group <- "11-"

df_b <- rbind(df1,df2,df3,df4)
 

cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2")

filename <- paste0("./Output/div_asset.pdf")
pdf(filename,8,5)

ggplot(df_b, aes(group,tass,colour = group)) + 
          geom_violin() +
          geom_point()+
          scale_colour_manual(values = cbPalette)+
          theme_bw()+
          xlab("Dividend Rate")+
          ylab("Assets")+
          scale_x_discrete(name ="Dividend Rate", 
                           limits=c("0-4","5-7","8-10","11-"))+
          theme(axis.title.x = element_text("")
                ,axis.text.x  = element_text(angle=0, size=12)
                ,legend.position="none"
                ,axis.text.y  = element_text(angle=0, size=12))

dev.off()




# Plot leverage distribution conditional on dividend rate


# Drop banks which's balance sheet do not make sense


df_d90 <- df_90[-which(df_90$IDNO %in% c(44,21)),]
df_d00 <- df_00[-which(df_00$IDNO %in% c(6,27)),]
df_d05 <- df_05[-which(df_05$IDNO %in% c(52)),]
df_d10 <- df_10

reg1 <- data.frame("div" = df_d90$Div,"lev" = df_d90$Totalass/(df_d90$Kpaid + df_d90$Rest),"tass" = df_d90$Totalass, "year" = "1890")
reg2 <- data.frame("div" =df_d00$Div,"lev" = df_d00$Totalass/(df_d00$Kpaid + df_d00$Rest),"tass" = df_d00$Totalass, "year" = "1900")
reg3 <- data.frame("div" =df_d05$Div,"lev" = df_d05$Totalass/(df_d05$Kpaid + df_d05$Rest),"tass" = df_d05$Totalass, "year" = "1905")
reg4 <- data.frame("div" =df_d10$Div,"lev" = df_d10$Totalass/(df_d10$Kpaid + df_d10$Rest),"tass" = df_d10$Totalass, "year" = "1910")

reg_t <- rbind(reg1,reg2,reg3,reg4)


# Disgression: regress dividend on log assets and leverage ratio

model <- lm(reg_t$div ~ reg_t$lev + log(reg_t$tass) + factor(reg_t$year), data = reg_t)
model2 <- lm(reg_t$div ~ log(reg_t$tass)+ factor(reg_t$year), data = reg_t)
model3 <- lm(reg_t$div ~ reg_t$lev+ factor(reg_t$year), data = reg_t)

model <- lm(log(reg_t$tass) ~ reg_t$lev + factor(reg_t$year), data = reg_t)

# End disgression

df1 <- reg_t[which(reg_t$div %in% c(0,1,2,3,4)),]
df1$group <- "0-4"
df2 <- reg_t[which(reg_t$div %in% c(5,6,7)),]
df2$group <- "5-7"
df3 <- reg_t[which(reg_t$div %in% c(8,9,10)),]
df3$group <- "8-10"
df4 <- reg_t[which(reg_t$div >= 11),]
df4$group <- "11-"

df_b <- rbind(df1,df2,df3,df4)


cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2")

filename <- paste0("./Output/div_lev.pdf")
pdf(filename,8,5)

ggplot(df_b, aes(group,lev,colour = group)) + 
          geom_violin(draw_quantiles = 0.5) +
          geom_point()+
          scale_colour_manual(values = cbPalette)+
          theme_bw()+
          xlab("Dividend Rate")+
          ylab("Leverage Ratio")+
          scale_x_discrete(name ="Dividend Rate", 
                           limits=c("0-4","5-7","8-10","11-"))+
          theme(axis.title.x = element_text("")
                ,axis.text.x  = element_text(angle=0, size=12)
                ,legend.position="none"
                ,axis.text.y  = element_text(angle=0, size=12))

dev.off()


# Inspect Bank of New Brunswick (highest dividend ratio)

df_90_n <- df_d90[order(df_d90$Totalass,decreasing = F),]
rank_90 <- which(df_90_n$IDNO == 3)/NROW(df_90_n)

df_00_n <- df_d00[order(df_d00$Totalass,decreasing = F),]
rank_00 <- which(df_00_n$IDNO == 3)/NROW(df_00_n)

df_05_n <- df_d05[order(df_d05$Totalass,decreasing = F),]
rank_05 <- which(df_05_n$IDNO == 3)/NROW(df_05_n)

df_10_n <- df_10[order(df_10$Totalass,decreasing = F),]
rank_10 <- which(df_10_n$IDNO == 3)/NROW(df_10_n)




# Capital restriction ####

# Curtis data

cbPalette <- c("#56B4E9", "#009E73")

filename <- paste0("./Output/Capital_rest_2.pdf")
pdf(filename,8,5)

d <- which(df$date <= "1913-12-28" & df$date >= "1891-07-28")
df_temp <- df[d,]
ggplot(df_temp, aes(df$date[d])) + 
          theme_bw()+
          ylab("")+
          xlab("")+
          scale_x_date(breaks = date_breaks("2 years"),labels = date_format("%Y"))+
          geom_rect(aes(xmin=as.Date("1907-06-28"), xmax=as.Date("1908-12-28"), ymin=-Inf, ymax=Inf),fill = "grey96") +
          geom_line(aes(y = maxnotes),colour = cbPalette[1]) + 
          geom_line(aes(y = capital),colour = cbPalette[2]) +
          theme(axis.title.x = element_text("")
                ,axis.text.x  = element_text(angle=0, size=16)
                ,legend.position="none"
                ,axis.text.y  = element_text(angle=0, size=12))

dev.off()

# Create df's with max notes over capital with individual bank data

d <- which(!is.na(df_90$Notes) & !is.na(df_90$Kpaid) & df_90$Kpaid != 0)
df1 <- data.frame(value = df_90$Notes[d]/df_90$Kpaid[d], group = "1890"
                  ,bank = df_90$IDNO[d],k = df_90$Kpaid[d])
d <- which(!is.na(df_00$MaxN) & !is.na(df_00$Kpaid) & df_00$Kpaid != 0)
df2 <- data.frame(value = df_00$MaxN[d]/df_00$Kpaid[d], group = "1900"
                  ,bank = df_00$IDNO[d],k = df_00$Kpaid[d])
d <- which(!is.na(df_05$MaxN) | is.na(!df_05$Kpaid) & df_05$Kpaid != 0)
df3 <- data.frame(value = df_05$MaxN[d]/df_05$Kpaid[d], group = "1905"
                  ,bank = df_05$IDNO[d],k = df_05$Kpaid[d])
d <- which(!is.na(df_10$MaxN) | !is.na(df_10$Kpaid) & df_10$Kpaid != 0)
df4 <- data.frame(value = df_10$MaxN[d]/df_10$Kpaid[d], group = "1910"
                  ,bank = df_10$IDNO[d],k = df_10$Kpaid[d])

df_b <- rbind(df1,df2,df3)

# "Correct" for seemengly error in 1905 data set
max_00 <- max(df_b$value[which(df_b$group == "1900")])
max_05 <- max(df_b$value[which(df_b$group == "1905")])
diff <- max_00 - max_05
df_b$value[which(df_b$group == "1905")] <- df_b$value[which(df_b$group == "1905")] + diff 


cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2")

filename <- paste0("./Output/Capital_rest.pdf")
pdf(filename,8,5)

ggplot(df_b, aes(group,value,colour = group)) + 
          geom_violin() +
          geom_point()+
          theme_bw()+
          xlab("")+
          ylab("Notes-capital ratio")+
          coord_flip()+
          theme(axis.title.x = element_text("")
                ,axis.text.x  = element_text(angle=0, size=16)
                ,legend.position="none"
                ,axis.text.y  = element_text(angle=0, size=12))

dev.off()


# Compute quartiles for year groups

quart_y <- df_b %>% 
          group_by(group) %>% 
          summarise(P25=quantile(value, 0.25),P50=quantile(value, 0.5), P75=quantile(value, 0.75),P90=quantile(value, 0.9))
library(xtable)


xtable(quart_y)

# Entry and exit ####

# Transorm to data.frame
df_e <- as.data.frame(df_e)
# Drop NA columns (banks which appear in var_lab_bankn but not in Beckhart)
df_e <- df_e[ , ! apply( df_e , 2 , function(x) any(is.na(x)) ) ]

# Compute number of entrants, exiters, incumbents, absorptions, liquidations, voluntary liquidations.

turnover <- function(x){y<-c("Incumbents" = length(which(df_e[x,] == 1)),
                                 "Entrants" = length(which(df_e[x,] == 1.1)),
                                 "Liquidations" = length(which(df_e[x,] == 0.2)),
                                 "Absorptions" = length(which(df_e[x,] == 0.1)),
                                 "Voluntary liquidations" = length(which(df_e[x,] == 0.3)))
return(y)}

n_turnover <- matrix(NA, nrow = NROW(df_e),ncol = 5)
for (i in 1:NROW(df_e)){
          n_turnover[i,] <- turnover(i)
}

n_turnover <- as.data.frame(n_turnover)
n_turnover$date <- seq(as.Date("1817-01-28"), as.Date("1929-12-28"), "years")


# Create summary table per decade

n_turnover <- n_turnover[which(n_turnover$date >= "1867-01-28"),]
n_turn_d1 <- colSums(n_turnover[1:10,-c(1,NCOL(n_turnover))])
n_turn_d2 <- colSums(n_turnover[11:20,-c(1,NCOL(n_turnover))])
n_turn_d3 <- colSums(n_turnover[21:30,-c(1,NCOL(n_turnover))])
n_turn_d4 <- colSums(n_turnover[31:40,-c(1,NCOL(n_turnover))])
n_turn_d5 <- colSums(n_turnover[41:50,-c(1,NCOL(n_turnover))])
n_turn_d6 <- colSums(n_turnover[51:60,-c(1,NCOL(n_turnover))])
n_turn_d7 <- rbind(mean(n_turnover[1:10,1])
                   ,mean(n_turnover[11:20,1])
                   ,mean(n_turnover[21:30,1])
                   ,mean(n_turnover[31:40,1])
                   ,mean(n_turnover[41:50,1])
                   ,mean(n_turnover[51:60,1]))

n_turn_d <- rbind(n_turn_d1,n_turn_d2,n_turn_d3,n_turn_d4,n_turn_d5,n_turn_d6)
n_turn_d <- cbind(round(n_turn_d7,0),n_turn_d)
colnames(n_turn_d) <- c("Incumbents","Entrants","Liquidations","Absorptions","Voluntary Liquidations")
rownames(n_turn_d) <- c("1867-1876","1877-1886","1887-1896","1897-1906","1907-1916","1917-1926")



x <- kable(n_turn_d,format =  "latex", booktabs = TRUE, linesep = "")
add_header_above(x,c("", "", "", "Exiters" = 3))



# Dominion reserve ratio ####

between <- which(!is.na(df_dom$ratio) & df_dom$date >= "1871-07-28")

series  <- df_dom$ratio[between]
dates   <- df_dom$date[between] 


filename <- paste0("./Output/Dom_RR.pdf")
pdf(filename,8,5)


par(lwd=1.4,cex=1,cex.lab=0.8)
tsplot <- zoo(series,dates)
plot(tsplot,lwd=2.5,main="",xaxt = "n",yaxt = "n",
     xlab="",ylab="",ylim = c(min(series),max(series,na.rm = T)))
abline(v = as.Date("1914-06-28"))

axis_1 <-seq(dates[1],dates[length(dates)],"year")
axis(1,axis_1 ,format(axis_1,"%Y"), cex.axis = 0.7,xlim=c(dates[1],dates[length(dates)]))
axis(2, at=pretty(series), lab=pretty(series), las=TRUE)
grid (NA,NULL, lty = 4)

dev.off()
# Test homogeneity assumption ####
dflist <- c("df_90","df_91","df_92","df_93","df_94","df_95","df_96","df_97","df_98","df_99","df_00",
            "df_01","df_02","df_03","df_04","df_05","df_06","df_07","df_08","df_09","df_10")
a <- apply(dflist, function(ddf){
  ddf$e <-rowSums(ddf$Kpaid,ddf$Rest,na.rm = T)
  ddf$e[ddf$e == 0] <- NA
  res <- ddf$Culoans/ddf$e
})



df_10$e <-rowSums(data.frame(df_10$Kpaid,df_10$Rest),na.rm = T)
df_10$e[df_10$e == 0] <- NA
res10 <- df_10$CuloansC/df_10$e

comb <- rbind(res91,res92,res93,res94,res95,res96,res97,res98,res99,res00,
              res01,res02,res03,res04,res05,res06,res07,res08,res09,res10)
comb <- as.vector(comb)
comb2 <- rbind(df_91$e,df_92$e,df_93$e,df_94$e,df_95$e,df_96$e,df_97$e,df_98$e,df_99$e,df_00$e,
               df_01$e,df_02$e,df_03$e,df_04$e,df_05$e,df_06$e,df_07$e,df_08$e,df_09$e,df_10$e)
comb2 <- as.vector(comb2)
final <- data.frame(comb2,comb)
final <- na.omit(final)
final<- final[-which(final$comb == 0),]
plot(final$comb2,final$comb)


comb <- rbind(df_95$e,df_96$e,df_97$e,df_98$e,df_99$e)
comb2 <- rbind(df_95$Totalass,df_96$Totalass,df_97$Totalass,df_98$Totalass
,df_99$Totalass)

# Regression test

ggplot(final,aes(x=final$comb2/1000,y=final$comb)) + geom_point(shape = 21,fill = "blue",alpha = 0.5)+
    geom_smooth(method=lm , color="red", se=FALSE)+
  scale_x_continuous(name = "Equity")+
  scale_y_continuous(name = "Loans/Equity")+
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=0, hjust=1))
  
a <- na.omit(data.frame(res04,df_04$e))

ggplot(a,aes(y=a$res04,x=a$df_04.e/1000)) + geom_point(shape = 21,fill = "blue",alpha = 0.5)+
  geom_smooth(method=lm , color="red", se=FALSE)+
  scale_x_continuous(name = "Equity")+
  scale_y_continuous(name = "Loans/Equity")+
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=0, hjust=1))

# Regression test with total assets

ta <-rbind(df_91$Totalass,df_92$Totalass,df_93$Totalass,df_94$Totalass
           ,df_95$Totalass,df_96$Totalass,df_97$Totalass,df_98$Totalass
           ,df_99$Totalass,df_00$Totalass,df_01$Totalass,df_02$Totalass
           ,df_03$Totalass,df_04$Totalass,df_05$Totalass,df_06$Totalass
           ,df_07$Totalass,df_08$Totalass,df_09$Totalass,df_10$Totalass)
ta <- as.vector(ta)
eq <- rbind(df_91$e,df_92$e,df_93$e,df_94$e,df_95$e,df_96$e,df_97$e,df_98$e,df_99$e,df_00$e,
               df_01$e,df_02$e,df_03$e,df_04$e,df_05$e,df_06$e,df_07$e,df_08$e,df_09$e,df_10$e)
eq <- as.vector(eq)
final <- data.frame(eq,ta)
final <- na.omit(final)

ggplot(final,aes(x=final$eq/1000,y=final$ta/1000)) + geom_point(shape = 21,fill = "blue",alpha = 0.5)+
  geom_smooth(method=lm , color="red", se=FALSE)+
  scale_x_continuous(name = "Equity")+
  scale_y_continuous(name = "Assets")+
  theme(text = element_text(size=20),
        axis.text.x = element_text(size = 10,angle=0, hjust=1),
        axis.text.y = element_text(size = 10,angle=0, hjust=1))

options(scipen = 100000)
require(gridExtra)
library(grid)
library(gridExtra)

p1 <- ggplot(df_95,aes(x=df_95$e/1000,y=df_95$Totalass/1000)) + geom_point(shape = 21,fill = "blue",alpha = 1)+
  geom_smooth(method=lm , color="gold4", se=FALSE)+
  scale_x_continuous(name = "")+
  scale_y_continuous(name = "")+
  theme(text = element_text(size=20),
        axis.text.x = element_text(size = 10,angle=0, hjust=1),
        axis.text.y = element_text(size = 10,angle=0, hjust=1))+
          annotate(geom="text", x=12500, y=5000, label=paste("R-squared =",round(summary(lm(df_95$Totalass~df_95$e))$r.square,2))
                   ,color="gold4")
p2 <- ggplot(df_00,aes(x=df_00$e/1000,y=df_00$Totalass/1000)) + geom_point(shape = 21,fill = "blue",alpha = 1)+
  geom_smooth(method=lm , color="gold4", se=FALSE)+
  scale_x_continuous(name = "")+
  scale_y_continuous(name = "")+
  theme(text = element_text(size=20),
        axis.text.x = element_text(size = 10,angle=0, hjust=1),
        axis.text.y = element_text(size = 10,angle=0, hjust=1))+
          annotate(geom="text", x=12500, y=5000, label=paste("R-squared =",round(summary(lm(df_00$Totalass~df_00$e))$r.square,2))
                   ,color="gold4")
p3 <- ggplot(df_05,aes(x=df_05$e/1000,y=df_05$Totalass/1000)) + geom_point(shape = 21,fill = "blue",alpha = 1)+
  geom_smooth(method=lm , color="gold4", se=FALSE)+
  scale_x_continuous(name = "")+
  scale_y_continuous(name = "")+
  theme(text = element_text(size=20),
        axis.text.x = element_text(size = 10,angle=0, hjust=1),
        axis.text.y = element_text(size = 10,angle=0, hjust=1)) +
          annotate(geom="text", x=15000, y=10000, label=paste("R-squared =",round(summary(lm(df_05$Totalass~df_05$e))$r.square,2))
                   ,color="gold4")
p4 <- ggplot(df_10,aes(x=df_10$e/1000,y=df_10$Totalass/1000)) + geom_point(shape = 21,fill = "blue",alpha = 1)+
  geom_smooth(method=lm , color="gold4", se=FALSE)+
  scale_x_continuous(name = "")+
  scale_y_continuous(name = "")+
  theme(text = element_text(size=20),
        axis.text.x = element_text(size = 10,angle=0, hjust=1),
        axis.text.y = element_text(size = 10,angle=0, hjust=1))+
annotate(geom="text", x=20000, y=10000, label=paste("R-squared =",round(summary(lm(df_10$Totalass~df_10$e))$r.square,2))
,color="gold4")


grid.arrange(p1,p2,p3,p4,ncol=2)

filename <- "./Output/lm_e_ta.pdf"
pdf(filename,12,7)
grid.arrange(p1,p2,p3,p4,ncol=2)
dev.off()

# Scatter equity and reserve ratio

res10 <-  (df_01$DdC)/(df_01$e)
plot(df_01$e,res10)


# ROE ####

# Construct real return on equity

T = length(df$date)
e <- df$reserve + df$capital
a <- vector(length = T)
b <- vector(length = T)

for (t in 2:T){
  a[t] <- 1 + (df$reserve[t] - df$reserve[t-1])/e[t-1]
  b[t] <- 1 +   (df_pi$pi[t] - df_pi$pi[t-1])/df_pi$pi[t-1]
  }

c <- a/b

q <- prod(c[-1],na.rm = T)
q <- q^(1/length(a))

# Canada vs US banknotes 

df_pi_aux <-df_pi$pindex[which(df$date <= "1914-01-28" & df$date >= "1874-01-28")]

df_nb_aux  <- df_nb[which(df_nb$date <= "1914-01-01"),]
df_aux <-df$notes[which(df$date <= "1914-01-28" & df$date >= "1874-01-28")]
df_aux <- cbind(df_nb_aux,df_aux)
colnames(df_aux) <- c("date","us","can")
df_aux$us <- df_aux$us/df_pi_aux 
df_aux$can <- df_aux$can/(1000*df_pi_aux) 

# Select series

serie <- df_aux$us
serie2 <- df_aux$can

# Create date serie

dates <- df_aux$date

varnames <- c("US","Canada")
series <- data.frame(serie,serie2)

# Plot series

filename <- "./Output/us_can_notes.pdf"
pdf(filename,8,5)
par(lwd=1.4,cex=1,cex.lab=0.8,mar=c(5, 4, 4, 6) + 0.1)

tsplot <- zoo(serie,dates)
tsplot2 <- zoo(serie2,dates)
# tsplot3 <- zoo(serie3,dates)

p<- plot(tsplot,lwd=2.5,main="",xaxt = "n",yaxt = "n", col = "red",
         xlab="",ylab="",ylim = c(0,max(series$serie,na.rm = T)))
# abline(v = as.Date("1907-1-28"),lty = 1)

axis_1 <-seq(dates[1],dates[length(dates)],"year")
axis(1,axis_1 ,format(axis_1,"%Y"), cex.axis = 1,xlim=c(dates[1],dates[length(dates)]))
axis(2, at=pretty(series$serie), lab=pretty(series$serie), las=TRUE,cex.axis = 1.2)
grid (NA,NULL, lty = 4)

par(new=TRUE)
p2<- plot(tsplot2,lwd=2.5,main="",xaxt = "n",yaxt = "n",
         xlab="",ylab="",ylim = c(0,max(series$serie2,na.rm = T)))
axis(4, at=pretty(series$serie2), lab=pretty(series$serie2), las=TRUE,cex.axis = 1.2)

# lines(tsplot2,col = "red")
# lines(tsplot3,col = "black")

# legend("topleft", varnames ,col = c("blue"),lty = 1,cex=0.7,box.lty=0)


print(p)


dev.off()




# Others ##### 

# Time serie plots

# Select series


serie_df <- "gold"
name <- var_lab$name[which(var_lab$varname == serie_df)]

serie <- df$currgold[which(df$date <= "1913-12-28")]
serie = serie/1000

serie_df_2 <- "capital"
name2 <- var_lab$name[which(var_lab$varname == serie_df_2)]
serie2 <- df$capital[which(df$date <= "1913-12-28")]


# Create date serie

dates <- df$date[which(df$date <= "1913-12-28")]

varnames <- c(name)
series <- c(serie)



# Plot series

filename <- "./Output/gold.pdf"
pdf(filename,8,5)
par(lwd=1.4,cex=1,cex.lab=0.8)

tsplot <- zoo(serie,dates)
# tsplot2 <- zoo(serie2,dates)
# tsplot3 <- zoo(serie3,dates)

p<- plot(tsplot,lwd=2.5,main="",xaxt = "n",yaxt = "n",
         xlab="",ylab="",ylim = c(0,max(series,na.rm = T)))
# abline(v = as.Date("1907-1-28"),lty = 1)
# lines(tsplot2,col = "red")
# lines(tsplot3,col = "black")

# legend("topleft", varnames ,col = c("blue"),lty = 1,cex=0.7,box.lty=0)

axis_1 <-seq(dates[1],dates[length(dates)],"year")
axis(1,axis_1 ,format(axis_1,"%Y"), cex.axis = 0.7,xlim=c(dates[1],dates[length(dates)]))
axis(2, at=pretty(rbind(series,series2)), lab=pretty(rbind(series,series2)), las=TRUE)
grid (NA,NULL, lty = 4)

print(p)


dev.off()




# Compute selected serie first difference and percent change




# Select serie

serie <- df$reserve
between <- which(serie != 0 & df$date <= "1913-12-28")
serie <- serie[between]
date <- df$date[between]


# 1st difference and percent change

serie_c <- vector(length = length(serie))
serie_p <- vector(length = length(serie))
a <- vector(length = length(serie))

for (t in 2:length(serie)){
          serie_c[t] <- serie[t]
          serie_p[t] <- serie[t-1]
          a[t] <- 1+(serie_c[t] - serie_p[t])/e[t-1]}


d_serie <- serie_c - serie_p
pc <- serie_c/serie_p -1
pc <- pc

# Autocorrelation

lm(serie_c ~ serie_p)

# Plot autocorrelation

plot(date,pc,type = "l")
plot(date,d_serie,type = "l")

# Compute ACF function

acf_serie<-Acf(d_serie,main = "")





# Capital restriction and the Crisis of 1907 
####




# Create df's with max notes over capital with individual bank data





d <- which(!is.na(df_97$Notes) & !is.na(df_97$Kpaid) & df_97$Kpaid != 0)
df1 <- data.frame(value = df_97$Notes[d]/df_97$Kpaid[d], group = "1897"
                  ,bank = df_97$IDNO[d],k = df_97$Kpaid[d])

d <- which(!is.na(df_98$Notes) & !is.na(df_98$Kpaid) & df_98$Kpaid != 0)
df2 <- data.frame(value = df_98$Notes[d]/df_98$Kpaid[d], group = "1898"
                  ,bank = df_98$IDNO[d],k = df_98$Kpaid[d])

d <- which(!is.na(df_99$Notes) & !is.na(df_99$Kpaid) & df_99$Kpaid != 0)
df3 <- data.frame(value = df_99$Notes[d]/df_99$Kpaid[d], group = "1899"
                  ,bank = df_99$IDNO[d],k = df_99$Kpaid[d])

d <- which(!is.na(df_00$Notes) & !is.na(df_00$Kpaid) & df_00$Kpaid != 0)
df4 <- data.frame(value = df_00$Notes[d]/df_00$Kpaid[d], group = "1900"
                  ,bank = df_00$IDNO[d],k = df_00$Kpaid[d])

d <- which(!is.na(df_03$Notes) & !is.na(df_03$Kpaid) & df_03$Kpaid != 0)
df5 <- data.frame(value = df_03$Notes[d]/df_03$Kpaid[d], group = "1903"
                  ,bank = df_03$IDNO[d],k = df_03$Kpaid[d])

d <- which(!is.na(df_04$Notes) & !is.na(df_04$Kpaid) & df_04$Kpaid != 0)
df6 <- data.frame(value = df_04$Notes[d]/df_04$Kpaid[d], group = "1904"
                  ,bank = df_04$IDNO[d],k = df_04$Kpaid[d])

d <- which(!is.na(df_05$Notes) & !is.na(df_05$Kpaid) & df_05$Kpaid != 0)
df7 <- data.frame(value = df_05$Notes[d]/df_05$Kpaid[d], group = "1905"
                  ,bank = df_05$IDNO[d],k = df_05$Kpaid[d])

d <- which(!is.na(df_06$Notes) & !is.na(df_06$Kpaid) & df_06$Kpaid != 0)
df8 <- data.frame(value = df_06$Notes[d]/df_06$Kpaid[d], group = "1906"
                  ,bank = df_06$IDNO[d],k = df_06$Kpaid[d])

d <- which(!is.na(df_07$Notes) & !is.na(df_07$Kpaid) & df_07$Kpaid != 0)
df9 <- data.frame(value = df_07$Notes[d]/df_07$Kpaid[d], group = "1907"
                  ,bank = df_07$IDNO[d],k = df_07$Kpaid[d])

d <- which(!is.na(df_08$Notes) & !is.na(df_08$Kpaid) & df_08$Kpaid != 0)
df10 <- data.frame(value = df_08$Notes[d]/df_08$Kpaid[d], group = "1908"
                  ,bank = df_08$IDNO[d],k = df_08$Kpaid[d])

d <- which(!is.na(df_09$Notes) & !is.na(df_09$Kpaid) & df_09$Kpaid != 0)
df11 <- data.frame(value = df_09$Notes[d]/df_09$Kpaid[d], group = "1909"
                  ,bank = df_09$IDNO[d],k = df_09$Kpaid[d])


df_b <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11)







# Plot distribution of note-capital ratio for all years








cbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")

filename <- paste0("./Output/Capital_rest_crisis.pdf")
pdf(filename,8,5)

ggplot(df_b, aes(group,value,colour = group)) + 
          geom_violin() +
          geom_point()+
          theme_bw()+
          xlab("")+
          ylab("Notes-capital ratio")+
          coord_flip()+
          theme(axis.title.x = element_text("")
                ,axis.text.x  = element_text(angle=0, size=16)
                ,legend.position="none"
                ,axis.text.y  = element_text(angle=0, size=12))

dev.off()







# Compute note-capital ratio quartiles for each year






quart_y <- df_b %>% 
          group_by(group) %>% 
          summarise(P25=quantile(value, 0.25),P50=quantile(value, 0.5), P75=quantile(value, 0.75),P90=quantile(value, 0.9))
library(xtable)


xtable(quart_y)






# Plot note-capital ratio paths for individual banks (a time line for each bank)






m <- matrix(nrow = 8,ncol = 59) # Number of years times number of banks
rownames(m) <- c(1900,1903:1909)
colnames(m) <- 1:59

# Fill matrix

for (i in rownames(m)){
          for (j in colnames(m)){
                    
                    if (j %in% df_b$bank[which(df_b$group == i)]){
                              m[i,j] <- df_b$value[which(df_b$bank == j & df_b$group == i)]
                    }
                    else {
                              m[i,j] <- NA
                    }
                           
          }
}


# Plot

filename <- paste0("./Output/Capital_rest_crisis_path.pdf")
pdf(filename,12,12)

m_x <- rep(rownames(m),59)
m_x <-  matrix(m_x,nrow = 8,ncol = 59)

matplot(m_x,m,type = "b",pch = 1,lty = 1)
axis(2, at = seq(0,1,0.025))

dev.off()





# Plot inter-bank secure lending 



df$loan

serie_df <- "loansdeptobankssec"
name <- var_lab$name[which(var_lab$varname == serie_df)]

serie <- df$loansdeptobankssec[which(df$date <= "1913-12-28")]

serie_df <- "loansbankscanadasec"
name2 <- var_lab$name[which(var_lab$varname == serie_df)]

serie2 <- df$loansbankscanadasec[which(df$date <= "1913-12-28")]



dates <- df$date[which(df$date <= "1913-12-28")]

varnames <- c(name,name2)
series <- c(serie,serie2)


# Plot series

filename <- paste0("./Output/interbank_lending.pdf")
pdf(filename,8,5)

par(lwd=1.4,cex=1,cex.lab=0.8)

tsplot <- zoo(serie,dates)
tsplot2 <- zoo(serie2,dates)

p<- plot(tsplot,lty = 2,main="",xaxt = "n",yaxt = "n",
         xlab="",ylab="",ylim = c(0,max(series,na.rm = T)))
lines(tsplot2,lty = 1)

legend("topleft", varnames ,col = c("black"),lty = c(2,1),cex=0.7,box.lty=0)

axis_1 <-seq(dates[1],dates[length(dates)],"year")
axis(1,axis_1 ,format(axis_1,"%Y"), cex.axis = 0.7,xlim=c(dates[1],dates[length(dates)]))
axis(2, at=pretty(series), lab=pretty(series), las=TRUE)
grid (NA,NULL, lty = 4)

dev.off()


# Histogram of losses for note holders and depositors

hist(df_loss$n, breaks=23, xlim=c(0,1), col=rgb(1,0,0,0.5), xlab="height", 
     ylab="nbr of plants", main="distribution of height of 2 durum wheat varieties" )
hist(df_loss$d, breaks=30, xlim=c(0,1), col=rgb(0,0,1,0.5), add=T)
legend("topright", legend=c("Ixos","Primadur"), col=c(rgb(1,0,0,0.5), 
                                                      rgb(0,0,1,0.5)), pt.cex=2, pch=15 )

# Third year seminar plots

alpha <- 0.67
p_lim <- 4

x <- seq(0,alpha,0.01)
cord.x <- c(0,x,alpha) 
cord.y <- c(0,dlnorm(x,(5/2)*0.82^2,0.82),0) 

x2 <- seq(0,p_lim,0.01)
plot(x2, dlnorm(x2,(5/2)*0.82^2,0.82), type = "l", ylab = "density",
     col = 1:3, lty = 1:3)

# Add the shaded area.
polygon(cord.x,cord.y,col='skyblue')
