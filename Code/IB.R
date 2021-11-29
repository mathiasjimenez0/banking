rm(list = ls())
setwd("D:/Dropbox/Angela-Mathias")
source("./Code/Source.R")

source("./Code/Data_to_R.R")
source("./Code/Load.R")

# Entry, exit, mergers ####

a <- Entry_and_Exit[which(Entry_and_Exit$...1 >= 1900 & Entry_and_Exit$...1 <= 1910),colnames(Entry_and_Exit) %in% c("...1","Incumbents","Absorptions","Liquidations","Entrants")]
a$...1 <- seq(as.Date("1900-03-31"), as.Date("1910-03-31"), "years")


I <- data.frame(date = a$...1 , num =  a$Incumbents,group ="I")

L <- data.frame(date = a$...1,num = -a$Liquidations,group = "Liquidations")
A <- data.frame(date = a$...1,num = -a$Absorptions,group = "Absorptions")
E <- data.frame(date = a$...1,num = a$Entrants,group = "Entrants")
a <- rbind(E,L,A)
a$num[which(a$date == "1906-03-31" & a$group == "Liquidations")] <- -1

a$date <- 1900:1910

ggplot(data = a, 
       aes(x =date, y = num, fill = group)) + 
  geom_bar(stat = 'identity',position = position_nudge(x = 0.5)) +
  # scale_fill_hue(l=40, c=85) + 
  # scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual("legend", values = c("Entrants" = "green3", "Liquidations" = "red3", "Absorptions" = "yellow3"))+
  theme(text = element_text(size=20), legend.position="top",legend.title = element_blank())+ 
  # scale_x_continuous(breaks = seq.Date(min(a$date),max(a$date),by = "year"))+
  scale_x_continuous(breaks = seq(1901,1911,1))+
  # scale_x_continuous(breaks = pretty_breaks(10))+
  xlab("")+
  ylab("Number of banks")
  



ggplot(data = df[b,], 
       aes(x = df$date[b], y = df$loansbankscanadasec[b]/1000)) + 
  geom_line(stat = 'identity') +
  scale_y_continuous(name="Inter-bank secured lending ($M)") + 
  theme(text = element_text(size=20))+ 
  # scale_x_continuous(breaks = seq(1900,1910,1))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  xlab("")



# IB exposure through time ####

# Compute aggregate foregin net exposure from individual bs

a <- aggregate(df_IB[,4:12], by = list(df_IB$Date),sum,na.rm = T)
colnames(a) <- c("Date","Totalass","Totalliab","IB_aEW", "IB_aUK","IB_lEW", "IB_lUK","CallOSC", "DOSC", "CuloansOSC")
for (i in 4:ncol(a)){a[,i] <- a[,i]/a[,2]}
a$net <- rowSums(data.frame(a$IB_aEW,a$IB_aUK,-a$IB_lEW,-a$IB_lUK,a$CallOSC,a$CuloansOSC,-a$DOSC),na.rm = T)
plot(a$Date,a$net, ylim = c(0.01,0.15), xlab = "Date", ylab = "series1")

# Compute individual net exposure

df_IB$net <- rowSums(data.frame(df_IB$IB_aEW,-df_IB$IB_lEW,
                                df_IB$IB_aUK,-df_IB$IB_lUK,
                                df_IB$CallOSC,df_IB$CuloansOSC,
                                -df_IB$DOSC),na.rm = T)

# Compute df with banks who appear in all dates from 1890 to 1910

N <- length(var_lab_in$IDNO)
Time <- length(unique(df_all$Date))
survived <- data.frame("IDNO" = rep(0,N), "S" = rep(0,N))
for (i in 1:N){
  if (length(unique(df_IB$Date[which(df_IB$IDNO == i)])) == Time)
  {survived$S[i] <- 1 
  survived$IDNO[i] <- i}
}


# Plot individual IB exposure to Can banks

p <- 0
e <- 0.1
a <- df_IB %>%
  filter(Date <= "1914-01-31", Totalass > Totalliab, Totalass > 0, (IDNO %in% survived$IDNO[survived$S == 1]))  %>%
  group_by(Date,"group_h" = Totalass >= quantile(Totalass,p,na.rm = T) & Totalass <= quantile(Totalass,p+e,na.rm = T)) %>%
  summarise(y = sum(IB_Cass, na.rm = T))

# a <- a[-which(is.na(a$group_h)),]
ggplot(a[which(a$group_h == T),],aes(x=Date, group = group_h, y=y, color = group_h)) +
geom_line() + ylab(p)

a <- a[order(a$group_h,a$Date,a$Totalass),] 


p <- 0
e <- 0.3
a <- df_IB %>%
  filter(Date <= "1914-01-31", Totalass > Totalliab, Totalass > 0
         , IDNO %in% df_IB$IDNO[which(df_IB$Date == "1890-03-31" & 
                                        df_IB$Totalass >= quantile(df_IB$Totalass[which(df_IB$Date == "1890-03-31")],p,na.rm = T) &
                                        df_IB$Totalass <= quantile(df_IB$Totalass[which(df_IB$Date == "1890-03-31")],p+e,na.rm = T))]) %>%
  group_by(Date) %>%
  summarise(y = sum(IB_Cass, na.rm = T))

    ggplot(a,aes(x=Date, y=y)) +
      geom_line() + ylab(p)
  
    
p <- 0.2
e <- 0
a <- df_IB %>%
      filter(Date <= "1914-03-31", Totalass > Totalliab, Totalass > 0)  %>%
      group_by(Date,"group_h" = Totalass >= quantile(Totalass,p,na.rm = T) & Totalass <= quantile(Totalass,p+e,na.rm = T)) %>%
      summarise(y = sum(IB_Cass, na.rm = T))
    
    ggplot(a[which(a$group_h == T),],aes(x=Date, group = group_h, y=y, color = group_h)) +
    geom_line() + ylab(p)
    
    
  
    p <- 0
    e <- 0.2
    a <- df_IB %>%
      filter(Date <= "1914-01-31", Totalass > Totalliab, Totalass > 0
             , IDNO %in% df_IB$IDNO[which(df_IB$Date == "1890-03-31" & 
                                            df_IB$Totalass >= quantile(df_IB$Totalass[which(df_IB$Date == "1890-03-31")],p,na.rm = T) &
                                            df_IB$Totalass <= quantile(df_IB$Totalass[which(df_IB$Date == "1890-03-31")],p+e,na.rm = T))]) %>%
      mutate(y = IB_Cass/Totalass)
    
    ggplot(a,aes(x=Date, y=y, group = IDNO, color = Name)) +
      geom_line() + ylab(p)    
    
    
    
    a <- df_IB %>%
      filter(Date <= "1916-01-31", Totalass > Totalliab, Totalass > 0, IDNO %in% c(30))
    
    ggplot(a) +
      # geom_line(aes(x=Date, y= IB_Cass, group = IDNO, color = Name)) +  theme(legend.position = "none")
    geom_line(aes(x=Date, y= IB_Cass, group = IDNO, color = Name)) 
      # +  geom_line(aes(x=Date, y= IB_Cliab, group = IDNO), color = "red")
    
    
      

# /sum(Totalass,Totalliab,Kpaid,Rest, na.rm = T)
# group_by(Date,"group" = (IDNO  %in% c(1,26,29,30,49,40,38,5,8,9,13,2,10))) %>%
  
    
    
  
# Compute total asset growth to see if there's break at 1900

q <- df_IB[order(df_IB$IDNO),]
q <- mutate(q, agr = log(Totalass) - log(lag(Totalass)))
qq <- filter(q,IDNO %in% 29)
qq[-1,] %>%
  ggplot( aes(x=Date, y=agr, group=IDNO, color=IDNO)) +
  geom_line()


# Explore notes and checks on other banks

b <- which(df$date >= "1872-01-28")
lim <- c(0,0.1)

plot(df$date[b],IB_Cnotes[b],type = "l", ylim = lim)
lines(df$date[b], IB_Cnotes[b])
lines(df$date[b],(df$foreigncurr[b] + df$notesonbanks[b])/df$totalassets[b],col = "red")
lines(df$date[b],(df$chequesonbanks[b])/df$totalassets[b],col = "red")
lines(df$date[b],(df$notesonbanks[b])/df$totalassets[b],col = "red")
lines(df$date[b],df$foreigncurr[b]/df$totalassets[b],col = "red")

b <- which(df$date >= "1880-01-28" & df$date <= "1913-01-28")
lim <- c(0,0.35)
plot(df$date[b],(df$currgold[b] + df$domnotes[b])/df$totalassets[b]
     ,col = "blue", type = "l", ylim = lim)
# lines(df$date[b],(df$currgold[b] + df$domnotes[b])/df$totalassets[b],col = "blue")
lines(df$date[b],df$notes[b]/df$totalassets[b],col = "red")
# lines(df$date[b],(df$deppubdemcanada[b] + df$duetoprov[b] + df$duetodomafterded[b] + 
#                     df$depnotcanada[b])/df$totalassets[b]
#      , col = "green", type = "l")
lines(df$date[b],(df$deppubdemcanada[b] + 
                    df$depnotcanada[b])/df$totalassets[b]
      , col = "green", type = "l")
# lines(df$date[b],(df$duetodomafterded[b] + df$duetoprov[b])/df$totalassets[b]
#      , col = "green2")
lines(df$date[b],df$deppubdemcanada[b]/df$totalassets[b]
     , col = "green")
lines(df$date[b],df$depnotcanada[b]/df$totalassets[b]
     , col = "green")
# lines(df$date[b],(df$domdepfix[b] + df$provdepfix[b])/df$totalassets[b]
#       , col = "green2")



# Re-constructing IB before 1900

# These two are equal to each other before 1900
b <- which(df$date >= "1890-01-28")
plot(df$date[b],IB_notC[b] - IB_notC_dueto[b],type = "l")
lines(df$date[b],(df$callloansnotcanada[b] + df$oloansdiscnotcanada[b])/df$totalassets[b]
      - df$depnotcanada[b]/df$totalassets[b] + IB_notC[b] - IB_notC_dueto[b], col = "blue")
lines(df$date[b],df$callloansnotcanada[b]/df$totalassets[b],col = "red")
lines(df$date[b],df$oloansdiscnotcanada[b]/df$totalassets[b],col = "green4")
lines(df$date[b],df$depnotcanada[b]/df$totalassets[b],col = "yellow4")



# Before 1871 IB_C included notC
b <- which(df$date >= "1890-01-28")
plot(df$date[b],IB_notC[b],type = "l")
lines(df$date[b],IB_Cdep[b], col = "orange4")

# Plots

b <- which(df$date >= "1881-07-28")


filename <- "./Output/IB_notC.pdf"
pdf(filename,12,5)
p_all <- plot(df$date[b],IB_all[b],type = "l", ylim = c(0, max(IB_all[b])), xlab = "", ylab = "")
lines(df$date[b], IB_notC[b], col = "blue")
lines(df$date[b],IB_C[b],col = "red")
legend("topright", legend=c("from banks","from banks not C", "from banks C"),
       col=c("black","blue","red"), lty = 1,cex=1, lwd = 2)
dev.off()

filename <- "./Output/IB_C.pdf"
pdf(filename,12,5)
p_all <- plot(df$date[b],IB_C[b],type = "l", ylim = c(0, max(IB_all[b] - IB_notC[b])), xlab = "", ylab = "")
lines(df$date[b], IB_Cnotes[b], col = "blue")
lines(df$date[b], IB_Cdep[b], col = "red")
legend("topleft", legend=c("from banks C", "notes from banks C", "deposits in banks C"),
       col=c("black","blue","red"), lty=1, cex=1, lwd = 2)
dev.off()


filename <- "./Output/IB_notC_level.pdf"
pdf(filename,12,5)
plot(df$date[b], IB_notC[b]*df$totalassets[b], type = "l", xlab = "", ylab = "", ylim = c(min(IB_notC_dueto[b]*df$totalassets[b]), max(IB_notC[b]*df$totalassets[b])))
lines(df$date[b], IB_notC_dueto[b]*df$totalassets[b], col = "blue")
legend("topleft", legend=c("from banks not C, level","to banks not C, level"),
       col=c("black","blue"), lty=1, cex=1, lwd = 2)
dev.off()




filename <- "./Output/IB_notes_cheques.pdf"
pdf(filename,12,5)
p <- plot(df$date[b],df$foreigncurr[b],type = "l"
          , ylim = c(0, max(df$chequesonbanks[b], na.rm = T))
          , xlab = "", ylab = "")
lines(df$date[b],df$notesonbanks[b], col = "blue")
lines(df$date[b],df$chequesonbanks[b], col = "red")
lines(df$date[b],df$noteschecksonbanks[b], col =  "green")
legend("topleft", legend=c("foreign currency","notes","cheques","notes and cheques"),
       col=c("black","blue","red","green"), lty = 1,cex=1, lwd = 2)
dev.off()


filename <- "./Output/IB_notes_demdep.pdf"
pdf(filename,12,5)
p <- plot(df$date[b],df$notes[b]/df$totalassets[b],type = "l"
        , ylim = c(0,max(df$deppubdemcanada[b]/df$totalassets[b]))
        , xlab = "", ylab = "")
lines(df$date[b],df$deppubdemcanada[b]/df$totalassets[b], col = "blue")
lines(df$date[b],df$depnotcanada[b]/df$totalassets[b], col = "red")
legend("topright", legend=c("notes","demand deposits C", "deposits not C"),
       col=c("black","blue","red"), lty = 1,cex=1, lwd = 2)
dev.off()




# Ovedue/total assets

df_all$od <- rowSums(data.frame(df_all$Overdue,df_all$Overduenotesu,df_all$Overdues,df_all$Overdueu),na.rm = T)

df_aux <- df_all %>% filter(Totalass > 0,!IDNO %in% 46)
df_aux$sec <- rowSums(data.frame(df_aux$Provothdebs,df_aux$Rwysec,as.numeric(df_aux$Secloansx),df_aux$Domdebs),na.rm = T)
df_aux$loans <- rowSums(data.frame(df_aux$Culoans,df_aux$CuloansC,df_aux$CuloansOSC, df_aux$CallOSC,df_aux$CallC,df_aux$Call),na.rm = T)

scatter.smooth(df_aux$Totalass,df_aux$od/(df_aux$Totalass - df_aux$sec))
