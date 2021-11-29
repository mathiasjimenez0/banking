rm(list = ls())
setwd("C:/Users/Mathias/Dropbox/Angela-Mathias")
source("./Code/Source.R")

source("./Code/Data_to_R.R")

source("./Code/Load.R")

source("./Code/Load.R")


filename <- "./Dataset/R/GIS.RData" 
load(filename)

# Start

d <- "1924-01-31"
id <- c(38,40,33)
id <- c(46,47,30,9)
id <- c(1,26,29,10,49)
id <- c(1,26,29,10,5) # banks which L > A
# id <- 7
# x <- 1
# id <- c(1,3,5,9,10,26,29,30,33,38)
# id <- 38
a <- df_IB_join %>%
  # filter(Date <= d, Totalass > Totalliab, Totalass > 0, IDNO %in% id)
filter(Date <= d, Totalass > Totalliab, Totalass > 0,
       # IDNO %in% unique(df_IB_join$IDNO[which(df_IB_join$Rank <= x+2 & df_IB_join$Rank >= x)]) )
         IDNO %in% id )
           # IDNO %in% c(30,38,9,46) )
       # Name == "St J")

ggplot(a) +
  geom_line(aes(x=Date, y=  IB_Cass + IB_Cass_sec  , group = IDNO, color = bank)) +
  geom_line(aes(x=Date, y= IB_Cliab + IB_Cliab_sec, group = IDNO)) + geom_vline(xintercept = as.numeric(as.Date("1908-07-01")), linetype=4)
  # theme(legend.position = "none") 
  # scale_y_continuous(limits = c(0,6e05))


d <- "1909-01-31"
d2 <- "1904-01-31"
id <- c(1)
# id <- c(26,10,5) # banks which L > A
# id <- c(1,29,26,10,5,9,49,38)
# id <- c(38,8,30,33,49,46)
a <- df_IB_join %>%
  filter(Date <= d, Date >= d2, Totalass > Totalliab, Totalass > 0,
         (IDNO %in% id))

ggplot(a) +
  geom_line(aes(x=Date, y=  IB_Cass  , group = IDNO), color = "black") +
  geom_line(aes(x=Date, y=  IB_Cass_sec  , group = IDNO), color = "black",linetype = "dashed") +
  geom_line(aes(x=Date, y=  IB_Cliab_sec  , group = IDNO), color = "red",linetype = "dashed") +
  geom_line(aes(x=Date, y= IB_Cliab, group = IDNO), color = "red") + 
  geom_line(aes(x=Date, y= , group = IDNO), color = "green")

+
  geom_vline(xintercept = as.numeric(as.Date("1907-01-01")), linetype=4) +
  geom_vline(xintercept = as.numeric(as.Date("1907-10-01")), linetype=4) +
  geom_vline(xintercept = as.numeric(as.Date("1908-08-01")), linetype=4)
 

id <- c(46,30)

# id <- c(38,40,33)

id <- c(46,30,47,9)
id <- c(43,42,29)
id <- c(52,56)
id <- c(49,46,11,2)
id <- c(5,3,32)


df_lr_year <- df_lr_year %>% mutate(demliab = rowSums(data.frame(DueDGd,DuePGd,DueDGnet,DuePG,DueDGn,DuePGn,
                                                                 Notes,DdC,Dd,
                                                                 IBdepCliab,IBdepCliaba, IBdepCliabx),na.rm = T))

df_lr_year <- df_lr_year %>% mutate(IB_notC = rowSums(data.frame(DueDGd,DuePGd,DueDGnet,DuePG,DueDGn,DuePGn,
                                                                 Notes,DdC,Dd,
                                                                 IBdepCliab,IBdepCliaba, IBdepCliabx),na.rm = T))

df_IB_join %>%  
  ggplot(aes(x= Date,y= (IB_aEW + IB_aUK) , group = IDNO, color =as.factor(IDNO)  )) +
  geom_line() + ylim(0,0.5)


geo <- st_as_sf(gis, coords = c('lon','lat'))

2,13,15,21,32,50,
,43,33,30,38,42,9,3,40,22,14,46,47,39,53,40,48

# geographic expansion 1900 1907
df_IB_join %>%  
  filter((IDNO %in% c(26,29,49,10,8,5)), IDNO >=1, IDNO <= 60, year(Date) <= 1910, month(Date) == 3) %>% 
  ggplot(aes(x= Date,y= IB_Cass, group = IDNO, color =as.factor(IDNO)  )) +
  geom_line() +
  ylim(0,20)

# high IB after 1910
df_IB_join %>%  
  filter((IDNO %in% c(33,38,40,48,39,59)), IDNO >=1, IDNO <= 60, year(Date) <= 1910) %>% 
  ggplot(aes(x= Date,y= IB_Cass, group = IDNO, color =as.factor(IDNO)  )) +
  geom_line()





id <- c(30)
geo_plot <- geo[which(geo$IDNO %in% id & geo$year %in% c(1907) ),]
ggplot() +
  geom_polygon(data = Can, aes(x=long, y = lat, group = group), fill="grey", alpha=0.5) +
  geom_sf(data = geo_plot)
  geom_point( data=geo_plot, aes(x=lon, y=lat, color = factor(IDNO)), alpha = 0.3)

#

  df_071 %>%  filter(!(IDNO %in% c(1,26)),Date == "1907-01-31") %>% 
    ggplot(aes(x=Totalass,y=(Domdebs + Rwysec)/Totalass)) +
    geom_point() + geom_smooth(method = 'lm', se = F) +
    geom_label(aes(label = IDNO) )
  
  df_lr_year %>%  
    filter(!(IDNO %in% c(1,26)),year %in% c(1904,1907,1910)) %>%
    ggplot(aes(x=Totalass,y=(Specie + DNs + CallOSC + IBdepCass)/Totalass, group = IDNO, color = year)) +
    geom_point() +
    geom_label(aes(label = IDNO) )
  
  
  
  
  
  
  df_lr_year  %>% filter(Date <= "1900-01-31") %>%
    ggplot(aes(x=Totalass,y=overdue/(currloans))) +
    geom_point() + geom_smooth(se = F) +
    geom_label(aes(label = IDNO) )
  
  df_lr_year  %>% filter(Date >= "1901-03-31") %>%
    ggplot(aes(x=Totalass,y=Totalass/(Totalass - Totalliab), group = year, color =as.factor(year)  )) +
    geom_point() + 
    ylim(0,15) +
    # theme(legend.title = element_blank()) +
    theme(legend.position = "none")
  
  df_lr_year  %>% 
    filter(Date <= "1901-03-31") %>%
    ggplot(aes(x=Totalass,y=overdue/(overdue + currloans), group = year, color =as.factor(year)  )) +
    geom_point() + 
    ylim(0,0.05) +
    # theme(legend.title = element_blank()) +
    theme(legend.position = "none")
  
  df_lr_year  %>% 
    filter(Date >= "1901-03-31") %>%
    ggplot(aes(x=Totalass,y=liquid/Totalass, group = year, color =as.factor(year)  )) +
    geom_point() + 
    # ylim(0,0.05) +
    # theme(legend.title = element_blank()) +
    theme(legend.position = "none")
  
  df_lr_year  %>% 
    filter(Date >= "1901-03-31") %>%
    ggplot(aes(x=Totalass,y=currloans/Totalass, group = year, color =as.factor(year)  )) +
    geom_point() + 
    # ylim(0,0.05) +
    # theme(legend.title = element_blank()) +
    theme(legend.position = "none")
  
  
  
  df_all2 <- df_all %>%
    group_by(Date) %>%
    summarise(currloans =  sum(currloans))
  
  
  b <- which(df$date >= "1906-03-28" & df$date >= "1910-03-28")
  
  
  df  %>%
    filter(date <= "1910-03-28", date >= "1905-03-28") %>%
    ggplot() +
    # geom_rect(data = data.frame(xmin = decimal_date(as.Date(c("1907-01-28"))),
    #                             xmax = decimal_date(as.Date(c("1909-01-28"))), 
    #                             ymin=0, ymax=Inf),
    #           aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    #           fill = "grey", alpha = 0.5) +
    geom_vline(xintercept= as.Date("1907-10-28")) +
    geom_line(aes(x=date,y=totalloanscanada))  
  
  
    xlim("1905-03-28","1910-03-28") +
    # theme(legend.title = element_blank()) +
    theme(legend.position = "none")
  
  
  
  
  
    geom_smooth(se = F) +
    geom_label(aes(label = IDNO) )
  
  
    q <- df_IB_join  %>% 
    filter(year(Date) >= 1900,year(Date) <= 1905)
  
  
  IDNO %in% c(40,9,47,4,15,3,2,57,39,37,36,27,18,16,13,7),
  !(IDNO %in% c(1,26,43,33,30,38,42,9,3,40,22,14,46,47,39,53,40,48)), 
  ,year(Date) %in% c(1890,1910)
  year(Date) >= 1900
  !(IDNO %in% c(1,26)),
  
  
z2 <-    q  %>% 
    group_by(IDNO) %>%
    filter(year(Date) >= 1900,year(Date) <= 1905, any(IB_Cliab_sec > 0)) %>%
    ggplot(aes(x=Date,y=IB_Cliab_sec, group = IDNO, color = as.factor(IDNO)  )) +
    # geom_point() +
    geom_line() +
    # geom_smooth(se = F, method = "loess",span = 1) +
    ylim(0,15) +
    xlim(2e6,6e8) +
    geom_label(aes(label = IDNO) )
  
  


  df_lr_year <- df_lr_year %>% mutate(overdue = rowSums(data.frame(Overdue,Overdueu,Overdues,Overduenotesu), na.rm = T))
  df_lr_year <- df_lr_year %>% mutate(currloans = rowSums(data.frame(LoansDG,LoansPG,Munloans,CuloansC ,CuloansOSC,Corploans,Culoans), na.rm = T))
  df_lr_year <- df_lr_year %>% mutate(loans = rowSums(data.frame(overdue,currloans), na.rm = T))
  df_lr_year <- df_lr_year %>% mutate(reserves = rowSums(data.frame(Specie,DNs,IBdepCass,IBdepCassa,IBdepCassx,Nchqobks),na.rm = T))
  df_lr_year <- df_lr_year %>% mutate(secreserves = rowSums(data.frame(CallOSC,Call,CallC),na.rm = T))
  df_lr_year <- df_lr_year %>% mutate(liquid = rowSums(data.frame(reserves,secreserves),na.rm = T))
  df_lr_year <- df_lr_year %>% mutate(secs = rowSums(data.frame(Domdebs,Rwysec,Provothdebs,Secloansx), na.rm = T))
  
  df_lr_year <- df_lr_year %>% mutate(pito = rowSums(data.frame(CallOSC,Call,CallC), na.rm = T))
  df_lr_year <- df_lr_year %>% mutate(sec = rowSums(data.frame(Provothdebs,Secloansx,Domdebs), na.rm = T))
  
  df_lr_year <- df_lr_year %>% mutate(z = sec/(currloans + sec), na.rm = T)
  
  df_lr_year <- df_lr_year %>% mutate(demliab = rowSums(data.frame(DueDGd,DuePGd,DueDGnet,DuePG,DueDGn,DuePGn,
                                                                   Notes,DdC,Dd,NdC,Nd,DOSC,
                                                                   IBdepCliab,IBdepCliaba, IBdepCliabx,IBduetoEW,IBduetoUK),na.rm = T))
  

  df_all <- df_all %>% mutate(overdue = rowSums(data.frame(Overdue,Overdueu,Overdues,Overduenotesu), na.rm = T))
  df_all <- df_all %>% mutate(equity = rowSums(data.frame(Rest,Kpaid), na.rm = T))
  df_all <- df_all %>% mutate(currloans = rowSums(data.frame(LoansDG,LoansPG,Munloans,CuloansC ,CuloansOSC,Corploans,Culoans,
                                                             IBduefrCsec,Loansdir), na.rm = T))
  df_all <- df_all %>% mutate(loans = rowSums(data.frame(CallC,Call,Domdebs,Rwysec,overdue,currloans,Othera,Mortgages,Premises,DepsDGsec,Realestate), na.rm = T))
  df_all <- df_all %>% mutate(loans2 = rowSums(data.frame(overdue,currloans,Call,CallC,Othera,Mortgages,Premises,DepsDGsec,Realestate,CuloansOSC), na.rm = T))
  df_all <- df_all %>% mutate(loans3 = rowSums(data.frame(overdue,currloans,Othera,Mortgages,Premises,DepsDGsec,Realestate,CuloansOSC), na.rm = T))
  
  df_all <- df_all %>% mutate(reserves = rowSums(data.frame(Specie,DNs,IBdepCass,IBdepCassa,IBdepCassx,Nchqobks),na.rm = T))
  df_all <- df_all %>% mutate(secreserves = rowSums(data.frame(CallOSC),na.rm = T))
  df_all <- df_all %>% mutate(secreserves2 = rowSums(data.frame(CallOSC),na.rm = T))
  df_all <- df_all %>% mutate(secreserves3 = rowSums(data.frame(CallOSC,Call,CallC),na.rm = T))
  df_all <- df_all %>% mutate(liquid = rowSums(data.frame(reserves,secreserves),na.rm = T))
  df_all <- df_all %>% mutate(liquid2 = rowSums(data.frame(reserves,secreserves2),na.rm = T))
  df_all <- df_all %>% mutate(liquid3 = rowSums(data.frame(reserves,secreserves3),na.rm = T))
  df_all <- df_all %>% mutate(secs = rowSums(data.frame(Provothdebs,Secloansx), na.rm = T))
  df_all <- df_all %>% mutate(secs2 = rowSums(data.frame(Domdebs,Provothdebs,Secloansx), na.rm = T))
  df_all <- df_all %>% mutate(secs3 = rowSums(data.frame(Provothdebs,Secloansx), na.rm = T))
  
  df_all <- df_all %>% mutate(pito = rowSums(data.frame(Call,CallC,secs), na.rm = T))
  df_all <- df_all %>% mutate(callc = rowSums(data.frame(Call,CallC), na.rm = T))
  df_all <- df_all %>% mutate(sec = rowSums(data.frame(Provothdebs,Secloansx), na.rm = T))
  df_all <- df_all %>% mutate(sec2 = rowSums(data.frame(Provothdebs,Secloansx,Domdebs,Rwysec), na.rm = T))
  df_all <- df_all %>% mutate(pito2 = rowSums(data.frame(Call,CallC,sec2), na.rm = T))
  df_all <- df_all %>% mutate(pito3 = rowSums(data.frame(Call,CallC,sec2,CallOSC), na.rm = T))
  df_all <- df_all %>% mutate(pito4 = rowSums(data.frame(Call,CallC,secs,CallOSC), na.rm = T))
  df_all <- df_all %>% mutate(pito5 = rowSums(data.frame(Provothdebs,-Munloans), na.rm = T))
  df_all <- df_all %>% mutate(pito6 = rowSums(data.frame(Provothdebs,Munloans), na.rm = T))
  
  df_all <- df_all %>% mutate(z = sec/(currloans + sec), na.rm = T)
  
  df_all <- df_all %>% mutate(demliab = rowSums(data.frame(DueDGd,DuePGd,DueDGnet,DuePG,DueDGn,DuePGn,
                                                                   Notes,DdC,Dd,NdC,Nd,DOSC,
                                                                   IBdepCliab,IBdepCliaba, IBdepCliabx,IBduetoEW,IBduetoUK),na.rm = T))
  
  df_all <- df_all %>% mutate(z2 = rowSums(data.frame(IBdepCass,IBdepCassa,IBdepCassx),na.rm = T)   )
  
  df_all <- df_all %>% mutate(z3 = rowSums(data.frame(Specie,DNs,CallOSC),na.rm = T))
  
  df_all <- df_all %>% mutate(demliab2 = rowSums(data.frame(DueDGd,DuePGd,DueDGnet,DuePG,DueDGn,DuePGn,Notes,DdC,Dd,NdC,Nd),na.rm = T))
  
  
  q2 <- df_lr_year %>% select(year,month,IDNO,currloans,overdue,loans,reserves,secreserves,Totalass,liquid,CallOSC,demliab,secs,pito,sec,z,Totalliab,turnover,Domdebs, Rwysec,Provothdebs,Secloansx)
  q2_all <- df_all %>% select(year,month,IDNO,currloans,overdue,loans,reserves,secreserves,Totalass,liquid,CallOSC,demliab,secs,pito,sec,z,Totalliab,turnover,Domdebs, Rwysec,Provothdebs,Secloansx,
                              callc,sec2,pito2,pito3, CuloansOSC,pito4,secs2,liquid2,secreserves2,loans2
                              ,secs3,liquid3,secreserves3,loans3, Munloans, Rwysec, pito5, pito6, demliab2,z2,z3,equity)
  

  # branches and overdue
  
  q <- branches %>% group_by(bank,year) %>% tally()
  q <- left_join(q,var_lab_in[,c(3,4)], by = "bank")
  q <- q %>% filter(bank %in% var_lab_in$bank, year %in% q2$year)

  q_year <- left_join(q,q2, by = c("year","IDNO"))
  q_year <- q_year %>% filter(currloans > 0, overdue > 0, Totalass > 0)
  
  q_all <- left_join(q,q2_all, by = c("year","IDNO"))
  q_all <- q_all %>% filter(currloans > 0, overdue > 0, Totalass > 0)
  
  
  
  filename <- "./Output/overdue_110620"
  pdf(filename,12,5)

  
  q  %>% 
    ggplot(aes(x=n,y=overdue/(overdue + currloans))) +
    geom_point(color="firebrick", size = 3, alpha = 0.5) +
    labs(title = "Number of branches and percentage of overdue loans, pooled years",
      x = "Number of branches", y = "% of overdue loans") +
    # theme_classic() +
    theme(
      title = element_markdown(),
      # subtitle = element_markdown(),
      plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                                margin = margin(10, 10, 10, 10)),
      # plot.subtitle = element_text(size=15, face="bold", hjust = 0.5, 
      #                           margin = margin(10, 10, 10, 10)),
      axis.text.x = element_text(size=12, vjust= 0.5),
      axis.title.x = element_text(size=15, vjust=0.5),
      axis.text.y = element_text(size=12, vjust=0.5),
      axis.title.y = element_text(size=15, vjust=1)
    ) +
    ylim(0,0.12)
  
    
  print(q)
  
  dev.off()
    
  
bp <-  ggplot(data=z,
         aes(x=year, y=value, colour=variable)) +
    geom_line(size = 2) +
    # labs(title = "",
    #      x = "", y = "") +
    # theme_classic() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      # legend.box = "vertical",
      legend.text=element_text(size=12),
      # legend.key.width=unit(4,"cm"),
      legend.key.size = unit(2,"line"),
      title = element_markdown(),
      # subtitle = element_markdown(),
      plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                                margin = margin(10, 10, 10, 10)),
      # plot.subtitle = element_text(size=15, face="bold", hjust = 0.5, 
      #                           margin = margin(10, 10, 10, 10)),
      axis.text.x = element_text(size=15, vjust= 0.5),
      axis.title.x = element_text(size=15, vjust=0.5),
      axis.text.y = element_text(size=12, vjust=0.5),
      axis.title.y = element_text(size=15, vjust=1)
    ) 
  
  
  bp + facet_wrap(~ variable, ncol=2, scales="free")
  
  
  
  
  
  +
    scale_y_continuous(labels = paste0(seq(-10,5,2.5), "M"),
                       breaks = 10^6 * seq(-10,5,2.5)) 
  
  df_all %>%
    group_by(BankName) %>%
    filter(!(IDNO %in% c(1,26))) %>%
    # filter((IDNO %in% c(13,37,36,50,16,7))) %>%
    # filter((IDNO %in% c(1,26,29,8,5,49))) %>%
    # filter(!(IDNO %in% c(13,37,36,50,16,7)), any(IBduetoCsec + IBa > 500000)) %>%
    # filter(year(Date) >= 1906,year(Date) <= 1909) %>%
    # filter((IDNO %in% c(57,13,36,37,50,7,11,21,52,54,59,43,56,55)), !(IDNO %in% c(1,26,29,30,8,49,38,5,10,49,9))) %>%
    ggplot(aes(x=Totalass,y= IBnca/Totalass, group = BankName, color = as.factor(BankName) )) +
    geom_point(size  = 1) +
    theme(legend.position = "none") +
    ylim(0,0.5) +
    xlim(0,6e7) +
    labs(title = "Net inter-bank position during US Panic of 1907",
         x = "", y = "Net position") +
    # theme_classic() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      # legend.box = "vertical",
      legend.text=element_text(size=12),
      # legend.key.width=unit(4,"cm"),
      legend.key.size = unit(2,"line"),
      title = element_markdown(),
      # subtitle = element_markdown(),
      plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                                margin = margin(10, 10, 10, 10)),
      # plot.subtitle = element_text(size=15, face="bold", hjust = 0.5, 
      #                           margin = margin(10, 10, 10, 10)),
      axis.text.x = element_text(size=15, vjust= 0.5),
      axis.title.x = element_text(size=15, vjust=0.5),
      axis.text.y = element_text(size=12, vjust=0.5),
      axis.title.y = element_text(size=15, vjust=1)
    ) +
    scale_y_continuous(labels = paste0(seq(-10,5,2.5), "M"),
                       breaks = 10^6 * seq(-10,5,2.5)) 
  
  
  
    # ggtitle('Temperature')
    theme(plot.title = element_text(size=20, face="bold", 
                                    margin = margin(10, 0, 10, 0))) +
    ylim(0,0.12)
    # theme(legend.title = element_blank()) +
    # geom_smooth(se = F, method = "loess",span = 1) +
    
    
  
    q2 <- q2 %>% filter(currloans > 0, overdue > 0, Totalass > 0)
  
  
  # branches and liquidity
    
    
    
    
     + 
      ylim(0,750000)
  

  q_all  %>% 
    filter( turnover == 1, month == 3) %>%
    ggplot(aes(x=Totalass,y=demliab/(n))) +
    geom_point(color="firebrick", size = 3, alpha = 0.5) + 
    labs(title = "Share of liquid assets, pooled years",
         x = "Number of Branches", y = "Share of liquid assets") +
    # theme_classic() +
  theme(
      title = element_markdown(),
      # subtitle = element_markdown(),
      plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                                margin = margin(10, 10, 10, 10)),
      # plot.subtitle = element_text(size=15, face="bold", hjust = 0.5, 
      #                           margin = margin(10, 10, 10, 10)),
      axis.text.x = element_text(size=12, vjust= 0.5),
      axis.title.x = element_text(size=15, vjust=0.5),
      axis.text.y = element_text(size=12, vjust=0.5),
      axis.title.y = element_text(size=15, vjust=1)
    ) +
    ylim(0,1e6)
  
  
  # duefr
  
years <- c(1900,1907)
  
b <- branches3 %>% filter(year %in% years)

b <- mutate(b, cc = b$place %in% 
                     c("Montreal","Toronto","Halifax","St John","Hamilton","Winnipeg","Victoria",
                       "Quebec","Vancouver","Ottawa"))
b1 <- b %>% group_by(IDNO,year) %>% filter(cc == 1) %>% summarise(CC = n_distinct(place))
b2 <- b %>% group_by(IDNO,year) %>% filter(cc == 0,!(IDNO %in% b1$IDNO)) %>% mutate(CC = 0)
b2 <- b2 %>% select(year,IDNO,CC)

b <- rbind(b1,b2)


c <- q_all %>% filter(year  %in% years, month == 3)
c <- left_join(c,b[,c("CC","IDNO")],by = "IDNO")  

  
  c  %>% 
    # filter(pito > 0, n > 5, Totalass > 0, year >= 1901,!(IDNO %in% c(1,26))) %>%
    filter(turnover == 1,month == 3, year %in% years) %>%
    ggplot(aes(x=n,y = (z2)/(z2 + z3))) +
    geom_point(size = 3)  +
    geom_label(aes(label = CC, fill = factor(year)) ) +
  labs(title = "Share of inter-bank assets over total liquid assets",
       x = "Number of branches", y = "Share of inter-bank assets")  +
    # theme_classic() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      # legend.box = "vertical",
      legend.text=element_text(size=12),
      # legend.key.width=unit(4,"cm"),
      # legend.key.size = unit(2,"line"),
      title = element_markdown(),
      # subtitle = element_markdown(),
      plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                                margin = margin(10, 10, 10, 10)),
      # plot.subtitle = element_text(size=15, face="bold", hjust = 0.5, 
      #                           margin = margin(10, 10, 10, 10)),
      axis.text.x = element_text(size=12, vjust= 0.5),
      axis.title.x = element_text(size=15, vjust=0.5),
      axis.text.y = element_text(size=12, vjust=0.5),
      axis.title.y = element_text(size=15, vjust=1)
    ) 
  

  
  # branches and market assets
  
  # years <- unique(q_all$year)
  years <- c(1901,1907)
  
  b <- branches3 %>% filter(year %in% years)
  b <- mutate(b, cc = b$place %in% 
                c("Montreal","Toronto"))
  b1 <- b %>% group_by(IDNO,year) %>% filter(cc == 1) %>% summarise(CC = n_distinct(place))
  b2 <- b %>% group_by(IDNO,year) %>% filter(cc == 0,!(IDNO %in% b1$IDNO)) %>% mutate(CC = 0)
  b2 <- b2 %>% select(year,IDNO,CC)
  
  b <- rbind(b1,b2)
  
  
  c <- q_all %>% filter(year  %in% years, month == 3)
  c <- left_join(c,b[,c("CC","IDNO")],by = "IDNO")  
  
  
   c  %>% 
    # filter(!(IDNO %in% c(1,26))) %>%
    filter(turnover == 1, year %in% years,month == 3) %>%
    ggplot(aes(x=n,y = (pito2)/(pito2 + loans3))) +
    geom_point(size = 3, alpha = 0.5)  +
     # geom_label(aes(label = CC, fill = factor(CC)) )+
     geom_label(aes(label = CC, fill = factor(year)) )+
     # geom_label(aes(label = IDNO) ) +
     # geom_smooth(se = F, method = 'lm') 
   # ylim(0,0.35) +
     
     labs(title = "Share of marketable over total non-liquid assets",
          x = "Number of branches", y = "Share of marketable assets")  +
     # theme_classic() +
     theme(
       legend.title = element_blank(),
       legend.position = "bottom",
       # legend.box = "vertical",
       legend.text=element_text(size=12),
       # legend.key.width=unit(4,"cm"),
       # legend.key.size = unit(2,"line"),
       title = element_markdown(),
       # subtitle = element_markdown(),
       plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                                 margin = margin(10, 10, 10, 10)),
       # plot.subtitle = element_text(size=15, face="bold", hjust = 0.5, 
       #                           margin = margin(10, 10, 10, 10)),
       axis.text.x = element_text(size=12, vjust= 0.5),
       axis.title.x = element_text(size=15, vjust=0.5),
       axis.text.y = element_text(size=12, vjust=0.5),
       axis.title.y = element_text(size=15, vjust=1)
     ) 
   
   
     
         
         df_all  %>% 
           filter(IDNO == 1) +
         # filter(pito > 0, n > 5, Totalass > 0, year >= 1901,!(IDNO %in% c(1,26))) %>%
         # filter(turnover == 1, year >= 1901,!(IDNO %in% c(1,26))) %>%
         ggplot(aes(x=Date,y = (liquid)/(Totalass))) +
         geom_line()  
   
   +
    
     
      ylim(0,0.5) +
     labs(title = "Share of marketable over total non-reserve assets",
          x = "Number of branches", y = "Share of marketable assets")  +
     # theme_classic() +
     theme(
       title = element_markdown(),
       # subtitle = element_markdown(),
       plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                                 margin = margin(10, 10, 10, 10)),
       # plot.subtitle = element_text(size=15, face="bold", hjust = 0.5, 
       #                           margin = margin(10, 10, 10, 10)),
       axis.text.x = element_text(size=12, vjust= 0.5),
       axis.title.x = element_text(size=15, vjust=0.5),
       axis.text.y = element_text(size=12, vjust=0.5),
       axis.title.y = element_text(size=15, vjust=1)
     ) 
   
  
  branches <- left_join(branches,var_lab_in[,c("bank","IDNO")],by = "bank")
  
  
  
  q <- branches %>% filter(year == 1901) %>%  group_by(IDNO) %>% tally()
  q <- q %>% filter(!(IDNO %in% unique(q$IDNO[-which(q$IDNO %in% dff$IDNO)])))
  
  dff <- df_01
  dff <- dff %>% filter(!(IDNO %in% unique(dff$IDNO[-which(dff$IDNO %in% q$IDNO)])))
  
  
  df_01$nb <- NA
  
  for (i in dff$IDNO){
    df_01$nb[which(df_01$IDNO == i)] <- q$n[which(q$IDNO == df_01$IDNO[which(df_01$IDNO == i)])] 
    }
  
  

  dff2 <- df_071
  q2 <- branches %>% filter(year == 1907) %>%  group_by(IDNO) %>% tally()
  
 q2 <- q2 %>% filter(!(IDNO %in% unique(q2$IDNO[-which(q2$IDNO %in% dff2$IDNO)])))
  dff2 <- dff2 %>% filter(!(IDNO %in% unique(dff2$IDNO[-which(dff2$IDNO %in% q2$IDNO)])))

  
 df_071$nb <- NA
   
 for (i in dff2$IDNO){
   df_071$nb[which(df_071$IDNO == i)] <- q2$n[which(q2$IDNO == df_071$IDNO[which(df_071$IDNO == i)])]  }
  

 a <- rbind(df_01,df_071)
 
 a <- a %>% mutate(currloans = rowSums(data.frame(Call,LoansDG,LoansPG,Munloans,CuloansC,Corploans,Culoans,CuloansOSC), na.rm = T))
 a <- a %>% mutate(demliab = rowSums(data.frame(DueDGd,DuePGd,DueDGnet,DuePG,DueDGn,DuePGn,
                                                                  Notes,DdC,Dd,NdC,Nd,DOSC),na.rm = T))
 
 
   a  %>%
     filter(!(IDNO %in% c(1,26))) %>%
    ggplot(aes(y=demliab ,x= nb, group = Date, color = as.factor(Date))) +
    geom_point() + 
    geom_smooth(se = F, method = "loess") +
    geom_label(aes(label = IDNO) )
  
  
# 
   
df_lr_year <- df_lr_year %>% mutate(overdue = rowSums(data.frame(Overdue,Overdueu,Overdues,Overduenotesu), na.rm = T))
df_lr_year <- df_lr_year %>% mutate(currloans = rowSums(data.frame(Call,LoansDG,LoansPG,Munloans,CuloansC ,CuloansOSC,Corploans,Culoans), na.rm = T))
df_lr_year <- df_lr_year %>% mutate(liquid = rowSums(data.frame(CallOSC,Specie,DNs,IBdepCass,IBdepCassa,IBdepCassx,Nchqobks),na.rm = T))
df_lr_year <- df_lr_year %>% mutate(secs = rowSums(data.frame(Domdebs,Rwysec,Provothdebs,Secloansx), na.rm = T))
df_lr_year <- df_lr_year %>% mutate(demliab = rowSums(data.frame(DueDGd,DuePGd,DueDGnet,DuePG,DueDGn,DuePGn,
                                                                 Notes,DdC,Dd,NdC,Nd,DOSC,
                                                                 IBdepCliab,IBdepCliaba, IBdepCliabx,IBduetoEW,IBduetoUK),na.rm = T))
df_lr_year <- df_lr_year %>% mutate(rr = reserves/demliab)
df_lr_year <- df_lr_year %>% mutate(IB_a = rowSums(data.frame(IBdepCass,IBdepCassa,IBdepCassx),na.rm = T))
df_lr_year <- df_lr_year %>% mutate(IB_l = rowSums(data.frame(IBdepCliab,IBdepCliaba,IBdepCliabx),na.rm = T))


df_all <- df_all %>% mutate(overdue = rowSums(data.frame(Overdue,Overdueu,Overdues,Overduenotesu), na.rm = T))
df_all <- df_all %>% mutate(currloans = rowSums(data.frame(Call,LoansDG,LoansPG,Munloans,CuloansC,CuloansOSC,Corploans,Culoans), na.rm = T))
df_all <- df_all %>% mutate(reserves = rowSums(data.frame(CallOSC,Specie,DNs,IBdepCass,IBdepCassa,IBdepCassx,IBduefrEW,IBduefrUK),na.rm = T))
df_all <- df_all %>% mutate(secs = rowSums(data.frame(Domdebs,Rwysec,Provothdebs,Secloansx), na.rm = T))
# df_all <- df_all %>% mutate(demliab = rowSums(data.frame(DueDGd,DuePGd,DueDGnet,DuePG,DdC,DOSC,Dd,IBdepCliab,IBduetoEW,IBduetoUK,Notes),na.rm = T))
df_all <- df_all %>% mutate(demliab = rowSums(data.frame(DueDGd,DuePGd,DueDGnet,DuePG,DdC,DOSC,Dd,IBdepCliab,IBduetoEW,IBduetoUK,Notes,
                                                                 DueDGn,DuePGn,
                                                                 NdC,Nd,
                                                                 IBdepCliaba, IBdepCliabx),na.rm = T))

df_all <- df_all %>% mutate(IB_a = rowSums(data.frame(IBdepCass,IBdepCassa,IBdepCassx),na.rm = T))


df_lr_year$year <- year(df_lr_year$Date)
df_lr_year$year <- as.numeric(df_lr_year$year)


df_IB_join$z <- 0

for (i in 1:length(df_IB_join$IDNO)){
  if (as.numeric(df_IB_join$Rank[i])  <= 1){
    df_IB_join$z[i] <- 1
  } 
}


df_IB_join$Total_IB <- rowSums(data.frame(df_IB_join$IB_Cass,df_IB_join$IB_Cass_sec),na.rm = T)

df_IB_join2 <- df_IB_join %>% group_by(Date,z) %>% summarise("Interbank deposits" = sum(Total_IB), "Total Assets" = sum(Totalass))


df_IB_join2 %>%  
  # group_by(IDNO >=1) %>%
  filter(year(Date) %in% c(1906,1907,1908)) %>%
  ggplot(aes(x= Date,y= `Interbank deposits`/`Total Assets`, group = z, color =as.factor(z) )) +
  # geom_point() +
  geom_line() +
  theme(legend.title = element_blank())


+
  scale_x_discrete(limits=c("BMO", "rest"))

  geom_boxplot() +
  ylim(-0.05,0.1) +
  geom_smooth(se = F, method = 'lm') +
  geom_label(aes(label = IDNO) )







  # geom_label(aes(label = IDNO) )
  # geom_smooth(se = F, method = "lm")

df_all$year <- year(df_all$Date)
,IBduefrEW,IBduefrUK,IBduefrCsec
,LoansDG,LoansPG

install.packages("growthrates")
library(growthrates)
,CuloansC,Corploans,Culoans
CallC,Call,Munloans

df_all <- df_all %>% mutate(currloans = rowSums(data.frame(CallC,Call,Munloans,CuloansC,Corploans,Culoans,LoansDG,LoansPG), na.rm = T))
df_all <- df_all %>% mutate(currloans2 = rowSums(data.frame(IBdepCass,IBdepCassa,IBdepCassx,IBduefrCsec,LoansDG,LoansPG,CuloansOSC), na.rm = T))

df_all <- df_all %>%
  group_by(IDNO) %>%
  mutate(currloans_g2 = (currloans2 - lag(currloans2))/lag(currloans2) + 1) # growth rate in percent


df_all <- df_all %>%
  group_by(IDNO) %>%
  mutate(currloans_g = (currloans - lag(currloans))/lag(currloans) + 1) # growth rate in percent


df_all <- df_all %>%
  group_by(IDNO) %>%
  mutate(Totalass_g = (Totalass - lag(Totalass))/lag(Totalass) + 1) # growth rate in percent


df_all$equity <- rowSums(data.frame(df_all$Kpaid,df_all$Rest,df_all$Totalass - df_all$Totalliab),na.rm = T)

df_all <- df_all %>%
  group_by(IDNO) %>%
  mutate(equity_g = (equity - lag(equity))/lag(equity) + 1) # growth rate in percent


# evolution

df_all2 <- df_all %>% filter(year >= 1907, year <= 1908,!(Date == "1907-01-31"))
df_all2 <- df_all2[order(df_all2$Date),]

df_all2$equity_g_c <- NA


for (i in 1:NROW(df_all2$IDNO)){
  
  id <- df_all2$IDNO[i]
  aux <- df_all2 %>% filter(IDNO == id)
  
  if (!(df_all2$Date[i] == aux$Date[1])){
    df_all2$equity_g_c[i] <- prod(aux$equity_g[which(aux$Date <= df_all2$Date[i])],na.rm = T)
  } else {
    df_all2$equity_g_c[i] <- 1
  }
  
}

# 57,13,36,37,50,7,11,21,52,54,59,43,56,55


df_all2$group <- NA


for (i in unique(df_all$IDNO)){
  
  if (i %in% c(1,26,5,8,38,29,30,49)){
    
    df_all2$group[which(df_all2$IDNO == i)] <- 1
    
  }else if (i %in% c(9,10,14,40,10,47,33,46,42)){
    
    df_all2$group[which(df_all2$IDNO == i)] <- 2
    
  }else if (i %in% c(2,3,15,39,48,53,51,54)){
    
    df_all2$group[which(df_all2$IDNO == i)] <- 3
  }
  
}




df_all2 %>%
  # filter((IDNO %in% c(1,26,5,8,9,10,29,30,47,49,33,46,38)), year >= 1907 , year <= 1908) %>%
  # filter((IDNO %in% c(2,3,15,39,48,53,51,54)),!(IDNO %in% c(1,26,5,8,38,29,30,49)),!(IDNO %in% c(9,10,14,40,10,47,33,46,42)), year >= 1906 , year <= 1910) %>%
  filter(!(IDNO %in% c(57,13,36,37,50,7,11,21,52,54,59,43,56,55)), !(IDNO %in% c(1,26,29,30,8,49,38,5)),(IDNO %in% c(14,9,33)), year >= 1907 , year <= 1907, group %in% c(1,2)) %>%
  # filter((IDNO %in% c(43,16,11,36,7,53,37,23,58,21,7,6,13,17,18,19,20,22,23,24,25,27)), month(Date) == 3, year >= 1901) %>%
    ggplot(aes(x=Date,y= currloans_g_c, group = IDNO, color = as.factor(IDNO) )) +
  geom_line() 


+ scale_color_manual(breaks = c("1e7", "5e7", "1e8"),
                       values=c("red", "blue", "green"))



  # xlim(0,2.5e7)
  ylim(0.8,3.25) +
  # geom_smooth(se = F, method = "lm") + 
  geom_label(aes(label = IDNO) )



,29,5,9,10,30,33,49,8,38,2,3,14,32,40,42,46


IBduefrEW,IBduefrUK
,IBdepCass,IBdepCassa,IBdepCassx
CallOSC,

df_all <- df_all %>% mutate(reserves = rowSums(data.frame(Specie,DNs),na.rm = T))
df_all <- df_all %>% mutate(reserves2 = rowSums(data.frame(IBdepCass,IBdepCassa,IBdepCassx,Nchqobks),na.rm = T))
df_all <- df_all %>% mutate(reserves3 = rowSums(data.frame(IBdepCass,IBdepCassa,IBdepCassx,Specie,DNs,
                                                           IBduefrEW,IBduefrUK),na.rm = T))

,Nchqobks

df_all <- df_all %>% mutate(reserves3 = rowSums(data.frame(IBduefrEW,IBduefrUK),na.rm = T))




df_all <- df_all %>% mutate(demliab = rowSums(data.frame(DueDGd,DuePGd,DueDGnet,DuePG,DdC,Dd,Notes,
                                                         DueDGn,DuePGn,
                                                         NdC,Nd),na.rm = T))


                                                         IBdepCliaba, IBdepCliabx,IBdepCliab,
                                                         DOSC,IBduetoEW,IBduetoUK),na.rm = T))


df_all <- df_all %>% mutate(TotalassC = rowSums(data.frame(Totalass,-CallOSC),na.rm = T))
df_all <- df_all %>% mutate(TotalliabC = rowSums(data.frame(Totalliab,-DOSC),na.rm = T))


!(IDNO %in% c(1,26,5,8,9,30,33,49,10,29)),
(IDNO %in% c(43,16,11,36,7,53,37,23,58,21,7))
c(1,26,5,8,9,30,33,49,10,29)

# 6,45,16,50,13,36,37
16,50,13,


df_all %>%
  filter(year %in% c(1901), month(Date) == 3) %>%
  # filter((IDNO %in% c(43,16,11,36,7,53,37,23,58,21,7,6,13,17,18,19,20,22,23,24,25,27)), month(Date) == 3, year >= 1901) %>%
  ggplot(aes(x=Totalass,y= (CallOSC)/Totalass, group = IDNO, color = as.factor(IDNO))) +
  geom_point() +
  # xlim(0,2.5e6) +
  # ylim(0,2.5) +
  # geom_smooth(se = F, method = "lm")+ 
  geom_label(aes(label = IDNO) )



df %>% filter(date >= as.Date("1906-01-28"),date <= as.Date("1909-01-28")) %>% ggplot(aes(x = date,y = otherloansdisccanada)) + geom_line()



,14,40,2,3,15,22,32,39,42


11,3,15,39,50,48,13,40,14,42 # estos bancos no ajustaron su CallC
# comment: 26 is weird in CallC

13,15,39,50 # no ajustan CuloansC

c(11,3,15,39,50,48,13,40,14,42)

df_all2 %>%
  # filter((IDNO %in% c(1,26,5,8,9,10,29,30,47,49,33,46,38)), year >= 1907 , year <= 1908) %>%
  filter(!(IDNO %in% c(2,11,51,3,15,39,50,48,56,53,13,43,52,54,37,55,21,36,7,57)),(IDNO %in% c(1,26,5,8,38,29,30,49)),!(IDNO %in% c(9,10,14,40,10,47,33,46,42)), year >= 1907 , year <= 1909) %>%
  # filter((IDNO %in% c(43,16,11,36,7,53,37,23,58,21,7,6,13,17,18,19,20,22,23,24,25,27)), month(Date) == 3, year >= 1901) %>%
  ggplot(aes(x=Date,y= currloans_g_c, group = IDNO, color = as.factor(IDNO))) +
  geom_line() +
  # xlim(0,2.5e7)
  ylim(0,6e6) +
  # geom_smooth(se = F, method = "lm") + 
  geom_label(aes(label = IDNO) )




# geom_label(aes(label = IDNO) )
# geom_smooth(se = F, method = "lm")

#

df_lr_year$year <- year(df_lr_year$Date)

df_lr_year <- df_lr_year %>%
  group_by(IDNO) %>%
  mutate(Totalass_1 = lag(Totalass, order_by=Date),
         Totalliab_1 = lag(Totalliab, order_by=Date))

df_lr_year <- df_lr_year %>%
  mutate(profits = Totalass - Totalliab - (Totalass_1 - Totalliab_1)) 

df_lr_year <- df_lr_year %>%
  mutate(leverage = Totalass/(Totalass - Totalliab), roa = profits/Totalass_1, roe = profits/(Totalass_1 - Totalliab_1)) 


df_all <- df_all %>%
  group_by(IDNO) %>%
  mutate(Totalass_1 = lag(Totalass, order_by=Date),
         Totalliab_1 = lag(Totalliab, order_by=Date),
         year = year(Date))

df_all <- df_all %>%
  mutate(profits = Totalass - Totalliab - (Totalass_1 - Totalliab_1)) 

df_all <- df_all %>%
  mutate(leverage = Totalass/(Totalass - Totalliab), roa = profits/Totalass_1, roe = profits/(Totalass_1 - Totalliab_1)) 




df_lr_year <- df_lr_year %>% mutate(liquid = rowSums(data.frame(Call,CallC,CallOSC,Specie,DNs,IBdepCass,IBdepCassa,IBdepCassx,Nchqobks),na.rm = T))

df_lr_year <- df_lr_year %>% mutate(liquid = rowSums(data.frame(Call,CallC),na.rm = T))

df_lr_year %>%  
  # filter(!(IDNO %in% c(1,26,29,30,10,5,8,49,38,9,33))) %>%
  filter(year >= 1901) %>%
  # filter((IDNO %in% c(17,21,57,36,31,21,23,28,36,41,37,43,27,11,20,45,18,39))) %>%
  # group_by()
  ggplot(aes(x=Totalass,y=liquid/Totalass, group = IDNO)) +
  geom_point() +
  # geom_label(aes(label = IDNO) ) +
  geom_smooth(se = F, span = 1) +
  ylim(0,15)
+
  geom_label(aes(label = IDNO) )+
  geom_smooth(se = F, method = "lm")


a <- df_lr_year %>% filter(IDNO %in% c(6,36,58))


# year %in%  c(1900,1909), 

# (IDNO %in% c(1,26,10,30,33,8,5,49,47))

#  
  
b <- gis %>% group_by(year,IDNO) %>% summarise(NROW(IDNO))





hist(df_all$Totalass[which( !(df_all$IDNO %in% c(1,26)) & df_all$Date == "1910-03-31")]
     , xlim = c(0,6e07), ylim = c(0,10),breaks = 30)



q <- df_all[-which(df_all$IDNO %in% c(1,26)),] %>% group_by(Date) %>% 
  summarise("sd" = sd(Totalass,na.rm = T)/mean(Totalass,na.rm = T))
plot(q$Date,q$sd)




######


df_all <- df_all %>% mutate(reserves = rowSums(data.frame(Specie,DNs),na.rm = T))
df_all <- df_all %>% mutate(IBa = rowSums(data.frame(IBdepCass,IBdepCassa,IBdepCassx),na.rm = T))
df_all <- df_all %>% mutate(IBl = rowSums(data.frame(IBdepCliab,IBdepCliaba,IBdepCliabx),na.rm = T))
df_all <- df_all %>% mutate(IBnca = rowSums(data.frame(IBduefrEW,IBduefrUK),na.rm = T))
df_all <- df_all %>% mutate(IBncl = rowSums(data.frame(IBduetoEW,IBduetoUK),na.rm = T))
df_all <- df_all %>% mutate(K = rowSums(data.frame(Totalass,-Totalliab),na.rm = T))
df_all <- df_all %>% mutate(loans = rowSums(data.frame(CuloansOSC,Corploans,Culoans,LoansDG,LoansPG,Munloans,CuloansC),na.rm = T))
df_all <- df_all %>% mutate(overdue = rowSums(data.frame(Overdue,Overduenotesu,Overdues,Overdueu),na.rm = T))
df_all <- df_all %>% mutate(overdue_rate = overdue/(overdue + loans))
df_all <- df_all %>% mutate(liquid = rowSums(data.frame(Call,CallC,CallOSC,Specie,DNs,IBdepCass,IBdepCassa,IBdepCassx,Nchqobks),na.rm = T))
df_all <- df_all %>% mutate(secs = rowSums(data.frame(Domdebs,Rwysec,Provothdebs,Secloansx), na.rm = T))
df_all <- df_all %>% mutate(demliab = rowSums(data.frame(DueDGd,DuePGd,DueDGnet,DuePG,DueDGn,DuePGn,
                                                                 Notes,DdC,Dd,NdC,Nd,DOSC,
                                                                 IBdepCliab,IBdepCliaba, IBdepCliabx,IBduetoEW,IBduetoUK),na.rm = T))


df_all <- df_all %>% mutate(IBnca = rowSums(data.frame(CallOSC,CuloansOSC),na.rm = T))

df_all <- df_all %>%
  group_by(IDNO) %>%
  mutate(demliab_g = (demliab - lag(demliab))/lag(demliab) + 1,
         reserves_g = (reserves - lag(reserves))/lag(reserves) + 1,
         reserves_g = (liquid - lag(liquid))/lag(liquid) + 1,
         IBa_g = (IBa - lag(IBa))/lag(IBa) + 1,
         IBl_g = (IBl - lag(IBl))/lag(IBl) + 1,
         IBnca_g = (IBnca - lag(IBnca))/lag(IBnca) + 1,
         IBncl_g = (IBncl - lag(IBncl))/lag(IBncl) + 1,
         IBasec_g = (IBduefrCsec - lag(IBduefrCsec))/lag(IBduefrCsec) + 1,
         IBlsec_g = (IBduetoCsec - lag(IBduetoCsec))/lag(IBduetoCsec) + 1,
         nch_g = (Nchqobks - lag(Nchqobks))/lag(Nchqobks) + 1,
         K_g = (K - lag(K))/lag(K) + 1,
         callc_g =(Call - lag(Call))/lag(Call) + 1,
         loans_g =(loans - lag(loans))/lag(loans) + 1,
         overdue_g = (overdue - lag(overdue))/lag(overdue) + 1) # growth rate in percent


df_all$year <- year(df_all$Date)
df_all$year <- as.numeric(df_all$year)
df_all2 <- df_all %>% filter(year >= 1906, year <= 1908,!(Date == "1906-01-31"))
df_all2 <- df_all2[order(df_all2$Date),]

# evolution

df_all2$demliab_g_c <- NA
df_all2$reserves_g_c <- NA
df_all2$liquid_g_c <- NA
df_all2$IBa_g_c <- NA
df_all2$IBl_g_c <- NA
df_all2$IBnca_g_c <- NA
df_all2$IBncl_g_c <- NA
df_all2$IBasec_g_c <- NA
df_all2$IBlsec_g_c <- NA
df_all2$nch_g_c <- NA
df_all2$K_g_c <- NA
df_all2$overdue_g_c <- NA


for (i in 1:NROW(df_all2$IDNO)){
  
  id <- df_all2$IDNO[i]
  aux <- df_all2 %>% filter(IDNO == id)
  
  if (!(df_all2$Date[i] == aux$Date[1])){
    df_all2$reserves_g_c[i] <- prod(aux$reserves_g[which(aux$Date <= df_all2$Date[i])],na.rm = T)
    df_all2$demliab_g_c[i] <- prod(aux$demliab_g[which(aux$Date <= df_all2$Date[i])],na.rm = T)
    df_all2$liquid_g_c[i] <- prod(aux$liquid_g[which(aux$Date <= df_all2$Date[i])],na.rm = T)
    df_all2$IBa_g_c[i] <- prod(aux$IBa_g[which(aux$Date <= df_all2$Date[i])],na.rm = T)
    df_all2$IBl_g_c[i] <- prod(aux$IBl_g[which(aux$Date <= df_all2$Date[i])],na.rm = T)
    df_all2$IBnca_g_c[i] <- prod(aux$IBnca_g[which(aux$Date <= df_all2$Date[i])],na.rm = T)
    df_all2$IBncl_g_c[i] <- prod(aux$IBncl_g[which(aux$Date <= df_all2$Date[i])],na.rm = T)
    df_all2$IBasec_g_c[i] <- prod(aux$IBasec_g[which(aux$Date <= df_all2$Date[i])],na.rm = T)
    df_all2$IBlsec_g_c[i] <- prod(aux$IBlsec_g[which(aux$Date <= df_all2$Date[i])],na.rm = T)
    df_all2$nch_g_c[i] <- prod(aux$nch_g[which(aux$Date <= df_all2$Date[i])],na.rm = T)
    df_all2$K_g_c[i] <- prod(aux$K_g[which(aux$Date <= df_all2$Date[i])],na.rm = T)
    df_all2$overdue_g_c[i] <- prod(aux$overdue_g[which(aux$Date <= df_all2$Date[i])],na.rm = T)
    
    } else {
    df_all2$demliab_g_c[i] <- 1
    df_all2$reserves_g_c[i] <- 1
    df_all2$liquid_g_c[i] <- 1
    df_all2$IBa_g_c[i] <- 1
    df_all2$IBl_g_c[i] <- 1
    df_all2$IBnca_g_c[i] <- 1
    df_all2$IBncl_g_c[i] <- 1
    df_all2$IBasec_g_c[i] <- 1
    df_all2$IBlsec_g_c[i] <- 1
    df_all2$nch_g_c[i] <- 1
    df_all2$K_g_c[i] <- 1
    df_all2$overdue_g_c[i] <- 1
    
      }
  
}


# 13,50,,16,7,57

df_all <- df_all %>% mutate(IB_net = rowSums(data.frame(IBa,IBduefrCsec,-IBl,-IBduetoCsec),na.rm = T))

var_lab_in$BankName[which(var_lab_in$IDNO == 1)] <- "Montreal"
var_lab_in$BankName[which(var_lab_in$IDNO == 26)] <- "Commerce"
var_lab_in$BankName[which(var_lab_in$IDNO == 29)] <- "Merchants"
var_lab_in$BankName[which(var_lab_in$IDNO == 13)] <- "Ontario"
var_lab_in$BankName[which(var_lab_in$IDNO == 50)] <- "Sovereign"

df_all <- left_join(df_all,var_lab_in[,c("IDNO","BankName")], by = "IDNO")


df_all %>%
  group_by(BankName) %>%
  filter(!(IDNO %in% c(1,26))) %>%
  # filter((IDNO %in% c(13,37,36,50,16,7))) %>%
  # filter((IDNO %in% c(1,26,29,8,5,49))) %>%
  # filter(!(IDNO %in% c(13,37,36,50,16,7)), any(IBduetoCsec + IBa > 500000)) %>%
  # filter(year(Date) >= 1906,year(Date) <= 1909) %>%
  # filter((IDNO %in% c(57,13,36,37,50,7,11,21,52,54,59,43,56,55)), !(IDNO %in% c(1,26,29,30,8,49,38,5,10,49,9))) %>%
  ggplot(aes(x=Totalass,y= IBnca/Totalass, group = BankName, color = as.factor(BankName) )) +
  geom_point(size  = 1) +
  theme(legend.position = "none") +
  ylim(0,0.5) +
  xlim(0,6e7) +
  labs(title = "Net inter-bank position during US Panic of 1907",
       x = "", y = "Net position") +
  # theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    # legend.box = "vertical",
    legend.text=element_text(size=12),
    # legend.key.width=unit(4,"cm"),
    legend.key.size = unit(2,"line"),
    title = element_markdown(),
    # subtitle = element_markdown(),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    # plot.subtitle = element_text(size=15, face="bold", hjust = 0.5, 
    #                           margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=15, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1)
  ) +
  scale_y_continuous(labels = paste0(seq(-10,5,2.5), "M"),
                     breaks = 10^6 * seq(-10,5,2.5)) 

