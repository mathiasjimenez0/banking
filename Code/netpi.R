install.packages(reshape2)
library(reshape2)
library(readxl)
source("./Code/Source.R")
load("./Dataset/R/Individual_bs.Rdata")
load("./Dataset/R/References.Rdata")

pi <- read_excel("./Dataset/misc/MT_Jan15_netprofits.xlsx")


# pi <- rename(pi,c("BankName" = "bank"))

pi$BankName <- gsub(".", "", pi$BankName,fixed = T)
pi$BankName <- gsub("(^\\s+)|(\\s+$)", "", pi$BankName)
# var_lab_in$BankName <- gsub(".", "", var_lab_in$BankName,fixed = T)

pi <- melt(data = pi, id.vars = c("BankName"), measure.vars = colnames(pi)[-which(colnames(pi) == "BankName")])
pi <- rename(pi,c("year" = "variable"))
pi <- rename(pi,c("profits" = "value"))

pi <- left_join(pi,var_lab_in[,2:3],by = "BankName")


pi <- pi %>% filter(!(is.na(profits)),!(is.na(IDNO)))
df_lr_year$year <- year(df_lr_year$Date)
df_lr_year$year <- as.factor(df_lr_year$year)

pi <- left_join(pi,df_lr_year[,c("IDNO","year","Totalass","Totalliab","Rest","Kpaid","turnover")], by = c("IDNO","year"))
pi <- filter(pi,!(is.na(Totalass) | is.na(Kpaid)))

pi$year <- as.numeric(as.character(pi$year))

pi$leverage <- NA
pi$roa <- NA
pi$roe <- NA

pi <- pi[order(pi$year),]

pi$Rest[which(is.na(pi$Rest))] <- 0

for (i in unique(pi$IDNO)){
  pi2 <- pi %>% 
    filter(IDNO == i)
  for (j in unique(pi2$year)){
    
    # if (j-1 %in% unique(pi2$year)){
    
      pi3 <- pi2 %>%
        filter(year == j)
        
      pi$leverage[which(pi$year == j & pi$IDNO == i)] <- pi3$Totalass[1]/(pi3$Totalass[1] - pi3$Totalliab[1] + pi3$Totalass[1] - pi3$Totalliab[1] - pi3$Rest[1] - pi3$Kpaid[1])
      
      pi$roa[which(pi$year == j & pi$IDNO == i)] <- pi3$profits[1]/pi3$Totalass[1]
      
      pi$roe[which(pi$year == j & pi$IDNO == i)] <- pi3$profits[1]/(pi3$Totalass[1] - pi3$Totalliab[1] + pi3$Totalass[1] - pi3$Totalliab[1] - pi3$Rest[1] - pi3$Kpaid[1])
      
      
    # }
  
  }
   
}

pi %>% 
  filter(turnover == 1) %>%
  ggplot(aes(x = Totalass, y = roe, group = year  )) + 
  geom_point() + 
  # ylim(0,0.05) +
  # geom_label(aes(label = IDNO) ) +
  # geom_smooth(method = "lm",se = F) +
  ylim(-0.15,0.15) 

pi %>% 
  filter(!(IDNO %in% c(1,26))) %>%
  filter(turnover == 1) %>%
  ggplot(aes(x = Totalass, y = roe, group = year, color = as.factor(year) )) +
  geom_point() +
  # geom_label(aes(label = IDNO) ) +
  ylim(0,0.15) +
  theme(legend.position = "none")


  geom_smooth(span = 0.8, se = F) 


+
  ylim(0,0.1) 

df_lr_year %>% 
  filter(year %in% c(1901,1907)) %>%
  ggplot(aes(x = Totalass, y = Totalass/(Kpaid + Rest), group = IDNO, color = year)) + 
  geom_point() + 
  ylim(-0.1,10) +
  geom_label(aes(label = IDNO) ) +
  geom_smooth(method = "lm", se = F)


a$year <- year(a$Date)

a %>% 
  filter(year %in% c(1901,1907),!(IDNO %in% c(1,26))) %>%
  ggplot(aes(x = nb, y = roe, group = IDNO, color =as.factor(year)  )) + 
  geom_point() + 
  # ylim(-0.1,10) +
  geom_label(aes(label = as.factor(IDNO)  ) ) +
  geom_smooth(method = "lm", se = F)

# filter(!(IDNO %in% c(1,26)),year  == 1910) %>% 