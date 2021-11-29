rm(list = ls())

# SET UP ####

# setwd("D:/Dropbox/JMP")
# source("./Code/Source.R")

# load("./Dataset/R/Interest Rates.RData")
# load("./Dataset/R/cualca.RData")

load("./Dataset/R/GIS.RData")

load("./Dataset/R/Census.RData")

# load("./Dataset/R/Aggregate_bs.RData")

load("./Dataset/R/Individual_bs.RData")


# Compute data coverage given turnover ####

a  <- edges %>% group_by(year) %>% tally()
a <- a$year[which(a$n <= 1)]
edges <- edges %>% filter(!(year %in% a))

b <- as.data.frame(branches) %>%
  group_by(year) %>%
  filter(!(duplicated(bank))) %>%
  select(year,bank)

e <- as.data.frame(edges) %>%
  group_by(year) %>%
  filter(!(duplicated(bank))) %>%
  select(year,bank)

bs <- df_all %>%
  group_by(year) %>%
  filter(!(duplicated(bank))) %>%
  select(year,bank)

b$branches <- 1
e$edges <- 1
bs$sheet <- 1

turnover <- left_join(turnover,var_lab_in[,c("bank","IDNO")], by = "IDNO")
turnover <- left_join(turnover,b, by = c("year","bank")) %>%
  left_join(.,e, by = c("year","bank")) %>%
  left_join(.,bs, by = c("year","bank"))
turnover <- turnover %>% mutate_cond(is.na(branches), branches = 0)
turnover <- turnover %>% mutate_cond(is.na(edges), edges = 0)
turnover <- turnover %>% mutate_cond(is.na(sheet), sheet = 0)
turnover$all <- turnover$branches*turnover$edges*turnover$sheet
turnover$one <- turnover$branches + turnover$edges + turnover$sheet
y <- turnover %>% group_by(year) %>% summarise("y" = sum(one, na.rm = T)) %>% filter(y > 0)
turnover_data <- turnover %>% filter(year %in% y$year)
turnover_data <- turnover_data %>% select(-one)

rm(b,e,bs,y,a)


# fix bugs in Data_to_R ####


# fixed

# branches$UID_CSD_11[which(branches$UID_CD_11 == "BC011c")] <- "BC012002c"
# branches$UID_CD_11[which(branches$UID_CD_11 == "BC011c")] <- "BC012c"
# ref_branches <- as.data.frame(branches) %>% select(-geometry)
# 
# q5 <- df_csd
# q5_aux <- q5 %>% select(c("subdistrict","province","geometry","area"))
# q5_aux2 <-as.data.frame(q5) %>% select(-c("geometry","area"))
# 
# df_csd_aux <- aggregate(q5_aux[,-which(colnames(q5_aux) %in% c("subdistrict","province"))], by =list("subdistrict" = q5_aux$subdistrict, "province" = q5_aux$province), FUN = function(x) sum(x,na.rm = T), do_union = T)
# df_csd_aux2 <- aggregate(q5_aux2[,-which(colnames(q5_aux2) %in% c("subdistrict","province"))], by =list("subdistrict" = q5_aux2$subdistrict, "province" = q5_aux2$province), FUN = function(x) extract2(x,1))
# df_csd <- left_join(df_csd_aux,df_csd_aux2, by = c("subdistrict","province"))
# 
# df_csd <- mutate(df_csd, area = st_area(geometry))
# ref_csd <- as.data.frame(df_csd) %>% select(-geometry)
# 
# 
# 
# edges_canada <- c(edges_canada,"Maritime Provinces, Canada")
# edges_canada <- edges_canada[-which(edges_canada == "Maritime Provinces")]
# edges_foreign <- edges_foreign[-which(edges_foreign %in% c("Novia Scotia","Maritime Provinces, Canada"))]
# 
# edges$where[which(edges$place == "Novia Scotia")] <- "Canada" 
# edges$where[which(edges$place == "Maritime Provinces, Canada")] <- "Canada" 
# 
# edges$where[which(edges$place == "")] <- "Canada" 
# edges$place[which(edges$place == "Novia Scotia")] <- "Nova Scotia" 
# 
# edges_count <- edges %>%  
#   group_by(bank,year,place) %>% 
#   tally()
# edges_count <- edges_count %>% mutate_cond(is.na(place), n = 0) 
# edges_count$where <- NA
# edges_count$where[which(edges_count$place %in% edges_canada)] <- "Canada"
# edges_count$where[which(edges_count$place %in% edges_foreign)] <- "elsewhere"
# 

# # When fixing something for Regina I did spline giving all years as input instead of Census years
# pop$population[which(pop$UID_CD_11 == "SK214" & pop$year %in% c(1900))] <- 0
# pop_aux <- pop[which(pop$UID_CD_11 == "SK214" & pop$year %in% c(1881,1891,1901)),]
# s <- spline(pop_aux$year,pop_aux$population, method = "natural", xmin =  1881, xmax =  1901, n = (1901 - 1881) + 1)
# df_aux <- data.frame("UID_CD_11" = rep("SK214",NROW(s[[1]])), "province" = rep(pr,NROW(s[[1]])),
#                      "district" = rep(d,NROW(s[[1]])), "year" = s[[1]], "population" = s[[2]])
# pop <- pop[-which(pop$UID_CD_11 == "SK214" & pop$year %in% c(1881:1901)),]
# pop <- rbind(pop,df_aux)

#

# df_11_cdcd <- df_11_cdcd %>% select(-UID_CSD_11,-CSD_NO_11,-subdistrict,-TYPE_11,-URB_RUR_11)
# ref_11_cdcd <- ref_11_cdcd %>% select(-UID_CSD_11,-CSD_NO_11,-subdistrict,-TYPE_11,-URB_RUR_11)

#

# df_csd <- filter(df_csd, !(str_detect(subdistrict,"Remainder of province|Unorganized Northern Parts|Northern Unorganized Parts|Rural Parts|Atlin|Fort George|
#                                        Terr W of R 28 N of TP 15 S of TP 28|Unsurveyed parts north of TCR|Parties non organisees")))
df_csd <- filter(df_csd, !(UID_CSD_21 %in% c("ON141046","ON141044","ON141043","ON102105","QC094050")))
df_csd <- filter(df_csd, !(str_detect(province,"North")))

# branches <- filter(branches, !(str_detect(subdistrict,"Remainder of province|Unorganized Northern Parts|Northern Unorganized Parts|Rural Parts|Atlin|Fort George|
#                                        Terr W of R 28 N of TP 15 S of TP 28|Unsurveyed parts north of TCR|Abitibi|Parties non organisees")))
branches <- filter(branches, !(UID_CSD_21 %in% c("ON141046","ON141044","ON141043","ON102105","QC094050")))
branches <- filter(branches, !(str_detect(province,"North")))

# pop_csd <- filter(pop_csd, !(str_detect(subdistrict,"Remainder of province|Unorganized Northern Parts|Northern Unorganized Parts|Rural Parts|Atlin|Fort George|
#                                        Terr W of R 28 N of TP 15 S of TP 28|Unsurveyed parts north of TCR|Abitibi|Parties non organisees")))
pop_csd <- filter(pop_csd, !(UID_CSD_21 %in% c("ON141046","ON141044","ON141043","ON102105","QC094050")))
pop_csd <- filter(pop_csd, !(str_detect(province,"North")))

# change branches

# aux <- branches %>% dplyr::filter(bank == "E Townships", year == 1876, place == "Richmond")
# branches[which(branches$bank == "E Townships" & branches$place == "Richmond" & branches$province == "British Columbia"),-which(colnames(branches) == "year") ] <- aux[,-which(colnames(branches) == "year")]
# rm(aux)
# aux <- branches_cd %>% dplyr::filter(bank == "E Townships", year == 1876, place == "Richmond")
# branches_cd[which(branches_cd$bank == "E Townships" & branches_cd$place == "Richmond" & branches_cd$province == "British Columbia"),-which(colnames(branches_cd) == "year") ] <- aux[,-which(colnames(branches_cd) == "year")]
# rm(aux)


# df_csd$UID_CSD_11[which(df_csd$UID_CSD_11 == "ON094001c")] <- "ON095002c"
# 
# q6 <- df_csd
# q6_aux <- q6 %>% select(c("UID_CSD_11","area","geometry"))
# q6_aux2 <-as.data.frame(q6) %>% select(-c("geometry","area"))
# 
# df_csd_aux <- aggregate(q6_aux[,-which(colnames(q6_aux) %in% c("UID_CSD_11"))], by =list("UID_CSD_11" = q6_aux$UID_CSD_11), FUN = function(x) sum(x,na.rm = T), do_union = T)
# df_csd_aux2 <- aggregate(q6_aux2[,-which(colnames(q6_aux2) %in% c("UID_CSD_11"))], by =list("UID_CSD_11" = q6_aux2$UID_CSD_11), FUN = function(x) extract2(x,1))
# df_csd <- left_join(df_csd_aux,df_csd_aux2, by = c("UID_CSD_11"))
# df_csd$UID_CD_11[which(df_csd$UID_CSD_11 == "ON095002c")] <- "ON095c"
# df_csd$CSD_NO_11[which(df_csd$UID_CSD_11 == "ON095002c")] <- "002c"
# df_csd$CD_NO_11[which(df_csd$UID_CSD_11 == "ON095002c")] <- "095"
# 
# ref_csd <- as.data.frame(df_csd) %>% select(-c("geometry"))
# rm(df_csd_aux,df_csd_aux2)
# 
# 
# 
# branches$UID_CD_11[which(branches$UID_CD_11 == "ON094c")] <- "ON095c"
# branches$UID_CSD_11[which(branches$UID_CSD_11 == "ON094001c")] <- "ON095002c"
# branches$subdistrict[which(branches$subdistrict == "London City_modrur")] <- "London City"
# 
# pop_csd$UID_CD_11[which(pop_csd$UID_CD_11 == "ON094c")] <- "ON095c"
# pop_csd$UID_CSD_11[which(pop_csd$UID_CSD_11 == "ON094001c")] <- "ON095002c"
# 
# 
# aux <- pop_csd[-which(pop_csd$UID_CD_11 == "ON095c"),]
# q6 <- pop_csd[which(pop_csd$UID_CD_11 == "ON095c"),]
# q6_aux <- q6 %>% select(c("year","method","population","population2"))
# q6_aux2 <-as.data.frame(q6) %>% select(-c("population","population2"))
# 
# pop_csd_aux <- aggregate(q6_aux[,-which(colnames(q6_aux) %in% c("year","method"))], by =list("year" = q6_aux$year,"method" = q6_aux$method), FUN = function(x) sum(x,na.rm = T))
# pop_csd_aux2 <- aggregate(q6_aux2[,-which(colnames(q6_aux2) %in% c("year","method"))], by =list("year" = q6_aux2$year,"method" = q6_aux2$method), FUN = function(x) extract2(x,1))
# a <- left_join(pop_csd_aux,pop_csd_aux2, by = c("year","method"))
# pop_csd <- rbind(aux,a)
# rm(pop_csd_aux,pop_csd_aux2,a,aux)
# pop_csd$subdistrict[which(pop_csd$subdistrict == "London City_modrur")] <- "London City"


# branches$UID_CD_11[which(branches$UID_CD_11 == "Kent West")] <- "ON085"
# branches$UID_CSD_11[which(branches$UID_CSD_11 == "Kent West")] <- "ON085007"
# pop_csd$UID_CD_11[which(pop_csd$UID_CD_11 == "Kent West")] <- "ON085"
# pop_csd$UID_CSD_11[which(pop_csd$UID_CSD_11 == "Kent West")] <- "ON085007"
# 
# pop <- pop %>% filter(!(UID_CD_11 == "ON070c"))
# pop_csd <- pop_csd %>% filter(!(UID_CD_11 == "ON070c"))
# df_cd <- df_cd %>% filter(!(UID_CD_11 == "ON070c"))
# df_csd <- df_csd %>% filter(!(UID_CD_11 == "ON070c"))
# ref_cd <- ref_cd %>% filter(!(UID_CD_11 == "ON070c"))
# ref_csd <- ref_csd %>% filter(!(UID_CD_11 == "ON070c"))

# wrong branches

# branches[which(branches$year == 1874 & branches$place == "Winnepeg"),-which(colnames(branches) == "year")] <- branches[which(branches$year == 1876 & branches$place == "Winnipeg City" & branches$bank == "Merchants"),-which(colnames(branches) == "year")] 


# ref_branches <- as.data.frame(branches) %>% select(-geometry)

#

# CC <- CC %>% rename("turnover" = "CC")
# 
# CC_g <- c("Montreal City","Toronto City","Halifax City","St John City","Victoria City","Winnipeg City")
# CC_p <- CC %>% filter(turnover %in% c(1,1.1))
# CC_p$group <- 0
# CC_p <- CC_p %>% mutate_cond(subdistrict %in% CC_g, group = 1)



#

# country_11_cd <- rename(country_11_cd, "UID_CD_11" = "UID_CD_11.x")
# country_11_cd <- country_11_cd %>% select(-UID_CD_11.y)

#

# ppop_csd <- pop_csd %>% filter(!duplicated(pop_csd))

#

# df_91_cd$population[which(df_91_cd$district == "Winnipeg City")] <- 25639
# 
# ref_71_cd <- as.data.frame(df_71_cd) %>% select(-geometry)
# ref_81_cd <- as.data.frame(df_81_cd) %>% select(-geometry)
# ref_91_cd <- as.data.frame(df_91_cd) %>% select(-geometry)
# ref_01_cd <- as.data.frame(df_01_cd) %>% select(-geometry)
# 
# 
# pop_csd$population[which(pop_csd$district == "Winnipeg City" & pop_csd$year == 1891)] <- 25639
# 
# p <- pop_csd %>% filter(subdistrict == "Winnipeg City", year %in% c(1851,1861,1871,1881,1891,1901,1911,1921), method == "linear") %>% select(-method,-population2)
# p$method <- NA
# p$population2 <- NA
# 
# m2 <- p %>% select(UID_CSD_11,population,year) %>% filter(population == 0)
# m2 <- m2 %>% group_by(UID_CSD_11) %>% summarise(max_year_zero = max(year, na.rm = T))
# 
# i <- "MB160002c"
# i_cd <- "MB160c"
# pop_csd_aux <- p %>% filter(UID_CSD_11 == i)
# d <- pop_csd_aux$subdistrict[1]
# d_cd <- pop_csd_aux$district[1]
# pr <- pop_csd_aux$province[1]
# s <- spline(pop_csd_aux$year,pop_csd_aux$population, method = "natural", xmin =  1851, xmax =  1933, n = (1933 - 1851) + 1, ties = min)
# l <- approx(pop_csd_aux$year,pop_csd_aux$population, method = "linear", n = (1921 - 1851) + 1, ties = min)
# df_aux <- data.frame("UID_CSD_11" = rep(i,NROW(s[[1]])),"UID_CD_11" = rep(i_cd,NROW(s[[1]])), "province" = rep(pr,NROW(s[[1]])),
#                      "subdistrict" = rep(d,NROW(s[[1]])),"district" = rep(d_cd,NROW(s[[1]])), "year" = s[[1]], "population" = s[[2]], "population2" = s[[2]], "method" = "natural")
# df_aux_l <- data.frame("UID_CSD_11" = rep(i,NROW(l[[1]])),"UID_CD_11" = rep(i_cd,NROW(l[[1]])), "province" = rep(pr,NROW(l[[1]])),
#                        "subdistrict" = rep(d,NROW(l[[1]])),"district" = rep(d_cd,NROW(l[[1]])), "year" = l[[1]], "population" = l[[2]], "population2" = l[[2]], "method" = "linear")
# 
# if (any(df_aux$population < 0)){
#   aux <- which(df_aux$population < 0)
#   aux <- aux[length(aux)]
#   df_aux$population[1:aux] <- 0}
# if (any(df_aux$population2 < 0)){
#   aux <- which(df_aux$population2 < 0)
#   aux <- aux[length(aux)]
#   df_aux$population2[1:aux] <- 0}
# 
# if (i %in% unique(m2$UID_CSD_11)){
#   df_aux$population2[which(df_aux$year < pmin((m2$max_year_zero[which(m2$UID_CSD_11 == i)] + 10), 1934)  )] <- 0
#   df_aux_l$population2[which(df_aux$year < pmin((m2$max_year_zero[which(m2$UID_CSD_11 == i)] + 10), 1922) )] <- 0
# }
# pp <- rbind(df_aux,df_aux_l)
# pop_csd <- pop_csd %>% filter(!(UID_CSD_11 == "MB160002c"))
# pop_csd <- rbind(pp,pop_csd)


#

# p <- pop %>% filter(year %in% c(1851,1861,1871,1881,1891,1901,1911,1921), province %in% c("British Columbia","Yukon","North Western Territories"), method == "linear") %>% select(-method,-population2)
# 
# p$method <- NA
# p$population2 <- NA
# 
# m2 <- p %>% select(UID_CD_11,population,year) %>% filter(population == 0)
# m2 <- m2 %>% group_by(UID_CD_11) %>% summarise(max_year_zero = max(year, na.rm = T))
# 
# pp <- NULL
# n <- 0
# for (i in unique(p$UID_CD_11)){
#   n = n + 1
#   print(n)  
#   
#   pop_cd_aux <- p %>% filter(UID_CD_11 == i)
#   d <- pop_cd_aux$district[1]
#   pr <- pop_cd_aux$province[1]
#   s <- spline(pop_cd_aux$year,pop_cd_aux$population, method = "natural", xmin =  1851, xmax =  1933, n = (1933 - 1851) + 1, ties = min)
#   l <- approx(pop_cd_aux$year,pop_cd_aux$population, method = "linear", n = (1921 - 1851) + 1, ties = min)
#   df_aux <- data.frame("province" = rep(pr,NROW(s[[1]])),
#                        "district" = rep(d,NROW(s[[1]])), "year" = s[[1]], "population" = s[[2]], "population2" = s[[2]], "method" = "natural")
#   df_aux_l <- data.frame("UID_CD_11" = rep(i,NROW(l[[1]])), "province" = rep(pr,NROW(l[[1]])),
#                          "district" = rep(d,NROW(l[[1]])), "year" = l[[1]], "population" = l[[2]], "population2" = l[[2]], "method" = "linear")
#   
#   if (any(df_aux$population < 0)){
#     aux <- which(df_aux$population < 0)
#     aux <- aux[length(aux)]
#     df_aux$population[1:aux] <- 0}
#   if (any(df_aux$population2 < 0)){
#     aux <- which(df_aux$population2 < 0)
#     aux <- aux[length(aux)]
#     df_aux$population2[1:aux] <- 0}
#   
#   if (i %in% unique(m2$UID_CD_11)){
#     df_aux$population2[which(df_aux$year < pmin((m2$max_year_zero[which(m2$UID_CD_11 == i)] + 10), 1934)  )] <- 0
#     df_aux_l$population2[which(df_aux$year < pmin((m2$max_year_zero[which(m2$UID_CD_11 == i)] + 10), 1922) )] <- 0
#   }
#   
#   
#   pp <- rbind(pp,df_aux,df_aux_l)
# }



#

# distr <- c("Vancouver City","Victoria City","Winnipeg City","St John City","Halifax City","Ottawa City",
#            "Toronto City","Montreal City","Quebec City","Trois Rivieres City",
#            "Hamilton City","London City","Kingston City","Windsor City")
# df_11_cdcd <- df_11_cdcd %>% mutate_cond(district %in% distr,UID_CD_11 = paste0(UID_CD_11,"c"))
# ref_11_cdcd <- as.data.frame(df_11_cdcd) %>% select(-geometry)
# manuf_11_cd <- manuf_11_cd %>% mutate_cond(district %in% distr,UID_CD_11 = paste0(UID_CD_11,"c"))
# country_11_cd <- country_11_cd %>% mutate_cond(district %in% distr,UID_CD_11 = paste0(UID_CD_11,"c"))

#

# trains_place <- st_transform(trains_place,crs = 32610)

# trains_place$CNSTRCTD <- NA
# trains_place$ABNDND <- NA

# # trains_buff <- st_buffer(trains,dist = 16100) # 10 miles from railway
# # trains_p_buff <- st_buffer(trains_place,dist = 18024) # 11.2 miles radius from train station
# 
# id <- st_intersects(trains_place,trains)
# 
# for (i in 1:length(trains_place$OBJECTID)){
#   if (length(id[[i]]) > 0){
#     trains_place$CNSTRCTD[i] <- trains$CNSTRCTD[id[[i]]]
#     trains_place$ABNDND[i] <- trains$ABNDND[id[[i]]]
#     # trains_p_buff$CNSTRCTD[i] <- trains$CNSTRCTD[id[[i]]]
#     # trains_p_buff$ABNDND[i] <- trains$ABNDND[id[[i]]]
#   } else{
#     trains_place$CNSTRCTD[i] <- trains$CNSTRCTD[which.min(st_distance(trains$geometry, trains_place$geometry[i]))]
#     trains_place$ABNDND[i] <- trains$ABNDND[which.min(st_distance(trains$geometry, trains_place$geometry[i]))]
#     # trains_p_buff$CNSTRCTD[i] <- trains$CNSTRCTD[which.min(st_distance(trains$geometry, trains_place$geometry[i]))]
#     # trains_p_buff$ABNDND[i] <- trains$ABNDND[which.min(st_distance(trains$geometry, trains_place$geometry[i]))]
#   }
# }

#


#
# manuf_csd$sales[which(manuf_csd$year == 1911 & manuf_csd$subdistrict == "Montreal City")] <- 166296972
manuf_cd <- rename(manuf_cd,c("number" = "establishments","labour" = "employees","sales" = "value")) # it's already in crisis_aggregate
# manuf_cd$sales[which(manuf_cd$year == 1911 & manuf_cd$district == "Montreal City")] <- 166296972


# CLEAN #####

df_all$Date[which(month(df_all$Date) == 12 & df_all$year <= 1867)] <- as.Date(paste(df_all$year[which(month(df_all$Date) == 12 & df_all$year <= 1867)],3,31, sep = "-"))

pop <- pop %>% filter(method == "linear")
pop_csd <- pop_csd %>% filter(method == "linear")

# CLEAN ####


# branches <- branches %>% filter(!(year %in% c(1901:1904,1906:1909))) # unccomment this if you want to have only 5 year intervals

branches$bank <- gsub(" Of ", " of ", branches$bank,fixed = T)
branches <- left_join(branches,var_lab_in[,which(colnames(var_lab_in) %in% c("IDNO","bank"))],by = c("bank"))
# branches <- left_join(branches,ref_21_cd[,which(colnames(ref_21_cd) %in% c("district","province","population","pop_01","value",
#                                                                            "field crops, fruits and vegetables and nursery",
#                                                                            "live stock sold in year and total animal products",
#                                                                            "land_buildings_implements","live stock","capital",
#                                                                            "employees","weeks of labour, number",
#                                                                            "cost of hired labour","wages","cost"))],
#                       by = c("district","province"))
branches <- left_join(branches,pop[,which(colnames(pop) %in% c("UID_CD_21","year","population"))],
                      by = c("UID_CD_21","year"))

# branches <- rename(branches, "s1_c" = "field crops, fruits and vegetables and nursery", "s2_c" = "live stock sold in year and total animal products", "s_m" = "value",
#                    "k1_c" = "land_buildings_implements", "k2_c" = "live stock", "k_m" = "capital","L_m" = "employees","weeksL_c" = "weeks of labour, number",
#                    "wages_c" = "cost of hired labour","wages_m" = "wages","cost_m" = "cost")


# branches <- branches %>% filter(!(is.na(k_m)),(weeksL_c > 0))

# q_b <- as.data.frame(branches) %>%
#   select(-geometry) %>%
#   filter(!is.na(IDNO))
q_b <- as.data.frame(branches) %>%
  select(-geometry)
q_b <- left_join(q_b,pop_csd[,c("UID_CSD_21","year","population")], by = c("UID_CSD","year"))


# q_b$year[which(q_b$year == 1880)] <- 1882



df_all <- df_all %>% mutate(reserves = rowSums(data.frame(Specie,DNs),na.rm = T))

df_all <- df_all %>% mutate(callosc = rowSums(data.frame(CallOSC),na.rm = T))

df_all <- df_all %>% mutate(secs = rowSums(data.frame(Domdebs,Provothdebs,Secloansx,Rwysec,LoansDG,LoansPG), na.rm = T))

df_all <- df_all %>% mutate(callc = rowSums(data.frame(Call,CallC), na.rm = T))

df_all <- df_all %>% mutate(overdue = rowSums(data.frame(Overdue,Overdueu,Overdues,Overduenotesu), na.rm = T))

df_all <- df_all %>% mutate(currloans = rowSums(data.frame(LoansDG,LoansPG,Munloans,CuloansC,Corploans,Culoans), na.rm = T))
df_all <- df_all %>% mutate(currloans2 = rowSums(data.frame(Munloans,CuloansC,Corploans,Culoans), na.rm = T))


df_all <- df_all %>% mutate(iba_sec = rowSums(data.frame(IBduefrCsec),na.rm = T))

df_all <- df_all %>% mutate(iba = rowSums(data.frame(IBdepCass,IBdepCassa,IBdepCassx),na.rm = T))

# df_all <- df_all %>% mutate(nchqobks = rowSums(data.frame(Nchqobks),na.rm = T))

df_all <- df_all %>% mutate(oa = rowSums(data.frame(Othera,Mortgages,Premises,DepsDGsec,Realestate,Nchqobks), na.rm = T))
df_all <- df_all %>% mutate(othera = rowSums(data.frame(Othera), na.rm = T))

df_all <- df_all %>% mutate(rowa = rowSums(data.frame(CuloansOSC),na.rm = T))


df_all <- df_all %>% mutate(demliab = rowSums(data.frame(DueDGd,DuePGd,DueDGnet,DuePG,Notes,DdC,Dd),na.rm = T))
df_all <- df_all %>% mutate(timeliab = rowSums(data.frame(DueDGn,DuePGn,NdC,Nd),na.rm = T))
df_all <- df_all %>% mutate(demliab2 = rowSums(data.frame(Notes,DdC,Dd),na.rm = T))
df_all <- df_all %>% mutate(timeliab2 = rowSums(data.frame(NdC,Nd),na.rm = T))

df_all <- df_all %>% mutate(undp = rowSums(data.frame(Totalass,-Totalliab,-Kpaid,-Rest), na.rm = T))
df_all <- df_all %>% mutate(eq = rowSums(data.frame(Rest,Kpaid,undp), na.rm = T))


df_all <- df_all %>% mutate(ibl_sec = rowSums(data.frame(IBduetoCsec),na.rm = T))

df_all <- df_all %>% mutate(ibl = rowSums(data.frame(IBdepCliab,IBdepCliaba,IBdepCliabx),na.rm = T))


df_all <- df_all %>% mutate(ol = rowSums(data.frame(Otherliab),na.rm = T))

df_all <- df_all %>% mutate(rowl = rowSums(data.frame(DOSC),na.rm = T))

df_all <- df_all %>% mutate(iba_row = rowSums(data.frame(IBduefrUK,IBduefrEW),na.rm = T))
df_all <- df_all %>% mutate(ibl_row = rowSums(data.frame(IBduetoUK,IBduetoEW),na.rm = T))

# ROW 1900 ####


# # row net always in duefrEW, TA is with net row. Discard row accounts
# df_all <- df_all %>% mutate(ROW_net = rowSums(data.frame(IBduefrEW,ROWA,-DOSC),na.rm = T))
# # df_all <- df_all %>% mutate(IBduefrUK_net = rowSums(data.frame(IBduefrUK,-SECOSC),na.rm = T)) # substract predicted foriegn secs for before 1900 which was included in frUK
# # df_all <- df_all %>% mutate(secs_net = rowSums(data.frame(secs,SECOSC),na.rm = T)) # add predicted foriegn secs for before 1900 which was included in frUK
# df_all <- df_all %>% mutate(TA_net = rowSums(data.frame(Totalass,-DOSC),na.rm = T))
# df_all <- df_all %>% mutate(TL_net = rowSums(data.frame(Totalliab,-DOSC),na.rm = T))
# 
# # row accounts always in domestic accounts. Discard row accounts
# df_all <- df_all %>% mutate(CUL = rowSums(data.frame(currloans,CuloansOSC),na.rm = T))
# df_all <- df_all %>% mutate(CALL = rowSums(data.frame(callc,CallOSC),na.rm = T))
# df_all <- df_all %>% mutate(DEML = rowSums(data.frame(demliab,DOSC),na.rm = T))

# row accounts nowhere, TA without row, discard row accounts.
# df_all <- df_all %>% mutate(TA_dom = rowSums(data.frame(Totalass,-ROWA,-SEC0SC),na.rm = T))

# df_all  %>% filter(IDNO == 26, year >= 1900, year <= 1901) %>%
#   ggplot(aes(x=Date,y=currloans/(currloans + rowa)), color= "red") +
#   # ggplot(aes(x=Date,y=currloans/(currloans + rowa + callosc)), color= "red") +
#   geom_point() + 
#   # geom_point(aes(x=Date,y=rowa/(currloans + rowa + callosc)), color= "green")+
#   geom_point(aes(x=Date,y=rowa/(currloans + rowa)), color= "green")+
#   geom_point(aes(x=Date,y=callosc/(callc + callosc)), color= "purple")+
#   # geom_point(aes(x=Date,y=callosc/(currloans + rowa + callosc)), color= "purple")+
#   geom_point(aes(x=Date,y=demliab/(demliab + rowl)), color= "blue")+ 
#   geom_point(aes(x=Date,y=rowl/(demliab + rowl)), color= "gold")     + 
#   geom_point(aes(x=Date,y=callc/(callc + callosc)), color= "grey")
# 
# df_all  %>% filter(IDNO == 26, year >= 1900, year <= 1901) %>%
#   ggplot(aes(x=Date,y=currloans/(currloans + rowa)), color= "red") +
#   # ggplot(aes(x=Date,y=currloans/(currloans + rowa + callosc)), color= "red") +
#   geom_point() + 
#   # geom_point(aes(x=Date,y=rowa/(currloans + rowa + callosc)), color= "green")+
#   geom_point(aes(x=Date,y=rowa/(currloans + rowa)), color= "green")+
#   geom_point(aes(x=Date,y=callosc/(callc + callosc)), color= "purple")+
#   # geom_point(aes(x=Date,y=callosc/(currloans + rowa + callosc)), color= "purple")+
#   geom_point(aes(x=Date,y=demliab/(demliab + rowl)), color= "blue")+ 
#   geom_point(aes(x=Date,y=rowl/(demliab + rowl)), color= "gold")     + 
#   geom_point(aes(x=Date,y=callc/(callc + callosc)), color= "grey")

# df_all  %>% filter(IDNO == 1, year <= 1910, year >= 1900) %>%
#   ggplot(aes(x=Date,y=Totalass/Totalass)) +
#   geom_point()+
#   geom_point(aes(x=Date,y=callosc/Totalass), color= "green")+
#   geom_point(aes(x=Date,y=DOSC/Totalass), color= "blue")+
#   geom_point(aes(x=Date,y=rowa/Totalass), color= "yellow2")
# +
#   geom_point(aes(x=Date,y=IBduefrUK/Totalass), color= "gold3")+
#   geom_point(aes(x=Date,y=IBduetoEW/Totalass), color= "purple")+
#   geom_point(aes(x=Date,y=IBduetoUK/Totalass), color= "pink2")
# 
# df_all  %>% filter(IDNO == 1, year >= 1880, year <= 1905) %>%
#   ggplot(aes(x=Date,y=IBduefrEW)) +
#   geom_point() +
#   geom_point(aes(x=Date,y=IBduefrUK), color= "green")  +
#   # geom_point(aes(x=Date,y=IBduefrUK + IBduefrEW + callosc + rowa - rowl), color= "red") +
#   geom_point(aes(x=Date,y=callosc), color= "blue") +
#   geom_point(aes(x=Date,y=rowa), color= "gold2") +
#   geom_point(aes(x=Date,y=rowl), color= "purple")


a1 <- df_all %>% filter(year == 1900, month == (8), IDNO %in% c(5,29,3)) %>%
  summarise(Date = Date,IDNO = IDNO, p_dem = rowl/(demliab + rowl), p_call = callosc/(callc + callosc), p_cul = rowa/(currloans + rowa),
            p_curr_cul = rowa/(currloans + rowa + callosc), p_curr_call = callosc/(currloans + rowa + callosc) )

a2 <- df_all %>% filter(year == 1900, month == (7), IDNO %in% c(26,49)) %>%
  summarise(Date = Date,IDNO = IDNO, p_dem = rowl/(demliab + rowl), p_call = callosc/(callc + callosc), p_cul = rowa/(currloans + rowa),
            p_curr_cul = rowa/(currloans + rowa + callosc), p_curr_call = callosc/(currloans + rowa + callosc),
            p_ta = oa/Totalass, p_tl = ol/Totalliab)

a3 <- df_all %>% filter(year == 1900, month == (8), IDNO %in% c(8)) %>%
  summarise(Date = Date,IDNO = IDNO, p_dem = rowl/(demliab + rowl), p_call = callosc/(callc + callosc), p_cul = rowa/(currloans + rowa),
            p_curr_cul = rowa/(currloans + rowa + callosc), p_curr_call = callosc/(currloans + rowa + callosc),
            p_ta = oa/Totalass, p_tl = ol/Totalliab)

# a4 <- df_all %>% filter(year == 1900, month %in% c(6,7), IDNO %in% c(1)) %>%
#   summarise(Date = Date,IDNO = IDNO, p_frew = IBduefrEW/(IBduefrEW + Lag(IBduefrEW)),
#             p_call = callosc/(callosc + rowa - rowl), p_cul = rowa/(callosc + rowa - rowl), p_dosc = rowl/(callosc + rowa - rowl),
#             p_sec = secs - Lag(secs))

a4 <- df_all %>% filter(year == 1900, month %in% c(6,7), IDNO %in% c(1)) %>%
  summarise(Date = Date,IDNO = IDNO, p_frewuk = iba_row/(iba_row + callosc + rowa - rowl),
            p_call = callosc/(iba_row + callosc + rowa - rowl), p_cul = rowa/(iba_row + callosc + rowa - rowl), p_dosc = rowl/(iba_row + callosc + rowa - rowl),
            p_sec = secs - Lag(secs))



df_all$rowa[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 5)] <- df_all$currloans[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 5)]*a1$p_curr_cul[which(a1$IDNO == 5)]
df_all$callosc[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 5)] <- df_all$currloans[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 5)]*a1$p_curr_call[which(a1$IDNO == 5)]
df_all$rowl[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 5)] <- df_all$demliab[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 5)]*a1$p_dem[which(a1$IDNO == 5)]
df_all$currloans[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 5)] <- df_all$currloans[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 5)]*(1 - a1$p_curr_call[which(a1$IDNO == 5)] - a1$p_curr_cul[which(a1$IDNO == 5)])
df_all$demliab[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 5)] <- df_all$demliab[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 5)]*(1 -  a1$p_dem[which(a1$IDNO == 5)])

df_all$rowa[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 3)] <- df_all$currloans[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 3)]*a1$p_curr_cul[which(a1$IDNO == 3)]
df_all$callosc[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 3)] <- df_all$currloans[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 3)]*a1$p_curr_call[which(a1$IDNO == 3)]
df_all$rowl[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 3)] <- df_all$demliab[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 3)]*a1$p_dem[which(a1$IDNO == 3)]
df_all$currloans[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 3)] <- df_all$currloans[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 3)]*(1 - a1$p_curr_call[which(a1$IDNO == 3)] - a1$p_curr_cul[which(a1$IDNO == 3)])
df_all$demliab[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 3)] <- df_all$demliab[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 3)]*(1 -  a1$p_dem[which(a1$IDNO == 3)])

df_all$rowa[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 29)] <- df_all$currloans[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 29)]*a1$p_cul[which(a1$IDNO == 29)]
df_all$callosc[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 29)] <- df_all$callc[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 29)]*a1$p_call[which(a1$IDNO == 29)]
df_all$rowl[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 29)] <- df_all$demliab[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 29)]*a1$p_dem[which(a1$IDNO == 29)]
df_all$currloans[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 29)] <- df_all$currloans[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 29)]*(1 - a1$p_cul[which(a1$IDNO == 29)])
df_all$callc[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 29)] <- df_all$callc[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 29)]*(1 - a1$p_call[which(a1$IDNO == 29)])
df_all$demliab[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 29)] <- df_all$demliab[which(df_all$Date < unique(a1$Date) & df_all$IDNO == 29)]*(1 -  a1$p_dem[which(a1$IDNO == 29)])


df_all$rowa[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)] <- df_all$currloans[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)]*a2$p_cul[which(a2$IDNO == 26)]
df_all$callosc[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)] <- df_all$callc[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)]*a2$p_call[which(a2$IDNO == 26)]
df_all$rowl[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)] <- df_all$demliab[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)]*a2$p_dem[which(a2$IDNO == 26)]
df_all$Totalass[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)] <- df_all$Totalass[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)] + 
  df_all$rowa[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)] + 
  df_all$callosc[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)]
df_all$Totalliab[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)] <- df_all$Totalliab[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)] + 
  df_all$rowl[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)] 

# aux <- df_all$Totalass[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)] - df_all$Totalliab[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)] - df_all$eq[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)]
# df_all$IBduetoUK[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)] <- df_all$IBduetoUK[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)] + aux
# df_all$Totalliab[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)] <- df_all$Totalliab[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 26)] + aux


df_all$rowa[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)] <- df_all$currloans[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)]*a2$p_cul[which(a2$IDNO == 49)]
df_all$callosc[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)] <- df_all$callc[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)]*a2$p_call[which(a2$IDNO == 49)]
df_all$rowl[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)] <- df_all$demliab[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)]*a2$p_dem[which(a2$IDNO == 49)]
df_all$Totalass[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)] <- df_all$Totalass[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)] + 
  df_all$rowa[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)] + 
  df_all$callosc[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)]
df_all$Totalliab[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)] <- df_all$Totalliab[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)] + 
  df_all$rowl[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)] 

# aux <- df_all$Totalass[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)] - df_all$Totalliab[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)] - df_all$eq[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)]
# df_all$eq[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)] <- df_all$eq[which(df_all$Date < unique(a2$Date) & df_all$IDNO == 49)] + aux

df_all$rowa[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] <- df_all$currloans[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)]*a3$p_cul[which(a3$IDNO == 8)]
df_all$callosc[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] <- df_all$callc[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)]*a3$p_call[which(a3$IDNO == 8)]
df_all$rowl[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] <- df_all$demliab[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)]*a3$p_dem[which(a3$IDNO == 8)]
df_all$oa[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] <- df_all$Totalass[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)]*a3$p_ta[which(a3$IDNO == 8)]
df_all$ol[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] <- df_all$Totalliab[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)]*a3$p_tl[which(a3$IDNO == 8)]

df_all$Totalass[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] <- df_all$Totalass[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] +
  df_all$rowa[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] + df_all$oa[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] +
  df_all$callosc[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)]
df_all$Totalliab[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] <- df_all$Totalliab[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] +
  df_all$rowl[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] + df_all$ol[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)]

# aux <- df_all$Totalass[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] - df_all$Totalliab[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] - df_all$eq[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)]
# df_all$ol[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] <- df_all$ol[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] + aux
# df_all$Totalliab[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] <- df_all$Totalliab[which(df_all$Date < unique(a3$Date) & df_all$IDNO == 8)] + aux


# aux <- df_all$IBduefrEW[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)]*(1 - a4$p_frew[2])

# aux <- rowSums(data.frame(df_all$IBduefrEW[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)],df_all$IBduefrUK[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)]),na.rm = T)
aux <- rowSums(data.frame(df_all$iba_row[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)]),na.rm = T)
df_all$iba_row[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)] <- aux*a4$p_frewuk[2]
# df_all$IBduefrUK[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)] <- aux*a4$p_fruk[2]

# df_all$callosc[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)] <- aux*a4$p_call[2]
# df_all$rowa[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)] <- aux*a4$p_cul[2]  
# df_all$rowl[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)] <- aux*a4$p_dosc[2]

df_all$callosc[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)] <- aux*a4$p_call[2]
df_all$rowa[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)] <- aux*a4$p_cul[2]  
df_all$rowl[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)] <- aux*a4$p_dosc[2]

df_all$Totalass[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)] <- df_all$Totalass[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)] +
  df_all$rowl[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)]
df_all$Totalliab[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)] <- df_all$Totalliab[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)] +
  df_all$rowl[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)]

# df_all$secs[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)] <- df_all$secs[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)] + a4$p_sec[2]
# df_all$IBduefrUK[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)] <- df_all$IBduefrUK[which(df_all$Date < unique(a4$Date[2]) & df_all$IDNO == 1)] - a4$p_sec[2]

rm(a1,a2,a3,a4)

# # IB BEFORE 1871 ####
# df_aux <- df_all %>% filter(year %in% c(1874,1876,1878))
# for (i in unique(df_aux$IDNO)){
# 
#   a5 <- df_aux %>% filter(IDNO == i) %>%
#     summarise(Date = Date,IDNO = IDNO, p_frewuk = iba_row/(iba_row + iba),
#               p_toewuk = ibl_row/(ibl_row + ibl),
#               p_iba = iba/(iba_row + iba),p_ibl = ibl/(ibl_row + ibl))
# 
#   if (1880 %in% year(a5$Date)){y = "1880-03-31"}else{y = max(unique(a5$Date), na.rm = T)}
# 
#   aux <- rowSums(data.frame(df_all$iba[which(df_all$Date < y & df_all$IDNO == i)]),na.rm = T)
#   # df_all$IBduefrEW[which(df_all$Date < y & df_all$IDNO == 1)] <- aux*a5$p_frew[1]
#   # df_all$IBduefrUK[which(df_all$Date < y & df_all$IDNO == 1)] <- aux*a5$p_fruk[1]
#   df_all$iba_row[which(df_all$Date < y & df_all$IDNO == i)] <- aux*a5$p_frewuk[NROW(a5$p_frewuk)]
#   df_all$ibl_row[which(df_all$Date < y & df_all$IDNO == i)] <- aux*a5$p_toewuk[NROW(a5$p_frewuk)]
#   df_all$iba[which(df_all$Date < y & df_all$IDNO == i)] <- aux*a5$p_iba[NROW(a5$p_frewuk)]
#   df_all$ibl[which(df_all$Date < y & df_all$IDNO == i)] <- aux*a5$p_ibl[NROW(a5$p_frewuk)]
# 
# }




#
# Compute aggregate accounts ####

df_all <- df_all %>% mutate(a = rowSums(data.frame(reserves,callosc,secs,callc,overdue,currloans,iba_sec,iba,iba_row,oa,rowa),na.rm = T))
df_all <- df_all %>% mutate(a2 = rowSums(data.frame(reserves,callosc,secs,callc,overdue,currloans2,iba_sec,iba,iba_row,oa,rowa),na.rm = T))
# df_all <- df_all %>% mutate(a2 = rowSums(data.frame(reserves,callosc,secs,callc,overdue,currloans,iba_sec,iba,iba_row,oa,rowa,-rowl,-ibl_row),na.rm = T))
df_all <- df_all %>% mutate(Totalass2 = rowSums(data.frame(Totalass,-rowl),na.rm = T))
df_all <- df_all %>% mutate(Totalass4 = rowSums(data.frame(Totalass,-rowa,-callosc),na.rm = T))
df_all <- df_all %>% mutate(ROWA = rowSums(data.frame(rowa,callosc),na.rm = T))


df_all <- df_all %>% mutate(Totalliab2 = rowSums(data.frame(Totalliab,-rowl),na.rm = T))
df_all <- df_all %>% mutate(Totalass3 = rowSums(data.frame(Totalass,-rowl,-ibl_row),na.rm = T))
df_all <- df_all %>% mutate(Totalliab3 = rowSums(data.frame(Totalliab,-rowl,-ibl_row),na.rm = T))
df_all <- df_all %>% mutate(l = rowSums(data.frame(demliab,timeliab,ibl_sec,ibl,ibl_row,ol,rowl),na.rm = T))
df_all <- df_all %>% mutate(l2 = rowSums(data.frame(demliab2,timeliab2,ibl_sec,ibl,ibl_row,ol,rowl),na.rm = T))
# df_all <- df_all %>% mutate(l2 = rowSums(data.frame(demliab,timeliab,ibl_sec,ibl,ibl_row,ol,rowl,-rowl,-ibl_row),na.rm = T))
df_all <- df_all %>% mutate(eq2 = rowSums(data.frame(a,-l),na.rm = T))
df_all <- df_all %>% mutate(eq3 = rowSums(data.frame(Totalass,-Totalliab),na.rm = T))



# WHAT IS THIS FOR??
# p_ib <- df_all %>% filter(year == 1876) %>%
#   select(IDNO,IBdepCass,IBdepCliab,IBduefrUK,IBduefrEW,IBduetoUK,IBduetoEW) %>%
#   mutate(p_iba = IBdepCass/(IBdepCass + IBduefrUK + IBduefrEW), p_ibl = IBdepCliab/(IBdepCliab + IBduetoUK + IBduetoEW))



# CLEAN 2 ####

q_b_d <- aggregate(q_b[,-which(colnames(q_b) %in% c("IDNO","year","UID_CD_21"))], 
                   by =list("UID_CD_21" = q_b$UID_CD_21, "IDNO" = q_b$IDNO, "year" = q_b$year), 
                   FUN = function(x) extract(x,1))

# THERE'S AN ERROR HERE. IF ONE BANK HAS BRANCHES IN TWO DISTRICTS WITH THE SAME NAME ONLY ONE IS CHOSEN.
q_b_dcov <- aggregate(q_b[,-which(colnames(q_b) %in% c("UID_CD_21","year"))], 
                      by =list("UID_CD_21" = q_b$UID_CD_21, "year" = q_b$year), 
                      FUN = function(x) extract(x,1))


nd_year_prov <-  q_b_dcov %>% group_by(year,province) %>% tally(name = "nd_cov")
nd_year <-  q_b_dcov %>% group_by(year) %>% tally(name = "nd_cov")

pop_year <-  q_b_dcov %>% group_by(year) %>% summarise("pop_year" = sum(population, na.rm = T))
pop_year_prov <-  q_b_dcov %>% group_by(year,province) %>% summarise("pop_year" = sum(population, na.rm = T))

q_b_d_prov <- q_b_d %>% group_by(IDNO,year,province) %>% tally(name = "nd")
q_b_d <- q_b_d %>% group_by(IDNO,year) %>% tally(name = "nd")

q_b_d$nd_noverl <- NA
q_b_d$nb_noverl <- NA

for (t in unique(q_b_d$year)){
  for (i in unique(q_b_d$IDNO[which(q_b_d$year == t)])){
    q_b_d$nd_noverl[which(q_b_d$year == t & q_b_d$IDNO == i)] <- 
      sum(!(unique(q_b$UID_CD_21[which(q_b$year == t & q_b$IDNO == i)]) %in% unique(q_b$UID_CD_21[which(q_b$year == t & !(q_b$IDNO == i))])))
    q_b_d$nb_noverl[which(q_b_d$year == t & q_b_d$IDNO == i)] <- 
      sum(!(q_b$UID_CD_21[which(q_b$year == t & q_b$IDNO == i)] %in% unique(q_b$UID_CD_21[which(q_b$year == t & !(q_b$IDNO == i))])))
  }
}

q_b_d_prov$nd_noverl <- NA
q_b_d_prov$nb_noverl <- NA

for (t in unique(q_b_d_prov$year)){
  for (i in unique(q_b_d_prov$IDNO[which(q_b_d_prov$year == t)])){
    for (j in unique(q_b_d_prov$province[which(q_b_d_prov$year == t & q_b_d_prov$IDNO == i)])){
      q_b_d_prov$nd_noverl[which(q_b_d_prov$year == t & q_b_d_prov$IDNO == i & q_b_d_prov$province == j)] <- 
        sum(!(unique(q_b$UID_CD_21[which(q_b$year == t & q_b$IDNO == i & q_b$province == j)]) %in% unique(q_b$UID_CD_21[which(q_b$year == t & !(q_b$IDNO == i) & q_b$province == j)])))
      q_b_d_prov$nb_noverl[which(q_b_d_prov$year == t & q_b_d_prov$IDNO == i & q_b_d_prov$province == j)] <- 
        sum(!(q_b$UID_CD_21[which(q_b$year == t & q_b$IDNO == i & q_b$province == j)] %in% unique(q_b$UID_CD_21[which(q_b$year == t & !(q_b$IDNO == i) & q_b$province == j)])))
    }
  }
}

q_b_d <- left_join(q_b_d,nd_year, by = c("year"))   
q_b_d_prov <- left_join(q_b_d_prov,nd_year_prov, by = c("year","province"))   

# 

CC_g <- CC_g[-c(5,8,9,11)]
CC_p2 <- CC_p %>% filter(subdistrict %in% CC_g)
q_b_cc <- NULL # comment to drop cc

q_b_cc <- q_b %>%
  mutate(ncc = UID_CD_21 %in% unique(CC_p2$UID_CD_21))

q_b_cc <- q_b_cc %>%
  group_by(IDNO,year,province,district) %>%
  summarise(ncc_nb  = sum(ncc), ncc_pop = extract(population,1))

q_b_cc <- q_b_cc %>%
  mutate(ncc_nd = ifelse(ncc_nb > 0, ncc_nb/ncc_nb, 0))
q_b_cc <- q_b_cc %>%
  mutate(ncc_pop = ifelse(ncc_nb > 0, ncc_pop, 0))
q_b_cc <- q_b_cc %>%
  mutate(ncc_nb_pop = ifelse(ncc_nb > 0, ncc_nb*ncc_pop, 0))

ncc_year <- aggregate(q_b_cc[,-which(colnames(q_b_cc) %in% c("district","year","province"))], 
                      by =list("district" = q_b_cc$district, "year" = q_b_cc$year, "province" = q_b_cc$province), 
                      FUN = function(x) extract(x,1))


ncc_year <- ncc_year %>% filter(ncc_nd == 1) %>% group_by(year) %>% 
  summarise(ncc_pop_year = sum(ncc_pop), ncc_year = sum(ncc_nd))



q_b_cc <- q_b_cc %>% select(-district) %>% group_by(year,province,IDNO) %>% summarise_all(sum)


q_b_n <- q_b %>% group_by(IDNO,year,province) %>% tally(name = "nb")

q_b_pd <- q_b %>% group_by(district,province,year) %>% tally(name = "nb_dist")
q_b <- left_join(q_b,q_b_pd, by = c("district","province","year"))  
q_b_nbj <- q_b %>% group_by(IDNO,year,province,district) %>% tally(name = "nb_j")
q_b <- left_join(q_b,q_b_nbj, by = c("IDNO","district","province","year"))  


# # reg_aux <- ref_11_cd %>% lm(formula = log((value- cost)) ~ log(capital) + log(employees) + factor(province) + factor(province)*log(capital) + factor(province)*log(employees))
# reg <- q_b %>% lm(formula = log((s_m - cost_m)) ~ log(k_m) + log(L_m))
# alpha <- coefficients(reg)[2]
# A_j <- residuals.lm(reg)
# A_j <- exp(A_j)
# aux <- (alpha*A_j/1.05)^(1/(1-alpha))
# 
# reg2 <- q_b %>% lm(formula = log((s1_c + s2_c)) ~ log(k1_c + k2_c) + log(weeksL_c/50))
# 
# q_b <- cbind(q_b,aux)
# 
# # q_b <- left_join(q_b,q_b_nbj, by = c("IDNO","district","province","year"))  



# q_b_p <- q_b %>% group_by(IDNO,year,province) %>% distinct(district, .keep_all = T) %>% summarise("nb_pms" = ceiling(sum(population*(nb_j/nb_dist), na.rm = T)),
#                                                                                                   "nb_new" = ceiling(sum(aux*population*(nb_j/nb_dist), na.rm = T)),
#                                                                                                   "nb_new2" = ceiling(sum(aux*population, na.rm = T)),
#                                                                                                   "nb_new3" = ceiling(sum(wages_c/(weeksL_c/50) + wages_m/L_m, na.rm = T)),
# 
#                                                                                                   "nb_sms" = ceiling(sum((s1_c + s2_c + s_m)*(nb_j/nb_dist), na.rm = T)),
#                                                                                                   "nb_spcms" = ceiling(sum(((s1_c + s2_c + s_m)/population)*(nb_j/nb_dist), na.rm = T)),
#                                                                                                   "nb_kms" = ceiling(sum((k1_c + k2_c + k_m)*(nb_j/nb_dist), na.rm = T)),
#                                                                                                   "nb_kpcms" = ceiling(sum(((k1_c + k2_c + k_m)/population)*(nb_j/nb_dist), na.rm = T)),
#                                                                                                   "nb_pms_01" = ceiling(sum(pop_01*(nb_j/nb_dist), na.rm = T)),
#                                                                                                   "pop" = sum(population, na.rm = T),
#                                                                                                   "pop_01" = sum(pop_01, na.rm = T),
#                                                                                                   "nb_dist" = sum(nb_dist, na.rm = T),
#                                                                                                   "k" = sum(k1_c + k2_c + k_m, na.rm = T),
#                                                                                                   "sales" = sum(s1_c + s2_c + s_m, na.rm = T))

q_b_p <- q_b %>% group_by(IDNO,year,province) %>% distinct(district, .keep_all = T) %>% summarise("pop" = sum(population, na.rm = T),
                                                                                                  "nb_dist" = sum(nb_dist, na.rm = T),
                                                                                                  "nb_pms" = ceiling(sum(population*(nb_j/nb_dist), na.rm = T)))


# q_b_p2 <- q_b %>% group_by(IDNO,year,province) %>% summarise("nb_pms_rep" = ceiling(sum(population*(nb_j/nb_dist), na.rm = T)),
#                                                              "nb_new_rep" = ceiling(sum(aux*population*(nb_j/nb_dist), na.rm = T)),
#                                                              "nb_new2_rep" = ceiling(sum(aux*population, na.rm = T)),
#                                                              "nb_new3_rep" = ceiling(sum(wages_c/(weeksL_c/50) + wages_m/L_m, na.rm = T)),
#                                                              
#                                                              "nb_sms_rep" = ceiling(sum((s1_c + s2_c + s_m)*(nb_j/nb_dist), na.rm = T)),
#                                                              "nb_spcms_rep" = ceiling(sum(((s1_c + s2_c + s_m)/population)*(nb_j/nb_dist), na.rm = T)),
#                                                              "nb_kms_rep" = ceiling(sum((k1_c + k2_c + k_m)*(nb_j/nb_dist), na.rm = T)),
#                                                              "nb_kpcms_rep" = ceiling(sum(((k1_c + k2_c + k_m)/population)*(nb_j/nb_dist), na.rm = T)),
#                                                              "nb_pms_01_rep" = ceiling(sum(pop_01*(nb_j/nb_dist), na.rm = T)),
#                                                              "pop_rep" = sum(population, na.rm = T),
#                                                              "pop_01_rep" = sum(pop_01, na.rm = T), 
#                                                              "nb_dist_rep" = sum(nb_dist, na.rm = T),
#                                                              "k_rep" = sum(k1_c + k2_c + k_m, na.rm = T),
#                                                              "sales_rep" = sum(s1_c + s2_c + s_m, na.rm = T)) 

# q_b_agg <- left_join(q_b_cc,q_b_n, by = c("year","IDNO","province")) %>%
#   left_join(.,q_b_p, by = c("year","IDNO","province")) %>%
#   left_join(.,q_b_p2, by = c("year","IDNO","province"))
q_b_agg <- left_join(q_b_cc,q_b_n, by = c("year","IDNO","province")) %>%
  left_join(.,q_b_p, by = c("year","IDNO","province"))




q_b_agg_idno <- aggregate(q_b_agg[,-which(colnames(q_b_agg) %in% c("IDNO","year","province"))], 
                          by =list("IDNO" = q_b_agg$IDNO, "year" = q_b_agg$year), 
                          FUN = function(x) sum(x,na.rm = T))

q_b_agg_idno <- left_join(q_b_agg_idno,q_b_d, by = c("year","IDNO"))
q_b_agg_idno <- left_join(q_b_agg_idno,pop_year, by = c("year"))
q_b_agg_idno <- left_join(q_b_agg_idno,ncc_year, by = c("year"))


q_b_agg_idno <- q_b_agg_idno %>% mutate(nd = nd - ncc_nd)
q_b_agg_idno <- q_b_agg_idno %>% mutate(nb = nb - ncc_nb)
q_b_agg_idno <- q_b_agg_idno %>% mutate(ndC = nb - nd)
q_b_agg_idno <- q_b_agg_idno %>% mutate(nccC = ncc_nb - ncc_nd)


q_b_agg_idno <- q_b_agg_idno %>% group_by(year) %>%
  mutate(n_share = nb/sum(nb,na.rm = T), na.rm = T) %>%
  mutate(n_rel = nb/max(nb,na.rm = T), na.rm = T) %>%
  mutate(cc_rel = ncc_nd/max(ncc_nd,na.rm = T), na.rm = T) %>% # comment to drop cc
  mutate(d_rel = nd/max(nd,na.rm = T), na.rm = T) 

# q_b_agg_idno <- q_b_agg_idno %>% mutate(ndC = 1 + nb - nd)
q_b_agg_idno <- q_b_agg_idno %>% mutate(ndc_nd = ndC/nd, na.rm = T)
q_b_agg_idno <- q_b_agg_idno %>% mutate(nd_nb = nd/nb, na.rm = T)
q_b_agg_idno <- q_b_agg_idno %>% mutate(d_cover = nd/nd_cov, na.rm = T)
q_b_agg_idno <- q_b_agg_idno %>% mutate(nd_overl = nd - nd_noverl, na.rm = T)
q_b_agg_idno <- q_b_agg_idno %>% mutate(nb_overl = nb - nb_noverl, na.rm = T)
q_b_agg_idno <- q_b_agg_idno %>% mutate(nd_noverlj = nd_cov - nd, na.rm = T)
# q_b_agg_idno <- q_b_agg_idno %>% mutate(nb_p = pop, na.rm = T)
q_b_agg_idno <- q_b_agg_idno %>% mutate(nb_p = pop, na.rm = T)
# q_b_agg_idno <- q_b_agg_idno %>% mutate(nb_p_rep = pop_rep, na.rm = T)

q_b_agg_idno <- q_b_agg_idno %>% mutate(ncc_p = ncc_pop/ncc_pop_year, na.rm = T)
q_b_agg_idno <- q_b_agg_idno %>% mutate(ncc_cover = ncc_nd/ncc_year, na.rm = T)



# PROV



q_b_agg_prov <- left_join(q_b_agg,ncc_year, by = c("year"))
q_b_agg_prov <- left_join(q_b_agg,q_b_d_prov, by = c("year","IDNO","province"))
q_b_agg_prov <- left_join(q_b_agg_prov,pop_year_prov, by = c("year","province"))

q_b_agg_prov <- q_b_agg_prov %>% mutate(nd = nd - ncc_nd)
q_b_agg_prov <- q_b_agg_prov %>% mutate(nb = nb - ncc_nb)
q_b_agg_prov <- q_b_agg_prov %>% mutate(ndC = nb - nd)
q_b_agg_prov <- q_b_agg_prov %>% mutate(nccC = ncc_nb - ncc_nd)


q_b_agg_prov <- q_b_agg_prov %>% group_by(year,IDNO) %>%
    mutate(nbp_share = nb/sum(nb,na.rm = T), na.rm = T) 
q_b_agg_prov <- q_b_agg_prov %>% mutate_cond(is.nan(nbp_share), nbp_share = 0)  

q_b_agg_prov <- q_b_agg_prov %>% filter(!is.na(IDNO))  

  #   mutate(n_share = nb/sum(nb,na.rm = T), na.rm = T) %>%
  # mutate(n_rel = nb/max(nb,na.rm = T), na.rm = T) %>%
  # mutate(cc_rel = ncc_nd/max(ncc_nd,na.rm = T), na.rm = T) %>% # comment to drop cc
  # mutate(d_rel = nd/max(nd,na.rm = T), na.rm = T)

 
# q_b_agg_prov <- q_b_agg_prov %>% mutate(ndC = 1 + nb - nd)
# q_b_agg_prov <- q_b_agg_prov %>% mutate(ndc_nd = ndC/nd, na.rm = T)
# q_b_agg_prov <- q_b_agg_prov %>% mutate(nd_nb = nd/nb, na.rm = T)
# q_b_agg_prov <- q_b_agg_prov %>% mutate(d_cover = nd/nd_cov, na.rm = T)
# q_b_agg_prov <- q_b_agg_prov %>% mutate(nd_overl = nd - nd_noverl, na.rm = T)
# q_b_agg_prov <- q_b_agg_prov %>% mutate(nb_overl = nb - nb_noverl, na.rm = T)
# q_b_agg_prov <- q_b_agg_prov %>% mutate(nd_noverlj = nd_cov - nd, na.rm = T)
# q_b_agg_prov <- q_b_agg_prov %>% mutate(nb_p = pop, na.rm = T)
# 
# q_b_agg_prov2 <- aggregate(q_b_agg_prov[,-which(colnames(q_b_agg_prov) %in% c("IDNO","year","province"))], 
#                            by =list("IDNO" = q_b_agg_prov$IDNO, "year" = q_b_agg_prov$year), 
#                            FUN = function(x) mean(x,na.rm = T))
# 
# colnames(q_b_agg_prov2) <- unlist(c("IDNO","year",lapply(colnames(q_b_agg_prov2[,-which(colnames(q_b_agg_prov2) %in% c("year","IDNO"))]),function(x) paste0(x,".p")))) 


# df_all <- left_join(df_all,q_b_agg_idno, by = c("year","IDNO")) %>%
#   left_join(.,q_b_agg_prov2, by = c("year","IDNO"))
df_all <- left_join(df_all,q_b_agg_idno, by = c("year","IDNO")) 

df_all <- df_all %>% mutate(ov_share = overdue/(overdue + currloans), na.rm = T)
df_all <- df_all %>% mutate(loans = overdue + currloans, na.rm = T)
df_all <- df_all %>% group_by(Date) %>% 
  mutate(IBl_share = ibl/sum(ibl,na.rm = T), na.rm = T) 
df_all <- df_all %>% group_by(Date) %>% 
  mutate(IBa_share = iba/sum(iba,na.rm = T), na.rm = T) 
df_all <- df_all %>% group_by(Date) %>%
  mutate(IB_net_share = (iba - ibl)/sum(iba + ibl,na.rm = T), na.rm = T)
df_all <- df_all %>% group_by(Date) %>% 
  mutate(dep_share = (demliab + timeliab)/sum(demliab + timeliab,na.rm = T), na.rm = T) 
df_all <- df_all %>% group_by(Date) %>% 
  mutate(a_share = a/sum(a,na.rm = T), na.rm = T) 


rm(ncc_year,nd_year,pop_year,q_b_cc,q_b_d,q_b_dcov,q_b_n,q_b_p,q_b_pd,q_b_nbj,q_b_d_prov)
rm(i,j,t,aux)
rm(nd_year_prov,pop_year_prov,q_b_agg)
# rm(id)

# DEFINE DFS #####


# q_bs <- df_all %>% filter(year %in% c(1882:1925), !(is.na(IDNO)),currloans > 0, Totalass > 0)
q_bs <- df_all %>% filter(year %in% c(1857:1925), !(is.na(IDNO)),currloans > 0, Totalass > 0)

q_bs_inc3 <- q_bs %>% filter(!(turnover %in% c(0)))
q_bs_inc3_year <- q_bs_inc3 %>% filter(month %in% c(3,4))
q_bs_inc3_year <- q_bs_inc3_year[!(duplicated(q_bs_inc3_year[c("IDNO","year")])),]

q_bs_inc <- q_bs %>% filter(nb > 0, !(turnover %in% c(0)))
q_bs_inc2 <- q_bs %>% filter((turnover %in% c(1,1.1)), nb > 0)
q_bs_all <- q_bs %>% filter(nb > 0, !(turnover %in% c(0)))
q_bs_inc_year <- q_bs_inc %>% filter(month %in% c(3,4))
q_bs_inc_year <- q_bs_inc_year[!(duplicated(q_bs_inc_year[c("IDNO","year")])),]
q_bs_all_year <- q_bs_all %>% filter(month %in% c(3,4))
q_bs_all_year <- q_bs_all_year[!(duplicated(q_bs_all_year[c("IDNO","year")])),]
q_bs_inc2_year <- q_bs_inc2 %>% filter(month %in% c(3,4))
q_bs_inc2_year <- q_bs_inc2_year[!(duplicated(q_bs_inc2_year[c("IDNO","year")])),]












# OV SHARE #####

q <- q_bs_inc_year %>% filter(ov_share < 0.15, year <= 1913) # filter ovshare only for picture

q_bs_all  %>%
  ggplot(aes(y=ov_share,x=ncc_nd)) +
  geom_point(size = 3, alpha = 0.5) +
  # stat_smooth(colour = "red", method = "lm", formula = y ~ x + I(x^2),size= 2, se = F) +
  # stat_smooth(method = "lm", formula = y ~ I(x^-1) , size = 2, se = F) +
  geom_label(aes(label = IDNO) ) +
  labs(title = "Bank expansion and overdue loans",
       x = "Number of districts with at least one branch", y = "Share of overdue loans")  +
  theme(
    legend.title = element_blank(),
    legend.text=element_text(size=12),
    title = element_markdown(),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1)
  ) 


# 
q <- q_bs_inc_year %>% filter(year <= 1913)
q2 <- q_bs_inc_year %>% filter(year <= 1913, ov_share < 0.25)


# Call:
#   lm(formula = ov_share ~ nd + I(nd^2), data = q)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.03374 -0.01167 -0.00304  0.00447  0.88767 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.537e-02  4.575e-03   7.732 6.73e-14 ***
#   nd          -1.653e-03  3.541e-04  -4.666 4.02e-06 ***
#   I(nd^2)      1.797e-05  5.177e-06   3.471 0.000568 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04908 on 460 degrees of freedom
# Multiple R-squared:  0.05656,	Adjusted R-squared:  0.05245 
# F-statistic: 13.79 on 2 and 460 DF,  p-value: 1.53e-06



# Call:
#   lm(formula = ov_share ~ nd + I(nd^2) + factor(nd > 7), data = q)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.03789 -0.00668 -0.00182  0.00234  0.88352 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         3.841e-02  4.672e-03   8.222 2.08e-15 ***
#   nd                 -5.232e-04  5.376e-04  -0.973  0.33101    
# I(nd^2)             5.619e-06  6.797e-06   0.827  0.40886    
# factor(nd > 7)TRUE -2.330e-02  8.393e-03  -2.776  0.00572 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04873 on 459 degrees of freedom
# Multiple R-squared:  0.07214,	Adjusted R-squared:  0.06607 
# F-statistic:  11.9 on 3 and 459 DF,  p-value: 1.628e-07



# Call:
#   lm(formula = ov_share ~ nd + I(nd^2) + I(nd^3) + I(nd^4), data = q)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.04477 -0.00691 -0.00195  0.00278  0.87664 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.992e-02  6.474e-03   7.711 7.86e-14 ***
#   nd          -5.349e-03  1.356e-03  -3.945 9.23e-05 ***
#   I(nd^2)      2.070e-04  7.914e-05   2.616   0.0092 ** 
#   I(nd^3)     -3.134e-06  1.620e-06  -1.934   0.0537 .  
# I(nd^4)      1.610e-08  1.048e-08   1.536   0.1252    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04862 on 458 degrees of freedom
# Multiple R-squared:  0.0785,	Adjusted R-squared:  0.07045 
# F-statistic: 9.754 on 4 and 458 DF,  p-value: 1.404e-07

# Q2

# Call:
#   lm(formula = ov_share ~ nd + I(nd^2) + I(nd^3) + I(nd^4), data = q2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.030973 -0.005205 -0.001496  0.003391  0.148971 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.409e-02  2.060e-03  16.552  < 2e-16 ***
#   nd          -3.239e-03  4.297e-04  -7.537 2.61e-13 ***
#   I(nd^2)      1.193e-04  2.505e-05   4.761 2.59e-06 ***
#   I(nd^3)     -1.726e-06  5.126e-07  -3.368 0.000822 ***
#   I(nd^4)      8.499e-09  3.314e-09   2.564 0.010652 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01536 on 456 degrees of freedom
# Multiple R-squared:  0.2616,	Adjusted R-squared:  0.2551 
# F-statistic: 40.39 on 4 and 456 DF,  p-value: < 2.2e-16



# Call:
#   lm(formula = ov_share ~ nd + I(nd^-1), data = q)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.04978 -0.00777 -0.00311  0.00213  0.87163 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.0099825  0.0048403   2.062   0.0397 *  
#   nd          -0.0001398  0.0001520  -0.920   0.3581    
# I(nd^-1)     0.0399345  0.0089477   4.463 1.02e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04868 on 460 degrees of freedom
# Multiple R-squared:  0.07203,	Adjusted R-squared:  0.068 
# F-statistic: 17.85 on 2 and 460 DF,  p-value: 3.409e-08

# Q2

# Call:
#   lm(formula = ov_share ~ nd + I(nd^-1), data = q2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.032956 -0.006111 -0.002529  0.002914  0.146988 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.007e-02  1.570e-03   6.413 3.57e-10 ***
#   nd          -1.283e-04  4.924e-05  -2.606  0.00946 ** 
#   I(nd^-1)     2.302e-02  2.915e-03   7.896 2.15e-14 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01575 on 458 degrees of freedom
# Multiple R-squared:   0.22,	Adjusted R-squared:  0.2166 
# F-statistic:  64.6 on 2 and 458 DF,  p-value: < 2.2e-16


# Call:
#   lm(formula = ov_share ~ nd + I(nd^-1) + factor(nd > 7), data = q)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.04767 -0.00584 -0.00241  0.00225  0.87374 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)         2.276e-02  8.069e-03   2.820   0.0050 **
#   nd                 -3.701e-05  1.602e-04  -0.231   0.8174   
# I(nd^-1)            2.495e-02  1.171e-02   2.130   0.0337 * 
#   factor(nd > 7)TRUE -1.639e-02  8.297e-03  -1.975   0.0488 * 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04853 on 459 degrees of freedom
# Multiple R-squared:  0.07985,	Adjusted R-squared:  0.07384 
# F-statistic: 13.28 on 3 and 459 DF,  p-value: 2.514e-08




a <- lm(ov_share ~ nd + I(nd^-1) + factor(IDNO) - 1, data = q_bs_all_year[-which(q_bs_all_year$IDNO == 50),])






# Call:
#   lm(formula = ov_share ~ nd + I(nd^-1) + factor(IDNO) - 1, data = q)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.21339 -0.00469 -0.00097  0.00224  0.79884 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# nd             -0.0002244  0.0002353  -0.954  0.34078    
# I(nd^-1)        0.0118927  0.0166309   0.715  0.47495    
# factor(IDNO)1   0.0181929  0.0174339   1.044  0.29732    
# factor(IDNO)2   0.0129811  0.0128324   1.012  0.31233    
# factor(IDNO)3   0.0049922  0.0146647   0.340  0.73371    
# factor(IDNO)4   0.0116054  0.0178741   0.649  0.51652    
# factor(IDNO)5   0.0110753  0.0145773   0.760  0.44784    
# factor(IDNO)6   0.0265977  0.0278175   0.956  0.33956    
# factor(IDNO)7   0.0187524  0.0210690   0.890  0.37396    
# factor(IDNO)8   0.0143947  0.0138952   1.036  0.30084    
# factor(IDNO)9   0.0141084  0.0147341   0.958  0.33886    
# factor(IDNO)10  0.0070373  0.0137478   0.512  0.60901    
# factor(IDNO)11  0.0089492  0.0131176   0.682  0.49548    
# factor(IDNO)13  0.0082918  0.0152104   0.545  0.58596    
# factor(IDNO)14  0.0138447  0.0130703   1.059  0.29011    
# factor(IDNO)15  0.0213197  0.0132625   1.608  0.10871    
# factor(IDNO)16  0.1109068  0.0227441   4.876 1.55e-06 ***
#   factor(IDNO)18  0.0689147  0.0242140   2.846  0.00465 ** 
#   factor(IDNO)19  0.0097186  0.0269858   0.360  0.71893    
# factor(IDNO)20  0.0103381  0.0159903   0.647  0.51830    
# factor(IDNO)21  0.0055351  0.0227441   0.243  0.80784    
# factor(IDNO)23  0.0509518  0.0207745   2.453  0.01460 *  
#   factor(IDNO)26  0.0198685  0.0185482   1.071  0.28472    
# factor(IDNO)27  0.0498715  0.0341669   1.460  0.14516    
# factor(IDNO)29  0.0251501  0.0179308   1.403  0.16149    
# factor(IDNO)30  0.0080704  0.0135591   0.595  0.55204    
# factor(IDNO)31  0.0060938  0.0174394   0.349  0.72695    
# factor(IDNO)32  0.0046828  0.0171194   0.274  0.78458    
# factor(IDNO)33  0.0110771  0.0143786   0.770  0.44152    
# factor(IDNO)34  0.0069085  0.0464404   0.149  0.88182    
# factor(IDNO)36  0.0434529  0.0163295   2.661  0.00810 ** 
#   factor(IDNO)37  0.0219706  0.0146266   1.502  0.13384    
# factor(IDNO)38  0.0092541  0.0140316   0.660  0.50994    
# factor(IDNO)39  0.0139210  0.0134679   1.034  0.30191    
# factor(IDNO)40  0.0131308  0.0137427   0.955  0.33990    
# factor(IDNO)41  0.0049936  0.0464451   0.108  0.91443    
# factor(IDNO)42  0.0075697  0.0132393   0.572  0.56780    
# factor(IDNO)43  0.0084484  0.0143760   0.588  0.55708    
# factor(IDNO)45  0.2505148  0.0343112   7.301 1.50e-12 ***
#   factor(IDNO)46  0.0085223  0.0146860   0.580  0.56203    
# factor(IDNO)47  0.0131560  0.0148808   0.884  0.37717    
# factor(IDNO)48  0.0124750  0.0160254   0.778  0.43675    
# factor(IDNO)49  0.0106492  0.0147245   0.723  0.46995    
# factor(IDNO)50  0.0089576  0.0214955   0.417  0.67710    
# factor(IDNO)52  0.0078784  0.0235028   0.335  0.73764    
# factor(IDNO)53  0.0105854  0.0234193   0.452  0.65151    
# factor(IDNO)54  0.0118934  0.0240135   0.495  0.62067    
# factor(IDNO)55  0.0089741  0.0235895   0.380  0.70382    
# factor(IDNO)56  0.0084709  0.0332653   0.255  0.79912    
# factor(IDNO)57  0.0230923  0.0272978   0.846  0.39808    
# factor(IDNO)58 -0.0063714  0.0315332  -0.202  0.83997    
# factor(IDNO)59  0.0178989  0.0347641   0.515  0.60692    
# factor(IDNO)60 -0.0087972  0.0492492  -0.179  0.85832    
# factor(IDNO)73 -0.0116683  0.0492492  -0.237  0.81283    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04633 on 409 degrees of freedom
# Multiple R-squared:  0.3112,	Adjusted R-squared:  0.2203 
# F-statistic: 3.422 on 54 and 409 DF,  p-value: 1.083e-12


# Q2

# Call:
#   lm(formula = ov_share ~ nd + I(nd^-1) + factor(IDNO) - 1, data = q2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.037876 -0.004261 -0.000446  0.002732  0.106264 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# nd             -1.592e-04  6.436e-05  -2.474 0.013777 *  
#   I(nd^-1)        3.436e-02  4.638e-03   7.409 7.43e-13 ***
#   factor(IDNO)1   1.433e-02  4.767e-03   3.007 0.002806 ** 
#   factor(IDNO)2   1.008e-02  3.509e-03   2.873 0.004284 ** 
#   factor(IDNO)3  -5.354e-03  4.030e-03  -1.329 0.184727    
# factor(IDNO)4   8.194e-03  4.887e-03   1.677 0.094364 .  
# factor(IDNO)5   8.146e-03  3.986e-03   2.044 0.041604 *  
#   factor(IDNO)6   1.649e-02  7.613e-03   2.165 0.030932 *  
#   factor(IDNO)7  -3.780e-03  5.832e-03  -0.648 0.517256    
# factor(IDNO)8   1.173e-02  3.799e-03   3.088 0.002150 ** 
#   factor(IDNO)9   1.119e-02  4.028e-03   2.778 0.005719 ** 
#   factor(IDNO)10  4.202e-03  3.759e-03   1.118 0.264263    
# factor(IDNO)11  4.960e-03  3.589e-03   1.382 0.167650    
# factor(IDNO)13  5.798e-03  4.158e-03   1.394 0.163931    
# factor(IDNO)14  1.083e-02  3.574e-03   3.031 0.002591 ** 
#   factor(IDNO)15  1.865e-02  3.626e-03   5.143 4.22e-07 ***
#   factor(IDNO)16 -1.148e-02  6.459e-03  -1.777 0.076254 .  
# factor(IDNO)18  5.968e-02  6.628e-03   9.005  < 2e-16 ***
#   factor(IDNO)19  5.355e-03  7.377e-03   0.726 0.468311    
# factor(IDNO)20  6.675e-03  4.372e-03   1.527 0.127613    
# factor(IDNO)21 -1.700e-02  6.284e-03  -2.705 0.007122 ** 
#   factor(IDNO)23  3.948e-02  5.697e-03   6.930 1.65e-11 ***
#   factor(IDNO)26  1.562e-02  5.072e-03   3.079 0.002215 ** 
#   factor(IDNO)27  3.694e-02  9.352e-03   3.950 9.21e-05 ***
#   factor(IDNO)29  2.114e-02  4.903e-03   4.311 2.04e-05 ***
#   factor(IDNO)30  5.314e-03  3.707e-03   1.433 0.152510    
# factor(IDNO)31 -1.827e-03  4.777e-03  -0.382 0.702333    
# factor(IDNO)32 -4.871e-04  4.683e-03  -0.104 0.917220    
# factor(IDNO)33  7.963e-03  3.931e-03   2.025 0.043480 *  
#   factor(IDNO)34  3.578e-03  1.269e-02   0.282 0.778128    
# factor(IDNO)36  3.209e-02  4.487e-03   7.152 4.01e-12 ***
#   factor(IDNO)37  1.419e-02  4.010e-03   3.538 0.000449 ***
#   factor(IDNO)38  6.472e-03  3.836e-03   1.687 0.092367 .  
# factor(IDNO)39  1.105e-02  3.682e-03   3.001 0.002855 ** 
#   factor(IDNO)40  9.704e-03  3.758e-03   2.582 0.010173 *  
#   factor(IDNO)41  2.095e-03  1.269e-02   0.165 0.868995    
# factor(IDNO)42  4.999e-03  3.620e-03   1.381 0.168026    
# factor(IDNO)43  4.952e-03  3.931e-03   1.260 0.208554    
# factor(IDNO)45  1.459e-02  1.349e-02   1.082 0.280054    
# factor(IDNO)46  5.700e-03  4.015e-03   1.420 0.156503    
# factor(IDNO)47  9.864e-03  4.069e-03   2.424 0.015772 *  
#   factor(IDNO)48  9.745e-03  4.381e-03   2.224 0.026666 *  
#   factor(IDNO)49  7.498e-03  4.026e-03   1.862 0.063279 .  
# factor(IDNO)50  6.395e-03  5.875e-03   1.089 0.277002    
# factor(IDNO)52  5.311e-03  6.424e-03   0.827 0.408815    
# factor(IDNO)53  7.807e-03  6.401e-03   1.220 0.223288    
# factor(IDNO)54  9.333e-03  6.563e-03   1.422 0.155777    
# factor(IDNO)55  3.350e-03  6.451e-03   0.519 0.603794    
# factor(IDNO)56  5.979e-03  9.091e-03   0.658 0.511156    
# factor(IDNO)57  2.061e-02  7.461e-03   2.762 0.006007 ** 
#   factor(IDNO)58 -2.890e-02  8.667e-03  -3.335 0.000932 ***
#   factor(IDNO)59  1.425e-02  9.501e-03   1.499 0.134587    
# factor(IDNO)60 -3.133e-02  1.349e-02  -2.322 0.020709 *  
#   factor(IDNO)73 -3.420e-02  1.349e-02  -2.535 0.011614 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01266 on 407 degrees of freedom
# Multiple R-squared:  0.6883,	Adjusted R-squared:  0.647 
# F-statistic: 16.65 on 54 and 407 DF,  p-value: < 2.2e-16


# Call:
#   lm(formula = ov_share ~ nd + I(nd^2) + factor(IDNO) - 1, data = q)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.20943 -0.00510 -0.00081  0.00258  0.79884 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# nd             -6.563e-04  5.567e-04  -1.179 0.239111    
# I(nd^2)         5.281e-06  6.956e-06   0.759 0.448158    
# factor(IDNO)1   2.497e-02  1.790e-02   1.395 0.163701    
# factor(IDNO)2   1.816e-02  1.344e-02   1.351 0.177594    
# factor(IDNO)3   1.248e-02  1.271e-02   0.982 0.326842    
# factor(IDNO)4   1.693e-02  1.823e-02   0.929 0.353591    
# factor(IDNO)5   1.927e-02  1.655e-02   1.164 0.244978    
# factor(IDNO)6   3.365e-02  2.686e-02   1.253 0.210908    
# factor(IDNO)7   3.107e-02  1.286e-02   2.416 0.016132 *  
#   factor(IDNO)8   2.219e-02  1.587e-02   1.398 0.162819    
# factor(IDNO)9   2.257e-02  1.686e-02   1.339 0.181355    
# factor(IDNO)10  1.411e-02  1.522e-02   0.927 0.354341    
# factor(IDNO)11  1.514e-02  1.375e-02   1.102 0.271239    
# factor(IDNO)13  1.447e-02  1.628e-02   0.889 0.374516    
# factor(IDNO)14  1.989e-02  1.402e-02   1.419 0.156762    
# factor(IDNO)15  2.801e-02  1.467e-02   1.909 0.056967 .  
# factor(IDNO)16  1.232e-01  1.545e-02   7.974 1.55e-14 ***
#   factor(IDNO)18  7.580e-02  2.334e-02   3.248 0.001260 ** 
#   factor(IDNO)19  1.411e-02  2.691e-02   0.524 0.600350    
# factor(IDNO)20  1.636e-02  1.656e-02   0.988 0.323550    
# factor(IDNO)21  1.785e-02  1.545e-02   1.155 0.248594    
# factor(IDNO)23  5.838e-02  1.901e-02   3.071 0.002277 ** 
#   factor(IDNO)26  2.483e-02  1.830e-02   1.357 0.175541    
# factor(IDNO)27  5.833e-02  3.284e-02   1.776 0.076433 .  
# factor(IDNO)29  3.220e-02  1.841e-02   1.749 0.081088 .  
# factor(IDNO)30  1.506e-02  1.505e-02   1.001 0.317584    
# factor(IDNO)31  1.169e-02  1.650e-02   0.709 0.478873    
# factor(IDNO)32  1.145e-02  1.741e-02   0.658 0.510995    
# factor(IDNO)33  1.881e-02  1.601e-02   1.175 0.240717    
# factor(IDNO)34  1.151e-02  4.651e-02   0.248 0.804618    
# factor(IDNO)36  5.024e-02  1.401e-02   3.586 0.000377 ***
#   factor(IDNO)37  2.775e-02  1.358e-02   2.044 0.041616 *  
#   factor(IDNO)38  1.711e-02  1.597e-02   1.072 0.284383    
# factor(IDNO)39  1.993e-02  1.444e-02   1.381 0.168051    
# factor(IDNO)40  2.073e-02  1.528e-02   1.357 0.175565    
# factor(IDNO)41  9.973e-03  4.659e-02   0.214 0.830610    
# factor(IDNO)42  1.420e-02  1.466e-02   0.969 0.333287    
# factor(IDNO)43  1.335e-02  1.464e-02   0.912 0.362274    
# factor(IDNO)45  2.589e-01  3.280e-02   7.893 2.73e-14 ***
#   factor(IDNO)46  1.637e-02  1.649e-02   0.992 0.321608    
# factor(IDNO)47  2.088e-02  1.634e-02   1.278 0.202021    
# factor(IDNO)48  1.870e-02  1.698e-02   1.101 0.271446    
# factor(IDNO)49  1.828e-02  1.622e-02   1.127 0.260253    
# factor(IDNO)50  1.650e-02  2.275e-02   0.725 0.468860    
# factor(IDNO)52  1.390e-02  2.414e-02   0.576 0.565143    
# factor(IDNO)53  1.590e-02  2.382e-02   0.668 0.504814    
# factor(IDNO)54  2.001e-02  2.537e-02   0.789 0.430861    
# factor(IDNO)55  1.403e-02  2.335e-02   0.601 0.548210    
# factor(IDNO)56  1.620e-02  3.416e-02   0.474 0.635619    
# factor(IDNO)57  3.053e-02  2.829e-02   1.079 0.281118    
# factor(IDNO)58  5.948e-03  2.675e-02   0.222 0.824176    
# factor(IDNO)59  2.662e-02  3.555e-02   0.749 0.454441    
# factor(IDNO)60  3.522e-03  4.633e-02   0.076 0.939441    
# factor(IDNO)73  6.510e-04  4.633e-02   0.014 0.988797    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04633 on 409 degrees of freedom
# Multiple R-squared:  0.3113,	Adjusted R-squared:  0.2204 
# F-statistic: 3.424 on 54 and 409 DF,  p-value: 1.058e-12

# Q2 

# Call:
#   lm(formula = ov_share ~ nd + I(nd^2) + factor(IDNO) - 1, data = q2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.039696 -0.005259 -0.000624  0.002889  0.121935 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# nd             -7.690e-04  1.601e-04  -4.805 2.18e-06 ***
#   I(nd^2)         6.550e-06  2.000e-06   3.275 0.001147 ** 
#   factor(IDNO)1   2.701e-02  5.145e-03   5.250 2.46e-07 ***
#   factor(IDNO)2   1.922e-02  3.864e-03   4.974 9.70e-07 ***
#   factor(IDNO)3   1.306e-02  3.653e-03   3.575 0.000392 ***
#   factor(IDNO)4   1.795e-02  5.239e-03   3.427 0.000672 ***
#   factor(IDNO)5   2.141e-02  4.756e-03   4.502 8.80e-06 ***
#   factor(IDNO)6   3.414e-02  7.718e-03   4.424 1.25e-05 ***
#   factor(IDNO)7   3.118e-02  3.696e-03   8.437 5.76e-16 ***
#   factor(IDNO)8   2.417e-02  4.561e-03   5.298 1.92e-07 ***
#   factor(IDNO)9   2.480e-02  4.845e-03   5.119 4.74e-07 ***
#   factor(IDNO)10  1.587e-02  4.375e-03   3.627 0.000323 ***
#   factor(IDNO)11  1.635e-02  3.951e-03   4.139 4.25e-05 ***
#   factor(IDNO)13  1.591e-02  4.680e-03   3.400 0.000740 ***
#   factor(IDNO)14  2.122e-02  4.030e-03   5.265 2.27e-07 ***
#   factor(IDNO)15  2.960e-02  4.218e-03   7.018 9.47e-12 ***
#   factor(IDNO)16  2.348e-02  4.710e-03   4.986 9.15e-07 ***
#   factor(IDNO)18  7.638e-02  6.708e-03  11.387  < 2e-16 ***
#   factor(IDNO)19  1.471e-02  7.734e-03   1.902 0.057914 .  
# factor(IDNO)20  1.757e-02  4.758e-03   3.693 0.000252 ***
#   factor(IDNO)21  1.797e-02  4.441e-03   4.046 6.25e-05 ***
#   factor(IDNO)23  5.877e-02  5.464e-03  10.757  < 2e-16 ***
#   factor(IDNO)26  2.649e-02  5.259e-03   5.037 7.12e-07 ***
#   factor(IDNO)27  5.880e-02  9.438e-03   6.230 1.16e-09 ***
#   factor(IDNO)29  3.433e-02  5.292e-03   6.487 2.55e-10 ***
#   factor(IDNO)30  1.677e-02  4.327e-03   3.877 0.000123 ***
#   factor(IDNO)31  1.210e-02  4.742e-03   2.551 0.011098 *  
#   factor(IDNO)32  1.264e-02  5.003e-03   2.527 0.011875 *  
#   factor(IDNO)33  2.079e-02  4.603e-03   4.518 8.20e-06 ***
#   factor(IDNO)34  1.233e-02  1.337e-02   0.923 0.356677    
# factor(IDNO)36  5.046e-02  4.027e-03  12.532  < 2e-16 ***
#   factor(IDNO)37  2.823e-02  3.903e-03   7.234 2.35e-12 ***
#   factor(IDNO)38  1.912e-02  4.589e-03   4.166 3.80e-05 ***
#   factor(IDNO)39  2.127e-02  4.149e-03   5.125 4.60e-07 ***
#   factor(IDNO)40  2.252e-02  4.392e-03   5.128 4.54e-07 ***
#   factor(IDNO)41  1.097e-02  1.339e-02   0.820 0.412931    
# factor(IDNO)42  1.579e-02  4.215e-03   3.746 0.000206 ***
#   factor(IDNO)43  1.424e-02  4.207e-03   3.385 0.000782 ***
#   factor(IDNO)45  4.955e-02  1.332e-02   3.722 0.000226 ***
#   factor(IDNO)46  1.840e-02  4.741e-03   3.881 0.000121 ***
#   factor(IDNO)47  2.290e-02  4.696e-03   4.877 1.55e-06 ***
#   factor(IDNO)48  2.012e-02  4.880e-03   4.124 4.52e-05 ***
#   factor(IDNO)49  2.028e-02  4.661e-03   4.352 1.71e-05 ***
#   factor(IDNO)50  1.838e-02  6.539e-03   2.811 0.005175 ** 
#   factor(IDNO)52  1.527e-02  6.938e-03   2.201 0.028274 *  
#   factor(IDNO)53  1.702e-02  6.845e-03   2.487 0.013295 *  
#   factor(IDNO)54  2.208e-02  7.292e-03   3.028 0.002619 ** 
#   factor(IDNO)55  1.463e-02  6.711e-03   2.180 0.029796 *  
#   factor(IDNO)56  1.815e-02  9.818e-03   1.848 0.065294 .  
# factor(IDNO)57  3.238e-02  8.129e-03   3.983 8.07e-05 ***
#   factor(IDNO)58  6.059e-03  7.689e-03   0.788 0.431097    
# factor(IDNO)59  2.910e-02  1.022e-02   2.848 0.004625 ** 
#   factor(IDNO)60  3.634e-03  1.332e-02   0.273 0.785072    
# factor(IDNO)73  7.625e-04  1.332e-02   0.057 0.954363    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01331 on 407 degrees of freedom
# Multiple R-squared:  0.6554,	Adjusted R-squared:  0.6097 
# F-statistic: 14.33 on 54 and 407 DF,  p-value: < 2.2e-16


# Call:
#   lm(formula = ov_share ~ nd + I(nd^2) + factor(nd > 7) + factor(IDNO), 
#      data = q)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.20883 -0.00425 -0.00074  0.00237  0.79884 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         2.927e-02  1.839e-02   1.591 0.112337    
# nd                 -3.347e-04  6.407e-04  -0.522 0.601654    
# I(nd^2)             1.967e-06  7.686e-06   0.256 0.798152    
# factor(nd > 7)TRUE -1.100e-02  1.085e-02  -1.014 0.311396    
# factor(IDNO)2      -4.744e-03  1.993e-02  -0.238 0.811992    
# factor(IDNO)3      -1.532e-02  2.077e-02  -0.738 0.461142    
# factor(IDNO)4      -5.870e-03  2.351e-02  -0.250 0.802974    
# factor(IDNO)5      -5.468e-03  1.830e-02  -0.299 0.765169    
# factor(IDNO)6       2.980e-03  3.201e-02   0.093 0.925874    
# factor(IDNO)7       1.486e-03  2.226e-02   0.067 0.946812    
# factor(IDNO)8      -1.948e-03  1.866e-02  -0.104 0.916915    
# factor(IDNO)9      -2.437e-03  1.828e-02  -0.133 0.893984    
# factor(IDNO)10     -9.385e-03  1.872e-02  -0.501 0.616373    
# factor(IDNO)11     -8.987e-03  1.959e-02  -0.459 0.646737    
# factor(IDNO)13     -7.963e-03  2.102e-02  -0.379 0.705017    
# factor(IDNO)14     -4.606e-03  1.940e-02  -0.237 0.812427    
# factor(IDNO)15      4.316e-03  1.914e-02   0.225 0.821725    
# factor(IDNO)16      9.364e-02  2.385e-02   3.925 0.000102 ***
#   factor(IDNO)18      4.761e-02  2.858e-02   1.666 0.096471 .  
# factor(IDNO)19     -1.687e-02  3.195e-02  -0.528 0.597656    
# factor(IDNO)20     -7.852e-03  2.166e-02  -0.362 0.717200    
# factor(IDNO)21     -1.173e-02  2.385e-02  -0.492 0.623143    
# factor(IDNO)23      2.800e-02  2.592e-02   1.080 0.280703    
# factor(IDNO)26      6.529e-04  1.781e-02   0.037 0.970771    
# factor(IDNO)27      3.323e-02  3.677e-02   0.904 0.366783    
# factor(IDNO)29      6.890e-03  1.753e-02   0.393 0.694465    
# factor(IDNO)30     -8.265e-03  1.889e-02  -0.438 0.661909    
# factor(IDNO)31     -1.873e-02  2.412e-02  -0.776 0.438012    
# factor(IDNO)32     -1.165e-02  2.242e-02  -0.520 0.603509    
# factor(IDNO)33     -6.215e-03  1.832e-02  -0.339 0.734555    
# factor(IDNO)34     -9.116e-03  4.919e-02  -0.185 0.853062    
# factor(IDNO)36      2.034e-02  2.278e-02   0.893 0.372283    
# factor(IDNO)37     -2.882e-03  2.211e-02  -0.130 0.896355    
# factor(IDNO)38     -7.111e-03  1.857e-02  -0.383 0.701992    
# factor(IDNO)39     -3.890e-03  1.977e-02  -0.197 0.844130    
# factor(IDNO)40     -4.398e-03  1.877e-02  -0.234 0.814823    
# factor(IDNO)41     -1.118e-02  4.903e-02  -0.228 0.819779    
# factor(IDNO)42     -8.693e-03  1.922e-02  -0.452 0.651351    
# factor(IDNO)43     -1.247e-02  2.111e-02  -0.591 0.554876    
# factor(IDNO)45      2.287e-01  3.729e-02   6.132 2.05e-09 ***
#   factor(IDNO)46     -8.001e-03  1.872e-02  -0.427 0.669351    
# factor(IDNO)47     -4.382e-03  1.807e-02  -0.243 0.808450    
# factor(IDNO)48     -3.723e-03  2.154e-02  -0.173 0.862841    
# factor(IDNO)49     -6.887e-03  1.812e-02  -0.380 0.704155    
# factor(IDNO)50     -7.326e-03  2.512e-02  -0.292 0.770699    
# factor(IDNO)52     -8.359e-03  2.770e-02  -0.302 0.762977    
# factor(IDNO)53     -5.610e-03  2.801e-02  -0.200 0.841373    
# factor(IDNO)54     -4.379e-03  2.708e-02  -0.162 0.871620    
# factor(IDNO)55     -1.421e-02  2.857e-02  -0.497 0.619349    
# factor(IDNO)56     -7.787e-03  3.571e-02  -0.218 0.827481    
# factor(IDNO)57      6.832e-03  3.034e-02   0.225 0.821977    
# factor(IDNO)58     -2.364e-02  3.234e-02  -0.731 0.465277    
# factor(IDNO)59      5.545e-04  3.515e-02   0.016 0.987421    
# factor(IDNO)60     -2.606e-02  4.977e-02  -0.524 0.600770    
# factor(IDNO)73     -2.893e-02  4.977e-02  -0.581 0.561296    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04633 on 408 degrees of freedom
# Multiple R-squared:  0.2545,	Adjusted R-squared:  0.1559 
# F-statistic:  2.58 on 54 and 408 DF,  p-value: 7.601e-08

# Q2

# Call:
#   lm(formula = ov_share ~ nd + I(nd^2) + factor(nd > 7) + factor(IDNO), 
#      data = q2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.038364 -0.004299 -0.000488  0.002581  0.122643 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         3.080e-02  5.229e-03   5.891 8.07e-09 ***
#   nd                 -4.840e-04  1.823e-04  -2.656 0.008229 ** 
#   I(nd^2)             3.612e-06  2.186e-06   1.652 0.099315 .  
# factor(nd > 7)TRUE -9.741e-03  3.086e-03  -3.157 0.001714 ** 
#   factor(IDNO)2      -5.952e-03  5.667e-03  -1.050 0.294240    
# factor(IDNO)3      -1.645e-02  5.904e-03  -2.785 0.005596 ** 
#   factor(IDNO)4      -7.125e-03  6.685e-03  -1.066 0.287086    
# factor(IDNO)5      -5.383e-03  5.201e-03  -1.035 0.301279    
# factor(IDNO)6       2.092e-03  9.100e-03   0.230 0.818294    
# factor(IDNO)7       9.623e-05  6.329e-03   0.015 0.987877    
# factor(IDNO)8      -2.098e-03  5.305e-03  -0.395 0.692695    
# factor(IDNO)9      -2.231e-03  5.196e-03  -0.429 0.667821    
# factor(IDNO)10     -9.832e-03  5.321e-03  -1.848 0.065380 .  
# factor(IDNO)11     -9.908e-03  5.570e-03  -1.779 0.076048 .  
# factor(IDNO)13     -8.849e-03  5.976e-03  -1.481 0.139440    
# factor(IDNO)14     -5.365e-03  5.514e-03  -0.973 0.331192    
# factor(IDNO)15      3.724e-03  5.442e-03   0.684 0.494193    
# factor(IDNO)16     -7.604e-03  6.957e-03  -1.093 0.275060    
# factor(IDNO)18      4.653e-02  8.125e-03   5.727 1.99e-08 ***
#   factor(IDNO)19     -1.762e-02  9.081e-03  -1.940 0.053074 .  
# factor(IDNO)20     -8.764e-03  6.158e-03  -1.423 0.155486    
# factor(IDNO)21     -1.312e-02  6.782e-03  -1.935 0.053713 .  
# factor(IDNO)23      2.698e-02  7.369e-03   3.661 0.000284 ***
#   factor(IDNO)26      1.898e-04  5.062e-03   0.038 0.970104    
# factor(IDNO)27      3.168e-02  1.045e-02   3.030 0.002601 ** 
#   factor(IDNO)29      7.024e-03  4.983e-03   1.410 0.159422    
# factor(IDNO)30     -8.776e-03  5.369e-03  -1.634 0.102936    
# factor(IDNO)31     -1.973e-02  6.858e-03  -2.877 0.004226 ** 
#   factor(IDNO)32     -1.271e-02  6.372e-03  -1.994 0.046846 *  
#   factor(IDNO)33     -6.263e-03  5.207e-03  -1.203 0.229809    
# factor(IDNO)34     -1.082e-02  1.398e-02  -0.774 0.439368    
# factor(IDNO)36      1.910e-02  6.475e-03   2.949 0.003367 ** 
#   factor(IDNO)37     -3.786e-03  6.286e-03  -0.602 0.547274    
# factor(IDNO)38     -7.229e-03  5.280e-03  -1.369 0.171692    
# factor(IDNO)39     -4.723e-03  5.621e-03  -0.840 0.401211    
# factor(IDNO)40     -4.624e-03  5.335e-03  -0.867 0.386587    
# factor(IDNO)41     -1.265e-02  1.394e-02  -0.907 0.364813    
# factor(IDNO)42     -9.380e-03  5.465e-03  -1.716 0.086839 .  
# factor(IDNO)43     -1.352e-02  6.001e-03  -2.253 0.024792 *  
#   factor(IDNO)45      1.847e-02  1.415e-02   1.305 0.192516    
# factor(IDNO)46     -8.071e-03  5.322e-03  -1.516 0.130213    
# factor(IDNO)47     -4.358e-03  5.136e-03  -0.849 0.396585    
# factor(IDNO)48     -4.623e-03  6.123e-03  -0.755 0.450665    
# factor(IDNO)49     -6.896e-03  5.152e-03  -1.339 0.181466    
# factor(IDNO)50     -7.604e-03  7.141e-03  -1.065 0.287544    
# factor(IDNO)52     -9.327e-03  7.875e-03  -1.184 0.236917    
# factor(IDNO)53     -6.916e-03  7.964e-03  -0.868 0.385687    
# factor(IDNO)54     -4.407e-03  7.698e-03  -0.572 0.567363    
# factor(IDNO)55     -1.526e-02  8.123e-03  -1.879 0.060977 .  
# factor(IDNO)56     -7.987e-03  1.015e-02  -0.787 0.431841    
# factor(IDNO)57      6.503e-03  8.626e-03   0.754 0.451376    
# factor(IDNO)58     -2.503e-02  9.194e-03  -2.722 0.006767 ** 
#   factor(IDNO)59      1.122e-03  9.992e-03   0.112 0.910612    
# factor(IDNO)60     -2.745e-02  1.415e-02  -1.940 0.053020 .  
# factor(IDNO)73     -3.032e-02  1.415e-02  -2.143 0.032677 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01317 on 406 degrees of freedom
# Multiple R-squared:  0.5165,	Adjusted R-squared:  0.4522 
# F-statistic: 8.031 on 54 and 406 DF,  p-value: < 2.2e-16



a <- lm(ov_share ~ nd + loans, data = q_bs_all_year)

# Call:
#   lm(formula = ov_share ~ nd + loans, data = q)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.02506 -0.01333 -0.00608  0.00240  0.89640 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.553e-02  3.605e-03   7.082 5.36e-12 ***
#   nd          -6.290e-04  2.411e-04  -2.609  0.00937 ** 
#   loans        1.784e-10  2.979e-10   0.599  0.54955    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.0497 on 460 degrees of freedom
# Multiple R-squared:  0.0326,	Adjusted R-squared:  0.0284 
# F-statistic: 7.751 on 2 and 460 DF,  p-value: 0.0004888


# Call:
#   lm(formula = ov_share ~ nd + factor(nd > 7) + loans, data = q)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.03702 -0.00619 -0.00250  0.00245  0.88438 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         3.712e-02  4.431e-03   8.378 6.60e-16 ***
#   nd                 -6.417e-05  2.699e-04  -0.238    0.812    
# factor(nd > 7)TRUE -2.802e-02  6.449e-03  -4.344 1.72e-05 ***
#   loans              -4.653e-11  2.968e-10  -0.157    0.876    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04876 on 459 degrees of freedom
# Multiple R-squared:  0.07081,	Adjusted R-squared:  0.06473 
# F-statistic: 11.66 on 3 and 459 DF,  p-value: 2.243e-07


a <- lm(ov_share ~ nd + ndC, data = q)

# Call:
#   lm(formula = ov_share ~ nd + ndC, data = q)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.02504 -0.01332 -0.00634  0.00203  0.89637 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.0256494  0.0036826   6.965 1.14e-11 ***
#   nd          -0.0006136  0.0002384  -2.573   0.0104 *  
#   ndC          0.0001354  0.0002549   0.531   0.5957    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04971 on 460 degrees of freedom
# Multiple R-squared:  0.03244,	Adjusted R-squared:  0.02823 
# F-statistic: 7.712 on 2 and 460 DF,  p-value: 0.0005079



# Call:
#   lm(formula = ov_share ~ nd + factor(nd > 7) + ndC, data = q)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.03698 -0.00623 -0.00231  0.00225  0.88443 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         3.701e-02  4.454e-03   8.309 1.09e-15 ***
#   nd                 -3.480e-05  2.689e-04  -0.129    0.897    
# factor(nd > 7)TRUE -2.820e-02  6.467e-03  -4.361 1.60e-05 ***
#   ndC                -7.456e-05  2.547e-04  -0.293    0.770    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04876 on 459 degrees of freedom
# Multiple R-squared:  0.07093,	Adjusted R-squared:  0.06486 
# F-statistic: 11.68 on 3 and 459 DF,  p-value: 2.177e-07


# Call:
#   lm(formula = ov_share ~ nd + nb, data = q)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.02504 -0.01332 -0.00634  0.00203  0.89637 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.0256494  0.0036826   6.965 1.14e-11 ***
#   nd          -0.0007489  0.0004728  -1.584    0.114    
# nb           0.0001354  0.0002549   0.531    0.596    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04971 on 460 degrees of freedom
# Multiple R-squared:  0.03244,	Adjusted R-squared:  0.02823 
# F-statistic: 7.712 on 2 and 460 DF,  p-value: 0.0005079

# Q2 

# Call:
#   lm(formula = ov_share ~ nd + nb, data = q2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.018561 -0.009242 -0.004144  0.003763  0.161383 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.894e-02  1.248e-03  15.173  < 2e-16 ***
#   nd          -4.361e-04  1.597e-04  -2.731  0.00656 ** 
#   nb           5.466e-05  8.606e-05   0.635  0.52569    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01678 on 458 degrees of freedom
# Multiple R-squared:  0.1147,	Adjusted R-squared:  0.1108 
# F-statistic: 29.65 on 2 and 458 DF,  p-value: 7.75e-13


# Call:
#   lm(formula = ov_share ~ nd + factor(nd > 7) + nb, data = q)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.03698 -0.00623 -0.00231  0.00225  0.88443 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         3.701e-02  4.454e-03   8.309 1.09e-15 ***
#   nd                  3.975e-05  4.979e-04   0.080    0.936    
# factor(nd > 7)TRUE -2.820e-02  6.467e-03  -4.361 1.60e-05 ***
#   nb                 -7.456e-05  2.547e-04  -0.293    0.770    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04876 on 459 degrees of freedom
# Multiple R-squared:  0.07093,	Adjusted R-squared:  0.06486 
# F-statistic: 11.68 on 3 and 459 DF,  p-value: 2.177e-07

# Q2 

# Call:
#   lm(formula = ov_share ~ nd + factor(nd > 7) + nb, data = q2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.026101 -0.005315 -0.001637  0.003019  0.153842 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         2.612e-02  1.438e-03  18.171  < 2e-16 ***
#   nd                  5.363e-05  1.594e-04   0.336    0.737    
# factor(nd > 7)TRUE -1.763e-02  2.079e-03  -8.480 3.15e-16 ***
#   nb                 -7.586e-05  8.155e-05  -0.930    0.353    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01561 on 457 degrees of freedom
# Multiple R-squared:  0.235,	Adjusted R-squared:   0.23 
# F-statistic:  46.8 on 3 and 457 DF,  p-value: < 2.2e-16


# IB SHARE, IB PORT #####


q <- q_bs_inc_year %>% filter(year >= 1874)
q2 <- q_bs_inc_year %>% filter(year <= 1913, ov_share < 0.25)

# q <- q_bs_inc %>% filter(year %in% c(1894,1900:1910))



qq <- q_bs_all %>% mutate(group = IDNO %in% c(1,43))
# q <- q %>% filter(group == 0)

# q  %>% filter(year %in% c(1882,1889,1894)) %>%
# qq  %>% filter(year %in% c(1876,1880)) %>%
  qq  %>%
  ggplot(aes(x=pop/pop_year,y=IBa_share)) +
  geom_point(aes(group = as.factor(group), color = as.factor(group)), size = 3, alpha = 0.5)
  
  +
  geom_label(aes(label = IDNO) )


+
  # ylim(0,0.5)+
  # geom_point(aes(group = as.factor(year), color = as.factor(year)), size = 3, alpha = 0.5) +
  # geom_point(size = 3, alpha = 0.5) +
  geom_label(aes(label = year) ) +
  # geom_label(aes(label = year) ) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2),size= 2, se = T) +
  # stat_smooth(aes(group = as.factor(year), color = as.factor(year)), method = "lm",
  # formula = y ~ x + I(x^2),size= 2, se = F)+
  labs(title = "Bank expansion and correspondent assets",
       x = "Share of district coverage", y = "Share of inter-bank correspondent assets")  +
  theme(
    legend.title = element_blank(),
    legend.text=element_text(size=12),
    title = element_markdown(),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1)
  ) 


qq <- q %>% mutate(group = IDNO %in% c(26,10))

# qq  %>%
  qq  %>% filter(year >= 1874) %>%
  ggplot(aes(x= ncc_p ,y= IBl_share)) +
  # ggplot(aes(x=1/2*d_rel.p + 1/2*cc_rel.p,y=IBl_share)) +
  # geom_point(aes(group = as.factor(group), color = as.factor(group)), size = 3, alpha = 0.5) +
  geom_point(size = 3, alpha = 0.5) +
  geom_label(aes(label = IDNO) ) +
  # geom_label(aes(label = year) ) +
  # stat_smooth(method = "lm", formula = y ~ x + I(x^2),size= 2, se = F)+
  # stat_smooth(aes(group = as.factor(year), color = as.factor(year)), method = "lm", formula = y ~ x + I(x^2),size= 2, se = F)+
  labs(title = "Bank expansion and correspondent liabilities",
       x = "Relative number of districts with at least one branch", y = "Share of inter-bank correspondent liabilities")  +
  theme(
    legend.title = element_blank(),
    legend.text=element_text(size=12),
    title = element_markdown(),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1)
  ) 






#  


# qq <- q %>% mutate(group = IDNO %in% c(21,3))
qq <- q %>% mutate(group = cc_rel > 0)
qq  %>%
  # qq  %>% filter(year >= 1894) %>%
  # ggplot(aes(x=d_rel.p + cc_rel.p + nd_noverl.p/nd,y=ibl/Totalass)) +
  ggplot(aes(x=ncc_p,y=ibl/Totalass)) +
  # geom_point(aes(group = group, color = group), size = 3, alpha = 0.5) +
  geom_point(size = 3, alpha = 0.5) +
  ylim(0,0.2)
  # geom_label(aes(label = year) ) +
  # geom_label(aes(label = IDNO) ) +
  # stat_smooth(aes(group = as.factor(year), color = as.factor(year)), method = "lm", 
  # formula = y ~ x + I(x^2),size= 2, se = F)+
  labs(title = "Bank expansion and correspondent liabilities",
       x = "Relative number of districts with at least one branch",
       y = "Portfolio of inter-bank correspondent liabilities")  +
  theme(
    legend.title = element_blank(),
    legend.text=element_text(size=12),
    title = element_markdown(),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1)
  ) + ylim(0,0.075)


  
  
# q <- q %>% mutate(group = IDNO %in% c(31,21))
q <- q %>% mutate(group = cc_rel <= 1/5)
# q  %>% filter(!(IDNO == 21)) %>%
# q_bs_inc_year %>% filter(year <= c(1913)) %>%
  # q_bs_inc_year %>% filter(year >= c(1874)) %>%
  q_bs_inc_year  %>%
  # ggplot(aes(x=d_cover,y=(iba + Nchqobks)/Totalass)) +
  ggplot(aes(y=ncc_p,x=pop/pop_year)) +
  # ggplot(aes(x=d_cover,y=(iba)/Totalass)) +
  # geom_point(aes(group = IDNO, color = as.factor(IDNO)), size = 3, alpha = 0.5) + xlim(0,1) +
  # geom_label(aes(label = IDNO) )
  geom_point(size = 3, alpha = 0.5) + xlim(0,1)

+
  # geom_point(size = 3, alpha = 0.5)
  # + xlim(0,5e7)
  geom_label(aes(label = year) ) +
  # geom_label(aes(label = year) ) +
  labs(title = "Bank expansion and correspondent assets",
       x = "Share of district coverage", y = "Portfolio of inter-bank correspondent assets")  +
  theme(
    legend.title = element_blank(),
    legend.text=element_text(size=12),
    title = element_markdown(),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1))


+ ylim(0,0.25)

# + xlim(0,0.01) + ylim(0,0.025)




z <- lm(IBl_share ~ d_rel + I(d_rel^2) +  factor(year) - 1, data = q[which(q$group == 0),])

z <- lm(IBa_share ~ d_rel + I(d_rel^2), data = q)



# predict failure
# include notes and cheques



# VARIANCE #####  


q <- q_bs_all %>% group_by(IDNO,year) %>%
  summarise(ov_share_sd = sd(ov_share), 
            ov_share_cv = sd(ov_share)/mean(ov_share),
            ov_share_m = mean(ov_share))

q <- left_join(q_bs_all_year,q, by = c("IDNO","year"))

q  %>%
  ggplot(aes(x=ov_share_m,y= ov_share_sd,group = factor(IDNO), color = factor(IDNO))) +
  # geom_point(size = 3, alpha = 0.5) +
  geom_point(size = 3, alpha = 0.5) +
  ylim(0,0.03) +
  xlim(0,0.1) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^-1), size = 1, se = F)



q <- q %>%
  group_by(IDNO) %>%
  mutate(nb_ch = nb - Lag(nb), ovsd_ch = ov_share_sd - Lag(ov_share_sd)) # growth rate in percent

q  %>%
  ggplot(aes(x=nb_ch,y= ovsd_ch,group = factor(IDNO), color = factor(IDNO))) +
  # geom_point(size = 3, alpha = 0.5) +
  geom_point(size = 3, alpha = 0.5) +
  ylim(-0.03,0.03) +
  xlim(-10,50) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^-1), size = 1, se = F)


# IDNO INFO #####

i <- 9
z2 <- q_b_agg  %>% filter(IDNO %in% i) 
br <- branches %>% filter(IDNO == i)
bs <- df_all %>% filter(IDNO == i)
tu <- turnover %>% filter(IDNO == i)




# BS CROSS SECTION #####

q <- q_bs_inc %>% filter(year <= 1913)

q$group <- NULL
q$group2 <- NULL
q$group3 <- NULL
q$group4 <- NULL
q$group5 <- NULL

quant <- quantile(q$d_rel, c(0.25, 0.5, 0.75)) 
quant2 <- quantile(q$d_rel, c(0.333, 0.6667)) 
quant3 <- quantile(q$d_rel, c(0.25, 0.75)) 
quant4 <- quantile(q$d_rel, c(0.2, 0.8)) 
quant5 <- quantile(q$d_rel, c(0.1, 0.9))


for (i in 1:NROW(q)){
  
  if (q$d_rel[i] <= quant[1]){
    
    q$group[i] <- 1
    
  } else if (q$d_rel[i] <= quant[2]){
    
    q$group[i] <- 2
    
  }else if (q$d_rel[i] <= quant[3]){
    
    q$group[i] <- 3
    
  }else {
    
    q$group[i] <- 4
    
  }
  
}


for (i in 1:NROW(q)){
  
  if (q$d_rel[i] <= quant2[1]){
    
    q$group2[i] <- 1
    
  } else if (q$d_rel[i] <= quant2[2]){
    
    q$group2[i] <- 2
    
  }else {
    
    q$group2[i] <- 3
    
  }
  
}

for (i in 1:NROW(q)){
  
  if (q$d_rel[i] <= quant3[1]){
    
    q$group3[i] <- "p < 0.25"
    
  } else if (q$d_rel[i] <= quant3[2]){
    
    q$group3[i] <- ""
    
  }else {
    
    q$group3[i] <- "p > 0.75"
    
  }
  
}
q$group3 = factor(q$group3,levels= c("p < 0.25","","p > 0.75")) 


for (i in 1:NROW(q)){
  
  if (q$d_rel[i] <= quant4[1]){
    
    q$group4[i] <- "p < 0.2"
    
  } else if (q$d_rel[i] <= quant4[2]){
    
    q$group4[i] <- ""
    
  }else {
    
    q$group4[i] <- "p > 0.8"
    
  }
  
}

for (i in 1:NROW(q)){
  
  if (q$d_rel[i] <= quant5[1]){
    
    q$group5[i] <- "p < 0.1"
    
  } else if (q$d_rel[i] <= quant5[2]){
    
    q$group5[i] <- ""
    
  }else {
    
    q$group5[i] <- "p > 0.9"
    
  }
  
}

q$group4 = factor(q$group4,levels= c("p < 0.2","","p > 0.8")) 

q$group5 = factor(q$group5,levels= c("p < 0.1","","p > 0.9")) 


quant <- quantile(q$d_cover, c(0.25, 0.5, 0.75)) 
quant2 <- quantile(q$d_cover, c(0.333, 0.6667)) 
quant3 <- quantile(q$d_cover, c(0.25, 0.75)) 
quant4 <- quantile(q$d_cover, c(0.2, 0.8)) 
quant5 <- quantile(q$d_cover, c(0.1, 0.9))

for (i in 1:NROW(q)){
  
  if (q$d_cover[i] <= quant[1]){
    
    q$group[i] <- 1
    
  } else if (q$d_cover[i] <= quant[2]){
    
    q$group[i] <- 2
    
  }else if (q$d_cover[i] <= quant[3]){
    
    q$group[i] <- 3
    
  }else {
    
    q$group[i] <- 4
    
  }
  
}


for (i in 1:NROW(q)){
  
  if (q$d_cover[i] <= quant2[1]){
    
    q$group2[i] <- 1
    
  } else if (q$d_cover[i] <= quant2[2]){
    
    q$group2[i] <- 2
    
  }else {
    
    q$group2[i] <- 3
    
  }
  
}

for (i in 1:NROW(q)){
  
  if (q$d_cover[i] <= quant3[1]){
    
    q$group3[i] <- "p < 0.25"
    
  } else if (q$d_cover[i] <= quant3[2]){
    
    q$group3[i] <- ""
    
  }else {
    
    q$group3[i] <- "p > 0.75"
    
  }
  
}
q$group3 = factor(q$group3,levels= c("p < 0.25","","p > 0.75")) 


for (i in 1:NROW(q)){
  
  if (q$d_cover[i] <= quant4[1]){
    
    q$group4[i] <- "p < 0.2"
    
  } else if (q$d_cover[i] <= quant4[2]){
    
    q$group4[i] <- ""
    
  }else {
    
    q$group4[i] <- "p > 0.8"
    
  }
  
}

for (i in 1:NROW(q)){
  
  if (q$d_cover[i] <= quant5[1]){
    
    q$group5[i] <- "p < 0.1"
    
  } else if (q$d_cover[i] <= quant5[2]){
    
    q$group5[i] <- ""
    
  }else {
    
    q$group5[i] <- "p > 0.9"
    
  }
  
}

q$group4 = factor(q$group4,levels= c("p < 0.2","","p > 0.8")) 

q$group5 = factor(q$group5,levels= c("p < 0.1","","p > 0.9")) 



quant <- quantile(q$Totalass, c(0.25, 0.5, 0.75)) 
quant2 <- quantile(q$Totalass, c(0.333, 0.6667)) 
quant3 <- quantile(q$Totalass, c(0.25, 0.75)) 
quant4 <- quantile(q$Totalass, c(0.2, 0.8)) 
quant5 <- quantile(q$Totalass, c(0.1, 0.9))

for (i in 1:NROW(q)){
  
  if (q$Totalass[i] <= quant[1]){
    
    q$group[i] <- 1
    
  } else if (q$Totalass[i] <= quant[2]){
    
    q$group[i] <- 2
    
  }else if (q$Totalass[i] <= quant[3]){
    
    q$group[i] <- 3
    
  }else {
    
    q$group[i] <- 4
    
  }
  
}


for (i in 1:NROW(q)){
  
  if (q$Totalass[i] <= quant2[1]){
    
    q$group2[i] <- 1
    
  } else if (q$Totalass[i] <= quant2[2]){
    
    q$group2[i] <- 2
    
  }else {
    
    q$group2[i] <- 3
    
  }
  
}

for (i in 1:NROW(q)){
  
  if (q$Totalass[i] <= quant3[1]){
    
    q$group3[i] <- "p < 0.25"
    
  } else if (q$Totalass[i] <= quant3[2]){
    
    q$group3[i] <- ""
    
  }else {
    
    q$group3[i] <- "p > 0.75"
    
  }
  
}
q$group3 = factor(q$group3,levels= c("p < 0.25","","p > 0.75")) 


for (i in 1:NROW(q)){
  
  if (q$Totalass[i] <= quant4[1]){
    
    q$group4[i] <- "p < 0.2"
    
  } else if (q$Totalass[i] <= quant4[2]){
    
    q$group4[i] <- ""
    
  }else {
    
    q$group4[i] <- "p > 0.8"
    
  }
  
}

for (i in 1:NROW(q)){
  
  if (q$Totalass[i] <= quant5[1]){
    
    q$group5[i] <- "p < 0.1"
    
  } else if (q$Totalass[i] <= quant5[2]){
    
    q$group5[i] <- ""
    
  }else {
    
    q$group5[i] <- "p > 0.9"
    
  }
  
}

q$group4 = factor(q$group4,levels= c("p < 0.2","","p > 0.8")) 

q$group5 = factor(q$group5,levels= c("p < 0.1","","p > 0.9")) 






q_sr <- q

q_sr$g <- q_sr$group5



q1 <- q_sr %>% 
  group_by(g) %>% 
  summarise(reserves =  sum(reserves, na.rm = T),
            callosc      =  sum(callosc, na.rm = T),
            secs      =  sum(secs, na.rm = T),
            callc      =  sum(callc, na.rm = T),
            overdue      =  sum(overdue, na.rm = T),
            currloans      =  sum(currloans, na.rm = T),
            iba_sec      =  sum(iba_sec, na.rm = T),
            iba      =  sum(iba, na.rm = T),
            oa      =  sum(oa, na.rm = T),
            rowa      =  sum(rowa, na.rm = T),
            iba_row      =  sum(iba_row, na.rm = T),
            # a      =  sum(a, na.rm = T),
            ll      =  sum(demliab, na.rm = T),
            ill      =  sum(timeliab, na.rm = T),
            ibl_sec      =  sum(ibl_sec, na.rm = T),
            ibl      =  sum(ibl, na.rm = T),
            ol      =  sum(ol, na.rm = T),
            rowl      =  sum(rowl, na.rm = T),
            ibl_row      =  sum(ibl_row, na.rm = T),
            l      =  sum(l, na.rm = T),
            eq      =  sum(eq, na.rm = T))
# eq2      =  sum(eq2, na.rm = T), 
# eq3      =  sum(eq3, na.rm = T))


q1 <- q1 %>% mutate(la = callosc + secs + callc, ila = currloans + overdue, rowa = iba_row + rowa, rowl = ibl_row + rowl, iba = iba + iba_sec, ibl = ibl + ibl_sec)

qa <- q1 %>% select(g,reserves,iba,la,ila,rowa)
colnames(qa) <- c("group","Reserves","Inter-bank assets", "Liquid assets", "Illiquid assets","ROW assets")
ql <- q1 %>% select(g,ibl,ll,ill,rowl,eq)
colnames(ql) <- c("group","Inter-bank liabilities","Liquid liabilities", "Illiquid liabilities","ROW liabilities","Equity")

qa <- melt(qa, id.vars = "group")
qa <- qa %>% mutate(f = "Assets")
ql <- melt(ql, id.vars = "group")
ql <- ql %>% mutate(f = "Liabilities")

q2 <- rbind(qa,ql)




q2  %>%
  ggplot(aes(fill=variable, y=value, x=group)) + 
  geom_bar(position="fill", stat="identity") +
  facet_wrap(~f) +
  scale_fill_manual(values=c("Reserves" = "cyan",
                             "Inter-bank assets" = "dodgerblue4",
                             "Liquid assets" =  "dodgerblue1",
                             "Illiquid assets" =  "firebrick4",
                             "ROW assets" = "black",
                             "Inter-bank liabilities" = "dodgerblue4",
                             "Liquid liabilities" =  "dodgerblue1",
                             "Illiquid liabilities" =  "firebrick4",
                             "ROW liabilities" = "black",
                             "Equity" = "salmon2"
  )) +
  labs(title = "Bank coverage percentiles and balance sheets",
       x = "", y = "Share")  +
  theme(
    legend.title = element_blank(),
    legend.text=element_text(size=15),
    legend.key.size = unit(1, "cm"),
    # legend.position = "bottom",
    title = element_markdown(),
    plot.title = element_text(size=15, face="bold", hjust = 0.5,
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=15, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1),
    strip.text = element_text(size = 15)
  ) 


# BS TIME SERIES #####

q <- q_bs_all_year %>% filter(year >= 1901, year <= 1910)


z1 <- q  %>%
  ggplot(aes(x=Date,y=timeliab/Totalass)) +
  geom_point(aes(group = factor(group), color = factor(group)), size = 3, alpha = 0.5) +
  labs(title = "Branch expansion and balance sheets",
       x = "", y = "Liquid liabilities")  +
  theme(
    legend.title = element_blank(),
    legend.text=element_text(size=12),
    title = element_markdown(),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1)
  )



z2 <- q %>%
  ggplot(aes(x=Date,y=ill)) +
  geom_point(aes(group = factor(group), color = factor(group)), size = 3, alpha = 0.5)+
  labs(title = "",
       x = "", y = "Illiquid liabilities")  +
  theme(
    legend.title = element_blank(),
    legend.text=element_text(size=12),
    title = element_markdown(),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1)
  )

z3 <- q %>%
  ggplot(aes(x=Date,y=eq)) +
  geom_point(aes(group = factor(group), color = factor(group)), size = 3, alpha = 0.5)+
  labs(title = "",
       x = "", y = "Capital ratio")  +
  theme(
    legend.title = element_blank(),
    legend.text=element_text(size=12),
    title = element_markdown(),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1)
  )

z4 <- q  %>%
  ggplot(aes(x=Date,y=ol)) +
  geom_point(aes(group = factor(group), color = factor(group)), size = 3, alpha = 0.5)+
  labs(title = "",
       x = "", y = "Other liabilities")  +
  theme(
    legend.title = element_blank(),
    legend.text=element_text(size=12),
    title = element_markdown(),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1)
  )




z3 <- q %>%
  ggplot(aes(x=Date,y=iba/Totalass)) +
  geom_point(aes(group = factor(group), color = factor(group)), size = 3, alpha = 0.5)+
  labs(title = "",
       x = "", y = "Capital ratio")  +
  theme(
    legend.title = element_blank(),
    legend.text=element_text(size=12),
    title = element_markdown(),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1)
  )

z4 <- q  %>%
  ggplot(aes(x=Date,y=ibl/Totalass)) +
  geom_point(aes(group = factor(group), color = factor(group)), size = 3, alpha = 0.5)+
  labs(title = "",
       x = "", y = "Other liabilities")  +
  theme(
    legend.title = element_blank(),
    legend.text=element_text(size=12),
    title = element_markdown(),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1)
  )



# library(grid)
# install.packages("gtable")

grid.newpage()
# grid.draw(rbind(ggplotGrob(z1), ggplotGrob(z2),ggplotGrob(z3), ggplotGrob(z4), size = "last"))
grid.draw(rbind(ggplotGrob(z3), ggplotGrob(z4), size = "last"))



# also need to do ROE etc
# and network

# BRANCH EXPANSION #####

q <- q_b_agg_idno %>% mutate(group = n_rel > 0.5)

q <- q_b_agg_idno %>% filter(year <= 1933)

# q <- q %>% mutate(group = IDNO %in% c(1,5,26,49,10,30,29,8,9))
q <- q %>% mutate(group = IDNO %in% c(1,5,26,49,10,30,29,8,9,47,40,33,14))

q <- left_join(q,turnover, by = c("year","IDNO"))
q <- q %>% filter(!(turnover == 0))


q  %>% filter(IDNO %in% c(1,40,47,49,50)) %>%
# q  %>% filter(year %in% c(1869,1870,1876,1882,1889)) %>%
  ggplot(aes(x=nd,y=ndC, group = IDNO, color = as.factor(IDNO))) +
  geom_point(size = 2, alpha = 0.5) +
  geom_abline(intercept = 0) + 
  stat_smooth(method = "lm", formula = y ~ x, size = 1, se = F) +
  labs(title = "Branch expansion strategy",
       x = "Number of districts with at least one branch", y = "Number of duplicated district branches")  +
  # geom_label(aes(label = IDNO) )+
  theme(
    legend.title = element_blank(),
    legend.text=element_text(size=12),
    # title = element_markdown(),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1)
  ) 



# q %>%
# q  %>% filter(year == 1889) %>%
q  %>% 
  ggplot(aes(x=nd,y=ndC, group = year, color = as.factor(year))) +
  geom_point(size = 3, alpha = 0.5) +
  geom_abline(intercept = 0) + 
  # geom_smooth(method = 'loess', se = F) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = F) +
  geom_label(aes(label = year) )
labs(title = "Branch expansion for 8 largest banks and rest, only survivors",
     x = "Number of districts with at least one branch", y = "Number of duplicated district branches")  +
  # theme_classic() +
  theme(
    legend.title = element_blank(),
    # legend.position = "bottom",
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






# q  %>% filter(group == 0) %>%
q  %>% filter(year >= 1870) %>%
  ggplot(aes(x=nd,y=ndC, group = year, color = as.factor(year))) +
  geom_point(size = 3, alpha = 0.1) +
  geom_abline(intercept = 0) + 
  xlim(0,20) +
  ylim(0,20) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = F) +
  labs(title = "Branch expansion strategy, by year",
       x = "Number of districts with at least one branch", y = "Number of duplicated district branches")  +
  theme(
    legend.title = element_blank(),
    legend.spacing.y = unit(0, 'cm'),
    legend.text=element_text(size=12),
    title = element_markdown(),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1)
  ) 





z <- q %>% group_by(group,year) %>% summarise(ndc_nd = mean(ndc_nd),nd = mean(nd),ndC = mean(ndC))

# q  %>% filter(year >= 1900, group == 1) %>%
q %>%
  ggplot(aes(x=year,y=ndc_nd, group = group, color = as.factor(group))) +
  geom_point(size = 2, alpha = 1) +
  ylim(0,50) +
  geom_abline(intercept = 0) + 
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 2, se = F)



q  %>% filter(year >= 1870) %>%
  ggplot(aes(x = year, y = ncc, fill = as.factor(ncc))) + 
  geom_bar(stat = "identity")


z <- q %>% group_by(year) %>% summarise(cover = mean(nd_cov, na.rm = T))



# TURNOVER PREDICTION ####

q <- q_bs_inc %>% filter(year <= 1913)

q$group <- NA
for (i in 1:NROW(q)){
  
  if ((q$turnover[i] %in% c(0.2,0.3,0.4,0.5))){
    
    q$group[i] <- 1
    
  } else{
    q$group[i] <- 0
    
    
  }
  
}



# q2 <- q %>% filter(turnover %in% c(1,1.1,0.1))

plot(q$ov_share,q$group)
plot(q$nd,q$group)

a <- glm(formula = group ~ secs/Totalass +  eq/Totalass, data = q, family = binomial(link = "logit"))

a <- glm(formula = group ~ nd + Totalass, data = q, family = binomial(link = "logit"))

a <- lm(formula = group ~  ov_share + Totalass + d_rel + d_cover, data = q)

# CRISIS OF 1907 ####

# df_all %>%0
# df_all %>% filter(!(IDNO %in% c(49,1,5,8,9,10,26,29,30,33,49,13,50)), year <= 1910, year >= 1850) %>%
df_all %>% filter(year <= 1895, year >= 1885, IDNO %in% c(40)) %>%
  # df_all %>% filter(year <= 1910, year >= 1850) %>%
  # df_all %>% filter((IDNO %in% c(49,1,5,8,9,10,26,29,30,33,49)), year <= 1913, year >= 1850) %>%
  # df_all %>% filter(year <= 1983, year >= 1850) %>%
  # df_all %>% filter(year <= 1905, year >= 1851) %>%
  ggplot(aes(x= Date, y= Call, group = IDNO, color = as.factor(IDNO))) +
  geom_line(size = 1)  + ylim(0,0.33)
geom_line(aes(x = Date, y = rowl), size = 1, color = "green") +
  geom_line(aes(x = Date, y =  callosc),color = "blue") +
  
  geom_line(aes(x = Date, y = currloans), color = "gold4")+
  # geom_line(aes(x = Date, y = demliab), color = "purple")+ 
  geom_line(aes(x = Date, y = currloans - iba - iba_sec), color = "green2") 
# geom_line(aes(x = Date, y = timeliab + demliab), color = "grey")+
# geom_line(aes(x = Date, y = secs), color = "red")


# JOINT DISTRIBUTION ####

q <- q_b_agg_idno
# q <- q_b_agg_prov

q <- q %>% filter(year <= 1933)
q <- left_join(q,turnover, by = c("year","IDNO"))
q <- q %>% filter(!(turnover == 0))

# q <- q %>% group_by(IDNO,province) %>% mutate(var_nd = nd - Lag(nd),var_nb = nb - Lag(nb), var_ndC =  ndC - Lag(ndC), var_ncc_nb =  ncc_nb - Lag(ncc_nb), var_ncc_nd =  ncc_nd - Lag(ncc_nd), var_nccC =  nccC - Lag(nccC))
# q <- q %>% group_by(IDNO,province) %>% mutate(var_year = year - Lag(year))
# q <- q %>% mutate(var_nb_py = var_nb/var_year,var_nd_py = var_nd/var_year,var_ndC_py = var_ndC/var_year,var_ncc_nb_py = var_ncc_nb/var_year,var_ncc_nd_py = var_ncc_nd/var_year,var_nccC_py = var_nccC/var_year)

# q <- q %>% group_by(IDNO,province) %>%  mutate(var_nb_pyp = var_nb_py/Lag(nb),var_nd_pyp = var_nd_py/Lag(nd),var_ndC_pyp = var_ndC_py/Lag(ndC),var_ncc_nb_pyp = var_ncc_nb_py/Lag(ncc_nb),var_ncc_nd_pyp = var_ncc_nd_py/Lag(ncc_nd))


q <- q %>% group_by(IDNO) %>% mutate(var_nd = nd - Lag(nd),var_nb = nb - Lag(nb), var_ndC =  ndC - Lag(ndC), var_ncc_nb =  ncc_nb - Lag(ncc_nb), var_ncc_nd =  ncc_nd - Lag(ncc_nd), var_nccC =  nccC - Lag(nccC))
q <- q %>% group_by(IDNO) %>% mutate(var_year = year - Lag(year))
q <- q %>% mutate(var_nb_py = var_nb/var_year,var_nd_py = var_nd/var_year,var_ndC_py = var_ndC/var_year,var_ncc_nb_py = var_ncc_nb/var_year,var_ncc_nd_py = var_ncc_nd/var_year,var_nccC_py = var_nccC/var_year)

# q <- q %>% group_by(IDNO) %>%  mutate(var_nb_pyp = var_nb_py/Lag(nb),var_nd_pyp = var_nd_py/Lag(nd),var_ndC_pyp = var_ndC_py/Lag(ndC),var_ncc_nb_pyp = var_ncc_nb_py/Lag(ncc_nb),var_ncc_nd_pyp = var_ncc_nd_py/Lag(ncc_nd))


q2 <- q %>% filter(!(year == 1920))
q2 <- melt(q2, measure.vars = c("var_nd","var_ndC","var_ncc_nd","var_nccC"))
q2 <- q2 %>% filter(!(is.na(value)))
q3 <- q2 %>% filter(!(value == 0))
q4 <- q2 %>% filter((value == 0))

q5 <- NULL
for (i in 1:length(q3$IDNO)){

    q3_aux <- q3[i,]
    
    q3_aux <-  q3_aux[rep(1, each = abs(q3_aux$value[1])), ]
    q5 <- rbind(q5,q3_aux)
    

}

q6 <- rbind(q4,q5)
q6$variable <- as.character(q6$variable)
q6$variable[which(str_detect(as.character(q6$value),"-"))] <- paste0("-",q6$variable[which(str_detect(as.character(q6$value),"-"))])  
q6 <- q6 %>% select(bank,IDNO,year,nd,ndC,ncc_nd,nccC,variable) %>% filter(!(duplicated(q6)))

  
q7 <- mlogit.data(q6,shape = "wide",choice = "variable")



q_bs_inc_year %>% filter(year %in% c(1857,1868,1880,1892,1910,1920)) %>%
  ggplot() +
  geom_histogram(aes(pop/pop_year,group = as.factor(year)), color = "steelblue", size = 1, alpha = 0) +
  labs(title = "",
       x = "Population coverage", y = "Number of banks") +
  guides(color = guide_legend(nrow = 2, byrow = T, title = "Year", override.aes = list(size=5))) +
  # scale_y_continuous()
scale_y_continuous(breaks = round(seq(0, 10, by = 2),0)) +
  # scale_color_manual(values = col[c(15)]) +
  # scale_color_brewer("PuBu")
  theme(
    legend.title = element_text(size = 15),
    legend.text=element_text(size= 15),
    axis.title = element_text(size= 15),
    # legend.key.size = unit(20,"cm")
    # title = element_markdown()
    # plot.title = element_text(size=15, face="bold", hjust = 0.5,
    #                           margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    legend.position = "bottom",
    # axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5)
    # axis.title.y = element_text(size=15, vjust=1)
  ) + 
  facet_wrap(year ~.)



q_b_agg_idno %>%
  ggplot() +
  geom_point(aes(x = pop, y = ncc_p,group = as.factor(IDNO)),color = "steelblue", size = 1, alpha = 0) +
  labs(title = "",
       x = "Population coverage", y = "Number of banks") +

  q_b_agg_idno %>%
  ggplot() +
  geom_line(aes(x = pop - ncc_pop, y = ncc_pop,group = as.factor(IDNO)),color = "steelblue", size = 0.75, alpha = 0.6) +
scale_y_continuous(breaks = round(seq(0, 1500000, by = 250000),0)) +
  scale_x_continuous(breaks = round(seq(0, 5000000, by = 1000000),0)) +
  labs(title = "",
       x = "Population coverage", y = "Clearing House population coverage") +
  # scale_color_manual(values = col[c(15)]) +
  # scale_color_brewer("PuBu")
  theme(
    legend.title = element_text(size = 15),
    legend.text=element_text(size= 15),
    axis.title = element_text(size= 15),
    # legend.key.size = unit(20,"cm")
    # title = element_markdown()
    # plot.title = element_text(size=15, face="bold", hjust = 0.5,
    #                           margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    legend.position = "bottom",
    # axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5)
    # axis.title.y = element_text(size=15, vjust=1)
  ) 



# q_bs_inc_year %>% filter(year >= c(1870), year <= 1910, IDNO %in% c(7,14,16,21,33,36,37,40,43,58,60,23)) %>%
q_bs_inc_year %>% filter(year >= c(1870), year <= 1910) %>%
  ggplot(aes(x = year, y = ncc_nd,group = IDNO, color = as.factor(IDNO))) +
  geom_line(size = 1, alpha = 1) 





q_bs_inc_year <- q_bs_inc_year %>% group_by(IDNO) %>% mutate(var_nd = nd - lag(nd, order_by = Date),var_nb = nb - lag(nb, order_by = Date),
                                                             var_ndC =  ndC - lag(ndC, order_by = Date), var_nbp = nb_p - lag(nb_p, order_by = Date))
q_bs_inc_year <- q_bs_inc_year %>% group_by(IDNO) %>% mutate(var_year = year - lag(year, order_by = Date))
q_bs_inc_year <- q_bs_inc_year %>% mutate(var_nb_py = var_nb/var_year,var_nbp_py = var_nbp/var_year,var_nd_py = var_nd/var_year,var_ndC_py = var_ndC/var_year)
q_bs_inc_year <- q_bs_inc_year %>% group_by(IDNO) %>%  mutate(var_nb_pyp = var_nb_py/lag(nb, order_by = Date),
                                                              var_nd_pyp = var_nd_py/lag(nd, order_by = Date),
                                                              var_ndC_pyp = var_ndC_py/lag(ndC, order_by = Date),
                                                              var_nbp_pyp = var_nbp_py/lag(nb_p, order_by = Date))

q_bs_inc_year %>% 
  filter(year <= 1913) %>%
  ggplot(aes(x =  nb_p  , y =  demliab + timeliab ,group = year, color = as.factor(year))) +
  geom_point(size = 2, alpha = 1)  +
  stat_smooth(method = "loess", formula = y ~ x^2 , size = 1, se = F) +
  geom_label(aes(label = IDNO) ) +
  # xlim(0,100) + ylim(0,7e7)
  
  # q_bs_inc_year %>% filter(year %in% c(1901:1910)) %>%
  q_bs_inc_year %>% filter(year %in% c(1882,1889,1894,1900)) %>%
  # q_bs_inc_year %>% filter(year %in% c(1910), !(IDNO %in% c(1,26))) %>%
  ggplot(aes(x = pop, y = a ,group = IDNO, color = as.factor(IDNO))) +
  geom_point(size = 2, alpha = 1)  +
  # stat_smooth(method = "lm", formula = y ~ x  - 1, size = 1, se = F) +
  geom_label(aes(label = IDNO), size = 4 ) + 
  xlim(0,40) + ylim(0,2.5e6)


q_bs_inc_year %>% filter(year %in% c(1882,1889,1894,1900,1905,1910)) %>%
  ggplot() +
  geom_density(aes(  currloans/nb_p  ,group = year, color = as.factor(year)), size = 1, alpha = 0)

q_bs_inc_year %>% filter(year %in% c(1882,1889,1894,1900,1905,1910), (IDNO %in% c(10,33,26,30,13))) %>%
  ggplot(aes(x = year, y = iba/Totalass   ,group = IDNO, color = as.factor(IDNO))) +
  # ggplot(aes(x = year, y = (l - (ibl_sec + ibl + ibl_row + ol + rowl))/nb  ,group = IDNO, color = as.factor(IDNO))) +
  geom_line(size = 1, alpha = 1) +
  geom_label(aes(label = IDNO) )


# how var_l_pyp changes when var_nd_py changes  
``
p <- function(.q,x){
  
  x <- enquo(x)
  
  .q <- .q %>% group_by(IDNO) %>% mutate(var_x_py = ({{x}} - lag({{x}}, order_by = Date))/(year - lag(year, order_by = Date)))
  
  .q <- .q %>% group_by(IDNO) %>% mutate(var_x_pyp = var_x_py/lag({{x}}, order_by = Date))
  
  
  .q %>% filter(year %in% c(1882,1889,1894,1900,1905,1910)) %>%
    ggplot(aes(x=var_nbp_py,y= var_x_py, group = year, color = as.factor(year))) +
    geom_point(size = 2, alpha = 0.5) +
    stat_smooth(method = "lm", formula = y ~ x, size = 1, se = F)
  
}



z <- q_bs_inc_year %>% filter(year <= c(1910)) 
z <- within(z, IDNO <- relevel(as.factor(IDNO), ref = "40"))

a <- lm(formula = var_eq_py ~ var_nd_py + var_ndC_py, data = q_bs_inc_year)

a <- lm(formula = a ~ nb + factor(IDNO)*nb - 1 + pop, data = z)
coef_test(a, vcov = "CR2", cluster = z$IDNO, test = "Satterthwaite")

# BRANCH TURNOVER  ####


q %>% filter(year >= 1901, year <= 1910,!(var_nb_pyp == 0)) %>%
  # q  %>% filter(year %in% c(1876,1882,1889,1894,1900,1902,1905,1910)) %>%
  # q  %>% 
  ggplot(aes(x = var_ndC_pyp, y = var_nd_pyp,group = year, color = as.factor(year))) +
  geom_point(size = 2, alpha = 1)  +
  stat_smooth(method = "lm", formula = y ~ x, size = 1, se = F) +
  ylim(-0.25,0.6) + 
  xlim(-0.5,2)

# q %>% filter(year >= c(1870), year <= 1910, IDNO %in% c(1:10)) %>%
q  %>% filter(year %in% c(1882,1889,1894,1900,1905,1910), !(IDNO %in% c(3,32,55)), !(IDNO %in% c(1,5,8,9,10,26,29,30,33,49))) %>%
  ggplot(aes(x = year, y = var_nd_pyp,group = IDNO, color = as.factor(IDNO))) +
  geom_line(size = 1, alpha = 1)  +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = F)

# q %>% filter(year %in% c(1870,1882,1889,1894,1900:1910), IDNO == 1) %>%
# q %>% filter(year %in% c(1901:1910), IDNO == 1, province == "Manitoba") %>%
q %>% filter(year %in% c(1901:1910), !(IDNO %in% w)) %>%
  # q_bs_inc_year %>% filter(year %in% c(1870,1882)) %>%
  ggplot() +
  geom_histogram(aes( var_nd ,group = year, color = as.factor(year)), size = 1, alpha = 0)



p <- function(.q,xx,yy,gg,l,a,b,c,g,y1,y2){
  
  xx <- enquo(xx)
  yy <- enquo(yy)
  gg <- enquo(gg)
  
  if (g == 1){
    
    pp <- .q %>% filter(year >= c(y1), year <= y2) %>%
      ggplot2::ggplot(aes(x = {{xx}}, y = {{yy}}, group = {{gg}} , color = as.factor({{gg}}))) +
      geom_point(size = 1, alpha = 1)  
    
  } else {
    
    pp <- .q %>% filter(year >= c(y1), year <= y2) %>%
      ggplot2::ggplot(aes(x = {{xx}}, y = {{yy}})) +
      geom_point(size = 1, alpha = 1) 
    
  }
  
  
  
  if (l == 1){
    
    pp <- pp +  ylim(a,b)
    
  }
  
  if (c == 1){
    pp <- pp +
      stat_smooth(method = "lm", formula = y ~ x, size = 1, se = F) 
  }
  
  print(pp)
  
}





z <- q %>% filter(year <= 1910, year >= 1870) %>%  group_by(year) %>% summarise(m = mean(var_nd_py,na.rm = T), med = median(var_nd_py,na.rm = T))  
z <- q %>% group_by(year) %>% summarise(m = mean(var_nb_py, na.rm = T))
z <- q %>% group_by(year) %>% summarise(m = mode(var_nb_py))



# POPULATION AND BRANCHES ####

mz <- manuf_cd
mz$year[which(mz$year == "1891")] <- 1892
mz$year[which(mz$year == "1911")] <- 1910

a <- ref_branches %>%  group_by(year,UID_CD_11) %>%  tally()
z <- left_join(pop,a, by = c("UID_CD_11","year"))
z <- left_join(z,mz[,c("UID_CD_11","year","value")], by = c("UID_CD_11","year"))
z <- z %>% mutate(br_pc = (population)/n)

mz_csd <- manuf_csd
mz_csd$year[which(mz_csd$year == "1891")] <- 1892
mz_csd$year[which(mz_csd$year == "1911")] <- 1910

a <- ref_branches %>%  group_by(year,UID_CSD_11) %>%  tally(name = "nb")
a2 <- ref_branches %>%  group_by(year,UID_CSD_11) %>%  distinct(bank, keep.all = T) %>% tally(name = "nbanks")
z_csd <- left_join(pop_csd,a, by = c("UID_CSD_11","year")) %>%
  left_join(.,a2, by = c("UID_CSD_11","year"))
# z_csd <- left_join(z_csd,mz_csd[,c("UID_CSD_11","year","value")], by = c("UID_CSD_11","year"))  
# z_csd <- z_csd %>% mutate(br_pc = (population)/n)


# branches per capita


z2 <- z %>% filter(year == 1924)
z2 <- z %>% filter(year == 1910)
z2 <- z %>% filter(!(is.na(br_pc)), year <= 1913)

z2 %>% 
  # filter(population > 0, year == 1910) %>%
  filter(population > 0, !(province %in% c("Yukon","North Western Territories"))) %>%
  ggplot(aes(x = population, y = n ,group = year, color = as.factor(year))) +
  # ggplot(aes(x = population, y = n)) +
  geom_point(size = 1, alpha = 0.1)  +
  # ylim(0,50) +
  # xlim(0,2e5) +
stat_smooth(method = "lm", formula = y ~ x , size = 1, se = F)





# lead-lag




a <- ref_branches %>%  group_by(year,UID_CD_11) %>%  tally(name = "nb")
a2 <- ref_branches %>%  group_by(year,UID_CD_11) %>%  distinct(bank, keep.all = T) %>% tally(name = "nbanks")
z_cd <- left_join(pop,a, by = c("UID_CD_11","year")) %>%
  left_join(.,a2, by = c("UID_CD_11","year"))


z_cd$train <- NA
trains_buff <- trains_place
id <- st_intersects(trains_buff,df_cd, sparse = T)
for (i in 1:length(trains_buff$OBJECTID)){
  # for (i in 1:length(trains_buff$OBJECTID)){
  if (length(id[[i]]) > 0){
    
    aux <- as.data.frame(df_cd) %>% filter(UID_CD_11 %in% ref_cd$UID_CD_11[id[[i]]]) %>% select(UID_CD_11)
    aux <- aux$UID_CD_11 
    
    z_cd$train[which(z_cd$UID_CD_11 %in% aux & z_cd$year >= trains_buff$CNSTRCTD[i] & z_cd$year <= trains_buff$ABNDND[i])] <- 1  
  }
}

z_cd$train[which(is.na(z_cd$train))] <- 0
z_cd$nb[which(is.na(z_cd$nb))] <- 0
z_cd$nbanks[which(is.na(z_cd$nbanks))] <- 0



y1 <- z_cd %>% group_by(UID_CD_11) %>% filter(population > 100) %>% summarise(pop = extract(year,1))
y2 <- z_cd %>% group_by(UID_CD_11) %>% filter(population2 > 100) %>% summarise(pop2 = extract(year,1))
y <- left_join(y1,y2, by = "UID_CD_11")

z_cd <- left_join(z_cd,y, by = "UID_CD_11")
z_cd <- z_cd %>% filter(method == "linear", year %in% unique(a$year))

z_cd$period <- NA

for (i in unique(y1$UID_CD_11)){
  # for (i in unique(y2$UID_CD_11)){
 yy <- y1$pop[which(y1$UID_CD_11 == i)]
 # yy <- y2$pop2[which(y2$UID_CD_11 == i)]
 yy <- unique(z_cd$year)[which.min(abs(yy - unique(z_cd$year)))]  
 
   z_cd$period[which(z_cd$UID_CD_11 == i & z_cd$year == yy)] <- 0
   if (yy < 1920)
   z_cd$period[which(z_cd$UID_CD_11 == i & z_cd$year > yy)] <- seq(1,length(z_cd$period[which(z_cd$UID_CD_11 == i & z_cd$year > yy)]), by = 1) 
   if (yy > 1857){
   z_cd$period[which(z_cd$UID_CD_11 == i & z_cd$year < yy)] <- seq(-length(z_cd$period[which(z_cd$UID_CD_11 == i & z_cd$year < yy)]),-1, by = 1)
 }
}
z_cd <- z_cd %>% mutate(nb_d = nb > 0, nbanks_d = nbanks > 1)

z_cd <- z_cd %>% filter(!(district %in% c("Nanaimo")))


z_cd2 <- z_cd %>% filter(!(pop == 1851)) %>% group_by(period) %>% summarise(mnb = mean(nb,na.rm = T),mnbanks = mean(nbanks, na.rm = T), mtrain = mean(train, na.rm = T), mdnb = mean(nb_d, na.rm = T), mnbanks_d = mean(nbanks_d, na.rm = T), mpop = mean(population, na.rm = T)/10000)
z_cd2 <- melt(z_cd2, id.vars = "period")

z_cd2 %>% filter(variable %in% c("mtrain","mdnb","mnbanks_d")) %>%
  # z_cd2 %>% filter(variable %in% c("mnb","mnbanks","mpop")) %>%
  ggplot() +
  geom_line(aes(x = period,y = value, group = as.factor(variable), colour = as.factor(variable)), size = 1, alpha = 1) +
  labs(title = "Lead-lag relative to population settlement",
       x = "Lead-lag periods", y = "Fraction") +
    # labs(title = "Lead-lag relative to population settlement",
    #      x = "Lead-lag periods", y = "Number") +
  scale_colour_manual(name = "", labels = c("Train","Bank","Competition"), values = c("red3","orange","midnightblue")) +
  # scale_colour_manual(name = "", labels = c("Branches","Banks","Population (p/10k)"), values = c("red3","orange","midnightblue")) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  scale_x_continuous(breaks = round(seq(-20, 30, by = 5),0)) +
  # scale_y_continuous(breaks = round(seq(0, 15, by = 2.5),1)) +
  scale_y_continuous(breaks = round(seq(0, 1, by = 0.1),1)) +
  geom_vline(xintercept=0, lty = 2) +
   theme(
    legend.text=element_text(size= 15),
    axis.title = element_text(size= 12),
    plot.title = element_text(size=15, face="bold", hjust = 0.5,
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=15, vjust= 0.5),
    axis.text.y = element_text(size=15, vjust=0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=0.5),
    legend.position = "bottom"
  )


# joint dist train-branch conditional on positive population

zz <- z_cd %>% filter(population > 100, !(pop == 1851)) %>% group_by(train,nb_d) %>% tally()
# z <- z_cd %>% filter(population > 100) %>% group_by(train,nb_d) %>% tally()
zz$p <- zz$n/sum(zz$n)



# lead-lag csd




a <- ref_branches %>%  group_by(year,UID_CSD_21) %>%  tally(name = "nb")
a2 <- ref_branches %>%  group_by(year,UID_CSD_21) %>%  distinct(bank, keep.all = T) %>% tally(name = "nbanks")
z_csd <- left_join(pop_csd,a, by = c("UID_CSD_21","year")) %>%
  left_join(.,a2, by = c("UID_CSD_21","year"))


z_csd$train <- NA
trains_buff <- trains
id <- st_intersects(trains_buff,df_csd, sparse = T)
for (i in 1:length(trains_buff$OBJECTID)){
  # for (i in 1:length(trains_buff$OBJECTID)){
  if (length(id[[i]]) > 0){
    
    aux <- as.data.frame(df_csd) %>% filter(UID_CSD_21 %in% ref_csd$UID_CSD_21[id[[i]]]) %>% select(UID_CSD_21)
    aux <- aux$UID_CSD_21 
    
    z_csd$train[which(z_csd$UID_CSD_21 %in% aux & z_csd$year >= trains_buff$CNSTRCTD[i] & z_csd$year <= trains_buff$ABNDND[i])] <- 1  
  }
}

z_csd$train[which(is.na(z_csd$train))] <- 0
z_csd$nb[which(is.na(z_csd$nb))] <- 0
z_csd$nbanks[which(is.na(z_csd$nbanks))] <- 0



y1 <- z_csd %>% group_by(UID_CSD_21) %>% filter(population > 100) %>% summarise(pop = extract(year,1))
y2 <- z_csd %>% group_by(UID_CSD_21) %>% filter(population2 > 100) %>% summarise(pop2 = extract(year,1))
y <- left_join(y1,y2, by = "UID_CSD_21")

z_csd <- left_join(z_csd,y, by = "UID_CSD_21")
z_csd <- z_csd %>% filter(method == "linear", year %in% unique(a$year))

z_csd$period <- NA

for (i in unique(y1$UID_CSD_21)){
  # for (i in unique(y2$UID_CSD_21)){
  yy <- y1$pop[which(y1$UID_CSD_21 == i)]
  # yy <- y2$pop2[which(y2$UID_CSD_21 == i)]
  yy <- unique(z_csd$year)[which.min(abs(yy - unique(z_csd$year)))]  
  
  z_csd$period[which(z_csd$UID_CSD_21 == i & z_csd$year == yy)] <- 0
  if (yy < 1920)
    z_csd$period[which(z_csd$UID_CSD_21 == i & z_csd$year > yy)] <- seq(1,length(z_csd$period[which(z_csd$UID_CSD_21 == i & z_csd$year > yy)]), by = 1) 
  if (yy > 1857){
    z_csd$period[which(z_csd$UID_CSD_21 == i & z_csd$year < yy)] <- seq(-length(z_csd$period[which(z_csd$UID_CSD_21 == i & z_csd$year < yy)]),-1, by = 1)
  }
}
z_csd <- z_csd %>% mutate(nb_d = nb > 0, nbanks_d = nbanks > 1)

# z_csd <- z_csd %>% filter(!(district %in% c("Nanaimo")))

# z_csd2 <- z_csd %>% filter(!(pop == 1851)) %>% group_by(period) %>% summarise(mnb = mean(nb,na.rm = T),mnbanks = mean(nbanks, na.rm = T), mtrain = mean(train, na.rm = T), mdnb = mean(nb_d, na.rm = T), mnbanks_d = mean(nbanks_d, na.rm = T), mpop = mean(population, na.rm = T)/10000)
# z_csd2 <- melt(z_csd2, id.vars = "period")
# 
# z_csdd <- z_csdd %>% filter(!(pop == 1851)) %>% group_by(period) %>% mutate(zz = population/sum(population,na.rm = T))
# z_csd2 <- z_csdd %>% filter(nb_d == 1) %>% group_by(period) %>% summarise(mnb = sum(zz,na.rm = T))
# z_csd2 <- melt(z_csd2, id.vars = "period")



# p <- seq(0,1, by = 0.1)
p <- c(0,0.9,0.99,1)
z_csdd <- z_csd %>% filter(!(pop == 1851)) %>% group_by(UID_CSD_21) %>% mutate(z = nb_d - lag(nb_d))
# z_csdd <- z_cd %>% filter(!(pop == 1851)) %>% group_by(UID_CD_21) %>% mutate(z = nb_d - lag(nb_d))
d <- z_csdd %>% group_by(year) %>% summarise(decile = as.vector(quantile(population, probs = p)))
# d$n <- rep(1:10,length(unique(d$year)))

z_csdd$breaks <- NA
for (j in unique(d$year)){
  
  z_csdd$breaks[which(z_csdd$year == j)] <- cut(z_csdd$population[which(z_csdd$year == j)] , 
                                                breaks = unique(d$decile[which(d$year == j)]), 
                                                include.lowest = T)
                                                # labels = seq(10 - (n_distinct(d$decile[which(d$year == j)]) - 2),10, by = 1))
  # labels = seq(10,10 - (n_distinct(d$decile[which(d$year == j)]) - 2), by = -1))
  
  z_csdd$breaks[which(z_csdd$year == j)] <- z_csdd$breaks[which(z_csdd$year == j)] + (length(p) - max(z_csdd$breaks[which(z_csdd$year == j)],na.rm = T))
}



# z_csddd <- z_csdd %>% mutate_cond(breaks < 9, breaks = 8)
# z_csddd <- z_csdd %>% mutate_cond(breaks < 5, breaks = 4)
z_csddd <- z_csdd

# z_csd2 <- melt(z_csddd, measure.vars = c("nb_d","train","nbanks_d"))


# a <-  z_csdd[order(z_csdd$period),] %>% group_by(UID_CSD_11) %>% filter(nb_d == 1) %>% summarise(zz = extract(period,1))
# a <- left_join(z_csdd,a, by = "UID_CSD_11")
# a <- a %>% filter(zz == period)


z_csd2 <- z_csddd %>% group_by(period, breaks) %>% summarise(mnb = mean(nb,na.rm = T),
                                                            mnbanks = mean(nbanks, na.rm = T),
                                                            mtrain = mean(train, na.rm = T),
                                                            mdnb = mean(nb_d, na.rm = T),
                                                            mnbanks_d = mean(nbanks_d, na.rm = T),
                                                            mpop = mean(population, na.rm = T)/10000)
z_csd2 <- melt(z_csd2, id.vars = c("period","breaks"))

# z_csd2 %>% filter(variable %in% c("mtrain","mdnb","mnbanks_d")) %>%
  z_csd2 %>% filter(variable %in% c("mdnb")) %>%
  # z_csd2 %>% filter(variable %in% c("mnb","mnbanks","mpop")) %>%
  ggplot() +
  geom_line(aes(x = period,y = value, group = as.factor(breaks), colour = as.factor(breaks)), size = 1, alpha = 1) +
  # geom_line(aes(x = period,y = value, group = as.factor(variable), colour = as.factor(variable)), size = 1, alpha = 1) +
  # geom_line(aes(x = year,y = value, group = as.factor(variable), colour = as.factor(variable)), size = 1, alpha = 1) +
  labs(title = "Lead-lag relative to population settlement",
       x = "Lead-lag periods", y = "Fraction") +
  # labs(title = "Lead-lag relative to population settlement",
  #      x = "Lead-lag periods", y = "Number") +
  scale_colour_manual(name = "", labels = c("Bank: Bottom 90%","Bank: Top 10% - 1%","Bank: Top 1%"), values = c("red3","orange","midnightblue")) +
  # scale_colour_manual(name = "", labels = c("Branches","Banks","Population (p/10k)"), values = c("red3","orange","midnightblue")) +
  # scale_colour_manual(name = "", labels = c("Train","1st Bank","2nd Bank"), values = c("red3","orange","midnightblue")) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  scale_x_continuous(breaks = round(seq(-20, 30, by = 5),0)) +
  # scale_y_continuous(breaks = round(seq(0, 15, by = 2.5),1)) +
  scale_y_continuous(breaks = round(seq(0, 1, by = 0.1),1)) +
  geom_vline(xintercept=0, lty = 2) +
  theme(
    legend.text=element_text(size= 15),
    axis.title = element_text(size= 12),
    plot.title = element_text(size=15, face="bold", hjust = 0.5,
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=15, vjust= 0.5),
    axis.text.y = element_text(size=15, vjust=0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=0.5),
    legend.position = "bottom"
  )

  
  
  
  z_csd2 <- z_csddd %>% group_by(period) %>% summarise(mnb = mean(nb,na.rm = T),
                                                       mnbanks = mean(nbanks, na.rm = T),
                                                       mtrain = mean(train, na.rm = T),
                                                       mdnb = mean(nb_d, na.rm = T),
                                                       mnbanks_d = mean(nbanks_d, na.rm = T),
                                                       mpop = mean(population, na.rm = T)/10000)
  z_csd2 <- melt(z_csd2, id.vars = c("period"))
  
  
  z_csd2 %>% filter(variable %in% c("mtrain","mdnb","mnbanks_d")) %>%
  # z_csd2 %>% filter(variable %in% c("mdnb")) %>%
    # z_csd2 %>% filter(variable %in% c("mnb","mnbanks","mpop")) %>%
    ggplot() +
    # geom_line(aes(x = period,y = value, group = as.factor(breaks), colour = as.factor(breaks)), size = 1, alpha = 1) +
    geom_line(aes(x = period,y = value, group = as.factor(variable), colour = as.factor(variable)), size = 1, alpha = 1) +
    # geom_line(aes(x = year,y = value, group = as.factor(variable), colour = as.factor(variable)), size = 1, alpha = 1) +
    labs(title = "Lead-lag relative to population settlement",
         x = "Lead-lag periods", y = "Fraction") +
    # labs(title = "Lead-lag relative to population settlement",
    #      x = "Lead-lag periods", y = "Number") +
    # scale_colour_manual(name = "", labels = c("Bank: Bottom 90%","Bank: Top 10%","Competition"), values = c("red3","orange","midnightblue")) +
    # scale_colour_manual(name = "", labels = c("Branches","Banks","Population (p/10k)"), values = c("red3","orange","midnightblue")) +
    scale_colour_manual(name = "", labels = c("Train","1st Bank","2nd Bank"), values = c("red3","orange","midnightblue")) +
    guides(colour = guide_legend(override.aes = list(size = 5))) +
    scale_x_continuous(breaks = round(seq(-20, 30, by = 5),0)) +
    # scale_y_continuous(breaks = round(seq(0, 15, by = 2.5),1)) +
    scale_y_continuous(breaks = round(seq(0, 1, by = 0.1),1)) +
    geom_vline(xintercept=0, lty = 2) +
    theme(
      legend.text=element_text(size= 15),
      axis.title = element_text(size= 12),
      plot.title = element_text(size=15, face="bold", hjust = 0.5,
                                margin = margin(10, 10, 10, 10)),
      axis.text.x = element_text(size=15, vjust= 0.5),
      axis.text.y = element_text(size=15, vjust=0.5),
      axis.title.x = element_text(size=15, vjust=0.5),
      axis.title.y = element_text(size=15, vjust=0.5),
      legend.position = "bottom"
    )
  
  
  
  
  
  

z_csd2 %>% 
  ggplot() +
  geom_point(aes(x = period,y = value, group = as.factor(variable), colour = as.factor(variable)), size = 1, alpha = 1) +
  labs(title = "Lead-lag relative to population settlement",
       x = "Lead-lag periods", y = "Fraction") +
  # labs(title = "Lead-lag relative to population settlement",
  #      x = "Lead-lag periods", y = "Number") +
  # scale_colour_manual(name = "", labels = c("Bank: Bottom 90%","Bank: Top 10%","Competition"), values = c("red3","orange","midnightblue")) +
  # scale_colour_manual(name = "", labels = c("Branches","Banks","Population (p/10k)"), values = c("red3","orange","midnightblue")) +
  scale_colour_manual(name = "", labels = c("Train","1st Bank","2nd Bank"), values = c("red3","orange","midnightblue")) +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  scale_x_continuous(breaks = round(seq(-20, 30, by = 5),0)) +
  # scale_y_continuous(breaks = round(seq(0, 15, by = 2.5),1)) +
  scale_y_continuous(breaks = round(seq(0.01, 0.35, by = 0.1),1)) +
  geom_vline(xintercept=0, lty = 2) +
  theme(
    legend.text=element_text(size= 15),
    axis.title = element_text(size= 12),
    plot.title = element_text(size=15, face="bold", hjust = 0.5,
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=15, vjust= 0.5),
    axis.text.y = element_text(size=15, vjust=0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=0.5),
    legend.position = "bottom"
  ) + geom_smooth(aes(x = period, y = value, group = as.factor(variable), colour = as.factor(variable)), method = 'loess', se = T) 





# joint dist train-branch conditional on positive population

z <- z_csd %>% filter(population > 10, !(pop == 1851)) %>% group_by(train,nb_d) %>% tally()
# z <- z_csd %>% filter(population > 100) %>% group_by(train,nb_d) %>% tally()
z$p <- z$n/sum(z$n)






# pop unitl 1st branch




z3 <- z %>% filter((is.na(br_pc)),population >= 0, year %in% unique(a$year))
# z4 <- z3 %>% filter(year == 1924)
z3 <- z3 %>% group_by(UID_CD_11) %>% filter(year == max(year))

a <- pop %>% group_by(year) %>% summarise(m = mean(population,na.rm = T))
a <- a %>% mutate(y_axis = 0)

z3 %>% filter(!(province %in% c("Yukon","North Western Territories"))) %>%
  ggplot() +
  # geom_histogram(aes(population,group = as.factor(province), color = as.factor(province)), size = 1, alpha = 1) +
  geom_histogram(aes(population), color = "black", fill = "steelblue", size = 1, alpha = 1, binwidth = 1000) +
  geom_point(data = a, aes(x = m, y = y_axis), size = 5, color = "yellow2", pch = 3) +
  labs(title = "District population until first branch",
       x = "Population", y = "Number of districts") +
  scale_x_continuous(breaks = round(seq(0, 60000, by = 10000),0)) +
  scale_y_continuous(breaks = round(seq(0, 18, by = 2),0)) +
  theme(
    # legend.title = element_text(size = 15),
    # legend.text=element_text(size= 15),
    # legend.key.size = unit(20,"cm")
    axis.title = element_text(size= 15),
    # legend.key.size = unit(20,"cm")
    # title = element_markdown()
    plot.title = element_text(size=20, face="bold", hjust = 0.5,
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    legend.position = "bottom",
    # axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5)
  )


z4 <- z %>% filter(population > 0, year %in% unique(a$year))
z4 <- z4 %>% group_by(UID_CD_11) %>% filter(year == min(year))
z5 <- left_join(z3,z4, by ="UID_CD_11")
z5 <- z5 %>% mutate(diff = year.x - year.y)


z5 %>% filter(!(province.x %in% c("Yukon","North Western Territories"))) %>%
  ggplot() +
  # geom_histogram(aes(population,group = as.factor(province), color = as.factor(province)), size = 1, alpha = 1) +
  geom_histogram(aes(diff), color = "black", fill = "steelblue", size = 1, alpha = 1, binwidth = 1) +
  # geom_point(data = a, aes(x = m, y = y_axis), size = 5, color = "yellow2", pch = 3) +
  labs(title = "Years until first branch",
       x = "Years", y = "Number of districts") +
  scale_x_continuous(breaks = round(seq(0, 70, by = 5),0)) +
  scale_y_continuous(breaks = round(seq(0, 25, by = 2),0)) +
  theme(
    # legend.title = element_text(size = 15),
    # legend.text=element_text(size= 15),
    # legend.key.size = unit(20,"cm")
    axis.title = element_text(size= 15),
    # legend.key.size = unit(20,"cm")
    # title = element_markdown()
    plot.title = element_text(size=20, face="bold", hjust = 0.5,
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    legend.position = "bottom",
    # axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5)
  )

#




# coverage

zz <- data.frame("province" = sort(rep(unique(z$province),NROW(unique(a$year)))), "year" = rep(unique(a$year),NROW(unique(z$province)))) 




z2 <- z %>% 
  filter(!(is.na(br_pc)), year %in% unique(a$year)) %>%
  group_by(year,province) %>% 
  summarise("cover" = sum(population,na.rm = T))

z3 <- z %>% 
  filter((is.na(br_pc)), year %in% unique(a$year)) %>% 
  group_by(year,province) %>% 
  summarise("not_cover" = sum(population,na.rm = T))

zz <- left_join(zz,z2, by = c("year","province")) %>%
  left_join(.,z3, by = c("year","province"))
zz$cover[which(is.na(zz$cover))] <- 0
zz$not_cover[which(is.na(zz$not_cover))] <- 0
zz <- zz %>% mutate(cover_p = cover/(cover + not_cover))
zz <- zz %>% filter(!(is.nan(cover_p)))


zz %>% filter(!(province %in% c("Yukon","North Western Territories"))) %>%
  ggplot(aes(x = year, y = cover_p,group = as.factor(province), color = as.factor(province))) +
  geom_line(size = 1.5, alpha = 1) +
labs(title = "",
     x = "Year", y = "Population coverage") +
  guides(color = guide_legend(nrow = 3, byrow = T, title = "Province", override.aes = list(size=5))) +
  theme(
    legend.title = element_text(size = 15),
    legend.text=element_text(size= 15),
    axis.title = element_text(size= 15),
    # legend.key.size = unit(20,"cm")
    # title = element_markdown()
    # plot.title = element_text(size=15, face="bold", hjust = 0.5,
    #                           margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    legend.position = "bottom",
    # axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5)
    # axis.title.y = element_text(size=15, vjust=1)
  )



zz <- data.frame("province" = sort(rep(unique(z$province),NROW(unique(mz$year)))), "year" = rep(unique(mz$year),NROW(unique(z$province)))) 


z2 <- z %>% 
  filter(!(is.na(n)), year %in% unique(mz$year)) %>%
  group_by(year,province) %>% 
  summarise("cover" = sum(value,na.rm = T))

z3 <- z %>% 
  filter((is.na(n)), year %in% unique(mz$year)) %>% 
  group_by(year,province) %>% 
  summarise("not_cover" = sum(value,na.rm = T))

zz <- left_join(zz,z2, by = c("year","province")) %>%
  left_join(.,z3, by = c("year","province"))
zz$cover[which(is.na(zz$cover))] <- 0
zz$not_cover[which(is.na(zz$not_cover))] <- 0
zz <- zz %>% mutate(cover_p = cover/(cover + not_cover))
zz <- zz %>% filter(!(is.nan(cover_p)))

zz$group <- 0
zz <- zz %>% mutate_cond(province %in% c("Quebec","Ontario","New Brunswick","Nova Scotia"), group = 1)
zz <- zz %>% mutate_cond(province %in% c("Saskatchewan"), group = 2)
zz <- zz %>% mutate_cond(province %in% c("Alberta"), group = 3)
zz <- zz %>% mutate_cond(province %in% c("Manitoba"), group = 4)
zz %>% filter(!(province %in% c("Yukon","North Western Territories"))) %>%
  ggplot(aes(x = year, y = cover_p,  color = factor(province))) +
  geom_line(size = 1.5, alpha = 1, show.legend = F)  +
  labs(title = "",
       x = "Year", y = "Manufacturing sales coverage") +
  theme(
    # legend.title = element_text(size = 15),
    # legend.text=element_text(size= 15),
    # legend.key.size = unit(20,"cm")
    axis.title = element_text(size= 15),
    # legend.key.size = unit(20,"cm")
    # title = element_markdown()
    # plot.title = element_text(size=15, face="bold", hjust = 0.5,
    #                           margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    legend.position = "bottom",
    # axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5)
  ) + ylim(0,1)
  # facet_grid(group ~ .)

# zz %>% filter(!(province %in% c("Yukon","North Western Territories"))) %>%
#   ggplot(aes(x = factor(year), fill = as.factor(province))) +
#   geom_col(aes(y = cover_p), position = "dodge") 



# CC percentile




z2 <- z %>% group_by(year,province) %>% mutate(quants = rank(-population, ties.method = "min")/length(population))
# z2 <- z_csd %>% group_by(year,province) %>% mutate(quants = rank(-population, ties.method = "min")/length(population))

z3 <- z2 %>% group_by(year,province) %>% filter(any(population > 0), UID_CD_11 %in% unique(CC_p$UID_CD_11))
# z3 <- z2 %>% group_by(year,province) %>% filter(any(population > 0), UID_CSD_11 %in% unique(CC_p$UID_CSD_11))

aux <- as.data.frame(CC_p)%>% filter(turnover %in% c(1.1)) %>% select(year,UID_CD_11,district,group,province)
aux <- left_join(aux,z3[,c("UID_CD_11","year","quants")], by = c("UID_CD_11","year"))
# aux <- as.data.frame(CC_p)%>% filter(turnover %in% c(1.1)) %>% select(year,UID_CSD_11,subdistrict,group,province) 
# aux <- left_join(aux,z3[,c("UID_CSD_11","year","quants")], by = c("UID_CSD_11","year"))




z3 <- z3 %>% filter(!(province %in% c("Yukon","North Western Territories")))

colourCount = length(unique(z3$UID_CD_11))
# colourCount = length(unique(z3$UID_CSD_11))
getPalette = colorRampPalette(brewer.pal(8, "Set1"))

ggplot() +
  geom_line(data = z3, aes(x = year, y = quants, group = UID_CD_11, color = UID_CD_11), size = 1, alpha = 0.5, show.legend = F)  +
  # geom_line(data = z3, aes(x = year, y = quants, group = UID_CSD_11, color = UID_CSD_11), size = 1, alpha = 0.5, show.legend = F)  +
  scale_color_manual(values =  sample(getPalette(colourCount), replace = F))  +
  new_scale_color() +
  geom_point(data = aux, aes(x = year, y = quants, fill = factor(group)), size = 2, pch = 21, alpha = 1) + 
  scale_fill_manual(name = "Settlement", labels = c("Drafts", "Gold"), values = c("red","yellow")) +
  guides(fill = guide_legend(override.aes = list(size=5))) +
  facet_wrap(province ~ ., ncol = 2, nrow = 4) + 
  labs(title = "",
       x = "Year", y = "Rank Pertcentile") +
  theme(
    legend.title = element_text(size = 15),
    legend.text=element_text(size= 15),
    # legend.key.size = unit(20,"cm")
    # title = element_markdown()
    # plot.title = element_text(size=15, face="bold", hjust = 0.5,
    #                           margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5)
    # axis.title.x = element_text(size=15, vjust=0.5),
    # axis.text.y = element_text(size=12, vjust=0.5),
    # axis.title.y = element_text(size=15, vjust=1)
  ) + 
  ylim(0,1)


# manuf

z2 <- z %>% group_by(year,province) %>% mutate(quants = rank(-value, ties.method = "min")/length(value))
# z2 <- z_csd %>% group_by(year,province) %>% mutate(quants = rank(-value, ties.method = "min")/length(value))
# z3 <- z2 %>% group_by(year,province) %>% filter(any(value > 0), UID_CSD_11 %in% unique(CC_p$UID_CSD_11))
z3 <- z2 %>% group_by(year,province) %>% filter(any(value > 0), UID_CD_11 %in% unique(CC_p$UID_CD_11))
z3 <- z3 %>% mutate_cond(is.na(value), value = 0)
# z3 <- z2 %>% group_by(year,province) %>% filter(any(value > 0), UID_CSD_11 %in% unique(CC_p$UID_CSD_11))
z3$group <- 0
z3 <- z3 %>% mutate_cond(district %in% CC_g, group = 1)
# z3 <- z3 %>% mutate_cond(subdistrict %in% CC_g, group = 1)

ggplot() +
  geom_line(data = z3, aes(x = year, y = quants, group = UID_CD_11, colour = factor(group)), size = 1, alpha = 0.5, show.legend = F)  +
  # geom_line(data = z3, aes(x = year, y = quants, group = UID_CSD_11, colour = factor(group)), size = 1, alpha = 0.5, show.legend = F)  +
  scale_colour_manual(name = "Settlement", labels = c("Drafts", "Gold"), values = c("red","yellow")) +
  facet_wrap(province ~ ., ncol = 2, nrow = 4)  +
  labs(title = "",
       x = "Year", y = "Rank Pertcentile") +
  theme(
    # legend.title = element_text(size = 15),
    # legend.text=element_text(size= 15),
    # legend.key.size = unit(20,"cm")
    # title = element_markdown()
    # plot.title = element_text(size=15, face="bold", hjust = 0.5,
    #                           margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5)
    # axis.title.x = element_text(size=15, vjust=0.5),
    # axis.text.y = element_text(size=12, vjust=0.5),
    # axis.title.y = element_text(size=15, vjust=1)
  ) + 
  ylim(0,1)



# TURNOVER STATS ####

t <- turnover %>% filter(!(turnover == 0))
t$turnover[which(t$turnover %in% c(0.2,0.3,0.4,0.5))] <- 0.2
t <- t %>% group_by(turnover,year) %>% tally()
t$n[which(t$turnover %in% c(0.1,0.2,0.3,0.4,0.5))] <- -t$n[which(t$turnover %in% c(0.1,0.2,0.3,0.4,0.5))]

t %>%
  ggplot(aes(x = year, y = n, fill = as.factor(turnover))) + 
  geom_bar(stat = "identity")

tt <- t %>% filter(turnover == 1) %>% select(year,n)
tt <- rename(tt,"inc" = "n")
t <- left_join(t,tt[,-1], by = "year")
t <- t %>% group_by(year) %>% mutate(n_rel = n/inc)

t %>% filter(!(turnover == 1),year >= 1857, year <= 1913) %>%
  ggplot(aes(x = year, y = n_rel, fill = as.factor(turnover))) + 
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = round(seq(1857, 1913, by = 5),1))








# NEW


qq <- q_bs %>%
  # q_bs %>% filter((IDNO %in% c(1,26,29,5,49,8,10,9,30))) %>%
  group_by(Date) %>% 
  summarise("Aggregate portfolio share" = sum(iba + iba_sec, na.rm = T)/sum(a, na.rm = T),
            "Aggregate portfolio share, ROW" = sum(iba_row, na.rm = T)/sum(a, na.rm = T),
            "Aggregate portfolio share (unweigthed)" = sum((iba + iba_sec)/a, na.rm = T)/n(),
            "Aggregate portfolio share, ROW (unweigthed)" = sum((iba_row)/a, na.rm = T)/n(),
            "Aggregate portfolio" = sum(iba + iba_sec, na.rm = T),
            "Aggregate portfolio, ROW" = sum(iba_row, na.rm = T),
            "Aggregate portfolio (unweigthed)" = sum((iba + iba_sec), na.rm = T)/n(),
            "Aggregate portfolio, ROW (unweigthed)" = sum((iba_row), na.rm = T)/n())

qq <- melt(qq, id.vars = "Date")


qq %>% filter(variable %in% c("Aggregate portfolio share","Aggregate portfolio share, ROW")) %>%
  ggplot() +
  geom_line(aes(x = Date, y = value, colour = variable), size = 1) +
  scale_x_date(date_breaks = "7 years",date_labels = "%Y") +
  scale_y_continuous(breaks = round(seq(0, max(qq$value[which(qq$variable == "Aggregate portfolio share")]), by = 0.02),2)) +
  labs(title = "",
     x = "", y = "Percent")  +
  theme(
    legend.position = c(0.7,0.8),
    legend.title = element_blank(),
    legend.text=element_text(size=12),
    legend.box.background = element_blank(),
    legend.key.size = unit(10,"cm"),
    legend.key.width  = unit(1,"cm"),
    legend.key.height = unit(1,"cm"),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=15, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1),
    axis.text.x.bottom = element_text(angle = -45)
  ) 


qq %>% filter(variable %in% c("Aggregate portfolio","Aggregate portfolio, ROW")) %>%
  ggplot() +
  geom_line(aes(x = Date, y = value, colour = variable), size = 1) +
  scale_x_date(date_breaks = "7 years",date_labels = "%Y") +
  scale_y_continuous(breaks = round(seq(0, max(qq$value[which(qq$variable == "Aggregate portfolio")]), by = 0.02),2)) +
  labs(title = "",
       x = "", y = "(inter-bank correspondent assets)/(total assets)")  +
  theme(
    legend.position = c(0.7,0.8),
    legend.title = element_blank(),
    legend.text=element_text(size=12),
    legend.box.background = element_blank(),
    legend.key.size = unit(10,"cm"),
    legend.key.width  = unit(1,"cm"),
    legend.key.height = unit(1,"cm"),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=15, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1),
    axis.text.x.bottom = element_text(angle = -45)
  ) 



qq %>% filter(variable %in% c("Aggregate portfolio share (unweigthed)","Aggregate portfolio share, ROW (unweigthed)")) %>%
  ggplot() +
  geom_line(aes(x = Date, y = value, colour = variable), size = 1) +
  scale_x_date(date_breaks = "7 years",date_labels = "%Y") +
  scale_y_continuous(breaks = round(seq(0, max(qq$value[which(qq$variable == "Aggregate portfolio share (unweigthed)")]), by = 0.02),2)) +
  labs(title = "",
       x = "", y = "(inter-bank correspondent assets)/(total assets)")  +
  theme(
    legend.position = c(0.7,0.8),
    legend.title = element_blank(),
    legend.text=element_text(size=12),
    legend.box.background = element_blank(),
    legend.key.size = unit(10,"cm"),
    legend.key.width  = unit(1,"cm"),
    legend.key.height = unit(1,"cm"),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1),
    axis.text.x.bottom = element_text(angle = -45)
  ) 




    

# static network

y <- 1924

nodes <- q_b_agg_idno %>% filter(year %in% unique(edges$year)) %>%
  select(year,IDNO,ncc_nd)
nodes <- left_join(nodes,var_lab_in[,c("bank","IDNO")], by = "IDNO")
nodes <- nodes %>% filter(year == y)
nodes <- nodes %>% filter(IDNO %in% turnover$IDNO[which(turnover$turnover %in% c(1,1.1) & turnover$year == y)])
nodes <- nodes %>% select(-IDNO)
nodes <- nodes[,c(3,1,2)]

links <- edges %>% filter(!(is.na(place)),year == y, bank %in% unique(nodes$bank), !(bank %in% c("Commercial NL","Union NL")),
                          correspondent %in% unique(nodes$bank))

net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)
net <- simplify(net, remove.multiple = T, remove.loops = T) 

# ggraph(net) +
#   geom_edge_link(aes(color = where)) +           # colors by edge type 
#   geom_node_point(aes(size = nd)) +  # size by audience size  
#   theme_void()

# l <- layout.reingold.tilford(net, circular = F)
l <- layout.fruchterman.reingold(net, niter=100000)

net <- asNetwork(net) #ggnet requires a network object

ggnet2(net, mode = l,
       size = "ncc_nd",
       node.color = "steelblue", 
       edge.size = 0.75, edge.color = "orange",edge.alpha = 0.85,weights = "",
       arrow.size = 8,arrow.gap = 0.0275, arrow.type = "open") +
       theme(panel.background = element_rect(color = "grey"), legend.key.size = unit(1,"cm"), legend.title =  element_blank()) 

# +  scale_size_discrete("", range = c(5, 10), breaks = seq(10, 2, -2))

ggnet2(net, mode = l, size = "pop",max_size = 25, legend = "Number of districts")  



# join dist
# PLOT




qq <- q_bs_inc_year %>% filter(year >= 1874, year <= 1913) %>% mutate(pop_cover = pop/pop_year)

qq <- within(qq, quant <- as.integer(cut(ncc_p, unique(quantile(ncc_p, probs=0:10/10)), include.lowest=TRUE)))
qq <- qq %>% select(year,IDNO,ncc_p,quant,IB_net_share)


qq  %>% filter(year >= 1857, year <= 1913) %>%
  ggplot(aes(x= quant , y=  IB_net_share, group = as.factor(quant))) +
  # ggplot(aes(x=d_cover,y=IBa_share)) +
  geom_boxplot() + 
  # geom_point(size = 3, alpha = 0.75, color = "steelblue")     +
  geom_hline(yintercept=0, size = 1, color = "steelblue", lty = "dashed") +
  # axis(1, at = 1:8, labels = c("1-3", "4", "5", "6", "7", "8"), cex.axis = 1.5)
  # scale_color_discrete(name = "Reference", labels = c("At Canadian banks","At Foreign banks")) +
  labs(title = "",
       x = "Deciles of bank Clearing Centers' population coverage", y = "(bank net position)/(sum of gross positions)")  +
  theme(
    # legend.position = c(0.8,0.8),
    # legend.title = element_blank(),
    # legend.text=element_text(size=15),
    # legend.box.background = ele(),
    # legend.key.size = unit(1,"cm"),
    # legend.key.width  = unit(1,"cm"),
    # title = element_markdown(),
    # plot.title = element_text(size=15, face="bold", hjust = 0.5, 
    #                           margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=13, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=1),
    axis.text.y = element_text(size=13, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1)
  ) + 
  scale_x_continuous(breaks = round(seq(min(qq$quant), max(qq$quant), by = 1),1), labels = c("1-3", "4", "5", "6", "7", "8", "9", "10")) +
  scale_y_continuous(breaks = round(seq(min(qq$IB_net_share), max(qq$IB_net_share), by = 0.05),2))




# qqq <- qq %>% group_by(quant) %>%  within(., quant2 <- as.integer(cut(IB_net_share, quantile(IB_net_share, probs=0:4/4), include.lowest=TRUE)))
# qqq <- qqq %>% group_by(quant,quant2) %>% 
#   summarise(m = median(IBa_share,na.rm = T))

w <-  edges %>% filter(year == 1924) %>%
  group_by(bank,correspondent,year) %>% 
  distinct(correspondent, keep.all = T) %>% 
  tally(name = "resp")
w$resp[is.na(w$correspondent)] <- 0

# w1 <- w %>% group_by(bank,year) %>%  tally(name = "n_corr")
w1 <- w %>% group_by(bank,year) %>%  summarise("n_corr" = sum(resp,na.rm = T))

# w2 <- w %>% group_by(correspondent,year) %>% tally(name = "n_resp")
w2 <- w %>% group_by(correspondent,year) %>%  summarise("n_resp" = sum(resp,na.rm = T))
w2 <- rename(w2, "bank" = "correspondent")

w3 <- left_join(w1,w2, by = c("bank","year"))
w3$n_resp[is.na(w3$n_resp)] <- 0
# w$n_corr[is.na(w$n_corr)] <- 0

w3 <- w3 %>% group_by(n_corr,n_resp) %>% tally(name = "freq")

w3 <- w3 %>% mutate(r_freq = freq/sum(w3$freq, na.rm = T))


ggplot(w3, aes(n_corr, n_resp, fill= as.factor(freq))) + 
  geom_tile() +  
  # scale_fill_discrete(low = "skyblue",high = "blue4") +
scale_fill_brewer(palette = "Blues") +
  # scale_fill_gradient(low="white", high="blue2") +
  labs(title = "",
       x = "Number of correspondents", y = "Number of respondents")  +
  theme(
    # legend.position = "topright",
    legend.position = c(0.85,0.75),
    legend.title = element_blank(),
    legend.text=element_text(size=12),
    legend.box.background = element_blank(),
    legend.key.size = unit(10,"cm"),
    legend.key.width  = unit(1,"cm"),
    legend.key.height = unit(1,"cm"),
    plot.title = element_text(size=15, face="bold", hjust = 0.5, 
                              margin = margin(10, 10, 10, 10)),
    axis.text.x = element_text(size=12, vjust= 0.5),
    axis.title.x = element_text(size=15, vjust=0.5),
    axis.text.y = element_text(size=12, vjust=0.5),
    axis.title.y = element_text(size=15, vjust=1)
  ) + xlim(-1,12) + ylim(-1,12)


# diamonds.color.cut.df %>%
#   dcast(color ~ cut, value.nar = "n") %>%
#   kable(align = "l", format = "html",
#         table.attr='class="table table-striped table-hover"')


# BRANCH PREDICTION ####

for (i in 1:length(q_b_agg_prov$year)){
  
  if (q_b_agg_prov$province[i] %in% c("Alberta","Saskatchewan")){
    
    if (length(q_b_agg_prov$ncc_nd[which(q_b_agg_prov$province == "Manitoba" & q_b_agg_prov$year == q_b_agg_prov$year[i] & q_b_agg_prov$IDNO == q_b_agg_prov$IDNO[i])])   >  0){
      
      if (q_b_agg_prov$ncc_nd[which(q_b_agg_prov$province == "Manitoba" & q_b_agg_prov$year == q_b_agg_prov$year[i] & q_b_agg_prov$IDNO == q_b_agg_prov$IDNO[i])]   ==  1){
        
        q_b_agg_prov$ncc_nd[i] <- 1
      }
    }
  }
}

rm("branches_f")
rm("branches_cd")
rm("country_01_cd")
rm("country_11_cd")
rm("df_11_cdcd")
rm("df_71_cd")
rm("df_81_cd")
rm("df_01_cd")
rm("df_91_cd")
rm("df_cd")
rm("edges_count")
rm("manuf_71_cd")
rm("manuf_81_cd")
rm("manuf_11_cd")
rm("manuf_01_cd")
rm("manuf_91_cd")
rm("manuf_cd")
# rm("manuf_csd")
# rm("pop")
rm("ref_01_cd")
rm("ref_11_cdcd")
rm("ref_71_cd")
rm("ref_81_cd")
rm("ref_91_cd")
rm("ref_branches_cd")
rm("ref_cd")
# rm("trains_place")
rm("var_lab_ia")
rm("var_lab_il")


rm("q_bs","q_bs_all","q_bs_inc","q_bs_inc2","q_bs_inc2_year","q_bs_inc_year")
rm("q_bs_all_year")

z <- pop_csd
z$train <- NA
trains_buff <- trains
id <- st_intersects(trains_buff,df_csd, sparse = T)
for (i in 1:length(trains_buff$OBJECTID)){
  # for (i in 1:length(trains_buff$OBJECTID)){
  if (length(id[[i]]) > 0){
    
    aux2 <- as.data.frame(df_csd) %>% filter(UID_CSD_21 %in% ref_csd$UID_CSD_21[id[[i]]]) %>% select(UID_CSD_21)
    aux2 <- aux2$UID_CSD_21 
    
    z$train[which(z$UID_CSD_21 %in% aux2 & z$year >= trains_buff$CNSTRCTD[i] & z$year <= trains_buff$ABNDND[i])] <- 1  
  }
}

z$train[which(is.na(z$train))] <- 0

ref_branches <- left_join(ref_branches,var_lab_in[,c("IDNO","bank")], by = "bank")
ref_branches$y <- 1

t  <- turnover %>% filter(!(turnover == 0),year %in% unique(turnover_data$year)) %>% select(year,IDNO)
t2 <- sort(rep(ref_csd$UID_CSD_21,NROW(t)))
t <- t[rep(seq_len(nrow(t)), NROW(ref_csd)),]
t <- cbind(t,t2)
t <- rename(t,"UID_CSD_21" = "t2")
rm(t2)
t <- left_join(t,z[which(z$method == "linear"),c("year","UID_CSD_21","population","province","train")], by = c("year","UID_CSD_21"))
t <- left_join(t,q_bs_inc3_year[,c("year","IDNO","Totalass")], by = c("year","IDNO"))

t <- left_join(t,q_b_agg_prov[,c("province","year","IDNO","nbp_share","ncc_nd")], by = c("year","IDNO","province"))
t$nbp_share[which(is.na(t$nbp_share))] <- 0
t$ncc_nd[which(is.na(t$ncc_nd))] <- 0
t <- left_join(t,ref_branches[,c("UID_CSD_21","IDNO","year","y")], by = c("UID_CSD_21","IDNO","year"))
t$y[which(is.na(t$y))] <- 0

t <- t %>% filter(year %in% unique(ref_branches$year))
t <- t %>% filter(!(year == 1924))
t <- t %>% filter(!(province == "Yukon"))



rm(list=setdiff(ls(), "t"))

# install.packages("pglm")
# library(pglm)
install.packages("bife")
library(bife)
# install.packages("margins")
# library(margins)
# install.packages("fixest")
# library(fixest)

# install.packages("dplyr")
# library(dplyr)

# install.packages("raster")
# library(raster)


# m <- pglm(y ~ population, data = t, family = binomial('logit'),
            # model = "within", index = c("year"), na.action = na.omit)

# m <- pglm(y ~ -1 + log(1 + population) + year + IDNO + train + nbp_share + ncc_nd + nbp_share*ncc_nd, index = c("year","IDNO","train"), data = t, family = binomial('logit'))
          # model = "pooling", na.action = na.omit)

# m2 <- bife(y ~ log(1 + population) + factor(year) + factor(train) + nbp_share + ncc_nd + nbp_share*ncc_nd | IDNO , data = t, model = 'logit')
# m3 <- bife(y ~ log(1 + population) + factor(year) + factor(IDNO) + nbp_share + ncc_nd + nbp_share*ncc_nd | train , data = t, model = 'logit')
# m <- glm(y ~ -1 + log(1 + population) + factor(year) + factor(train) + factor(IDNO) + nbp_share + ncc_nd + nbp_share*ncc_nd, data = t, family = binomial('logit'))




m1 <- feglm(y ~ log(1 + population) + factor(train) + L.nbp_share + factor(L.ncc_nd) | year + IDNO + province, data = t, family = binomial('logit'), cluster = "IDNO")
m2 <- feglm(y ~ log(1 + population) + factor(train) + factor(L.ncc_nd)               | year + IDNO + province, data = t, family = binomial('logit'), cluster = "IDNO")
m3 <- feglm(y ~ log(1 + population) + factor(train) + L.nbp_share                    | year + IDNO + province, data = t, family = binomial('logit'), cluster = "IDNO")

m4 <- feglm(y ~ log(1 + population) + factor(train) + L.nbp_share + factor(L.ncc_nd) | year + IDNO               ,data = t, family = binomial('logit'), cluster = "IDNO")
m5 <- feglm(y ~ log(1 + population) + factor(train) + L.nbp_share + factor(L.ncc_nd) | year +        province    ,data = t, family = binomial('logit'), cluster = "IDNO")
m6 <- feglm(y ~ log(1 + population) + factor(train) + L.nbp_share + factor(L.ncc_nd) |        IDNO + province    ,data = t, family = binomial('logit'), cluster = "IDNO")

m7 <- feglm(y ~ log(1 + population) + factor(train) + L.nbp_share + factor(L.ncc_nd) | year + IDNO + province    ,data = t, family = binomial('logit'), cluster = "year")
m8 <- feglm(y ~ log(1 + population) + factor(train) + L.nbp_share + factor(L.ncc_nd) | year + IDNO + province    ,data = t, family = binomial('logit'), cluster = "province")


etable(m1,m2,m3,m4,m5,m6,m7,m8, tex = T,style.tex = style.tex("aer"))

a <- t %>% group_by(year,province) %>% summarise(train = all(train == 0),y = all(y == 0),ncc_nd = all(ncc_nd == 0))
a$z = a$train*a$y*a$ncc_nd
a <- a %>% filter(z == 0)
a <- a %>% mutate(z2 = paste0(province,year))
t <- t %>% mutate(z2 = paste0(province,year))
t2 <- t %>% filter(z2 %in% a$z2)
 

m9 <- feglm(y ~ log(1 + population) + factor(train) + L.nbp_share + factor(L.ncc_nd) | year + IDNO + province, data = t2, family = binomial('logit'), cluster = "IDNO")
m10 <- feglm(y ~ log(1 + population) + factor(train) + factor(L.ncc_nd)               | year + IDNO + province, data = t2, family = binomial('logit'), cluster = "IDNO")
m11 <- feglm(y ~ log(1 + population) + factor(train) + L.nbp_share                    | year + IDNO + province, data = t2, family = binomial('logit'), cluster = "IDNO")

m12 <- feglm(y ~ log(1 + population) + factor(train) + L.nbp_share + factor(L.ncc_nd) | year + IDNO               ,data = t2, family = binomial('logit'), cluster = "IDNO")
m13 <- feglm(y ~ log(1 + population) + factor(train) + L.nbp_share + factor(L.ncc_nd) | year +        province    ,data = t2, family = binomial('logit'), cluster = "IDNO")
m14 <- feglm(y ~ log(1 + population) + factor(train) + L.nbp_share + factor(L.ncc_nd) |        IDNO + province    ,data = t2, family = binomial('logit'), cluster = "IDNO")

m15 <- feglm(y ~ log(1 + population) + factor(train) + L.nbp_share + factor(L.ncc_nd) | year + IDNO + province    ,data = t2, family = binomial('logit'), cluster = "year")
m16 <- feglm(y ~ log(1 + population) + factor(train) + L.nbp_share + factor(L.ncc_nd) | year + IDNO + province    ,data = t2, family = binomial('logit'), cluster = "province")



t <- t %>%
  group_by(IDNO,UID_CSD_21) %>%
  mutate(L.ncc_nd = lag(ncc_nd, n = 1),L.nbp_share = lag(nbp_share, n = 1))
t <- t %>%
  group_by(IDNO,UID_CSD_21) %>%
  mutate(L2.ncc_nd = lag(ncc_nd, n = 2),L3.ncc_nd = lag(ncc_nd, n = 3),L4.ncc_nd = lag(ncc_nd, n = 4),L5.ncc_nd = lag(ncc_nd, n = 5))
t <- t %>%
  group_by(IDNO,UID_CSD_21) %>%
  mutate(L2.nbp_share = lag(nbp_share, n = 2),L3.nbp_share = lag(nbp_share, n = 3))

t3 <- t %>% group_by(IDNO,year,province) %>% filter(!duplicated(IDNO))


install.packages("betareg")
library(betareg)
install.packages("texreg")
library(texreg)

t3$nbp_share <- (t3$nbp_share*(NROW(t3$year) - 1) + .5)/NROW(t3$year)
n1 <- betareg(nbp_share ~  factor(L.ncc_nd) + factor(L2.ncc_nd) + L.nbp_share + L2.nbp_share + L3.nbp_share + factor(year) + factor(IDNO) + factor(province)  , data = t3)
n2 <- betareg(nbp_share ~  factor(L.ncc_nd) + L.nbp_share + L2.nbp_share + L3.nbp_share + factor(year) + factor(IDNO)                     , data = t3)
n3 <- betareg(nbp_share ~  factor(L.ncc_nd) + L.nbp_share + L2.nbp_share + L3.nbp_share + factor(year) +                factor(province)  , data = t3)
n4 <- betareg(nbp_share ~  factor(L.ncc_nd) + L.nbp_share + L2.nbp_share + L3.nbp_share +                factor(IDNO) + factor(province)  , data = t3)
n5 <- betareg(nbp_share ~  factor(L.ncc_nd) + L.nbp_share + L2.nbp_share + L3.nbp_share + factor(year)                                    , data = t3)
n6 <- betareg(nbp_share ~  factor(L.ncc_nd) + L.nbp_share + L2.nbp_share + L3.nbp_share +                factor(IDNO)                     , data = t3)
n7 <- betareg(nbp_share ~  factor(L.ncc_nd) + L.nbp_share + L2.nbp_share + L3.nbp_share +                               factor(province)  , data = t3)

# n1 <- feglm(nbp_share ~  factor(L.ncc_nd) + L.nbp_share + L2.nbp_share + L3.nbp_share | year + IDNO + province, data = t3, cluster = "IDNO")
# n2 <- feglm(nbp_share ~  factor(L.ncc_nd) + L.nbp_share + L2.nbp_share + L3.nbp_share |        IDNO + province, data = t3, cluster = "IDNO")
# n3 <- feglm(nbp_share ~  factor(L.ncc_nd) + L.nbp_share + L2.nbp_share + L3.nbp_share | year        + province, data = t3, cluster = "IDNO")
# n4 <- feglm(nbp_share ~  factor(L.ncc_nd) + L.nbp_share + L2.nbp_share + L3.nbp_share | year                  , data = t3, cluster = "IDNO")
# n5 <- feglm(nbp_share ~  factor(L.ncc_nd) + L.nbp_share + L2.nbp_share + L3.nbp_share |        IDNO           , data = t3, cluster = "IDNO")
# n6 <- feglm(nbp_share ~  factor(L.ncc_nd) + L.nbp_share + L2.nbp_share + L3.nbp_share |               province, data = t3, cluster = "IDNO")

texreg(list(n1,n2,n3,n4,n5,n6,n7))


etable(n1,n2,n3,n4,n5,n6,n7,n8, tex = T,style.tex = style.tex("aer"))

rm(list=setdiff(ls(), c("t","t2","t3")))



# me <- c(mean(log(t$population + 1)), mean(t$train), mean(t$nbp_share), mean(t$ncc_nd))
# me <- c(1,mean(log(t$population + 1)), mean(t$train), mean(t$nbp_share), 0)
# me <- c(mean(log(t$population + 1)), 1, 0)
me <- c(log(10000 + 1), 0, 1)

co <- as.vector(coef(m))
aux <- me*co

meb <- t %>% group_by(IDNO) %>% tally()
meb$n <- meb$n/sum(meb$n)
mey <- t %>% group_by(year) %>% tally()
mey$n <- mey$n/sum(mey$n)
mep <- t %>% group_by(province) %>% tally()
mep$n <- mep$n/sum(mep$n)

fe <- fixef(m)
auxy <- fe$year*mey$n
auxb <- fe$IDNO*meb$n[-which(meb$IDNO == 51)]
auxp <- fe$province*mep$n

e <- c(aux,auxy,auxb,auxp)

p <- exp(sum(e))
y <- p/(1+p)
coef(m)[2]*y*(1-y)





## fixed-effects logit -- note we must omit gender

install.packages("glmmML")
library(glmmML)

## A fixed effects logit.  Note that we must omit gender

m <- glmmboot(y ~ log(1 + population) + factor(year) + factor(IDNO) + factor(train) + nbp_share + ncc_nd + nbp_share*ncc_nd, data = t, family = binomial('logit'), cluster = IDNO)
