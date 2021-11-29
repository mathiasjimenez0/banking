
# interpolate population at the subdistrict level

pop_csd <- as.data.frame(df_212) %>% select(UID_CSD_21,UID_CD_21,district,subdistrict,province,pop_51,pop_61,pop_71,pop_81,pop_91,pop_01,population,pop_11)
oldnames = c("pop_51","pop_61","pop_71","pop_81","pop_91","pop_01","pop_11","population")
newnames = c("1851","1861","1871","1881","1891","1901","1911","1921")

pop_csd <- pop_csd %>% rename_at(oldnames, ~ newnames)
pop_csd <- melt(pop_csd,id.vars = c("UID_CSD_21","UID_CD_21","district","subdistrict","province"), variable.name = "year")
pop_csd <- rename(pop_csd, "population" = "value")
pop_csd <- pop_csd %>% mutate(year  = as.numeric(as.character(pop_csd$year)))



pop_csd$method <- NA
pop_csd$population2 <- NA

p <- pop_csd %>% select(-UID_CD_21,-district)
p$population[which(is.nan(p$population) | is.na(p$population))] <- 0
m2 <- p %>% select(UID_CSD_21,population,year) %>% filter(population == 0)
m2 <- m2 %>% group_by(UID_CSD_21) %>% summarise(max_year_zero = max(year, na.rm = T))

pp <- NULL
n <- 0
for (i in unique(p$UID_CSD_21)){
  n = n + 1
  print(n)  
  
  pop_csd_aux <- p %>% filter(UID_CSD_21 == i)
  d <- pop_csd_aux$subdistrict[1]
  pr <- pop_csd_aux$province[1]
  s <- spline(pop_csd_aux$year,pop_csd_aux$population, method = "natural", xmin =  1851, xmax =  1933, n = (1933 - 1851) + 1, ties = min)
  l <- approx(pop_csd_aux$year,pop_csd_aux$population, method = "linear", n = (1921 - 1851) + 1, ties = min)
  df_aux <- data.frame("UID_CSD_21" = rep(i,NROW(s[[1]])), "province" = rep(pr,NROW(s[[1]])),
                       "subdistrict" = rep(d,NROW(s[[1]])), "year" = s[[1]], "population" = s[[2]], "population2" = s[[2]], "method" = "natural")
  df_aux_l <- data.frame("UID_CSD_21" = rep(i,NROW(l[[1]])), "province" = rep(pr,NROW(l[[1]])),
                         "subdistrict" = rep(d,NROW(l[[1]])), "year" = l[[1]], "population" = l[[2]], "population2" = l[[2]], "method" = "linear")
  
  if (any(df_aux$population < 0)){
    aux <- which(df_aux$population < 0)
    aux <- aux[length(aux)]
    df_aux$population[1:aux] <- 0}
  if (any(df_aux$population2 < 0)){
    aux <- which(df_aux$population2 < 0)
    aux <- aux[length(aux)]
    df_aux$population2[1:aux] <- 0}
  
  if (i %in% unique(m2$UID_CSD_21)){
    df_aux$population2[which(df_aux$year < pmin((m2$max_year_zero[which(m2$UID_CSD_21 == i)] + 10), 1934)  )] <- 0
    df_aux_l$population2[which(df_aux$year < pmin((m2$max_year_zero[which(m2$UID_CSD_21 == i)] + 10), 1922) )] <- 0
  }
  
  
  pp <- rbind(pp,df_aux,df_aux_l)
}
  pop_aux <- pop_csd[!duplicated(pop_csd$UID_CSD_21),c("UID_CSD_21","UID_CD_21","district")]
  pop_csd <- left_join(pp,pop_aux, by = "UID_CSD_21")
  
# exchange 21 geography for 21 grography in mid canada

# remove population from dfs

df_212 <- df_212 %>% select(-population,-pop_01,-pop_avg,-pop_51,-pop_61,-pop_71,-pop_81,-pop_91,-pop_11)
ref_212 <- as.data.frame(df_212) %>% select(-geometry)

#

save.image("./almost_finished_052121.RData")
Sys.sleep(60)

#

df_2122 <- filter(df_212, !(str_detect(subdistrict,"Unorganized Parts|No Data|Autres parties|Other Parts|Remainder of province|Unorganized Northern Parts|Northern Unorganized Parts|Rural Parts|
                                       Terr W of R 28 N of TP 15 S of TP 28|Unsurveyed parts north of TCR|Parties non organisees")))
# id <- st_within(st_buffer(df_2122,dist = -100),st_convex_hull(st_remove_holes(df_2122)))
id <- st_within(df_2122,st_remove_holes(df_2122))

df_2122$UID_CSD_212 <- NA
for (i in 1:length(df_2122$UID_CSD_21)){

  if (length(id[[i]]) == 2){
    
  id_df_2122 <- unlist(id[i])[1]
  df_2122$UID_CSD_212[i] <- df_2122$UID_CSD_21[id_df_2122]
    
  } else if (length(id[[i]]) == 1){
    
    df_2122$UID_CSD_212[i] <- df_2122$UID_CSD_21[i]
  }
  
}
ref_2122 <- as.data.frame(df_2122) %>% select(-geometry)



branches3 <- filter(branches3, !(str_detect(subdistrict,"Unorganized Parts|No Data|Autres parties|Other Parts|Remainder of province|Unorganized Northern Parts|Northern Unorganized Parts|Rural Parts|
                                       Terr W of R 28 N of TP 15 S of TP 28|Unsurveyed parts north of TCR|Parties non organisees")))
pop_csd <- filter(pop_csd, !(str_detect(subdistrict,"Unorganized Parts|No Data|Autres parties|Other Parts|Remainder of province|Unorganized Northern Parts|Northern Unorganized Parts|Rural Parts|
                                       Terr W of R 28 N of TP 15 S of TP 28|Unsurveyed parts north of TCR|Parties non organisees")))
manuf3 <- filter(manuf3, !(str_detect(subdistrict,"Unorganized Parts|No Data|Autres parties|Other Parts|Remainder of province|Unorganized Northern Parts|Northern Unorganized Parts|Rural Parts|
                                       Terr W of R 28 N of TP 15 S of TP 28|Unsurveyed parts north of TCR|Parties non organisees")))
imuni3 <- filter(imuni3, !(str_detect(subdistrict,"Unorganized Parts|No Data|Autres parties|Other Parts|Remainder of province|Unorganized Northern Parts|Northern Unorganized Parts|Rural Parts|
                                       Terr W of R 28 N of TP 15 S of TP 28|Unsurveyed parts north of TCR|Parties non organisees")))

branches3 <- left_join(branches3,ref_2122[,c("UID_CSD_21","UID_CSD_212")])
pop_csd <- left_join(pop_csd,ref_2122[,c("UID_CSD_21","UID_CSD_212")])
pop_csd <- pop_csd %>% select(-UID_CSD_21)
branches3 <- branches3 %>% select(-UID_CSD_21)
CC <- left_join(CC,ref_2122[,c("UID_CSD_21","UID_CSD_212")])
CC <- CC %>% select(-UID_CSD_21)
CC_p <- left_join(CC_p,ref_2122[,c("UID_CSD_21","UID_CSD_212")])
CC_p <- CC_p %>% select(-UID_CSD_21)
manuf3 <- left_join(manuf3,ref_2122[,c("UID_CSD_21","UID_CSD_212")])
manuf3 <- manuf3 %>% select(-UID_CSD_21)
imuni3 <- left_join(imuni3,ref_2122[,c("UID_CSD_21","UID_CSD_212")])
imuni3 <- imuni3 %>% select(-UID_CSD_21)
#

aux <- CC_p %>% select(subdistrict,UID_CSD_212)

for (i in aux$UID_CSD_212){
  
  df_2122$subdistrict[which(df_2122$UID_CSD_212 == i)] <- aux$subdistrict[which(aux$UID_CSD_212 == i)]
  ref_2122$subdistrict[which(ref_2122$UID_CSD_212 == i)] <- aux$subdistrict[which(aux$UID_CSD_212 == i)]
  pop_csd$subdistrict[which(pop_csd$UID_CSD_212 == i)] <- aux$subdistrict[which(aux$UID_CSD_212 == i)]
  manuf3$subdistrict[which(manuf3$UID_CSD_212 == i)] <- aux$subdistrict[which(aux$UID_CSD_212 == i)]
  imuni3$subdistrict[which(imuni3$UID_CSD_212 == i)] <- aux$subdistrict[which(aux$UID_CSD_212 == i)]
  
}

#
q6 <- df_2122
q6_aux <- q6 %>% select(c("UID_CSD_212","geometry","area"))
q6_aux2 <-as.data.frame(q6) %>% select(-c("geometry","area"))

df_21_csd2_aux <- aggregate(q6_aux[,-which(colnames(q6_aux) %in% c("UID_CSD_212"))], by =list("UID_CSD_212" = q6_aux$UID_CSD_212), FUN = function(x) sum(x,na.rm = T), do_union = T)
df_21_csd2_aux2 <- aggregate(q6_aux2[,-which(colnames(q6_aux2) %in% c("UID_CSD_212"))], by =list("UID_CSD_212" = q6_aux2$UID_CSD_212), FUN = function(x) extract2(x,1))
df_2122 <- left_join(df_21_csd2_aux,df_21_csd2_aux2, by = c("UID_CSD_212"))

df_2122 <- df_2122 %>% select(-UID_CSD_21)
df_2122 <- mutate(df_2122, area = st_area(geometry))
ref_2122 <- as.data.frame(df_2122) %>% select(-geometry)

#
q6 <- pop_csd
q6_aux <- q6 %>% select(c("UID_CSD_212","year","method","population","population2"))
q6_aux2 <-as.data.frame(q6) %>% select(-c("population","population2"))

df_21_csd2_aux <- aggregate(q6_aux[,-which(colnames(q6_aux) %in% c("UID_CSD_212","year","method"))], by =list("UID_CSD_212" = q6_aux$UID_CSD_212,"year" = q6_aux$year,"method" = q6_aux$method), FUN = function(x) sum(x,na.rm = T))
df_21_csd2_aux2 <- aggregate(q6_aux2[,-which(colnames(q6_aux2) %in% c("UID_CSD_212","year","method"))], by =list("UID_CSD_212" = q6_aux2$UID_CSD_212,"year" = q6_aux2$year,"method" = q6_aux2$method), FUN = function(x) extract2(x,1))
pop_csd <- left_join(df_21_csd2_aux,df_21_csd2_aux2, by = c("UID_CSD_212","year","method"))

# aggregate to the district level

q6 <- df_2122
q6_aux <- q6 %>% select(c("district","province","geometry","area"))
q6_aux2 <-as.data.frame(q6) %>% select(-c("geometry","area"))

df_21_cd2_aux <- aggregate(q6_aux[,-which(colnames(q6_aux) %in% c("district","province"))], by =list("district" = q6_aux$district, "province" = q6_aux$province), FUN = function(x) sum(x,na.rm = T), do_union = T)
df_21_cd2_aux2 <- aggregate(q6_aux2[,-which(colnames(q6_aux2) %in% c("district","province"))], by =list("district" = q6_aux2$district, "province" = q6_aux2$province), FUN = function(x) extract2(x,1))
df_21_cd2 <- left_join(df_21_cd2_aux,df_21_cd2_aux2, by = c("district","province"))

df_21_cd2 <- mutate(df_21_cd2, area = st_area(geometry))

# remove irrelevant columns from dfs

df_21_cd2 <- df_21_cd2 %>% select(-UID_CSD_212,-subdistrict,-TYPE_21,-URB_RUR_21,-CSD_NO_21)
ref_21_cd2 <- as.data.frame(df_21_cd2) %>% select(-geometry)



# interpolate population

q6 <- pop_csd
q6_aux <- q6 %>% select(c("district","province","year","method","population","population2"))
q6_aux2 <- q6 %>% select(-c("population","population2"))

pop_aux <- aggregate(q6_aux[,-which(colnames(q6_aux) %in% c("district","province","year","method"))], by =list("district" = q6_aux$district, "province" = q6_aux$province, "year" = q6_aux$year, "method" = q6_aux$method), FUN = function(x) sum(x,na.rm = T))
pop_aux2 <- aggregate(q6_aux2[,-which(colnames(q6_aux2) %in% c("district","province","year","method"))], by =list("district" = q6_aux2$district, "province" = q6_aux2$province, "year" = q6_aux2$year, "method" = q6_aux2$method), FUN = function(x) extract2(x,1))
pop <- left_join(pop_aux,pop_aux2, by = c("district","province","year","method"))
pop <- pop %>% select(-UID_CSD_212,-subdistrict)

#
manuf3[c("population","establishments","capital","employees","wages","cost","value")] <- sapply(manuf3[c("population","establishments","capital","employees","wages","cost","value")],as.numeric)
q6 <- manuf3
q6_aux <- q6 %>% select(c("district","province","year","population","establishments","capital","employees","wages","cost","value"))
q6_aux2 <- q6 %>% select(-c("population","establishments","capital","employees","wages","cost","value"))

manuf3_aux <- aggregate(q6_aux[,-which(colnames(q6_aux) %in% c("district","province","year"))], by =list("district" = q6_aux$district, "province" = q6_aux$province, "year" = q6_aux$year), FUN = function(x) sum(x,na.rm = T))
manuf3_aux2 <- aggregate(q6_aux2[,-which(colnames(q6_aux2) %in% c("district","province","year"))], by =list("district" = q6_aux2$district, "province" = q6_aux2$province, "year" = q6_aux2$year), FUN = function(x) extract2(x,1))
manuf_cd <- left_join(manuf3_aux,manuf3_aux2, by = c("district","province","year"))
manuf_cd <- manuf_cd %>% select(-UID_CSD_212,-subdistrict)


#
df_2122 <- rename(df_2122, "UID_CSD_21" = "UID_CSD_212")
ref_2122 <- rename(ref_2122, "UID_CSD_21" = "UID_CSD_212")
branches3 <- rename(branches3, "UID_CSD_21" = "UID_CSD_212")
manuf3 <- rename(manuf3, "UID_CSD_21" = "UID_CSD_212")
imuni3 <- rename(imuni3, "UID_CSD_21" = "UID_CSD_212")
pop_csd <- rename(pop_csd, "UID_CSD_21" = "UID_CSD_212")
CC <- rename(CC, "UID_CSD_21" = "UID_CSD_212")
CC_p <- rename(CC_p, "UID_CSD_21" = "UID_CSD_212")

# aggregate manuf_11 repeated districts

manuf_11$establishments <- as.numeric(manuf_11$establishments)
manuf_11$capital<- as.numeric(manuf_11$capital)
manuf_11$employees <- as.numeric(manuf_11$employees)
manuf_11$wages <- as.numeric(manuf_11$wages)
manuf_11$cost <- as.numeric(manuf_11$cost)
manuf_11$value <- as.numeric(manuf_11$value)

manuf_11 <- aggregate(manuf_11[,-which(colnames(manuf_11) %in% c("district","province"))], by =list("district" = manuf_11$district, "province" = manuf_11$province), FUN = function(x) sum(x,na.rm = T))
manuf_11_cd <- right_join(ref_11_cdcd[,c("district","province","UID_CD_11")], manuf_11, by = c("district" = "district", "province" = "province"))


country_11$`farms leased, number` <- as.numeric(country_11$`farms leased, number`)
country_11$`land leased, acres` <- as.numeric(country_11$`land leased, acres`)
country_11 <- country_11 %>% select(-UID_CD_11,year)
country_11 <- aggregate(country_11[,-which(colnames(country_11) %in% c("district","province"))], by =list("district" = country_11$district, "province" = country_11$province), FUN = function(x) sum(x,na.rm = T))
country_11_cd <- right_join(ref_11_cdcd[,c("district","province","UID_CD_11")], country_11, by = c("district" = "district", "province" = "province"))


ref_01_cd$UID_CD_01 <- 1:length(ref_01_cd$district)
df_01_cd$UID_CD_01 <- 1:length(df_01_cd$district)
country_01 <- country_01 %>% select(-year,-UID_CD_01)
country_01$`weeks of labour, number` <- as.numeric(country_01$`weeks of labour, number`)
country_01 <- aggregate(country_01[,-which(colnames(country_01) %in% c("district","province"))], by =list("district" = country_01$district, "province" = country_01$province), FUN = function(x) sum(x,na.rm = T))
country_01_cd <- right_join(ref_01_cd[,c("district","province","UID_CD_01")], country_01, by = c("district" = "district", "province" = "province"))


manuf_01_aux <- manuf_01 %>% filter(!(is.na(province)))
manuf_01_aux2 <- manuf_01 %>% filter(is.na(province)) %>% select(-province)
manuf_01_aux2 <- right_join(ref_01_cd[,c("district","province")], manuf_01_aux2, by = c("district" = "district"))
manuf_01_cd <- rbind(manuf_01_aux,manuf_01_aux2)

manuf_01$number <- as.numeric(manuf_01$number)
manuf_01$capital <- as.numeric(manuf_01$capital)
manuf_01$labour_s <- as.numeric(manuf_01$labour_s)
manuf_01$salaries <- as.numeric(manuf_01$salaries)
manuf_01$labour_w <- as.numeric(manuf_01$labour_w)
manuf_01$wages <- as.numeric(manuf_01$wages)
manuf_01$raw <- as.numeric(manuf_01$raw)
manuf_01$value <- as.numeric(manuf_01$value)

manuf_01 <- aggregate(manuf_01[,-which(colnames(manuf_01) %in% c("district","province"))], by =list("district" = manuf_01$district, "province" = manuf_01$province), FUN = function(x) sum(x,na.rm = T))
manuf_01_cd <- right_join(ref_01_cd[,c("district","province","UID_CD_01")], manuf_01, by = c("district" = "district", "province" = "province"))

rm(manuf_01_aux,manuf_01_aux2)


ref_91_cd$UID_CD_91 <- 1:length(ref_91_cd$district)
df_91_cd$UID_CD_91 <- 1:length(df_91_cd$district)

manuf_91_aux <- manuf_91 %>% filter(!(is.na(province)))
manuf_91_aux2 <- manuf_91 %>% filter(is.na(province)) %>% select(-province)
manuf_91_aux2 <- right_join(ref_91_cd[,c("district","province")], manuf_91_aux2, by = c("district" = "district"))
manuf_91_cd <- rbind(manuf_91_aux,manuf_91_aux2)

manuf_91 <- aggregate(manuf_91[,-which(colnames(manuf_91) %in% c("district","province"))], by =list("district" = manuf_91$district, "province" = manuf_91$province), FUN = function(x) sum(x,na.rm = T))
manuf_91_cd <- right_join(ref_91_cd[,c("district","province","UID_CD_91")], manuf_91, by = c("district" = "district", "province" = "province"))

rm(manuf_91_aux,manuf_91_aux2)



ref_81_cd$UID_CD_81 <- 1:length(ref_81_cd$district)
df_81_cd$UID_CD_81 <- 1:length(df_81_cd$district)

manuf_81 <- manuf_81 %>% select(-id)
manuf_81_aux <- manuf_81 %>% filter(!(is.na(province)))
manuf_81_aux2 <- manuf_81 %>% filter(is.na(province)) %>% select(-province)
manuf_81_aux2 <- right_join(ref_81_cd[,c("district","province")], manuf_81_aux2, by = c("district" = "district"))
manuf_81 <- rbind(manuf_81_aux,manuf_81_aux2)

manuf_81 <- aggregate(manuf_81[,-which(colnames(manuf_81) %in% c("district","province"))], by =list("district" = manuf_81$district, "province" = manuf_81$province), FUN = function(x) sum(x,na.rm = T))
manuf_81_cd <- right_join(ref_81_cd[,c("district","province","UID_CD_81")], manuf_81, by = c("district" = "district", "province" = "province"))

rm(manuf_81_aux,manuf_81_aux2)


ref_71_cd$UID_CD_71 <- 1:length(ref_71_cd$district)
df_71_cd$UID_CD_71 <- 1:length(df_71_cd$district)

manuf_71 <- manuf_71 %>% select(-id)
manuf_71_aux <- manuf_71 %>% filter(!(is.na(province)))
manuf_71_aux2 <- manuf_71 %>% filter(is.na(province)) %>% select(-province)
manuf_71_aux2 <- right_join(ref_71_cd[,c("district","province")], manuf_71_aux2, by = c("district" = "district"))
manuf_71 <- rbind(manuf_71_aux,manuf_71_aux2)

manuf_71 <- aggregate(manuf_71[,-which(colnames(manuf_71) %in% c("district","province"))], by =list("district" = manuf_71$district, "province" = manuf_71$province), FUN = function(x) sum(x,na.rm = T))
manuf_71_cd <- right_join(ref_71_cd[,c("district","province","UID_CD_71")], manuf_71, by = c("district" = "district", "province" = "province"))

rm(manuf_71_aux,manuf_71_aux2)


# add columns to branches3 to know to which district they belong each year
branches_cd <- branches3 %>% select(-UID_CSD_21,-UID_CD_21)

id7 <- st_within(branches_cd, df_71_cd, sparse = TRUE, prepared = TRUE)
id8 <- st_within(branches_cd, df_81_cd, sparse = TRUE, prepared = TRUE)
id9 <- st_within(branches_cd, df_91_cd, sparse = TRUE, prepared = TRUE)
id0 <- st_within(branches_cd, df_01_cd, sparse = TRUE, prepared = TRUE)
id1 <- st_within(branches_cd, df_11_cdcd, sparse = TRUE, prepared = TRUE)

branches_cd$UID_CD_11 <- NA
branches_cd$UID_CD_01 <- NA
branches_cd$UID_CD_91 <- NA
branches_cd$UID_CD_81 <- NA
branches_cd$UID_CD_71 <- NA

# for (i in 1:length(id7)){
for (i in which(is.na(branches_cd$UID_CD_71))){
  
  
  if (length(id1[[i]]) > 0){
    
    branches_cd$UID_CD_11[i] <- df_11_cdcd$UID_CD_11[id1[[i]]]
    
  } else{
    
    branches_cd$UID_CD_11[i] <- df_11_cdcd$UID_CD_11[which.min(st_distance(df_11_cdcd$geometry, branches_cd$geometry[i]))]
    
  }
  if (length(id0[[i]]) > 0){
    
    branches_cd$UID_CD_01[i] <- df_01_cd$UID_CD_01[id0[[i]]]
    
  } else{
    
    branches_cd$UID_CD_01[i] <- df_01_cd$UID_CD_01[which.min(st_distance(df_01_cd$geometry, branches_cd$geometry[i]))]
    
  }
  if (length(id9[[i]]) > 0){
    
    branches_cd$UID_CD_91[i] <- df_91_cd$UID_CD_91[id9[[i]]]
    
  } else{
    
    branches_cd$UID_CD_91[i] <- df_91_cd$UID_CD_91[which.min(st_distance(df_91_cd$geometry, branches_cd$geometry[i]))]
    
  }
  if (length(id8[[i]]) > 0){
    
    branches_cd$UID_CD_81[i] <- df_81_cd$UID_CD_81[id8[[i]]]
    
  } else{
    
    branches_cd$UID_CD_81[i] <- df_81_cd$UID_CD_81[which.min(st_distance(df_81_cd$geometry, branches_cd$geometry[i]))]
    
  }
  if (length(id7[[i]]) > 0){
    
    branches_cd$UID_CD_71[i] <- df_71_cd$UID_CD_71[id7[[i]]]
    
  } else{
    
    branches_cd$UID_CD_71[i] <- df_71_cd$UID_CD_71[which.min(st_distance(df_71_cd$geometry, branches_cd$geometry[i]))]
    
  }
}

if (change == 0){
  # change columns
  
  # df_11_mid$UID_CD_11 <- gsub("cc","c",df_11_mid$UID_CD_11)
  # df_11_mid$UID_CSD_11 <- gsub("cc","c",df_11_mid$UID_CSD_11)
  # df_21_mid$UID_CD_11 <- gsub("cc","c",df_21_mid$UID_CD_11)
  # 
  # ref_21_mid$UID_CD_11 <- gsub("cc","c",ref_21_mid$UID_CD_11)
  # 
  # df_112$UID_CD_11 <- gsub("cc","c",df_112$UID_CD_11)
  # df_112$UID_CSD_11 <- gsub("cc","c",df_112$UID_CSD_11)
  # df_11_cd2$UID_CD_11 <- gsub("cc","c",df_11_cd2$UID_CD_11)
  # 
  # df_11_cdcd$UID_CD_11 <- gsub("cc","c",df_11_cdcd$UID_CD_11)
  # ref_11_cdcd$UID_CD_11 <- gsub("cc","c",ref_11_cdcd$UID_CD_11)
  # 
  # df_21_csd$UID_CSD_21 <- gsub("cc","c",df_21_csd$UID_CSD_21)
  # df_21_csd$UID_CD_21 <- gsub("cc","c",df_21_csd$UID_CD_21)
  # df_21_cd$UID_CD_21 <- gsub("cc","c",df_21_cd$UID_CD_21)
  # 
  # ref_112$UID_CD_11 <- gsub("cc","c",ref_112$UID_CD_11)
  # ref_112$UID_CSD_11 <- gsub("cc","c",ref_112$UID_CSD_11)
  # ref_11_cd2$UID_CD_11 <- gsub("cc","c",ref_11_cd2$UID_CD_11)
  # ref_21_csd$UID_CD_21 <- gsub("cc","c",ref_21_csd$UID_CD_21)
  # ref_21_csd$UID_CSD_21 <- gsub("cc","c",ref_21_csd$UID_CSD_21)
  # ref_21_cd$UID_CD_21 <- gsub("cc","c",ref_21_cd$UID_CD_21)
  # 
  # 
  # pop_csd$UID_CD_11 <- gsub("cc","c",pop_csd$UID_CD_11)
  # pop_csd$UID_CSD_11 <- gsub("cc","c",pop_csd$UID_CSD_11)
  # branches3$UID_CD_11 <- gsub("cc","c",branches3$UID_CD_11)
  # branches3$UID_CSD_11 <- gsub("cc","c",branches3$UID_CSD_11)
  # branches_nfld$UID_CD_11 <- gsub("cc","c",branches_nfld$UID_CD_11)
  # branches_nfld$UID_CSD_11 <- gsub("cc","c",branches_nfld$UID_CSD_11)
  # 
  # CC$UID_CD_11 <- gsub("cc","c",CC$UID_CD_11)
  # CC$UID_CSD_11 <- gsub("cc","c",CC$UID_CSD_11)
  # CC_p$UID_CD_11 <- gsub("cc","c",CC_p$UID_CD_11)
  # CC_p$UID_CSD_11 <- gsub("cc","c",CC_p$UID_CSD_11)
  # 
  # country_01$UID_CD_01 <- gsub("cc","c",country_01$UID_CD_01)
  # country_11$UID_CD_11 <- gsub("cc","c",country_11$UID_CD_11)
  # 
  # # df_01_cd$UID_CD_01 <- gsub("cc","c",df_01_cd$UID_CD_01)
  # # df_91_cd$UID_CD_91 <- gsub("cc","c",df_91_cd$UID_CD_91)
  # # df_81_cd$UID_CD_81 <- gsub("cc","c",df_81_cd$UID_CD_81)
  # # df_71_cd$UID_CD_71 <- gsub("cc","c",df_71_cd$UID_CD_71)
  # # df_21_cd$UID_CD_21 <- gsub("cc","c",df_21_cd$UID_CD_21)
  # #
  # # ref_01_cd$UID_CD_11 <- gsub("cc","c",ref_01_cd$UID_CD_11)
  # # ref_91_cd$UID_CD_11 <- gsub("cc","c",ref_01_cd$UID_CD_11)
  # # ref_81_cd$UID_CD_11 <- gsub("cc","c",ref_01_cd$UID_CD_11)
  # # ref_71_cd$UID_CD_11 <- gsub("cc","c",ref_01_cd$UID_CD_11)
  # # ref_21_cd$UID_CD_11 <- gsub("cc","c",ref_01_cd$UID_CD_11)
  # 
  # imuni3$UID_CD_11 <- gsub("cc","c",imuni3$UID_CD_11)
  # imuni3$UID_CSD_11 <- gsub("cc","c",imuni3$UID_CSD_11)
  # 
  # # manuf_01$UID_CD_01 <- gsub("cc","c",manuf_01$UID_CD_01)
  # # manuf_91$UID_CD_91 <- gsub("cc","c",manuf_91$UID_CD_91)
  # # manuf_81$UID_CD_81 <- gsub("cc","c",manuf_81$UID_CD_81)
  # # manuf_71$UID_CD_71 <- gsub("cc","c",manuf_71$UID_CD_71)
  # # manuf_21$UID_CD_21 <- gsub("cc","c",manuf_21$UID_CD_21)
  # # manuf_11$UID_CD_11 <- gsub("cc","c",manuf_11$UID_CD_11)
  # 
  # manuf_csd$UID_CD_11 <- gsub("cc","c",manuf_csd$UID_CD_11)
  # manuf_csd$UID_CSD_11 <- gsub("cc","c",manuf_csd$UID_CSD_11)
  # manuf3_cd$UID_CD_11 <- gsub("cc","c",manuf3_cd$UID_CD_11)
  
  
  manuf_csd <- manuf3
  imuni_csd <- imuni3
  manuf_cd <- rename(manuf_cd,c("number" = "establishments","labour" = "employees","sales" = "value"))
  manuf_csd <- rename(manuf_csd,c("number" = "establishments","labour" = "employees","sales" = "value"))
  country_01_cd <- rename(country_01_cd,c("capital_stock" = "live stock","sales_stock" = "live stock sold in year and total animal products",
                                          "leased_acres" = "land leased, acres","labour_weeks" = "weeks of labour, number","capital_lbt" = "land_buildings_implements",
                                          "sales_field" = "field crops, fruits and vegetables and nursery", "leased_number" = "farms leased, number",
                                          "rent" = "rent of land and buildings","wages" = "cost of hired labour"))
  country_11_cd <- rename(country_11_cd,c("capital_stock" = "live stock","sales_stock" = "live stock sold in year and total animal products",
                                          "leased_acres" = "land leased, acres","labour_weeks" = "weeks of labour, number","capital_lbt" = "land_buildings_implements",
                                          "sales_field" = "field crops, fruits and vegetables and nursery", "leased_number" = "farms leased, number",
                                          "rent" = "rent of land and buildings","wages" = "cost of hired labour"))
  manuf_11_cd <- rename(manuf_11_cd,c("number" = "establishments","labour" = "employees","sales" = "value"))
  manuf_01_cd <- rename(manuf_01_cd,c("cost" = "raw","sales" = "value"))
  manuf_01_cd <- manuf_01_cd %>% mutate(labour = labour_s + labour_w)
  manuf_01_cd <- manuf_01_cd %>% mutate(wages = wages + salaries)
  manuf_01_cd <- manuf_01_cd %>% select(-labour_s,-labour_w,-salaries)
  manuf_91_cd <- rename(manuf_91_cd,c("cost" = "raw","capital_l" = "capital_land","capital_b" = "capital_buildings","capital_t" = "capital_tools","capital_w" = "capital_working"))
  manuf_81_cd <- rename(manuf_81_cd,c("cost" = "raw","sales" = "value"))
  manuf_71_cd <- rename(manuf_71_cd,c("cost" = "raw","sales" = "value"))
  
  
  # q5 <- imuni3
  # q5_aux <- q5 %>% select(c("district","province"))
  # 
  # imuni3_cd_aux <- aggregate(q5_aux[,-which(colnames(q5_aux) %in% c("district","province"))], by =list("district" = q5_aux$district, "province" = q5_aux$province), FUN = function(x) sum(x,na.rm = T), do_union = T)
  # imuni3_cd_aux2 <- aggregate(q5_aux2[,-which(colnames(q5_aux2) %in% c("district","province"))], by =list("district" = q5_aux2$district, "province" = q5_aux2$province), FUN = function(x) extract2(x,1))
  # imuni3_cd <- left_join(imuni3_cd_aux,imuni3_cd_aux2, by = c("district","province"))
  # df_11_cd2 <- st_join(df_11_cd2,imuni3_cd, by = c("district" = "district", "province" = "province"))
  
  
  # ref_manuf3_cd <- as.data.frame(manuf3_cd) %>% select(-c("geometry"))
  # ref_imuni3_cd <- as.data.frame(imuni3_cd) %>% select(-c("geometry"))
}



