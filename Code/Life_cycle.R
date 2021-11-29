# Modify cities in 1911/1901 so they are consistent with ones before that

# # aux was created to look for cities in maps before 1901
# ref_early_cd <- rbind(ref_51_cd,ref_61_cd,ref_71_cd,ref_81_cd,ref_91_cd)
# aux <- ref_early_cd %>% filter(str_detect(district,
#"Vancouver|Victoria|Winnipeg|John|Halifax|Ottawa|Toronto|Montreal|Quebec|Sherbrooke|Trois Rivieres|Hamilton|Regina|Saskatoon|Calgary|Edmonton"))

# Halifax: Halifax, ward 1-6

df_11_csd <- df_11_csd %>% mutate_cond(str_detect(district,"Halifax") & TYPE_11 %in% c("W"),
                                         district = "Halifax City")
df_21_csd <- df_21_csd %>% mutate_cond(str_detect(district,"Halifax") & TYPE_21 %in% c("C"),
                                       district = "Halifax City")

# df_11_csd$district[which(!grepl("Halifax",df_11_csd$subdistrict) & df_11_csd$district == "Halifax City & County")] <- "Halifax County"
# df_21_csd$district[which(!grepl("City",df_21_csd$subdistrict) & df_21_csd$district == "Halifax")] <- "Halifax County"

df_11_csd <- df_11_csd %>% mutate_cond(district == "Halifax City", 
                                       subdistrict = "Halifax City")
df_21_csd <- df_21_csd %>% mutate_cond(district == "Halifax City", 
                                       subdistrict = "Halifax City")

# Hamilton: Hamiltron East, Hamilton West, Wentworth, Barton

df_11_csd <- df_11_csd %>% mutate_cond(subdistrict %in% c("Hamilton (part)","Barton") & TYPE_11 %in% c("C_PT","TP"), 
                                         district = "Hamilton City")
df_21_csd <- df_21_csd %>% mutate_cond(subdistrict %in% c("Hamilton","Barton") & TYPE_21 %in% c("C","TP"), 
                                       district = "Hamilton City")

df_11_csd <- df_11_csd %>% mutate_cond(district == "Hamilton City", 
                                       subdistrict = "Hamilton City")
df_21_csd <- df_21_csd %>% mutate_cond(district == "Hamilton City", 
                                       subdistrict = "Hamilton City")
# Montreal 

# df_11_csd2 <- df_11_csd %>% mutate_cond(str_detect(district,"Montreal") & TYPE_11 %in% c("C_PT","W","ND","W_PT"), 
#                                         district = "Montreal City")
# df_11_csd2 <- df_11_csd2 %>% mutate_cond(district == "Montreal City", 
#                                          subdistrict = "Montreal City")
# 
# df_11_csd2 <- df_11_csd2 %>% mutate_cond(str_detect(district,"Jacques Cartier|Hochelaga|Maisonneuve") & TYPE_11 %in% c("C_PT","W","ND","W_PT"), 
#                                         district = "Montreal City") 
# df_11_csd2 <- df_11_csd2 %>% mutate_cond(district == "Montreal City", 
#                                          subdistrict = "Montreal City")
# 
# 
# df_11_csd2 <- df_11_csd %>% mutate_cond(str_detect(district,"Jacques Cartier|Hochelaga|Maisonneuve|Laval|Montreal") & TYPE_11 %in% c("C_PT","W","ND","W_PT"), 
#                                        district = "Montreal City") 
# df_11_csd2 <- df_11_csd2 %>% mutate_cond(str_detect(subdistrict,"Maisonneuve|Westmount|Ile Ste Helene|Outremont|Bordeaux"), 
#                                        district = "Montreal City")
# df_11_csd2 <- df_11_csd2 %>% mutate_cond(district == "Montreal City", 
#                                        subdistrict = "Montreal City")

#

df_11_csd <- df_11_csd %>% mutate_cond(str_detect(district,"Jacques Cartier|Hochelaga|Maisonneuve|Laval|Montreal") & TYPE_11 %in% c("C_PT","W","ND","W_PT"), 
                            district = "Montreal City") 


df_11_csd <- df_11_csd %>% mutate_cond(str_detect(subdistrict,"Maisonneuve|Westmount|Ile Ste Helene|Outremont|Bordeaux"), 
                                       district = "Montreal City")

df_21_csd <- df_21_csd %>% mutate_cond(str_detect(subdistrict,"Montreal|Westmount|Outremont") & TYPE_21 %in% c("C"), 
                                       district = "Montreal City")
df_21_csd <- df_21_csd %>% mutate_cond(str_detect(subdistrict,"Asile St Jean de Dieu"), 
                                       district = "Montreal City")

df_11_csd <- df_11_csd %>% mutate_cond(district == "Montreal City", 
                                       subdistrict = "Montreal City")
df_21_csd <- df_21_csd %>% mutate_cond(district == "Montreal City", 
                                       subdistrict = "Montreal City")

# Ottawa

df_11_csd <- df_11_csd %>% mutate_cond(str_detect(district,"Carleton|Ottawa|Russell") & TYPE_11 %in% c("C_PT","W","ND"),
                                         district = "Ottawa City") 
df_21_csd <- df_21_csd %>% mutate_cond(str_detect(subdistrict,"Ottawa"),
                                       district = "Ottawa City") 

df_11_csd <- df_11_csd %>% mutate_cond(district == "Ottawa City", 
                                       subdistrict = "Ottawa City")
df_21_csd <- df_21_csd %>% mutate_cond(district == "Ottawa City", 
                                       subdistrict = "Ottawa City")

# Quebec

df_11_csd <- df_11_csd %>% mutate_cond(str_detect(district,"Quebec") & TYPE_11 %in% c("C_PT","W","ND"),
                                         district = "Quebec City")
df_21_csd <- df_21_csd %>% mutate_cond(str_detect(subdistrict,"Quebec"),
                                       district = "Quebec City")

df_11_csd <- df_11_csd %>% mutate_cond(district == "Quebec City", 
                                       subdistrict = "Quebec City")
df_21_csd <- df_21_csd %>% mutate_cond(district == "Quebec City", 
                                       subdistrict = "Quebec City")

# Sherbrooke, not sure

# df_11_csd <- df_11_csd %>% mutate_cond(subdistrict %in% c("Sherbrooke","Ascot","Lennoxville") & TYPE_11 %in% c("C","W","V","VL"),
#                                          district = "Sherbrooke City") 
# 
# df_11_csd <- df_11_csd %>% mutate_cond(district == "Sherbrooke City", 
#                                        subdistrict = "Sherbrooke City")

# St John

df_11_csd <- df_11_csd %>% mutate_cond(str_detect(subdistrict,c("St John City")) & TYPE_11 %in% c("W"),
                                         district = "St John City") 
# df_11_csd <- df_11_csd %>% mutate_cond(str_detect(subdistrict,c("Lancaster")) & province == "New Brunswick",
#                                        district = "St John City")
df_21_csd <- df_21_csd %>% mutate_cond(str_detect(subdistrict,c("St John")),
                                       district = "St John City") 
# df_21_csd <- df_21_csd %>% mutate_cond(str_detect(subdistrict,c("Lancaster")) & province == "New Brunswick",
#                                        district = "St John City")

# df_11_csd$district[which(!grepl("St John",df_11_csd$subdistrict) & df_11_csd$district == "St John City & County")] <- "St John County"
# df_21_csd$district[which(!grepl("St John",df_21_csd$subdistrict) & df_11_csd$district == "St John")] <- "St John County"

df_11_csd <- df_11_csd %>% mutate_cond(district == "St John City", 
                                       subdistrict = "St John City")
df_21_csd <- df_21_csd %>% mutate_cond(district == "St John City", 
                                       subdistrict = "St John City")

# Trois Rivieres: Trois Rivieres, C. I think the proper one includes Shawningan Falls

df_11_csd <- df_11_csd %>% mutate_cond(str_detect(subdistrict,c("Trois Rivieres")),
                                         district = "Trois Rivieres City") 
df_21_csd <- df_21_csd %>% mutate_cond(str_detect(subdistrict,c("Trois Rivieres")) & TYPE_21 %in% c("C"),
                                       district = "Trois Rivieres City") 

df_11_csd <- df_11_csd %>% mutate_cond(district == "Trois Rivieres City", 
                                       subdistrict = "Trois Rivieres City")
df_21_csd <- df_21_csd %>% mutate_cond(district == "Trois Rivieres City", 
                                       subdistrict = "Trois Rivieres City")

# Vancouver. I think the proper one is without the New Westminster subdistricts but I still add them

df_11_csd <- df_11_csd %>% mutate_cond(str_detect(subdistrict,"Vancouver|Point Grey") & !(subdistrict == "North Vancouver"),
                                    district = "Vancouver City") 

df_11_csd$district[which(df_11_csd$subdistrict == "North Vancouver")] <- "North Vancouver"
df_21_csd <- df_21_csd %>% mutate_cond(str_detect(subdistrict,"Vancouver|Point Grey") & !(subdistrict == "North Vancouver"),
                                       district = "Vancouver City")

df_11_csd <- df_11_csd %>% mutate_cond(district == "Vancouver City", 
                                       subdistrict = "Vancouver City")
df_21_csd <- df_21_csd %>% mutate_cond(district == "Vancouver City", 
                                       subdistrict = "Vancouver City")

# Victoria: Victoria City

df_11_csd$subdistrict[which(df_11_csd$district == "Victoria City")] <- "Victoria City"
df_21_csd$subdistrict[which(df_21_csd$district == "Victoria City")] <- "Victoria City"

# Winnipeg

df_11_csd <- df_11_csd %>% mutate_cond(subdistrict %in% c("St Boniface","Winnipeg (part)") & province == "Manitoba" & URB_RUR_11 == "Urban",
                                    district = "Winnipeg City")
df_21_csd <- df_21_csd %>% mutate_cond(subdistrict %in% c("St Boniface","Winnipeg"),
                                       district = "Winnipeg City")

df_11_csd <- df_11_csd %>% mutate_cond(district == "Winnipeg City", 
                                       subdistrict = "Winnipeg City")
df_21_csd <- df_21_csd %>% mutate_cond(district == "Winnipeg City", 
                                       subdistrict = "Winnipeg City")

# Toronto: doesn't sum exactly but gets close

df_11_csd <- df_11_csd %>% mutate_cond(str_detect(district,"Toronto|York South") & (TYPE_11 %in% c("C_PT","W_PT","T") | str_detect(subdistrict,"Toronto")),
                                       district = "Toronto City") 
df_21_csd <- df_21_csd %>% mutate_cond(subdistrict == "Toronto",
                                       district = "Toronto City") 

df_21_csd <- df_21_csd %>% mutate_cond(district == "Toronto City", 
                                       subdistrict = "Toronto City")

# Windsor

df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Windsor" & df_11_csd$province == "Ontario")] <- "Windsor City"
df_11_csd$district[which(df_11_csd$subdistrict == "Windsor City")] <- "Windsor City"
df_21_csd$subdistrict[which(df_21_csd$subdistrict == "Windsor" & df_21_csd$province == "Ontario")] <- "Windsor City"
df_21_csd$district[which(df_21_csd$subdistrict == "Windsor City")] <- "Windsor City"


# London

df_11_csd$subdistrict[which(df_11_csd$subdistrict == "London" & df_11_csd$province == "Ontario")] <- "London City"
df_11_csd$district[which(df_11_csd$subdistrict == "London City")] <- "London City"
df_21_csd$subdistrict[which(df_21_csd$subdistrict == "London" & df_21_csd$province == "Ontario")] <- "London City"
df_21_csd$district[which(df_21_csd$subdistrict == "London City")] <- "London City"

# Kingston

df_11_csd$subdistrict[which((df_11_csd$subdistrict == "Kingston" & df_11_csd$province == "Ontario" & df_11_csd$TYPE_11 == "C") | (df_11_csd$subdistrict == "Portsmouth" & df_11_csd$province == "Ontario"))] <- "Kingston City"
df_11_csd$district[which(df_11_csd$subdistrict == "Kingston City")] <- "Kingston City"
df_21_csd$subdistrict[which((df_21_csd$subdistrict == "Kingston" & df_21_csd$province == "Ontario" & df_21_csd$TYPE_21 == "C") | (df_21_csd$subdistrict == "Portsmouth" & df_21_csd$province == "Ontario"))] <- "Kingston City"
df_21_csd$district[which(df_21_csd$subdistrict == "Kingston City")] <- "Kingston City"

# estimate population for 51-21 at the subdistrict levels of 1921

# eliminate Newfoundland. Could add NL population at the district level using its Census. Only adding total population for NL
# is not nice because if a bank has one branch at St John's then in the model it reaches much more people than in reality.

df_51_cd <- df_51_cd %>% filter(!(province %in% "Newfoundland"))
df_61_cd <- df_61_cd %>% filter(!(province %in% "Newfoundland"))
df_71_cd <- df_71_cd %>% filter(!(province %in% "Newfoundland"))
df_81_cd <- df_81_cd %>% filter(!(province %in% "Newfoundland"))
df_91_cd <- df_91_cd %>% filter(!(province %in% "Newfoundland"))
df_01_cd <- df_01_cd %>% filter(!(province %in% "Newfoundland"))
df_11_csd <- df_11_csd %>% filter(!(province %in% "Newfoundland"))
df_11_cd <- df_11_cd %>% filter(!(province %in% "Newfoundland"))
df_21_csd <- df_21_csd %>% filter(!(province %in% "Newfoundland"))
df_21_cd <- df_21_cd %>% filter(!(province %in% "Newfoundland"))

# aggregate before changing more and use it for manuf_11 and country_11 district data

q6 <- df_11_csd
q6_aux <- q6 %>% select(c("district","province","geometry","area","population"))
q6_aux2 <-as.data.frame(q6) %>% select(-c("geometry","area","population"))

df_11_csd_aux <- aggregate(q6_aux[,-which(colnames(q6_aux) %in% c("district","province"))], by =list("district" = q6_aux$district, "province" = q6_aux$province), FUN = function(x) sum(x,na.rm = T), do_union = T)
df_11_csd_aux2 <- aggregate(q6_aux2[,-which(colnames(q6_aux2) %in% c("district","province"))], by =list("district" = q6_aux2$district, "province" = q6_aux2$province), FUN = function(x) extract2(x,1))
df_11_cdcd <- left_join(df_11_csd_aux,df_11_csd_aux2, by = c("district","province"))

distr <- c("Vancouver City","Victoria City","Winnipeg City","St John City","Halifax City","Ottawa City",
           "Toronto City","Montreal City","Quebec City","Trois Rivieres City",
           "Hamilton City","London City","Kingston City","Windsor City")
# "Sherbrooke City"
# "Regina City","Saskatoon City","Calgary City","Edmonton City"
df_11_cdcd <- df_11_cdcd %>% mutate_cond(district %in% distr,UID_CD_11 = paste0(UID_CD_11,"c"))

df_11_cdcd <- mutate(df_11_cdcd, area = st_area(geometry))
ref_11_cdcd <- as.data.frame(df_11_cdcd) %>% select(-c("geometry","pop_01"))
df_11_cdcd <- df_11_cdcd %>% select(-UID_CSD_11,-CSD_NO_11,-subdistrict,-TYPE_11,-URB_RUR_11)
ref_11_cdcd <- ref_11_cdcd %>% select(-UID_CSD_11,-CSD_NO_11,-subdistrict,-TYPE_11,-URB_RUR_11)

# substitute mid Canada geoegraphy and population for 1881 and 1891 with better Census data from published tables

source("./Code/change_polygons.R")

# substitue mid Canada cd in 1911 using the ones in 1921

# source("./Code/change_polygons_11.R")

#

# df_11_csd <- df_11_csd %>% select(-pop_01) # If you want to go back to using df_11_csd pop_01 column comment this line
# df_11_csd$pop_01 <- NA
df_21_csd$pop_51 <- NA
df_21_csd$pop_61 <- NA
df_21_csd$pop_71 <- NA
df_21_csd$pop_81 <- NA
df_21_csd$pop_91 <- NA
df_21_csd$pop_01 <- NA
df_21_csd$pop_11 <- NA

df_21_csd <- df_21_csd %>% mutate_cond(district %in% distr , UID_CSD_21 = paste0(UID_CSD_21,"c")) # with these you still have same district-subistrict but different id's. Not big deal since later I extract 1 id when I unite geography.
df_21_csd <- df_21_csd %>% mutate_cond(district %in% distr , UID_CD_21 = paste0(UID_CD_21,"c"))

# fill blanks in pop_01 with df_11_csd
int <- st_intersects(df_11_csd,df_01_cd, sparse = F)
ref_11_csd_aux <- as.data.frame(df_11_csd) %>% select(-geometry)

for (i in 1:NROW(df_11_csd)){
  
  if (str_detect(ref_11_csd_aux[i,"province"],"West|Yuk") | ref_11_csd_aux[i,"UID_CD_11"] %in% c("NS048","QC20N") | ref_11_csd_aux[i,"district"] %in% c("Nanaimo","Yale & Cariboo","North Vancouver","Kootenay")){
    aux <- 0
    for (j in which(int[i,])){
      
      aux <- aux + (ref_11_csd_aux[i,"population"]/sum(ref_11_csd_aux[which(int[,j]),"population"], na.rm = T))*df_01_cd$population[j]
      
    }
    if (length(which(int[i,])) == 0){
      
      j <- which.min(st_distance(df_11_csd$geometry[i],df_01_cd))
      int[i,j] <- TRUE
      aux <- (ref_11_csd_aux$population[i]/sum(ref_11_csd_aux$population[which(int[,j])], na.rm = T))*df_01_cd$population[j]
    }
    df_11_csd[i, which(colnames(df_11_csd) == "pop_01")] <- aux
  }
  
}

df_01_csd <- df_11_csd %>% select(-population) # separate pop_01 into a new df
df_01_csd <-  rename(df_01_csd,"population" = "pop_01")
df_01_csd$population[which(is.na(df_01_csd$population))] <- 0
df_11_csd <- df_11_csd %>% select(-pop_01)

# l <- list(df_51_cd,df_61_cd,df_71_cd,df_81_cd,df_91_cd,df_01_cd,df_21_csd) # I'm using 21 data at district level because at subdistrict level population doesn't transfer fully to 11 subdistricts
l <- list(df_51_cd,df_61_cd,df_71_cd,df_81_cd,df_91_cd,df_01_csd,df_11_csd) # I'm using 21 data at district level because at subdistrict level population doesn't transfer fully to 11 subdistricts
# d <- c("51","61","71","81","91","01","21")
d <- c("21","11","01","91","81","71","61","51")


# start
d2 <- 1
for (df in l[length(l):1]){
  d2 <- d2 + 1
  # if (d2 == 2){
  #   d2 <- d2 + 1
  # }
  print(d[d2])
  
  
  distr_df <- as.data.frame(df) %>% select(district) %>% filter(district %in% distr)
  distr_df <- distr_df$district
  df_aux <- df %>% filter(!(district %in% distr))
  df_21_csd_aux <- df_21_csd %>% filter(!(district %in% distr_df))  
  
  
  int <- st_intersects(df_21_csd_aux,df_aux, sparse = F)
  out <- rowSums(int,na.rm = T)
  out <- which(out == 0)
  df_21_csd_auxx <- as.data.frame(df_21_csd_aux) %>% select(-geometry)   
  
  for (i in out){
    
    j <- which.min(st_distance(df_21_csd_aux$geometry[i],df_aux))
    int[i,j] <- TRUE
    
  }
  
  for (i in 1:NROW(df_21_csd_aux)){
    
    aux <- 0
    
    for (j in which(int[i,])){
    
      if (d[d2] == "11"){
        aux <- aux + (df_21_csd_auxx[i, which(colnames(df_21_csd_auxx) == "population")]/sum(df_21_csd_auxx[which(int[,j]), which(colnames(df_21_csd_auxx) == "population" )], na.rm = T))*df_aux$population[j]
      } else{
        aux <- aux + (df_21_csd_auxx[i, which(colnames(df_21_csd_auxx) == paste0("pop_",d[d2-1]))]/sum(df_21_csd_auxx[which(int[,j]), which(colnames(df_21_csd_auxx) == paste0("pop_",d[d2-1]) )], na.rm = T))*df_aux$population[j] 
      }
    }
    
    df_21_csd_aux[i, which(colnames(df_21_csd_aux) == paste0("pop_",d[d2]) )] <- aux 
  }
  
  
  df_aux2 <- as.data.frame(df) %>% filter(district %in% distr) %>% select(-geometry)
  df_21_csd_aux2 <- df_21_csd %>% filter(district %in% distr_df)
  df_21_csd_auxx2 <- as.data.frame(df_21_csd_aux2) %>% select(-geometry)
  
  for (h in as.list(unique(df_21_csd_aux2$district))){
    
    if (d[d2] == "11"){
      df_21_csd_aux2[which(df_21_csd_aux2$district == h),
                     which(colnames(df_21_csd_aux2) == paste0("pop_",d[d2]) )] <- (df_21_csd_auxx2[
                       which(df_21_csd_auxx2$district == h), which(colnames(df_21_csd_auxx2) == "population")
                       ]/sum(df_21_csd_auxx2[
                         which(df_21_csd_auxx2$district == h), which(colnames(df_21_csd_auxx2) == "population")]      
                         , na.rm = T))*sum(df_aux2$population[which(df_aux2$district == h)], na.rm = T)   
    } else{
      
      df_21_csd_aux2[which(df_21_csd_aux2$district == h),
                     which(colnames(df_21_csd_aux2) == paste0("pop_",d[d2]) )] <- (df_21_csd_auxx2[
                       which(df_21_csd_auxx2$district == h), which(colnames(df_21_csd_auxx2) == paste0("pop_",d[d2-1]))
                       ]/sum(df_21_csd_auxx2[
                         which(df_21_csd_auxx2$district == h), which(colnames(df_21_csd_auxx2) == paste0("pop_",d[d2-1]))]      
                         , na.rm = T))*sum(df_aux2$population[which(df_aux2$district == h)], na.rm = T)  
      
    }
    
    
  }
  
  df_21_csd <- rbind(df_21_csd_aux,df_21_csd_aux2)
  
  
}

df_21_csd$pop_11[is.nan(df_21_csd$pop_11)] <- 0
df_21_csd$pop_01[is.nan(df_21_csd$pop_01)] <- 0
df_21_csd$pop_91[is.nan(df_21_csd$pop_91)] <- 0
df_21_csd$pop_81[is.nan(df_21_csd$pop_81)] <- 0
df_21_csd$pop_71[is.nan(df_21_csd$pop_71)] <- 0
df_21_csd$pop_61[is.nan(df_21_csd$pop_61)] <- 0
df_21_csd$pop_51[is.nan(df_21_csd$pop_51)] <- 0
df_21_csd <- df_21_csd %>% mutate(pop_avg = rowMeans(select(as.data.frame(df_21_csd),pop_11,pop_01), na.rm = TRUE))

