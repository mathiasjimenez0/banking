# #####
# rm(list = ls())
# 
# change = 0
# 
# if (change == 0){
#   
#   # Census
#   
#   pop_11 <- read_excel("Dataset/census/pop_1901-1911_csd.xls")
#   pop_21 <- read_excel("Dataset/census/pop_1921_csd.xls")
#   
#   # filename <- "./Dataset/census/Population_by_CDs_1851_1961.xls"
#   # pop_21_cd <- read_excel(filename, sheet =  "1921")
#   
#   country_01 <- read_excel("Dataset/census/NatualProducts_1901_excel.xlsx",
#                            sheet = "Sheet1")
#   country_11 <- read_excel("Dataset/census/Agric_1911_excel.xlsx",
#                            sheet = "Sheet1")
#   
#   manuf <- read_excel("Dataset/census/Manu_1911.xlsx",
#                       sheet = "Sheet1")
#   
#   manuf_11 <- read_excel("Dataset/census/Manu_1911.xlsx",
#                          sheet = "Sheet2")
#   
#   manuf_01 <- read_excel("Dataset/census/Jaworski/1901_raw.xlsx",
#                          sheet = "Sheet1", skip = 1)
#   manuf_91 <- read_excel("Dataset/census/Jaworski/1891_raw.xls",
#                          sheet = "Sheet1", skip = 2)
#   
#   file.list <- list.files("./Dataset/census/Jaworski/1881_raw/", pattern = "*.xls", full.names = T)
#   df.list_81 <- lapply(file.list,function(x) read_excel(x,skip = 4))
#   df_end_81 <- read_excel("./Dataset/census/Jaworski/1881_raw/end/Pg160-174_LIV.xls", skip = 2)
#   
#   file.list <- list.files("./Dataset/census/Jaworski/1871_raw/", pattern = "*.xls", full.names = T)
#   df.list_71 <- lapply(file.list,function(x) read_excel(x,skip = 4))
#   df_end_71 <- read_excel("./Dataset/census/Jaworski/1871_raw/end/Pg148-157_LIII.xls", skip = 2)
#   
#   source("./Code/Clean_Census.R")
#   
#   # interest_rates
#   
#   icity <- read_excel("Dataset/interest_rates/interest_rates.xlsx",
#                       sheet = 1)
#   
#   ifarm <- read_excel("Dataset/interest_rates/interest_rates.xlsx",
#                       sheet = 2)
#   
#   iavg <- read_excel("Dataset/interest_rates/interest_rates.xlsx",
#                      sheet = 3)
#   
#   imuni <- read_excel("./Dataset/interest_rates/interest_rates.xlsx",
#                       sheet = 4)
#   
#   source("./Code/Clean_Rates.R")  
#   
# }
# 
# # GIS
# 
# 
# if (change == 1){
#   branches <- read_xlsx("./Dataset/gis/branches.xlsx", sheet = "change", col_types = c("text", "text", "text", "text", "numeric","text","numeric")) 
#   load("./Dataset/R/branches_change.RData")
# } else{
#   df_51_cd <- st_read("./Dataset/gis/OpenContent_HACOLP_1851_CD_Canada/OpenContent_HACOLP_1851_CD_Canada.shp", stringsAsFactors = F)
#   df_61_cd <- st_read("./Dataset/gis/OpenContent_HACOLP_1861_CD_Canada/OpenContent_HACOLP_1861_CD_Canada.shp", stringsAsFactors = F)
#   df_71_cd <- st_read("./Dataset/gis/OpenContent_HACOLP_1871_CD_Canada/data/HACOLP_cds_1871.shp", stringsAsFactors = F)
#   df_81_cd <- st_read("./Dataset/gis/OpenContent_HACOLP_1881_CD_Canada/data/HACOLP_cds_1881.shp", stringsAsFactors = F)
#   df_91_cd <- st_read("./Dataset/gis/OpenContent_HACOLP_1891_CD_Canada/data/HACOLP_cds_1891.shp", stringsAsFactors = F)
#   df_01_cd <- st_read("./Dataset/gis/OpenContent_HACOLP_1901_CD_Canada/data/HACOLP_cds_1901.shp", stringsAsFactors = F)
#   df_11_csd <- st_read("./Dataset/gis/CCRI_CANADA_CENSUS_SUB_DIVISIONS_1911/Shapefile/CCRI_Shapefiles_CSD_1911/CANADA_CSD_1911_MW.shp", stringsAsFactors = F)
#   df_11_cd <- st_read("./Dataset/gis/CCRI_CANADA_CENSUS_DIVISIONS_1911/Shapefile/CCRI_Shapefiles_CD_1911/CANADA_CD_1911_MW.shp", stringsAsFactors = F)
#   df_21_cd <- st_read("./Dataset/gis/CCRI_CANADA_CENSUS_DIVISIONS_1921/Shapefile/CCRI_Shapefiles_CD_1921/CANADA_CD_1921_MW.shp", stringsAsFactors = F)
#   df_21_csd <- st_read("./Dataset/gis/CCRI_CANADA_CENSUS_SUB_DIVISIONS_1921/Shapefile/CCRI_Shapefiles_CSD_1921/CANADA_CSD_1921_MW.shp", stringsAsFactors = F)
#   
#   branches <- read_xlsx("./Dataset/gis/branches.xlsx", sheet = "branches", col_types = c("text", "text", "text", "text", "numeric"))  
# }
# 
# edges <- read_xlsx("./Dataset/gis/edges.xlsx", sheet = 1)
# CC <- read_excel("Dataset/misc/CC.xlsx")
# source("./Code/Clean_branches_edges.R")
# source("./Code/Clean_GIS.R") 
# 
# # Modify cities in 1911/1901 so they are consistent with ones before that
# 
# # # aux was created to look for cities in maps before 1901
# # ref_early_cd <- rbind(ref_51_cd,ref_61_cd,ref_71_cd,ref_81_cd,ref_91_cd)
# # aux <- ref_early_cd %>% filter(str_detect(district,
# #"Vancouver|Victoria|Winnipeg|John|Halifax|Ottawa|Toronto|Montreal|Quebec|Sherbrooke|Trois Rivieres|Hamilton|Regina|Saskatoon|Calgary|Edmonton"))
# 
# # Halifax: Halifax, ward 1-6
# 
# df_11_csd <- df_11_csd %>% mutate_cond(str_detect(district,"Halifax") & TYPE_11 %in% c("W"),
#                                        district = "Halifax City") 
# 
# df_11_csd$district[which(!grepl("Halifax",df_11_csd$subdistrict) & df_11_csd$district == "Halifax City & County")] <- "Halifax County"
# 
# df_11_csd <- df_11_csd %>% mutate_cond(district == "Halifax City", 
#                                        subdistrict = "Halifax City")
# 
# # Hamilton: Hamiltron East, Hamilton West, Wentworth, Barton
# 
# df_11_csd <- df_11_csd %>% mutate_cond(subdistrict %in% c("Hamilton (part)","Barton") & TYPE_11 %in% c("C_PT","TP"), 
#                                        district = "Hamilton City")
# 
# df_11_csd <- df_11_csd %>% mutate_cond(district == "Hamilton City", 
#                                        subdistrict = "Hamilton City")
# # Montreal 
# 
# df_11_csd <- df_11_csd %>% mutate_cond(str_detect(district,"Jacques Cartier|Hochelaga|Maisonneuve|Laval|Montreal") & TYPE_11 %in% c("C_PT","W","ND","W_PT"), 
#                                        district = "Montreal City") 
# df_11_csd <- df_11_csd %>% mutate_cond(str_detect(subdistrict,"Maisonneuve|Westmount"), 
#                                        district = "Montreal City") 
# 
# df_11_csd <- df_11_csd %>% mutate_cond(district == "Montreal City", 
#                                        subdistrict = "Montreal City")
# 
# # Ottawa
# 
# df_11_csd <- df_11_csd %>% mutate_cond(str_detect(district,"Carleton|Ottawa|Russell") & TYPE_11 %in% c("C_PT","W","ND"),
#                                        district = "Ottawa City") 
# 
# df_11_csd <- df_11_csd %>% mutate_cond(district == "Ottawa City", 
#                                        subdistrict = "Ottawa City")
# 
# # Quebec
# 
# df_11_csd <- df_11_csd %>% mutate_cond(str_detect(district,"Quebec") & TYPE_11 %in% c("C_PT","W","ND"),
#                                        district = "Quebec City") 
# 
# df_11_csd <- df_11_csd %>% mutate_cond(district == "Quebec City", 
#                                        subdistrict = "Quebec City")
# 
# # Sherbrooke, not sure
# 
# # df_11_csd <- df_11_csd %>% mutate_cond(subdistrict %in% c("Sherbrooke","Ascot","Lennoxville") & TYPE_11 %in% c("C","W","V","VL"),
# #                                          district = "Sherbrooke City") 
# # 
# # df_11_csd <- df_11_csd %>% mutate_cond(district == "Sherbrooke City", 
# #                                        subdistrict = "Sherbrooke City")
# 
# # St John
# 
# df_11_csd <- df_11_csd %>% mutate_cond(str_detect(subdistrict,c("St John City")) & TYPE_11 %in% c("W"),
#                                        district = "St John City") 
# df_11_csd <- df_11_csd %>% mutate_cond(str_detect(subdistrict,c("Lancaster")) & province == "New Brunswick",
#                                        district = "St John City") 
# 
# df_11_csd$district[which(!grepl("St John",df_11_csd$subdistrict) & df_11_csd$district == "St John City & County")] <- "St John County"
# 
# df_11_csd <- df_11_csd %>% mutate_cond(district == "St John City", 
#                                        subdistrict = "St John City")
# 
# # Trois Rivieres: Trois Rivieres, C. I think the proper one includes Shawningan Falls
# 
# df_11_csd <- df_11_csd %>% mutate_cond(str_detect(subdistrict,c("Trois Rivieres")),
#                                        district = "Trois Rivieres City") 
# 
# df_11_csd <- df_11_csd %>% mutate_cond(district == "Trois Rivieres City", 
#                                        subdistrict = "Trois Rivieres City")
# 
# # Vancouver. I think the proper one is without the New Westminster subdistricts but I still add them
# 
# df_11_csd <- df_11_csd %>% mutate_cond(str_detect(subdistrict,"Vancouver|Point Grey") & !(subdistrict == "North Vancouver"),
#                                        district = "Vancouver City") 
# 
# df_11_csd$district[which(df_11_csd$subdistrict == "North Vancouver")] <- "North Vancouver"
# 
# df_11_csd <- df_11_csd %>% mutate_cond(district == "Vancouver City", 
#                                        subdistrict = "Vancouver City")
# 
# # Victoria: Victoria City
# 
# df_11_csd$subdistrict[which(df_11_csd$district == "Victoria City")] <- "Victoria City"
# 
# df_11_csd <- df_11_csd %>% mutate_cond(district == "Victoria City", 
#                                        subdistrict = "Victoria City")
# 
# # Winnipeg
# 
# df_11_csd <- df_11_csd %>% mutate_cond(subdistrict %in% c("St Boniface","Selkirk","Winnipeg (part)") & province == "Manitoba" & URB_RUR_11 == "Urban",
#                                        district = "Winnipeg City") 
# 
# df_11_csd <- df_11_csd %>% mutate_cond(district == "Winnipeg City", 
#                                        subdistrict = "Winnipeg City")
# 
# # Toronto: doesn't sum exactly but gets close
# 
# df_11_csd <- df_11_csd %>% mutate_cond(str_detect(district,"Toronto|York South") & (TYPE_11 %in% c("C_PT","W_PT") | str_detect(subdistrict,"Toronto")),
#                                        district = "Toronto City") 
# 
# df_11_csd <- df_11_csd %>% mutate_cond(district == "Toronto City", 
#                                        subdistrict = "Toronto City")
# 
# # Windsor
# 
# df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Windsor" & df_11_csd$province == "Ontario")] <- "Windsor City"
# df_11_csd$district[which(df_11_csd$subdistrict == "Windsor City")] <- "Windsor City"
# 
# # df_11_csd <- df_11_csd %>% mutate_cond(district == "Windsor City", 
# #                                        subdistrict = "Windsor City")
# 
# 
# # London
# 
# df_11_csd$subdistrict[which(df_11_csd$subdistrict == "London" & df_11_csd$province == "Ontario")] <- "London City"
# df_11_csd$district[which(df_11_csd$subdistrict == "London City")] <- "London City"
# 
# # Kingston
# 
# df_11_csd$subdistrict[which((df_11_csd$subdistrict == "Kingston" & df_11_csd$province == "Ontario" & df_11_csd$TYPE_11 == "C") | (df_11_csd$subdistrict == "Portsmouth" & df_11_csd$province == "Ontario"))] <- "Kingston City"
# df_11_csd$district[which(df_11_csd$subdistrict == "Kingston City")] <- "Kingston City"
# 
# # estimate population for 51-21 at the subdistrict levels of 1911
# 
# # eliminate Newfoundland. Could add NL population at the district level using its Census. Only adding total population for NL
# # is not nice because if a bank has one branch at St John's then in the model it reaches much more people than in reality.
# 
# df_51_cd <- df_51_cd %>% filter(!(province %in% "Newfoundland"))
# df_61_cd <- df_61_cd %>% filter(!(province %in% "Newfoundland"))
# df_71_cd <- df_71_cd %>% filter(!(province %in% "Newfoundland"))
# df_81_cd <- df_81_cd %>% filter(!(province %in% "Newfoundland"))
# df_91_cd <- df_91_cd %>% filter(!(province %in% "Newfoundland"))
# df_01_cd <- df_01_cd %>% filter(!(province %in% "Newfoundland"))
# df_11_csd <- df_11_csd %>% filter(!(province %in% "Newfoundland"))
# df_11_cd <- df_11_cd %>% filter(!(province %in% "Newfoundland"))
# df_21_csd <- df_21_csd %>% filter(!(province %in% "Newfoundland"))
# df_21_cd <- df_21_cd %>% filter(!(province %in% "Newfoundland"))
# 
# 
# 
# 
# 
# 
# 
# 
# # 
# #####


# Winnipeg

aux <- df_21_csd$UID_CSD_21[which(df_21_csd$subdistrict == "Winnipeg" & df_21_csd$province == "Manitoba" &  df_21_csd$TYPE_21 == "C")]
aux2 <- df_21_csd$CD_NO_21[which(df_21_csd$subdistrict == "Winnipeg" & df_21_csd$province == "Manitoba" &  df_21_csd$TYPE_21 == "C")]
aux3 <- df_21_csd$CSD_NO_21[which(df_21_csd$subdistrict == "Winnipeg" & df_21_csd$province == "Manitoba" &  df_21_csd$TYPE_21 == "C")]

df_21_csd <- df_21_csd %>% mutate_cond(subdistrict %in% c("St Boniface") & province == "Manitoba" & TYPE_21 == "C", UID_CSD_21 = aux)
df_21_csd <- df_21_csd %>% mutate_cond(subdistrict %in% c("St Boniface") & province == "Manitoba" & TYPE_21 == "C", CD_NO_21 = aux2)
df_21_csd <- df_21_csd %>% mutate_cond(subdistrict %in% c("St Boniface") & province == "Manitoba" & TYPE_21 == "C", CSD_NO_21 = aux3)

df_21_csd <- df_21_csd %>% mutate_cond(subdistrict %in% c("St Boniface","Winnipeg") & province == "Manitoba" & TYPE_21 == "C",
                                       district = "Winnipeg City") 

df_21_csd <- df_21_csd %>% mutate_cond(district == "Winnipeg City", 
                                       subdistrict = "Winnipeg City")

df_21_csd$UID_CD_21[which(df_21_csd$subdistrict == "Winnipeg City")] <- paste0(df_21_csd$UID_CD_21[which(df_21_csd$subdistrict == "Winnipeg City")],"c")
df_21_csd$UID_CSD_21[which(df_21_csd$subdistrict == "Winnipeg City")] <- paste0(df_21_csd$UID_CSD_21[which(df_21_csd$subdistrict == "Winnipeg City")],"c")
ref_21_csd <- as.data.frame(df_21_csd) %>% select(-c("geometry"))

df_21_mid <- df_21_csd %>% filter(province %in% c("Alberta","Saskatchewan","Manitoba"))

q6 <- df_21_mid
q6_aux <- q6 %>% select(c("UID_CD_21","geometry","area","population"))
q6_aux2 <-as.data.frame(q6) %>% select(-c("geometry","area","population"))

df_21_aux <- aggregate(q6_aux[,-which(colnames(q6_aux) %in% c("UID_CD_21"))], by =list("UID_CD_21" = q6_aux$UID_CD_21), FUN = function(x) sum(x,na.rm = T), do_union = T)
df_21_aux2 <- aggregate(q6_aux2[,-which(colnames(q6_aux2) %in% c("UID_CD_21"))], by =list("UID_CD_21" = q6_aux2$UID_CD_21), FUN = function(x) extract2(x,1))
df_21_mid <- left_join(df_21_aux,df_21_aux2, by = c("UID_CD_21"))
df_21_mid <- df_21_mid %>% select(-population,-UID_CSD_21,-CSD_NO_21,-TYPE_21,-URB_RUR_21,-subdistrict)
df_21_mid <- rename(df_21_mid, c("UID_CD_11" = "UID_CD_21","CD_NO_11" = "CD_NO_21"))
ref_21_mid <- as.data.frame(df_21_mid) %>% select(-c("geometry"))





# df_11_mid <- df_11_csd %>% filter(province %in% c("Alberta","Saskatchewan","Manitoba"))
# df_21_mid$pop_11 <- NA
# 
# distr <- "Winnipeg City"
# distr_df <- as.data.frame(df_11_mid) %>% select(district) %>% filter(district %in% distr)
# distr_df <- unique(distr_df$district)
# df_aux <- df_11_mid %>% filter(!(district %in% distr))
# df_21_aux <- df_21_mid %>% filter(!(district %in% distr_df))
# 
# 
# int <- st_intersects(df_21_aux,df_aux, sparse = F)
# 
# for (i in 1:NROW(df_21_aux)){
#   
#   aux <- 0
# 
#   df_21_auxx <- as.data.frame(df_21_aux) %>% select(-geometry)      
#   
#   for (j in which(int[i,])){
#     
#     if (i == min(which(int[,j]))){
#       aux <- aux + (df_21_auxx[i, which(colnames(df_21_auxx) == "population")]/sum(df_21_auxx[which(int[,j]), which(colnames(df_21_auxx) == "population")], na.rm = T))*df_aux$population[j]  
#     }
#   }
# 
# df_21_aux[i, which(colnames(df_21_aux) == "pop_11" )] <- aux
# 
# }
# 
# 
# df_aux2 <- as.data.frame(df_11_mid) %>% filter(district %in% distr) %>% select(-geometry)
# df_21_aux2 <- df_21_mid %>% filter(district %in% distr_df)
# df_21_auxx2 <- as.data.frame(df_21_aux2) %>% select(-geometry)
# 
# for (h in as.list(unique(df_21_aux2$district))){
#   
#   df_21_aux2[which(df_21_aux2$district == h),
#                  which(colnames(df_21_aux2) == "pop_11" )] <- (df_21_auxx2[
#                    which(df_21_auxx2$district == h), which(colnames(df_21_auxx2) == "population")
#                    ]/sum(df_21_auxx2[
#                      which(df_21_auxx2$district == h), which(colnames(df_21_auxx2) == "population")]      
#                      , na.rm = T))*sum(df_aux2$population[which(df_aux2$district == h)],na.rm = T) 
#   
# }
# 
# df_21_mid <- rbind(df_21_aux,df_21_aux2)
# df_21_mid <- df_21_mid %>% select(-population,-UID_CSD_21,-CSD_NO_21,-TYPE_21,-URB_RUR_21,-subdistrict)
# df_21_mid <- rename(df_21_mid, "population" = "pop_11")
# ref_21_mid <- as.data.frame(df_21_mid) %>% select(-c("geometry"))





df_11_mid <- df_11_csd %>% filter(province %in% c("Alberta","Saskatchewan","Manitoba"))

df_11_mid$UID_CSD_11 <- NA
df_11_mid$UID_CD_11 <- NA
df_11_mid$CD_NO_11 <- NA
df_11_mid$CSD_NO_11 <- NA
df_11_mid$district <- NA

id <- st_intersects(df_11_mid, df_21_mid, sparse = TRUE, prepared = TRUE)
for (i in 1:length(id)){
  
  if (df_11_mid$subdistrict[i] == "Winnipeg City"){
    
    df_11_mid$UID_CD_11[i] <- df_21_mid$UID_CD_11[which(df_21_mid$district == "Winnipeg City")]
    df_11_mid$district[i] <- df_21_mid$district[which(df_21_mid$district == "Winnipeg City")]
    df_11_mid$province[i] <- df_21_mid$province[which(df_21_mid$district == "Winnipeg City")]
    
  } else {
    
  
  if (length(id[[i]]) == 1){
    
    df_11_mid$UID_CD_11[i] <- df_21_mid$UID_CD_11[id[[i]]]
    df_11_mid$district[i] <- df_21_mid$district[id[[i]]]
    df_11_mid$province[i] <- df_21_mid$province[id[[i]]]
    
  } else if (length(id[[i]]) > 1){
    
    id[[i]] <- min(id[[i]])
    
    df_11_mid$UID_CD_11[i] <- df_21_mid$UID_CD_11[id[[i]]]
    df_11_mid$district[i] <- df_21_mid$district[id[[i]]]
    df_11_mid$province[i] <- df_21_mid$province[id[[i]]]
    
  }
 }
}

df_11_mid$subdistrict[which(df_11_mid$subdistrict == "011_02_E1")] <- "Winnipeg City"

df_11_mid$UID_CSD_11 <- df_11_mid$UID_CD_11
df_11_mid$UID_CSD_11 <- gsub("c","",df_11_mid$UID_CSD_11)
  
df_11_mid <- df_11_mid %>% group_by(UID_CD_11) %>% mutate(CSD_NO_11 = sprintf("%03d", row_number()))
df_11_mid <- df_11_mid %>% mutate_cond(str_detect(df_11_mid$UID_CD_11,"c"), CSD_NO_11 = paste0(CSD_NO_11,"c"))
df_11_mid$CD_NO_11 <- gsub("[^0-9.]", "",df_11_mid$UID_CD_11)  
df_11_mid$UID_CSD_11 <- paste0(df_11_mid$UID_CSD_11,df_11_mid$CSD_NO_11)
df_11_mid <- st_as_sf(df_11_mid, crs = 32610)
# df_11_mid$UID_CSD_11[which(df_11_mid$subdistrict == "Winnipeg City")] <- paste0("MB160",df_11_mid$CSD_NO_11[which(df_11_mid$subdistrict == "Winnipeg City")],"c")

df_11_csd <- df_11_csd %>% filter(!(province %in% c("Alberta","Saskatchewan","Manitoba")))
df_11_csd <- rbind(df_11_csd,df_11_mid)
ref_11_csd <- as.data.frame(df_11_csd) %>% select(-c("geometry"))
