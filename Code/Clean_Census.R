############

# population 1921

############

colnames(pop_21) <- c("UID_CSD_21","UID_CD_21","province","district","subdistrict","type","area","population")
pop_21 <- pop_21 %>%
  filter(!(str_detect(UID_CSD_21,"000000")))

pop_21 <- pop_21 %>% mutate("UID_CD_21" = paste0(province,UID_CD_21))
pop_21 <- pop_21 %>% group_by(UID_CD_21) %>% mutate("district" = subdistrict[which.max(population)])


pop_21$subdistrict <- stri_trans_general(pop_21$subdistrict ,"Latin-ASCII")
pop_21$province <- stri_trans_general(pop_21$province ,"Latin-ASCII")
pop_21$district <- stri_trans_general(pop_21$district ,"Latin-ASCII")
pop_21$subdistrict <- gsub(".", "", pop_21$subdistrict,fixed = T)
pop_21$subdistrict <- gsub("-", " ", pop_21$subdistrict,fixed = T)
pop_21$subdistrict <- trimws(pop_21$subdistrict, "r")
pop_21$district <- gsub(".", "", pop_21$district,fixed = T)
pop_21$district <- gsub("-", " ", pop_21$district,fixed = T)
pop_21$district <- trimws(pop_21$district, "r")
pop_21$subdistrict <- gsub("(^\\s+)|(\\s+$)", "", pop_21$subdistrict,fixed = T)
pop_21$district <- gsub("(^\\s+)|(\\s+$)", "", pop_21$district,fixed = T)
pop_21$province <- as.character(pop_21$province)


pop_21$district <- gsub("-", " ", pop_21$district,fixed = T)
pop_21$district <- gsub(".", "", pop_21$district,fixed = T)
pop_21$district <- gsub(" E$", " East", pop_21$district)
pop_21$district <- gsub(" Est$", " East", pop_21$district)
pop_21$district <- gsub(" S$", " South", pop_21$district)
pop_21$district <- gsub(" N$", " North", pop_21$district)
pop_21$district <- gsub(" W O$", " West", pop_21$district)
pop_21$district <- gsub(" C$"," City",pop_21$district)

pop_21$district <- gsub(" E ", " East ", pop_21$district,fixed = T)
pop_21$district <- gsub(" S ", " South ", pop_21$district,fixed = T)
pop_21$district <- gsub(" N ", " North ", pop_21$district,fixed = T)
pop_21$district <- gsub(" W O ", " West ", pop_21$district,fixed = T)

pop_21$subdistrict <- gsub("-", " ", pop_21$subdistrict,fixed = T)
pop_21$subdistrict <- gsub(".", "", pop_21$subdistrict,fixed = T)
pop_21$subdistrict <- gsub(" E$", " East", pop_21$subdistrict)
pop_21$subdistrict <- gsub(" Est$", " East", pop_21$subdistrict)
pop_21$subdistrict <- gsub(" S$", " South", pop_21$subdistrict)
pop_21$subdistrict <- gsub(" N$", " North", pop_21$subdistrict)
pop_21$subdistrict <- gsub(" W O$", " West", pop_21$subdistrict)
pop_21$subdistrict <- gsub(" C$"," City",pop_21$subdistrict)


pop_21$subdistrict <- gsub(" E ", " East ", pop_21$subdistrict,fixed = T)
pop_21$subdistrict <- gsub(" S ", " South ", pop_21$subdistrict,fixed = T)
pop_21$subdistrict <- gsub(" N ", " North ", pop_21$subdistrict,fixed = T)
pop_21$subdistrict <- gsub(" W O ", " West ", pop_21$subdistrict,fixed = T)


pop_21$province[which(pop_21$province == "ON")] <- "Ontario"
pop_21$province[which(pop_21$province == "AB")] <- "Alberta"
pop_21$province[which(pop_21$province == "MB")] <- "Manitoba"
pop_21$province[which(pop_21$province == "BC")] <- "British Columbia"
pop_21$province[which(pop_21$province == "NB")] <- "New Brunswick"
pop_21$province[which(pop_21$province == "ON")] <- "Ontario"
pop_21$province[which(pop_21$province == "NL")] <- "Newfoundland"
pop_21$province[which(pop_21$province == "NS")] <- "Nova Scotia"
pop_21$province[which(pop_21$province == "NT")] <- "North Western Territories"
pop_21$province[which(pop_21$province == "PE")] <- "Prince Edward Island"
pop_21$province[which(pop_21$province == "QC")] <- "Quebec"
pop_21$province[which(pop_21$province == "SK")] <- "Saskatchewan"
pop_21$province[which(pop_21$province == "YT")] <- "Yukon"

pop_21_csd <- pop_21 %>% 
  filter(!(str_detect(UID_CSD_21,"000$")))

pop_21_csd <- pop_21_csd %>% ungroup() %>%  select(-UID_CD_21)

pop_21_cd <- pop_21 %>% 
  filter((str_detect(UID_CSD_21,"000$|NT220001|BC215001|BC217001"))) # there are three districts with only one sub-district and they end with 1 instead of 0

pop_21_cd <- pop_21_cd %>% ungroup() %>%  select(-subdistrict,-UID_CSD_21)

rm(pop_21)


############

# population 1911

############

colnames(pop_11) <- c("UID_CSD_11","province","district","subdistrict","type","area","population","pop_01")
pop_11$area <- pop_11$area*2.59
pop_11$subdistrict <- stri_trans_general(pop_11$subdistrict ,"Latin-ASCII")
pop_11$province <- stri_trans_general(pop_11$province ,"Latin-ASCII")
pop_11$district <- stri_trans_general(pop_11$district ,"Latin-ASCII")
pop_11$subdistrict <- gsub(".", "", pop_11$subdistrict,fixed = T)
pop_11$subdistrict <- gsub("-", " ", pop_11$subdistrict,fixed = T)
pop_11$subdistrict <- trimws(pop_11$subdistrict, "r")
pop_11$district <- gsub(".", "", pop_11$district,fixed = T)
pop_11$district <- gsub("-", " ", pop_11$district,fixed = T)
pop_11$district <- trimws(pop_11$district, "r")
pop_11$subdistrict <- gsub("(^\\s+)|(\\s+$)", "", pop_11$subdistrict,fixed = T)
pop_11$district <- gsub("(^\\s+)|(\\s+$)", "", pop_11$district,fixed = T)
pop_11$province <- as.character(pop_11$province)


pop_11$district <- gsub("-", " ", pop_11$district,fixed = T)
pop_11$district <- gsub(".", "", pop_11$district,fixed = T)
pop_11$district <- gsub(" E$", " East", pop_11$district)
pop_11$district <- gsub(" Est$", " East", pop_11$district)
pop_11$district <- gsub(" S$", " South", pop_11$district)
pop_11$district <- gsub(" N$", " North", pop_11$district)
pop_11$district <- gsub(" W O$", " West", pop_11$district)
pop_11$district <- gsub(" C$"," City",pop_11$district)

pop_11$district <- gsub(" E ", " East ", pop_11$district,fixed = T)
pop_11$district <- gsub(" S ", " South ", pop_11$district,fixed = T)
pop_11$district <- gsub(" N ", " North ", pop_11$district,fixed = T)
pop_11$district <- gsub(" W O ", " West ", pop_11$district,fixed = T)

pop_11$subdistrict <- gsub("-", " ", pop_11$subdistrict,fixed = T)
pop_11$subdistrict <- gsub(".", "", pop_11$subdistrict,fixed = T)
pop_11$subdistrict <- gsub(" E$", " East", pop_11$subdistrict)
pop_11$subdistrict <- gsub(" Est$", " East", pop_11$subdistrict)
pop_11$subdistrict <- gsub(" S$", " South", pop_11$subdistrict)
pop_11$subdistrict <- gsub(" N$", " North", pop_11$subdistrict)
pop_11$subdistrict <- gsub(" W O$", " West", pop_11$subdistrict)
pop_11$subdistrict <- gsub(" C$"," City",pop_11$subdistrict)


pop_11$subdistrict <- gsub(" E ", " East ", pop_11$subdistrict,fixed = T)
pop_11$subdistrict <- gsub(" S ", " South ", pop_11$subdistrict,fixed = T)
pop_11$subdistrict <- gsub(" N ", " North ", pop_11$subdistrict,fixed = T)
pop_11$subdistrict <- gsub(" W O ", " West ", pop_11$subdistrict,fixed = T)


pop_11$province[which(pop_11$province == "ON")] <- "Ontario"
pop_11$province[which(pop_11$province == "AB")] <- "Alberta"
pop_11$province[which(pop_11$province == "MB")] <- "Manitoba"
pop_11$province[which(pop_11$province == "BC")] <- "British Columbia"
pop_11$province[which(pop_11$province == "NB")] <- "New Brunswick"
pop_11$province[which(pop_11$province == "ON")] <- "Ontario"
pop_11$province[which(pop_11$province == "NL")] <- "Newfoundland"
pop_11$province[which(pop_11$province == "NS")] <- "Nova Scotia"
pop_11$province[which(pop_11$province == "NT")] <- "North Western Territories"
pop_11$province[which(pop_11$province == "PE")] <- "Prince Edward Island"
pop_11$province[which(pop_11$province == "QC")] <- "Quebec"
pop_11$province[which(pop_11$province == "SK")] <- "Saskatchewan"
pop_11$province[which(pop_11$province == "YT")] <- "Yukon"


pop_11_cd <- pop_11 %>%
  filter(is.na(district),!(str_detect(pop_11$UID_CSD_11,"000000")))

pop_11_cd <- pop_11_cd[,-which(colnames(pop_11_cd) %in% c("type","district"))]
pop_11_cd$UID_CD_11 <- gsub("000$","",pop_11_cd$UID_CSD_11)
pop_11_cd <- rename(pop_11_cd,"district" = "subdistrict")

pop_11_cd <- pop_11_cd %>% ungroup() %>%  select(-UID_CSD_11)

pop_11_csd <- pop_11 %>%
  filter(!is.na(district))

pop_11_cd$district[which(pop_11_cd$district == "Northumber Land East")] <- "Northumberland East"
pop_11_cd$district[which(pop_11_cd$district == "Northumber Land West")] <- "Northumberland West"
pop_11_cd$district[which(pop_11_cd$district == "Ottawa City Cite Pt")] <- "Ottawa City"
pop_11_cd$district[which(pop_11_cd$district == "Portage La Prairie")] <- "Portage la Prairie"

pop_11_csd$district[which(pop_11_csd$district == "Northumber Land East")] <- "Northumberland East"
pop_11_csd$district[which(pop_11_csd$district == "Northumber Land West")] <- "Northumberland West"
pop_11_csd$district[which(pop_11_csd$district == "Ottawa City Cite Pt")] <- "Ottawa City"
pop_11_csd$district[which(pop_11_csd$district == "Portage La Prairie")] <- "Portage la Prairie"

pop_11_cd$district <- gsub(",", "", pop_11_cd$district,fixed = T)
pop_11_csd$district <- gsub(",", "", pop_11_csd$district,fixed = T)



rm(pop_11)

############

# Agric and manu 

############

manuf_11 <- manuf_11 %>% filter(!str_detect(manuf_11$district,"000"),!(is.na(manuf_11$province)))
manuf_11$district <- stri_trans_general(manuf_11$district ,"Latin-ASCII")
manuf_11$district <- gsub("CITE","",manuf_11$district)
manuf_11$district <- gsub("GITE","",manuf_11$district)
manuf_11$district <- gsub("-COMTE","",manuf_11$district)
manuf_11$district <- gsub(" AND "," & ",manuf_11$district)
manuf_11$district <- gsub("SUD","",manuf_11$district)
manuf_11$district <- gsub("& CO","",manuf_11$district)
manuf_11$district <- gsub("ET CO","",manuf_11$district)
manuf_11$district <- gsub("ET CQ","",manuf_11$district)
manuf_11$district <- gsub("-EST","",manuf_11$district)
manuf_11$district <- gsub("W'EST","WEST",manuf_11$district)
manuf_11$district <- gsub("'EST","",manuf_11$district)
manuf_11$district <- gsub("OUEST","",manuf_11$district)
manuf_11$district <- gsub("NORD","",manuf_11$district)

manuf_11$district <- sub(".*? ","",manuf_11$district)
manuf_11$district <- gsub("[.,-]","",manuf_11$district)
manuf_11$district <- gsub("juu.*","",manuf_11$district)
manuf_11$district <- gsub("audess.*","",manuf_11$district)
manuf_11$district <- gsub(".*over et.*","",manuf_11$district)
manuf_11$district <- gsub(".*500.*","",manuf_11$district)
manuf_11$district <- gsub(".*25.*","",manuf_11$district)

manuf_11 <- manuf_11 %>% filter(!(str_detect(manuf_11$district,"^$")))
manuf_11$province <- gsub("(^\\s+)|(\\s+$)", "", manuf_11$province)
manuf_11$district <- gsub("(^\\s+)|(\\s+$)", "", manuf_11$district)



country_11 <- country_11 %>% filter(!(is.na(country_11$id)))

country_01$district <- stri_trans_general(country_01$district ,"Latin-ASCII")
country_11$district <- stri_trans_general(country_11$district ,"Latin-ASCII")
manuf$place <- stri_trans_general(manuf$place ,"Latin-ASCII")

country_01$province <- gsub(".", "", country_01$province,fixed = T)
country_01$province <- gsub("-", " ", country_01$province,fixed = T)
country_01$province <- trimws(country_01$province, "r")

country_11$province <- gsub(".", "", country_11$province,fixed = T)
country_11$province <- gsub("-", " ", country_11$province,fixed = T)
country_11$province <- trimws(country_11$province, "r")

manuf$province <- gsub(".", "", manuf$province,fixed = T)
manuf$province <- gsub("-", " ", manuf$province,fixed = T)
manuf$province <- trimws(manuf$province , "r")

country_01$district <- gsub(".", "", country_01$district,fixed = T)
country_01$district <- gsub(",", "", country_01$district,fixed = T)
country_01$district <- gsub("-", " ", country_01$district,fixed = T)
country_01$district <- trimws(country_01$district, "r")

country_11$district <- gsub(".", "", country_11$district,fixed = T)
country_11$district <- gsub("-", " ", country_11$district,fixed = T)
country_11$district <- trimws(country_11$district, "r")

manuf$place <- gsub(".", "", manuf$place,fixed = T)
manuf$place <- gsub("-", " ", manuf$place,fixed = T)
manuf$place <- trimws(manuf$place , "r")



manuf$province <- gsub("(^\\s+)|(\\s+$)", "", manuf$province)
manuf$place <- gsub("(^\\s+)|(\\s+$)", "", manuf$place)
country_11$district <- gsub("(^\\s+)|(\\s+$)", "", country_11$district)
country_11$province <- gsub("(^\\s+)|(\\s+$)", "", country_11$province)
country_01$district <- gsub("(^\\s+)|(\\s+$)", "", country_01$district)
country_01$province <- gsub("(^\\s+)|(\\s+$)", "", country_01$province)



manuf$place <- gsub("  ", "", manuf$place)
country_11$district <- gsub("  ", "", country_11$district)
country_01$district <- gsub("  ", "", country_01$district)

country_11$province <- as.character(country_11$province)
country_01$province <- as.character(country_01$province)
manuf$province <- as.character(manuf$province)




country_11$district <- gsub(" N$"," North",country_11$district)
country_11$district <- gsub(" S$"," South",country_11$district)
country_11$district <- gsub(" E$"," East",country_11$district)
country_11$district <- gsub(" W O$"," West",country_11$district)

country_01$district <- gsub(" Ouest","",country_01$district)
country_01$district <- gsub(" Est","",country_01$district)
country_01$district <- gsub(" Nord","",country_01$district)
country_01$district <- gsub(" Sud","",country_01$district)
country_01$district <- gsub("Ouest$","",country_01$district)
country_01$district <- gsub("Est$","",country_01$district)
country_01$district <- gsub("Nord$","",country_01$district)
country_01$district <- gsub("Sud$","",country_01$district)
country_01$district <- gsub(" -Sud","",country_01$district)
country_01$district <- gsub(" Cite","",country_01$district)
country_01$district <- gsub("Co et co","Country",country_01$district)
country_01$district <- gsub("Countv Comte","Country",country_01$district)

country_01$district[which(country_01$district == "Ni pissing")] <- "Nipissing"
country_01$district[which(country_01$district == "Soul anges")] <- "Soulanges"

country_11$district <- gsub(" W 0$"," West",country_11$district)

country_11$district <- gsub(" C$"," City",country_11$district)
country_01$district <- gsub(" C$"," City",country_01$district)

manuf$place <- gsub("Saint ","St ",manuf$place)
manuf$place <- gsub("Sainte ","Ste ",manuf$place)


country_01$province[which(country_01$province == "Ontario")] <- "ON"
country_01$province[which(country_01$province == "Alberta")] <- "AB"
country_01$province[which(country_01$province == "Manitoba")] <- "MB"
country_01$province[which(country_01$province == "British Columbia")] <- "BC"
country_01$province[which(country_01$province == "New Brunswick")] <- "NB"
country_01$province[which(country_01$province == "Ontario")] <- "ON"
country_01$province[which(country_01$province == "Newfoundland")] <- "NL"
country_01$province[which(country_01$province == "Nova Scotia")] <- "NS"
country_01$province[which(country_01$province %in% c("North Western Territories","The Territories"))] <- "NT"
country_01$province[which(country_01$province == "Prince Edward Island")] <- "PE"
country_01$province[which(country_01$province == "Quebec")] <- "QC"
country_01$province[which(country_01$province == "Saskatchewan")] <- "SK"
country_01$province[which(country_01$province == "Yukon")] <- "YT"

country_01$id <- gsub(" to .*","",country_01$id)
country_01$id <- as.numeric(country_01$id)
country_01$UID_CD_01 <- paste0(country_01$province,sprintf("%03d", country_01$id))

country_01$province[which(country_01$province == "ON")] <- "Ontario"
country_01$province[which(country_01$province == "AB")] <- "Alberta"
country_01$province[which(country_01$province == "MB")] <- "Manitoba"
country_01$province[which(country_01$province == "BC")] <- "British Columbia"
country_01$province[which(country_01$province == "NB")] <- "New Brunswick"
country_01$province[which(country_01$province == "ON")] <- "Ontario"
country_01$province[which(country_01$province == "NL")] <- "Newfoundland"
country_01$province[which(country_01$province == "NS")] <- "Nova Scotia"
country_01$province[which(country_01$province == "NT")] <- "North Western Territories"
country_01$province[which(country_01$province == "PE")] <- "Prince Edward Island"
country_01$province[which(country_01$province == "QC")] <- "Quebec"
country_01$province[which(country_01$province == "SK")] <- "Saskatchewan"
country_01$province[which(country_01$province == "YT")] <- "Yukon"

country_01 <- country_01 %>% select(-c("id"))


country_11$province[which(country_11$province == "Ontario")] <- "ON"
country_11$province[which(country_11$province == "Alberta")] <- "AB"
country_11$province[which(country_11$province == "Manitoba")] <- "MB"
country_11$province[which(country_11$province == "British Columbia")] <- "BC"
country_11$province[which(country_11$province == "New Brunswick")] <- "NB"
country_11$province[which(country_11$province == "Ontario")] <- "ON"
country_11$province[which(country_11$province == "Newfoundland")] <- "NL"
country_11$province[which(country_11$province == "Nova Scotia")] <- "NS"
country_11$province[which(country_11$province == "North Western Territories")] <- "NT"
country_11$province[which(country_11$province == "Prince Edward Island")] <- "PE"
country_11$province[which(country_11$province == "Quebec")] <- "QC"
country_11$province[which(country_11$province == "Saskatchewan")] <- "SK"
country_11$province[which(country_11$province == "Yukon")] <- "YT"

country_11$id <- gsub(" to .*","",country_11$id)
country_11$id <- as.numeric(country_11$id)
country_11$UID_CD_11 <- paste0(country_11$province,sprintf("%03d", country_11$id))

country_11$province[which(country_11$province == "ON")] <- "Ontario"
country_11$province[which(country_11$province == "AB")] <- "Alberta"
country_11$province[which(country_11$province == "MB")] <- "Manitoba"
country_11$province[which(country_11$province == "BC")] <- "British Columbia"
country_11$province[which(country_11$province == "NB")] <- "New Brunswick"
country_11$province[which(country_11$province == "ON")] <- "Ontario"
country_11$province[which(country_11$province == "NL")] <- "Newfoundland"
country_11$province[which(country_11$province == "NS")] <- "Nova Scotia"
country_11$province[which(country_11$province == "NT")] <- "North Western Territories"
country_11$province[which(country_11$province == "PE")] <- "Prince Edward Island"
country_11$province[which(country_11$province == "QC")] <- "Quebec"
country_11$province[which(country_11$province == "SK")] <- "Saskatchewan"
country_11$province[which(country_11$province == "YT")] <- "Yukon"

country_11 <- country_11 %>% select(-c("id"))


manuf$place[which(manuf$place == "Valley field'")] <- "Valleyfield"

manuf_11$district[which(manuf_11$district == "COMOXATLIN")] <- "Comox Atlin"
manuf_11$district <- str_to_title(manuf_11$district)
manuf_11$district[which(manuf_11$district == "Portage La Prairie")] <- "Portage la Prairie"
manuf_11$district[which(manuf_11$district == "L'islet")] <- "L'Islet"
manuf_11$district[which(manuf_11$district == "L'assomption")] <- "L'Assomption"
manuf_11$district[which(manuf_11$district == "Qu'appelle")] <- "Qu'Appelle"
manuf_11$district[which(manuf_11$district == "Moose Jaw")] <- "Moosejaw"

manuf_11$district[which(manuf_11$district == "Rivieres & St Maurice")] <- "Trois Rivieres & St Maurice"
manuf_11$district[which(manuf_11$district == "Victoria" & manuf_11$province == "New Brunswick")] <- "Victoria & Madawaska"
manuf_11$district[which(manuf_11$district == "Victoria & Haliburton")] <- "Victoria"


#

# 1881 manuf

colnames(df_end_81) <- c("district","number","M","W","B","G","wages","raw","value")
df_end_81 <- df_end_81[which(str_detect(df_end_81$district, "^[0-9].*")),]
df_end_81 <- separate(df_end_81,1,c("id","district"), sep = "\\s", extra = "merge")
df_end_81$id <- gsub("\\.","",df_end_81$id)

df.list_812 <- df.list_81[unlist(lapply(df.list_81, function(df){!(NCOL(df) == 25)}))]
df1 <- df.list_812[[1]]
colnames(df1) <-c("district","number","M","W","B","G","wages","raw","value",
                                                          "number","M","B","wages","raw","value",
                                                          "number","M","W","B","G","wages","raw","value")
df1 <- df1[which(str_detect(df1$district, "^[0-9].*")),]


df2 <- df.list_812[[2]]
df2 <- df2[,-NCOL(df2)]
df.list_81 <- df.list_81[unlist(lapply(df.list_81, function(df){(NCOL(df) == 25)}))]
df.list_81 <- c(df.list_81,list(df2))
rm(df2,df.list_812)

df.list_81 <- lapply(df.list_81,function(df) {colnames(df) <- c("district","number","M","W","B","G","wages","raw","value",
                                                          "number","M","W","B","G","wages","raw","value",
                                                          "number","M","W","B","G","wages","raw","value");return(df)})
df.list_81 <- lapply(df.list_81,function(df) df <- df[which(str_detect(df$district, "^[0-9].*")),])


df.list_81 <- lapply(df.list_81,function(df){df <- rbind(df[,c(1,2:9)],df[,c(1,10:17)],df[,c(1,18:25)]);return(df)})
df1 <- bind_rows(df1[,c(1,2:9)],df1[,c(1,10:15)],df1[,c(1,16:23)])

df1 <- separate(df1,1,c("id","district"), sep = "\\s", extra = "merge")
df1$id <- gsub("\\.","",df1$id)
df.list_81 <- lapply(df.list_81,function(df) df <- separate(df,1,c("id","district"), sep = "\\s", extra = "merge"))
df.list_81 <- lapply(df.list_81,function(df){df$id <- gsub("\\.","",df$id);return(df)})


df_aux <- aggregate(x = df1[2], by = df1[1], FUN = function(x) extract(x,1))  
df_aux2 <- aggregate(x = df1[-c(1,2)], by = df1[1], FUN = function(x) sum(x, na.rm = T))  

df1 <- left_join(df_aux,df_aux2, by = "id")

df_aux <- aggregate(x = df_end_81[2], by = df_end_81[1], FUN = function(x) extract(x,1))  
df_aux2 <- aggregate(x = df_end_81[-c(1,2)], by = df_end_81[1], FUN = function(x) sum(x, na.rm = T))  

df_end_81 <- left_join(df_aux,df_aux2, by = "id")


df.list_81 <- c(df.list_81,list(df1,df_end_81))
rm(df1,df_end_81)

df <- bind_rows(df.list_81)
rm(df.list_81)

df_aux <- aggregate(x = df[2], by = df[1], FUN = function(x) extract(x,1))  
df_aux2 <- aggregate(x = df[-c(1,2)], by = df[1], FUN = function(x) sum(x, na.rm = T))  

df <- left_join(df_aux,df_aux2, by = "id")

df <- df %>% mutate(labour = M+W+B+G) %>% select(-M,-W,-B,-G)
df$id <- as.numeric(df$id)

manuf_81 <- df
rm(df,df_aux,df_aux2)


manuf_81$district <- stri_trans_general(manuf_81$district ,"Latin-ASCII")
manuf_81$district <- gsub(".", "", manuf_81$district,fixed = T)
manuf_81$district <- gsub(",", "", manuf_81$district,fixed = T)
manuf_81$district <- gsub("-", " ", manuf_81$district,fixed = T)
manuf_81$district <- trimws(manuf_81$district, "r")

manuf_81$district <- gsub(" N$"," North",manuf_81$district)
manuf_81$district <- gsub(" S$"," South",manuf_81$district)
manuf_81$district <- gsub(" E$"," East",manuf_81$district)
manuf_81$district <- gsub(" W O$"," West",manuf_81$district)
manuf_81$district <- gsub(" W$"," West",manuf_81$district)

manuf_81$district <- gsub("C V","City",manuf_81$district)
manuf_81$district <- gsub("C$","Centre",manuf_81$district)
manuf_81$district <- gsub("North'l'd","Northumberland",manuf_81$district)
manuf_81$district <- gsub("l'd","land",manuf_81$district)

manuf_81$district[which(str_detect(manuf_81$district,"Chicoutimi"))] <- "Chicoutimi & Saguenay"
manuf_81$district[which(str_detect(manuf_81$district,"Montagnes"))] <- "Deux Montagnes"
manuf_81$district[which(str_detect(manuf_81$district,"Drumm"))] <- "Drummond & Arthabaska"
manuf_81$district[which(manuf_81$district == "Halifax Co")] <- "Halifax"
manuf_81$district[which(manuf_81$district == "Ottawa Co")] <- "Ottawa"
manuf_81$district[which(manuf_81$district == "Quebec Co")] <- "Quebec"
manuf_81$district[which(manuf_81$district == "St John Co")] <- "St John"
manuf_81$district[which(str_detect(manuf_81$district,"Jacques"))] <- "Jacques Cartier"
manuf_81$district[which(str_detect(manuf_81$district,"Grenv N"))] <- "Leeds & Grenville N"
manuf_81$district[which(str_detect(manuf_81$district,"Westm'"))] <- "New Westminster"
manuf_81$district[which(str_detect(manuf_81$district,"Richm'd & West"))] <- "Richmond & Wolfe"
manuf_81$district <- gsub("Peterboro'","Peterborough",manuf_81$district)
manuf_81$district <- gsub("Edw'd","Edward",manuf_81$district)

manuf_81$district[which(manuf_81$district == "King's")] <- "Kings"
manuf_81$district[which(manuf_81$district == "Queen's")] <- "Queens"

manuf_81$district[which(manuf_81$district == "Westmoreland")] <- "Westmorland"
manuf_81$district[which(manuf_81$district == "L'lslet")] <- "L'Islet"
manuf_81$district[which(manuf_81$district == "Portneut")] <- "Portneuf"
manuf_81$district[which(str_detect(manuf_81$district,"Trois"))] <- "Trois Rivieres City"
manuf_81$district[which(manuf_81$district == "Beance")] <- "Beauce"

manuf_81$province <- NA
manuf_81$province[which(manuf_81$id == 108)] <- "Ontario"
manuf_81$province[which(manuf_81$id == 31)] <- "New Brunswick"

manuf_81$province[which(manuf_81$id == 17)] <- "Nova Scotia"
manuf_81$province[which(manuf_81$id == 3)] <- "Prince Edward Island"
manuf_81$province[which(manuf_81$id == 27)] <- "New Brunswick"

manuf_81$province[which(manuf_81$id == 12)] <- "Nova Scotia"
manuf_81$province[which(manuf_81$id == 2)] <- "Prince Edward Island"
manuf_81$province[which(manuf_81$id == 28)] <- "New Brunswick"

manuf_81$province[which(manuf_81$id == 5)] <- "Nova Scotia"
manuf_81$province[which(manuf_81$id == 32)] <- "New Brunswick"
manuf_81$province[which(manuf_81$id == 190)] <- "British Columbia"

# a <- left_join(df_71_cd,manuf_71[which(is.na(manuf_71$province)),], by = "district")





# 1871 manuf

df.list_71 <- lapply(df.list_71,function(df){

  df$labour <- rowSums(df[,which(str_detect(colnames(df),"^M.*|^F.*|^O.*|^U.*"))], na.rm = T)
  df <- df[,-which(str_detect(colnames(df),"^M.*|^F.*|^O.*|^U.*"))]
  
  return(df)
})

df.list_71[[17]] <- df.list_71[[17]][,-which(str_detect(colnames(df.list_71[[17]]),".*23.*"))]

df_end_71$labour <- rowSums(df_end_71[,which(str_detect(colnames(df_end_71),"^M.*|^F.*|^O.*|^U.*"))], na.rm = T)
df_end_71 <- df_end_71[,-which(str_detect(colnames(df_end_71),"^M.*|^F.*|^O.*|^U.*"))]

colnames(df_end_71) <- c("district","number","wages","raw","value","labour")
df_end_71 <- df_end_71[which(str_detect(df_end_71$district, "^[0-9].*")),]
df_end_71 <- separate(df_end_71,1,c("id","district"), sep = "\\s", extra = "merge")
df_end_71$id <- gsub("\\.","",df_end_71$id)

df.list_71 <- lapply(df.list_71,function(df){

  colnames(df) <- c("district","number","wages","raw","value","number","wages","raw","value","number","wages","raw","value","labour")
  df <- df[which(str_detect(df$district, "^[0-9].*")),]
  
  df$number1 <- rowSums(df[,which(str_detect(colnames(df),"number"))], na.rm = T)
  df <- df[,-which(colnames(df) == "number")]
  df$wages1 <- rowSums(df[,which(str_detect(colnames(df),"wages"))], na.rm = T)
  df <- df[,-which(colnames(df) == "wages")]
  df$raw1 <- rowSums(df[,which(str_detect(colnames(df),"raw"))], na.rm = T)
  df <- df[,-which(colnames(df) == "raw")]
  df$value1 <- rowSums(df[,which(str_detect(colnames(df),"value"))], na.rm = T)
  df <- df[,-which(colnames(df) == "value")]
  
  colnames(df) <- gsub("\\d$","",colnames(df))
  
  df <- separate(df,1,c("id","district"), sep = "\\s", extra = "merge")
  df$id <- gsub("\\.","",df$id)

  return(df)
})

df.list_71 <- c(df.list_71,list(df_end_71))
rm(df_end_71)

df <- bind_rows(df.list_71)
rm(df.list_71)

df_aux <- aggregate(x = df[2], by = df[1], FUN = function(x) extract(x,1))  
df_aux2 <- aggregate(x = df[-c(1,2)], by = df[1], FUN = function(x) sum(x, na.rm = T))  

df <- left_join(df_aux,df_aux2, by = "id")
df$id <- as.numeric(df$id)

manuf_71 <- df
rm(df,df_aux,df_aux2)



manuf_71$district <- stri_trans_general(manuf_71$district ,"Latin-ASCII")
manuf_71$district <- gsub(".", "", manuf_71$district,fixed = T)
manuf_71$district <- gsub(",", "", manuf_71$district,fixed = T)
manuf_71$district <- gsub("-", " ", manuf_71$district,fixed = T)
manuf_71$district <- trimws(manuf_71$district, "r")

manuf_71$district <- gsub(" N$"," North",manuf_71$district)
manuf_71$district <- gsub(" S$"," South",manuf_71$district)
manuf_71$district <- gsub(" E$"," East",manuf_71$district)
manuf_71$district <- gsub(" W O$"," West",manuf_71$district)
manuf_71$district <- gsub(" W$"," West",manuf_71$district)

manuf_71$district[which(str_detect(manuf_71$district,"Montreal"))] <- "Montreal City"
manuf_71$id[which(str_detect(manuf_71$district,"Montreal"))] <- manuf_71$id[which(str_detect(manuf_71$district,"Montreal"))][1]

manuf_71$district[which(str_detect(manuf_71$district,"Ja'ques"))] <- "Jacques Cartier"

manuf_71$district[which(str_detect(manuf_71$district,"Quebec") & !(str_detect(manuf_71$district,"Co de Quebec")))] <- "Quebec City"
manuf_71$id[which(str_detect(manuf_71$district,"Quebec") & !(str_detect(manuf_71$district,"Co de Quebec")))] <- manuf_71$id[which(str_detect(manuf_71$district,"Quebec") & !(str_detect(manuf_71$district,"Co de Quebec")))][1]

manuf_71$district[which(str_detect(manuf_71$district,"Co de Quebec"))] <- "Quebec"

manuf_71$district[which(str_detect(manuf_71$district,"Brant N or East"))] <- "Brant North"
manuf_71$district[which(str_detect(manuf_71$district,"Brant S or West"))] <- "Brant South"

manuf_71$district[which(str_detect(manuf_71$district,"Montagnes"))] <- "Deux Montagnes"
manuf_71$district[which(str_detect(manuf_71$district,"Dorchester O"))] <- "Dorchester West"
manuf_71$district[which(str_detect(manuf_71$district,"Elgin W C"))] <- "Elgin West"
manuf_71$district[which(str_detect(manuf_71$district,"Magdeleine"))] <- "Madeleine Islands"
manuf_71$district[which(str_detect(manuf_71$district,"Gr'ville"))] <- "Grenville N & Leeds N"

manuf_71$district[which(str_detect(manuf_71$district,"North'b'l'nd East"))] <- "Northumberland East"
manuf_71$district[which(str_detect(manuf_71$district,"North'b'l'nd West"))] <- "Northumberland West"
manuf_71$district[which(str_detect(manuf_71$district,"Northumberl'd"))] <- "Northumberland"

manuf_71$district[which(str_detect(manuf_71$district,"Ottawa"))] <- "Ottawa City"
manuf_71$id[which(str_detect(manuf_71$district,"Ottawa"))] <- manuf_71$id[which(str_detect(manuf_71$district,"Ottawa"))][1]

manuf_71$district <- gsub("Peterboro|Peterboro'","Peterborough",manuf_71$district)

manuf_71$district[which(manuf_71$district == "Rimouski O")] <- "Rimouski West"
manuf_71$district[which(manuf_71$district == "St John")] <- "St John City"

manuf_71$district[which(str_detect(manuf_71$district,"Toronto"))] <- "Toronto City"
manuf_71$id[which(str_detect(manuf_71$district,"Toronto"))] <- manuf_71$id[which(str_detect(manuf_71$district,"Toronto"))][1]

manuf_71$district[which(manuf_71$id == 153)] <- "Levis Town"

manuf_71$district[which(manuf_71$district == "Trois Rivieres")] <- "Trois Rivieres City"
manuf_71$district[which(manuf_71$district == "Wellington C")] <- "Wellington Centre"
manuf_71$district[which(manuf_71$district == "York East")] <- "East York"

manuf_71$district[which(manuf_71$district == "Kingston")] <- "Kingston City"
manuf_71$district[which(manuf_71$district == "Halifax West")] <- "Halifax City"
manuf_71$district[which(manuf_71$district == "Restigouehe")] <- "Restigouche"
manuf_71$district[which(manuf_71$district == "Bellechase North")] <- "Bellechasse North"
manuf_71$district[which(manuf_71$district == "London")] <- "London City"
manuf_71$district[which(manuf_71$district == "Hamilton")] <- "Hamilton City"
manuf_71$district[which(manuf_71$district == "King's")] <- "Kings"
manuf_71$district[which(manuf_71$district == "Queen's")] <- "Queens"
manuf_71$district <- gsub("Ottaoua","Outaouais",manuf_71$district)

manuf_71_aux <- aggregate(x = manuf_71[2], by = manuf_71[1], FUN = function(x) extract(x,1))  
manuf_71_aux2 <- aggregate(x = manuf_71[-c(1,2)], by = manuf_71[1], FUN = function(x) sum(x, na.rm = T))  
manuf_71 <- left_join(manuf_71_aux,manuf_71_aux2, by = "id")
rm(manuf_71_aux,manuf_71_aux2)


manuf_71$province <- NA
manuf_71$province[which(manuf_71$id == 78)] <- "Ontario"
manuf_71$province[which(manuf_71$id == 180)] <- "New Brunswick"
manuf_71$province[which(manuf_71$id == 189)] <- "Nova Scotia"
manuf_71$province[which(manuf_71$id == 194)] <- "Nova Scotia"
manuf_71$province[which(manuf_71$id == 138)] <- "Quebec"
manuf_71$province[which(manuf_71$id == 206)] <- "Nova Scotia"
manuf_71$province[which(manuf_71$id == 181)] <- "New Brunswick"
manuf_71$province[which(manuf_71$id == 204)] <- "Nova Scotia"
manuf_71$province[which(manuf_71$id == 2)] <- "Ontario"
manuf_71$province[which(manuf_71$id == 185)] <- "New Brunswick"
manuf_71$province[which(manuf_71$id == 176)] <- "New Brunswick"
manuf_71$province[which(manuf_71$id == 177)] <- "New Brunswick"

# a <- left_join(df_71_cd,manuf_71[which(is.na(manuf_71$province)),], by = "district")

# 1891

colnames(manuf_91) <- c("province","district","number","capital_land","capital_buildings","capital_tools","capital_working","M","W","B","G","wages","raw","value")

manuf_91 <- manuf_91 %>% filter(!(is.na(province)))
manuf_91$province <- gsub("\\d",NA,manuf_91$province)
manuf_91$province <- gsub("\\.","",manuf_91$province)
manuf_91$province <- str_to_title(manuf_91$province)
manuf_91 <- manuf_91 %>% filter(is.na(province) | str_detect(province,"British|Quebec|Ontario|Prince|Territories|Brunswick|Scotia|Manitoba|Sasketchewan|Alberta"))
manuf_91 <- manuf_91 %>% fill(province)
manuf_91 <- manuf_91 %>% filter(!(is.na(district)))

manuf_91$labour <- rowSums(manuf_91[,which(colnames(manuf_91) %in% c("M","W","B","G"))], na.rm = T)
manuf_91 <- manuf_91 %>% select(-M,-W,-B,-G)

manuf_91 <- separate(manuf_91,2,c("id","district"), sep = "\\s", extra = "merge")
manuf_91$id <- gsub("\\.","",manuf_91$id)
manuf_91_aux <- aggregate(x = manuf_91[c(1,3)], by = manuf_91[2], FUN = function(x) extract(x,1))  
manuf_91_aux2 <- aggregate(x = manuf_91[-c(1,2,3)], by = manuf_91[2], FUN = function(x) sum(x, na.rm = T))  
manuf_91 <- left_join(manuf_91_aux,manuf_91_aux2, by = "id")
manuf_91 <- manuf_91 %>% select(-id)
rm(manuf_91_aux,manuf_91_aux2)


manuf_91$district <- stri_trans_general(manuf_91$district ,"Latin-ASCII")
manuf_91$district <- gsub(".", "", manuf_91$district,fixed = T)
manuf_91$district <- gsub(",", "", manuf_91$district,fixed = T)
manuf_91$district <- gsub("-", " ", manuf_91$district,fixed = T)
manuf_91$district <- trimws(manuf_91$district, "r")

manuf_91$district <- gsub(" N$"," North",manuf_91$district)
manuf_91$district <- gsub(" S$"," South",manuf_91$district)
manuf_91$district <- gsub(" E$"," East",manuf_91$district)
manuf_91$district <- gsub(" W O$"," West",manuf_91$district)
manuf_91$district <- gsub(" W$"," West",manuf_91$district)

manuf_91$district <- gsub(" C$"," City",manuf_91$district)
manuf_91$district[which(manuf_91$district == "King's")] <- "Kings"
manuf_91$district[which(manuf_91$district == "Queen's")] <- "Queens"

manuf_91$district <- gsub(" and "," & ",manuf_91$district)
manuf_91$district <- gsub(" Co$","",manuf_91$district)

manuf_91$district[which(manuf_91$district == "Simcoe South")] <- "Simcoe East"
manuf_91_aux2 <- aggregate(x = manuf_91[-c(1,2)], by = manuf_91[c(1,2)], FUN = function(x) sum(x, na.rm = T))  
rm(manuf_91_aux2)

manuf_91$district[which(manuf_91$district == "Veroheres")] <- "Vercheres"
manuf_91$district[which(manuf_91$district == "Westmoreland")] <- "Westmorland"
manuf_91$district[which(manuf_91$district == "Piotou")] <- "Pictou"
manuf_91$district[which(manuf_91$district == "Glengary")] <- "Glengarry"
manuf_91$district[which(str_detect(manuf_91$district,"Drummond"))] <- "Drummond"
manuf_91$district[which(str_detect(manuf_91$district,"Liagar"))] <- "Lisgar"
manuf_91$district[which(str_detect(manuf_91$district,"Richmond"))] <- "Richmond"
manuf_91$district[which(str_detect(manuf_91$district,"Trois"))] <- "Trois Rivieres City"
manuf_91$district[which(str_detect(manuf_91$district,"& Grenville"))] <- "Leeds & Grenville N"




# 1901


manuf_01 <- manuf_01[,-c(1,NCOL(manuf_01)-1,NCOL(manuf_01))]
colnames(manuf_01) <- c("province","district","ind1","ind2","number","capital","labour_s","salaries","labour_w","wages","raw","value")
manuf_01 <- manuf_01 %>% filter(!(is.na(ind2)))
manuf_01 <- manuf_01[,-c(3,4)]

manuf_01 <- separate(manuf_01,2,c("id","district"), sep = "\\s", extra = "merge")
manuf_01$id <- gsub("\\.","",manuf_01$id)
manuf_01$district <- gsub("\\.","",manuf_01$district)

manuf_01[,c(4:NCOL(manuf_01))] <- apply(manuf_01[,c(4:NCOL(manuf_01))], 2, function(x) as.numeric(gsub("\\.",NA,x)))

manuf_01_aux <- aggregate(x = manuf_01[c(1,3)], by = manuf_01[2], FUN = function(x) extract(x,1))  
manuf_01_aux2 <- aggregate(x = manuf_01[-c(1,2,3)], by = manuf_01[2], FUN = function(x) sum(x, na.rm = T))  
manuf_01 <- left_join(manuf_01_aux,manuf_01_aux2, by = "id")
manuf_01 <- manuf_01 %>% select(-id)
rm(manuf_01_aux,manuf_01_aux2)


manuf_01$district <- stri_trans_general(manuf_01$district ,"Latin-ASCII")
manuf_01$district <- gsub(".", "", manuf_01$district,fixed = T)
manuf_01$district <- gsub(",", "", manuf_01$district,fixed = T)
manuf_01$district <- gsub("-", " ", manuf_01$district,fixed = T)
manuf_01$district <- trimws(manuf_01$district, "r")

manuf_01$district <- gsub(" and "," & ",manuf_01$district)
manuf_01$district <- gsub(" Co$","",manuf_01$district)

aux <- c("Nova Scotia","Halifax City",104,6637888,0,0,3203,1238385,4502707,6927552)

manuf_01$district[which(manuf_01$district == "Halifax City &")] <- "Halifax"
manuf_01[which(manuf_01$district == "Halifax"),c(3:NCOL(manuf_01))] <- as.numeric(manuf_01[which(manuf_01$district == "Halifax"),c(3:NCOL(manuf_01))]) - c(104,6637888,0,0,3203,1238385,4502707,6927552)
manuf_01 <- rbind(manuf_01,aux)

manuf_01$district[which(manuf_01$district == "St John City &")] <- "St John"

aux <- c("New Brunswick","St John City",187,5252797,0,0,4688,1634051,3545549,6712769)

manuf_01[which(manuf_01$district == "St John"),c(3:NCOL(manuf_01))] <- as.numeric(manuf_01[which(manuf_01$district == "St John"),c(3:NCOL(manuf_01))]) - c(187,5252797,0,0,4688,1634051,3545549,6712769)
manuf_01$salaries[which(manuf_01$district == "St John")] <- as.numeric(manuf_01$salaries[which(manuf_01$district == "St John")]) + as.numeric(manuf_01$wages[which(manuf_01$district == "St John")]) 
manuf_01$wages[which(manuf_01$district == "St John")] <- 0
manuf_01 <- rbind(manuf_01,aux)
rm(aux)

manuf_01$district[which(manuf_01$district == "Toronto Centre")] <- "Toronto City"
manuf_01$district[which(manuf_01$district == "Toronto East")] <- "Toronto City"
manuf_01$district[which(manuf_01$district == "Toronto West")] <- "Toronto City"
manuf_01$district[which(manuf_01$district == "Winnipeg")] <- "Winnipeg City"
manuf_01$district[which(manuf_01$district == "Wentworth & Brant North")] <- "Wentworth N & Brant N"
manuf_01$district[which(manuf_01$district == "Chambly & Veroheres")] <- "Chambly & Vercheres"
manuf_01$district[which(manuf_01$district == "St Hyacinths")] <- "St Hyacinthe"
manuf_01$district[which(manuf_01$district == "Leeds & Grenville North")] <- "Leeds N & Grenville N"
manuf_01$district[which(manuf_01$district == "Three Rivers & St Maurice")] <- "Trois Rivieres & St Maurice"





