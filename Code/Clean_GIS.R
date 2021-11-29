############

# maps

############

# re-project gis to have same crs in all maps

df_51_cd <- st_transform(df_51_cd,
                         crs = 32610)
df_61_cd <- st_transform(df_61_cd,
                         crs = 32610)
df_71_cd <- st_transform(df_71_cd,
                         crs = 32610)
df_81_cd <- st_transform(df_81_cd,
                         crs = 32610)
df_91_cd <- st_transform(df_91_cd,
                         crs = 32610)
df_01_cd <- st_transform(df_01_cd,
                         crs = 32610)
df_21_cd <- st_transform(df_21_cd,
                         crs = 32610)
df_11_cd <- st_transform(df_11_cd,
                         crs = 32610)
df_11_csd <- st_transform(df_11_csd,
                          crs = 32610)
df_21_csd <- st_transform(df_21_csd,
                          crs = 32610)

#

df_51_cd <- df_51_cd %>% select(-Shape_Le_1)
df_61_cd <- df_61_cd %>% select(-Shape_Le_1)

df_51_cd <- df_51_cd[,-c(2,4)]
colnames(df_51_cd) <- c("district","province","population","area","density","shape_leng","shapre_area","geometry")
df_61_cd <- df_61_cd[,-c(2,4)]
colnames(df_61_cd) <- c("district","province","population","area","density","shape_leng","shapre_area","geometry")
df_71_cd$CDNAME[which(str_detect(df_71_cd$CDNAMEPROV,"East") & df_71_cd$CDNAME == "Halifax")] <- "Halifax East"
df_71_cd$CDNAME[which(str_detect(df_71_cd$CDNAMEPROV,"West") & df_71_cd$CDNAME == "Halifax")] <- "Halifax West"
df_71_cd$CDNAME[166] <- "Rimouski West" # don't know why but even if these are already called like that they don't show up if you search for the exact name
df_71_cd$CDNAME[192] <- "Victoria South"
df_71_cd <- df_71_cd[,-c(2,4)]
colnames(df_71_cd) <- c("district","province","population","area","density","shape_leng","shapre_area","geometry")
df_81_cd <- df_81_cd[,-c(2,4)]
colnames(df_81_cd) <- c("district","province","population","area","density","shape_leng","shapre_area","geometry")
df_91_cd <- df_91_cd[,-c(2,4)]
colnames(df_91_cd) <- c("district","province","population","area","density","shape_leng","shapre_area","geometry")
df_01_cd <- df_01_cd[,-c(2,4)]
colnames(df_01_cd) <- c("district","province","population","area","density","shape_leng","shapre_area","geometry")


df_11_csd <- df_11_csd[,-c(10:14)]
df_11_csd <- rename(df_11_csd, "district" = "CDNAME_11")
df_11_csd <- rename(df_11_csd, "subdistrict" = "CSDNAME_11")
df_11_csd <- rename(df_11_csd, "province" = "PR_11")

df_21_csd <- df_21_csd[,-c(10:13)]
df_21_csd <- rename(df_21_csd, "district" = "CDNAME_21")
df_21_csd <- rename(df_21_csd, "subdistrict" = "CSDNAME_21")
df_21_csd <- rename(df_21_csd, "province" = "PR_21")

df_11_cd <- df_11_cd[,-c(5:9)]
df_11_cd <- rename(df_11_cd, "district" = "CDNAME_11")
df_11_cd <- rename(df_11_cd, "province" = "PR_11")


df_21_cd <- df_21_cd[,-c(5:8)]
df_21_cd <- rename(df_21_cd, "district" = "CDNAME_21")
df_21_cd <- rename(df_21_cd, "province" = "PR_21")


df_51_cd$district <- stri_trans_general(df_51_cd$district ,"Latin-ASCII")
df_61_cd$district <- stri_trans_general(df_61_cd$district ,"Latin-ASCII")
df_71_cd$district <- stri_trans_general(df_71_cd$district ,"Latin-ASCII")
df_81_cd$district <- stri_trans_general(df_81_cd$district ,"Latin-ASCII")
df_91_cd$district <- stri_trans_general(df_91_cd$district ,"Latin-ASCII")
df_01_cd$district <- stri_trans_general(df_01_cd$district ,"Latin-ASCII")
df_11_csd$subdistrict <- stri_trans_general(df_11_csd$subdistrict ,"Latin-ASCII")
df_21_csd$subdistrict <- stri_trans_general(df_21_csd$subdistrict ,"Latin-ASCII")
df_21_cd$district <- stri_trans_general(df_21_cd$district ,"Latin-ASCII")
df_21_csd$district <- stri_trans_general(df_21_csd$district ,"Latin-ASCII")
df_11_csd$district <- stri_trans_general(df_11_csd$district ,"Latin-ASCII")
df_11_cd$district <- stri_trans_general(df_11_cd$district ,"Latin-ASCII")

df_51_cd$province <- stri_trans_general(df_51_cd$province ,"Latin-ASCII")
df_61_cd$province <- stri_trans_general(df_61_cd$province ,"Latin-ASCII")
df_71_cd$province <- stri_trans_general(df_71_cd$province ,"Latin-ASCII")
df_81_cd$province <- stri_trans_general(df_81_cd$province ,"Latin-ASCII")
df_91_cd$province <- stri_trans_general(df_91_cd$province ,"Latin-ASCII")
df_01_cd$province <- stri_trans_general(df_01_cd$province ,"Latin-ASCII")
df_11_cd$province <- stri_trans_general(df_11_cd$province ,"Latin-ASCII")
df_11_csd$province <- stri_trans_general(df_11_csd$province ,"Latin-ASCII")
df_21_csd$province <- stri_trans_general(df_21_csd$province ,"Latin-ASCII")
df_21_cd$province <- stri_trans_general(df_21_cd$province ,"Latin-ASCII")

df_51_cd$district <- gsub(".", "", df_51_cd$district,fixed = T)
df_51_cd$district <- gsub("-", " ", df_51_cd$district,fixed = T)
df_61_cd$district <- gsub(".", "", df_61_cd$district,fixed = T)
df_61_cd$district <- gsub("-", " ", df_61_cd$district,fixed = T)
df_71_cd$district <- gsub(".", "", df_71_cd$district,fixed = T)
df_71_cd$district <- gsub("-", " ", df_71_cd$district,fixed = T)
df_81_cd$district <- gsub(".", "", df_81_cd$district,fixed = T)
df_81_cd$district <- gsub("-", " ", df_81_cd$district,fixed = T)
df_91_cd$district <- gsub(".", "", df_91_cd$district,fixed = T)
df_91_cd$district <- gsub("-", " ", df_91_cd$district,fixed = T)
df_01_cd$district <- gsub(".", "", df_01_cd$district,fixed = T)
df_01_cd$district <- gsub("-", " ", df_01_cd$district,fixed = T)
df_11_csd$district <- gsub(".", "", df_11_csd$district,fixed = T)
df_11_csd$district <- gsub("-", " ", df_11_csd$district,fixed = T)
df_11_csd$subdistrict <- gsub(".", "", df_11_csd$subdistrict,fixed = T)
df_11_csd$subdistrict <- gsub("-", " ", df_11_csd$subdistrict,fixed = T)
df_21_csd$subdistrict <- gsub(".", "", df_21_csd$subdistrict,fixed = T)
df_21_csd$subdistrict <- gsub("-", " ", df_21_csd$subdistrict,fixed = T)
df_11_cd$district <- gsub(".", "", df_11_cd$district,fixed = T)
df_11_cd$district <- gsub("-", " ", df_11_cd$district,fixed = T)
df_21_cd$district <- gsub(".", "", df_21_cd$district,fixed = T)
df_21_cd$district <- gsub("-", " ", df_21_cd$district,fixed = T)
df_21_csd$district <- gsub(".", "", df_21_csd$district,fixed = T)
df_21_csd$district <- gsub("-", " ", df_21_csd$district,fixed = T)


df_51_cd$province <- gsub(".", "", df_51_cd$province,fixed = T)
df_51_cd$province <- gsub("-", " ", df_51_cd$province,fixed = T)
df_61_cd$province <- gsub(".", "", df_61_cd$province,fixed = T)
df_61_cd$province <- gsub("-", " ", df_61_cd$province,fixed = T)
df_71_cd$province <- gsub(".", "", df_71_cd$province,fixed = T)
df_71_cd$province <- gsub("-", " ", df_71_cd$province,fixed = T)
df_81_cd$province <- gsub(".", "", df_81_cd$province,fixed = T)
df_81_cd$province <- gsub("-", " ", df_81_cd$province,fixed = T)
df_91_cd$province <- gsub(".", "", df_91_cd$province,fixed = T)
df_91_cd$province <- gsub("-", " ", df_91_cd$province,fixed = T)
df_01_cd$province <- gsub(".", "", df_01_cd$province,fixed = T)
df_01_cd$province <- gsub("-", " ", df_01_cd$province,fixed = T)
df_11_csd$province <- gsub(".", "", df_11_csd$province,fixed = T)
df_11_csd$province <- gsub("-", " ", df_11_csd$province,fixed = T)
df_21_csd$province <- gsub(".", "", df_21_csd$province,fixed = T)
df_21_csd$province <- gsub("-", " ", df_21_csd$province,fixed = T)
df_11_cd$province <- gsub(".", "", df_11_cd$province,fixed = T)
df_11_cd$province <- gsub("-", " ", df_11_cd$province,fixed = T)
df_21_cd$province <- gsub(".", "", df_21_cd$province,fixed = T)
df_21_cd$province <- gsub("-", " ", df_21_cd$province,fixed = T)
df_21_csd$province <- gsub(".", "", df_21_csd$province,fixed = T)
df_21_csd$province <- gsub("-", " ", df_21_csd$province,fixed = T)

df_51_cd$district <- gsub(".", "", df_51_cd$district,fixed = T)
df_51_cd$district <- gsub("-", " ", df_51_cd$district,fixed = T)
df_51_cd$district <- gsub("(city)", " City", df_51_cd$district,fixed = T)
df_51_cd$district <- gsub("Saint", "St", df_51_cd$district,fixed = T)
df_51_cd$district <- gsub("(colony)", "", df_51_cd$district,fixed = T)

df_61_cd$district <- gsub(".", "", df_61_cd$district,fixed = T)
df_61_cd$district <- gsub("-", " ", df_61_cd$district,fixed = T)
df_61_cd$district <- gsub("(city)", " City", df_61_cd$district,fixed = T)
df_61_cd$district <- gsub("Saint", "St", df_61_cd$district,fixed = T)
df_61_cd$district <- gsub("(colony)", "", df_61_cd$district,fixed = T)

df_71_cd$district <- gsub(".", "", df_71_cd$district,fixed = T)
df_71_cd$district <- gsub("-", " ", df_71_cd$district,fixed = T)
df_71_cd$district <- gsub("(city)", " City", df_71_cd$district,fixed = T)
df_71_cd$district <- gsub("Saint", "St", df_71_cd$district,fixed = T)
df_71_cd$district <- gsub("(colony)", "", df_71_cd$district,fixed = T)
df_71_cd$district <- gsub("(town)", " Town", df_71_cd$district,fixed = T)

df_81_cd$district <- gsub(".", "", df_81_cd$district,fixed = T)
df_81_cd$district <- gsub("-", " ", df_81_cd$district,fixed = T)
df_81_cd$district <- gsub("(city)", " City", df_81_cd$district,fixed = T)
df_81_cd$district <- gsub("Saint", "St", df_81_cd$district,fixed = T)
df_81_cd$district <- gsub("(colony)", "", df_81_cd$district,fixed = T)

df_91_cd$district <- gsub(".", "", df_91_cd$district,fixed = T)
df_91_cd$district <- gsub("-", " ", df_91_cd$district,fixed = T)
df_91_cd$district <- gsub("(city)", " City", df_91_cd$district,fixed = T)
df_91_cd$district <- gsub("Saint", "St", df_91_cd$district,fixed = T)
df_91_cd$district <- gsub("(colony)", "", df_91_cd$district,fixed = T)

df_01_cd$district <- gsub(".", "", df_01_cd$district,fixed = T)
df_01_cd$district <- gsub("-", " ", df_01_cd$district,fixed = T)
df_01_cd$district <- gsub("(city)", " City", df_01_cd$district,fixed = T)
df_01_cd$district <- gsub("Saint", "St", df_01_cd$district,fixed = T)
df_01_cd$district <- gsub("(colony)", "", df_01_cd$district,fixed = T)

df_11_cd$district <- gsub(",", "", df_11_cd$district,fixed = T)
df_11_csd$district <- gsub(",", "", df_11_csd$district,fixed = T)
df_21_cd$district <- gsub(",", "", df_21_cd$district,fixed = T)
df_21_csd$district <- gsub(",", "", df_21_csd$district,fixed = T)
df_21_csd$subdistrict <- gsub(",", "", df_21_csd$subdistrict,fixed = T)
df_21_csd$subdistrict <- gsub(",", "", df_21_csd$subdistrict,fixed = T)

df_21_csd$subdistrict <- gsub("[0-9]+","",df_21_csd$subdistrict)
df_21_csd$subdistrict <- trimws(df_21_csd$subdistrict)

df_21_csd$district <- gsub(" Est"," East",df_21_csd$district)
df_21_csd$district <- gsub(" Ouest"," West",df_21_csd$district)
df_21_csd$district <- gsub(" Nord"," North",df_21_csd$district)
df_21_csd$district <- gsub(" Sud"," South",df_21_csd$district)

df_51_cd$province[which(df_51_cd$province == "Canada  West")] <- "Ontario"
df_61_cd$province[which(df_61_cd$province == "Canada West")] <- "Ontario"
df_51_cd$province[which(df_51_cd$province == "Canada East")] <- "Quebec"
df_61_cd$province[which(df_61_cd$province == "Canada East")] <- "Quebec"

df_11_cd$province[which(df_11_cd$province == "ON")] <- "Ontario"
df_11_cd$province[which(df_11_cd$province == "AB")] <- "Alberta"
df_11_cd$province[which(df_11_cd$province == "MB")] <- "Manitoba"
df_11_cd$province[which(df_11_cd$province == "BC")] <- "British Columbia"
df_11_cd$province[which(df_11_cd$province == "NB")] <- "New Brunswick"
df_11_cd$province[which(df_11_cd$province == "NL")] <- "Newfoundland"
df_11_cd$province[which(df_11_cd$province == "NS")] <- "Nova Scotia"
df_11_cd$province[which(df_11_cd$province == "NT")] <- "North Western Territories"
df_11_cd$province[which(df_11_cd$province == "PE")] <- "Prince Edward Island"
df_11_cd$province[which(df_11_cd$province == "QC")] <- "Quebec"
df_11_cd$province[which(df_11_cd$province == "SK")] <- "Saskatchewan"
df_11_cd$province[which(df_11_cd$province == "YT")] <- "Yukon"

df_11_csd$province[which(df_11_csd$province == "ON")] <- "Ontario"
df_11_csd$province[which(df_11_csd$province == "AB")] <- "Alberta"
df_11_csd$province[which(df_11_csd$province == "MB")] <- "Manitoba"
df_11_csd$province[which(df_11_csd$province == "BC")] <- "British Columbia"
df_11_csd$province[which(df_11_csd$province == "NB")] <- "New Brunswick"
df_11_csd$province[which(df_11_csd$province == "NL")] <- "Newfoundland"
df_11_csd$province[which(df_11_csd$province == "NS")] <- "Nova Scotia"
df_11_csd$province[which(df_11_csd$province == "NT")] <- "North Western Territories"
df_11_csd$province[which(df_11_csd$province == "PE")] <- "Prince Edward Island"
df_11_csd$province[which(df_11_csd$province == "QC")] <- "Quebec"
df_11_csd$province[which(df_11_csd$province == "SK")] <- "Saskatchewan"
df_11_csd$province[which(df_11_csd$province == "YT")] <- "Yukon"

df_21_csd$province[which(df_21_csd$province == "ON")] <- "Ontario"
df_21_csd$province[which(df_21_csd$province == "AB")] <- "Alberta"
df_21_csd$province[which(df_21_csd$province == "MB")] <- "Manitoba"
df_21_csd$province[which(df_21_csd$province == "BC")] <- "British Columbia"
df_21_csd$province[which(df_21_csd$province == "NB")] <- "New Brunswick"
df_21_csd$province[which(df_21_csd$province == "NL")] <- "Newfoundland"
df_21_csd$province[which(df_21_csd$province == "NS")] <- "Nova Scotia"
df_21_csd$province[which(df_21_csd$province == "NT")] <- "North Western Territories"
df_21_csd$province[which(df_21_csd$province == "PE")] <- "Prince Edward Island"
df_21_csd$province[which(df_21_csd$province == "QC")] <- "Quebec"
df_21_csd$province[which(df_21_csd$province == "SK")] <- "Saskatchewan"
df_21_csd$province[which(df_21_csd$province == "YT")] <- "Yukon"

df_21_cd$province[which(df_21_cd$province == "ON")] <- "Ontario"
df_21_cd$province[which(df_21_cd$province == "AB")] <- "Alberta"
df_21_cd$province[which(df_21_cd$province == "MB")] <- "Manitoba"
df_21_cd$province[which(df_21_cd$province == "BC")] <- "British Columbia"
df_21_cd$province[which(df_21_cd$province == "NB")] <- "New Brunswick"
df_21_cd$province[which(df_21_cd$province == "NL")] <- "Newfoundland"
df_21_cd$province[which(df_21_cd$province == "NS")] <- "Nova Scotia"
df_21_cd$province[which(df_21_cd$province == "NT")] <- "North Western Territories"
df_21_cd$province[which(df_21_cd$province == "PE")] <- "Prince Edward Island"
df_21_cd$province[which(df_21_cd$province == "QC")] <- "Quebec"
df_21_cd$province[which(df_21_cd$province == "SK")] <- "Saskatchewan"
df_21_cd$province[which(df_21_cd$province == "YT")] <- "Yukon"

df_51_cd$province[which(df_51_cd$province == "Territories")] <- "North Western Territories"
df_61_cd$province[which(df_61_cd$province == "Territories")] <- "North Western Territories"
df_71_cd$province[which(df_71_cd$province == "North West Territories")] <- "North Western Territories"
df_81_cd$province[which(df_81_cd$province == "North West Territories")] <- "North Western Territories"
df_91_cd$province[which(df_91_cd$province == "North West Territories")] <- "North Western Territories"
df_01_cd$province[which(df_01_cd$province == "Territories")] <- "North Western Territories" # this was commented before, don't know why
df_01_cd$province[which(df_01_cd$province == "Yukon Territory")] <- "Yukon" # this was commented before, don't know why

df_01_cd$district[which(df_01_cd$district == "StMaurice & Trois Rivieres")] <- "Trois Rivieres & St Maurice"


df_81_cd$province[which(df_81_cd$province == "Ontario/Manitoba disputed")] <- "Ontario"
df_21_cd$district <- gsub("Division No ", "", df_21_cd$district,fixed = T)
df_21_csd$district <- gsub("Division No ", "", df_21_csd$district,fixed = T)
df_11_cd$district[which(df_11_cd$district == "Lennox and Addington")] <- "Lennox & Addington"
df_11_csd$district[which(df_11_csd$district == "Lennox and Addington")] <- "Lennox & Addington"
df_11_cd$district[which(df_11_cd$district == "Ottawa City (part)")] <- "Ottawa City"
df_11_csd$district[which(df_11_csd$district == "Ottawa City (part)")] <- "Ottawa City"
df_11_cd$district[which(df_11_cd$district == "Thunder Bay and Rainy River")] <- "Thunder Bay & Rainy River"
df_11_csd$district[which(df_11_csd$district == "Thunder Bay and Rainy River")] <- "Thunder Bay & Rainy River"
df_21_cd$district[which(df_21_cd$district == "L'IsIet")] <- "L'Islet"
df_21_cd$district[str_detect(df_21_cd$district,"Montreal")] <- "Montreal & Jesus Island"
df_21_csd$district[str_detect(df_21_csd$district,"Montreal")] <- "Montreal & Jesus Island"

df_11_csd$subdistrict[which(grepl("Ladysmith",df_11_csd$subdistrict, fixed = T))] <- "Ladysmith"
df_11_csd$subdistrict[which(grepl("Berlin [Kitchener]",df_11_csd$subdistrict, fixed = T))] <- "Berlin"
df_21_csd$subdistrict[which(grepl("Berlin [Kitchener]",df_21_csd$subdistrict, fixed = T))] <- "Berlin"

df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Easthope North")] <- "North Easthope"
df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Easthope South")] <- "South Easthope"
df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Vancouver North")] <- "North Vancouver"
df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Vancouver South")] <- "South Vancouver"
df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Zorra West")] <- "West Zorra"
df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Zorra East")] <- "East Zorra"
df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Hatley Nord")] <- "North Hatley"
df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Gower North")] <- "North Gower"

df_21_csd$subdistrict[which(df_21_csd$subdistrict == "Easthope North")] <- "North Easthope"
df_21_csd$subdistrict[which(df_21_csd$subdistrict == "Easthope South")] <- "South Easthope"
df_21_csd$subdistrict[which(df_21_csd$subdistrict == "Zorra West")] <- "West Zorra"
df_21_csd$subdistrict[which(df_21_csd$subdistrict == "Zorra East")] <- "East Zorra"
df_21_csd$subdistrict[which(df_21_csd$subdistrict == "Hatley North")] <- "North Hatley"
df_21_csd$subdistrict[which(df_21_csd$subdistrict == "Hatley West")] <- "West Hatley"
df_21_csd$subdistrict[which(df_21_csd$subdistrict == "Gower North")] <- "North Gower"
df_21_csd$subdistrict[which(df_21_csd$subdistrict == "Gower South")] <- "South Gower"

df_11_csd$subdistrict[which(df_11_csd$subdistrict == "St Catherines")] <- "St Catharines"
df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Vermillion")] <- "Vermilion"
df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Moosejaw")] <- "Moose Jaw"
df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Newboro")] <- "Newboro'"
df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Parrsboro")] <- "Parrsboro'"
df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Laprairie")] <- "La Prairie"
df_11_csd$subdistrict[which(df_11_csd$subdistrict == "St Boniface (Shawinigan)")] <- "Shawinigan"
df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Grand'Mere")] <- "Grand Mere"
df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Three Rivers")] <- "Trois Rivieres"
df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Southhampton")] <- "Southampton"
df_11_csd$subdistrict[which(df_11_csd$subdistrict == "Three Rivers")] <- "Trois Rivieres"

df_21_csd$subdistrict[which(df_21_csd$subdistrict == "St Catherines")] <- "St Catharines"
df_21_csd$subdistrict <- gsub("Vermillion","Vermilion",df_21_csd$subdistrict)
df_21_csd$subdistrict[which(df_21_csd$subdistrict == "Moosejaw")] <- "Moose Jaw"
df_21_csd$subdistrict[which(df_21_csd$subdistrict == "Newboro")] <- "Newboro'"
df_21_csd$subdistrict[which(df_21_csd$subdistrict == "Parrsboro")] <- "Parrsboro'"
df_21_csd$subdistrict[which(df_21_csd$subdistrict == "Laprairie")] <- "La Prairie"

df_11_cd$district[which(df_11_cd$UID_CD_11 == "QC189")] <- "Quebec West"
df_11_csd$district[which(df_11_csd$UID_CD_11 == "QC189")] <- "Quebec West"

df_11_csd$subdistrict[which(df_11_csd$subdistrict == "StMartins")] <- "St Martins"

df_61_cd$district[which(df_61_cd$district == "St Johns")] <- "St Jean"
df_71_cd$district[which(df_71_cd$district == "St Johns")] <- "St Jean"
df_71_cd$district[which(df_71_cd$district == "Victoria" & df_71_cd$province == "Nova Scotia")] <- "Victoria"

df_51_cd$district[which(df_51_cd$district == "Bytown City")] <- "Ottawa City"

df_91_cd$district[which(str_detect(df_91_cd$district,"Algoma"))] <- "Algoma"

df_21_csd$subdistrict[which(df_21_csd$subdistrict == "N D des Anges de Stanbridge")] <- "Notre Dame des Anges de Stanbridge"
df_21_csd$subdistrict[which(df_21_csd$subdistrict == "St Gerard de Majella")] <- "St Gerard de Magella"

# 71 has Halifax splitted in two. Join them as is it in 51 and 61

df_71_cd$aux <- paste(df_71_cd$province,df_71_cd$district,sep = "_")
df_71_cd <- aggregate(df_71_cd[,-which(colnames(df_71_cd) %in% c("aux","district","province"))],
                      by =list(df_71_cd$aux), FUN = function(x) sum(x, na.rm = T), do_union = T)
df_71_cd <- rename(df_71_cd,c("aux" = "Group.1"))
df_71_cd <- separate(df_71_cd,"aux", into = c("province","district"), sep = "_")

# 11 has 3 NWT with same ID

df_11_cd <- aggregate(df_11_cd[,-which(colnames(df_11_cd) %in% c("UID_CD_11"))],
                      by =list(df_11_cd$UID_CD_11), FUN = function(x) extract(x,1), do_union = T)
df_11_cd <- rename(df_11_cd,c("UID_CD_11" = "Group.1"))

# add population

df_11_cd <- left_join(df_11_cd,pop_11_cd[,c("UID_CD_11","area","population","pop_01")], by = "UID_CD_11")
df_11_csd <- left_join(df_11_csd,pop_11_csd[,c("UID_CSD_11","area","population","pop_01")], by = "UID_CSD_11") 
df_21_cd <- left_join(df_21_cd,pop_21_cd[,c("UID_CD_21","area","population")], by = "UID_CD_21")
df_21_csd <- left_join(df_21_csd,pop_21_csd[,c("UID_CSD_21","area","population")], by = "UID_CSD_21") 

# change NAs for zeros in population

df_51_cd$population[which(is.na(df_51_cd$population))] <- 0
df_61_cd$population[which(is.na(df_61_cd$population))] <- 0
df_71_cd$population[which(is.na(df_71_cd$population))] <- 0
df_81_cd$population[which(is.na(df_81_cd$population))] <- 0
df_91_cd$population[which(is.na(df_91_cd$population))] <- 0
df_01_cd$population[which(is.na(df_01_cd$population))] <- 0
df_11_cd$population[which(is.na(df_11_cd$population))] <- 0
df_11_csd$population[which(is.na(df_11_csd$population))] <- 0
df_21_cd$population[which(is.na(df_21_cd$population))] <- 0
df_21_csd$population[which(is.na(df_21_csd$population))] <- 0

# Align maps

# # This commented part was to try examples of polygons to know how to rotate and translate the maps
# 
# b1 <- df_71_cd %>% select(district) %>% filter(district %in% c("Essex"))
# b2 <- df_11_csd %>% filter(subdistrict %in% c("Windsor"), province == "Ontario") %>% select(subdistrict,geometry)
# # b2 <- df_11_cd %>% filter(district %in% c("Essex North"), province == "Ontario") %>% select(district,geometry)
# 
# 
# b1 <- df_71_cd %>% select(district) %>% filter(district %in% c("Joliette"))
# b2 <- df_11_cd %>% filter(district %in% c("Joliette")) %>% select(district,geometry)
# 
# 
# b1 <- df_71_cd %>% select(district) %>% filter(district %in% c("Jacques Cartier","Hochelaga","Montreal City"))
# # b2 <- df_11_csd %>% filter(subdistrict %in% c("Windsor"), province == "Ontario") %>% select(subdistrict,geometry)
# b2 <- df_11_cd %>% filter(district %in% c("Jacques Cartier")) %>% select(district,geometry)
# #
# 
# 
# b1 <- df_91_cd %>% select(district) %>% filter(district %in% c("Dundas")) # Example 2 for robustness
# b2 <- df_11_cd %>% filter(district %in% c("Dundas")) %>% select(district,geometry)
# 
# 
# rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
# 
# # b1$geometry <- (b1$geometry - st_centroid(b1$geometry))*rot(0.0175) + c(0.055,0.005) + st_centroid(b1$geometry) # This one matches exactly part zoomed
# b1$geometry <- (b1$geometry - st_centroid(b1$geometry))*rot(0) + c(4000,2000) + st_centroid(b1$geometry)
# b1 <- st_set_crs(b1,st_crs(b2))
# #
# # b3 <- rbind(b1,b2)
# #
# # b1c <- st_crop(b1, xmin = -74, xmax = -72, # Uncomment for Zooming
# #                ymin = 45.5, ymax = 46.5)
# # b2c <- st_crop(b2, xmin = -74, xmax = -72,
# #                ymin = 45.5, ymax = 46.5)
# #
# ggplot() +
#   geom_sf(data = b1, color=alpha("red",1), fill = NA) +
#   geom_sf(data = b2, color=alpha("blue",1), fill = NA)




rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

# df_51_cd$geometry <- (df_51_cd$geometry - st_centroid(df_51_cd$geometry))*rot(0.005) + c(0.055,-0.008) + st_centroid(df_51_cd$geometry)
# df_51_cd <- st_set_crs(df_51_cd,st_crs(df_11_cd))
# df_61_cd$geometry <- (df_61_cd$geometry - st_centroid(df_61_cd$geometry))*rot(0.005) + c(0.055,-0.008) + st_centroid(df_61_cd$geometry)
# df_61_cd <- st_set_crs(df_61_cd,st_crs(df_11_cd))
# df_71_cd$geometry <- (df_71_cd$geometry - st_centroid(df_71_cd$geometry))*rot(0.005) + c(0.055,-0.008) + st_centroid(df_71_cd$geometry)
# df_71_cd <- st_set_crs(df_71_cd,st_crs(df_11_cd))
# df_81_cd$geometry <- (df_81_cd$geometry - st_centroid(df_81_cd$geometry))*rot(0.005) + c(0.055,-0.008) + st_centroid(df_81_cd$geometry)
# df_81_cd <- st_set_crs(df_81_cd,st_crs(df_11_cd))
# df_91_cd$geometry <- (df_91_cd$geometry - st_centroid(df_91_cd$geometry))*rot(0.005) + c(0.055,-0.008) + st_centroid(df_91_cd$geometry)
# df_91_cd <- st_set_crs(df_91_cd,st_crs(df_11_cd))
# df_01_cd$geometry <- (df_01_cd$geometry - st_centroid(df_01_cd$geometry))*rot(0.005) + c(0.055,-0.008) + st_centroid(df_01_cd$geometry)
# df_01_cd <- st_set_crs(df_01_cd,st_crs(df_11_cd))

df_51_cd$geometry <- (df_51_cd$geometry - st_centroid(df_51_cd$geometry))*rot(0) + c(3000,2000) + st_centroid(df_51_cd$geometry)
df_51_cd <- st_set_crs(df_51_cd,st_crs(df_11_cd))
df_61_cd$geometry <- (df_61_cd$geometry - st_centroid(df_61_cd$geometry))*rot(0) + c(3000,2000) + st_centroid(df_61_cd$geometry)
df_61_cd <- st_set_crs(df_61_cd,st_crs(df_11_cd))
df_71_cd$geometry <- (df_71_cd$geometry - st_centroid(df_71_cd$geometry))*rot(0) + c(3000,2000) + st_centroid(df_71_cd$geometry)
df_71_cd <- st_set_crs(df_71_cd,st_crs(df_11_cd))
df_81_cd$geometry <- (df_81_cd$geometry - st_centroid(df_81_cd$geometry))*rot(0) + c(3000,2000) + st_centroid(df_81_cd$geometry)
df_81_cd <- st_set_crs(df_81_cd,st_crs(df_11_cd))
df_91_cd$geometry <- (df_91_cd$geometry - st_centroid(df_91_cd$geometry))*rot(0) + c(3000,2000) + st_centroid(df_91_cd$geometry)
df_91_cd <- st_set_crs(df_91_cd,st_crs(df_11_cd))
df_01_cd$geometry <- (df_01_cd$geometry - st_centroid(df_01_cd$geometry))*rot(0) + c(3000,2000) + st_centroid(df_01_cd$geometry)
df_01_cd <- st_set_crs(df_01_cd,st_crs(df_11_cd))

#

ref_51_cd <- as.data.frame(df_51_cd)
ref_51_cd <- ref_51_cd[,-which(colnames(ref_51_cd) == "geometry")]
ref_61_cd <- as.data.frame(df_61_cd)
ref_61_cd <- ref_61_cd[,-which(colnames(ref_61_cd) == "geometry")]
ref_71_cd <- as.data.frame(df_71_cd)
ref_71_cd <- ref_71_cd[,-which(colnames(ref_71_cd) == "geometry")]
ref_81_cd <- as.data.frame(df_81_cd)
ref_81_cd <- ref_81_cd[,-which(colnames(ref_81_cd) == "geometry")]
ref_91_cd <- as.data.frame(df_91_cd)
ref_91_cd <- ref_91_cd[,-which(colnames(ref_91_cd) == "geometry")]
ref_01_cd <- as.data.frame(df_01_cd)
ref_01_cd <- ref_01_cd[,-which(colnames(ref_01_cd) == "geometry")]
ref_11_cd <- as.data.frame(df_11_cd)
ref_11_cd <- ref_11_cd[,-which(colnames(ref_11_cd) == "geometry")]
ref_11_csd <- as.data.frame(df_11_csd)
ref_11_csd <- ref_11_csd[,-which(colnames(ref_11_csd) == "geometry")]
ref_21_cd <- as.data.frame(df_21_cd)
ref_21_cd <- ref_21_cd[,-which(colnames(ref_21_cd) == "geometry")]
ref_21_csd <- as.data.frame(df_21_csd)
ref_21_csd <- ref_21_csd[,-which(colnames(ref_21_csd) == "geometry")]

# ref_51_cd <- ref_51_cd %>% mutate(year = 1851)
# ref_61_cd <- ref_61_cd %>% mutate(year = 1861)
# ref_71_cd <- ref_71_cd %>% mutate(year = 1871)
# ref_81_cd <- ref_81_cd %>% mutate(year = 1881)
# ref_91_cd <- ref_91_cd %>% mutate(year = 1891)
# ref_01_cd <- ref_01_cd %>% mutate(year = 1901)
# ref_11_cd <- ref_11_cd %>% mutate(year = 1911)
# ref_21_cd <- ref_21_cd %>% mutate(year = 1921)

# train

trains <- st_transform(trains,crs = 32610)
trains <- trains %>% filter(CNSTRCTD > 0) # negative refers to unknown date and zero I don't know what it refers to.
trains <- trains %>% mutate_cond(ABNDND <= 0, ABNDND = 2021) # I interpret ABNDND <= 0 as not abandoned, although < 0 actually refers to unknown date
trains <- trains %>% mutate_cond(ABNDND == 1, ABNDND = 2021) # There's one row with equal to 1, probably a typo

trains <- trains %>% select(-OBJECTID,-FNODE_,-TNODE_,-LPOLY_,-RPOLY_,-LENGTH,-ALRAILS_,
                            -ALRAILS_ID,-FCODE,-Shape_Leng,-source_fro,-TC1917,-TC1992)

colnames(trains) <- c("builder","inscription","year","abandoned","transcan","geometry")

# stage coach

stage <- st_as_sf(stage, crs = 32610)

stage <- stage[-which(is.na(stage$edge)),]
stage <- stage %>% select(-name)
stage$year[which(is.na(stage$year) & !(stage$edge == "Winnipeg_Edmonton"))] <- 1836 # assume this for NB edges
stage <- stage[-which(stage$edge == "Winnipeg_Edmonton"),] # I think this route wasn't really used extensively for commerce

# water

water <- water %>% filter(!(NAME == "UNK")) %>% select(-ISO,-COUNTRY,-F_CODE_DES)
water <- st_transform(water, crs = 32610)
water_subset <- as.vector(unlist(water_subset)) 
water_complement <- st_transform(water_complement, crs = 32610)

colnames(water) <- c("permanent","name","geometry")
water <- water %>% mutate(permanent = ifelse(permanent == "Perennial/Permanent",1,0))
water <- water %>% select(-permanent)

water$name <- stri_trans_general(water$name ,"Latin-ASCII")
water$name <- gsub("-", " ", water$name,fixed = T)
water$name <- gsub(".", "", water$name,fixed = T)
water$name <- str_to_title(water$name)

water <- water %>% filter(name %in% water_subset)


# remove polygons which I don't need. Usually just tiny waters.

water <- lapply(1:nrow(water), function(i) {
  st_cast(water[i, ], "POLYGON")
}) %>%
  do.call(rbind, .)

a <- st_transform(water, crs = 4326)

good_polys <- data.frame(st_coordinates(a)) %>%
  group_by(L2) %>%
  mutate(max_x = Y > 55) 

good_polys <- good_polys %>% group_by(L2) %>%
  mutate(z = any(max_x == T)) %>%
  distinct(L2, z)

water <- a[!good_polys$z,]

a <- st_transform(water, crs = 4326)

good_polys <- data.frame(st_coordinates(a)) %>%
  group_by(L2) %>%
  mutate(max_x = X > -85) %>%
  mutate(max_y = Y > 51)

good_polys <- good_polys %>% group_by(L2) %>%
  mutate(max = max_x*max_y)

good_polys <- good_polys %>% group_by(L2) %>%
  mutate(z = any(max == T)) %>%
  distinct(L2, z)

water <- a[!good_polys$z,]
water <- st_transform(water,crs = 32610)
#

# water_complement$permanent <- 1
water <- rbind(water,water_complement)
# water <- water[which(as.numeric(st_area(water$geometry))  > 1e4 | as.numeric(st_area(water$geometry)) == 0),]

water <- aggregate(water[,-which(colnames(water) %in% c("name"))],
                            by =list(water$name), FUN = function(x) extract(x,1), do_union = T)
colnames(water) <- c("name","geometry")
water$construction <- 1800
water$year <- 1800
water$depth <- NA


# canals


canals <- st_transform(canals,crs = 32610)

# change geometries for trivial polygons constructed which are actually innovations on previous canals.

for (i in 1:length(canals$geometry)){
canals$z[i] <- length(canals$geometry[[i]])  
}

for (i in unique(canals$name)){
  
  if (any(canals$z[which(canals$name == i)] == 4) & any(canals$z[which(canals$name == i)] > 4)){
    
    canals$geometry[which(canals$name == i & canals$z == 4)] <- canals$geometry[which(canals$name == i & !(canals$z == 4))]
    canals$z[which(canals$name == i & canals$z == 4)] <- canals$z[which(canals$name == i & !(canals$z == 4))]
    
  } else if (any(canals$z[which(canals$name == i)] == 4) & any(canals$z[which(canals$name == i)] == 1))
    
    canals$geometry[which(canals$name == i & canals$z == 4)] <- canals$geometry[which(canals$name == i & !(canals$z == 4))]
    canals$z[which(canals$name == i & canals$z == 4)] <- canals$z[which(canals$name == i & !(canals$z == 4))]
  
}

id <- st_intersects(canals[which(canals$z == 1),],water) # see which natural waterway was improved by canal
water <- rbind(water,canals[which(canals$z > 1),-which(colnames(canals) == "z")]) # add constructured canals that created a new waterway to water
canals <- canals %>% filter(z == 1) # keep just canals that improved natural waterways


# give innovations to natural waterway same geometry and name as natural waterway
for (i in 1:length(id)){
  
  canals$geometry[i] <- water$geometry[id[[i]]]
  canals$name[i] <- water$name[id[[i]]]
  
}

# add improvements in natural waterways to water
water<- rbind(water,canals[,-which(colnames(canals) == "z")])
rm(canals)

# oceans

oceans <- oceans %>% select(-HYDROUID,-PRUID)
colnames(oceans) <- c("name","geometry")

oceans$name <- stri_trans_general(oceans$name ,"Latin-ASCII")
oceans$name <- gsub("-", " ", oceans$name,fixed = T)
oceans$name <- gsub(".", "", oceans$name,fixed = T)



a <- st_transform(oceans, crs = 4326)

good_polys <- data.frame(st_coordinates(a)) %>%
  group_by(L2) %>%
  mutate(max_x = X > 0) 

good_polys <- good_polys %>% group_by(L2) %>%
  mutate(z = any(max_x == T)) %>%
  distinct(L2, z)

oceans <- a[!good_polys$z,]

a <- st_transform(oceans, crs = 4326)

good_polys <- data.frame(st_coordinates(a)) %>%
  group_by(L2) %>%
  mutate(max_x = X < -50) %>%
  mutate(max_y = Y > 61)

good_polys <- good_polys %>% group_by(L2) %>%
  mutate(max = max_x*max_y)

good_polys <- good_polys %>% group_by(L2) %>%
  mutate(z = any(max == T)) %>%
  distinct(L2, z)

oceans <- a[!good_polys$z,]

a <- st_transform(oceans, crs = 4326)

good_polys <- data.frame(st_coordinates(a)) %>%
  group_by(L2) %>%
  mutate(max_y = Y > 55)

good_polys <- good_polys %>% group_by(L2) %>%
  mutate(z = any(max_y == T)) %>%
  distinct(L2, z)

oceans <- a[!good_polys$z,]

oceans$area <- as.numeric(st_area(oceans$geometry)) 

oceans <- oceans %>% filter(area >= 29000)
# oceans <- oceans[!(str_detect(oceans$name,"Croix|Spednic|Sables")),] 

oceans <- st_transform(oceans, crs = 32610)

rm(good_polys)




ggplot() +
#   geom_sf(data = us) + 
  geom_sf(data = df_51_cd) +
  geom_sf(data = water, fill = "skyblue", color = "skyblue") +
  geom_sf(data = oceans, fill = "blue", color = "blue") +
#   geom_sf(data = oceans, fill = "skyblue", color = "skyblue") +
#   geom_sf(data = stage, fill = "red", color = "red") +
#   geom_sf(data = trains, fill = "yellow", color = "yellow") +
#   # coord_sf(crs = 4326,xlim = c(-75, -68), ylim = c(42, 48))
 coord_sf(crs = 4326)
#   coord_sf(crs = 4326,xlim = c(-140, -60), ylim = c(42, 80))