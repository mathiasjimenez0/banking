# setwd("C:/Users/Mathias/Dropbox/Mi PC (DESKTOP-HUKVD2P)/Desktop/Angela-Mathias")
setwd("D:/Dropbox/JMP/")

# Load data sources to save in R data files ####

# Load files from Curtis

filename <- "./Dataset/balance_sheets/Curtis.xlsx"
df <- read_excel(filename,sheet = 1)

filename <- "./Dataset/balance_sheets/Curtis.xlsx"
df_dom <- read_excel(filename,sheet = 2)

filename <- "./Dataset/balance_sheets/Curtis.xlsx"
df_pi  <- read_excel(filename,sheet = 3)

source("./Code/Clean_Curtis.R")

filename <- "./Dataset/R/Curtis_Dominion.rds"
saveRDS(df_dom,file= filename)
filename <- "./Dataset/R/Curtis_Price Index.rds"
saveRDS(df_pi,file= filename)

rm(df_dom,df_pi)

# Curtis IB accounts

IB_Csec <- rowSums(data.frame(df$loansbankscanadasec,df$loansdeptobankssec), na.rm = T)
IB_Csec_l <- rowSums(data.frame(df$loansfromdepbybankscanadasec, df$loansbanksfromcanadasec), na.rm = T)

IB_notC <-  rowSums(data.frame(df$duefrombanksnotcanuk,df$duefrombanksuk), na.rm = T)
IB_notC_l <-  rowSums(data.frame(df$duetobanksnotcanuk,df$duetobanksuk), na.rm = T)

IB_Cnotes <-rowSums(data.frame(df$noteschecksonbanks, df$notesonbanks, df$chequesonbanks, df$foreigncurr), na.rm = T) 

IB_Cdep <- rowSums(data.frame(df$depandduefrombankscanada,df$duefrombanks,
                              df$duefrombanksdailyex,df$loansdeptobanksunsec), na.rm = T)

IB_Cdep_l <- rowSums(data.frame(df$depandduetobankscanada,df$duetobankscanada,
                                df$duetobanksdailyex,df$loansfromdepbybankscanadaunsec), na.rm = T)

df_IB_a <-as.data.frame(cbind("date" = df$date,IB_Cdep,IB_Cdep_l,IB_Cnotes,IB_Csec,IB_Csec_l,IB_notC,IB_notC_l))  
df_IB_a$date <- as.Date(df_IB_a$date)

filename <- "./Dataset/balance_sheets/Aggregate References.xlsx"
var_lab_a <- read_excel(filename, sheet = 1)
var_lab_al <- read_excel(filename, sheet = 2)
var_lab_aa <- read_excel(filename, sheet = 3)

rm(list=setdiff(ls(), c("df","df_IB_a","var_lab_a","var_lab_al","var_lab_aa")))

filename <- "./Dataset/R/Aggregate_bs.RData"
save.image(file = filename)

# Load LSE data

df_lse <- read.csv("./Dataset/securities/Totaldata_LSE.txt")
filename <- "./Dataset/R/LSE.rds"
saveRDS(df_lse,file= filename)

# Load losses statistics from Quigley

filename <- "./Dataset/misc/Losses.xlsx"
df_loss <- read_excel(filename, sheet = 1)
saveRDS(df_loss,file = "./Dataset/R/Losses.rds")

# Load National Banknotes from US from FRED from 1874 to 1919

filename <- "./Dataset/misc/National Banknotes FRED.xls"
df_nb <- read_excel(filename, sheet = 1, range = "A11:B563")
df_nb[,1] <- seq.Date(as.Date("1874-01-01"),as.Date("1919-12-01"),"month") 
saveRDS(df_nb,file = "./Dataset/R/USnotes.rds")


# Load variable names references

rm(list = ls()) # remove previous to save entire workspace

filename <- "./Dataset/balance_sheets/Individual References.xlsx"
var_lab_ia <- read_excel(filename, sheet = "assetnames")
var_lab_il <- read_excel(filename, sheet = "liabnames")
var_lab_in <- read_excel(filename, sheet = "banks")

var_lab_in$bank <- c("Montreal","Quebec","N Brunswick","Halifax Bk Co","Nova Scotia","Du Peuple","St Stephens","BN America","Molsons","Toronto",
                     "Union H","Bank of PEI","Ontario","E Townships","La Banq Nat","Yarmouth","Union PEI","Jacq Cartier","B Columbia","Peoples H",
                     "Peoples NB","Central","Windsor","Mechanics","Royal Canad","Commerce","Ville Marie","Exchange Yarmouth","Merchants","Dominion",
                     "Merchants PEI","Metropolitan","Hamilton","Exchange","Ville Marie Montreal","St Jean","St Hyacinthe","Imperial","DHochelaga","Ottawa",
                     "Federal","Standard","Western","London","Com Bk of Man","Traders","Union","La Banq Prov","Royal","Sovereign",
                     "Metro of M", "Crown","Home","Sterling","Empire","Northern","Farmers","Summerside","Northern Crown","Pictou",
                     "Weyburn","Cons Bk of C","Canadienne","Stadacona","Commercial","Gore","Commercial NB","Acadia","Niagara","St Lawrence",
                     "Liverpool","Mar Bk of Do","Vancouver", "City Bank","Upper Canada","Brantford","Central NB","Westmorland")

# International and Zimmerman (1855-1863, changed name in 1857 to Bank of Clifton after owned died) not included


filename <- "./Dataset/misc/Turnover.xlsx"
turnover <- read_excel(filename, sheet = 1)
turnover <- melt(data = turnover, id.vars = c("year"), measure.vars = as.character(c(1:NROW(var_lab_in$IDNO))),value.name = "turnover")
turnover <- rename(turnover,"IDNO" = "variable")
turnover <- turnover %>%
  separate(turnover, c("turnover", "month","IDNO1","IDNO2"), "_" )
turnover <- lapply(turnover, function(x){as.numeric(as.character(x))})
turnover <- as.data.frame(turnover)


filename <- "./Dataset/misc/Clean_bs.xlsx"
ref_50s <- read_excel(filename, sheet = "1857-1871")
ref_70s <- read_excel(filename, sheet = "1874-1889")
ref_00s <- read_excel(filename, sheet = "1899-1905")
ref_09 <- read_excel(filename, sheet = "1909-1913")
ref_06 <- read_excel(filename, sheet = "1906")
ref_13 <- read_excel(filename, sheet = "1913")

source("./Code/last_day.R")
source("./Code/Clean_bs.R")

rm(list=ls()[! ls() %in% c("df_all",
"var_lab_i","var_lab_in","var_lab_ia","var_lab_il","turnover")])

save.image(file = "./Dataset/R/Individual_bs.RData")






rm(list = ls())

change = 0

if (change == 0){
  
  # Census
  
  pop_11 <- read_excel("Dataset/census/pop_1901-1911_csd.xls")
  pop_21 <- read_excel("Dataset/census/pop_1921_csd.xls")
  
  # filename <- "./Dataset/census/Population_by_CDs_1851_1961.xls"
  # pop_21_cd <- read_excel(filename, sheet =  "1921")
  
  country_01 <- read_excel("Dataset/census/NatualProducts_1901_excel.xlsx",
                           sheet = "Sheet1")
  country_11 <- read_excel("Dataset/census/Agric_1911_excel.xlsx",
                           sheet = "Sheet1")
  
  manuf <- read_excel("Dataset/census/Manu_1911.xlsx",
                      sheet = "Sheet1")
  
  manuf_11 <- read_excel("Dataset/census/Manu_1911.xlsx",
                         sheet = "Sheet2")
  
  manuf_01 <- read_excel("Dataset/census/Jaworski/1901_raw.xlsx",
                         sheet = "Sheet1", skip = 1)
  manuf_91 <- read_excel("Dataset/census/Jaworski/1891_raw.xls",
                         sheet = "Sheet1", skip = 2)
  
  file.list <- list.files("./Dataset/census/Jaworski/1881_raw/", pattern = "*.xls", full.names = T)
  df.list_81 <- lapply(file.list,function(x) read_excel(x,skip = 4))
  df_end_81 <- read_excel("./Dataset/census/Jaworski/1881_raw/end/Pg160-174_LIV.xls", skip = 2)
  
  file.list <- list.files("./Dataset/census/Jaworski/1871_raw/", pattern = "*.xls", full.names = T)
  df.list_71 <- lapply(file.list,function(x) read_excel(x,skip = 4))
  df_end_71 <- read_excel("./Dataset/census/Jaworski/1871_raw/end/Pg148-157_LIII.xls", skip = 2)
  
  source("./Code/Clean_Census.R")
  
  # interest_rates
  
  icity <- read_excel("Dataset/interest_rates/interest_rates.xlsx",
                      sheet = 1)
  
  ifarm <- read_excel("Dataset/interest_rates/interest_rates.xlsx",
                      sheet = 2)
  
  iavg <- read_excel("Dataset/interest_rates/interest_rates.xlsx",
                     sheet = 3)
  
  imuni <- read_excel("./Dataset/interest_rates/interest_rates.xlsx",
                      sheet = 4)
  
  source("./Code/Clean_Rates.R")  
  
}

# GIS


if (change == 1){
  branches <- read_xlsx("./Dataset/gis/branches.xlsx", sheet = "change", col_types = c("text", "text", "text", "text", "numeric","text","numeric")) 
  load("./Dataset/R/branches_change.RData")
} else{
  df_51_cd <- st_read("./Dataset/gis/OpenContent_HACOLP_1851_CD_Canada/OpenContent_HACOLP_1851_CD_Canada.shp", stringsAsFactors = F)
  df_61_cd <- st_read("./Dataset/gis/OpenContent_HACOLP_1861_CD_Canada/OpenContent_HACOLP_1861_CD_Canada.shp", stringsAsFactors = F)
  df_71_cd <- st_read("./Dataset/gis/OpenContent_HACOLP_1871_CD_Canada/data/HACOLP_cds_1871.shp", stringsAsFactors = F)
  df_81_cd <- st_read("./Dataset/gis/OpenContent_HACOLP_1881_CD_Canada/data/HACOLP_cds_1881.shp", stringsAsFactors = F)
  df_91_cd <- st_read("./Dataset/gis/OpenContent_HACOLP_1891_CD_Canada/data/HACOLP_cds_1891.shp", stringsAsFactors = F)
  df_01_cd <- st_read("./Dataset/gis/OpenContent_HACOLP_1901_CD_Canada/data/HACOLP_cds_1901.shp", stringsAsFactors = F)
  df_11_csd <- st_read("./Dataset/gis/CCRI_CANADA_CENSUS_SUB_DIVISIONS_1911/Shapefile/CCRI_Shapefiles_CSD_1911/CANADA_CSD_1911_MW.shp", stringsAsFactors = F)
  df_11_cd <- st_read("./Dataset/gis/CCRI_CANADA_CENSUS_DIVISIONS_1911/Shapefile/CCRI_Shapefiles_CD_1911/CANADA_CD_1911_MW.shp", stringsAsFactors = F)
  df_21_cd <- st_read("./Dataset/gis/CCRI_CANADA_CENSUS_DIVISIONS_1921/Shapefile/CCRI_Shapefiles_CD_1921/CANADA_CD_1921_MW.shp", stringsAsFactors = F)
  df_21_csd <- st_read("./Dataset/gis/CCRI_CANADA_CENSUS_SUB_DIVISIONS_1921/Shapefile/CCRI_Shapefiles_CSD_1921/CANADA_CSD_1921_MW.shp", stringsAsFactors = F)
  
  branches <- read_xlsx("./Dataset/gis/branches.xlsx", sheet = "branches", col_types = c("text", "text", "text", "text", "numeric"))
  branches_f <- read_xlsx("./Dataset/gis/branches.xlsx", sheet = "foreign", skip = 1, col_types = c("text", "text", "text", "numeric"))
}

edges <- read_xlsx("./Dataset/gis/edges.xlsx", sheet = 1)
CC <- read_excel("Dataset/misc/CC.xlsx")
source("./Code/Clean_branches_edges.R")


# trains_place <- st_read("./Dataset/gis/Trains Canada/hr_places_all.shp", stringsAsFactors = F)
trains <- st_read("./Dataset/gis/Trains Canada/HR_rails_NEW.shp", stringsAsFactors = F)
stage <- geojson_sf("./Dataset/gis/stagecoach.geojson")

us <- st_read("./Dataset/gis/cb_2018_us_state_20m/cb_2018_us_state_20m.shp", stringsAsFactors = F)
us <- us %>% filter(!(NAME %in% c("Hawaii","Puerto Rico","Alaska")))
oceans <- st_read("./Dataset/gis/lhy_000h16a_e/lhy_000h16a_e.shp", stringsAsFactors = F)
water <- st_read("./Dataset/gis/CAN_wat/CAN_water_areas_dcw.shp", stringsAsFactors = F)
water_subset <- as.list(read.delim("./Dataset/gis/water_subset.txt", header = F, sep = ";")) 
water_complement <- geojson_sf("./Dataset/gis/water_complement.geojson")
canals <- geojson_sf("./Dataset/gis/canals.geojson")



# a <- st_read("./Dataset/census/early/Atlas.shp", stringsAsFactors = F)

# if (change == 0){
  source("./Code/Clean_GIS.R") 
  source("./Code/Life_cycle.R")  
# }
source("./Code/Crisis_union.R")
save.image("./half_time.RData")
print("finished union")
source("./Code/Crisis_match.R")
print("finished match")
save.image("./match.RData")
if (change == 0){
  source("./Code/Crisis_aggregate.R")
}


if (change == 1){
  load("./Dataset/R/GIS.RData")
  branches_change <- branches3
  
  branches_change <- branches_change %>% filter(add == 1)
  remove <- as.data.frame(branches_change) %>% filter(action == "remove") %>% select(-action,-geometry)
  if (NROW(remove$bank) > 0){
    branches_aux <- as.data.frame(branches) %>% select(-geometry) # I need to do this because exact branch location is different within subdistrict due to st_sample
    branches <- branches[-which(do.call(paste0, branches_aux) %in% do.call(paste0, remove)),]
  }
  add <- branches_change %>% filter(action == "add") %>% select(-action)
  branches <- rbind(branches,add)
  branches_old <- branches
  ref_branches <- as.data.frame(branches) %>% select(-geometry)

  branches_old_cd <- branches_cd
  ref_branches_cd <- as.data.frame(branches_cd) %>% select(-geometry)
    
  save(branches,ref_branches,branches_cd,ref_branches_cd,edges,df_11_csd,df_11_cd,ref_11_cd,ref_11_csd,edges_foreign,edges_canada,edges_count,
       df_prov,ref_prov,CC,df_11_cdcd,df_01_cd,df_91_cd,df_81_cd,df_71_cd,ref_11_cdcd,ref_01_cd,ref_91_cd,ref_81_cd,ref_71_cd,  file = "./Dataset/R/GIS.RData")
  save(branches_old,df_112,ref_112,df_11_match,ref_c_match,df_11_cdcd,df_01_cd,df_91_cd,df_81_cd,df_71_cd,ref_11_cdcd,ref_01_cd,ref_91_cd,ref_81_cd,ref_71_cd, file = "./Dataset/R/branches_change.RData")
    
} else{
  
  df_cd <- df_21_cd2
  df_csd <- df_2122
  ref_cd <- ref_21_cd2
  ref_csd <- ref_2122
  
  # df_prov <- df_11_prov
  # ref_prov <- ref_11_prov
  
  branches_old <- branches3
  branches <- branches3
  ref_branches <- as.data.frame(branches) %>% select(-geometry)
  
  branches_old_cd <- branches_cd
  ref_branches_cd <- as.data.frame(branches_cd) %>% select(-geometry)
  
  save(branches_old,branches_old_cd,df_212,ref_212,df_21_match,ref_c_match,df_11_cdcd,df_01_cd,df_91_cd,df_81_cd,df_71_cd,ref_11_cdcd,ref_01_cd,ref_91_cd,ref_81_cd,ref_71_cd, file = "./Dataset/R/branches_change.RData")
  save(ifarm,icity,iavg,imuni_csd,file = "./Dataset/R/interest_rates.RData")
  save(branches,ref_branches,branches_cd,ref_branches_cd,branches_f,edges,df_csd,df_cd,ref_cd,ref_csd,edges_foreign,edges_canada,edges_count,
       CC,CC_p,CC_g,df_11_cdcd,df_01_cd,df_91_cd,df_81_cd,df_71_cd,ref_11_cdcd,ref_01_cd,ref_91_cd,ref_81_cd,ref_71_cd, file = "./Dataset/R/GIS.RData")
  save(manuf_cd,manuf_csd,country_01_cd,country_11_cd,pop,pop_csd,manuf_11_cd,manuf_01_cd,manuf_91_cd,manuf_81_cd,manuf_71_cd, file = "./Dataset/R/Census.RData")
}

# END ####