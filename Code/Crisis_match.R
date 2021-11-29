#######################################################

# match

#######################################################

# if could not find place in map manually using google maps and agent isnot NA then use agent

for (i in 1:length(branches$place)){
  if (!(is.na(branches$agent[i])) & (branches$keep[i] %in% c("ny"))){
    branches$place[i] <- branches$agent[i]
    branches$province[i] <- branches$province_agent[i]
  }
  
  if (!(is.na(branches$agent[i])) & is.na(branches$province_agent[i]) & !(branches$keep[i] == "ny")){
    
    branches$province_agent[i] <- branches$province[i]
  }
}

if (change == 0){ # change == 0

df_21_match <- filter(df_212,!(str_detect(subdistrict,"Indian Reserves| Indian Reserves|LID|ND|T  R  W|T   R   W|Township|Unorganized Parts|Reserve indienne|Reserves Indiennes|Reserve Indienne|Indian reserves|No Data|Autres parties|Other Parts,
                                          Remainder of province|Unorganized Northern Parts|Northern Unorganized Parts|Rural Parts|
                                       Terr W of R 28 N of TP 15 S of TP 28|Unsurveyed parts north of TCR|Parties non organisees")))

ref_c_match <- filter(ref_212,!(str_detect(subdistrict,"Indian Reserves| Indian Reserves|LID|ND|T  R  W|T   R   W|Township|Unorganized Parts|Reserve indienne|Reserves Indiennes|Reserve Indienne|Indian reserves|No Data|Autres parties|Other Parts,
                                          Remainder of province|Unorganized Northern Parts|Northern Unorganized Parts|Rural Parts|
                                       Terr W of R 28 N of TP 15 S of TP 28|Unsurveyed parts north of TCR|Parties non organisees")))


ref_c_match$pop_01 <- NA
ref_c_match$population <- NA
ref_c_match$pop_avg <- NA

for (i in 1:length(ref_c_match$UID_CSD_21)){
  if (ref_c_match$UID_CSD_21[i] %in% df_21_match$UID_CSD_21){
    code <-  ref_c_match$UID_CSD_21[i] 
    id <- which(df_21_match$UID_CSD_21 == code)
    
    pop_01 <- df_21_match$pop_01[id]
    population <- df_21_match$population[id]
    pop_avg <- df_21_match$pop_avg[id]
    
    ref_c_match$pop_01[i] <- pop_01
    ref_c_match$population[i] <- population
    ref_c_match$pop_avg[i] <- pop_avg
  }
}

} # change == 0

branches$subdistrict <- NA
branches$district <- NA
branches$UID_CSD_21 <- NA
branches$UID_CD_21 <- NA
branches$geometry <- NA

for (i in 1:length(branches$place)){
  
  print(i)


  if (!(is.na(branches$province[i]))){

    aux <- df_21_match[which(df_21_match$province == branches$province[i]),]
    id <- amatch(branches$place[i],aux$subdistrict, maxDist = 0.1)

    if (!(is.na(id))){

      branches$subdistrict[i] <-  aux$subdistrict[id]
      branches$district[i] <-  aux$district[id]
      branches$UID_CSD_21[i] <-  aux$UID_CSD_21[id]
      branches$UID_CD_21[i] <-  aux$UID_CD_21[id]
      branches$geometry[i] <- st_sample(aux$geometry[id],1)
    }

  } else {

    aux <- filter(df_21_match,df_21_match$subdistrict == branches$place[i])


    if (length(aux$subdistrict) == 1){

      branches$subdistrict[i] <- aux$subdistrict
      branches$district[i] <-  aux$district
      branches$UID_CSD_21[i] <-  aux$UID_CSD_21
      branches$UID_CD_21[i] <-  aux$UID_CD_21
      branches$province[i] <-  aux$province
      branches$geometry[i] <- st_sample(aux$geometry,1)

    } else if (length(aux$subdistrict) > 1){

      if (!(all(aux$pop_avg == 0)) ){

        id <- which.max(aux$pop_avg)
        branches$subdistrict[i] <- aux$subdistrict[id]
        branches$district[i] <-  aux$district[id]
        branches$UID_CSD_21[i] <-  aux$UID_CSD_21[id]
        branches$UID_CD_21[i] <-  aux$UID_CD_21[id]
        branches$province[i] <-  aux$province[id]
        branches$geometry[i] <- st_sample(aux$geometry[id],1)

      }
    }

  }
  
  if (is.na(branches$UID_CSD_21[i]) & !(is.na(branches$agent[i])) & !(branches$keep[i] == "ny")){
    
    
    
    if (!(is.na(branches$province_agent[i]))){
      
      aux <- df_21_match[which(df_21_match$province == branches$province_agent[i]),]
      id <- amatch(branches$agent[i],aux$subdistrict, maxDist = 0.1)
      
      if (!(is.na(id))){
        
        branches$subdistrict[i] <-  aux$subdistrict[id]
        branches$district[i] <-  aux$district[id]
        branches$UID_CSD_21[i] <-  aux$UID_CSD_21[id]
        branches$UID_CD_21[i] <-  aux$UID_CD_21[id]
        branches$province[i] <-  aux$province[id]
        branches$geometry[i] <- st_sample(aux$geometry[id],1)
      }
      
    } else {
      
      aux <- filter(df_21_match,df_21_match$subdistrict == branches$agent[i])
      
      
      if (length(aux$subdistrict) == 1){
        
        branches$subdistrict[i] <- aux$subdistrict
        branches$district[i] <-  aux$district
        branches$UID_CSD_21[i] <-  aux$UID_CSD_21
        branches$UID_CD_21[i] <-  aux$UID_CD_21
        branches$province[i] <-  aux$province
        branches$geometry[i] <- st_sample(aux$geometry,1)
        
      } else if (length(aux$subdistrict) > 1){
        
        if (!(all(aux$pop_avg == 0)) ){
          
          id <- which.max(aux$pop_avg)
          branches$subdistrict[i] <- aux$subdistrict[id]
          branches$district[i] <-  aux$district[id]
          branches$UID_CSD_21[i] <-  aux$UID_CSD_21[id]
          branches$UID_CD_21[i] <-  aux$UID_CD_21[id]
          branches$province[i] <-  aux$province[id]
          branches$geometry[i] <- st_sample(aux$geometry[id],1)
          
        }
      }
      
    }
    
    
    
  }
  
  
}

branches <- select(branches,-c("agent","province_agent"))

# Drop places already matched

branches2 <- branches %>% filter(is.na(branches$UID_CSD_21))
branches2 <- as.data.frame(branches2) %>% select(-c("geometry"))

branches <- branches %>% filter(!(is.na(branches$UID_CSD_21)))
branches <- st_as_sf(branches,crs=32610)
print("finished branches")
# branches <- right_join(df_11_match[,c("UID_CSD_11","geometry")],branches, by = "UID_CSD_11")

if (change == 0){ # change == 0
  
imuni$subdistrict <- NA
imuni$district <- NA
imuni$UID_CSD_21 <- NA
imuni$UID_CD_21 <- NA

for (i in 1:length(imuni$place)){


  if (!(is.na(imuni$province[i]))){

    aux <- df_21_match[which(df_21_match$province == imuni$province[i]),]
    id <- amatch(imuni$place[i],aux$subdistrict, maxDist = 0.1)

    if (!(is.na(id))){

      imuni$subdistrict[i] <-  aux$subdistrict[id]
      imuni$district[i] <-  aux$district[id]
      imuni$UID_CSD_21[i] <-  aux$UID_CSD_21[id]
      imuni$UID_CD_21[i] <-  aux$UID_CD_21[id]
    }

  } else {

    aux <- filter(df_21_match,df_21_match$subdistrict == imuni$place[i])


    if (length(aux$subdistrict) == 1){

      imuni$subdistrict[i] <- aux$subdistrict
      imuni$district[i] <-  aux$district
      imuni$UID_CSD_21[i] <-  aux$UID_CSD_21
      imuni$UID_CD_21[i] <-  aux$UID_CD_21


    } else if (length(aux$subdistrict) > 1){

      if (!(all(aux$pop_avg == 0)) ){

        id <- which.max(aux$pop_avg)
        imuni$subdistrict[i] <- aux$subdistrict[id]
        imuni$district[i] <-  aux$district[id]
        imuni$UID_CSD_21[i] <-  aux$UID_CSD_21[id]
        imuni$UID_CD_21[i] <-  aux$UID_CD_21[id]


      }
    }

  }
}

# imuni <- right_join(df_21_match[,c("UID_CSD_21","geometry")],imuni, by = "UID_CSD_21")


manuf$subdistrict <- NA
manuf$district <- NA
manuf$UID_CSD_21 <- NA
manuf$UID_CD_21 <- NA

for (i in 1:length(manuf$place)){


  if (!(is.na(manuf$province[i]))){

    aux <- df_21_match[which(df_21_match$province == manuf$province[i]),]
    id <- amatch(manuf$place[i],aux$subdistrict, maxDist = 0.1)

    if (!(is.na(id))){

      manuf$subdistrict[i] <-  aux$subdistrict[id]
      manuf$district[i] <-  aux$district[id]
      manuf$UID_CSD_21[i] <-  aux$UID_CSD_21[id]
      manuf$UID_CD_21[i] <-  aux$UID_CD_21[id]
      
    }

  } else {

    aux <- filter(df_21_match,df_21_match$subdistrict == manuf$place[i])


    if (length(aux$subdistrict) == 1){

      manuf$subdistrict[i] <- aux$subdistrict
      manuf$district[i] <-  aux$district
      manuf$UID_CSD_21[i] <-  aux$UID_CSD_21
      manuf$UID_CD_21[i] <-  aux$UID_CD_21


    } else if (length(aux$subdistrict) > 1){

      if (!(all(aux$pop_avg == 0)) ){

        id <- which.max(aux$pop_avg)
        manuf$subdistrict[i] <- aux$subdistrict[id]
        manuf$district[i] <-  aux$district[id]
        manuf$UID_CSD_21[i] <-  aux$UID_CSD_21[id]
        manuf$UID_CD_21[i] <-  aux$UID_CD_21[id]


      }
    }

  }
}

# manuf <- right_join(df_21_match[,c("UID_CSD_21","geometry")],manuf, by = "UID_CSD_21")

} # change == 0

# google maps complement for places not found on df_11_match ####

# change so that google maps finds unique place

branches2$place[which(branches2$place == "Lynden")] <- "Lynden, Hamilton"
branches2$province[which(branches2$place == "St Chrysostom")] <- "Quebec"
branches2$province[which(branches2$place == "Yorkton" & branches2$province == "North Western Territories")] <- "Saskatchewan"
branches2$province[which(branches2$place == "Lumsden" & branches2$province == "North Western Territories")] <- "Saskatchewan"
branches2$province[which(branches2$place == "Macleod" & branches2$province == "North Western Territories")] <- "Alberta"
branches2$province[which(branches2$place == "Greenville Ferry")] <- "Nova Scotia"
branches2$place[which(branches2$place == "Greenville Ferry")] <- "Greenville"
branches2$place[which(branches2$place == "Cypress Road")] <- "Cypress River"
branches2$province[which(branches2$place == "Lenore")] <- "Manitoba"

branches2$province[which(branches2$place == "Dublin")] <- "Ontario"
branches2$province[which(branches2$place == "Gauvin")] <- "Quebec"

branches2 <- filter(branches2,!(place %in% c("Wycollar","Clarkstown")))
branches2$place[which(branches2$place == "Port St Charles")] <- "Cape Charles"
branches2$province[which(branches2$place == "Red Deer")] <- "Alberta"
branches2$province[which(branches2$place == "Metabetchouan")] <- "Quebec"
branches2$place[which(branches2$place == "Sunnyside")] <- "Sunnyside, Old Toronto"
branches2$place[which(branches2$place == "Temiskaming")] <- "Temiscaming"
branches2$province[which(branches2$place == "Estuary")] <- "Saskatchewan"


# adjust columns for google mapps format


branches2 <- unite(branches2,"place",place:province,sep = ", ", na.rm = T)
branches2$place <- paste0(branches2$place,", Canada")


# geocode in google maps


# register_google(key = "AIzaSyAjn0ILaUnDxatOcKX5VHy8fHjl8fCfCGg", write = T)
# register_google(key = "AIzaSyA40-YoB-EoGLhiDMSf_np8xV-FJTC3IRo", write = T)


# branches2_aux <- as.data.frame(unique(branches2$place))
# colnames(branches2_aux) <- "place"
# branches2_aux <- mutate_geocode(branches2_aux,place)
# branches2_aux <- branches2_aux %>% filter(lat >= 42, lon < 0, !(place == "Cape Charles, Canada"))

# if (change == 0){
#   filename <- "./Dataset/R/geocodes2.rds"
#   saveRDS(branches2_aux,file= filename)
# }
branches2_aux <- readRDS("./Dataset/R/geocodes2.rds")

branches2 <- left_join(branches2,branches2_aux,by = c("place"))
branches2 <- filter(branches2,!(is.na(lon)))
branches2 <- st_as_sf(branches2,coords=c("lon","lat"),crs=4326)
# branches2 <- branches2 %>% select(-c("lon","lat"))
branches2 <- st_transform(branches2,crs = 32610)

# go back to usual column format


branches2$place <- gsub(", Canada","",branches2$place)
branches2 <- separate(branches2,"place", into = c("place","province"), sep = ", ")


# Add province
print("I'm adding stuff to branches2")


id <- st_within(branches2, df_212, sparse = TRUE, prepared = TRUE)
for (i in 1:length(id)){
  if (length(id[[i]]) > 0){
    
    branches2$UID_CSD_21[i] <- df_212$UID_CSD_21[id[[i]]]
    branches2$UID_CD_21[i] <- df_212$UID_CD_21[id[[i]]]
    branches2$subdistrict[i] <- df_212$subdistrict[id[[i]]]
    branches2$district[i] <- df_212$district[id[[i]]]
    # branches2$geometry[i] <- st_sample(df_212$geometry[id[[i]]],1)  
    # branches2$geometry[i] <- df_212$geometry[id[[i]]]  
    branches2$province[i] <- df_212$province[id[[i]]]
    
  }
}

# id2 <- which(is.na(branches$province))
# id <- st_within(branches[id2,], df_112, sparse = TRUE, prepared = TRUE)
# for (i in 1:length(id2)){
#   if (length(id[[i]]) > 0){
#     
#     branches$province[id2[i]] <- df_112$province[id[[i]]]
#     
#   }
# }


# bind places found before with matches with google maps


branches_nfld <- rbind(branches,branches2)
aux <- as.data.frame(branches_nfld[which(branches_nfld$province == "Newfoundland"),]) %>% select(place,province,bank,year,-geometry)
aux$agent <- NA
aux$place <- paste(aux$place,aux$province, sep = ", ")
aux <- aux %>% select(-province)
branches_f <- rbind(branches_f,aux)

# random sample of where are branches within polygon

branches3 <- branches_nfld %>% filter(!(is.na(branches_nfld$UID_CSD_21))) # drop Newfoundland and a few others not in Canada map

# # w <- which(str_detect(branches3$subdistrict,"City"))
# for (i in 1:length(branches3$UID_CSD_11)){
# # for (i in w){
#   # print(i)
#   branches3$geometry[i] <- st_sample(branches3$geometry[i],1)
# }

# if (change == 0){
#   filename <- "./Dataset/R/branches_sampled.rds"
#   saveRDS(branches3,file= filename)  
# }

# creates references

if (change == 0){# change == 0
ref_branches <- as.data.frame(branches3) %>%  select(-c("geometry"))
ref_branches_nfld <- as.data.frame(branches_nfld) %>%  select(-c("geometry"))
} # change == 0


# left join province geometry to edges

# aggregate map to province level for edges

q5 <- df_212
q5_aux <- q5 %>% select(c("province","geometry","area","population","pop_01","pop_avg","pop_51","pop_61","pop_71","pop_81","pop_91","pop_11"))
q5_aux2 <-as.data.frame(q5) %>% select(-c("geometry","area","population","pop_01","pop_avg","pop_51","pop_61","pop_71","pop_81","pop_91","pop_11"))

df_21_prov_aux <- aggregate(q5_aux[,-which(colnames(q5_aux) %in% c("province"))], by =list("province" = q5_aux$province), FUN = function(x) sum(x,na.rm = T), do_union = T)
df_21_prov_aux2 <- aggregate(q5_aux2[,-which(colnames(q5_aux2) %in% c("province"))], by =list("province" = q5_aux2$province), FUN = function(x) extract2(x,1))
df_21_prov <- left_join(df_21_prov_aux,df_21_prov_aux2, by = c("province"))

df_21_prov$area <- st_area(df_21_prov)/1000000 
df_21_prov <- df_21_prov %>% select(c("province","geometry","area","pop_51","pop_61","pop_71","pop_81","pop_91","pop_01","population","pop_11"))
ref_21_prov <- as.data.frame(df_21_prov) %>% select(-c("geometry"))


# edges

edges$where <- NA
edges$where[which(edges$place %in% edges_canada)] <- "Canada"
edges$where[which(edges$place %in% edges_foreign)] <- "elsewhere"

# edges$place[which(is.na(edges$place))] <- "None"
edges_count <- edges %>%  
  group_by(bank,year,place) %>% 
  tally()
edges_count <- edges_count %>% mutate_cond(is.na(place), n = 0) 
edges_count$where <- NA
edges_count$where[which(edges_count$place %in% edges_canada)] <- "Canada"
edges_count$where[which(edges_count$place %in% edges_foreign)] <- "elsewhere"


# CC

CC$province <- NA
CC$subdistrict <- NA
CC$district <- NA
CC$UID_CSD_21 <- NA
CC$UID_CD_21 <- NA

w <- which(CC$place %in% c("Vancouver","Victoria","St John","Windsor","London","Toronto","Montreal","Winnipeg","Halifax","Ottawa",
                                 "Quebec","Trois Rivieres","Hamilton"))
# ,"Sherbrooke"
CC$place[w] <- paste0(CC$place[w]," City")

for (i in 1:NROW(CC)){
  
  id <- amatch(CC$place[i],ref_c_match$subdistrict, maxDist = 0.1)
  
  CC$UID_CSD_21[i] <- ref_c_match$UID_CSD_21[id]
  CC$UID_CD_21[i] <- ref_c_match$UID_CD_21[id]
  CC$subdistrict[i] <- ref_c_match$subdistrict[id]
  CC$district[i] <- ref_c_match$district[id]
  CC$province[i] <- ref_c_match$province[id]
  
}

# CC <- right_join(df_21_match[,c("UID_CSD_21","geometry")],CC, by = "UID_CSD_21")


CC_g <- c("Montreal City","Toronto City","Halifax City","St John City","Victoria City","Winnipeg City","Vancouver City",
          "Fredericton","Ottawa City","Charlottetown & Royalty","Summerside")
CC_p <- CC %>% filter(turnover %in% c(1.1))
CC_p$group <- 0
CC_p <- CC_p %>% mutate_cond(subdistrict %in% CC_g, group = 1)


# Do all the same for imuni and manuf



if (change == 0){# change == 0
# 1

imuni2 <- imuni %>% filter(is.na(imuni$UID_CSD_21))
imuni <- imuni %>% filter(!(is.na(imuni$UID_CSD_21)))
# imuni2 <- as.data.frame(imuni2) %>% select(-c("geometry"))

manuf2 <- manuf %>% filter(is.na(manuf$UID_CSD_21))
manuf <- manuf %>% filter(!(is.na(manuf$UID_CSD_21)))
# manuf2 <- as.data.frame(manuf2) %>% select(-c("geometry"))

if (NROW(imuni2) > 0){
  
  imuni2$province[which(imuni2$place == "Shawinigan")] <- "Quebec"
  imuni2$province[which(imuni2$place == "Stewartville")] <- "Ontario"
  imuni2$province[which(imuni2$place == "Silver Plains")] <- "Manitoba"
  imuni2$province[which(imuni2$place == "Saginaw")] <- "Ontario"
  imuni2$province[which(imuni2$place == "Ribstone")] <- "Alberta"
  imuni2$province[which(imuni2$place == "Pleasant Grove")] <- "Prince Edward Island"

    imuni2 <- unite(imuni2,"place",place:province,sep = ", ", na.rm = T)
  
    
    imuni2$place <- paste0(imuni2$place,", Canada")
    manuf2$place <- paste0(manuf2$place,", Canada")
    
    
    imuni2 <- mutate_geocode(imuni2,place)
    imuni2 <- imuni2 %>% filter(!(str_detect(place,"Cologne|Wentworth, Ontario|Mornington, Manitoba")))
    imuni2 <- st_as_sf(imuni2,coords=c("lon","lat"),crs=32610)
    
    imuni2$place <- gsub(", Canada","",imuni2$place)
    imuni2 <- separate(imuni2,"place", into = c("place","province"), sep = ", ")

    
    id <- st_within(imuni2, df_212, sparse = TRUE, prepared = TRUE)
    for (i in 1:length(imuni2$UID_CSD_21)){
      if (length(id[[i]]) > 0){
        imuni2$UID_CSD_21[i] <- df_212$UID_CSD_21[id[[i]]]
        imuni2$UID_CD_21[i] <- df_212$UID_CD_21[id[[i]]]
        imuni2$subdistrict[i] <- df_212$subdistrict[id[[i]]]
        imuni2$district[i] <- df_212$district[id[[i]]]
        # imuni2$geometry[i] <- df_212$geometry[id[[i]]]
        imuni2$province[i] <- df_212$province[id[[i]]]
      }
    }
    imuni2 <- as.data.frame(imuni2) %>% select(-c("geometry"))
    
    imuni3 <- rbind(imuni,imuni2)
    
} else{
  
  imuni3 <- imuni
}


if (NROW(manuf2) > 0){
  
  
  manuf2$province[which(manuf2$province == "P E Island")] <- "Prince Edward Island"
  
  
  manuf2 <- unite(manuf2,"place",place:province,sep = ", ", na.rm = T)
  
  
  manuf2 <- mutate_geocode(manuf2,place)
  manuf2 <- st_as_sf(manuf2,coords=c("lon","lat"),crs=32610)
  
  manuf2$place <- gsub(", Canada","",manuf2$place)
  manuf2 <- separate(manuf2,"place", into = c("place","province"), sep = ", ")
  
  
  id <- st_within(manuf2, df_212, sparse = TRUE, prepared = TRUE)
  for (i in 1:length(manuf2$UID_CSD_21)){
    if (length(id[[i]]) > 0){
      manuf2$UID_CSD_21[i] <- df_212$UID_CSD_21[id[[i]]]
      manuf2$UID_CD_21[i] <- df_212$UID_CD_21[id[[i]]]
      manuf2$subdistrict[i] <- df_212$subdistrict[id[[i]]]
      manuf2$district[i] <- df_212$district[id[[i]]]
      # manuf2$geometry[i] <- df_212$geometry[id[[i]]]
      manuf2$province[i] <- df_212$province[id[[i]]]
    }
  }
  manuf2 <- as.data.frame(manuf2) %>% select(-c("geometry"))
  
  manuf3 <- rbind(manuf,manuf2)
  
  
} else {
  
  manuf3 <- manuf
  
}


} # change == 0