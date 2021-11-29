branches$place <- stri_trans_general(branches$place ,"Latin-ASCII")
branches$agent <- stri_trans_general(branches$agent ,"Latin-ASCII")
branches$bank <- stri_trans_general(branches$bank ,"Latin-ASCII")
edges$bank <- stri_trans_general(edges$bank ,"Latin-ASCII")
edges$correspondent <- stri_trans_general(edges$correspondent ,"Latin-ASCII")

branches$place <- gsub(", Chicoutimi","",branches$place)
branches$place <- gsub(", Portneuf","",branches$place)
branches$place <- gsub(", Dorchester","",branches$place)
branches$place <- gsub(", de Levis","",branches$place)
branches$place <- gsub(", Charlevoix","",branches$place)
branches$place <- gsub(", Chateuguay","",branches$place)
branches$place <- gsub(", Chateauguay","",branches$place)
branches$place <- gsub(", River",",",branches$place)
branches$place <- gsub(", Centre","",branches$place)
branches$place <- gsub(", Bellechasse","",branches$place)
branches$place <- gsub(", Lotbiniere","",branches$place)
branches$place <- gsub(", Dorch","",branches$place)
branches$place <- gsub(", Beauce","",branches$place)

branches <- separate(branches,place,c("place","province"), sep = ",")


branches$place <- gsub(".", "", branches$place,fixed = T)
branches$place <- gsub(" Station", "", branches$place,fixed = T)
branches$place <- gsub("(^\\s+)|(\\s+$)", "", branches$place)
branches$place <- trimws(branches$place)

branches$province <- gsub(".", "", branches$province,fixed = T)
branches$bank <- gsub(".", "", branches$bank,fixed = T)
branches$agent <- gsub(".", "", branches$agent,fixed = T)
branches$bank <- gsub(",", "", branches$bank,fixed = T)
branches$bank <- gsub("B N", "BN", branches$bank,fixed = T)
branches$bank <- gsub("P E I", "PEI", branches$bank,fixed = T)
branches$bank <- gsub("N B", "NB", branches$bank,fixed = T)
branches$bank <- gsub("N S", "NS", branches$bank,fixed = T)
branches$bank <- gsub("(^\\s+)|(\\s+$)", "", branches$bank)
branches$bank <- trimws(branches$bank)
branches$bank <- gsub("'", "", branches$bank,fixed = T)
branches$bank <- gsub("'", "", branches$bank,fixed = T)
branches$bank <- str_to_title(branches$bank)
branches$bank <- gsub("  ", " ", branches$bank,fixed = T)

branches$bank <- gsub("Nbr", "N Br", branches$bank,fixed = T)
branches$bank <- gsub("Lmperial", "Imperial", branches$bank,fixed = T)
branches$bank <- gsub("Bn", "BN", branches$bank,fixed = T)
branches$bank <- gsub("Pei", "PEI", branches$bank,fixed = T)
branches$bank <- gsub("Dhoch", "DHoch", branches$bank,fixed = T)
branches$bank <- gsub("Nb", "NB", branches$bank,fixed = T)
branches$bank <- gsub("Dn", "Du", branches$bank,fixed = T)
branches$bank <- gsub(" Of ", " of ", branches$bank,fixed = T)

branches$bank <- gsub("'", "", branches$bank,fixed = T)
branches$place <- gsub("-", " ", branches$place,fixed = T)
branches$place <- gsub("  ", "", branches$place,fixed = T)
branches$province <- gsub("  ", "", branches$province,fixed = T)
branches$province <- gsub("(^\\s+)|(\\s+$)", "", branches$province)
branches$province <- trimws(branches$province)


branches$agent <- gsub("-", " ", branches$agent,fixed = T)
branches$agent <- gsub(".", "", branches$agent,fixed = T)
branches <- separate(branches,agent,c("agent","province_agent"), sep = ",")
branches$agent <- gsub("Sub to ", "", branches$agent,fixed = T)
branches$agent <- gsub("Sub ", "", branches$agent,fixed = T)
branches$agent <- gsub("Sub To ", "", branches$agent,fixed = T)
branches$agent <- gsub("To ", "", branches$agent,fixed = T)
branches$agent <- gsub(" Station", "", branches$agent,fixed = T)
branches$province_agent <- gsub("(^\\s+)|(\\s+$)", "", branches$province_agent)
branches$province_agent <- trimws(branches$province_agent)



branches$province[which(branches$province %in% c("Sask","Assa","Sk","Sas","S","Saak","S'sk","S'k","sask"))] <- "Saskatchewan"
branches$province[which(branches$province %in% c("NWT"))] <-                                  "North Western Territories"
branches$province[which(branches$province %in% c("NS","CB","N S"))] <-                              "Nova Scotia"
branches$province[which(branches$province %in% c("Newfoundland","Nfld","Nfd","Nd","Nfl'd","Nf'ld","Nf'd","NfId"))] <- "Newfoundland"
branches$province[which(branches$province %in% c("Q...","Q","Que","QC","Qu"))] <-             "Quebec"
branches$province[which(branches$province %in% c("NB", "N B"))] <-                                   "New Brunswick"
branches$province[which(branches$province %in% c("Ont","O","0","??"))] <-                          "Ontario"
branches$province[which(branches$province %in% c("Alb","Alta","Atta","Al","Alberta","A","Altaa","All","Alt","AIt"
                                                 ,"AIt","Alebrta","Alb'rta","Albrta"))] <-    "Alberta"
branches$province[which(branches$province %in% c("BC","VI","EastBC"))] <-                     "British Columbia"
branches$province[which(branches$province %in% c("PEI","PEl"))] <-                            "Prince Edward Island"
branches$province[which(branches$province %in% c("M","Man","Mann"))] <- "Manitoba"
branches$province[which(branches$province %in% c("Yukon","YT","Y"))] <- "Yukon"

branches$province_agent[which(branches$province_agent %in% c("Sask"))] <- "Saskatchewan"
branches$province_agent[which(branches$province_agent %in% c("NS"))] <-                              "Nova Scotia"
branches$province_agent[which(branches$province_agent %in% c("Q","Que"))] <-             "Quebec"
branches$province_agent[which(branches$province_agent %in% c("N B"))] <-                                   "New Brunswick"
branches$province_agent[which(branches$province_agent %in% c("Ont","O","0","??"))] <-                          "Ontario"
branches$province_agent[which(branches$province_agent %in% c("British Columbia"))] <-                     "British Columbia"
branches$province_agent[which(branches$province_agent %in% c("P E I"))] <-                            "Prince Edward Island"
branches$province_agent[which(branches$province_agent %in% c("Man"))] <- "Manitoba"

prov <- c("Saskatchewan","North Western Territories","Nova Scotia","Newfoundland","Quebec","New Brunswick",
          "Ontario","Alberta","British Columbia","Prince Edward Island","Manitoba","Yukon")


branches$province[-which(branches$province %in% prov)] <- NA

branches$keep[is.na(branches$keep)] <- "yy"
branches <- branches %>% filter(!(keep == "nn"))

branches$place[which(branches$place == "St Johns" & branches$province == "Quebec")] <- "St Jean"
branches$place[which(branches$place == "St John's" & branches$province == "Quebec")] <- "St Jean"
branches$agent[which(branches$agent == "St John" & branches$province_agent == "Quebec")] <- "St Jean"
branches$place <- gsub("Saint ","St ",branches$place)
branches$place <- gsub("Sainte ","Ste ",branches$place)
branches$place[which(branches$place == "St Marie")] <- "Ste Marie"
branches$place[which(branches$place == "St Marys")] <- "St Mary's"

branches$agent[which(branches$agent == "St John Station")] <- "St Jean"


branches$place[which(branches$place == "East Nissouri'")] <- "Nissouri East"
branches$place[which(branches$place == "West Nissouri'")] <- "Nissouri West"

branches$place[which(branches$place == "Guysboro'")] <- "Guysborough"

branches$place[which(branches$place == "Metcalf'")] <- "Metcalfe"

branches$place[which(branches$place == "Lt Glace Bay'")] <- "Glace Bay"

branches$province[which(branches$place == "Battleford" & branches$province == "MB")] <- "SK"

branches$province[which(branches$place == "Yorkton" & branches$province == "NT")] <- "SK"

branches$place[which(branches$place == "Coteau'")] <- "Coteau Station"
branches$agent[which(branches$agent == "Coteau'")] <- "Coteau Station"
branches$place[which(branches$place == "Hebertville'")] <- "Hebertville Station"
branches$agent[which(branches$agent == "Hebertville'")] <- "Hebertville Station"

branches$place[which(branches$place == "Bytown'")] <- "Ottaway City"
branches$agent[which(branches$agent == "Bytown'")] <- "Ottaway City"

branches$place[which(branches$place == "Kitchener")] <- "Berlin"
branches$agent[which(branches$agent == "Kitchener")] <- "Berlin"


edges$bank <- gsub("'", "", edges$bank,fixed = T)
edges$bank <- gsub("'", "", edges$bank,fixed = T)
edges$bank <- gsub(".", "", edges$bank,fixed = T)
edges$bank <- gsub(",", "", edges$bank,fixed = T)
edges$bank <- trimws(edges$bank)
edges$correspondent <- gsub("'", "", edges$correspondent,fixed = T)
edges$correspondent <- gsub("'", "", edges$correspondent,fixed = T)
edges$correspondent <- gsub(".", "", edges$correspondent,fixed = T)
edges$correspondent <- gsub(",", "", edges$correspondent,fixed = T)
edges$correspondent <- trimws(edges$correspondent)

# unify banks which changed names

edges$bank[which(edges$bank == "Merchants H")] <- "Royal"
branches$bank[which(branches$bank == "Merchants H")] <- "Royal"

edges$correspondent[which(edges$correspondent == "Merchants H")] <- "Royal"

edges$bank[which(edges$bank == "Union LC")] <- "Union"
edges$correspondent[which(edges$correspondent == "Union LC")] <- "Union"

# clean correspondent location

# edges$place[is.na(edges$place)] <- 0
# edges$place[edges$place == 0] <- NA

# edges <- mutate_cond(edges,place == "BC", place = "British Columbia")
# edges <- mutate_cond(edges,place == "BO", place = "Boston")
# edges <- mutate_cond(edges,place == "CA", place = "Canada")
# edges <- mutate_cond(edges,place == "CHI", place = "Chicago")
# edges <- mutate_cond(edges,place == "CU", place = "La Havana, Cuba")
# edges <- mutate_cond(edges,place == "Eastern CA", place = "Eastern Canada")
# edges <- mutate_cond(edges,place == "FC", place = "Fredericton")
# edges <- mutate_cond(edges,place == "HX", place = "Halifax")
# edges <- mutate_cond(edges,place == "JA", place = "Kingston, Jamaica")
# edges <- mutate_cond(edges,place == "LO", place = "London, England")
# edges <- mutate_cond(edges,place == "Maritime", place = "Maritime Provinces, Canada")
# edges <- mutate_cond(edges,place == "MB", place = "Manitoba")
# edges <- mutate_cond(edges,place == "MO", place = "Montreal")
# edges <- mutate_cond(edges,place == "NB", place = "New Brunswick")
# edges <- mutate_cond(edges,place == "NL", place = "Newfoundland")
# edges <- mutate_cond(edges,place == "NS", place = "Nova Scotia")
# edges <- mutate_cond(edges,place == "NY", place = "New York")
# edges <- mutate_cond(edges,place == "ON", place = "Ontario")
# edges <- mutate_cond(edges,place == "OT", place = "Ottawa")
# edges <- mutate_cond(edges,place == "PA", place = "Paris")
# edges <- mutate_cond(edges,place == "PEI", place = "Prince Edward Island")
# edges <- mutate_cond(edges,place == "PO", place = "Portland")
# edges <- mutate_cond(edges,place == "QC", place = "Quebec")
# edges <- mutate_cond(edges,place == "SE", place = "Seattle")
# edges <- mutate_cond(edges,place == "SF", place = "San Francisco")
# edges <- mutate_cond(edges,place == "TA", place = "Tacoma")
# edges <- mutate_cond(edges,place == "TO", place = "Toronto")
# edges <- mutate_cond(edges,place == "WI", place = "West Indies")
# edges <- mutate_cond(edges,place == "WSH", place = "Washington State")
# edges <- mutate_cond(edges,place == "YT", place = "Yukon")
# 
# 
# edges_canada <- c("British Columbia","Canada","Eastern Canada","Fredericton","Halifax","Maritime Provinces","Manitoba","Montreal",
#             "New Brunswick","Nova Scotia","Ontario","Ottawa","Prince Edward Island","Quebec","Toronto","Yukon")

edges$place[is.na(edges$place)] <- 0
edges <- mutate_cond(edges,place == "BC", place = "British Columbia")
edges <- mutate_cond(edges,place == "BO", place = "Boston, US")
edges <- mutate_cond(edges,place == "CA", place = "Canada")
edges <- mutate_cond(edges,place == "CHI", place = "Chicago, US")
edges <- mutate_cond(edges,place == "CU", place = "Cuba")
edges <- mutate_cond(edges,place == "Eastern CA", place = "Eastern Canada")
edges <- mutate_cond(edges,place == "FC", place = "New Brunswick")
edges <- mutate_cond(edges,place == "HX", place = "Nova Scotia")
edges <- mutate_cond(edges,place == "JA", place = "Jamaica")
edges <- mutate_cond(edges,place == "LO", place = "London, England")
edges <- mutate_cond(edges,place == "Maritime", place = "Maritime Provinces, Canada")
edges <- mutate_cond(edges,place == "MB", place = "Manitoba")
edges <- mutate_cond(edges,place == "MO", place = "Quebec")
edges <- mutate_cond(edges,place == "NB", place = "New Brunswick")
edges <- mutate_cond(edges,place == "NL", place = "Newfoundland")
edges <- mutate_cond(edges,place == "NS", place = "Nova Scotia")
edges <- mutate_cond(edges,place == "NY", place = "New York, US")
edges <- mutate_cond(edges,place == "ON", place = "Ontario")
edges <- mutate_cond(edges,place == "OT", place = "Ontario")
edges <- mutate_cond(edges,place == "PA", place = "Paris, France")
edges <- mutate_cond(edges,place == "PEI", place = "Prince Edward Island")
edges <- mutate_cond(edges,place == "PO", place = "Portland, US")
edges <- mutate_cond(edges,place == "QC", place = "Quebec")
edges <- mutate_cond(edges,place == "SE", place = "Seattle, US")
edges <- mutate_cond(edges,place == "SF", place = "San Francisco, US")
edges <- mutate_cond(edges,place == "TA", place = "Tacoma, US")
edges <- mutate_cond(edges,place == "TO", place = "Ontario")
edges <- mutate_cond(edges,place == "WI", place = "West Indies")
edges <- mutate_cond(edges,place == "WSH", place = "Washington State")
edges <- mutate_cond(edges,place == "YT", place = "Yukon")
edges$place[edges$place == 0] <- NA

aux <- NULL
for (i in 1:length(edges$place)){
  if (!(is.na(edges$place[i]))){
   
    if (edges$place[i] == "Eastern Canada"){
      
      aux2 <- edges[rep(i,4),] 
      aux2$place <- c("Ontario","Quebec","New Brunswick","Nova Scotia")
      aux <- rbind(aux,aux2)  
    }
    
    
    if (edges$place[i] == "Maritime Provinces, Canada"){
      
      aux2 <- edges[rep(i,3),] 
      aux2$place <- c("New Brunswick","Nova Scotia","Prince Edward Island")
      aux <- rbind(aux,aux2)  
    }
    
    if (edges$place[i] == "Washington State"){
      
      aux2 <- edges[rep(i,2),] 
      aux2$place <- c("Tacoma, US","Seattle, US")
      aux <- rbind(aux,aux2)  
    }
    
    if (edges$place[i] == "West Indies"){
      
      aux2 <- edges[rep(i,3),] 
      aux2$place <- c("Havana, Cuba","San Juan, Puerto Rico","Kingston, Jamaica")
      aux <- rbind(aux,aux2)  
    }
    
    if (edges$place[i] == "Newfoundland"){
      
      aux2 <- edges[rep(i,1),] 
      aux2$place <- c("St John's, Newfoundland")
      aux <- rbind(aux,aux2)  
    }
  }
}
edges <- edges %>% filter(!(place %in% c("Eastern Canada","Maritime Provinces, Canada","Washington State","West Indies","Newfoundland")))
edges <- rbind(edges,aux)

# edges_canada <- c("British Columbia","Canada","Eastern Canada","Fredericton","Halifax","Maritime Provinces, Canada","Manitoba","Montreal",
#                   "New Brunswick","Nova Scotia","Ontario","Ottawa","Prince Edward Island","Quebec","Toronto","Yukon")
edges_canada <- c("British Columbia","Canada","Fredericton","Halifax","Manitoba","Montreal",
                  "New Brunswick","Nova Scotia","Ontario","Ottawa","Prince Edward Island","Quebec","Toronto","Yukon")

edges_foreign <- unique(edges$place[-which(edges$place %in% edges_canada | is.na(edges$place))])  

# Clearing Houses

CC <- melt(data = CC, id.vars = c("year"), value.name = "turnover" )
CC <- rename(CC, "place" = "variable")
CC <- separate(CC,"turnover",c("turnover","month"),"_")
CC$place <- as.character(CC$place)

CC$place[which(CC$place == "Kitchener")] <- "Berlin"
CC$place[which(CC$place == "Peterboro")] <- "Peterborough"
CC$place[which(CC$place == "Charlottetown")] <- "Charlottetown & Royalty"
