if (change == 0){
  # countryside data 1911
  
  country_11$district[which(country_11$district == "Vancouver")] <- "Vancouver City"
  country_11$district[which(country_11$district == "Victoria" & country_11$province == "British Columbia")] <- "Victoria City"
  # country_11$district[which(country_11$district == "Sherbrooke")] <- "Sherbrooke City"
  country_11$district[which(country_11$district == "St John City & County")] <- "St John County"
  country_11$district[which(country_11$district == "Halifax City & County")] <- "Halifax County"
  country_11$district[which(country_11$district == "Winnipeg")] <- "Winnipeg City"
  country_11$district[which(country_11$district == "London")] <- "London City"
  country_11$district[which(country_11$district == "Kingston")] <- "Kingston City"
  country_11$district[which(country_11$district == "Hochelaga")] <- "Montreal City"
  
  # manufacturing data 1911
  
  # manuf_11$district[which(manuf_11$district == "Sherbrooke")] <- "Sherbrooke City"
  manuf_11$district[which(manuf_11$district == "Trois Rivieres & St Maurice")] <- "Trois Rivieres City"
  manuf_11$district[which(manuf_11$district %in% c("Toronto","Toronto East","Toronto North",
                                                   "Toronto Centre","Toronto West","Toronto South"))] <- "Toronto City"
  manuf_11$district[which(str_detect(manuf_11$district,"Montreal|Hochelaga|Maisonneuve"))] <- "Montreal City"
  manuf_11$district[which(manuf_11$district %in% c("Quebec","Quebec East","Quebec Centre","Quebec West"))] <- "Quebec City"
  manuf_11$district[which(manuf_11$district %in% c("Hamilton","Hamilton East","Hamilton West"))] <- "Hamilton City"
  manuf_11$district[which(manuf_11$district == "Essex North")] <- "Windsor City"
  manuf_11$district[which(manuf_11$district == "London")] <- "London City"
  manuf_11$district[which(manuf_11$district == "Kingston")] <- "Kingston City"
  
  # manuf 01
  
  manuf_01$district[which(manuf_01$district == "Montreal Ste Anne")] <- "Montreal City"
  manuf_01$district[which(manuf_01$district == "Montreal St Antoine")] <- "Montreal City"
  manuf_01$district[which(manuf_01$district == "Montreal St Jacques")] <- "Montreal City"
  manuf_01$district[which(manuf_01$district == "Montreal St Laurent")] <- "Montreal City"
  manuf_01$district[which(manuf_01$district == "Montreal Ste Marie")] <- "Montreal City"
  manuf_01$district[which(manuf_01$district == "Quebec Centre")] <- "Quebec City"
  manuf_01$district[which(manuf_01$district == "Quebec East")] <- "Quebec City"
  manuf_01$district[which(manuf_01$district == "Quebec West")] <- "Quebec City"
  manuf_01$district[which(manuf_01$district == "Quebec County")] <- "Quebec"
  
  # Municipal bond data
  
  imuni$place[which(imuni$place %in% c("Vancouver","South Vancouver"))] <- "Vancouver City"
  imuni$place[which(imuni$place %in% c("Toronto","East Toronto","North Toronto"))] <- "Toronto City"
  imuni$place[which(imuni$place == "Victoria" & imuni$province %in% c("British Columbia",NA))] <- "Victoria City"
  
  w <- which(imuni$place %in% c("St John","Montreal","Winnipeg","Halifax","Ottawa",
                                "Quebec","Trois Rivieres","Hamilton","London","Kingston"))
  # ,"Sherbrooke"
  imuni$place[w] <- paste0(imuni$place[w]," City")
  
  imuni$place[which(imuni$place == "Windsor" & imuni$province == "Ontario")] <- "Windsor City"
  
  # manufacturing data 1891-1901-1911
  
  w <- which(manuf$place %in% c("Vancouver","St John","Toronto","Montreal","Winnipeg","Ottawa",
                                "Quebec","Hamilton","Halifax","London","Kingston"))
  # ,"Sherbrooke"
  
  manuf$place[w] <- paste0(manuf$place[w]," City")
  
  
  manuf$place[which(manuf$place == "Three Rivers")] <- "Trois Rivieres City"
  manuf$place[which(manuf$place == "Windsor" & manuf$province == "Ontario")] <- "Windsor City"
  manuf$place[which(manuf$place == "Victoria" & manuf$province == "British Columbia")] <- "Victoria City"
  
  # country data for 1901
  
  country_01$district[which(country_01$district == "St John City & Country")] <- "St John"
  country_01$district[which(country_01$district == "Halifax City & Country")] <- "Halifax"
  country_01$district[which(country_01$district == "Winnipeg")] <- "Winnipeg City"
  
  country_01$district[which(str_detect(country_01$district,"Montreal"))] <- "Montreal City"
  country_01$district[which(str_detect(country_01$district,"Toronto"))] <- "Toronto City"
  
  country_01$district[which(country_01$district == "Quebec Centre")] <- "Quebec City"
  country_01$district[which(country_01$district == "Quebec East")] <- "Quebec City"
  country_01$district[which(country_01$district == "Quebec West")] <- "Quebec City"
  country_01$district[which(country_01$district == "Quebec Country")] <- "Quebec"
  country_01$district[which(country_01$district == "Leeds & Grenville North")] <- "Leeds N & Grenville N"
  country_01$district[which(country_01$district == "L'lslet")] <- "L'Islet"
  
  
}

# change branch names to coincide with city names

w <- which(branches$place %in% c("East Vancouver","West Vancouver","Vancouver East","Victoria West","West St John",
                                 "West Toronto","East Toronto","New Toronto","East Montreal","St Henri Montreal",
                                 "Montreal West","Montreal South","Montreal North","Montreal Nord","Sherbrooke Est",
                                 "Sherbrooke Ouest"))

branches$place[w] <- gsub("East|West|Ouest|Est|Nord|New|St Henri","",branches$place[w])
branches$place[w] <- trimws(branches$place[w])

w <- which(branches$agent %in% c("East Vancouver","West Vancouver","Vancouver East","Victoria West","West St John",
                                 "West Toronto","East Toronto","New Toronto","East Montreal","St Henri Montreal",
                                 "Montreal West","Montreal South","Montreal North","Montreal Nord","Sherbrooke Est",
                                 "Sherbrooke Ouest"))

branches$agent[w] <- gsub("East|West|Ouest|Est|Nord|New|St Henri","",branches$agent[w])
branches$agent[w] <- trimws(branches$agent[w])

w <- which(branches$place %in% c("Vancouver","Victoria","St John","Toronto","Montreal","Winnipeg","Halifax","Ottawa",
                                 "Quebec","Trois Rivieres","Hamilton"))
# ,"Sherbrooke"

branches$place[w] <- paste0(branches$place[w]," City")

w <- which(branches$agent %in% c("Vancouver","Victoria","St John","Toronto","Montreal","Winnipeg","Halifax","Ottawa",
                                 "Quebec","Trois Rivieres","Hamilton"))
# ,"Sherbrooke"

branches$agent[w] <- paste0(branches$agent[w]," City")

w <- which(branches$place %in% c("St Jean Baptiste","Hochelaga","St Henri"))
branches$place[w] <- "Montreal City"

w <- which(branches$agent %in% c("St Jean Baptiste","Hochelaga","St Henri"))
branches$agent[w] <- "Montreal City"

branches$place[which(branches$place == "Windsor" & branches$province == "Ontario")] <- "Windsor City"
branches$place[which(branches$place == "London" & branches$province == "Ontario")] <- "London City"
branches$place[which(branches$place == "Kingston" & branches$province == "Ontario")] <- "Kingston City"

branches$place[which(branches$place == "Dawson City")] <- "Dawson"
branches$place[which(branches$place == "London East")] <- "London City"
branches$place[which(branches$place == "London North")] <- "London City"

branches$place[which(branches$place == "Maisonneuve")] <- "Montreal City"
branches$place[which(branches$place == "Westmount")] <- "Montreal City"
branches$place[which(branches$place == "Outremont")] <- "Montreal City"


branches$place[which(branches$place == "Lancaster" & branches$province == "New Brunswick")] <- "St John City"

branches$place[which(branches$place == "St Boniface" & branches$province == "Manitoba")] <- "Winnipeg City"

# df_21

# a <- ref_21_csd[str_detect(ref_21_csd$subdistrict,"\\(|\\,"),]


#  Find duplicates and change name for smaller
if (change == 0){
  
  
  
  
  df_21 <- df_21_csd
  ref_21 <- as.data.frame(df_21)
  ref_21 <- ref_21[,-which(colnames(ref_21) == "geometry")]
  
df_21$subdistrict[which(df_21$subdistrict == "Richmond (part)")] <- "Richmond"
ref_21$subdistrict[which(ref_21$subdistrict == "Richmond (part)")] <- "Richmond"

df_21$subdistrict[which(df_21$subdistrict == "Lloydminster (part)")] <- "Lloydminster"
ref_21$subdistrict[which(ref_21$subdistrict == "Lloydminster (part)")] <- "Lloydminster"

df_21$subdistrict[which(df_21$subdistrict == "Yale (part)")] <- "Yale"
ref_21$subdistrict[which(ref_21$subdistrict == "Yale (part)")] <- "Yale"

df_21$subdistrict[which(df_21$subdistrict == "Boularderie (part)")] <- "Boularderie"
ref_21$subdistrict[which(ref_21$subdistrict == "Boularderie (part)")] <- "Boularderie"

df_21$subdistrict[which(df_21$subdistrict == "Tilbury (part)")] <- "Tilbury"
ref_21$subdistrict[which(ref_21$subdistrict == "Tilbury (part)")] <- "Tilbury"

df_21$subdistrict[which(df_21$subdistrict == "Tavistock (part)")] <- "Tavistock"
ref_21$subdistrict[which(ref_21$subdistrict == "Tavistock (part)")] <- "Tavistock"

df_21$subdistrict[which(df_21$subdistrict == "Eganville (part)")] <- "Eganville"
ref_21$subdistrict[which(ref_21$subdistrict == "Eganville (part)")] <- "Eganville"

df_21$subdistrict <- gsub(" \\(part\\)","",df_21$subdistrict)
ref_21$subdistrict <- gsub(" \\(part\\)","",ref_21$subdistrict)
df_21$subdistrict <- gsub(" I R","",df_21$subdistrict)
ref_21$subdistrict <- gsub(" I R","",ref_21$subdistrict)

aux <- df_21 %>% filter(str_detect(subdistrict,"de\\)|du\\)"))
aux <- separate(aux,"subdistrict",c("sub1","sub2"),sep = "\\(")
aux$sub2 <- gsub("\\)","",aux$sub2)
aux$sub1 <- paste0(aux$sub1," ",aux$sub2)
aux <- rename(aux,"subdistrict" = "sub1")
aux <- aux %>% select(-sub2)
auxx <- as.data.frame(aux) %>% select(-geometry)

aux1 <- df_21 %>% filter(!str_detect(subdistrict,"de\\)|du\\)"))
aux2 <- ref_21 %>% filter(!str_detect(subdistrict,"de\\)|du\\)"))
df_21 <- rbind(aux1,aux)
ref_21 <- rbind(aux2,auxx)

df_21$subdistrict <- gsub(" \\(.*\\)","",df_21$subdistrict)
ref_21$subdistrict <- gsub(" \\(.*\\)","",ref_21$subdistrict)

# df_21$subdistrict[which((df_21$subdistrict == "St Bruno") & (df_21$district == "Chicoutimi & Saguenay" | df_21$URB_RUR_21 == "Rural"))] <- "St Bruno_small"
# ref_21$subdistrict[which((ref_21$subdistrict == "St Bruno") & (ref_21$district == "Chicoutimi & Saguenay" | ref_21$URB_RUR_21 == "Rural"))] <- "St Bruno_small"

df_21$subdistrict[which((df_21$subdistrict == "St Jerome") & (df_21$district == "Terrebone" | df_21$URB_RUR_21 == "Rural"))] <- "St Jerome_small"
ref_21$subdistrict[which((ref_21$subdistrict == "St Jerome") & (ref_21$district == "Terrebone" | ref_21$URB_RUR_21 == "Rural"))] <- "St Jerome_small"

df_21$subdistrict[which((df_21$subdistrict == "St Joseph") & (df_21$district == "St Hyacinthe"))] <- "St Joseph_small"
df_21$subdistrict[which((df_21$subdistrict == "St Joseph") & (df_21$district == "Beauce" | df_21$URB_RUR_21 == "Rural"))] <- "St Joseph_small"
ref_21$subdistrict[which((ref_21$subdistrict == "St Joseph") & (ref_21$district == "St Hyacinthe"))] <- "St Joseph_small"
ref_21$subdistrict[which((ref_21$subdistrict == "St Joseph") & (ref_21$district == "Beauce" | ref_21$URB_RUR_21 == "Rural"))] <- "St Joseph_small"

df_21$subdistrict[which((df_21$subdistrict == "Farnham") & (df_21$district == "Brome"))] <- "Farnham_small"
ref_21$subdistrict[which((ref_21$subdistrict == "Farnham") & (ref_21$district == "Brome"))] <- "Farnham_small"

df_21$subdistrict[which((df_21$subdistrict == "St Jerome") & (df_21$district == "Lac St Jean"))] <- "St Jerome_small"
ref_21$subdistrict[which((ref_21$subdistrict == "St Jerome") & (ref_21$district == "Lac St Jean"))] <- "St Jerome_small"

# df_21$subdistrict[which((df_21$subdistrict == "Hamilton") & (df_21$district == "Northumberland West"))] <- "Hamilton_small"
# ref_21$subdistrict[which((ref_21$subdistrict == "Hamilton") & (ref_21$district == "Northumberland West"))] <- "Hamilton_small"

# df_21$subdistrict[which((df_21$subdistrict == "Montreal") & (df_21$district == "Chambly & Vercheres"))] <- "Montreal_small"
# ref_21$subdistrict[which((ref_21$subdistrict == "Montreal") & (ref_21$district == "Chambly & Vercheres"))] <- "Montreal_small"

# df_21$subdistrict[which((df_21$subdistrict == "Toronto") & (df_21$district == "Peel"))] <- "Toronto_small"
# ref_21$subdistrict[which((ref_21$subdistrict == "Toronto") & (ref_21$district == "Peel"))] <- "Toronto_small"

df_21$subdistrict[which(df_21$subdistrict == "St Francois" & df_21$district == "Montmorency")] <- "St Francois_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Francois" &  ref_21$district == "Montmorency")] <- "St Francois_small"


imuni$place[which(imuni$place == "East Tilbury")] <- "Tilbury"

df_21$subdistrict[which(df_21$subdistrict == "Tilbury" & df_21$district == "Essex")] <- "Tilbury_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Tilbury" &  ref_21$district == "Essex")] <- "Tilbury_small"

df_21$subdistrict[which(df_21$subdistrict == "Tavistock" & df_21$district == "Oxford")] <- "Tavistock_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Tavistock" &  ref_21$district == "Oxford")] <- "Tavistock_small"

df_21$subdistrict[which(df_21$subdistrict == "Burpee" & df_21$district == "Parry Sound")] <- "Burpee_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Burpee" &  ref_21$district == "Parry Sound")] <- "Burpee_small"

df_21$subdistrict[which(df_21$subdistrict == "Camden" & df_21$district == "Kent")] <- "Camden_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Camden" &  ref_21$district == "Kent")] <- "Camden_small"

df_21$subdistrict[which(df_21$subdistrict == "Mills" & df_21$district == "Manitoulin")] <- "Mills_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Mills" &  ref_21$district == "Manitoulin")] <- "Mills_small"

df_21$subdistrict[which(df_21$subdistrict == "Harrington" & df_21$district == "Saguenay")] <- "Harrington_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Harrington" &  ref_21$district == "Saguenay")] <- "Harrington_small"

df_21$subdistrict[which(df_21$subdistrict == "Longue Pointe" & df_21$district == "Temiscamingue")] <- "Longue Pointe_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Longue Pointe" &  ref_21$district == "Temiscamingue")] <- "Longue Pointe_small"

df_21$subdistrict[which(df_21$subdistrict == "Newport" & df_21$district == "Compton")] <- "Newport_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Newport" &  ref_21$district == "Compton")] <- "Newport_small"

df_21$subdistrict[which(df_21$subdistrict == "Notre Dame de Bonsecours" & df_21$district == "Rouville")] <- "Notre Dame de Bonsecours_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Notre Dame de Bonsecours" &  ref_21$district == "Rouville")] <- "Notre Dame de Bonsecours_small"

df_21$subdistrict[which(df_21$subdistrict == "St Antoine" & df_21$district == "Yamaska")] <- "St Antoine_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Antoine" &  ref_21$district == "Yamaska")] <- "St Antoine_small"

df_21$subdistrict[which(df_21$subdistrict == "St Barnabe" & df_21$district == "St Hyacinthe")] <- "St Barnabe_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Barnabe" &  ref_21$district == "St Hyacinthe")] <- "St Barnabe_small"

df_21$subdistrict[which(df_21$subdistrict == "St Camille" & df_21$district == "Wolfe")] <- "St Camille_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Camille" &  ref_21$district == "Wolfe")] <- "St Camille_small"

df_21$subdistrict[which(df_21$subdistrict == "St Clement" & df_21$district == "Beauharnois")] <- "St Clement_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Clement" &  ref_21$district == "Beauharnois")] <- "St Clement_small"

df_21$subdistrict[which(df_21$subdistrict == "St Cyprien" & df_21$district == "Dorchester")] <- "St Cyprien_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Cyprien" &  ref_21$district == "Dorchester")] <- "St Cyprien_small"

df_21$subdistrict[which(df_21$subdistrict == "St Damase" & df_21$district == "Matane")] <- "St Damase_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Damase" &  ref_21$district == "Matane")] <- "St Damase_small"

df_21$subdistrict[which(df_21$subdistrict == "St Gerard de Magella" & df_21$district == "L'Assomption")] <- "St Gerard de Magella_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Gerard de Magella" &  ref_21$district == "Yamaska")] <- "St Gerard de Magella_small"

df_21$subdistrict[which(df_21$subdistrict == "St Isidore" & df_21$district == "Laprairie")] <- "St Isidore_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Isidore" &  ref_21$district == "Laprairie")] <- "St Isidore_small"

df_21$subdistrict[which(df_21$subdistrict == "St Jean Chrysostome" & df_21$district == "Levis")] <- "St Jean Chrysostome_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Jean Chrysostome" &  ref_21$district == "Levis")] <- "St Jean Chrysostome_small"

df_21$subdistrict[which(df_21$subdistrict == "St Jean l'Evangeliste" & df_21$district == "St Jean")] <- "St Jean l'Evangeliste_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Jean l'Evangeliste" &  ref_21$district == "St Jean")] <- "St Jean l'Evangeliste_small"

df_21$subdistrict[which(df_21$subdistrict == "St Lazare" & df_21$district == "Bellechasse")] <- "St Lazare_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Lazare" &  ref_21$district == "Bellechasse")] <- "St Lazare_small"

df_21$subdistrict[which(df_21$subdistrict == "St Louis de Gonzague" & df_21$district == "Dorchester")] <- "St Louis de Gonzague_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Louis de Gonzague" &  ref_21$district == "Dorchester")] <- "St Louis de Gonzague_small"

df_21$subdistrict[which(df_21$subdistrict == "St Luc" & df_21$district == "St Jean")] <- "St Luc_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Luc" &  ref_21$district == "St Jean")] <- "St Luc_small"

df_21$subdistrict[which(df_21$subdistrict == "St Malachie" & df_21$district == "Labele")] <- "St Malachie_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Malachie" &  ref_21$district == "Labele")] <- "St Malachie_small"


df_21$subdistrict[which(df_21$subdistrict == "St Mathieu" & df_21$district == "Laprairie")] <- "St Mathieu_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Mathieu" &  ref_21$district == "Laprairie")] <- "St Mathieu_small"

df_21$subdistrict[which(df_21$subdistrict == "St Narcisse" & df_21$district == "Rimouski")] <- "St Narcisse_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Narcisse" &  ref_21$district == "Rimouski")] <- "St Narcisse_small"

df_21$subdistrict[which(df_21$subdistrict == "St Placide" & df_21$district == "Temiscamingue")] <- "St Placide_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Placide" &  ref_21$district == "Temiscamingue")] <- "St Placide_small"

df_21$subdistrict[which(df_21$subdistrict == "St Prosper" & df_21$district == "Champlian")] <- "St Prosper_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Prosper" &  ref_21$district == "Champlain")] <- "St Prosper_small"

df_21$subdistrict[which(df_21$subdistrict == "St Severin" & df_21$district == "Beauce")] <- "St Severin_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Severin" &  ref_21$district == "Beauce")] <- "St Severin_small"

df_21$subdistrict[which(df_21$subdistrict == "St Telesphore" & df_21$district == "Levis")] <- "St Telesphore_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Telesphore" &  ref_21$district == "Levis")] <- "St Telesphore_small"

df_21$subdistrict[which(df_21$subdistrict == "St Theophile" & df_21$district == "Beauce")] <- "St Theophile_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Theophile" &  ref_21$district == "Beauce")] <- "St Theophile_small"

df_21$subdistrict[which(df_21$subdistrict == "St Thomas" & df_21$district == "Joliette")] <- "St Thomas_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Thomas" &  ref_21$district == "Joliette")] <- "St Thomas_small"

df_21$subdistrict[which(df_21$subdistrict == "Ste Christine" & df_21$district == "Portneuf")] <- "Ste Christine_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Ste Christine" &  ref_21$district == "Portneuf")] <- "Ste Christine_small"

df_21$subdistrict[which(df_21$subdistrict == "Ste Croix" & df_21$district == "Lac St Jean")] <- "Ste Croix_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Ste Croix" &  ref_21$district == "Lac St Jean")] <- "Ste Croix_small"

df_21$subdistrict[which(df_21$subdistrict == "Ste Helene" & df_21$district == "Bagot")] <- "Ste Helene_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Ste Helene" &  ref_21$district == "Bagot")] <- "Ste Helene_small"

df_21$subdistrict[which(df_21$subdistrict == "Ste Marguerite" & df_21$district == "Terrebonne")] <- "Ste Marguerite_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Ste Marguerite" &  ref_21$district == "Terrebonne")] <- "Ste Marguerite_small"

df_21$subdistrict[which(df_21$subdistrict == "Ste Monique" & df_21$district == "Deux Montagnes")] <- "Ste Monique_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Ste Monique" &  ref_21$district == "Deux Montagnes")] <- "Ste Monique_small"

df_21$subdistrict[which(df_21$subdistrict == "Ste Perpetue" & df_21$district == "Nicolet")] <- "Ste Perpetue_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Ste Perpetue" &  ref_21$district == "Nicolet")] <- "Ste Perpetue_small"

df_21$subdistrict[which(df_21$subdistrict == "Ste Victoire" & df_21$district == "Arthabaska")] <- "Ste Victoire_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Ste Victoire" &  ref_21$district == "Arthabaska")] <- "Ste Victoire_small"

# df_21$subdistrict[which(df_21$subdistrict == "Argyle Head")] <- "Argyle"
# ref_21$subdistrict[which(ref_21$subdistrict == "Argyle Head")] <- "Argyle"
# 
# df_21$subdistrict[which(df_21$subdistrict == "Assinioboine I R")] <- "Assinioboia"
# ref_21$subdistrict[which(ref_21$subdistrict == "Assinioboine I R")] <- "Assinioboia"

df_21$subdistrict[which(df_21$subdistrict == "Athabaska")] <- "Athabasca"
ref_21$subdistrict[which(ref_21$subdistrict == "Athabaska")] <- "Athabasca"

df_21$subdistrict[which(df_21$subdistrict == "Ayers Cliff")] <- "Ayer's Cliff"
ref_21$subdistrict[which(ref_21$subdistrict == "Ayers Cliff")] <- "Ayer's Cliff"

df_21$subdistrict[which(str_detect(df_21$subdistrict,"Arichat"))] <- "Arichat"
ref_21$subdistrict[which(str_detect(ref_21$subdistrict,"Arichat"))] <- "Arichat"

# df_21$subdistrict[which(df_21$subdistrict == "Bolton East")] <- "Bolton"
# ref_21$subdistrict[which(ref_21$subdistrict == "Bolton East")] <- "Bolton"
# df_21$subdistrict[which(df_21$subdistrict == "Bolton Ouest")] <- "Bolton"
# ref_21$subdistrict[which(ref_21$subdistrict == "Bolton Ouest")] <- "Bolton"

# df_21$subdistrict[which(df_21$subdistrict == "Brantford East")] <- "Brantford"
# ref_21$subdistrict[which(ref_21$subdistrict == "Brantford East")] <- "Brantford"
# df_21$subdistrict[which(df_21$subdistrict == "Brantford West")] <- "Brantford"
# ref_21$subdistrict[which(ref_21$subdistrict == "Brantford West")] <- "Brantford"

df_21$subdistrict[which(df_21$subdistrict == "Charlottetown City & Royalty")] <- "Charlottetown"
ref_21$subdistrict[which(ref_21$subdistrict == "Charlottetown City & Royalty")] <- "Charlottetown"

df_21$subdistrict[which(df_21$subdistrict == "Chester Basin")] <- "Chester"
ref_21$subdistrict[which(ref_21$subdistrict == "Chester Basin")] <- "Chester"

# df_21$subdistrict[which(df_21$subdistrict == "Dorchester East")] <- "Dorchester"
# ref_21$subdistrict[which(ref_21$subdistrict == "Dorchester East")] <- "Dorchester"
# df_21$subdistrict[which(df_21$subdistrict == "Dorchester West")] <- "Dorchester"
# ref_21$subdistrict[which(ref_21$subdistrict == "Dorchester West")] <- "Dorchester"

# df_21$subdistrict[which(df_21$subdistrict == "Ft Qu'Appelle")] <- "Fort Qu'Appelle"
# ref_21$subdistrict[which(ref_21$subdistrict == "Ft Qu'Appelle")] <- "Fort Qu'Appelle"

# df_21$subdistrict[which(df_21$subdistrict == "LLoydminster")] <- "Lloydminster"
# ref_21$subdistrict[which(ref_21$subdistrict == "LLoydminster")] <- "Lloydminster"

# df_21$subdistrict[which(df_21$subdistrict == "Oak River I R")] <- "Oak River"
# ref_21$subdistrict[which(ref_21$subdistrict == "Oak River I R")] <- "Oak River"
# 
# df_21$subdistrict[which(df_21$subdistrict == "Piapot I R")] <- "Piapot"
# ref_21$subdistrict[which(ref_21$subdistrict == "Piapot I R")] <- "Piapot"

df_21$subdistrict[which(df_21$subdistrict == "Plantagenet North")] <- "Plantagenet"
ref_21$subdistrict[which(ref_21$subdistrict == "Plantagenet North")] <- "Plantagenet" # change in the other code

# df_21$subdistrict[which(df_21$subdistrict == "Swan Lake I R")] <- "Swan Lake"
# ref_21$subdistrict[which(ref_21$subdistrict == "Swan Lake I R")] <- "Swan Lake"

df_21$subdistrict[which(df_21$subdistrict == "Barrington Head & Passage")] <- "Barrington"
ref_21$subdistrict[which(ref_21$subdistrict == "Barrington Head & Passage")] <- "Barrington"

df_21$subdistrict[which(df_21$subdistrict == "Smiths Falls")] <- "Smith's Falls"
ref_21$subdistrict[which(ref_21$subdistrict == "Smiths Falls")] <- "Smith's Falls"

# branches$place[which(branches$place == "St Charles de Borromee")] <- "St Charles Borromee"

df_21$subdistrict[which(df_21$subdistrict == "St Denis")] <- "St Denis sur Richelieu"
ref_21$subdistrict[which(ref_21$subdistrict == "St Denis")] <- "St Denis sur Richelieu"

df_21$subdistrict[which(df_21$subdistrict == "Marmora and Lake")] <- "Marmora Lake"
ref_21$subdistrict[which(ref_21$subdistrict == "Marmora and Lake")] <- "Marmora Lake"

# df_21$subdistrict[which(df_21$subdistrict == "Alexander IR No 134")] <- "Alexader"
# ref_21$subdistrict[which(ref_21$subdistrict == "Alexander IR No 134")] <- "Alexander"

df_21$subdistrict <- gsub("St Marys","St Mary's",df_21$subdistrict)
ref_21$subdistrict <- gsub("St Marys","St Mary's",ref_21$subdistrict)

df_21$subdistrict <- gsub(" Settlement","",df_21$subdistrict)
ref_21$subdistrict <- gsub(" Settlement","",ref_21$subdistrict)

# df_21$subdistrict <- gsub(", UNO","",df_21$subdistrict)
# ref_21$subdistrict <- gsub(", UNO","",ref_21$subdistrict)

# df_11$subdistrict[which(df_11$subdistrict == "Dorchester South")] <- "Dorchester"
# ref_11$subdistrict[which(ref_11$subdistrict == "Dorchester South")] <- "Dorchester"
# df_11$subdistrict[which(df_11$subdistrict == "Dorchester North")] <- "Dorchester"
# ref_11$subdistrict[which(ref_11$subdistrict == "Dorchester North")] <- "Dorchester


# All Rural with more than 2 named the same


df_21$subdistrict[which(df_21$subdistrict == "Salmon River" & (df_21$district == "Digby" | df_21$district == "Guysborough"))] <- "Salmon River_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Salmon River" &  (ref_21$district == "Digby"| ref_21$district == "Guysborough"))] <- "Salmon River_small"

df_21$subdistrict[which(df_21$subdistrict == "St Augustin" & (df_21$district == "Saguenay" | df_21$district == "Deux Montagnes"))] <- "St Augustin_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Augustin" &  (ref_21$district == "Saguenay"| ref_21$district == "Deux Montagnes"))] <- "St Augustin_small"

df_21$subdistrict[which(df_21$subdistrict == "St Charles Borromee" & (df_21$district == "Chicoutimi" | df_21$district == "Joliette"))] <- "St Charles Borromee_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Charles Borromee" &  (ref_21$district == "Chicoutimi"| ref_21$district == "Joliette"))] <- "St Charles Borromee_small"

df_21$subdistrict[which(df_21$subdistrict == "St Joachim" & (df_21$district == "Montmorency" | df_21$district == "Shefford"))] <- "St Joachim_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Joachim" &  (ref_21$district == "Montmorency"| ref_21$district == "Shefford"))] <- "St Joachim_small"

df_21$subdistrict[which(df_21$subdistrict == "L'Ange Gardien" & (df_21$district == "Montmorency" | df_21$district == "Labelle"))] <- "L'Ange Gardien_small"
ref_21$subdistrict[which(ref_21$subdistrict == "L'Ange Gardien" &  (ref_21$district == "Montmorency"| ref_21$district == "Labelle"))] <- "L'Ange Gardien_small"


# extra eyeballing

df_21$subdistrict[which(df_21$subdistrict == "Annapolis Royal")] <- "Annapolis"
ref_21$subdistrict[which(ref_21$subdistrict == "Annapolis Royal")] <- "Annapolis"

df_21$subdistrict[which(df_21$subdistrict == "King")] <- "King City"
ref_21$subdistrict[which(ref_21$subdistrict == "King")] <- "King City"
df_21$subdistrict[which(df_21$subdistrict == "King City" & df_21$district == "3")] <- "King City_small"
ref_21$subdistrict[which(ref_21$subdistrict == "King City" &  ref_21$district == "3")] <- "King City_small"

df_21$subdistrict[which(df_21$subdistrict == "Middle Londonderry")] <- "Londonderry"
ref_21$subdistrict[which(ref_21$subdistrict == "Middle Londonderry")] <- "Londonderry"

imuni$place[which(imuni$place == "Carleton" & imuni$province == "Ontario")] <- "Carleton Place"

df_21$subdistrict[which(df_21$subdistrict == "Wawanosh East")] <- "Wawanosh"
ref_21$subdistrict[which(ref_21$subdistrict == "Wawanosh East")] <- "Wawanosh" # wrong in the other code

df_21$subdistrict[which(df_21$subdistrict == "Salaberry de Valleyfield")] <- "Valleyfield"
ref_21$subdistrict[which(ref_21$subdistrict == "Salaberry de Valleyfield")] <- "Valleyfield"

imuni$place[which(imuni$place == "East Wawanosh")] <- "Wawanosh"

#

df_21$subdistrict[which(df_21$subdistrict == "Richmond" & df_21$district == "Comox Alberni")] <- "Richmond_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Richmond" &  ref_21$district == "Comox Alberni")] <- "Richmond_small"

df_21$subdistrict[which(df_21$subdistrict == "Yale" & df_21$district == "Fraser Valley")] <- "Yale_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Yale" &  ref_21$district == "Fraser Valley")] <- "Yale_small"

df_21$subdistrict[which(df_21$subdistrict == "Simonds" & df_21$district == "Carleton")] <- "Simonds_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Simonds" &  ref_21$district == "Carleton")] <- "Simonds_small"

df_21$subdistrict[which(df_21$subdistrict == "Boularderie" & df_21$district == "Victoria")] <- "Boularderie_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Boularderie" &  ref_21$district == "Victoria")] <- "Boularderie_small"

df_21$subdistrict[which(df_21$subdistrict == "Caledonia" & df_21$district == "Queens")] <- "Caledonia_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Caledonia" &  ref_21$district == "Queens")] <- "Caledonia_small"

df_21$subdistrict[which(df_21$subdistrict == "Cheticamp" & df_21$district == "Digby")] <- "Cheticamp_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Cheticamp" &  ref_21$district == "Digby")] <- "Cheticamp_small"

df_21$subdistrict[which(df_21$subdistrict == "Dalhousie" & df_21$district == "Kings")] <- "Dalhousie_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Dalhousie" &  ref_21$district == "Kings")] <- "Dalhousie_small"

df_21$subdistrict[which(df_21$subdistrict == "Gay River" & df_21$district == "Colchester")] <- "Gay River_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Gay River" &  ref_21$district == "Colchester")] <- "Gay River_small"

df_21$subdistrict[which(df_21$subdistrict == "Kempt" & df_21$district == "Queens")] <- "Kempt_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Kempt" &  ref_21$district == "Queens")] <- "Kempt_small"

df_21$subdistrict[which(df_21$subdistrict == "Lawrencetown" & df_21$district == "Annapolis")] <- "Lawrencetown_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Lawrencetown" &  ref_21$district == "Annapolis")] <- "Lawrencetown_small"

df_21$subdistrict[which(df_21$subdistrict == "Little River" & df_21$district == "Halifax")] <- "Little River_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Little River" &  ref_21$district == "Halifax")] <- "Little River_small"

df_21$subdistrict[which(df_21$subdistrict == "Maitland" & df_21$district == "Annapolis")] <- "Maitland_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Maitland" &  ref_21$district == "Annapolis")] <- "Maitland_small"

df_21$subdistrict[which(df_21$subdistrict == "Middle River" & df_21$district == "Pictou")] <- "Middle River_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Middle River" &  ref_21$district == "Pictou")] <- "Middle River_small"

df_21$subdistrict[which(df_21$subdistrict == "North River" & df_21$district == "Victoria")] <- "North River_small"
ref_21$subdistrict[which(ref_21$subdistrict == "North River" &  ref_21$district == "Victoria")] <- "North River_small"

df_21$subdistrict[which(df_21$subdistrict == "Ohio" & df_21$district == "Shelburne")] <- "Ohio_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Ohio" &  ref_21$district == "Shelburne")] <- "Ohio_small"

df_21$subdistrict[which(df_21$subdistrict == "Smith Cove" & df_21$district == "Digby")] <- "Smith Cove_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Smith Cove" &  ref_21$district == "Digby")] <- "Smith Cove_small"

df_21$subdistrict[which(df_21$subdistrict == "Tracadie" & df_21$district == "Antigonish")] <- "Tracadie_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Tracadie" &  ref_21$district == "Antigonish")] <- "Tracadie_small"

df_21$subdistrict[which(df_21$subdistrict == "Notre Dame du Mont Carmel" & df_21$district == "St Jean")] <- "Notre Dame du Mont Carmel_small"
ref_21$subdistrict[which(ref_21$subdistrict == "Notre Dame du Mont Carmel" &  ref_21$district == "St Jean")] <- "Notre Dame du Mont Carmel_small"

df_21$subdistrict[which(df_21$subdistrict == "St Gerard de Magella" & df_21$district == "Yamaska")] <- "St Gerard de Magella_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Gerard de Magella" &  ref_21$district == "Yamaska")] <- "St Gerard de Magella_small"

df_21$subdistrict[which(df_21$subdistrict == "St Malachie" & df_21$district == "Labelle")] <- "St Malachie_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Malachie" &  ref_21$district == "Labelle")] <- "St Malachie_small"

df_21$subdistrict[which(df_21$subdistrict == "St Prosper" & df_21$district == "Dorchester")] <- "St Prosper_small"
ref_21$subdistrict[which(ref_21$subdistrict == "St Prosper" &  ref_21$district == "Dorchester")] <- "St Prosper_small"

}
###############################################################################

# union

###############################################################################

if (change == 0){
  
  ref_21$subdistrict_aux <- paste0(ref_21$province,ref_21$subdistrict)
  df_21$subdistrict_aux <- paste0(df_21$province,df_21$subdistrict)
  ref_21$subdistrict_aux2 <- paste0(ref_21$province,ref_21$subdistrict,ref_21$district)
  df_21$subdistrict_aux2 <- paste0(df_21$province,df_21$subdistrict,df_21$district)
  
  
  q <- filter(df_21,df_21$subdistrict_aux %in% df_21$subdistrict_aux[which(duplicated(df_21$subdistrict_aux))],
              !(str_detect(subdistrict,"Indian Reserves|Indian Reserves|LID|ND|T  R  W|T   R   W|Township|Unorganized Parts|Reserve indienne|Reserves Indiennes|Reserve Indienne|Indian reserves|No Data|Autres parties|Other Parts,
                           Remainder of province|Unorganized Northern Parts|Northern Unorganized Parts|Rural Parts|
                                       Terr W of R 28 N of TP 15 S of TP 28|Unsurveyed parts north of TCR|Parties non organisees")))
  
  for (i in as.list(unique(q$subdistrict_aux))){
    if (NROW(q$subdistrict_aux[which(q$subdistrict_aux == i & q$URB_RUR_21 == "Urban")]) == 1 & !(q$subdistrict[which(q$subdistrict_aux == i)] %in% distr)){
      
      df_21$subdistrict[which(df_21$subdistrict_aux == i & df_21$URB_RUR_21 == "Rural")] <- 
        paste0(df_21$subdistrict[which(df_21$subdistrict_aux == i & df_21$URB_RUR_21 == "Rural")],"_modrur")
      ref_21$subdistrict[which(ref_21$subdistrict_aux == i & ref_21$URB_RUR_21 == "Rural")] <- 
        paste0(ref_21$subdistrict[which(ref_21$subdistrict_aux == i & ref_21$URB_RUR_21 == "Rural")],"_modrur")
      
    } 
  }
  
  
aux <- filter(df_21,df_21$subdistrict == "St Joachim de la Pointe Claire")
NEW_joachim <- aggregate(aux[,-which(colnames(aux) %in% c("subdistrict"))], by =list(aux$subdistrict), FUN = function(x) extract2(x,1), do_union = T)
NEW_joachim <- rename(NEW_joachim,c("subdistrict" = "Group.1"))
  
q2 <- q %>% filter(!(str_detect(q$subdistrict,"_modrur") | str_detect(q$subdistrict,"_small")))

q3 <- filter(q2,(q2$subdistrict %in% distr))
q3_aux <- q3 %>% select(c("subdistrict","geometry","area","population","pop_01","pop_avg","pop_51","pop_61","pop_71","pop_81","pop_91","pop_11"))
q3_aux2 <-as.data.frame(q3) %>% select(-c("geometry","area","population","pop_01","pop_avg","pop_51","pop_61","pop_71","pop_81","pop_91","pop_11"))


NEW_distr_aux <- aggregate(q3_aux[,-which(colnames(q3_aux) %in% c("subdistrict"))], by =list(q3_aux$subdistrict), FUN = function(x) sum(x,na.rm = T), do_union = T)
NEW_distr_aux <- rename(NEW_distr_aux,c("subdistrict" = "Group.1"))
NEW_distr_aux2 <- aggregate(q3_aux2[,-which(colnames(q3_aux2) %in% c("subdistrict"))], by =list(q3_aux2$subdistrict), FUN = function(x) extract2(x,1))
NEW_distr_aux2 <- rename(NEW_distr_aux2,c("subdistrict" = "Group.1"))

NEW_distr <- left_join(NEW_distr_aux,NEW_distr_aux2, by = "subdistrict")


q3 <- filter(q2,!(q2$subdistrict == "St Joachim de la Pointe Claire") & !(q2$URB_RUR_21 == "Rural") & !(q2$subdistrict %in% distr))
q3 <- filter(q3,q3$subdistrict_aux2 %in% q3$subdistrict_aux2[which(duplicated(q3$subdistrict_aux2))])

if (NROW(q3) > 0){

  q3_aux <- q3 %>% select(c("subdistrict","geometry","area","population","pop_01","pop_avg","pop_51","pop_61","pop_71","pop_81","pop_91","pop_11"))
  q3_aux2 <-as.data.frame(q3) %>% select(-c("geometry","area","population","pop_01","pop_avg","pop_51","pop_61","pop_71","pop_81","pop_91","pop_11"))
  
  
  NEW_urban_aux <- aggregate(q3_aux[,-which(colnames(q3_aux) %in% c("subdistrict"))], by =list(q3_aux$subdistrict), FUN = function(x) sum(x,na.rm = T), do_union = T)
  NEW_urban_aux <- rename(NEW_urban_aux,c("subdistrict" = "Group.1"))
  NEW_urban_aux2 <- aggregate(q3_aux2[,-which(colnames(q3_aux2) %in% c("subdistrict"))], by =list(q3_aux2$subdistrict), FUN = function(x) extract2(x,1))
  NEW_urban_aux2 <- rename(NEW_urban_aux2,c("subdistrict" = "Group.1"))
  
  NEW_urban <- left_join(NEW_urban_aux,NEW_urban_aux2, by = "subdistrict")
  
  
} else{
  
  NEW_urban <- NULL
  
}


q4 <- filter(q2,!(q2$subdistrict == "St Joachim de la Pointe Claire") & !(q2$URB_RUR_21 == "Urban") & !(q2$subdistrict %in% distr))
q4 <- filter(q4,q4$subdistrict_aux2 %in% q4$subdistrict_aux2[which(duplicated(q4$subdistrict_aux2))])

aux <- as.data.frame(q4) %>% group_by(subdistrict) %>% count()

q4 <- q4[q4$subdistrict %in% aux$subdistrict[which(aux$n == 2)],]
q4_aux <- q4 %>% select(c("subdistrict","geometry","area","population","pop_01","pop_avg","pop_51","pop_61","pop_71","pop_81","pop_91","pop_11"))
q4_aux2 <-as.data.frame(q4) %>% select(-c("geometry","area","population","pop_01","pop_avg","pop_51","pop_61","pop_71","pop_81","pop_91","pop_11"))

NEW_rural_aux <- aggregate(q4_aux[,-which(colnames(q4_aux) %in% c("subdistrict"))], by =list(q4_aux$subdistrict), FUN = function(x) sum(x,na.rm = T), do_union = T)
NEW_rural_aux <- rename(NEW_rural_aux,c("subdistrict" = "Group.1"))
NEW_rural_aux2 <- aggregate(q4_aux2[,-which(colnames(q4_aux2) %in% c("subdistrict"))], by =list(q4_aux2$subdistrict), FUN = function(x) extract2(x,1))
NEW_rural_aux2 <- rename(NEW_rural_aux2,c("subdistrict" = "Group.1"))

NEW_rural <- left_join(NEW_rural_aux,NEW_rural_aux2, by = "subdistrict")

# add NEW

# bind all new data frames and aggregate the ones left (only Vancouver City)

df_21_NEW <- rbind(NEW_joachim,NEW_distr,NEW_rural,NEW_urban, deparse.level = 1)
# q5_aux <- q5 %>% select(c("subdistrict","geometry","area","population","pop_01","pop_avg","pop_51","pop_61","pop_71","pop_81","pop_91","pop_11"))
# q5_aux2 <-as.data.frame(q5) %>% select(-c("geometry","area","population","pop_01","pop_avg","pop_51","pop_61","pop_71","pop_81","pop_91","pop_11"))


# df_21_NEW_aux <- aggregate(q5_aux[,-which(colnames(q5_aux) %in% c("subdistrict"))], by =list(q5_aux$subdistrict), FUN = function(x) sum(x,na.rm = T), do_union = T)
# df_21_NEW_aux <- rename(df_21_NEW_aux,c("subdistrict" = "Group.1"))
# df_21_NEW_aux2 <- aggregate(q5_aux2[,-which(colnames(q5_aux2) %in% c("subdistrict"))], by =list(q5_aux2$subdistrict), FUN = function(x) extract2(x,1))
# df_21_NEW_aux2 <- rename(df_21_NEW_aux2,c("subdistrict" = "Group.1"))
# 
# df_21_NEW <- left_join(df_21_NEW_aux,df_21_NEW_aux2, by = "subdistrict")

# get non-changed ones

df_21_OLD <- df_21[-which(df_21$subdistrict_aux2 %in% df_21_NEW$subdistrict_aux2),]

# bind both old and new

df_212 <- rbind(df_21_OLD,df_21_NEW, deparse.level = 1)
ref_212 <- as.data.frame(df_212) %>% select(-geometry)

# bind smalls and modrurs with equal subdist, dist and prov.

ref_212$subdistrict_aux <- paste0(ref_212$province,ref_212$subdistrict)
df_212$subdistrict_aux <- paste0(df_212$province,df_212$subdistrict)
ref_212$subdistrict_aux2 <- paste0(ref_212$province,ref_212$subdistrict,ref_212$district)
df_212$subdistrict_aux2 <- paste0(df_212$province,df_212$subdistrict,df_212$district)

q <- filter(df_212,df_212$subdistrict_aux %in% df_212$subdistrict_aux[which(duplicated(df_212$subdistrict_aux))],
            !(str_detect(subdistrict,"Indian Reserves|Indian Reserves|LID|ND|T  R  W|T   R   W|Township|Unorganized Parts|Reserve indienne|Reserves Indiennes|Reserve Indienne|Indian reserves|No Data|Autres parties|Other Parts,
                           Remainder of province|Unorganized Northern Parts|Northern Unorganized Parts|Rural Parts|
                                       Terr W of R 28 N of TP 15 S of TP 28|Unsurveyed parts north of TCR|Parties non organisees")))
q6 <- q
q6_aux <- q6 %>% select(c("subdistrict_aux2","geometry","area","population","pop_01","pop_avg","pop_51","pop_61","pop_71","pop_81","pop_91","pop_11"))
q6_aux2 <-as.data.frame(q6) %>% select(-c("geometry","area","population","pop_01","pop_avg","pop_51","pop_61","pop_71","pop_81","pop_91","pop_11"))

df_212_NEW_aux <- aggregate(q6_aux[,-which(colnames(q6_aux) %in% c("subdistrict_aux2"))], by =list(q6_aux$subdistrict_aux2), FUN = function(x) sum(x,na.rm = T), do_union = T)
df_212_NEW_aux <- rename(df_212_NEW_aux,c("subdistrict_aux2" = "Group.1"))
df_212_NEW_aux2 <- aggregate(q6_aux2[,-which(colnames(q6_aux2) %in% c("subdistrict_aux2"))], by =list(q6_aux2$subdistrict_aux2), FUN = function(x) extract2(x,1))
df_212_NEW_aux2 <- rename(df_212_NEW_aux2,c("subdistrict_aux2" = "Group.1"))

df_21_NEW <- left_join(df_212_NEW_aux,df_212_NEW_aux2, by = "subdistrict_aux2")
df_21_NEW <- df_21_NEW %>% select(-subdistrict_aux2,-subdistrict_aux)

# get non-changed ones

df_21_OLD <- df_212[-which(df_212$subdistrict_aux %in% q$subdistrict_aux),]
df_21_OLD <- df_21_OLD %>% select(-subdistrict_aux2,-subdistrict_aux)

# bind both old and new

df_212 <- rbind(df_21_OLD,df_21_NEW, deparse.level = 1)
ref_212 <- as.data.frame(df_212) %>% select(-geometry)


}