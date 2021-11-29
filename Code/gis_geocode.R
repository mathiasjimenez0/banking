filename <- "./Dataset/R/GIS.RData" 
load(filename)

gis$place[which(gis$place == "Lynden")] <- "Lynden, Hamilton"
gis$province[which(gis$place == "St Chrysostom")] <- "QC"
gis$province[which(gis$place == "Yorkton" & gis$province == "NT")] <- "SK"
gis$province[which(gis$place == "Lumsden" & gis$province == "NT")] <- "SK"
gis$province[which(gis$place == "Macleod" & gis$province == "NT")] <- "AB"
gis$province[which(gis$place == "Greenville Ferry")] <- "NS"
gis$place[which(gis$place == "Greenville Ferry")] <- "Greenville"
gis$place[which(gis$place == "Cypress Road")] <- "Cypress River"
gis$province[which(gis$place == "Lenore")] <- "MB"

gis <- unite(gis,"place",place:province,sep = ",", na.rm = T)
gis$place <- paste0(gis$place,",Canada")

gis <- mutate_geocode(gis,place)
Can <- map_data("world") %>% filter(region=="Canada")

filename <- "./Dataset/R/gis_geocode.RData"
save.image(file = filename)


