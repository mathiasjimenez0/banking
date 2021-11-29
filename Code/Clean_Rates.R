# Interest rates


# create date column

imuni$month[which(imuni$month == "June")] <- "Jun"
imuni$month[which(imuni$month == "July")] <- "Jul"
imuni$month <- match(imuni$month,month.abb)
imuni$date <-  as.yearmon(paste(imuni$year,imuni$month), "%Y %m")

imuni <- select(imuni,-c(year,month))


imuni <- filter(imuni, str_length(imuni$place) > 1, !(is.na(imuni$place)))
imuni <- filter(imuni,  is.na(imuni$sd) )


imuni <- select(imuni,-place2)
imuni <- imuni[-which(str_detect(imuni$place,"(Province of|Dominion of Canada)")),]
imuni$place <- gsub(".", "", imuni$place,fixed = T)
imuni <- separate(imuni,place,c("place","province"), ",")

imuni$province <- trimws(imuni$province , "both")

imuni$province[which(imuni$province %in% c("Sask","Sas"))] <- "Saskatchewan"
imuni$province[which(imuni$province %in% c("NS"))] <- "Nova Scotia"
imuni$province[which(imuni$province %in% c("Que","Quebec"))] <- "Quebec"
imuni$province[which(imuni$province %in% c("NB"))] <- "New Brunswick"
imuni$province[which(imuni$province %in% c("Ont"))] <- "Ontario"
imuni$province[which(imuni$province %in% c("Alta","Alberta"))] <- "Alberta"
imuni$province[which(imuni$province %in% c("BC"))] <- "British Columbia"
imuni$province[which(imuni$province %in% c("PEI"))] <- "Prince Edward Island"
imuni$province[which(imuni$province %in% c("Man"))] <- "Manitoba"


imuni$rate <- lapply(strsplit(
  as.character(imuni$rate), ",", fixed=T),
  function(x) mean(as.numeric(x)))

imuni$rate <- as.numeric(imuni$rate)

imuni$issue <- str_remove(imuni$issue,"[[:space:]][[0-9]][[0-9]]$")
imuni$highest <- str_remove(imuni$highest,"[[:space:]][[0-9]][[0-9]]$")
imuni$lowest <- str_remove(imuni$lowest,"[[:space:]][[0-9]][[0-9]]$")

imuni$place <- gsub("(Township|Tp|Twp|Municipality|County|Township of |Tp of |Twp of |Rural Municipality of |County of )",""
                     ,imuni$place)

# write_csv2(imuni, "./Dataset/Interest Rates_imuni.csv")

imuni$place <- stri_trans_general(imuni$place ,"Latin-ASCII")
iavg$place <- stri_trans_general(iavg$place ,"Latin-ASCII")
icity$place <- stri_trans_general(icity$place ,"Latin-ASCII")
ifarm$place <- stri_trans_general(ifarm$place ,"Latin-ASCII")

imuni$place <- gsub("-"," ",imuni$place)
iavg$place <- gsub("-"," ",iavg$place)
icity$place <- gsub("-"," ",icity$place)
ifarm$place <- gsub("-"," ",ifarm$place)

imuni$place <- gsub("\\.","",imuni$place)
iavg$place <- gsub("\\.","",iavg$place)
icity$place <- gsub("\\.","",icity$place)
ifarm$place <- gsub("\\.","",ifarm$place)

imuni$province <- gsub("(^\\s+)|(\\s+$)", "", imuni$province)
imuni$place <- gsub("(^\\s+)|(\\s+$)", "", imuni$place)
iavg$province <- gsub("(^\\s+)|(\\s+$)", "", iavg$province)
iavg$place <- gsub("(^\\s+)|(\\s+$)", "", iavg$place)
icity$province <- gsub("(^\\s+)|(\\s+$)", "", icity$province)
icity$place <- gsub("(^\\s+)|(\\s+$)", "", icity$place)
ifarm$province <- gsub("(^\\s+)|(\\s+$)", "", ifarm$province)
ifarm$place <- gsub("(^\\s+)|(\\s+$)", "", ifarm$place)

imuni$place <- gsub("  ", "", imuni$place)
iavg$place <- gsub("  ", "", iavg$place)
icity$place <- gsub("  ", "", icity$place)
ifarm$place <- gsub("  ", "", ifarm$place)

imuni$province <- as.character(imuni$province)
icity$province <- as.character(icity$province)
ifarm$province <- as.character(ifarm$province)
iavg$province <- as.character(iavg$province)

icity <- gather(icity, year, rate, -table, -province, -place)
ifarm <- select(ifarm,-notes)
ifarm <- gather(ifarm, year, rate, -table, -province, -place)
iavg <- gather(iavg, year, rate, -table, -province, -place)

icity$rate <- lapply(strsplit(
  as.character(icity$rate), ",", fixed=T),
  function(x) mean(as.numeric(x)))

ifarm$rate <- lapply(strsplit(
  as.character(ifarm$rate), ",", fixed=T),
  function(x) mean(as.numeric(x)))

iavg$rate <- lapply(strsplit(
  as.character(iavg$rate), ",", fixed=T),
  function(x) mean(as.numeric(x)))

imuni$place[which(imuni$place == "Catholic Montreal")] <- "Montreal"
imuni$place[which(imuni$place == "St Jean de la Croix")] <- "St Jean"

imuni$place <- gsub("Saint ","St ",imuni$place)
imuni$place <- gsub("Sainte ","Ste ",imuni$place)

imuni$place[which(imuni$place == "East Nissouri")] <- "Nissouri East"
imuni$place[which(imuni$place == "West Nissouri")] <- "Nissouri West"

imuni$place[which(imuni$place == "St Marie")] <- "Ste Marie"

imuni$place[which(imuni$place == "Peterboro")] <- "Peterborough"

imuni$place[which(imuni$place == "St Boniface" & imuni$province == "QC")] <- "Shawinigan"

imuni$place[which(imuni$place == "Metcalf")] <- "Metcalfe"


