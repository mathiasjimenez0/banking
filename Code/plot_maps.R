# pop_csd, pop, manuf_csd, manuf_cd

# years <- c(1874,1891,1911,1921)
y <- 1921
p <- 1
# years <- c(1891,1905,1921)
years <- 1921
sd <- 1
mid <- 0
meth <- "linear"
popu <- "population"
#####
if (p == 1){
  if (sd == 1){
    pop_aux <- pop_csd[which(pop_csd$year %in% years),c("UID_CSD_21","year","method",popu)] %>% filter(method == meth)
    pop_aux$aux <- paste0(pop_aux$UID_CSD_21,pop_aux$year)    
    if (mid == 1){
      pop_aux <- pop_aux %>% filter(str_detect(UID_CSD_21,"AB|MB|SK"))      
    } else{
      # pop_aux <- pop_aux %>% filter(str_detect(UID_CSD_21,"QC|ON"))
      # pop_aux <- pop_aux %>% filter(str_detect(UID_CSD_21,"NB|NS|PE"))
      # pop_aux <- pop_aux %>% filter(str_detect(UID_CSD_21,"BC")) 
    }
  } else{
    pop_aux <- pop[which(pop$year %in% years),c("UID_CD_21",popu,"year","method")] %>% filter(method == meth)
    pop_aux$aux <- paste0(pop_aux$UID_CD_21,pop_aux$year)  
    if (mid == 1){
      pop_aux <- pop_aux %>% filter(str_detect(UID_CD_21,"AB|MB|SK"))      
    } else{
      # pop_aux <- pop_aux %>% filter(str_detect(UID_CD_21,"QC|ON"))
      # pop_aux <- pop_aux %>% filter(str_detect(UID_CD_21,"NB|NS|PE")) 
      # pop_aux <- pop_aux %>% filter(str_detect(UID_CD_21,"BC")) 
    }
  }

  pop_aux <- pop_aux %>% filter(!(duplicated(aux))) 
  pop_aux$aux <- NULL
  
  pop_aux <- pop_aux %>% filter(year == y)
  br <- as.vector(quantile(pop_aux[which(pop_aux[,c(popu)] > 0),c(popu)],probs = seq(0,1,0.1),  na.rm = T ))
  br <- c(0,br)
  value_aux <- pop_aux %>% rename(value = popu)
  
}else{
 
  if (sd == 1){
    manuf_aux <- manuf_csd[which(manuf_csd$year %in% years),c("UID_CSD_21","sales","year")]
    manuf_aux$aux <- paste0(manuf_aux$UID_CSD_21,manuf_aux$year)    
    if (mid == 1){
      manuf_aux <- manuf_aux %>% filter(str_detect(UID_CSD_21,"AB|MB|SK"))      
    }
  } else{
    manuf_aux <- manuf_cd[which(manuf_cd$year %in% years),c("UID_CD_21","sales","year")]
    manuf_aux$aux <- paste0(manuf_aux$UID_CD_21,manuf_aux$year)  
    if (mid == 1){
      manuf_aux <- manuf_aux %>% filter(str_detect(UID_CD_21,"AB|MB|SK"))      
    }
  }
  manuf_aux <- manuf_aux %>% filter(!(duplicated(aux))) 
  manuf_aux$aux <- NULL
  
  br <- as.vector(quantile(manuf_aux$sales,probs = seq(0,1,0.1),  na.rm = T ))
  manuf_aux <- manuf_aux %>% filter(year == y)
  value_aux <- manuf_aux %>% rename(value = sales)
}

if (mid == 1){
  if (sd == 1){
    
    df_aux <- df_csd %>% filter(str_detect(UID_CSD_21,"AB|MB|SK"))  
    
    z <- left_join(df_aux,value_aux, by = c("UID_CSD_21"))
  } else{
    df_aux <- df_cd %>% filter(str_detect(UID_CD_21,"AB|MB|SK"))
    z <- left_join(df_aux,value_aux, by = c("UID_CD_21"))
  }
} else{
  if (sd == 1){
    df_aux <- df_csd
    # df_aux <- df_csd %>% filter(str_detect(UID_CSD_21,"QC|ON"))
    # df_aux <- df_csd %>% filter(str_detect(UID_CSD_21,"NB|NS|PE")) 
    # df_aux <- df_csd %>% filter(str_detect(UID_CSD_21,"BC"))
    
    z <- left_join(df_aux,value_aux, by = c("UID_CSD_21"))
  } else{
    df_aux <- df_cd
    # df_aux <- df_cd %>% filter(str_detect(UID_CD_21,"QC|ON"))
    # df_aux <- df_cd %>% filter(str_detect(UID_CD_21,"NB|NS|PE"))
    # df_aux <- df_cd %>% filter(str_detect(UID_CD_21,"BC"))
    z <- left_join(df_aux,value_aux, by = c("UID_CD_21"))
  }
}



# z <- z %>% filter(!(province %in% "North Western Territories" & value == 0))

# u <- 1
# r1 <- -2
# r2 <- -3
u <- 1
r1 <- 1
r2 <- 1
# l <- c( paste0("[",as.character(round(br[1],r1)/u)," - ",as.character(round(br[4],r1)/u),")"),
l <- c( paste0("[",as.character(round(br[1],r1)/u)," - ",as.character(round(br[2],r1)/u),")"),
paste0("[",as.character(round(br[2],r1)/u)," - ",as.character(round(br[3],r2)/u),")"),
paste0("[",as.character(round(br[3],r2)/u)," - ",as.character(round(br[4],r2)/u),")"),
paste0("[",as.character(round(br[4],r2)/u)," - ",as.character(round(br[5],r2)/u),")"),
paste0("[",as.character(round(br[5],r2)/u)," - ",as.character(round(br[6],r2)/u),")"),
        paste0("[",as.character(round(br[6],r2)/u)," - ",as.character(round(br[7],r2)/u),")"),
        paste0("[",as.character(round(br[7],r2)/u)," - ",as.character(round(br[8],r2)/u),")"),
        paste0("[",as.character(round(br[8],r2)/u)," - ",as.character(round(br[9],r2)/u),")"),
        paste0("[",as.character(round(br[9],r2)/u)," - ",as.character(round(br[10],r2)/u),")"),
        paste0("[",as.character(round(br[10],r2)/u)," - ",as.character(round(br[11],r2)/u),")"),
        paste0("[",as.character(round(br[11],r2)/u)," - ",as.character(round(br[12],r2)/u),")"))

getPalette = colorRampPalette(brewer.pal(8, "PuBu"))
z$breaks <- cut(z$value, 
                # breaks= unique(br[-c(2,3)]),
                breaks= unique(br),
                labels=l, include.lowest = T)
z$position <- cut(z$value, 
                  breaks= unique(br),
                  # breaks= unique(br[-c(2,3)]),
                  # labels=1:(length(unique(br[-c(2,3)]))-1), include.lowest = T)
labels=1:(length(unique(br))-1), include.lowest = T)

col <- getPalette(length(l))

# pdf(file = paste0("./race_",str_sub(y,3,4),".pdf"),   # The directory you want to save the file in
#     width = 8, # The width of the plot in inches
#     height = 6) # The height of the plot in inches


p <- ggplot() +
  geom_sf(data = z, aes(fill = breaks)) +
  # scale_fill_manual(values =  col[c(sort(unique(z$position)))] , name = "Manufacturing + \n farm sales ($M)") +
  scale_fill_manual(values =  col[c(sort(unique(z$position)))] , name = "Population") +
  geom_sf(data = trains[which(trains$CNSTRCTD <= y & trains$ABNDND >= y),], color = "yellow", alpha = 1) +
  # geom_sf(data = trains_place[which(trains_place$CNSTRCTD <= y & trains_place$ABNDND >= y),], aes(geometry = geometry), pch = 21, alpha = 1, fill = "red", stat = "sf_coordinates" ) +
  geom_point(data = branches[which(branches$year == branches$year[which.min(abs(y-branches$year))]),], aes(geometry = geometry), size = 1.5,pch = 21, alpha = 1, fill = "red", stat = "sf_coordinates" ) +
  labs(title = as.character(y),
       x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(nrow = 2, byrow = T)) +
  theme(
    legend.title = element_text(size = 15),
    legend.text=element_text(size= 15),
    plot.title = element_text(hjust = 0.5,vjust = 0.5, size = 25),
    legend.position = "bottom"
  ) 

#

if (mid == 1){
  p <- p + coord_sf(crs = 4326,xlim = c(-115, -95), ylim = c(49, 54)) 
}else{
  # p <- p + coord_sf(crs = 4326,xlim = c(-90, -70), ylim = c(42, 50)) # AB|SK|MB
  # p <- p + coord_sf(crs = 4326,xlim = c(-130, -110), ylim = c(48, 54)) # BC
  # p <- p + coord_sf(crs = 4326,xlim = c(-75, -68), ylim = c(45, 48)) # QC
  p <- p + coord_sf(crs = 4326,xlim = c(-84, -74), ylim = c(42, 48)) # ON
  # p <- p + coord_sf(crs = 4326,xlim = c(-68, -60), ylim = c(43, 48)) # NB|NS|PEI
}
# p <- p + coord_sf(crs = 4326) 

# dev.off()




# branches without trains



# a <- z_csd %>% filter(period > 0, train == 0,nb_d == 1, year == 1910)
a <- z_csd %>% filter(pop <= 1851)
df_csd$z <- 0
df_csd$z[which(df_csd$UID_CSD_21 %in% a$UID_CSD_21)] <- 1

ggplot() +
  geom_sf(data = df_csd, aes(fill= as.factor(z))) +
  # geom_sf(data = df_csd) +
  geom_sf(data = trains[which(trains$CNSTRCTD <= 1857 & trains$ABNDND >= 1857),], color = "yellow", alpha = 1) +
  # geom_sf(data = trains[which(trains$CNSTRCTD <= 0),], color = "yellow", alpha = 1) +
  geom_point(data = branches[which(branches$year == 1857),], aes(geometry = geometry), size = 1.5,pch = 21, alpha = 1, fill = "red", stat = "sf_coordinates" ) +
  labs(title = "",
       x = "Longitude", y = "Latitude") +
  scale_discrete_manual(aesthetics = c("fill"), values =  c("white","blue"), name = "Branch without train?") +
  theme(
    legend.title = element_text(size = 15),
    legend.text=element_text(size= 15),
    plot.title = element_text(hjust = 0.5,vjust = 0.5, size = 25),
    legend.position = "bottom",
    # panel.background = element_rect(fill = "lightblue",
    #                                 colour = "lightblue",
    #                                 size = 0.5, linetype = "solid"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  # coord_sf(crs = 4326)
coord_sf(crs = 4326,xlim = c(-85, -60), ylim = c(42, 50))



#####
p <- 0
m = 1
c = 1

y <- 1911
yy <- "11"

# df_aux <- df_91_cd %>% filter(str_detect(province,"Manitoba|Western"))
# ref_aux <- ref_91_cd %>% filter(str_detect(province,"Manitoba|Western"))

df_aux <- df_11_cdcd %>% filter(str_detect(UID_CD_11,"AB|MB|SK"))
ref_aux <- ref_11_cdcd %>% filter(str_detect(UID_CD_11,"AB|MB|SK"))

pop_aux_all <- c(ref_aux$population)
# pop_aux_all <- c(ref_71_cd$population,ref_81_cd$population,ref_91_cd$population,ref_01_cd$population,ref_11_cdcd$population)

manuf_cd_aux <- manuf_11_cd %>% filter(str_detect(UID_CD_11,"AB|MB|SK"))
country_cd_aux <- country_11_cd %>% filter(str_detect(UID_CD_11,"AB|MB|SK"))

#####
if (p == 1){

  br <- as.vector(quantile(pop_aux_all,probs = seq(0,1,0.1),  na.rm = T ))
  z <- df_aux %>% rename(value = population)
  
}else{
  
  if (m == 1 & c == 1){
    country_cd_aux <- country_cd_aux %>% mutate(sales_c = sales_stock + sales_field) %>% select(-sales_field,-sales_stock)
    
    
    value_aux <- left_join(manuf_cd_aux[,c(paste0("UID_CD_",yy),"sales")],country_cd_aux[,c(paste0("UID_CD_",yy),"sales_c")], by = paste0("UID_CD_",yy))
    value_aux <- value_aux %>% mutate(value = sales + sales_c) %>% select(-sales_c,-sales)
    
  }else if (m == 1 & c == 0){
    
    value_aux <- manuf_cd_aux %>% rename(value = sales)
    
  }else if (m == 0 & c == 1){
    
    country_cd_aux <- country_cd_aux %>% mutate(sales_c = sales_stock + sales_field) %>% select(-sales_field,-sales_stock)
    
    value_aux <- country_cd_aux %>% rename(value = sales_c)
    
    
  }
  br <- as.vector(quantile(value_aux$value,probs = seq(0,1,0.1),  na.rm = T ))
  z <- left_join(df_aux,value_aux, by = c(paste0("UID_CD_",yy)))
}


# z <- z %>% filter(!(province %in% "North Western Territories" & value == 0))


u <- 1
r1 <- 0
r2 <- 0
# l <- c( paste0("[",as.character(round(br[1],r1)/u)," - ",as.character(round(br[5],r1)/u),")"),
        l <- c( paste0("[",as.character(round(br[1],r1)/u)," - ",as.character(round(br[2],r1)/u),")"),
        paste0("[",as.character(round(br[2],r1)/u)," - ",as.character(round(br[3],r2)/u),")"),
        paste0("[",as.character(round(br[3],r2)/u)," - ",as.character(round(br[4],r2)/u),")"),
        paste0("[",as.character(round(br[4],r2)/u)," - ",as.character(round(br[5],r2)/u),")"),
        paste0("[",as.character(round(br[5],r2)/u)," - ",as.character(round(br[6],r2)/u),")"),
        paste0("[",as.character(round(br[6],r2)/u)," - ",as.character(round(br[7],r2)/u),")"),
        paste0("[",as.character(round(br[7],r2)/u)," - ",as.character(round(br[8],r2)/u),")"),
        paste0("[",as.character(round(br[8],r2)/u)," - ",as.character(round(br[9],r2)/u),")"),
        paste0("[",as.character(round(br[9],r2)/u)," - ",as.character(round(br[10],r2)/u),")"),
        paste0("[",as.character(round(br[10],r2)/u)," - ",as.character(round(br[11],r2)/u),")"))

getPalette = colorRampPalette(brewer.pal(8, "PuBu"))
z$breaks <- cut(z$value, 
                breaks= unique(br), 
                labels=l, include.lowest = T)
z$position <- cut(z$value, 
                  breaks= unique(br), 
                  labels=1:(length(unique(br))-1), include.lowest = T)

col <- getPalette(length(l))

# pdf(file = paste0("./race_",str_sub(y,3,4),".pdf"),   # The directory you want to save the file in
#     width = 8, # The width of the plot in inches
#     height = 6) # The height of the plot in inches

ggplot() +
  geom_sf(data = z, aes(fill = breaks)) +
  # scale_fill_manual(values =  col[c(sort(unique(z$position)))] , name = "Manufacturing + \n farm sales ($M)") +
  scale_fill_manual(values =  col[c(sort(unique(z$position)))] , name = "Population") +
  geom_sf(data = trains[which(trains$CNSTRCTD <= y),], color = "yellow", size = 1) +
  geom_point(data = branches_cd[which(branches_cd$year == branches_cd$year[which.min(abs(y-branches_cd$year))]),], aes(geometry = geometry), size = 1.5,pch = 21, fill = "red", stat = "sf_coordinates" ) +
  coord_sf(crs = 4326,xlim = c(-115, -95), ylim = c(49, 54)) +
  labs(title = as.character(y),
       x = "Longitude", y = "Latitude") +
  guides(fill = guide_legend(nrow = 2, byrow = T)) +
  theme(
    legend.title = element_text(size = 15),
    legend.text=element_text(size= 15),
    plot.title = element_text(hjust = 0.5,vjust = 0.5, size = 25),
    legend.position = "bottom"
  ) 

# dev.off()
#####



z <- z %>% filter(district %in% c("Toronto City","York South"))

ggplot() +
  geom_sf(data = z) +
  # geom_sf(data = trains[which(trains$CNSTRCTD <= 1910),], color = "yellow") +
  geom_point(data = branches[which(branches$year == 1910),], aes(geometry = geometry), size = 1, alpha = 0.25, color = "red", stat = "sf_coordinates" ) +
  # coord_sf(crs = 4326,xlim = c(-115, -95), ylim = c(48, 55)) + 
  # scale_fill_brewer(palette = "PuBu") +
  coord_sf(crs = 4326,xlim = c(-80, -79), ylim = c(43.5, 44))
# coord_sf(crs = 4326)


# CC

# CC_g <- c("Montreal City","Toronto City","Halifax City","St John City","Victoria City","Winnipeg City")
# CC_p <- CC %>% filter(turnover %in% c(1,1.1))
# CC_p$group <- 0
# CC_p <- CC_p %>% mutate_cond(subdistrict %in% CC_g, group = 1)

z <- df_cd %>% filter(!(province %in% "North Western Territories"))
z2 <- CC_p
z2$geometry <- st_centroid(CC_p$geometry)
z3 <- z2 %>% filter(year %in% c("1874","1891","1911","1921"))
z3 <- rbind(z3,z3[1,])
z3$year[NROW(z3)] <- 1874
z3$geometry[NROW(z3)] <- NA

ggplot() +
  geom_sf(data = z, color = "grey", alpha = 0.35) +
  # scale_color_manual(values = c("blue")) +
  new_scale_color() +
  geom_point(data = z3, aes(geometry = geometry, fill = factor(group)), size = 2, pch = 21, alpha = 1, stat = "sf_coordinates" ) + 
  # scale_fill_manual(values = c("red","yellow")) +
  coord_sf(crs = 4326, ylim = c(42, 55), xlim = c(-130, -60)) +
  scale_fill_manual(name = "Settlement", labels = c("Drafts", "Gold"), values = c("red","yellow")) +
  guides(fill = guide_legend(override.aes = list(size=5))) +
  labs(title = "",
       x = "Longitude", y = "Latitude") +
  theme(
    legend.title = element_text(size = 15),
    legend.text=element_text(size= 15),
    legend.position = "bottom"
    # legend.key.size = unit(20,"cm")
    # title = element_markdown()
    # plot.title = element_text(size=15, face="bold", hjust = 0.5,
    #                           margin = margin(10, 10, 10, 10)),
    # axis.text.x = element_text(size=12, vjust= 0.5),
    # axis.title.x = element_text(size=15, vjust=0.5),
    # axis.text.y = element_text(size=12, vjust=0.5),
    # axis.title.y = element_text(size=15, vjust=1)
  ) + 
  facet_wrap(year ~., ncol = 2, nrow = 2)
