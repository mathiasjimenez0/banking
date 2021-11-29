# 1891: use 1901 districts for Manitoba and 1891 subdistricts for the other provinces

df_aux_01 <- df_01_cd %>% filter(province %in% c("Manitoba","North Western Territories"))
df_aux_01 <- st_transform(df_aux_01, crs = 4326)
df_aux_01[,c(3,4,5,6,7)] <- NA
df_aux_91 <- df_aux_01[c(rep(1,3),rep(2,2),rep(3,4),rep(10,3),rep(12,1),
                    rep(4,1),rep(5,1),rep(6,1),rep(7,1),rep(9,1),rep(11,1),
                    rep(13,1),rep(8,1)),]
df_aux_91$district <- c("Calgary & Red Deer","Edmonton","McLeod","Broadview","Qu'Appelle","Maple Creek","Medicine Hat","Moose Jaw & Regina",
                           "Swift Current","Battleford","Carrot River & Lake Winnipeg","Prince Albert",df_aux_91$district[13:NROW(df_aux_91)])
df_aux_91 <- df_aux_91[,c(2,1,NCOL(df_aux_91),3:(NCOL(df_aux_91)-1))]
df_aux_91$population <- c(11199,6875,7203,13676,6806,689,1316,7565,320,2790,1484,6876,32168,25575,28585,22776,13123,15469,21339,25639,0)

multipoints <- st_multipoint(matrix(c(-120,53,-110,53,-120,50,-110,50,-102.75,48,-102.75,53,-106.5,48,-106.5,53,-108,48,-108,53,
                                      -109.5,48,-109.5,53,
                                      -103,51,-103,56,-107,51,-107,56), nrow = 16, byrow = TRUE), dim = "XY")
points <- st_cast(st_geometry(multipoints), "POINT") 

# Number of total linestrings to be created
n <- length(points)

# Build linestrings
linestrings <- lapply(X = seq(1,n, by = 2), FUN = function(x) {
  
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING")
  return(line)
  
})

# Alberta

m <- st_multilinestring(do.call("rbind", linestrings[1:2]))

m <- st_sfc(m, crs = 4326)
int <- st_intersection(df_aux_01$geometry[1],m)
buf <- st_buffer(int,dist = 0.000001)
# buf <- st_multilinestring(do.call("rbind", buf))
# buf <- st_sfc(buf, crs = 4326)
# buf <- st_cast(buf, "MULTI")
diff <- st_difference(df_aux_01$geometry[1],buf)
diff <- st_cast(diff, "POLYGON")

df_aux_91$geometry[1] <- diff[2]
df_aux_91$geometry[2] <- diff[1]
df_aux_91$geometry[3] <- diff[3]

# Assiniboia East

m <- st_multilinestring(do.call("rbind", linestrings[3]))

m <- st_sfc(m, crs = 4326)
int <- st_intersection(df_aux_01$geometry[2],m)
buf <- st_buffer(int,dist = 0.000001)
diff <- st_difference(df_aux_01$geometry[2],buf)
diff <- st_cast(diff, "POLYGON")

df_aux_91$geometry[4] <- diff[2]
df_aux_91$geometry[5] <- diff[1]

# Assiniboia West

m <- st_multilinestring(do.call("rbind", linestrings[4:6]))

m <- st_sfc(m, crs = 4326)
int <- st_intersection(df_aux_01$geometry[3],m)
buf <- st_buffer(int,dist = 0.000001)
diff <- st_difference(df_aux_01$geometry[3],buf)
diff <- st_cast(diff, "POLYGON")

df_aux_91$geometry[6] <- diff[2]
df_aux_91$geometry[7] <- diff[1]
df_aux_91$geometry[8] <- diff[4]
df_aux_91$geometry[9] <- diff[3]

# Saskatchewan

m <- st_multilinestring(do.call("rbind", linestrings[7:8]))

m <- st_sfc(m, crs = 4326)
int <- st_intersection(df_aux_01$geometry[10],m)
buf <- st_buffer(int,dist = 0.000001)
diff <- st_difference(df_aux_01$geometry[10],buf)
diff <- st_cast(diff, "POLYGON")

df_aux_91$geometry[10] <- diff[1]
df_aux_91$geometry[11] <- diff[3]
df_aux_91$geometry[12] <- diff[2]

df_aux_91 <- st_transform(df_aux_91, crs = 32610)

df_91_cd <- df_91_cd %>% filter(!(province %in% c("Manitoba","North Western Territories")))
df_91_cd <- rbind(df_91_cd,df_aux_91)


# 1881: use 1901 districts

df_aux_81 <- df_aux_01
df_aux_81$population <- c(0,0,25515,4921,7900,10200,4196,0,12779,0,14279,0,7985)

df_aux_81 <- st_transform(df_aux_81, crs = 32610)

df_81_cd <- df_81_cd %>% filter(!(province %in% c("Manitoba","North Western Territories")))
df_81_cd <- rbind(df_81_cd,df_aux_81)

#
rm(df_aux_01,df_aux_81,df_aux_91,m,diff,buf,int,n,points,multipoints,linestrings)




