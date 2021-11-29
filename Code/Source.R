if (!("zoo" %in% rownames(installed.packages()))){
        install.packages("zoo")
}

if (!("writexl" %in% rownames(installed.packages()))){
  install.packages("writexl")
  
}

if (!("glue" %in% rownames(installed.packages()))){
  install.packages("glue")
  
}

if (!("dplyr" %in% rownames(installed.packages()))){
  install.packages("dplyr")
  
}

if (!("tidyverse" %in% rownames(installed.packages()))){
          install.packages("tidyverse")
          
}
if (!("ggthemes" %in% rownames(installed.packages()))){
          install.packages("ggthemes")
          
}
if (!("ggpubr" %in% rownames(installed.packages()))){
          install.packages("ggpubr")
          
}
if (!("xlsx" %in% rownames(installed.packages()))){
  install.packages("xlsx")
  
  
}

if (!("stringdist" %in% rownames(installed.packages()))){
  install.packages("stringdist")
  
}


if (!("stringi" %in% rownames(installed.packages()))){
  install.packages("stringi")
  
}
if (!("sf" %in% rownames(installed.packages()))){
  install.packages("sf")
  
}

# if (!("mapview" %in% rownames(installed.packages()))){
#   install.packages("mapview")
#   
# }


if (!("maps" %in% rownames(installed.packages()))){
  install.packages("maps")
  
}


if (!("rgdal" %in% rownames(installed.packages()))){
  install.packages("rgdal")
  
}


if (!("RCzechia" %in% rownames(installed.packages()))){
  install.packages("RCzechia")
  
}


if (!("mapproj" %in% rownames(installed.packages()))){
  install.packages("mapproj")
  
}

if (!("dat" %in% rownames(installed.packages()))){
  install.packages("dat")
  
}
if (!("lwgeom" %in% rownames(installed.packages()))){
  install.packages("lwgeom")
  
}

if (!("reshape2" %in% rownames(installed.packages()))){
  install.packages("reshape2")
  
}

if (!("ggmap" %in% rownames(installed.packages()))){
  # install.packages("ggmap", dependencies = TRUE, INSTALL_opts = '--no-lock')
  install.packages("ggmap")

}
library(ggmap)

if (!("plm" %in% rownames(installed.packages()))){
  # install.packages("ggmap", dependencies = TRUE, INSTALL_opts = '--no-lock')
  install.packages("plm")
  
}
library(plm)



if (!("Hmisc" %in% rownames(installed.packages()))){
  # install.packages("ggmap", dependencies = TRUE, INSTALL_opts = '--no-lock')
  install.packages("Hmisc")
}


if (!("sfheaders" %in% rownames(installed.packages()))){
  # install.packages("ggmap", dependencies = TRUE, INSTALL_opts = '--no-lock')
  install.packages("sfheaders")
}

if (!("magrittr" %in% rownames(installed.packages()))){
  # install.packages("ggmap", dependencies = TRUE, INSTALL_opts = '--no-lock')
  install.packages("magrittr")
}
if (!("demography" %in% rownames(installed.packages()))){
  # install.packages("ggmap", dependencies = TRUE, INSTALL_opts = '--no-lock')
  install.packages("demography")
}

# install.packages("rmarkdown")
# install.packages(c("digest", "caTools", "bitops"))
install.packages("ggraph")

install.packages("nngeo")

install.packages("geojsonsf")


  
# install.packages("ggtext")  
# library(ggtext)
# install.packages("growthrates")
# library(growthrates)
# install.packages("gtools")  
# library(gtools)

install.packages("DSSAT")

# install.packages("rlang")

# install.packages("clubSandwich")


install.packages("igraph") 
install.packages("network") 
install.packages("sna")
install.packages("ggraph")
install.packages("visNetwork")
install.packages("threejs")
install.packages("networkD3")
install.packages("ndtv")

# ggnet
install.packages("GGally")
install.packages("intergraph")

# install.packages("knitr")

install.packages("geojsonio")
library(geojsonio)

# install.packages("geojsonsf")
# library(geojsonsf)



library(glue)
library(dplyr)
library(tidyverse)
library(readxl)
library(Hmisc)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(zoo)
library(RColorBrewer)
library(stringdist)
library(rJava)
library(xlsx)
library(sf)
# library(mapview)
library(maps)
# library(sjmisc)
library(rgdal)
library(stringi)
library(RCzechia)
library(mapproj)
library(dat)
library(lwgeom)
library(reshape2)
library(sfheaders)
library(DSSAT)
# library(rlang)
library(magrittr)
library(demography)
library(geojsonsf)

# library(clubSandwich)

# library(rmarkdown)
# library(digest)
# library(bitops)

library(igraph)
library(ggraph)
# ggnet
library(GGally)
library(intergraph)
library(nngeo)
# library(knitr)
