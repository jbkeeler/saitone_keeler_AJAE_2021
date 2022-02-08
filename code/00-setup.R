install.packages("pacman")

library(pacman)


# some of these might not be used anymore or are already included in the tidyverse package
pacman::p_load(
  devtools,sf, rgdal, rgeos, tidyverse, raster, ncdf4, jsonlite, geojsonsf, lubridate,
  httr, rvest, XML, readr, transformr, zoo, viridis, rasterVis, ggthemes, knitr, kableExtra,
  RColorBrewer, combinat
)



rm(list = ls())
