# Classify PRFIP grids by Elevation
# - Written by James Keeler, jbkeeler@ucdavis.edu
# - 2019

ned_file_names <- unlist(read.table("data_raw/TNM_cartExport_20191231_220907.txt"))
ned_file_names <- gsub("https:\\/\\/prd-tnm\\.s3\\.amazonaws\\.com\\/StagedProducts\\/Elevation\\/1\\/ArcGrid\\/|USGS_NED_1_|_ArcGrid|\\.zip","",ned_file_names)
 

test_raster <- raster(paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/1/TIFF/",ned_file_names[1],"/USGS_1_",ned_file_names[1],".tif"))

PRF_grids <- readRDS("data_intermediary/PRF_grids.RDS") %>%
  rownames_to_column(var = "ID") %>%
  mutate(ID = as.integer(ID)) %>%
	st_as_sf %>%
	st_transform(crs(test_raster,asText=TRUE))

getUSGS_NED_elev <- function(file) {

	message(paste0("Extracting from ",file))

	elev_raster <- raster(paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/1/TIFF/",file,"/USGS_1_",file,".tif"))

	elev_raster <- elev_raster %>%
	crop(PRF_grids) %>%
	mask(PRF_grids)

	elev_grid <- raster::extract(elev_raster,veg_jepson, fun = mean, df = TRUE, sp = TRUE) %>%
		as.data.frame

	return(elev_grid)
}

file <- ned_file_names[1]

message(paste0("Extracting from ",file))

elev_raster <- raster(paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/1/TIFF/",file,"/USGS_1_",file,".tif"))

elev_raster <- elev_raster %>%
  crop(PRF_grids) %>%
  mask(PRF_grids)

elev_grid <- raster::extract(elev_raster,PRF_grids, fun = mean, df = TRUE, sp = TRUE) %>%
  as.data.frame



ca_elev <- sapply(ned_file_names,getUSGS_NED_elev,simplify=FALSE)

ca_elev[c("n43w120","n43w121","n34w121")] <- NULL   # drop empty DEMs

ca_elev_merge <- ca_elev %>% 
	bind_rows() %>%
	pivot_longer(11:78, names_to = "usgs_dem", values_to = "elevation") %>%
	select(grid,usgs_dem,elevation) %>%
	filter(!is.na(elevation)) %>%
	arrange(grid) %>%
	group_by(grid) %>%
	summarise(elevation = mean(elevation,na.rm = TRUE))


PRF_grids_elev <- left_join(PRF_grids,ca_elev_merge, by = "grid") %>%
	mutate(elev_leq_1524 = ifelse(elevation <= 1524,1,0)) %>%
	st_as_sf


saveRDS(select(as.data.frame(PRF_grids_elev),grid,elevation,elev_leq_1524),"data_intermediary/PRF_NED_elevation.RDS")

rm(list = ls())