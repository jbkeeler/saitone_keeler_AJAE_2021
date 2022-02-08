# Classify PRFIP grids by 2018 vegetation cover from CAL FIRE FRAPS map.
# - Written by James Keeler, jbkeeler@ucdavis.edu
# - 2019

veg_raster <- raster("data_raw/calfire_landuse.tif")

PRF_grids <- readRDS("data_intermediary/PRF_grids.RDS") %>%
  	unique %>%
  	rownames_to_column(var = "ID") %>%
    mutate(ID = as.integer(ID)) %>%
  	st_as_sf %>%
  	st_transform(crs(veg_raster,asText=TRUE))


veg_raster <- veg_raster %>%
	crop(PRF_grids) %>%
	mask(PRF_grids)

raster_attribute_table <- fromJSON(
		"https://egis.fire.ca.gov/arcgis/rest/services/FRAP/fveg/ImageServer/rasterAttributeTable?f=pjson",
		simplifyDataFrame = TRUE,
		flatten = TRUE
	)$features

names(raster_attribute_table) <- gsub("attributes.","",names(raster_attribute_table))

raster_attribute_table <- raster_attribute_table %>%
	select(VALUE,WHRNAME)

grassland_veg <- c("Annual Grassland","Perennial Grassland")
woodland_veg <- c(
	"Juniper","Pinyon-Juniper","Blue Oak Woodland","Blue Oak-Foothill Pine","Coastal Oak Woodland",
	"Eucalyptus","Hardwood","Valley Foothill Riparian","Valley Oak Woodland","Desert Riparian",
	"Joshua Tree","Palm Oasis"
	)
shrub_veg <- c(
	"Alkali Desert Scrub","Desert Scrub","Desert Succulent Shrub","Desert Wash",
	"Bitterbrush","Chamise-Redshank Chaparral","Coastal Scrub","Low Sage",
	"Mixed Chaparra","Montane Chaparral","Sagebrush","Undetermined Shrub"
	)

PRF_grids_veg <- raster::extract(veg_raster,PRF_grids,df = TRUE,weights = TRUE, normalizeWeights = TRUE) %>%  #,sp = TRUE
		rename(VALUE = calfire_landuse) %>%
		mutate(area_sqm = 30*30) %>%
		left_join(raster_attribute_table,by=c("VALUE")) %>%
		group_by(ID,WHRNAME) %>%
		summarise(
			weight = sum(weight,na.rm = TRUE),
			area_sqm = sum(area_sqm)
		) %>%
		ungroup %>%
		group_by(ID) %>%
		mutate(
			common_veg = max(weight,na.rm = TRUE)
		) %>%
		ungroup %>%
		right_join(PRF_grids,by=c("ID")) %>%
		mutate(
			rangeland = ifelse(WHRNAME %in% c(grassland_veg,woodland_veg,shrub_veg,"Pasture") & !is.na(WHRNAME),1,0),
			grassland = ifelse(WHRNAME %in% grassland_veg & !is.na(WHRNAME),1,0),
			woodland = ifelse(WHRNAME %in% woodland_veg & !is.na(WHRNAME),1,0),
			shrub = ifelse(WHRNAME %in% shrub_veg & !is.na(WHRNAME),1,0),
			pasture = ifelse(WHRNAME == "Pasture" & !is.na(WHRNAME),1,0),
			veg_cat = ifelse(WHRNAME %in% grassland_veg & !is.na(WHRNAME),"grassland_rangeland",
				ifelse(WHRNAME %in% woodland_veg & !is.na(WHRNAME),"woodland_rangeland",
					ifelse(WHRNAME %in% shrub_veg & !is.na(WHRNAME),"shrubland_rangeland",
						ifelse(WHRNAME == "Pasture" & !is.na(WHRNAME),"pasture_rangeland", "non_rangeland"))))
		) %>%
		filter(weight == common_veg) %>%
		group_by(ID) %>%
		mutate(n_in_seq = 1:n()) %>%
		ungroup %>%
		filter(n_in_seq == 1) %>%
		rename(vegetation = WHRNAME) %>% 
		select(grid,vegetation,rangeland,grassland,woodland,shrub,pasture,veg_cat,area_sqm,geometry) %>%
		st_as_sf

saveRDS(select(as.data.frame(PRF_grids_veg),-geometry,-area_sqm),"data_intermediary/PRF_cal_fraps_veg.RDS")

rm(list = ls())


# tot_rangeland_acreage <- PRF_grids_veg %>%
# 	group_by(rangeland) %>%
# 	summarise(rangeland_acreage = sum(area_sqm/4047,na.rm = TRUE)) %>%
# 	ungroup %>%
# 	select(rangeland,rangeland_acreage)