# Get county, state, and PRF grid geometry for CA
# - Written by James Keeler, jbkeeler@ucdavis.edu
# - 2019

# Get CA county and state geometry from US Census Bureau tigerline shapefiles
ca_counties_sf <- read_sf('data_raw/US_States/tl_2018_us_county.shp') %>%
	filter(STATEFP == '06') %>%
	mutate(
		county_code = as.integer(COUNTYFP),
		state_code = as.integer(STATEFP)
	) %>%
	st_as_sf %>%
	st_transform(4326) 

saveRDS(ca_counties_sf,"data_intermediary/ca_counties.RDS")

ca_state_sf <- read_sf('data_raw/US_States/tl_2018_us_state.shp') %>%
  filter(STUSPS == 'CA') %>%
  st_as_sf

saveRDS(ca_state_sf,"data_intermediary/ca_state.RDS")


# Get PRF grid ids from USDA RMA's PRF Support Tool (grids for rainfall index)
query_grids <- function(cnty,index){
	baseurl <- "https://prodwebnlb.rma.usda.gov/apps/PrfWebApi/PrfExternalStates/GetSubCountiesByCountyAndState"
	ri_county_grids <- fromJSON(URLencode(paste0(baseurl, "?stateCode=",cnty$STATEFP,"&countyCode=", cnty$COUNTYFP)))
	return(list(state_code = cnty$STATEFP, county_code = cnty$COUNTYFP, grid= ri_county_grids))
}

PRF_grids <- apply(ca_counties_sf, 1, query_grids) %>%
	bind_rows %>%
	filter(lapply(grid,length) > 0) %>%
	unnest(grid) %>%
	group_by(grid) %>%
	summarize(counties = list(county_code),states=list(state_code)) %>%
	ungroup %>%
	select(grid,counties) %>%
	unique



# Get PRF grid geometry from USDA RMA's PRF Support Tool (grids for rainfall index), code using the agforce website for either RI or NDVI grids is available
url_north <- "https://prodwebnlb.rma.usda.gov/apps/prf/proxy/proxy.ashx?https://pdgis.rm.usda.net/arcgis/rest/services/ITM/PRF_Grid/MapServer/0/query?f=json&returnGeometry=true&spatialRel=esriSpatialRelIntersects&maxAllowableOffset=152&geometry={%22xmin%22:-124.49249,%22ymin%22:37.18379,%22xmax%22:-114.00055,%22ymax%22:42.02478,%22spatialReference%22:{%22wkid%22:4326}}&geometryType=esriGeometryEnvelope&inSR=4326&outFields=*&outSR=4326"
url_south <- "https://prodwebnlb.rma.usda.gov/apps/prf/proxy/proxy.ashx?https://pdgis.rm.usda.net/arcgis/rest/services/ITM/PRF_Grid/MapServer/0/query?f=json&returnGeometry=true&spatialRel=esriSpatialRelIntersects&maxAllowableOffset=152&geometry={%22xmin%22:-124.49249,%22ymin%22:32.34280,%22xmax%22:-114.00055,%22ymax%22:37.18379,%22spatialReference%22:{%22wkid%22:4326}}&geometryType=esriGeometryEnvelope&inSR=4326&outFields=*&outSR=4326"


grids_north <- fromJSON(URLencode(url_north))$features$attributes %>%
	rowwise %>%
	mutate(wkt = paste0("POLYGON ((",X_MIN," ",Y_MIN,", ",X_MIN," ",Y_MAX,", ",X_MAX," ",Y_MAX,", ",X_MAX," ",Y_MIN,", ",X_MIN," ",Y_MIN,"))")) %>%
	ungroup %>%
	dplyr::select(GRIDCODE,wkt) %>%
	filter(GRIDCODE %in% PRF_grids$grid)

grids_south <- fromJSON(URLencode(url_south))$features$attributes %>%
	rowwise %>%
	mutate(wkt = paste0("POLYGON ((",X_MIN," ",Y_MIN,", ",X_MIN," ",Y_MAX,", ",X_MAX," ",Y_MAX,", ",X_MAX," ",Y_MIN,", ",X_MIN," ",Y_MIN,"))")) %>%
	ungroup %>%
	dplyr::select(GRIDCODE,wkt) %>%
	filter(GRIDCODE %in% PRF_grids$grid)

PRF_grids_sf <- rbind(grids_north,grids_south) %>% 
	unique %>%
	st_as_sf(crs=4326,wkt = "wkt") %>%
	st_transform(st_crs(ca_state_sf))%>%
	st_intersection(ca_state_sf$geometry) %>%
	rename(
		grid = GRIDCODE,
		geometry = wkt
	) %>%
	mutate(
		grid_area = as.numeric(st_area(.))
	) %>%
	st_as_sf %>%
	st_transform(st_crs(ca_counties_sf))

saveRDS(PRF_grids_sf,"data_intermediary/PRF_grids.RDS")
st_write(dplyr::select(PRF_grids_sf,grid), "data_intermediary/PRF_grids/PRF_grids.shp")


# Intersect PRF grids with county geometry
PRF_grids_counties_sf <- st_intersection(st_as_sf(ca_counties_sf),PRF_grids_sf) %>%
	as.data.frame %>%
	select(grid,county_code,geometry) %>%
	mutate(county_code = as.integer(as.character(county_code)))

rownames(PRF_grids_counties_sf) <- NULL

PRF_grids_counties_sf <- PRF_grids_counties_sf %>%
	st_as_sf

saveRDS(PRF_grids_counties_sf,"data_intermediary/PRF_grids_counties.RDS")


rm(list = ls())
