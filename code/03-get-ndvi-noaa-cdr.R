# Download daily NOAA CDR NDVI raster in CA from 1981 to 2019
# - Written by James Keeler, jbkeeler@ucdavis.edu
# - 2019

# Transform CRS of grid, county, and state geometry to match NOAA CDR raster data
ca_counties <- readRDS("data_intermediary/ca_counties.RDS") %>%
	unique %>%
	st_as_sf %>%
	st_transform("EPSG:4326")

PRF_grids <- readRDS("data_intermediary/PRF_grids.RDS") %>%
  unique %>%
  st_as_sf %>% 
  st_transform("EPSG:4326")

ca_state <- readRDS("data_intermediary/ca_state.RDS") %>%
  st_as_sf %>% 
  st_transform("EPSG:4326")


years <- 1981:2019


get_noaa_ndvi_daily <- function(f,year) {

		message(paste0("Downloading ", f))

		noaa_file_url <- paste0("https://www.ncei.noaa.gov/data/avhrr-land-normalized-difference-vegetation-index/access/",year,"/",f)

		file_path <- paste0("data_intermediary/NOAA_CDR_NDVI/",f)

	    download.file(
	    	url=noaa_file_url,
	        destfile=file_path, 
	        method="curl"
	    )

		noaa_raster <- raster(file_path,varname = "NDVI")
		
		crs(noaa_raster) <- "EPSG:4326"

		rb <- crop(noaa_raster,PRF_grids)
		rr <- mask(rb,PRF_grids)

		saveRDS(rr,paste0("data_intermediary/NOAA_CDR_NDVI/",gsub("-preliminary", "", substr(f,1,nchar(f)-3)),".RDS"))

    file.remove(file_path)

}

# Ignore the "Error in CRS(x): NA" message. Changes to the various spatial 
# packages (depend on PROJ) broke backwards compatibility with some files, so we 
# have to manually add the crs.
for(year in years){
	
	noaa_url <- paste0("https://www.ncei.noaa.gov/data/avhrr-land-normalized-difference-vegetation-index/access/",year)

	r <- GET(noaa_url)

	ftp_names <- readHTMLTable(content(r,"text"))[["NULL"]]$Name
	filenames <- ftp_names[4:length(ftp_names)-1]

	for(f in filenames){

		try(get_noaa_ndvi_daily(f,year))
    #unlink(directory,recursive = TRUE)

	}
	
}

rm(list = ls())