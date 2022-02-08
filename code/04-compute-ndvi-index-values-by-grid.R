# Compute NDVI index values (bimonthly intervals + mar-apr-may) by PRFIP grid in CA from 1981 to 2019
# - Written by James Keeler, jbkeeler@ucdavis.edu
# - 2019

PRF_grids <- readRDS("data_intermediary/PRF_grids.RDS") %>%
  unique %>%
  st_as_sf %>% 
  st_transform("EPSG:4326")

# Extract mean daily NDVI by PRFIP grid from NOAA CDR raster
# mean is across pixels in grid
get_NDVI_by_Grid <- function(file) {
  
  message(paste0("Extracting from ",file))
  
  grids_ndvi <- NULL
  
  rr <- readRDS(paste0("data_intermediary/NOAA_CDR_NDVI/",file))
  
  grids_ndvi <- raster::extract(rr,PRF_grids,fun=mean) %>%
    as.data.frame %>%
    mutate(
      ndvi_grid_id = PRF_grids$grid,
      year = substr(file,33,36),
      month = substr(file,37,38),
      day = substr(file,39,40),
      source = "NOAA_CDR"
    )
  
  colnames(grids_ndvi)[1] <- "NDVI_measurement"
  
  # save hard copy so we can restart in place if necessary
  saveRDS(grids_ndvi,paste0("data_intermediary/NOAA_CDR_NDVI_processed/processed",file))
  
  return(grids_ndvi)
  
}

# Merge daily NDVI measurements by grid into a single data file
files <- list.files("data_intermediary/NOAA_CDR_NDVI/")


grids_ndvi <- sapply(files,get_NDVI_by_Grid,simplify=FALSE) %>%
  bind_rows %>%
  arrange(year,ndvi_grid_id)


# do.call(get_NDVI_by_Grid,list(file = files))
# if the processeing fails part way, use the saved files instead
grids_ndvi <- list.files("data_intermediary/NOAA_CDR_NDVI_processed/",full.names = TRUE) %>%
  map(readRDS) %>% 
  data.table::rbindlist() %>%
  arrange(year,ndvi_grid_id)


saveRDS(grids_ndvi,paste0("data_intermediary/PRF_NDVI_index_values_NOAA_CDR.RDS"))


# Compute NDVI bimonthly index interval values
yrs <- 1981:2019

intervals <- list(
  c("January","February"),
  c("February","March"),
  c("March","April"),
  c("April","May"),
  c("May","June"),
  c("June","July"),
  c("July","August"),
  c("August","September"),
  c("September","October"),
  c("October","November"),
  c("November","December")
)

interval_lookup <- data.frame(
  "yr" = rep(yrs,each=11), 
  "start_month" = rep(sapply(intervals, '[',1),times=length(yrs)),
  "end_month" = rep(sapply(intervals,  '[', 2),times=length(yrs))
) %>%
  mutate(
    start_date = as.Date(paste0(yr,start_month,1),"%Y%B%d"),
    end_date = rollback(as.Date(paste0(yr,end_month,1),"%Y%B%d") %m+% months(1)),
    start_yweek = week(start_date),
    end_yweek = week(end_date),
    index_interval = interval(start_date,end_date)
  )



getIndexIntervalsNDVI <- function(month){
  index_intervals <- sapply(intervals, function(y) if(month %in% y) return(paste(y[1],y[2],sep="-")), simplify = FALSE)
  return(unlist(index_intervals))
}


grids_ndvi <- readRDS("data_intermediary/PRF_NDVI_index_values_NOAA_CDR.RDS") %>%
  unique %>%
  mutate(
    month_num = month,
    month = month.name[as.integer(month_num)]
  ) %>%
  dplyr::select(ndvi_grid_id,year,month,month_num,day,source,NDVI_measurement) %>%
  arrange(ndvi_grid_id,year,month_num) %>%
  group_by(ndvi_grid_id,year,month_num) %>%
  distinct() %>%
  ungroup %>%
  mutate(interval = sapply(month, getIndexIntervalsNDVI, simplify=FALSE)) %>%
  unnest(interval) %>%
  ungroup %>%
  unnest(interval) %>%
  dplyr::select(-month) %>%
  group_by(ndvi_grid_id,year,interval) %>%
  summarize(NDVI_measurement = mean(NDVI_measurement, na.rm=TRUE)) %>%
  ungroup %>%
  mutate(NDVI_measurement_NOAA_CDR = NDVI_measurement) %>%
  arrange(ndvi_grid_id,year,interval)

saveRDS(grids_ndvi,paste0("data_intermediary/PRF_NDVI_index_values_NOAA_CDR_agg.RDS"))


# Compute NDVI index value for March, April, May interval
grids_ndvi_mar_apr_may <- readRDS("data_intermediary/PRF_NDVI_index_values_NOAA_CDR.RDS") %>%
  unique %>%
  mutate(
    month_num = month,
    month = month.name[as.integer(month_num)]
  ) %>%
  dplyr::select(ndvi_grid_id,year,month,month_num,day,NDVI_measurement) %>%
  arrange(ndvi_grid_id,year,month_num) %>%
  filter(month %in% c("March","April","May")) %>%
  group_by(ndvi_grid_id,year) %>%
  summarize(NDVI_measurement_march_april_may_NOAA_CDR = mean(NDVI_measurement, na.rm=TRUE)) %>%
  ungroup %>%
  mutate(
    grid = as.integer(ndvi_grid_id),
    year = as.integer(year),
    NDVI_measurement_march_april_may_NOAA_CDR = ifelse(is.nan(NDVI_measurement_march_april_may_NOAA_CDR),NA,NDVI_measurement_march_april_may_NOAA_CDR)
  ) %>%
  group_by(grid) %>%
  arrange(grid,year) %>%
  mutate(
    NDVI_avg_march_april_may_measurement_NOAA_CDR = rollapply(lag(NDVI_measurement_march_april_may_NOAA_CDR,2), width = ifelse(seq_along(NDVI_measurement_march_april_may_NOAA_CDR)-2 > 0,seq_along(NDVI_measurement_march_april_may_NOAA_CDR)-2,1), FUN = mean, align = "right", partial = TRUE)
  ) %>%
  ungroup %>%
  mutate(
    NDVI_percent_of_normal_march_april_may_NOAA_CDR = NDVI_measurement_march_april_may_NOAA_CDR/NDVI_avg_march_april_may_measurement_NOAA_CDR,
    grid = as.integer(grid)
  ) %>%
  dplyr::select(grid,year,NDVI_measurement_march_april_may_NOAA_CDR,NDVI_avg_march_april_may_measurement_NOAA_CDR,NDVI_percent_of_normal_march_april_may_NOAA_CDR)


saveRDS(grids_ndvi_mar_apr_may,"data_intermediary/PRF_NDVI_index_values_mar_apr_may_NOAA_CDR.RDS")


rm(list = ls())