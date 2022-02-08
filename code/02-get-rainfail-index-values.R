# Compute PRFIP rainfall index values by grid in CA from 1948 to 2019
# - Written by James Keeler, jbkeeler@ucdavis.edu
# - 2019

ca_counties <- readRDS("data_intermediary/ca_counties.RDS")
PRF_grids <- readRDS("data_intermediary/PRF_grids.RDS")

baseurl <- "https://prodwebnlb.rma.usda.gov/apps/PrfWebApi/PrfExternalIndexes/GetIndexValues"

# look-up table for PRFIP index intervals
PRF_RI_index_codes <- data.frame(
	interval_code = 625:635,
	month1 = c("January","February","March","April","May","June","July","August","September","October","November"),
	month2 = c("February","March","April","May","June","July","August","September","October","November","December")
)

PRF_RI_index_values <- list()

# For each grid, scrape the 1948 to 2019 index values
i <- 1
for(grid_id in PRF_grids %>% pull(grid)){
	message(paste0("Querying ",baseurl, "?intervalType=BiMonthly&sampleYearMinimum=1948&sampleYearMaximum=2019&gridId=", grid_id))
	PRF_RI_index_values[[i]] <- bind_rows(fromJSON(URLencode(paste0(baseurl, "?intervalType=BiMonthly&sampleYearMinimum=1948&sampleYearMaximum=2019&gridId=", grid_id)))[["HistoricalIndexRows"]]$HistoricalIndexDataColumns)
	i <- i + 1
}

# Merge and clean rainfall index values data
PRF_RI_index_values <- bind_rows(PRF_RI_index_values) %>%
	mutate(interval_code = as.numeric(IntervalCode)) %>%
	left_join(PRF_RI_index_codes,by="interval_code") %>%
  rename(
    grid = GridId,
    year = Year,
    percent_of_normal = PercentOfNormal,
    RI_measurement = IntervalMeasurement,
    avg_interval_measurement = AverageIntervalMeasurement
  ) %>%
	mutate(
	    interval = paste0(month1,"-",month2),
	    interval_code = as.numeric(IntervalCode)
	)

saveRDS(PRF_RI_index_values,"data_intermediary/PRF_rainfall_interval_measurements.RDS")

rm(list = ls())
