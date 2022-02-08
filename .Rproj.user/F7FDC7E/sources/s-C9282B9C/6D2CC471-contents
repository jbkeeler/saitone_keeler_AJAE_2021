# Compile main data sets for analysis
# - Written by James Keeler, jbkeeler@ucdavis.edu
# - 2019


ca_counties <- readRDS("data_intermediary/ca_counties.RDS")

ca_state <- readRDS("data_intermediary/ca_state.RDS") 

PRF_grids <- readRDS("data_intermediary/PRF_grids.RDS")

PRF_grids_counties <- readRDS("data_intermediary/PRF_grids_counties.RDS")

veg <- readRDS("data_intermediary/PRF_cal_fraps_veg.RDS")

elev <- readRDS("data_intermediary/PRF_NED_elevation.RDS")


county_base_values <- readRDS("data_intermediary/PRF_grid_county_base_values.RDS")

premium_rates <- readRDS("data_intermediary/PRF_grid_county_premium_rates.RDS")


rainfall_interval_measurements <- readRDS("data_intermediary/PRF_rainfall_interval_measurements.RDS")

NDVI_interval_measurements <- readRDS("data_intermediary/PRF_NDVI_index_values_NOAA_CDR_agg.RDS") %>%
	mutate(source = "NOAA CDR") %>%
	dplyr::select(-NDVI_measurement_NOAA_CDR)


NDVI_index_mar_apr_may <- readRDS("data_intermediary/PRF_NDVI_index_values_mar_apr_may_NOAA_CDR.RDS") %>%
	mutate(
		grid = as.integer(grid),
		year = as.integer(year),
		NDVI_measurement_march_april_may = NDVI_measurement_march_april_may_NOAA_CDR,
		NDVI_avg_march_april_may_measurement = NDVI_avg_march_april_may_measurement_NOAA_CDR,
		NDVI_percent_of_normal_march_april_may = NDVI_percent_of_normal_march_april_may_NOAA_CDR,
		source = "NOAA CDR"
	) %>%
	dplyr::select(-NDVI_measurement_march_april_may_NOAA_CDR,-NDVI_avg_march_april_may_measurement_NOAA_CDR,-NDVI_percent_of_normal_march_april_may_NOAA_CDR)

setIntervalCode <- function(interval){
  if(grepl("January-February", interval, ignore.case = TRUE)){
    return(625)
  } else if(grepl("February-March", interval, ignore.case = TRUE)){
    return(626)
  } else if(grepl("March-April", interval, ignore.case = TRUE)){
    return(627)
  } else if(grepl("April-May", interval, ignore.case = TRUE)){
    return(628)
  } else if(grepl("May-June", interval, ignore.case = TRUE)){
    return(629)
  } else if(grepl("June-July", interval, ignore.case = TRUE)){
    return(630)
  } else if(grepl("July-August", interval, ignore.case = TRUE)){
    return(631)
  } else if(grepl("Lagged August-September", interval, ignore.case = TRUE)){
    return(532)
  } else if(grepl("Lagged September-October", interval, ignore.case = TRUE)){
    return(533)
  } else if(grepl("Lagged October-November", interval, ignore.case = TRUE)){
    return(534)
  } else if(grepl("Lagged November-December", interval, ignore.case = TRUE)){
    return(535)
  } else if(grepl("August-September", interval, ignore.case = TRUE)){
    return(632)
  } else if(grepl("September-October", interval, ignore.case = TRUE)){
    return(633)
  } else if(grepl("October-November", interval, ignore.case = TRUE)){
    return(634)
  } else if(grepl("November-December", interval, ignore.case = TRUE)){
    return(635)
  } else {
    return(NA)
  }
}

# should probably move index value calculations into the get NDVI and get Rainfall R files
# 2021 Note: the index value calculations are redundant here because they were moved into script 04
# I've left them here anyways to keep everything documented.


intervals_to_lag <- c(634,635)

ndvi_index_values <- NDVI_interval_measurements %>%
	mutate(
		grid = as.integer(ndvi_grid_id),
		year = as.integer(year),
		interval_code = sapply(interval,setIntervalCode),
		NDVI_measurement = ifelse(is.nan(NDVI_measurement),NA,NDVI_measurement),
	) %>%
	dplyr::select(year,interval,interval_code,grid,NDVI_measurement,source) %>%
	group_by(source,grid,interval_code) %>%
	arrange(source,grid,interval_code,year) %>%
	mutate(
		avg_interval_measurement = rollapply(lag(NDVI_measurement,2), width = ifelse(seq_along(NDVI_measurement)-2 > 0,seq_along(NDVI_measurement)-2,1), FUN = mean, align = "right", partial = TRUE,na.rm=TRUE)
	) %>%
	ungroup %>%
	mutate(
		NDVI_percent_of_normal = NDVI_measurement/avg_interval_measurement
	) %>%
	dplyr::select(grid,year,interval,interval_code,NDVI_measurement,NDVI_percent_of_normal,source)

ndvi_index_values_lagged <- ndvi_index_values %>%
	filter(interval_code %in% intervals_to_lag) %>%
  	group_by(source,grid,interval_code) %>%
  	arrange(source,grid,interval_code) %>%
  	mutate(
    	NDVI_percent_of_normal = dplyr::lag(NDVI_percent_of_normal)
    ) %>%
  	ungroup %>%
  	mutate(
    	interval_code = interval_code-100,
    	interval = paste0("Lagged ",interval)
    )

ndvi_index_values <- bind_rows(ndvi_index_values,ndvi_index_values_lagged)


rainfall_index_values <- rainfall_interval_measurements %>%
	#filter(year %in% 2003:2019) %>% # if we want to make indexes cover the same period
	dplyr::select(year,interval,interval_code,grid,percent_of_normal,RI_measurement,avg_interval_measurement) %>%
	filter(grid != 0) %>%
	group_by(grid,interval_code) %>%
	arrange(grid,interval_code,year) %>%
	mutate(avg_interval_measurement = rollapply(lag(RI_measurement,2), width = ifelse(seq_along(RI_measurement)-2 > 0,seq_along(RI_measurement)-2,1), FUN = mean, align = "right", partial = TRUE, na.rm = TRUE)) %>%
	ungroup %>%
	mutate(RI_percent_of_normal = RI_measurement/avg_interval_measurement) %>%
	dplyr::select(grid,year,interval,interval_code,RI_measurement,RI_percent_of_normal) 

rainfall_index_values_lagged <- rainfall_index_values %>%
	filter(interval_code %in% intervals_to_lag) %>%
  	group_by(grid,interval_code) %>%
  	arrange(grid,interval_code) %>%
  	mutate(
    	RI_percent_of_normal = dplyr::lag(RI_percent_of_normal),
    ) %>%
  	ungroup %>%
  	mutate(
    	interval_code = interval_code-100,
    	interval = paste0("Lagged ",interval)
    )

rainfall_index_values <- bind_rows(rainfall_index_values,rainfall_index_values_lagged)


rainfall_ndvi <- right_join(rainfall_index_values,ndvi_index_values,by=c("grid","year","interval","interval_code")) %>%
	filter(year > 1980)

saveRDS(rainfall_ndvi,"data_intermediary/PRF_RI_NDVI_values.RDS")
write.csv(rainfall_ndvi,"data_intermediary/PRF_RI_NDVI_values.csv", na=".")


coverage_levels <- list(seq(0.70,0.90,by=0.05))


rangeland_RI_NDVI <- rainfall_ndvi %>%
  filter(source == "NOAA CDR") %>%
	dplyr::select(grid,year,interval,interval_code,RI_measurement,RI_percent_of_normal,NDVI_measurement,NDVI_percent_of_normal,source) %>%
	unique %>%
	left_join(NDVI_index_mar_apr_may, by = c("source","grid","year")) %>%
	left_join(PRF_grids, by = c("grid")) %>%
	left_join(veg, by = c("grid")) %>%
	left_join(elev, by = c("grid")) %>%
	mutate(coverage_level = coverage_levels) %>% 
	unnest(coverage_level) %>%
	mutate(
		coverage_level = as.numeric(as.character(coverage_level)),
		RI_payoff = ifelse(RI_percent_of_normal < coverage_level,1,0),
		NDVI_payoff = ifelse(NDVI_percent_of_normal < coverage_level,1,0),
		false_negative_payoff = ifelse(RI_payoff == 0 & NDVI_payoff == 1,1,0),
		false_positive_payoff = ifelse(RI_payoff == 1 & NDVI_payoff == 0,1,0)
	) %>%
	group_by(source,grid) %>%
	mutate(
		grid_obs = n(),
	) %>%
	ungroup %>%
	as.data.frame %>%
	dplyr::select(-geometry)


NDVI_april_may <- filter(rangeland_RI_NDVI,interval == "April-May") %>%
	mutate(
		NDVI_percent_of_normal_april_may = NDVI_percent_of_normal,
	) %>%
	dplyr::select(grid,year,coverage_level,NDVI_percent_of_normal_april_may,source)

rangeland_RI_NDVI <- left_join(rangeland_RI_NDVI,NDVI_april_may,by = c("source","grid","year","coverage_level"))


saveRDS(rangeland_RI_NDVI,"data_final/rangeland_RI_NDVI_NOAA_CDR.RDS")
write.csv(rangeland_RI_NDVI, "data_final/rangeland_RI_NDVI_NOAA_CDR.csv", na=".")	


rangeland_RI_NDVI <- readRDS("data_final/rangeland_RI_NDVI_NOAA_CDR.RDS")


aib <- right_join(county_base_values,premium_rates,by= c("grid","county_code","year","interval_code","interval"))


rangeland_RI_NDVI_aib <- rangeland_RI_NDVI %>%
	right_join(PRF_grids_counties,by=c("grid")) %>%
	left_join(aib, by = c("year","grid","county_code","interval","interval_code","coverage_level"))  %>%
	as.data.frame %>%
	dplyr::select(-geometry)


subsidy_lookup <- data.frame(
		coverage_level=seq(0.70,0.90,by=0.05), 
		subsidy_factor=c(0.590,0.590,0.550,0.550,0.510),
		stringsAsFactors = FALSE
	) %>%
	mutate(
		coverage_level_char = paste0(100*coverage_level, "% Coverage Level"),
		coverage_level = as.numeric(as.character(coverage_level))
	) %>%
  dplyr::select(coverage_level,coverage_level_char,subsidy_factor)

 
rangeland_RI_NDVI_benefits <- rangeland_RI_NDVI_aib %>%
	left_join(subsidy_lookup, by = c("coverage_level")) %>%
	mutate(
		NDVI_payoff_april_may = ifelse(NDVI_percent_of_normal_april_may < coverage_level,1,0),
		NDVI_payoff_march_april_may = ifelse(NDVI_percent_of_normal_march_april_may < coverage_level,1,0),
		false_negative_payoff_april_may = ifelse(RI_payoff == 0 & NDVI_payoff_april_may == 1,1,0),
		false_negative_payoff_march_april_may = ifelse(RI_payoff == 0 & NDVI_payoff_march_april_may == 1,1,0),
		false_positive_payoff_april_may = ifelse(RI_payoff == 1 & NDVI_payoff_april_may == 0,1,0),
		false_positive_payoff_march_april_may = ifelse(RI_payoff == 1 & NDVI_payoff_march_april_may == 0,1,0),
		productivity_factor = 1.0,
        acreage = 1,
        insured_interest = 1.0,
        index_percent = 0.50,
        protection_per_acre = county_base_value*coverage_level*productivity_factor,
        total_protection = protection_per_acre*acreage*index_percent*insured_interest,
        RI_indemnity_factor = ifelse(RI_payoff == 1, (coverage_level-RI_percent_of_normal)/coverage_level,0),
        RI_indemnity = RI_indemnity_factor*total_protection,
        NDVI_indemnity_factor = ifelse(NDVI_payoff == 1, (coverage_level-NDVI_percent_of_normal)/coverage_level,0),
        NDVI_indemnity = NDVI_indemnity_factor*total_protection,
        NDVI_indemnity_factor_april_may = ifelse(NDVI_payoff_april_may == 1, (coverage_level-NDVI_percent_of_normal_april_may)/coverage_level,0),
        NDVI_indemnity_april_may = NDVI_indemnity_factor_april_may*total_protection,
        NDVI_indemnity_factor_march_april_may = ifelse(NDVI_payoff_march_april_may == 1, (coverage_level-NDVI_percent_of_normal_march_april_may)/coverage_level,0),
        NDVI_indemnity_march_april_may = NDVI_indemnity_factor_march_april_may*total_protection,
        premium = protection_per_acre*premium_rate*acreage*index_percent*insured_interest*(1-subsidy_factor),
	) %>%
	mutate_at(
		vars(
			RI_indemnity,NDVI_indemnity,NDVI_indemnity_april_may,NDVI_indemnity_march_april_may,
			premium,source
		), 
		~replace(., is.na(.), 0)
	) %>%
	mutate(
		RI_earnings = RI_indemnity - premium,
        NDVI_earnings = NDVI_indemnity - premium,
        NDVI_earnings_april_may = NDVI_indemnity_april_may - premium,
        NDVI_earnings_march_april_may = NDVI_indemnity_march_april_may - premium
	)


saveRDS(rangeland_RI_NDVI_benefits,"data_final/rangeland_RI_NDVI_benefits_NOAA_CDR.RDS")
write.csv(rangeland_RI_NDVI_benefits, "data_final/rangeland_RI_NDVI_benefits_NOAA_CDR.csv", na=".")	



# Generate contract combo data for revision

rangeland_RI_NDVI_NOAA_CDR <- readRDS("data_final/rangeland_RI_NDVI_NOAA_CDR.RDS") %>%
  mutate(
    NDVI_percent_of_normal = ifelse(is.nan(NDVI_percent_of_normal) | NDVI_percent_of_normal == -Inf,NA,NDVI_percent_of_normal)
  )

rangeland_RI_NDVI_benefits_NOAA_CDR <- readRDS("data_final/rangeland_RI_NDVI_benefits_NOAA_CDR.RDS") %>%
  mutate(
    NDVI_percent_of_normal = ifelse(is.nan(NDVI_percent_of_normal) | NDVI_percent_of_normal == -Inf,NA,NDVI_percent_of_normal)
  )


# relic of hard coded cbv, now just used to get list grids and their 2018 cbv
contract_quality_grids <- rangeland_RI_NDVI_benefits_NOAA_CDR %>%
  filter(
    year == 2018,
    interval_code >= 600,
    coverage_level == 0.85,
    rangeland == 1,
    !is.na(county_base_value),
    county_base_value != ".",
  ) %>%
  dplyr::select(
    grid,county_base_value
  ) %>%
  group_by(grid) %>%
  mutate(
    county_base_value = max(county_base_value)
  ) %>% 
  ungroup %>%
  unique %>%
  mutate(grid = as.integer(grid))

saveRDS(contract_quality_grids,"data_final/contract_quality_grids.RDS")
write.csv(contract_quality_grids, "data_final/contract_quality_grids.csv", na=".")


county_base_values <- unique(contract_quality_grids$county_base_value)


i <-c(
  "Lagged October-November",
  "Lagged November-December",
  "January-February",
  "February-March",
  "March-April",
  "April-May",
  "May-June"
)


combos <- list(
  list("January-February","February-March","March-April","April-May","May-June"),
  list("January-February","February-March","March-April","April-May","May-June"),
  list("Lagged October-November","Lagged November-December","March-April","April-May","May-June"),
  list("Lagged October-November","Lagged November-December","April-May","May-June"),
  list("Lagged October-November","Lagged November-December","January-February","May-June"),
  list("Lagged October-November","Lagged November-December","January-February","February-March"),
  list("Lagged October-November","Lagged November-December","January-February","February-March","March-April")
)

# after adding the slice, this is pretty much the same as the automatic approach
interval_combos <- data.frame(interval = i, combo = I(combos)) %>%
  dplyr::select(interval,combo) %>%
  unnest(combo) %>%
  mutate(
    interval_combo = paste0(interval,", ", combo),
    left = interval,
    right = as.character(combo)
  ) %>%
  dplyr::select(left,right,interval_combo) %>%
  slice(1:10,13:15,18,19,23) %>%
  pivot_longer(-interval_combo,names_to = "direction", values_to = "interval")

contracts <- interval_combos %>%
  dplyr::select(interval_combo) %>%
  unique

saveRDS(contracts,"data_final/contract_combinations.RDS")
write.csv(contracts, "data_final/contract_combinations.csv", na=".")	


grid_combo_contract_values  <- rangeland_RI_NDVI_NOAA_CDR %>%
  filter(
    grid %in% contract_quality_grids$grid,
    interval_code %in% c(534,535,625,626,627,628,629),
    rangeland == 1
  ) %>%
  dplyr::select(
    grid,elev_leq_1524,year,interval,interval_code,RI_percent_of_normal,
    NDVI_percent_of_normal,NDVI_percent_of_normal_march_april_may,NDVI_percent_of_normal_april_may
  ) %>%
  arrange(interval_code) %>%
  unique %>%
  left_join(interval_combos, by = c("interval")) %>%
  left_join(contract_quality_grids, by = c("grid")) %>%
  dplyr::select(
    grid,elev_leq_1524,year,county_base_value,interval,interval_code,interval_combo,direction,RI_percent_of_normal,
    NDVI_percent_of_normal,NDVI_percent_of_normal_march_april_may,NDVI_percent_of_normal_april_may
  )

saveRDS(grid_combo_contract_values,"data_final/grid_combo_contracts_RI_NDVI.RDS")
write.csv(grid_combo_contract_values, "data_final/grid_combo_contracts_RI_NDVI.csv", na=".")	


combo_grid_year_contract_analysis <- grid_combo_contract_values %>%
  mutate(
    coverage_level = 0.85,
    acreage = 1,
    productivity_factor = 1,
    percent_of_value = 0.5, # ifelse(direction == "left",0.6,0.4)
    NDVI_loss = ifelse(NDVI_percent_of_normal < 1,1,0),
    NDVI_no_loss = ifelse(NDVI_percent_of_normal >= 1,1,0),
    actual_dollar_loss = ifelse(NDVI_percent_of_normal >= 1.0,0,percent_of_value*(1-NDVI_percent_of_normal)*acreage*county_base_value),
    NDVI_march_april_may_loss = ifelse(NDVI_percent_of_normal_march_april_may < 1,1,0),
    NDV_march_april_may_no_loss = ifelse(NDVI_percent_of_normal_march_april_may >= 1,1,0),
    actual_dollar_loss_march_april_may = ifelse(NDVI_percent_of_normal_march_april_may >= 1.0,0,percent_of_value*(1-NDVI_percent_of_normal_march_april_may)*acreage*county_base_value),
    rain_loss = ifelse(RI_percent_of_normal < coverage_level,1,0),
    rain_no_loss = ifelse(RI_percent_of_normal >= coverage_level,1,0),
    rain_dollar_paid = ifelse(rain_loss == 0,0,percent_of_value*((coverage_level - RI_percent_of_normal)/coverage_level)*acreage*county_base_value),
    false_negative_march_april_may = ifelse(NDVI_percent_of_normal_march_april_may < coverage_level & RI_percent_of_normal >= coverage_level,1,0),
    NDVI_payoff = ifelse(NDVI_percent_of_normal < coverage_level,1,0),
    NDVI_no_payoff = ifelse(NDVI_percent_of_normal >= coverage_level,1,0),
    NDVI_march_april_may_payoff = ifelse(NDVI_percent_of_normal_march_april_may < coverage_level,1,0),
    NDV_march_april_may_no_payoff = ifelse(NDVI_percent_of_normal_march_april_may >= coverage_level,1,0)
  ) %>%
  group_by(grid,year,county_base_value,interval_combo,elev_leq_1524) %>%
  summarize(
    NDVI_percent_of_normal = sum(percent_of_value*NDVI_percent_of_normal,na.rm=TRUE),
    NDVI_percent_of_normal_march_april_may= sum(percent_of_value*NDVI_percent_of_normal_march_april_may,na.rm=TRUE),
    RI_percent_of_normal = sum(percent_of_value*RI_percent_of_normal,na.rm=TRUE),
    NDVI_loss = ifelse(sum(NDVI_loss,na.rm=TRUE) > 0,1,0),
    NDVI_loss_march_april_may = ifelse(sum(NDVI_march_april_may_loss,na.rm=TRUE) > 0 ,1,0),
    NDVI_march_april_may_payoff = first(NDVI_march_april_may_payoff), # check this, assumes that this is the same every year, which should be the case
    rain_loss = ifelse(sum(rain_loss,na.rm=TRUE) > 0,1,0),
    rain_dollar_paid = sum(rain_dollar_paid,na.rm = TRUE),
    actual_dollar_loss = sum(actual_dollar_loss,na.rm = TRUE),
    actual_dollar_loss_march_april_may = sum(actual_dollar_loss_march_april_may,na.rm = TRUE),
    false_negative_march_april_may_single = ifelse(sum(false_negative_march_april_may,na.rm=TRUE) == 1,1,0),
    false_negative_march_april_may_both = ifelse(sum(false_negative_march_april_may,na.rm=TRUE) == 2,1,0),
    under = ifelse(NDVI_loss > 0 & rain_loss > 0 & rain_dollar_paid < actual_dollar_loss,1,0),
    under_march_april_may = ifelse(NDVI_loss_march_april_may > 0 & rain_loss > 0 & rain_dollar_paid < actual_dollar_loss_march_april_may,1,0),
    underpay = ifelse(under == 1, rain_dollar_paid-actual_dollar_loss,0),
    underpay_march_april_may = ifelse(under_march_april_may == 1, rain_dollar_paid-actual_dollar_loss_march_april_may,0),
    over = ifelse(NDVI_loss > 0 & rain_loss > 0 & rain_dollar_paid >= actual_dollar_loss,1,0),
    over_march_april_may = ifelse(NDVI_loss_march_april_may > 0 & rain_loss > 0 & rain_dollar_paid >= actual_dollar_loss_march_april_may,1,0),
    overpay = ifelse(over == 1, rain_dollar_paid-actual_dollar_loss,0),
    overpay_march_april_may = ifelse(over_march_april_may == 1, rain_dollar_paid-actual_dollar_loss_march_april_may,0),
    insurance_outcome = (rain_dollar_paid-actual_dollar_loss),
    insurance_outcome_march_april_may = (rain_dollar_paid-actual_dollar_loss_march_april_may)
  ) %>%
  ungroup


combo_contract_analysis <- combo_grid_year_contract_analysis %>%
  filter(year %in% 2011:2019 & elev_leq_1524 == 1) %>%
  group_by(interval_combo,county_base_value) %>%
  summarize(
    obs = n(),
    NDVI_loss_n = sum(NDVI_loss,na.rm = TRUE),
    # NDVI_no_loss_n = sum(NDVI_no_loss,na.rm = TRUE),
    NDVI_loss_march_april_may_n = sum(NDVI_loss_march_april_may,na.rm = TRUE),
    single_n = sum(false_negative_march_april_may_single,na.rm=TRUE),
    both_n = sum(false_negative_march_april_may_both,na.rm=TRUE),
    false_negative_probability_march_april_may_single = (sum(false_negative_march_april_may_single,na.rm=TRUE)/obs)/((sum(NDVI_march_april_may_payoff,na.rm=TRUE))/obs),
    false_negative_probability_march_april_may_both = (sum(false_negative_march_april_may_both,na.rm=TRUE)/obs)/((sum(NDVI_march_april_may_payoff,na.rm=TRUE))/obs),
    # RI_indemnity_avg = mean(rain_dollar_paid,na.rm = TRUE),
    underpayment_n = sum(under,na.rm = TRUE),
    underpayment_avg = mean(underpay,na.rm = TRUE),
    underpayment_march_april_may_n = sum(under_march_april_may,na.rm = TRUE),
    underpayment_march_april_may_avg = mean(underpay_march_april_may,na.rm = TRUE),
    overpayment_n = sum(over,na.rm = TRUE),
    overpayment_avg = mean(overpay,na.rm = TRUE),
    overpayment_march_april_may_n = sum(over_march_april_may,na.rm = TRUE),
    overpayment_march_april_may_avg = mean(overpay_march_april_may,na.rm = TRUE),
    years = "2011-2019"
  ) %>%
  ungroup

saveRDS(combo_grid_year_contract_analysis,"data_final/combo_analysis_grid_year_contract.RDS")
write.csv(combo_grid_year_contract_analysis, "data_final/combo_analysis_grid_year_contract.csv", na=".")	

saveRDS(combo_contract_analysis,"data_final/combo_analysis_contract.RDS")
write.csv(combo_contract_analysis, "data_final/combo_analysis_contract.csv", na=".")	


grid_combo_cor <- grid_combo_contract_values %>%
  dplyr::select(grid,year,interval,interval_code,interval_combo,RI_percent_of_normal,NDVI_percent_of_normal_march_april_may,direction) %>%
  pivot_wider(names_from = direction, values_from = c("interval","interval_code","RI_percent_of_normal")) %>%
  filter(!is.na(NDVI_percent_of_normal_march_april_may)) %>%
  group_by(grid,interval_combo) %>%
  summarize(
    RI_NDVI_R2 = summary(lm(NDVI_percent_of_normal_march_april_may ~ RI_percent_of_normal_left + RI_percent_of_normal_right))$r.squared
  ) %>%
  ungroup %>%
  group_by(grid) %>%
  mutate(
    max_R2 = max(RI_NDVI_R2),
    optimal = ifelse(RI_NDVI_R2 == max_R2,TRUE,FALSE)
  ) %>%
  ungroup


saveRDS(grid_combo_cor,"data_final/grid_interval_combo_R2.RDS")
write.csv(grid_combo_cor, "data_final/grid_interval_combo_R2.csv", na=".")


rm(list = ls())
