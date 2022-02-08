# Generate unformatted tables for paper
# - Written by James Keeler, jbkeeler@ucdavis.edu
# - 2019
# Note: Most of the tables presented in the final paper were generated in stata

sob_cov <- readRDS("data_intermediary/sob_cov.RDS") %>%
	mutate(QuantityType = str_trim(QuantityType))

sob_tpu <- readRDS("data_intermediary/sob_tpu.RDS") %>%
	mutate(CommodityReportLevelType = str_trim(CommodityReportLevelType))

rangeland_RI_NDVI_NOAA_CDR <- readRDS("data_final/rangeland_RI_NDVI_NOAA_CDR.RDS") %>%
	filter(!is.na(interval)) %>%
	mutate(
		NDVI_percent_of_normal = ifelse(is.nan(NDVI_percent_of_normal) | NDVI_percent_of_normal == -Inf,NA,NDVI_percent_of_normal)
	)

rangeland_RI_NDVI_benefits_NOAA_CDR <- readRDS("data_final/rangeland_RI_NDVI_benefits_NOAA_CDR.RDS") %>%
	filter(!is.na(interval)) %>%
	mutate(
		NDVI_percent_of_normal = ifelse(is.nan(NDVI_percent_of_normal) | NDVI_percent_of_normal == -Inf,NA,NDVI_percent_of_normal)
	)



# Table A1 (Belasco and Hungerford Table 2 replicate)
belasco_table_2_ca <- sob_cov %>%
	filter(InsurancePlanCode %in% c(13,14) & !(CommodityCode %in% c(1191,332)) & QuantityType == "Acres" & CommodityYear == 2019 & StateAbbreviation == "CA") %>%
	filter(CoverageLevel %in% c(0.70,0.75,0.80,0.85,0.90)) %>%
	group_by(CoverageLevel) %>%
	summarise(
		insured_acreage_mil = sum(NetReportedQuantity, na.rm = TRUE)/1000000,
		avg_acreage_per_policy = mean(sum(NetReportedQuantity,na.rm=TRUE)/sum(PoliciesSoldCount,na.rm = TRUE),na.rm = TRUE),
		#subsidy_amount = mean(SubsidyAmount,na.rm = TRUE), # get from AIB code/data
		loss_ratio = sum(IndemnityAmount,na.rm = TRUE)/sum(TotalPremiumAmount,na.rm = TRUE),
		farmer_loss_ratio = sum(IndemnityAmount,na.rm = TRUE)/(sum(TotalPremiumAmount-SubsidyAmount,na.rm = TRUE))
		) %>%
	ungroup %>%
	mutate(CoverageLevel = as.character(CoverageLevel))

belasco_table_2_all_ca <- sob_cov %>%
	filter(InsurancePlanCode %in% c(13,14) & !(CommodityCode %in% c(1191,332)) & QuantityType == "Acres" & CommodityYear == 2019 & StateAbbreviation == "CA") %>% # also run for 2017 to compare with belasco
	filter(CoverageLevel %in% c(0.70,0.75,0.80,0.85,0.90)) %>%
	summarise(
		insured_acreage_mil = sum(NetReportedQuantity, na.rm = TRUE)/1000000,
		avg_acreage_per_policy = mean(sum(NetReportedQuantity,na.rm=TRUE)/sum(PoliciesSoldCount,na.rm = TRUE),na.rm = TRUE),
		#subsidy_amount = mean(SubsidyAmount,na.rm = TRUE), # get from AIB code/data
		loss_ratio = sum(IndemnityAmount,na.rm = TRUE)/sum(TotalPremiumAmount,na.rm = TRUE),
		farmer_loss_ratio = sum(IndemnityAmount,na.rm = TRUE)/(sum(TotalPremiumAmount-SubsidyAmount,na.rm = TRUE))
		) %>%
	ungroup %>%
	mutate(CoverageLevel = "Total")

belasco_table_2 <- bind_rows(belasco_table_2_ca,belasco_table_2_all_ca) %>%
	View

rm(list = ls())