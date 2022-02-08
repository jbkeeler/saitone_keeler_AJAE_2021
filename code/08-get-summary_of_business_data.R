# Clean USDA Summary of Business data
# - Written by James Keeler, jbkeeler@ucdavis.edu
# - 2019

years <- 2002:2019

sob_tpu_col_names <- 	
	c(
		"CommodityYear","StateCode","StateName","StateAbbreviation","CountyCode","CountyName",
		"CommodityCode","CommodityName","InsurancePlanCode","InsurancePlanAbbreviation",
		"CoverageTypeCode","CoverageLevelPercent","DeliveryID","TypeCode","TypeName","PracticeCode",
		"PracticeName","UnitStructureCode","UnitStructureName","NetCommodityReportingLevelAmount",
		"CommodityReportLevelType","LiabilityAmount","TotalPremiumAmount","SubsidyAmount",
		"IndemnityAmount","Loss Ratio","EndorsedCommodityReportingLevelAmount"
	)

read_SOBTPU_data_by_year <- function(year,aggr) {

	year_label <- substr(year, 3, 4)
	filename <- paste0("data_raw/USDA_RMA_Summary_of_Business/Type_Practice_Unit_Structure/SOBSCCTPU",year_label,".TXT")
	data <- read.delim(filename, sep = "|", header = FALSE,stringsAsFactors = FALSE) 
	names(data) <- sob_tpu_col_names

	return(data)

}

sob_tpu <- sapply(years, read_SOBTPU_data_by_year, simplify=FALSE) %>%
	bind_rows

saveRDS(sob_tpu,"data_intermediary/sob_tpu.RDS")

write.csv(sob_tpu, "data_intermediary/sob_tpu.csv", na=".")




sob_cov_col_names <- 	
	c(
		"CommodityYear","StateCode","StateAbbreviation","CountyCode","CountyName",
		"CommodityCode","CommodityName","InsurancePlanCode","InsurancePlanAbbreviation",
		"CoverageCategory","DeliveryType","CoverageLevel","PoliciesSoldCount","PoliciesEarningPremiumCount","PoliciesIndemnifiedCount",
		"UnitsEarningPremiumCount","UnitsIndemnifiedCount","QuantityType","NetReportedQuantity",
		"EndorsedCompanionAcres","LiabilityAmount","TotalPremiumAmount","SubsidyAmount","StatePrivateSubsidy","AdditionalSubsidy",
		"EFAPremiumDiscount","IndemnityAmount","LossRatio"
	)

read_SOBCOV_data_by_year <- function(year,aggr) {

	year_label <- substr(year, 3, 4)
	filename <- paste0("data_raw/USDA_RMA_Summary_of_Business/State_County_Crop_Coverage_Level/sobcov",year_label,".TXT")
	data <- read.delim(filename, sep = "|", header = FALSE,stringsAsFactors = FALSE) 
	names(data) <- sob_cov_col_names

	return(data)

}

sob_cov <- sapply(years, read_SOBCOV_data_by_year, simplify=FALSE) %>%
	bind_rows

saveRDS(sob_cov,"data_intermediary/sob_cov.RDS")

write.csv(sob_cov, "data_intermediary/sob_cov.csv", na=".")


# Just the PRF program data (double check that this doesn't inlude apiary data)
sob_cov_prf <- sob_cov  %>%
	filter(CommodityCode == 88 | InsurancePlanCode %in% c(13,14) & !(CommodityCode %in% c(1191,332)))

saveRDS(sob_cov_prf,"data_intermediary/sob_cov_prf.RDS")

write.csv(sob_cov_prf, "data_intermediary/sob_cov_prf.csv", na=".")


rm(list = ls())