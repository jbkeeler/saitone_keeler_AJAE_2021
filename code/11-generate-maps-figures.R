# Generate figures for paper
# - Written by James Keeler, jbkeeler@ucdavis.edu
# - 2019

ca_counties <- readRDS("data_intermediary/ca_counties.RDS") %>%
	rename(code = COUNTYFP)

ca_state <- readRDS("data_intermediary/ca_state.RDS") 

PRF_grids <- readRDS("data_intermediary/PRF_grids.RDS")

PRF_grids_counties <- readRDS("data_intermediary/PRF_grids_counties.RDS")

veg <- readRDS("data_intermediary/PRF_cal_fraps_veg.RDS")

elev <- readRDS("data_intermediary/PRF_NED_elevation.RDS")

sob_cov <- readRDS("data_intermediary/sob_cov.RDS") %>%
	mutate(QuantityType = str_trim(QuantityType))

sob_tpu <- readRDS("data_intermediary/sob_tpu.RDS") %>%
	mutate(CommodityReportLevelType = str_trim(CommodityReportLevelType))

rangeland_RI_NDVI_NOAA_CDR <- readRDS("data_final/rangeland_RI_NDVI_NOAA_CDR.RDS") %>%
	mutate(
		NDVI_percent_of_normal = ifelse(is.nan(NDVI_percent_of_normal) | NDVI_percent_of_normal == -Inf,NA,NDVI_percent_of_normal)
	)

contract_quality_grids <- readRDS("data_final/contract_quality_grids.RDS")

grid_combo_cor <- readRDS("data_final/grid_interval_combo_R2.RDS")

county_base_values <- unique(contract_quality_grids$county_base_value)

# Figure 1

PRF_grids_cbv <- left_join(PRF_grids,veg,by=c("grid")) %>%
  filter(!is.na(rangeland)) %>%
  left_join(contract_quality_grids,by=c("grid")) %>%
  mutate(
    county_base_value = ifelse(veg_cat == "non_rangeland","non_rangeland",county_base_value),
    cbv_factor = factor(
      county_base_value, 
      levels = c(sort(county_base_values),"non_rangeland"),
      labels = c("13.40","15.90","18.00","18.10","26.40","29.10","29.60","Non-Rangeland")
    )
  )

plot <- ggplot() +
  geom_sf(data=PRF_grids_cbv, aes(fill = cbv_factor),color = "black",alpha = 0.9,size=0.2) +
  geom_sf(data=ca_state, fill = NA,color = "black",alpha = 0.9,size=0.3) +
  coord_sf(crs = st_crs(ca_state), datum = NA) +
  scale_fill_manual(values = c("Non-Rangeland" = "white", "13.40" = "#d3d3d3", "15.90" = "#acacac", "18.00" = "#878787", "18.10" = "#646464", "26.40" = "#424242", "29.10" = "#242424", "29.60" = "#000000")) +
  ggtitle(paste0("PRF Rangeland Grids By County Base Value")) +
  labs(fill = "County Base Value", caption = "Sources: USDA RMA Pasture, Rangeland, Forage Support Tool; CAL FIRE FRAP") +
  theme_map() +
  theme(
    plot.title = element_text(size = 16,hjust=0.5),
    legend.position=c(0.65, 0.60), # "none
    # legend.title = element_blank(),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )

ggsave(plot,filename = paste0("Figures/prf_grid_rangelands_cbv.pdf"), width = 6, height = 7.5, device = cairo_pdf)



# Figure 2

rangeland_RI_NDVI_dist <- rangeland_RI_NDVI_NOAA_CDR %>%
	dplyr::select(grid,year,interval,interval_code,RI_percent_of_normal,NDVI_percent_of_normal,elev_leq_1524,rangeland) %>%
	unique %>%
	filter(rangeland == 1 & year %in% 2011:2018 & interval_code > 600, elev_leq_1524 == 1) %>%
	dplyr::select(RI_percent_of_normal,NDVI_percent_of_normal,elev_leq_1524) %>%
	pivot_longer(c("RI_percent_of_normal","NDVI_percent_of_normal"), names_to = "source", values_to = "index_value")


mean_ri <- mean(filter(rangeland_RI_NDVI_dist,source == "RI_percent_of_normal")$index_value, na.rm = TRUE)
max_ri <- max(filter(rangeland_RI_NDVI_dist,source == "RI_percent_of_normal")$index_value, na.rm = TRUE)

ggplot() +
	stat_density(data=filter(rangeland_RI_NDVI_dist,source == "RI_percent_of_normal"), aes(x=index_value),fill = "black", alpha = 0.85,geom = "area", position = "identity", bw = "SJ", kernel = "epanechnikov") +
	geom_vline(xintercept=mean_ri, color = "gray30", linetype="dashed", size=0.5, alpha = 0.8) +
	annotate(geom="label", x=mean_ri, y = Inf, hjust = "left", vjust = 1, label = paste0("Mean = ",round(mean_ri, digits = 3))) +
	annotate(geom="label", x=Inf, y = Inf, hjust = "right", vjust = 1, label = paste0("Max = ",round(max_ri, digits = 3))) +
	#scale_x_continuous(expand = c(0, 0)) +
	#facet_wrap(~status,ncol=1) +
	#scale_color_manual(values = c("CIMIS" = "#800000", "NOAA" = "#008080")) +
	coord_cartesian(xlim=c(0, 5)) +
	ggtitle("Distribution of Rainfall Index Values for California Rangelands, 2011-2018") +
	labs(x = "Rainfall Index Value", y = "Density") +
	theme_bw() +
	theme(
		plot.title = element_text(hjust = 0.5,size=11),
		legend.position = "none",
		panel.grid.major.x = element_blank(),
		#axis.title.x = element_text(family="serif"),
		#axis.title.x = element_blank(),
		axis.title.y = element_blank(),
		#axis.text.x = element_text(family="serif"),
		#axis.text.y = element_blank(),
		legend.text = element_text(size ="12"), # ,family="serif"
		legend.title = element_blank()
	)


ggsave(paste0("Figures/rangeland_RI_NDVI_elev_leq_1524_density_2011_2018_RI_NOAA_CDR.pdf"), width=8, height=4, device = cairo_pdf)

mean_ndvi <- mean(filter(rangeland_RI_NDVI_dist,source == "NDVI_percent_of_normal")$index_value, na.rm = TRUE)
max_ndvi <- max(filter(rangeland_RI_NDVI_dist,source == "NDVI_percent_of_normal")$index_value, na.rm = TRUE)
min_ndvi <- min(filter(rangeland_RI_NDVI_dist,source == "NDVI_percent_of_normal")$index_value, na.rm = TRUE)


ggplot() +
	stat_density(data=filter(rangeland_RI_NDVI_dist,source == "NDVI_percent_of_normal"), aes(x=index_value),fill = "black", alpha = 0.85,geom = "area", position = "identity", bw = "SJ", kernel = "epanechnikov") +
	geom_vline(xintercept=mean_ndvi, color = "gray30", linetype="dashed", size=0.5, alpha = 0.8) +
	annotate(geom="label", x=mean_ndvi, y = Inf, hjust = "left", vjust = 1, label = paste0("Mean = ",round(mean_ndvi, digits = 3))) +
	annotate(geom="label", x=Inf, y = Inf, hjust = "right", vjust = 1, label = paste0("Max = ",round(max_ndvi, digits = 3))) +
	annotate(geom="label", x=-Inf, y = Inf, hjust = "left", vjust = 1, label = paste0("Min = ",round(min_ndvi, digits = 3))) +
	#facet_wrap(~status,ncol=1) +
	#scale_color_manual(values = c("CIMIS" = "#800000", "NOAA" = "#008080")) +
	coord_cartesian(xlim=c(0, 2)) +
	ggtitle("Distribution of NDVI (NOAA CDR) Index Values for California Rangelands, 2011-2018") +
	labs(x = "NDVI Index Value", y = "Density") +
	theme_bw() +
	theme(
		plot.title = element_text(hjust = 0.5,size=11),
		legend.position = "none",
		panel.grid.major.x = element_blank(),
		#axis.title.x = element_text(family="serif"),
		#axis.title.x = element_blank(),
		axis.title.y = element_blank(),
		#axis.text.x = element_text(family="serif"),
		#axis.text.y = element_blank(),
		legend.text = element_text(size ="12"), # ,family="serif"
		legend.title = element_blank()
	)


ggsave(paste0("Figures/rangeland_RI_NDVI_elev_leq_1524_density_2011_2018_NDVI_NOAA_CDR.pdf"), width=8, height=4, device = cairo_pdf)



# Figure 3

PRF_grids_r2 <- left_join(PRF_grids,filter(grid_combo_cor,RI_NDVI_R2 == max_R2),by=c("grid")) %>%
  mutate(
    interval_combo = factor(
      interval_combo, 
      levels= c(
        "Lagged October-November, January-February", 
        "Lagged October-November, February-March",
        "Lagged October-November, March-April",
        "Lagged October-November, April-May",
        "Lagged October-November, May-June",
        "Lagged November-December, January-February",
        "Lagged November-December, February-March",
        "Lagged November-December, March-April",
        "Lagged November-December, April-May",
        "Lagged November-December, May-June",
        "January-February, March-April",
        "January-February, April-May",
        "January-February, May-June",
        "February-March, April-May",
        "February-March, May-June",
        "March-April, May-June"
      )#,
      #labels = c("October-November (t-1)", "November-December (t-1)", "January-February", "February-March")
    )
  )

combos <- unique(grid_combo_cor$interval_combo)


plot <- ggplot() +
  geom_sf(data=PRF_grids_r2, aes(fill = interval_combo),color = "black",alpha = 0.9,size=0.2) +
  geom_sf(data=ca_state, fill = NA,color = "black",alpha = 0.9,size=0.3) +
  coord_sf(crs = st_crs(ca_state), datum = NA) +
  scale_fill_discrete(na.value="transparent") +
  ggtitle(paste0("Optimal Interval Combos by PRF Grid (R-squared)")) +
  theme_map() +
  theme(
    plot.title = element_text(size = 16, hjust = 3),
    plot.margin = margin(0, 9, 0, 0, "cm"),
    legend.position=c(.90, 0.30), # "none
    legend.title = element_blank(),
    legend.text=element_text(size = 9.5),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )

ggsave(plot,filename = paste0("Figures/prf_grid_optimal_combos.pdf"), width = 9, height = 7.5, device = cairo_pdf)
rm(list = ls())
