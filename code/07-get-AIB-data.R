# Scrape county base values and premiums rates from USDA Actuarial Information Browser
# - Written by James Keeler, jbkeeler@ucdavis.edu
# - 2019

ca_counties <- readRDS("data_intermediary/ca_counties.RDS") %>%
  as.data.frame %>%
  dplyr::select(-geometry)

PRF_grids_counties <- readRDS("data_intermediary/PRF_grids_counties.RDS") %>%
	as.data.frame %>%
	left_join(ca_counties, by = "county_code") %>%
	rename(
		name = NAME,
		code = county_code
	) %>%
	mutate(
    code = sprintf("%03d", code),
    name_query = paste0(gsub(" ", "+", name, fixed = TRUE),"+(",code,")")
  ) 


# Generate list of county-grid-year combinations for query

county_grid_list <- list()

i <- 1
for(year in 2011:2021) {
  for(county_code in unique(PRF_grids_counties$code)) {
    county <- filter(PRF_grids_counties, code == county_code)
    for(grid in unlist(county$grid)) {

      county_grid_list[[i]] <- list(
        county_name = unique(county$name_query),
        year = year,
        county_code = county_code,
        grid = grid
      )

      i <- i + 1

    }
  }
}



# For each county-grid-year, scrape the cbv and prate.
# Note that the AIB presents this data in tables which changed format from year to year
# The 2019 to 2021 code is new and hasn't been tested as thoroughly
county_base_values <- list()
premium_rates <- list()


i <- 1
j <- 1
for(county_grid in county_grid_list) {

  print(paste0("COUNTY BASE VALUE ",county_grid$county_name,", ",county_grid$county_code,", ",county_grid$grid,", ",county_grid$year))

  url <- paste0("https://webapp.rma.usda.gov/apps/ActuarialInformationBrowser",county_grid$year,"/ReportDisplay.aspx")
  body <- list(DataSource = "R", 
               CountyName = county_grid$county_name, 
               #ReportID = "PremiumRate",
               ReportID = "CountyBaseValue", 
               InsurancePlanCode = "13", 
               CommodityCode = "0088", 
               CommodityYear = as.character(county_grid$year), 
               StateCode = "06", 
               County = county_grid$county_code, 
               GridID = as.character(county_grid$grid))
  
  aib_viewstate <- POST(url,body = body, encode = "form")
  aib_viewstate_params <- html_nodes(read_html(content(aib_viewstate,as = "text")), "input[type='hidden']") %>% html_attr("value")
  
  
  body <- list(uxScriptManager =  "uxScriptManager|uxReportViewer$ctl09$Reserved_AsyncLoadTarget",
               `__EVENTTARGET` =  "uxReportViewer$ctl09$Reserved_AsyncLoadTarget",
               `__EVENTARGUMENT` =  "",
               `__VIEWSTATE` =  aib_viewstate_params[[1]],
               `__VIEWSTATEGENERATOR` = aib_viewstate_params[[2]],
               `uxReportViewer$ctl03$ctl00` = "",
               `uxReportViewer$ctl03$ctl01` = "",
               `uxReportViewer$ctl10` = "ltr",
               `uxReportViewer$ctl11` = "standards",
               `uxReportViewer$AsyncWait$HiddenCancelField` = FALSE,
               `uxReportViewer$ToggleParam$store` = "",
               `uxReportViewer$ToggleParam$collapse`  = FALSE,
               `uxReportViewer$ctl05$ctl00$CurrentPage` = "",
               `uxReportViewer$ctl05$ctl03$ctl00` = "",
               `uxReportViewer$ctl08$ClientClickedId` = "",
               `uxReportViewer$ctl07$store` = "",
               `uxReportViewer$ctl07$collapse` =  TRUE,
               `uxReportViewer$ctl09$VisibilityState$ctl00` = "None",
               `uxReportViewer$ctl09$ScrollPosition` =  "",
               `uxReportViewer$ctl09$ReportControl$ctl02` = "",
               `uxReportViewer$ctl09$ReportControl$ctl03` = "",
               `uxReportViewer$ctl09$ReportControl$ctl04` = 100,
               ReportViewerState = "",
               `__ASYNCPOST` =  TRUE)
  
  aib_report_html <- POST(url,body = body, encode = "form")

  table <- list()
  
  if(county_grid$year %in% 2011:2012) {

    try({
    table <- readHTMLTable(content(aib_report_html, as = "text"),header = TRUE,trim=TRUE) %>% 
      tail(n=1) %>%
      as.data.frame %>%
      t %>%
      data.frame %>%
      dplyr::select(-1:-2) %>%
      slice(-1) 
    
    colnames(table) <- tolower(gsub(" ", "_", unlist(table[1,]), fixed = TRUE))
    rownames(table) <- 0:(nrow(table)-1)
    
    table <- table %>% 
      slice(-1) %>%
      as.data.frame() %>%
      mutate(year = county_grid$year,
             code = county_grid$county_code,
             grid = county_grid$grid,
             minimum_percent_of_value = minimum_acre_percent,
             maximum_percent_of_value = maximum_acre_percent) %>%
      mutate_all(as.character) %>%
      dplyr::select(year,code,grid,type,practice,county_base_value,minimum_percent_of_value,maximum_percent_of_value)
    })
    
  } else if(county_grid$year %in% 2013:2015) {

    try({
    table <- readHTMLTable(content(aib_report_html, as = "text"),header = TRUE,trim=TRUE) %>% 
      tail(n=1) %>%
      as.data.frame %>%
      t %>%
      data.frame %>%
      dplyr::select(-1:-2) %>%
      slice(-1)
    
    colnames(table) <- tolower(gsub(" ", "_", unlist(table[1,]), fixed = TRUE))
    rownames(table) <- 0:(nrow(table)-1)
    
    table <- table %>% 
      slice(-1) %>%
      as.data.frame() %>%
      mutate(year = county_grid$year,
             code = county_grid$county_code,
             grid = county_grid$grid) %>%
      mutate_all(as.character) %>%
      dplyr::select(year,code,grid,type,practice,county_base_value,minimum_percent_of_value,maximum_percent_of_value)
    })
    
  } else if(county_grid$year %in% 2016:2021) {
    
    try({
      table <- readHTMLTable(content(aib_report_html, as = "text"),header = TRUE,trim=TRUE) %>% 
      tail(n=2) 
    
    desc_table <- table[[1]] %>%
      t %>%
      data.frame %>%
      dplyr::select(-1:-2) %>%
      slice(-1)
    
    colnames(desc_table) <- tolower(gsub(" ", "_", unlist(desc_table[1,]), fixed = TRUE))
    rownames(desc_table) <- 0:(nrow(desc_table)-1)
    
    desc_table <- desc_table %>% 
      slice(-1) %>%
      as.data.frame() %>%
      mutate(year = county_grid$year,
             code = county_grid$county_code,
             grid = county_grid$grid) %>%
      dplyr::select(year,code,grid,type,practice)
    
    values_table <- table[[2]] %>%
      t %>%
      data.frame %>%
      dplyr::select(-1:-2) %>%
      slice(-1) %>%
      mutate(X3= lead(X3))
    
    colnames(values_table) <- tolower(gsub(" ", "_", unlist(values_table[1,]), fixed = TRUE))
    rownames(values_table) <- 0:(nrow(values_table)-1)
    
    values_table <- values_table %>% 
      slice(2:(nrow(values_table)-1)) %>%
      as.data.frame()
    
    table <- cbind(desc_table,values_table) %>%
      mutate_all(as.character) %>%
      dplyr::select(year,code,grid,type,practice,county_base_value,minimum_percent_of_value,maximum_percent_of_value)
    })
  }

  if(class(table) == "list"){
    print(paste0("LIST!!!: ","COUNTY BASE VALUE ",county_grid$county_name,", ",county_grid$county_code,", ",county_grid$grid,", ",county_grid$year))
  } else {
    county_base_values[[i]] <- table
  }

  print(paste0("PREMIUM RATE ",county_grid$county_name,", ",county_grid$county_code,", ",county_grid$grid,", ",county_grid$year))
      
  url <- paste0("https://webapp.rma.usda.gov/apps/ActuarialInformationBrowser",county_grid$year,"/ReportDisplay.aspx")
  body <- list(DataSource = "R", 
               CountyName = county_grid$county_name, 
               ReportID = "PremiumRate",
               InsurancePlanCode = "13", 
               CommodityCode = "0088", 
               CommodityYear = as.character(county_grid$year), 
               StateCode = "06", 
               County = county_grid$county_code, 
               GridID = as.character(county_grid$grid))
  
  aib_viewstate <- POST(url,body = body, encode = "form")
  aib_viewstate_params <- html_nodes(read_html(content(aib_viewstate,as = "text")), "input[type='hidden']") %>% html_attr("value")
  
  
  body <- list(uxScriptManager =  "uxScriptManager|uxReportViewer$ctl09$Reserved_AsyncLoadTarget",
               `__EVENTTARGET` =  "uxReportViewer$ctl09$Reserved_AsyncLoadTarget",
               `__EVENTARGUMENT` =  "",
               `__VIEWSTATE` =  aib_viewstate_params[[1]],
               `__VIEWSTATEGENERATOR` = aib_viewstate_params[[2]],
               `uxReportViewer$ctl03$ctl00` = "",
               `uxReportViewer$ctl03$ctl01` = "",
               `uxReportViewer$ctl10` = "ltr",
               `uxReportViewer$ctl11` = "standards",
               `uxReportViewer$AsyncWait$HiddenCancelField` = FALSE,
               `uxReportViewer$ToggleParam$store` = "",
               `uxReportViewer$ToggleParam$collapse`  = FALSE,
               `uxReportViewer$ctl05$ctl00$CurrentPage` = "",
               `uxReportViewer$ctl05$ctl03$ctl00` = "",
               `uxReportViewer$ctl08$ClientClickedId` = "",
               `uxReportViewer$ctl07$store` = "",
               `uxReportViewer$ctl07$collapse` =  TRUE,
               `uxReportViewer$ctl09$VisibilityState$ctl00` = "None",
               `uxReportViewer$ctl09$ScrollPosition` =  "",
               `uxReportViewer$ctl09$ReportControl$ctl02` = "",
               `uxReportViewer$ctl09$ReportControl$ctl03` = "",
               `uxReportViewer$ctl09$ReportControl$ctl04` = 100,
               ReportViewerState = "",
               `__ASYNCPOST` =  TRUE)
  
  aib_report_html <- POST(url,body = body, encode = "form")

  table <- list()
  
  if(county_grid$year == 2011) {
    try({
    table <- readHTMLTable(content(aib_report_html, as = "text"),header = TRUE,trim=TRUE) %>% 
      tail(n=1) %>%
      as.data.frame %>%
      t %>%
      data.frame %>%
      dplyr::select(-1) %>%
      mutate(X2= as.character(lag(X2))) %>%
      mutate(X3= lead(X3)) %>%
      slice(c(-1,-8)) %>%
      mutate_all(as.character) 
    
    table[1,1] <- "coverage_level"
    
    colnames(table) <- table[1,]
    rownames(table) <- 0:(nrow(table)-1)
    
    table <- table %>% 
      slice(-1) %>%
      melt(id.vars="coverage_level",value.name = "premium_rate",variable.name="practice") %>%
      as.data.frame() %>%
      mutate(year = county_grid$year,
             code = county_grid$county_code,
             grid = county_grid$grid,
             type = "Grazing") %>%
      mutate_all(as.character) %>%
      dplyr::select(year,code,grid,type,practice,coverage_level,premium_rate)
    })
    
  } else if(county_grid$year %in% 2012:2021) {

    try({
    
    table <- readHTMLTable(content(aib_report_html, as = "text"),header = TRUE,trim=TRUE) %>% 
      tail(n=2) 
    
    desc_table <- table[[1]] %>%
      t %>%
      data.frame 
    
    desc_table <- desc_table %>%
      dplyr::select(2:(ncol(desc_table)-1)) %>%
      slice(-1)
    
    colnames(desc_table) <- tolower(gsub(" ", "_", unlist(desc_table[1,]), fixed = TRUE))
    rownames(desc_table) <- 0:(nrow(desc_table)-1)
    
    desc_table <- desc_table %>% 
      slice(-1) %>%
      as.data.frame() %>%
      mutate_all(as.character) %>%
      mutate(year = county_grid$year,
             code = county_grid$county_code,
             grid = county_grid$grid,
             id = 1:n()) %>%
      dplyr::select(year,code,grid,type,practice,id)
    
    values_table <- table[[2]] %>%
      t %>%
      data.frame %>%
      mutate(X3= lead(X3,2)) %>%
      dplyr::select(-1:-2) %>%
      slice(-1) %>%
      mutate_all(as.character)
    
    colnames(values_table) <- values_table[1,]
    rownames(values_table) <- 1:(nrow(values_table))
    
    values_table <- values_table %>% 
      slice(2:(n()-2)) %>%
      melt(measure.vars = 1:5, variable.name = "coverage_level", value.name = "premium_rate") %>%
      group_by(coverage_level) %>%
      mutate(id = 1:n()) %>%
      ungroup
    
    
    table <-left_join(values_table,desc_table, by="id") %>%
      mutate_all(as.character) %>%
      dplyr::select(year,code,grid,type,practice,coverage_level,premium_rate)

    })
  }
  
  if(class(table) == "list"){
    print(paste0("LIST!!!: ","PREMIUM RATE ",county_grid$county_name,", ",county_grid$ounty_code,", ",county_grid$grid,", ",county_grid$year))
  } else {
    premium_rates[[i]] <- table
    i <- i + 1
  }

  county_grid_list[[j]] <- NULL

  j <- j +1
}


aib_base_values <- bind_rows(county_base_values)

saveRDS(aib_base_values,"data_intermediary/PRF_aib_base_values.RDS")


aib_premium_rates <- bind_rows(premium_rates)

saveRDS(aib_premium_rates,"data_intermediary/PRF_aib_premium_rates.RDS")



setIntervalCodeRI <- function(interval){
  if(grepl("Jan - Feb", interval, ignore.case = TRUE)){
    return(625)
  } else if(grepl("Feb - Mar", interval, ignore.case = TRUE)){
    return(626)
  } else if(grepl("Mar - Apr", interval, ignore.case = TRUE)){
    return(627)
  } else if(grepl("Apr - May", interval, ignore.case = TRUE)){
    return(628)
  } else if(grepl("May - Jun", interval, ignore.case = TRUE)){
    return(629)
  } else if(grepl("Jun - Jul", interval, ignore.case = TRUE)){
    return(630)
  } else if(grepl("Jul - Aug", interval, ignore.case = TRUE)){
    return(631)
  } else if(grepl("Aug - Sep", interval, ignore.case = TRUE)){
    return(632)
  } else if(grepl("Sep - Oct", interval, ignore.case = TRUE)){
    return(633)
  } else if(grepl("Oct - Nov", interval, ignore.case = TRUE)){
    return(634)
  } else if(grepl("Nov - Dec", interval, ignore.case = TRUE)){
    return(635)
  } else {
    return(NA)
  }
}



convertInterval <- function(interval){
  if(grepl("Jan - Feb", interval, ignore.case = TRUE)){
    return("January-February")
  } else if(grepl("Feb - Mar", interval, ignore.case = TRUE)){
    return("February-March")
  } else if(grepl("Mar - Apr", interval, ignore.case = TRUE)){
    return("March-April")
  } else if(grepl("Apr - May", interval, ignore.case = TRUE)){
    return("April-May")
  } else if(grepl("May - Jun", interval, ignore.case = TRUE)){
    return("May-June")
  } else if(grepl("Jun - Jul", interval, ignore.case = TRUE)){
    return("June-July")
  } else if(grepl("Jul - Aug", interval, ignore.case = TRUE)){
    return("July-August")
  } else if(grepl("Aug - Sep", interval, ignore.case = TRUE)){
    return("August-September")
  } else if(grepl("Sep - Oct", interval, ignore.case = TRUE)){
    return("September-October")
  } else if(grepl("Oct - Nov", interval, ignore.case = TRUE)){
    return("October-November")
  } else if(grepl("Nov - Dec", interval, ignore.case = TRUE)){
    return("November-December")
  } else {
    return(NA)
  }
}


intervals_to_lag <- c(634,635)


county_base_values <- readRDS("data_intermediary/PRF_aib_base_values.RDS") %>% # readRDS("Data/Intermediate/PRF_aib_base_values.RDS")
  select(year,code,grid,type,practice,county_base_value) %>%
  unique %>%
  filter(grepl("graz", type, ignore.case = TRUE)) %>%
  mutate(type = "grazing") %>%
  rowwise %>%
  mutate(
    interval_code = setIntervalCodeRI(practice),
      interval = convertInterval(practice)
  ) %>%
  ungroup %>%
  dplyr::select(-practice,-type) %>%
  rename(county_code = code) %>%
  mutate(
    year = as.integer(year),
    grid = as.integer(grid),
    county_code = as.integer(as.character(county_code)),
    county_base_value = as.numeric(county_base_value),
    county_base_value_available_index = 1
  ) %>%
  tidyr::complete(
    nesting(year,grid),
    nesting(interval_code,interval),
    fill = list(county_base_value_available_index=0)
  )

# don't actually need to lag these, but just maintaining structure across data sets
county_base_values_lagged <- county_base_values %>%
    filter(interval_code %in% intervals_to_lag) %>%
    group_by(grid,interval_code) %>%
    arrange(grid,interval_code) %>%
    mutate(
      county_base_value = dplyr::lag(county_base_value),
      county_base_value_available_index = dplyr::lag(county_base_value_available_index)
    ) %>%
    ungroup %>%
    mutate(
      interval_code = interval_code-100,
      interval = paste0("Lagged ",interval)
    )

county_base_values <- bind_rows(county_base_values,county_base_values_lagged)

saveRDS(county_base_values,"data_intermediary/PRF_grid_county_base_values.RDS") 




premium_rates <- readRDS("data_intermediary/PRF_aib_premium_rates.RDS") %>%
  select(year,code,grid,type,practice,coverage_level,premium_rate) %>%
  unique %>%
  filter(grepl("graz", type, ignore.case = TRUE)) %>%
  mutate(type = "grazing") %>%
  rowwise %>%
  mutate(
    interval_code = setIntervalCodeRI(practice),
      interval = convertInterval(practice)
  ) %>%
  ungroup %>%
  dplyr::select(-practice,-type) %>%
  rename(county_code = code) %>%
  mutate(
    year = as.integer(year),
    grid = as.integer(grid),
    county_code = as.integer(as.character(county_code)),
    coverage_level = as.numeric(as.character(coverage_level)),
    premium_rate = as.numeric(premium_rate),
    premium_rate_available_index = 1
  ) %>%
  tidyr::complete(
    nesting(year,grid),
    nesting(interval_code,interval),
    coverage_level,
    fill = list(premium_rate_available_index=0)
  )

premium_rates_lagged <- premium_rates %>%
  filter(interval_code %in% intervals_to_lag) %>%
    group_by(grid,interval_code,coverage_level) %>%
    arrange(grid,interval_code,coverage_level) %>%
    mutate(
      premium_rate = dplyr::lag(premium_rate),
    premium_rate_available_index = dplyr::lag(premium_rate_available_index)
    ) %>%
    ungroup %>%
    mutate(
      interval_code = interval_code-100,
      interval = paste0("Lagged ",interval)
    )

premium_rates <- bind_rows(premium_rates,premium_rates_lagged)

saveRDS(premium_rates,"data_intermediary/PRF_grid_county_premium_rates.RDS")


write.csv(county_base_values, "data_intermediary/PRF_CA_county_base_values.csv", na=".")

write.csv(premium_rates, "data_intermediary/PRF_CA_premium_rates.csv", na=".")  


rm(list = ls())
