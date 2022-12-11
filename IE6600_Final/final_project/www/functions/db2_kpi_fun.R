


db2_kpi_fun <- function(kpi, date_range, states, age_choices) {
  # kpi <- condition_covi_cleaned
  kpi <- kpi %>% filter(group == "By Month" & state != "United State")
  kpi$covi_death[is.na(kpi$covi_death)] <- 0
  kpi <- kpi %>% mutate("date" = make_date(year = year, month = month))
  kpi <- kpi %>% filter(tolower(state) %in% states)
  kpi <- subset(kpi, select = c("date", "condition_group", "age_group", "covi_death"))
  #filter by date and age
  kpi <- kpi %>% filter(date >= date_range[1] & date <= date_range[2])
  kpi <- kpi %>% filter(age_group %in% age_choices)
  
  
  kpi <- kpi %>% group_by(condition_group) %>% summarise(total_deaths <- sum(covi_death))
  colnames(kpi)[2] = "deaths"
  kpi$total_deaths <- sum(kpi$deaths)
  kpi$percent_100 <- round(kpi$deaths / kpi$total_deaths * 100, 1)
  kpi <- kpi %>% arrange(desc(percent_100))
  kpi
  
  kpi_values <- c(0, 0, 0)
  kpi_text <- c("", "", "")
  
  if(dim(kpi)[1] == 0){
    return (list(kpi_values, kpi_text))
  }
  
  kpi_values[1] <- kpi$percent_100[kpi$condition_group == "COVID-19"]
  kpi_text[1] <- "COVID-19"
  
  
  
  kpi <- kpi %>% filter(condition_group != "COVID-19")
  
  kpi_values[2] <- as.numeric(kpi[1, 4])
  kpi_values[3] <- as.numeric(kpi[2, 4])
  kpi_text[2] <- as.character(kpi$condition_group[1])
  kpi_text[3] <- as.character(kpi$condition_group[2])
  kpi_values
  
  return(list(kpi_values, kpi_text))
  
}

