


db1_kpi_fun <- function(kpi, date_range, sex_choices, race_choices, age_choices) {
  kpi <- kpi %>% mutate("date" = make_date(year = year_death, month = month_death))
  kpi <- kpi %>% filter(sex %in% sex_choices)
  kpi <- kpi %>% subset(select = c("date", "race", "age_group", "cause_all", "cause_other", "COVID-19 (U071, Underlying Cause of Death)"))
  
  kpi <- kpi %>% filter(age_group %in% age_choices & race %in% race_choices)
  kpi <- kpi %>% filter(date >= date_range[1] & date <= date_range[2])
  
  kpi_values <- c(0,0,0)
  
  kpi_values[1] <- round(sum(kpi$cause_all)/332000000 * 100, 1)
  kpi_values[2] <- round(sum(kpi$cause_other)/332000000 * 100, 1)
  kpi_values[3] <- round(sum(kpi$`COVID-19 (U071, Underlying Cause of Death)`)/332000000 * 100, 1)
  
  return(kpi_values)
}

