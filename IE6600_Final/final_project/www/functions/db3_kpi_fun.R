


db3_kpi_fun <- function(kpi, date_range, states) {
  
  kpi <- kpi %>% filter(date >= date_range[1] & date <= date_range[2])
  kpi <- kpi %>% filter(tolower(state) %in% states)
  
  kpi_cases <- kpi %>% filter(number_type == "new_case")
  kpi_cases <- sum(kpi_cases$number_value)
  kpi_cases <- round(kpi_cases/332000000 * 100, 1)
  
  kpi_deaths <- kpi %>% filter(number_type == "new_death")
  kpi_deaths <- sum(kpi_deaths$number_value)
  kpi_deaths <- round(kpi_deaths/332000000 * 100, 3)
  
  kpi_values <- c(0, 0)
  kpi_values[1] <- kpi_cases
  kpi_values[2] <- kpi_deaths
  
  return (kpi_values)
  
  
}

