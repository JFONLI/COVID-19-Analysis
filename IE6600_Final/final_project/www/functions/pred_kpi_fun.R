



pred_kpi_fun <- function(db, state_choice) {
  perState_covi_data = filter(db, state == state_choice) 
  kpi_values <- c(0, 0, 0)
  kpi_values[1] <- sum(perState_covi_data$new_case[perState_covi_data$is_forecast == "Y"])
  kpi_values[2] <- perState_covi_data$new_case[perState_covi_data$date == "2022-10-17"]
  kpi_values[3] <- perState_covi_data$new_case[perState_covi_data$date == "2023-01-25"]
  return (kpi_values)
  
}

