


db3_ts_fun <- function(db, date_range, states, case_choice) {
  db <- db %>% filter(tolower(state) %in% states)
  db <- db %>% filter(date >= date_range[1] & date <= date_range[2])
  
  # ggplot(db, aes(x = date)) +
  #   geom_area(aes(y = tot_cases), color = "blue", fill = "blue", size = 1) +
  #   geom_area(aes(y = new_case), color = "red", fill = "red", size = 1)
  
  if(case_choice == "Cases"){
    db <- subset(db, select = c("date", "new_case", "tot_cases"))
  }
  
  
  if(case_choice == "Deaths"){
    db <- subset(db, select = c("date", "new_death", "tot_death"))
  }
  
  colnames(db) <- c("date", "new", "total")

  ggplot(db, aes(x = date)) +
    geom_area(aes(y = total), color = "blue", fill = "blue", size = 1) +
    geom_area(aes(y = new), color = "red", fill = "red", size = 1)
}
