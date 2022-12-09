


db3_ts_fun <- function(db, date_range, states, choice) {
  db <- db %>% filter(tolower(state) %in% states)
  db <- db %>% filter(date >= date_range[1] & date <= date_range[2])
  
  ggplot(db, aes(x = date)) +
    geom_area(aes(y = tot_cases), color = "blue", fill = "blue", size = 1) +
    geom_area(aes(y = new_case), color = "red", fill = "red", size = 1)
  
  # if(choice == "Deaths"){
  #   ggplot(db, aes(x = date)) +
  #     geom_area(aes(y = tot_cases), color = "blue", fill = "blue", size = 1) +
  #     geom_area(aes(y = new_case), color = "red", fill = "red", size = 1)
  # }
  # if(choice == "Cases"){
  #   ggplot(db, aes(x = date)) +
  #     geom_area(aes(y = tot_death), color = "blue", fill = "blue", size = 1) +
  #     geom_area(aes(y = new_death), color = "red", fill = "red", size = 1)
  # }

  
}
