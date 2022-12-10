


db3_heat_fun <- function(heat, states, case_choice) {
  heat <- heat %>% filter(tolower(state) %in% states)
  # heat <- heat %>% group_by(day, month, year) %>% summarise(new_cases = sum(new_case))
  if(case_choice == "Cases"){
    heat <- heat %>% group_by(day, month, year) %>% summarise(new_cases = sum(new_case))
  }
  if(case_choice != "Cases"){
    heat <- heat %>% group_by(day, month, year) %>% summarise(new_cases = sum(new_death))
  }
  
  heat <- subset(heat, select = c("day", "month", "year", "new_cases"))
  heat <- heat %>% arrange(year, month, day)
  
  ggplot(heat, aes(month, day, fill = new_cases)) +
    geom_tile(color = "white", size = 0.1, width = 0.95) +
    theme_minimal(base_size = 10) +
    scale_y_continuous(trans = "reverse", breaks = unique(df$day)) +
    scale_x_continuous(breaks =c(1:12)) +
    # scale_fill_viridis(name="Hrly Temps C",option ="C") +
    scale_fill_gradient(name = case_choice, high = "red", low = "blue") +
    facet_grid(year ~.)
}
