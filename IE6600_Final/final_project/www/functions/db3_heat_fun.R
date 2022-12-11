


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
    scale_y_continuous(trans = "reverse", breaks = unique(heat$day)) +
    scale_x_continuous(breaks =c(1:12)) +
    # scale_fill_viridis(name="Hrly Temps C",option ="C") +
    scale_fill_gradient(name = case_choice, high = "#f39c12", low = "#0073b7") +
    facet_grid(year ~.) +
    theme(
      axis.text.x  = element_text(size = 20),
      axis.text.y  = element_text(size = 10),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      strip.text.y = element_text(size = 20)

    )
}
