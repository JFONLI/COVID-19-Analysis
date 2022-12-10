


db3_map_fun <- function(db, date_range, case_choice) {
  db <- db %>% filter(date >= date_range[1] & date <= date_range[2])
  if(case_choice == "Cases"){
    db <- db %>% filter(number_type == "tot_cases")
  }
  if(case_choice == "Deaths"){
    db <- db %>% filter(number_type == "tot_death")
  }

  db <- db %>% group_by(state) %>% summarise(total_cases = sum(number_value))
  plot_usmap(data = db, values = "total_cases") +
    scale_fill_gradientn(
      colours = colorRampPalette(c("white", "darkblue"))(10000),
      limits = c(0, 4443870030),
    )
}
