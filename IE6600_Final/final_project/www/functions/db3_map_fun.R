


db3_map_fun <- function(db, date_range, case_choice) {
  db <- db %>% filter(date >= date_range[1] & date <= date_range[2])
  if(case_choice == "Cases"){
    db <- db %>% filter(number_type == "tot_cases")
    color_choice = c("#0f5e9c", "#74ccf4")
  }
  if(case_choice == "Deaths"){
    db <- db %>% filter(number_type == "tot_death")
    color_choice = c("F25C54", "F7B267")
  }

  db <- db %>% group_by(state) %>% summarise(total_cases = sum(number_value))
  db <- db %>% arrange(desc(total_cases))
  ylim.cases <- db$total_cases[1]
  
  
  p <- plot_usmap(data = db, values = "total_cases")
    
  p + scale_fill_gradientn(
    colours = colorRampPalette(c("#74ccf4", "#0f5e9c"))(10000)
  )
  
  ggplotly(p)
  
}
