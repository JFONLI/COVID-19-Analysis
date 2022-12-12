


db2_ts_fun <- function(db, date_range, states, age_groups) {
  
  db <- db %>% filter(tolower(state) %in% states & age_group %in% age_groups)
  db <- db %>% mutate("date" = make_date(year = year, month = month))
  db <- db %>% drop_na()
  db <- db %>% filter(date >= date_range[1] & date <= date_range[2])
  db <- db %>% group_by(date, condition_group) %>% summarise(deaths = sum(covi_death))
  
  db$condition_group <- as.character(db$condition_group)
  
  db$condition_group[db$condition_group == "Intentional and unintentional injury, poisoning, and other adverse events"] <- 
    "Injury, poisoning, and other adverse events"
  
  db$condition_group[db$condition_group == "All other conditions and causes (residual)"] <- 
    "All other conditions and causes"
  
  p <- ggplot(db) +
          geom_line(aes(x = date, y = deaths, color = condition_group), size = 1.2)
  
  # p
  
  p <- p + theme_bw()
    
  
  ggplotly(p)
  
}
