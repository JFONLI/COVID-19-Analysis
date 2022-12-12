


db1_ts_fun <- function(ts, date_range, sex_choices, race_choices, age_choices) {
  ts <- filter(ts, sex %in% sex_choices & race %in% race_choices & age_group %in% age_choices)
  ts <- ts %>% filter(date_start >= date_range[1] & date_start <= date_range[2])
  
  ts <- ts %>% group_by(date_start, disease_type) %>% summarise(deaths = sum(death_by_disease))
  
  p <- ggplot(ts) +
    geom_line(aes(x = date_start, y = deaths, color = disease_type), size = 1.2)
  
  
  p <- p + theme_bw() + xlab("Date") + ylab("Deaths")
  
  
  ggplotly(p)
}

