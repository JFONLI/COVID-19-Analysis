


db2_bar_2_fun <- function(db, states, sex) {
  
  db <- db %>% filter(tolower(state) %in% states)
  db$covi_death[is.na(db$covi_death)] <- 0
  db$total_deaths <- sum(db$covi_death)
  db <- db %>% group_by(condition_group, total_deaths) %>% 
    summarise(deaths = sum(covi_death))
  db$percents_100 <- db$deaths / db$total_deaths * 100
  
  
  p <- ggplot(db) +
    geom_col(aes(x = percents_100, y = condition_group)) +
    theme(axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_blank()) +
    # geom_text(aes(y = 0.05, label = paste(percents_100, "%")), angle = 90, hjust = 0, check_overlap = T) +
    scale_y_discrete(position = "right") +
    scale_x_reverse()
  
  p
  # ggplotly(p)
  
}
