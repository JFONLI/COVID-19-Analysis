


db2_bar_1_fun <- function(db, states, sex) {
  
  db <- db %>% filter(tolower(state) %in% states)
  db$covi_death[is.na(db$covi_death)] <- 0
  
  
  p <- ggplot(db) +
    geom_col(aes(x = covi_death, y = condition_group, fill = age_group), position = "fill") +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_blank())
  
  p
  # ggplotly(p)
  
}
