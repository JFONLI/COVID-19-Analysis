


db1_bar_1_fun <- function(db, date_range, sex_choices, race_choices) {
  db <- filter(db, sex %in% sex_choices & race %in% race_choices)
  db <- db %>% filter(date_start >= date_range[1] & date_start <= date_range[2])
  
  db <- db %>% group_by(disease_type) %>% summarise(
    total_counts = sum(death_by_disease), all_causes = cause_all)
  db$sum_of_diseases <- sum(db$total_counts)
  db$percents_100 <- db$total_counts / db$sum_of_diseases
  
  ggplot(db) +
    geom_col(aes(y = disease_type, x = percents_100)) +
    scale_x_reverse() +
    scale_y_discrete(position = "right")
}

