


db1_bar_2_fun <- function(db, date_range, sex_choices, race_choices) {
  db <- filter(db, sex %in% sex_choices & race %in% race_choices)
  db <- db %>% filter(date_start >= date_range[1] & date_start <= date_range[2])
  ggplot() +
    geom_col(data = db, aes(y = disease_type,
                            x = death_by_disease, 
                            fill = age_group), position = "fill") +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
    )
}

