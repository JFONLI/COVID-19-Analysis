


db2_map_plot <- function(db, date_range, age_groups, condition_groups) {
  db <- db %>% filter(age_group %in% age_groups & condition_group %in% condition_groups)
  db <- db %>% mutate("date" = make_date(year = year, month = month))
  db <- db %>% filter(date >= date_range[1] & date <= date_range[2])
  db$covi_death[is.na(db$covi_death)] <- 0
  db <- db %>% group_by(state) %>% summarise(deaths = sum(covi_death))

  plot_usmap(data = db, values = "deaths") +
    # scale_fill_continuous(
    #   low = "white", high = "darkblue",
    #   limits = c(0, 850000),
    # )
    
    scale_fill_gradientn(
      colours = colorRampPalette(c("white", "darkblue"))(10000),
      limits = c(0, 850000),
    )

}

