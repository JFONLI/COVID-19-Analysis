


db1_loli_fun <- function(loli, date_range, sex_choices, race_choices) {
  # filter date sex race
  loli <- filter(loli, sex %in% sex_choices & race %in% race_choices)
  loli <- subset(loli, select = c("date_start", "disease_type", "sex", "age_group", "death_by_disease"))
  colnames(loli) <- c("date", "diseases", "sex", "age_group", "deaths")
  loli %>% arrange(desc(date))
  # filter date sex race
  loli <- loli %>% filter(date >= date_range[1] & date <= date_range[2])
  
  
  loli_plot1 <- loli
  loli_plot1 <- loli_plot1 %>% group_by(diseases) %>% summarise(total_deaths <- sum(deaths))
  colnames(loli_plot1)[2] <- "deaths"
  
  
  loli_plot1$total_deaths <- sum(loli_plot1$deaths)
  loli_plot1
  loli_plot1$percent_100 <- round(loli_plot1$deaths / loli_plot1$total_deaths * 100, 1)
  loli_plot1 <- loli_plot1 %>% arrange(desc(-percent_100))
  new_order <- loli_plot1$diseases
  
  ggplot(loli_plot1, aes(x = percent_100, y = reorder(diseases, percent_100))) +
    geom_text(aes(label = paste(percent_100, "%"), size = percent_100/6), vjust = -0.6, hjust = -0.3) +
    geom_point(aes(size = percent_100, color = percent_100), alpha = 0.5) +
    geom_segment(aes(
      x = 0,
      xend = percent_100,
      y = diseases,
      yend = diseases,
      color = percent_100
    )) +
    scale_size_continuous(range = c(5, 20)) +
    scale_x_reverse() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.text.y = element_text(size = 10)) +
    scale_y_discrete(position = "right")
}

