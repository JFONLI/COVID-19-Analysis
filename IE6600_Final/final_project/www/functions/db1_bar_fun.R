


db1_bar_fun <- function(loli, date_range, sex_choices, race_choices) {
  loli <- filter(loli, sex %in% sex_choices & race %in% race_choices)
  loli <- loli %>% filter(date_start >= date_range[1] & date_start <= date_range[2])
  
  loli <- subset(loli, select = c("date_start", "disease_type", "sex", "age_group", "death_by_disease"))
  colnames(loli) <- c("date", "diseases", "sex", "age_group", "deaths")
  loli %>% arrange(desc(date))
  # filter date sex race
  loli_plot1 <- loli
  loli_plot1 <- loli_plot1 %>% group_by(diseases) %>% summarise(total_deaths <- sum(deaths))
  colnames(loli_plot1)[2] <- "deaths"
  
  
  loli_plot1$total_deaths <- sum(loli_plot1$deaths)
  loli_plot1
  loli_plot1$percent_100 <- round(loli_plot1$deaths / loli_plot1$total_deaths * 100, 1)
  loli_plot1 <- loli_plot1 %>% arrange(desc(-percent_100))
  new_order <- loli_plot1$diseases
  
  loli$diseases <- ordered(loli$diseases, levels = new_order)
  
  ggplot(loli) +
    geom_col(aes(x = deaths, y = diseases, fill = age_group), position = "fill") +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )
}

