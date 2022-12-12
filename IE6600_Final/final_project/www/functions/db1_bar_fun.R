


db1_bar_fun <- function(loli, date_range, sex_choices, race_choices, age_choices) {
  loli <- filter(loli, sex %in% sex_choices & race %in% race_choices & age_group %in% age_choices)
  loli <- loli %>% filter(date_start >= date_range[1] & date_start <= date_range[2])
  
  loli <- subset(loli, select = c("date_start", "disease_type", "sex", "age_group", "death_by_disease"))
  colnames(loli) <- c("date", "diseases", "sex", "age_group", "deaths")
  # filter date sex race
  loli_plot1 <- loli
  loli_plot1 <- loli_plot1 %>% group_by(diseases) %>% summarise(total_deaths <- sum(deaths))
  colnames(loli_plot1)[2] <- "deaths"
  
  
  loli_plot1$total_deaths <- sum(loli_plot1$deaths)
  loli_plot1$percent_100 <- round(loli_plot1$deaths / loli_plot1$total_deaths * 100, 1)
  loli_plot1 <- loli_plot1 %>% arrange(desc(-percent_100))
  new_order <- loli_plot1$diseases
  
  loli$diseases <- ordered(loli$diseases, levels = new_order)
  
  shades <- colorRampPalette(c("#0f5e9c", "#2389da", "#1ca3ec", "#5abcd8", "#74ccf4"))(10)
  shades <- rev(shades)
  
  loli_sum <- loli %>% group_by(age_group, diseases) %>% summarise(deaths = sum(deaths))
  
  ggplot(loli_sum) +
    geom_col(aes(x = deaths, y = diseases, fill = age_group), position = "fill", alpha = 0.5) + 
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank()
    )
  
  
  # ggplot(loli) +
  #   geom_col(aes(y = deaths, x = diseases, fill = age_group), position = "fill") 
    # scale_fill_manual(values = shades) + 
    # theme(
    #   axis.title = element_blank(),
    #   axis.text = element_blank(),
    #   axis.ticks.x = element_blank(),
    #   axis.ticks.y = element_blank(),
    #   panel.grid.major = element_blank(),
    #   panel.grid.minor = element_blank(),
    #   panel.border = element_blank(),
    #   panel.background = element_blank()
    # )
}

