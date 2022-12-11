


db2_loli_fun <- function(loli, date_range, states, age_choices) {
  loli <- loli %>% filter(group == "By Month" & state != "United State")
  loli$covi_death[is.na(loli$covi_death)] <- 0
  loli <- loli %>% mutate("date" = make_date(year = year, month = month))
  loli <- loli %>% filter(tolower(state) %in% states)
  loli <- subset(loli, select = c("date", "condition_group", "age_group", "covi_death"))
  ## Here loli filter by date and age_group
  loli <- loli %>% filter(date >= date_range[1] & date <= date_range[2])
  loli <- loli %>% filter(age_group %in% age_choices)
  
  loli_plot1 <- loli %>% group_by(condition_group) %>% summarise(total_deaths <- sum(covi_death))
  
  loli <- loli %>% group_by(condition_group, age_group) %>% summarise(total_deaths <- sum(covi_death))
  
  colnames(loli)[3] = "deaths"
  colnames(loli_plot1)[2] = "deaths"
  
  
  
  loli_plot1$total_deaths <- sum(loli_plot1$deaths)
  loli_plot1$percent_100 <- round(loli_plot1$deaths / loli_plot1$total_deaths * 100, 1)
  loli_plot1 <- loli_plot1 %>% arrange(desc(-percent_100))
  new_order <- loli_plot1$condition_group
  
  ggplot(loli_plot1, aes(x = percent_100, y = reorder(condition_group, percent_100))) +
    geom_text(aes(label = paste(percent_100, "%")), vjust = -0.3, hjust = -0.7) +
    geom_point(aes(size = percent_100, color = percent_100), alpha = 0.5) +
    geom_segment(aes(
      x = 0,
      xend = percent_100,
      y = condition_group,
      yend = condition_group,
      color = percent_100
    )) +
    scale_size_continuous(range = c(5, 20)) +
    scale_x_reverse() +
    coord_cartesian(clip = 'off') +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.text.y = element_text(size = 12, face = "bold")) +
    scale_y_discrete(position = "right")
  
}

