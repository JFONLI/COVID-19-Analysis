


db2_bar_fun <- function(loli, date_range, states, age_choices) {
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
  
  loli$condition_group <- ordered(loli$condition_group, levels = new_order)
  
  ggplot(loli) +
    geom_col(aes(x = deaths, y = condition_group, fill = age_group), position = "fill") +
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

