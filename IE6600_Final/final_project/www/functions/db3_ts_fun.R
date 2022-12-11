


db3_ts_fun <- function(db, date_range, states, case_choice) {
  db <- db %>% filter(tolower(state) %in% states)
  db <- db %>% filter(date >= date_range[1] & date <= date_range[2])
  
  # ggplot(db, aes(x = date)) +
  #   geom_area(aes(y = tot_cases), color = "blue", fill = "blue", size = 1) +
  #   geom_area(aes(y = new_case), color = "red", fill = "red", size = 1)
  
  if(case_choice == "Cases"){
    db <- subset(db, select = c("date", "new_case", "tot_cases"))
  }
  
  
  if(case_choice == "Deaths"){
    db <- subset(db, select = c("date", "new_death", "tot_death"))
  }
  
  colnames(db) <- c("date", "new", "total")
  
  ylim.total <- c(0, max(db$total))
  ylim.new <- c(0, max(db$new))
  
  b <- diff(ylim.total)/diff(ylim.new)
  a <- ylim.total[1] - b*ylim.new[1]
  
  p <- ggplot(db, aes(x = date, y = total)) +
    geom_area(fill = "#0073b7", color = "#0073b7") +
    geom_area(aes(y = a + new*b), color = "#f39c12", fill = "#f39c12") +
    scale_y_continuous("Total Cases", sec.axis = sec_axis(~ (. - a)/b, name = "New Cases")) +
    theme_bw() +
    theme(
      axis.line.y.right = element_line(color = "#f39c12"), 
      axis.ticks.y.right = element_line(color = "#f39c12"),
      axis.text.y.right = element_text(color = "#f39c12"), 
      axis.title.y.right = element_text(color = "#f39c12"),
      
      axis.line.y.left = element_line(color = "#0073b7"), 
      axis.ticks.y.left = element_line(color = "#0073b7"),
      axis.text.y.left = element_text(color = "#0073b7"), 
      axis.title.y.left = element_text(color = "#0073b7")
    )
  
  ggplotly(p)

}
