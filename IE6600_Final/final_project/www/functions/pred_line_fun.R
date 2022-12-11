


pred_line_fun <- function(db, state_choice) {
  perState_covi_data = filter(db, state == state_choice)
  
  p <- ggplot(perState_covi_data)
  p + geom_line(data=subset(perState_covi_data, (as.Date("2022-07-01")<date) & date<as.Date("2022-10-19")), 
                aes(date, new_case, colour="Observations"), linetype=1) + 
    geom_line(data=subset(perState_covi_data, date>=as.Date("2022-10-19")), 
              aes(date, new_case, colour="Predictions"), linetype=2) +
    scale_color_manual(name = "Color", values = c("Observations" = "black", "Predictions" = "red")) + 
    xlab("Year 2022") + 
    ylab("Daily New Cases") + 
    ggtitle("Predictions for Daily New Cases in the US for Future 100 Days") + 
    theme(plot.title = element_text(hjust = 0.5))
  # }
}

