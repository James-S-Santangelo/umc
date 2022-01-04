library(scales)

ng1 <- theme(aspect.ratio=0.7,panel.background = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.border=element_blank(),
             axis.line.x = element_line(color="black",size=1),
             axis.line.y = element_line(color="black",size=1),
             axis.ticks=element_line(size = 1, color="black"),
             axis.ticks.length=unit(0.25, 'cm'),
             axis.text=element_text(color="black",size=15),
             axis.title=element_text(color="black",size=1),
             axis.title.y=element_text(vjust=2,size=17),
             axis.title.x=element_text(vjust=0.1,size=17),
             axis.text.x=element_text(size=15),
             axis.text.y=element_text(size=15),
             strip.text.x = element_text(size = 10, colour = "black",face = "bold"),
             strip.background = element_rect(colour="black"),
             legend.position = "top", legend.direction="vertical",
             legend.text=element_text(size=17), legend.key = element_rect(fill = "white"),
             legend.title = element_text(size=17),legend.key.size = unit(1.0, "cm"))

plot_meanTemp_byDate_byButton <- function(df){
  
  button <- df %>% pull(Button) %>% unique()

  plot <- ggplot(df, aes(x = as.Date(week_date, format = '%d/%m/%Y'), y = meanTemp)) + 
    geom_point(size = 2, color = 'black') +
    geom_line() +
    scale_x_date(labels = date_format("%B %d, %Y"), breaks = '4 weeks') +
    xlab("") + ylab("Mean temperature (°C)") +
    ng1 + theme(axis.text.x=element_text(size=15, angle = 45, hjust = 1))
  
  dir.create("analysis/temp_plots/meanTemp/by_button/", showWarnings = FALSE)
  outpath <- sprintf("analysis/temp_plots/meanTemp/by_button/%s_meanTemp_byDate.pdf", button)
  ggsave(filename = outpath, plot = plot, device = "pdf", width = 6, height = 6, units = "in", dpi = 300)
  
}

# Create plots of temperature over time for each button
merged_iButton_summaries %>% 
  ungroup() %>% 
  group_split(Button) %>% 
  purrr::walk(., plot_meanTemp_byDate_byButton)

# Summary with number of rounds by Button
merged_iButton_summaries %>% 
  group_by(Park, Button) %>% 
  distinct(Round, .keep_all = TRUE) %>% 
  summarise(n_rounds = n()) %>% 
  ungroup() %>% 
  group_by(Park, n_rounds) %>%
  summarise(n_buttons = n())
  
