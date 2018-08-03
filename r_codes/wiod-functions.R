make_facet_plot <- function(y_var, x_var, title, ylog=TRUE){
  if (ylog==TRUE){
    y_val_log <- paste0("log(", y_var, ")")
    facet_plot <- ggplot(work_data_1, aes_string(x=x_var, y_val_log, color="Country")) + 
      geom_point() +
      ggtitle(paste0(title, y_var)) + 
      facet_wrap(~Country, scales = "free_y") +
      geom_smooth(method = "lm", colour="black") + # formula = "log(y)~x"
      theme(legend.position = "bottom",
            panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
            panel.border = element_blank(), axis.line = element_line(),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))
  } else {
    facet_plot <- ggplot(work_data_1, aes_string(x=x_var, y_var, color="Country")) + 
      geom_point() +
      ggtitle(paste0(title, y_var)) + 
      facet_wrap(~Country, scales = "free_y") +
      geom_smooth(method = "lm", colour="black") +
      theme(legend.position = "bottom",
            panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
            panel.border = element_blank(), axis.line = element_line(),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))
  }
  return(facet_plot)
}

