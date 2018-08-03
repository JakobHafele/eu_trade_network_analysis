make_facet_plot <- function(y_var, x_var, title, ylog=TRUE){
  if (ylog==TRUE){
    y_val_log <- paste0("log(", y_var, ")")
    facet_plot <- ggplot(work_data_2, aes_string(x=x_var, y_val_log, color="year")) + 
      geom_point() +
      ggtitle(paste0(title, y_var)) + 
      facet_wrap(~year, scales = "free_y") +
      geom_smooth(method = "lm", colour="black") +
      theme(legend.position = "bottom",
            panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
            panel.border = element_blank(), axis.line = element_line(),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))
  } else {
    facet_plot <- ggplot(work_data_2, aes_string(x=x_var, y_var, color="year")) + 
      geom_point() +
      ggtitle(paste0(title, y_var)) + 
      facet_wrap(~year, scales = "free_y") +
      geom_smooth(method = "lm", colour="black") +
      theme(legend.position = "bottom",
            panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
            panel.border = element_blank(), axis.line = element_line(),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))
  }
  return(facet_plot)
}


calc_cors <- function(data, y_var, x_var){
  data %>%
  group_by(year) %>%
  summarize(bla=cor(x_var,y_var))
}

find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}
