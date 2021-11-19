plot_missing <- function(dataset, percent = FALSE) {
  library(cowplot)
  library(ggplot2)
  library(tidyverse)
  missing_patterns <- data.frame(is.na(dataset)) %>%
    group_by_all() %>%
    count(name = "cp", sort = TRUE) %>%
    ungroup()
  
  # Sort missing patterns 
  df <- missing_patterns %>% arrange(desc(cp))
  
  # names for types
  col_names <- names(missing_patterns)
  type_names <- col_names[col_names != "cp"]
  
  # Convert logical values to numerival
  cols <- sapply(df, is.logical)
  df[,cols] <- lapply(df[,cols], as.numeric)
  
  # add a pattern id column for missing patterns
  df$pattern <- seq(nrow(df), 1, -1)
  
  
  # create df for count/percent of missing types and patterns (will be used for side plots)
  type_sums <- colSums(df[, type_names]*df$cp)
  if (percent==TRUE){
    df_types <- data.frame(type = type_names, cp = type_sums/nrow(dataset) * 100) %>%
      arrange(desc(cp))
    df_patterns <- df %>% select(pattern, cp) %>% mutate(cp = cp/sum(cp) * 100) 
  }else{
    df_types <- data.frame(type = type_names, cp = type_sums) %>% arrange(desc(cp))
    df_patterns <- df %>% select(pattern, cp)
  }
  
  
  # change values for the complete case to 2 
  for(i in 1:nrow(df)) { 
    if (any(df[i, type_names]) == 0){
      df[i, type_names] = 2
      break
    }
  }
  
  # convert df to long form for geom_tile
  df <- df %>% gather("type", "missing", type_names)
  
  # convert missing values (0, 1, 2) to factors
  df$missing <- as.factor(df$missing)
  
  # sort type variables for order of x-axis
  df_types <- df_types %>% arrange(desc(cp))
  df_types$type <- factor(df_types$type,levels = df_types$type)
  df$type <- factor(df$type,levels = df_types$type)
  
  # main plot 
  p1 <- ggplot(df, aes(x=type, y=pattern, fill=missing)) + geom_tile(color = "white") +
    scale_fill_manual(values = c("0"="gray", "1"="blue", "2"="pink")) +
    scale_y_continuous(breaks = seq(max(df$pattern), min(df$pattern), by = -1))+
    theme(legend.position="none")+
    xlab("variable")+
    ylab("missing_pattern") 
  
  complete_id <- df$pattern[df$missing==2]
  
  if (length(df$pattern[df$missing==2])>0){
    p1 <- p1 + 
      annotate("text", x = length(type_names)/2, y = complete_id[1], label = "Complete Case")
  }
  
  
  # side plot for pattern count/percent
  p2 <- ggplot(df_patterns, aes(pattern, cp)) +
    geom_bar(stat='identity') +
    scale_x_continuous(breaks = seq(max(df$pattern), min(df$pattern), by = -1)) + 
    theme_bw() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    coord_flip() 
  
  # side plot for type count/percent
  p3 <- ggplot(df_types, aes(type, cp)) +
    geom_bar(stat='identity')+
    theme_bw() +
    theme(axis.title.x=element_blank())  +
    ggtitle("Missing value patterns")
  
  if(percent==TRUE){
    p2 <-  p2 + ylab("% rows") + scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) 
    p3 <-  p3 + ylab("% rows missing")  + scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25))
  }else{
    p2 <- p2 + ylab("row count")
    p3 <- p3 + ylab("num rows missing")
  }
  
  
  plot_grid(plotlist=list(p3, ggplot() + theme_bw()+theme(panel.border = element_blank()), 
                          p1, p2),
            ncol=2, 
            rel_widths=c(5,2), rel_heights=c(2,5), align="hv")
}
