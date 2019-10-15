# Functions 

ggTukey_char <- function(tukey_object, a.pri){

  list.of.packages <- c("tidyverse", "ggpubr")
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
 
   ### Load required libraries
  require(tidyverse)
  require(ggpubr)
  
  ### Saves THSD object's first index as a dataframe
  x <- as.data.frame(tukey_object[1])
  
  ### Separates names from THSD object
  factor_names <- row.names(x)
  
  ### Further manipulation of dataframe
  y <- x %>%
    ### Creates variable from names from THSD rownames
    mutate(names = factor_names) %>%
    ### Separates individual comparisons into x & y columns
    separate(names, sep = "-", into = c("x", "y")) %>%
    ### Keeps only p-value, x and y
    select(c(contains("p.adj"), "x", "y")) %>%
    mutate(sig = as.factor(ifelse(.[[1]] <= a.pri, "A", "B")))
  
  ### Creates plot pair-wise comparison
  plot <- ggplot(
    data = y,
    aes(
      x = x,
      y = y,
      fill = sig
    )
  ) +
    geom_point(
      ### ggpubr shape number
      shape = 21,
      color = "black",
      size = 3
    ) +
    scale_fill_manual(
      values=c("black", "white")
    ) +
    theme_minimal(
      
    ) +
    theme(
      legend.position = "none"
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )
  
  print(plot)
  
  # Selects significant values and creates dataframe
  sig_data <- y %>%
    filter(.[[1]] <= a.pri)
  
  sig_data <- sig_data[, -4]
  
  return(as.data.frame(sig_data))
}


ggTukey_num <- function(tukey_object, a.pri){
  
  list.of.packages <- c("tidyverse", "ggpubr")
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  require(tidyverse)
  require(ggpubr)
  
  x <- as.data.frame(tukey_object[1])
  
  names <- row.names(x)
  
  y <- x %>%
    mutate(names = names) %>%
    separate(names, sep = "-", into = c("x", "y")) %>%
    mutate(x = parse_number(x),
           y = parse_number(y)) %>%
    mutate(x = as.numeric(x),
           y = as.numeric(y)) %>%
    select(c(contains("adj"), "x", "y")) %>%
    mutate(sig = ifelse(.[[1]] <= a.pri, "A", "B"),
           sig = as.factor(sig))
  
  plot <- ggplot(
    data = y,
    aes(
      x = x,
      y = y,
      fill = sig
    )
  ) +
    geom_point(
      shape = 21,
      color = "black",
      size = 3
    ) +
    scale_fill_manual(
      values=c("black", "white")
    ) +
    theme_minimal(
      
    ) +
    theme(
      legend.position = "none"
    ) +
    scale_x_continuous(
      breaks = c(1:1000)
    ) +
    scale_y_continuous(
      breaks = c(1:1000)
    ) +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme(
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    )
  
  print(plot)
  
  # Selects significant values and creates dataframe
  sig_data <- y %>%
    filter(.[[1]] <= a.pri)
  
  sig_data <- sig_data[, -4]
  
  return(as.data.frame(sig_data))
  
}


