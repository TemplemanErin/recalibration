library(ggplot2)

## create theme - erin_theme
## increase font size, white background, increase title size

theme_presentation <- function(){
  
theme_bw() %+replace%         #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(), #strip major gridlines
      panel.grid.minor = element_blank(), #strip minor gridlines
      #axis.ticks = element_blank(),
      
      #text elements
      plot.title = element_text(size = 20, face = 'bold',
                                hjust = 0, vjust = 2),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(size = 9, hjust = 1),
      
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 22),
      axis.text.x = element_text(margin = margin(5, b = 10)),
      
      legend.title = element_text(size = 22),
      legend.text = element_text(size=22)
      
    )
  
}

theme_articles <- function(){
  
  theme_bw() %+replace%         #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(), #strip major gridlines
      panel.grid.minor = element_blank(), #strip minor gridlines
      #axis.ticks = element_blank(),
      
      #text elements
      plot.title = element_text(size = 20, face = 'bold',
                                hjust = 0, vjust = 2),
      plot.subtitle = element_text(size = 14),
      plot.caption = element_text(size = 9, hjust = 1),
      
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 18),
      axis.text.x = element_text(margin = margin(5, b = 10)),
      axis.text.y = element_text(margin = margin(5, b = 10)),
      
      legend.title = element_text(size = 12),
      legend.text = element_text(size=12)
      
    )
  
}
