# Setup -------------------------------------------------------------------
require(ggplot2)
require(lme4)
require(lmerTest)
require(combinat)
require(effectsize)
require(RColorBrewer)
require(dplyr)
require(tidyr)
require(forcats)
require(ggcorrplot)

theme_update(strip.background = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank(),
             plot.background = element_blank(),
             axis.text=element_text(size=30, colour = "black"),
             axis.title=element_text(size=18, face = "bold"),
             axis.title.x = element_text(vjust = 0),
             legend.title = element_text(size = 24, face = "bold"),
             legend.text = element_text(size = 20),
             plot.title = element_text(size = 26, face = "bold", vjust = 1),
             panel.margin = unit(1.0, "lines"), 
             plot.margin = unit(c(0.5,  0.5, 0.5, 0.5), "lines"),
             axis.line = element_line(colour = "black", size = 2),
             axis.ticks = element_line(color = 'black', size = 3),
             axis.ticks.length = unit(.25, 'cm')
)

theme_black = function(base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(fill = NA, color = "white"),  
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
    )
}
se = function(x) {return(sd(x, na.rm = T) / sqrt(sum(!is.na(x))))}
se.prop = function(x) {return(sqrt(mean(x, na.rm = T) * (1-mean(x, na.rm = T)) / sum(!is.na(x))))}
as.string.vector = function(x) {
  return(strsplit(x,',')[[1]])
}
as.numeric.vector = function(x) {
  return(as.numeric(strsplit(gsub('\\[|\\]','',x),',')[[1]]))
}
as.string = function(x) {
  return(paste(x, collapse = ','))
}
dodge <- position_dodge(width=0.9)

# Only works in RStudio -- otherwise you have to set the path manually
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Import ------------------------------------------------------------------

metadata_df = read.csv("formatted_imdb.csv")
df = read.csv('tagdl.csv')
testing = read.csv('mv_ids.csv')

metadata_df_200 = metadata_df %>% filter(item_id %in% testing$item_id)
df_200 = df %>% filter(item_id %in% metadata_df_200$item_id)

#the 12 tags
att.names = c('action', #or fight scenes
              'cult classic',
              'drama',
              'fantasy world',
              'science fiction',
              'romantic',
              'suspense',
              'art',
              'funny',
              'quirky',
              'adventure', #or civil war/ computer animation
              'violent'
              )

df.wide_200 = df_200 %>% pivot_wider(names_from = tag, values_from = score)

df.merge_200 = merge(metadata_df_200, df.wide_200, by = "item_id")
df.wide.filt_200 = df.merge_200 %>% select(item_id, att.names)

# compute the correlations b/w tags
df.cors_200 = round(cor(df.wide.filt_200),1)
ggcorrplot(df.cors_200, type = 'lower', lab = T)

df.filt_200 = df_200 %>% filter(tag %in% att.names)
ggplot(df.filt_200, aes(x = score)) +
  geom_histogram() +
  facet_wrap(~tag)

