bnssgtheme <- function(base_size = 12, base_family = "sans",base_colour = "black"){theme_bw() %+replace% theme(
  axis.title.x = element_text(size = 16, color = '#003087', face = 'bold', family = "sans", margin = margin(t = 0, r = 20, b = 0, l = 0)), #x Axis Titles
  axis.title.y = element_text(size = 16, color = '#003087', angle = 90, face = 'bold', family = "sans", margin = margin(t = 0, r = 20, b = 0, l = 0)), #y Axis Titles
  axis.text = element_text(size = 12,  family = "sans", color = 'black'), #Axis text
  panel.border = element_blank(), #remove plot border
  panel.grid.major.x = element_blank(), #no major vertical lines
  panel.grid.major.y = element_line(linetype = 'dotted', size = 1), #dotted major horizontal lines
  panel.grid.minor = element_blank(), #no minor lines
  legend.position = "top", #legend on top
  legend.justification='left', #legend left
  legend.direction='horizontal', #legend to be horizontal
  legend.title = element_blank(), #No legend title
  legend.text = element_text(size = 12, family = "sans",),
  legend.key.size = unit(0.3, "cm"),
  plot.title = element_text(size = 16, color = '#003087', face="bold", margin = margin(b = 10, t=10), hjust=0),
  plot.subtitle = element_text(size = 10, margin = margin(b = 10), hjust=0),
  # Customize facet title appearance
  strip.background = element_blank(),  # Set background to white
  strip.text = element_text(face = "bold", family = "sans", size = 12)  # Set font to Arial 12 for facet titles
) 
}
