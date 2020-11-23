library(plotly)
library(igraph)
library(dplyr)

# # set.seed(43)

graph <- graph_from_data_frame(edgelist)
graph <- g
L <- layout.fruchterman.reingold(graph)
vs <- V(graph)
es <- as.data.frame(get.edgelist(graph))
Ne <- length(es[1]$V1)
Xn <- L[,1]
Yn <- L[,2]
network <- plot_ly(x = ~Xn,
                   y = ~Yn,
                   mode = "markers",
                   text = vs$name,
                   hoverinfo = "text")


edge_shapes <- list()
for(i in 1:Ne) {
  v0 <- es[i,]$V1
  v1 <- es[i,]$V2
  
  edge_shape = list(
    type = "line",
    line = list(color = "#030303", width = 0.3),
    x0 = Xn[match(v0,names(vs))],
    y0 = Yn[match(v0,names(vs))],
    x1 = Xn[match(v1,names(vs))],
    y1 = Yn[match(v1,names(vs))],
    opacity = 1
  )
  
  edge_shapes[[i]] <- edge_shape
  }

network <- layout(
  network,
  shapes = edge_shapes,
  xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
  yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
)

network
########################
tab.count.melt %>%
  plot_ly(x = ~variable,
          y = ~value,
          type = 'bar', 
          text = ~objets,
          hoverinfo = 'text',
          hovertext = ~objets,
          name = ~objets,
          # color = ~rgb
          marker = list(color = tab.count.melt$rgb,
                        line = list(color = 'rgb(0,0,0)',
                                    width = 1))
  ) %>%
  layout(yaxis = list(title = 'Count'),
         barmode = 'stack')

tab.count.melt %>% 
  group_by(objets) %>% 
  arrange(variable) %>%
  plot_ly(
    x = ~variable, 
    y = ~value, 
    color= ~rgb,
    name = ~objets,
    # colors = ~rgb,
    type = 'bar') %>% 
  layout(
    yaxis = list(title = 'value'), 
    barmode = 'stack')

##################

dt <- as.data.frame(matrix(ncol = 13, nrow = 19))
colnames(dt) <- c("Entity", month.abb)

for (i in 1:nrow(dt)) {
  dt[i, 1] <- paste("Entity", i, sep="")
  dt[i, -1] <- floor(runif(12, min=0, max=100))
}

# assign colors to entities

dt$"EntityColor" <- c("#074263", "#0B5394", "#3D85C6", "#6D9EEB", "#A4C2F4", "#CFE2F3", "#5B0F00", "#85200C", "#A61C00", "#CC4125", "#DD7E6B", "#E6B8AF", "#F8CBAD", "#F4CCCC", "#274E13", "#38761D", "#E06666", "#CC0000", "#20124D")

data.table::melt(tab.count.col) %>%
  # tab.count.melt %>%
  plot_ly(x = ~variable,
          y = ~value,
          name= ~objets,
          type = "bar",
          color = ~couleur,
          marker = list(colors = ~couleur)
  ) %>%
  layout(yaxis = list(title = ""),
         xaxis = list(title = ""),
         barmode = 'stack')
