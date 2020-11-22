# # library(ggnetwork)
# library(plotly)
# library(network)
# # install.packages("GGally")
# # library(GGally)
# # devtools::install_github("briatte/ggnet")
# library(ggnet)
# n <- data.frame(event1 = c(-0.2,0.8,0.4,0),
#                 event2 = c(0.34,-0.17,0.3,0),
#                 event3 = c(0.2,0.1,-0.73,0),
#                 row.names = letters[1:4])
# net <- network(n,
#                matrix.type = "bipartite",
#                ignore.eval = FALSE,
#                names.eval = "weights")
# 
# col = c("actor" = "grey", "event" = "gold")
# set.edge.attribute(net, "color", ifelse(net %e% "weights" > 0, "green", "red"))
# g <- ggnet2(net, color = "mode", palette = col, label = TRUE, edge.color = "color",edge.label="weights")
# gp <- ggplotly(g)
# htmlwidgets::saveWidget(as_widget(gp), "ggplot.html")

################################################################

library(plotly)
library(igraph)
library(igraphdata)

set.seed(42)

# data(karate, package="igraphdata")
# G <- upgrade_graph(karate)
# L <- layout.circle(g) #position
L <- layout.fruchterman.reingold(g)
vs <- V(g)
vs$label <- vs$name
es <- as.data.frame(get.edgelist(g))

Nv <- length(vs)
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
  # i <- 1
  v0 <- es[i,]$V1
  v1 <- es[i,]$V2
  edge_shape = list(
    type = "line",
    line = list(color = "#030303", width = 0.3),
    x0 = Xn[v0],
    y0 = Yn[v0],
    x1 = Xn[v1],
    y1 = Yn[v1]
  )
  edge_shapes[[i]] <- edge_shape
}

axis <- list(title = "",
             # showgrid = FALSE,
             # showticklabels = FALSE,
             zeroline = FALSE)
layout(
  network,
  title = 'Karate Network',
  shapes = edge_shapes,
  xaxis = axis,
  yaxis = axis
  # mode = "markers",
  # text = ~lbl.r,
  # hoverinfo = 'text',
  # hovertext = ~lbl.r
)
# 
# plot_ly(data = roches.altis,
#         x = ~sectors,
#         y = ~Z,
#         color = ~sectors,
#         type = 'scatter',
#         alpha = .5,
#         mode = "markers",
#         text = ~lbl.r,
#         hoverinfo = 'text',
#         hovertext = ~lbl.r,
#         name = ~sectors) %>% 
#   add_trace(x=impr.rocks.altis$sectors,
#             y=impr.rocks.altis$Z,
#             type = 'scatter',
#             mode = "markers",
#             # color = "#000000",
#             inherit = F,
#             showlegend = F) %>%
# 
# fig
