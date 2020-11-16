# library(ggnetwork)
library(plotly)
library(network)
# install.packages("GGally")
# library(GGally)
# devtools::install_github("briatte/ggnet")
library(ggnet)
n <- data.frame(event1 = c(-0.2,0.8,0.4,0),
                event2 = c(0.34,-0.17,0.3,0),
                event3 = c(0.2,0.1,-0.73,0),
                row.names = letters[1:4])
net <- network(n,
               matrix.type = "bipartite",
               ignore.eval = FALSE,
               names.eval = "weights")

col = c("actor" = "grey", "event" = "gold")
set.edge.attribute(net, "color", ifelse(net %e% "weights" > 0, "green", "red"))
g <- ggnet2(net, color = "mode", palette = col, label = TRUE, edge.color = "color",edge.label="weights")
gp <- ggplotly(g)
htmlwidgets::saveWidget(as_widget(gp), "ggplot.html")
