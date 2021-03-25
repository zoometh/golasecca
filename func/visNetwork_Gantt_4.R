library(kableExtra) 
library(dplyr)
library(knitr)
library(shiny)
library(visNetwork)
library(stringi)
library(rdflib)


base <- "https://github.com/zoometh/golasecca/tree/main/"
# load(file=paste0(base,"df_per_count.RData"))
# load(paste0(getwd()"/LOD/df_per_count.RData"))


text.size <- 100

# insttutions
path.logos <- "https://raw.githubusercontent.com/zoometh/golasecca/main/img/logo/"
name.i <- c("CNRS-PSL, AOrOc, UMR8546")
logos <- ("aoroc.png")
n.inst <- length(name.i)
icons.path <- paste0(path.logos, logos)
inst <- data.frame(id= name.i,
                     label = name.i,
                     color = c("#ffff00"),
                     # title = c(""),
                     font = list(size = c(rep(text.size, n.inst))),
                     image = icons.path,
                     shape = c(rep("box", n.inst)),
                     size = c(rep(50, n.inst)),
                     group = c(rep("wkp", n.inst))
)
# wp
name.w <- c("WP1", "WP2", "WP3")
n.wkps <- length(name.w)
wkps <- data.frame(id= name.w,
                   label = name.w,
                   color = c("#ffff00","#e26b0a","#963634"),
                   # title = c("WP1 Chemical analyis",
                   #           "WP2 Archaeological & Technological anlysis",
                   #           "WP3 Database"),
                   font = list(size = c(rep(text.size, n.wkps))),
                   image = c(rep(icons.path, n.wkps)),
                   shape = c(rep("box", n.wkps)),
                   size = c(rep(50, n.wkps)),
                   group = c(rep("wkp", n.wkps))
)
# tasks
name.t <- c("ICP-MS analysis","Isotopic analysis",
            "SEM analysis","X-r analysis",
            
            "Inventory/Drawing", "Photogrammetry",
            "Scan 3D","Microscopic analyisis",
            
            "Web Database", "Mathematical-Spatial modeling",
            "Network analysis", "Web FAIR referencing")
n.tasks <- length(name.t)
tasks <- data.frame(id=name.t,
                    label = name.t,
                    color = c(rep("#808080", n.tasks)),
                    # title = c("Inductively coupled plasma mass spectrometry (ICP-MS) is a type of mass spectrometry that uses an Inductively coupled plasma to ionize the sample. <i>wiki</i>",
                    #           "Isotope analysis is the identification of isotopic signature, the abundance of certain stable isotopes and chemical elements within organic and inorganic compounds <i>wiki</i>",
                    #           "A scanning electron microscope (SEM) is a type of electron microscope that produces images of a sample by scanning the surface with a focused beam of electrons",
                    #           "X-ray fluorescence (XRF) is the emission of characteristic 'secondary' (or fluorescent) X-rays from a material that has been excited by being bombarded with high-energy X-rays or gamma rays",
                    #           rep(stri_rand_lipsum(1), 4),
                    #           rep(stri_rand_lipsum(1), 4)),
                    shape = c(rep("text", n.tasks)),
                    font = list(size = c(rep(text.size, n.tasks))),
                    image = c(rep(icons.path, n.tasks)),
                    size = c(rep(10, n.tasks)),
                    group = c(rep("tsk", n.tasks)))
nodes <- rbind(wkps, tasks, inst)

edges <- data.frame(from=c(name.t,
                           name.i),
                    to=c(rep("WP1", 4), rep("WP2", 4), rep("WP3", 4),
                         "WP1"),
                    width = ".5")

ui <- shinyUI(
  fluidPage(
    titlePanel("ITINERIS work packages"),
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("wkp", 
                           "workpackages:",
                           choices = name.w,
                           selected = name.w),
        # checkboxGroupInput("tsk", 
        #                    "tasks:",
        #                    choices = name.t,
        #                    selected = name.t),
        width = 3),
      mainPanel(
        visNetworkOutput("network", height = "600px"),
        width = 9
      )
    )
  )
)

server <- shinyServer(function(input, output) {
  output$network <- renderVisNetwork({
    # print(input$wkp)
    edges.selected <- edges[edges$from %in% input$wkp | edges$to %in% input$wkp,]
    nodes.selected <- unique(c(edges.selected$from, edges.selected$to))
    # print(nodes.selected)
    nodes.selected <- nodes[nodes$label %in% nodes.selected,]
    visNetwork(nodes.selected, edges.selected) %>%
      visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
      visLayout(randomSeed = 2)
    #   visIgraphLayout()
  })
})

shinyApp(ui, server, options = list(height = 600))