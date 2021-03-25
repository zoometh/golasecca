library(kableExtra) 
library(dplyr)
library(knitr)
library(shiny)
library(visNetwork)
library(stringi)
library(rdflib)

# fonctionne en local mais le déploiement web ne fonctionne pas


base <- "https://github.com/zoometh/golasecca/tree/main/"
# load(file=paste0(base,"df_per_count.RData"))
# load(paste0(getwd()"/LOD/df_per_count.RData"))


text.size <- 20

# insttutions
path.logos <- "https://raw.githubusercontent.com/zoometh/golasecca/main/img/logo/"
name.i <- c("CNRS-PSL,AOrOc,UMR8546",
            "C2RMF,Louvre",
            "Musée d'Archéologie,nationale",
            "University,of Padua",
            "University,of Turin",
            "Musei Reali,of Turin",
            "Musei Civici,of Reggio Emilia"
            )
name.ip <- c("Veronica CICOLANI<br>Julien ZURBACH<br>Agnès TRICOCHE<br>Jean-Baptiste HOUAL<br>Frédérique MARCHAND-BEAULIEU",
             "Benoît MILLE<br>Elsa LAMBERT",
             "Christine LORRE",
             "Gilberto ARTIOLI<br>Ivana ANGELINI",
             "Alessandro LO GIUDICE<br>Alessandro RE<br>Eliano DIANA",
             "Elisa PEREGO",
             "Giada PELLEGRINI")
# logos <- ("aoroc.png")
n.inst <- length(name.i)
icons.path <- paste0(path.logos, logos)
inst <- data.frame(id= name.i,
                   # label = name.i,
                   label = gsub(",","\n", name.i),
                   color = c(rep("#d3d3d3", n.inst)),
                   title = name.ip,
                   # title = c(name.ip, rep(stri_rand_lipsum(1), n.inst-1)),
                   font.size = rep(30, n.inst),
                   # font = list(size = c(rep(text.size, n.inst))),
                   # image = icons.path,
                   shape = c(rep("text", n.inst)),
                   size = c(rep(50, n.inst)),
                   group = c(rep("wkp", n.inst))
)
# wp
name.w <- c("WP1", "WP2", "WP3")
n.wkps <- length(name.w)
wkps <- data.frame(id= name.w,
                   label = name.w,
                   color = c("#ffff00","#e26b0a","#963634"),
                   title = c("WP1 Chemical analyis <a href='https://www.google.com'>link text</a>",
                             "WP2 Archaeological & Technological anlysis",
                             "WP3 Database"),
                   font.size = rep(text.size, n.wkps),
                   # font = list(size = c(rep(text.size, n.wkps))),
                   # image = c(rep(icons.path, n.wkps)),
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
            "Network analysis", "FAIR referencing")
n.tasks <- length(name.t)
tasks <- data.frame(id=name.t,
                    label = gsub(" ","\n", name.t),
                    color = c(rep("#808080", n.tasks)),
                    title = c("Inductively coupled plasma mass spectrometry (ICP-MS) is a type of mass spectrometry that uses an Inductively coupled plasma to ionize the sample. <i>wiki</i>",
                              "Isotope analysis is the identification of isotopic signature, the abundance of certain stable isotopes and chemical elements within organic and inorganic compounds <i>wiki</i>",
                              "A scanning electron microscope (SEM) is a type of electron microscope that produces images of a sample by scanning the surface with a focused beam of electrons",
                              "X-ray fluorescence (XRF) is the emission of characteristic 'secondary' (or fluorescent) X-rays from a material that has been excited by being bombarded with high-energy X-rays or gamma rays",
                              rep(stri_rand_lipsum(1), 4),
                              rep(stri_rand_lipsum(1), 4)),
                    shape = c(rep("text", n.tasks)),
                    font.size = rep(text.size, n.tasks),
                    
                    # font = list(size = c(rep(text.size, n.tasks))),
                    # image = c(rep(icons.path, n.tasks)),
                    size = c(rep(10, n.tasks)),
                    group = c(rep("tsk", n.tasks)))
nodes <- rbind(wkps, tasks, inst)

edges <- data.frame(from=c(name.t,
                           rep("CNRS-PSL,AOrOc,UMR8546", 3),
                           rep("C2RMF,Louvre", 2),
                           rep("Musée d'Archéologie,nationale", 2),
                           rep("University,of Padua", 1),
                           rep("University,of Turin", 2),
                           rep("Musei Reali,of Turin", 2),
                           rep("Musei Civici,of Reggio Emilia", 2)
                           ),
                    to=c(rep("WP1", 4), rep("WP2", 4), rep("WP3", 4),
                         c("WP1","WP2", "WP3"),
                         c("WP1", "WP2"),
                         c("WP1", "WP2"),
                         c("WP1"),
                         c("WP1", "WP2"),
                         c("WP1", "WP2"),
                         c("WP1", "WP2")
                         ),
                    width = ".5")

## check
# u.edges <- c(unique(edges$from), unique(edges$to))
# u.nodes <- nodes$id
# setdiff(u.edges, u.nodes)
# setdiff(u.nodes, u.edges)

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
    # nodes.selected <- c(unique(edges.selected$from), unique(edges.selected$to))
    # print(nodes.selected)
    nodes.selected <- unique(c(edges.selected$from, edges.selected$to))
    # print(nodes.selected)
    nodes.selected <- nodes[nodes$id %in% nodes.selected,]
    ## check
    # u.edges <- c(unique(edges.selected$from), unique(edges.selected$to))
    # u.nodes <- nodes.selected$id
    # print(setdiff(u.edges, u.nodes))
    # print(setdiff(u.nodes, u.edges))
    
    visNetwork(nodes.selected, edges.selected) %>%
      # visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
      # visLayout(randomSeed = 2)
      visIgraphLayout()
  })
})

shinyApp(ui, server, options = list(height = 600))