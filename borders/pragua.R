library(googledrive)
library(openxlsx)

source("func/functions.R") # load data, functions

# non local data (ie, googledrive data)
# /!\ need to switch the first ws to read it
drive_find(n_max = 30)
unlink("DATA_01_IT_VC.xlsx")
ggdocument.url <- "DATA_01_IT_VC.xlsx"
data.border <- drive_download(ggdocument.url)
data <- read.xlsx("DATA_01_IT_VC.xlsx")

chrono <- f.chrono(chm, 0)
data <- f.data(chm, 0)
tab <- cbind(data,chrono) # associe les tableaux
nb.sites <- length(unique(tab$sites))
nb.typ.obj <- length(unique(tab$objets))
obj.personnels <- unique(tab$objets)[1]
obj.collectifs <- paste0(unique(tab$objets)[2:4], collapse = ", ")
nb.typ.sites <- length(unique(tab$contexte.site))
nb.periodes <- nrow(periodes.df)
Gdebut <- min(periodes.df$tpq)
Gfin <- max(periodes.df$taq)

# data <- data[,c(1, 2, 7, 8, 13)] # subset
# colnames(data)[1] <- 'x' 
# colnames(data)[2] <- 'y' 
# colnames(data)[3] <- 'sites'
# colnames(data)[4] <- 'objets'
# colnames(data)[5] <- 'contexte.site' 
# View(data)


