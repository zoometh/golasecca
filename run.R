# run different variable
# to create leaflet .html ouputs
source("func/functions.R") # load/run ? function
chm <- getwd()
for(per in c("GIC_IIAB","GIIAB_IIIA1","GIIIA1_IIIA3","GIIIA3")){
  f.lflt.per(chm,tab,per) # create leaflet html map 
}
