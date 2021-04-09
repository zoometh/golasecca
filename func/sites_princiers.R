library(RPostgreSQL)

create_sp_from_PG <- T

if(create_sp_from_PG){
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv,
                   dbname="mailhac_9",
                   host="localhost",
                   port=5432,
                   user="postgres",
                   password="postgres")
  sites_princiers <- dbGetQuery(con,"select site, ST_X(geom) as x,  ST_Y(geom) as y from objets where type like 'site_princ%'")
}
