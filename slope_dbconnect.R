
#' Initialises connection with database.
#'
#' @param database Database to connect to.

db_connect <- function(database) {

# library(pool)
# library(dplyr)
# library(dbplyr)
# library(postGIStools)
# library(RPostgreSQL)
# library(R2jags)


#Set connection parameters
pg_drv<-dbDriver("PostgreSQL")
pg_db <- database
pg_schema <- "Hydrography"
pg_tmp_schema <- "temporary"
pg_user <- rstudioapi::askForPassword("Enter username")
pg_password <- rstudioapi::askForPassword(paste("Enter password for",pg_user))
pg_host <- "vm-srv-finstad.vm.ntnu.no"


#Initialise connection
con<-dbConnect(pg_drv,dbname=pg_db,user=pg_user, password=pg_password,host=pg_host)

# load db pool connection
# retrieves info on lake and presence data, plus most environmental data
nofa_db <- dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  user=pg_user,
  password=pg_password,
  host = "vm-srv-finstad.vm.ntnu.no",
  dbname = "nofa",
  options="-c search_path=nofa" # set db schema from where to look
)

# load db pool connection
# retrieves info on lake and presence data, plus most environmental data
temp_db <- dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  user=pg_user,
  password=pg_password,
  host = "vm-srv-finstad.vm.ntnu.no",
  dbname = "nofa",
  options="-c search_path=temporary" # set db schema from where to look
)


}
