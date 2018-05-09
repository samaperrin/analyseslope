
#' Initialises connection with database.
#'
#' @title Create database connections
#' @param database Database to connect to.
#'
#' @export



slope_dbconnect <- function(database, server) {
#Set connection parameters
pg_drv<-dbDriver("PostgreSQL")
pg_db <- database
pg_schema <- "Hydrography"
pg_tmp_schema <- "temporary_sweden_connectivity"
pg_user <- getPass::getPass(msg = 'USERNAME: ')
pg_password <- getPass::getPass(msg = 'PASSWORD: ')
pg_host <- server


#Initialise connection
con<-dbConnect(pg_drv,dbname=pg_db,user=pg_user, password=pg_password,host=pg_host)

# load db pool connection
# retrieves info on lake and presence data, plus most environmental data
#nofa_db <- dbPool(
#  drv = RPostgreSQL::PostgreSQL(),
#  user=pg_user,
#  password=pg_password,
#  host = "vm-srv-finstad.vm.ntnu.no",
#  dbname = "nofa",
#  options="-c search_path=nofa" # set db schema from where to look
#)

# load db pool connection
# retrieves info on lake and presence data, plus most environmental data
nofa_db <- dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  user=pg_user,
  password=pg_password,
  host = server,
  dbname = database,
  options="-c search_path=nofa" # set db schema from where to look
)


temp_db <- dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  user=pg_user,
  password=pg_password,
  host = server,
  dbname = database,
  options="-c search_path=temporary_sweden_connectivity" # set db schema from where to look
)


a <- list(con=con,temp_db=temp_db, nofa_db=nofa_db)
return(a)
}

