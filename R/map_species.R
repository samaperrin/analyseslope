#' Creates a map showing lakes and presence/absence of fish.
#'
#' @title Maps fish occurrence
#' @param connectivity Table showing at the minimum location IDs for your lakes. Can also show presence/absence for different species, and can contain latitude and longitude.
#' @param get.locations If you do not have latitude and longitude, this should be set to TRUE.
#' @param species The species to map. Can be ignored, in which case map will just plot lakes.
#' @param username This and "password" are your login credentials for the nofa server, which you will need to provide if get.locations is TRUE and you need to access nofa to find coordinates of your lakes.
#' @param maptype The type of map you would like to use. Defaults to "terrain".
#'
#' @export


#connectivity_raw <- read.csv(file="./Data/sweden_connection_wBarriers_update.csv",sep=',',header=T,row.names=NULL)
#connectivity <- connectivity_raw %>%
#  filter(!is.na(downstreamLakeID))

#species_map(connectivity, get.locations=TRUE,username="sam.perrin",password="vegemite", maptype="terrain")

map_species <- function(connectivity, species = NA, get.locations=FALSE, username = NA, password = NA, maptype)
{
  if (get.locations == TRUE) {
    if (is.na(username)) {stop("You need to provide access credential to the database if you don't have coordinates already (ie. get.locations = TRUE)")}
    focal.lakes <- connectivity$locationID
    connect <- dbPool(
      drv = RPostgreSQL::PostgreSQL(),
      user=username,
      password=password,
      host = "vm-srv-finstad.vm.ntnu.no",
      dbname = "nofa",
      options="-c search_path=nofa" # set db schema from where to look
    )
    locations <- tbl(connect, "location")
    locations <- locations %>%
      filter(locationID %in% connectivity$locationID) %>%
      select(locationID, decimalLongitude, decimalLatitude) %>%
      collect()
    connectivity <- merge(connectivity, locations, all.x=TRUE, by="locationID")
    }



  left <- min(connectivity$decimalLongitude)
  bottom <- min(connectivity$decimalLatitude)
  right <- max(connectivity$decimalLongitude)
  top <- max(connectivity$decimalLatitude)
  width <- max(connectivity$decimalLongitude)-min(connectivity$decimalLongitude)
  depth <- max(connectivity$decimalLatitude)-min(connectivity$decimalLatitude)

  box_map <- get_map(location = c(left-width/4,bottom-depth/4,right+width/4,top+depth/4), maptype=maptype,zoom=5)
  if (is.na(species)) {
  d <- data.frame(lat=connectivity_loc$decimalLatitude, lon=connectivity_loc$decimalLongitude)
  p <- ggmap(box_map) + geom_point(data=d, aes(lon,lat),col='black')

  } else {
    d1 <- data.frame(lat=connectivity_loc[connectivity_loc[,species] == 1,]$decimalLatitude,
                    lon=connectivity_loc[connectivity_loc[,species] == 1,]$decimalLongitude)
    d0 <- data.frame(lat=connectivity_loc[connectivity_loc[,species] == 0,]$decimalLatitude,
                     lon=connectivity_loc[connectivity_loc[,species] == 0,]$decimalLongitude)
    p <- ggmap(box_map) + geom_point(data=d1, aes(lon,lat),col='blue') + geom_point(data=d0, aes(lon,lat),col='red')
  }
  p
  }
