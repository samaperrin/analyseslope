
#' This function extracts slope parameters for the connection between two lakes from the requisite database.
#'
#' @param parameters_of_interest A vector containing the names of the slope parameters you wish to extract. These can be extracted using the code found in the examples.
#' @param connectivity A matrix which should include the upstream and downstream lake you wish to evalutate and the presence or absence of your target fish in the upstream lake.
#' @param con The connection parameters set to connect to your database
#' @return The connectivity table with slope parameters adjoined.
#' @examples
#'
#'
#' # Extract parameters using the following code:
#' parameters <- colnames(tbl(database_folder, "example_connectivity_matrix") %>%
#' head() %>%
#'  collect())
#'
#' # Check parameters you need
#' names(parameters)
#'
#' parameters_of_interest <- parameters[12:15]
#'
#' slope_table <- extract_slope_params(parameters_of_interest, connectivity)
#' @export

# Define parameters of interest

#parameters_of_interest <- c("slope_max_max","slope_max", "slope_perc_90", "slope_perc_90_max")
#con <- connections$con

#extract_slope_params(parameters_of_interest,connectivity,con,include.length = TRUE)
# Upload the lake connections

# connectivity_raw <- read.csv(file="./slope_analysis/Data/sweden_connections.csv",sep=',',header=T)
# connectivity <- connectivity_raw %>%
#  filter(!is.na(Present))
# head(connectivity)

extract_slope_params <- function(parameters_of_interest, connectivity, con,include.length=FALSE) {
  # Create query

  # Create full list of parameters of interest, plus split into upstreams and downstreams
  full_poi <- c(paste("downstream",parameters_of_interest,sep="_"),paste("upstream",parameters_of_interest,sep="_"))
  downstream_poi <- full_poi[1:(length(full_poi)/2)]
  upstream_poi <- full_poi[(length(full_poi)/2+1):length(full_poi)]


  if (include.length==FALSE) {upstream_slopes_con <- "SELECT * FROM (SELECT from_lake,to_lake"
  } else {upstream_slopes_con <- "SELECT * FROM (SELECT from_lake,to_lake,total_stream_length"}

    for (i in full_poi) {
    upstream_slopes_con <- paste(upstream_slopes_con,",",i,sep="")
  }
  upstream_slopes_con <- paste(upstream_slopes_con," FROM temporary_sweden_connectivity.lake_connectivity) AS x",sep="")

  # Run query and remove duplicates
  print("Running query")
  upstream_slopes <- get_postgis_query(con,upstream_slopes_con)
  print("Query complete")
  upstream_slopes <- upstream_slopes[!duplicated(upstream_slopes),]

  # Separate into 2 columns
  connectivity$from_lake <- with(connectivity, ifelse(lakeID > downstreamLakeID, downstreamLakeID, lakeID))
  connectivity$to_lake <- with(connectivity, ifelse(lakeID < downstreamLakeID, downstreamLakeID, lakeID))

  relevant_connections <- merge(connectivity,upstream_slopes,by=c("from_lake","to_lake"),all.x=TRUE)

  upstreams_cons <- relevant_connections[relevant_connections$direction =="normal",][,c("locationID","downstreamLakeID","lakeID","Pike","Perch",downstream_poi)]
  downstreams_cons <- relevant_connections[relevant_connections$direction =="reverse",][,c("locationID","downstreamLakeID","lakeID","Pike","Perch",upstream_poi)]

  names(upstreams_cons) <- gsub("downstream_","",names(upstreams_cons))
  names(downstreams_cons) <- gsub("upstream_","",names(downstreams_cons))

  connectivity_slopes <- rbind(upstreams_cons,downstreams_cons)
  if (include.length==TRUE) {connectivity_slopes <- merge(connectivity_slopes,relevant_connections[,c("locationID","total_stream_length")])}

  connectivity_slopes <- connectivity_slopes[complete.cases(connectivity_slopes),]
  return(connectivity_slopes)
}

# Test function 1
# upstream_slopes_test <- extract_slope_params(parameters_of_interest, connectivity)

