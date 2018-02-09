
##### ---- Function 1. Pull slope parameters from connectivity matrix and create data frame ###########

# Define parameters of interest

# parameters_of_interest <- c("upstream_lakes", "upstream_lakes_slope_max_max","upstream_lakes_slope_max", "upstream_lakes_slope_perc_90", "upstream_lakes_slope_perc_90_max")
# con already defined

# Upload the lake connections

# connectivity_raw <- read.csv(file="./slope_analysis/Data/sweden_connections.csv",sep=',',header=T)
# connectivity <- connectivity_raw %>% 
#  filter(!is.na(Present))
# head(connectivity)


extract_slope_params <- function(parameters_of_interest, connectivity) {
  # Create query
  
  upstream_slopes_con <- "SELECT * FROM (SELECT lakeid"
  for (i in parameters_of_interest) {
    upstream_slopes_con <- paste(upstream_slopes_con,", CAST(unnest(string_to_array(",i,", \', \')) AS numeric) AS ",i,sep="")
  }
  upstream_slopes_con <- paste(upstream_slopes_con," FROM temporary.connectivity_sweden_full WHERE upstream_lakes != \'\') AS x",sep="")
  
  # Run query and remove duplicates
  print("Running query")
  upstream_slopes <- get_postgis_query(con,upstream_slopes_con)
  print("Query complete")
  upstream_slopes <- upstream_slopes[!duplicated(upstream_slopes),]
  
  
  connectivity_addon <- as.data.frame(matrix(NA,ncol=length(parameters_of_interest)-1,nrow=nrow(connectivity)))
  colnames(connectivity_addon) <- gsub("upstream_lakes_","",parameters_of_interest[-1])
  
  for ( i in 1:nrow(connectivity)) {
    slope_1conx <- upstream_slopes %>%
      filter(lakeid == connectivity[i,"downstreamLakeID"] &
               upstream_lakes == connectivity[i,"lakeID"])
    connectivity_addon[i,1:ncol(connectivity_addon)] <- slope_1conx[3:(2+ncol(connectivity_addon))]
  }
  connectivity_slopes <- cbind(connectivity, connectivity_addon)
  
  
  
  return(connectivity_slopes)
}

# Test function 1
# upstream_slopes_test <- extract_slope_params(parameters_of_interest, connectivity)

