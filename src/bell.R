# Another example of how students pulled data from vertnet
bell2 <- occ(query = "Molothrus ater obscurus", from = "vertnet")

# The occ function returns a list of 9 elements for all possible data bases the data could have been pulled from (e.g., vertnet, GBIF). Since we specified to just pull out vernet data, the other slots in this list for the other databases are there, but do not contain any data. The code below pulls out just the vertnet data.
bell2sd <- bell2$vertnet$data$Molothrus_ater_obscurus 

# Filter out San Diego county
bell2sd <- filter(bell2, county=='San Diego')

# check the dimenions of the data
dim(bell)