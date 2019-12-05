library(spocc)

# One example of how students pulled data from vertnet
bell <- occ(query = "Molothrus ater obscurus", from = "vertnet", vertnetopts = list(stateprovince="california", county = "San Diego"))

# The occ function returns a list of 9 elements for all possible data bases the data could have been pulled from (e.g., vertnet, GBIF). Since we specified to just pull out vernet data, the other slots in this list for the other databases are there, but do not contain any data. The code below pulls out just the vertnet data.
bell <- bell$vertnet$data$Molothrus_ater_obscurus  

# check the dimenions of the data
dim(bell)