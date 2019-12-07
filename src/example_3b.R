#example 3b - Species occurrence data
#data for Polyergus lucidus (a slave-making ant)
#An example obtaining data from GBIF and iNaturalist
#Use provenance tools to determing why it doesn't work and fix it.

needlibs <- c("spocc", "scrubr", "mapr")

for(i in 1:length(needlibs)) {
	if(!is.installed(needlibs[i])){
		install.packages(needlibs[i])
	}
	require(needlibs[i], character.only=TRUE)
}


Polyergus <- occ(query="Polyergus lucidus", from=c("gbif", "inat"), has_coords=TRUE)

Polyergus <- fixnames(Polyergus, how="query")

Polyergus.df <- droplevels(occ2df(Polyergus))

map_plot(Polyergus)

