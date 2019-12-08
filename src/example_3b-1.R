#example 3b-1 - Species occurrence data
#data for Polyergus lucidus (a slave-making ant)
#Working example obtaining data from GBIF


Polyergus <- occ(query="Polyergus lucidus", from="gbif",has_coords = TRUE)

Polyergus <- fixnames(Polyergus, how = 'query')

P.map <- map_plot(Polyergus)



