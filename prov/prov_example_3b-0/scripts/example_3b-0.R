#example 3b-0 - Species occurrence data
#data for Polyergus lucidus (a slave-making ant)
#Working example obtaining data from GBIF

Polyergus <- occ(query="Polyergus lucidus", from="gbif",has_coords = TRUE)

Polyergus.df = data.frame(Polyergus$gbif$data$Polyergus_lucidus)

worldmap <- map_data('world')

Polyergus.map <- 
ggplot() +
  geom_polygon(data=worldmap, fill="white", color="black", aes(x=long, y=lat, group=group)) +
  geom_point(data=Polyergus.df, color='red',aes(x=longitude,y=latitude)) +
  coord_quickmap()

ggsave("../graphics/Polyergus.png")
