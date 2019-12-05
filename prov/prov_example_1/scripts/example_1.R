#Provenance defined and a first example (from Lerner et al. in prep)

#source("libsource.R")

#prov.init(prov.dir="../prov", overwrite=TRUE) #initialize provenance collection

#prov.init(prov.dir"../prov", overwrite=FALSE) #Change overwrite if time-stamp desired

# Load the mtcars data set that comes with R
data (mtcars)

# All the cars
allCars.df <- mtcars

# Create separate data frames for each number of cylinders
cars4Cyl.df <- allCars.df[allCars.df$cyl == 4, ]
cars6Cyl.df <- allCars.df[allCars.df$cyl == 6, ]
cars8Cyl.df <- allCars.df[allCars.df$cyl == 8, ]

# Create a table with the average mpg for each # cylinders
cylinders = c(4, 6, 8)
mpg = c(mean(cars4Cyl.df$mpg), mean(cars6Cyl.df$mpg), mean(cars8Cyl.df$mpg))
cyl.vs.mpg.df <- data.frame (cylinders, mpg)

# Plot it
plot(cylinders, mpg)

#prov.save() #end provenance collection and save
#prov.quit()

#Get basic information on computing environment

#prov.summarize()

#

