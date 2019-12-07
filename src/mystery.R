#run prov on example 3a
#generates mystery errors

source("libsource.R") #loads prov and other needed libraries

prov.run("example_3a.R", prov.dir="../prov", overwrite=FALSE)

prov.summarize()
prov.visualize()
debug.init()
debug.warning.trace()
debug.warning.trace(1)