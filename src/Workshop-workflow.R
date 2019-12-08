#Workflow

#clean up work space, source packages and libraries

source("libsource.R")

#Collect provenance on example 1 (uses rdtLite, provSummarizeR, provViz)

prov.run("example_1.R", prov.dir="../prov")


#Debug example 2a

prov.run("example_2a.R", prov.dir="../prov")
prov.summarize()
prov.visualize()

debug.init() 	#if prov.run(...) has already been run
			#otherwise, debug.init("<script.R>", dir=NULL)
			#or debug.init("<...>/prov.json")
debug.warning.trace()
debug.warning.trace(1)
debug.error.trace()
debug.error.trace(stack.overflow=TRUE)

debug.lineage("z")

#Debug example 2b

prov.run("example_2b.R", prov.dir="../prov")
prov.summarize()
prov.visualize()

debug.init() 	#if prov.run(...) has already been run
			#otherwise, debug.init("<script.R>", dir=NULL)
			#or debug.init("<...>/prov.json")

debug.variable.type()
debug.variable.type("x")

#Simulation modeling example 3

prov.run("example_3.R", prov.dir="../prov")
prov.summarize()
prov.visualize()

#Repeating published research example 3a

prov.run("example_3.R", prov.dir="../prov")
prov.summarize()
prov.visualize()

#Working with species occurrence data example 3b
#Development version of RDataTracker and contributing to the effort

prov.run("example_3.R", prov.dir="../prov")
