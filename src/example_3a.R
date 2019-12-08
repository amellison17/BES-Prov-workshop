# redraw of nutrient profiles with food enrichment
# data from Wakefield et al. 2005
# 25 June 2019
# NJG

# Load Libraries ----------------------------------------------------------
library(tidyverse)

# Source Files ------------------------------------------------------------
# NA

# Read Data ---------------------------------------------------------------
#metadata is at https://dx.doi.org/10.6073/pasta/6591267269a71eadc859e2497e6643cf

df <- read.csv(url("https://harvardforest.fas.harvard.edu/data/p32/hf328/hf328-01-sapu-prey-additions-2002.csv"))

# -------------------------------------------------------------------------
theme_set(theme_bw())

# Read in data and subset -------------------------------------------------
df <- df[,c(1:3,18:22)]
df <- rename(df,Ca.ppm=ca.ppm)
df <- rename(df,Percent.N=pct.n)
df <- rename(df, K.ppm=k.ppm)
df <- rename(df, Mg.ppm=mg.ppm)
df <- rename(df, P.ppm=p.ppm)

head(df)
# Convert to long form ----------------------------------------------------
df_long <- gather(data=df,
                  key="Nutrient",
                  value="Concentration",
                  4:8)


# Create ggplot facet figure ----------------------------------------------
Fig_Stoich <- ggplot(data=df_long,
                     aes(x=as.factor(fliesperwk),
                         y=Concentration)) +
                       geom_boxplot() +
                       facet_grid(Nutrient~., scales="free") +
                       xlab("Flies added per week")

plot(Fig_Stoich)
cairo_pdf("../graphics/Fig_Stoich.pdf", height=7, width=5)
plot(Fig_Stoich)
dev.off()


#A simple ANOVA on 


p.model1 <- aov(Concentration~fliesperwk, data=df_long[df_long$Nutrient=="P.ppm",])
summary(p.model1)
par(mfrow=c(2,2))
plot(p.model1)