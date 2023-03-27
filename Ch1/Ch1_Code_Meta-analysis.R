#load ggplot2 library
library(ggplot2)

#load meta-analysis data
ma.data <- read.csv("Ch1_Data_Meta-analysis.csv")

#make subset dataframe with only relevant variables
ma.subset <- ma.data[c(4:12,32:33,38:39)]

#make subset dataframe including only 'all megafauna' 
all <- ma.subset[which(ma.subset$Taxon=='All megafauna'), ]

#make subset dataframes for each category of the 'Recovery potential' variable
high <- ma.subset[which(ma.subset$Recovery.potential=='High'), ]
medium <- ma.subset[which(ma.subset$Recovery.potential=='Medium'& 
                            !ma.subset$Phylum=='All megafauna' &
                            !ma.subset$Phylum=='Brachiopoda'), ]
low <- ma.subset[which(ma.subset$Recovery.potential=='Low'), ]


#plot SMD of TOTAL megafauna between where trawling has ceased and where trawling is still active
#using hedge's g values
ggplot(all, aes(time.since.TC, g.TA, colour = Seamounts)) +
  geom_point(shape = 17, size = 3) +
  geom_errorbar(aes(ymin = g.TA-se.TA, ymax = g.TA+se.TA)) +
  xlab("Time since trawling ceased (y)") +
  ylab("Standardised mean difference") +
  ylim(-1.5,1.5) +
  geom_hline(yintercept = 0)  + 
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

#exploring general trends
#plot smd for taxa with high recovery potential

ggplot(high, aes(time.since.TC,g.TA,colour = Taxon, shape = Seamounts)) +
  geom_point(size = 2) +
  ylim(-0.75,0.75) + 
  geom_hline(yintercept = 0)  + theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  )


#separate above plot by phylum
ggplot(high, aes(time.since.TC,g.TA,colour = Taxon, shape = Seamounts)) +
  geom_point() +
  geom_errorbar(aes(ymin = g.TA-se.TA, ymax = g.TA+se.TA)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~Phylum) + theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  )


#plot smd for all taxa with medium recovery potential
#phylum brachiopoda was excluded due to lack of data
ggplot(medium, aes(time.since.TC,g.TA,colour = Taxon, shape = Seamounts)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0)  +
  ylim(-1,1) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggplot(medium, aes(time.since.TC,g.TA,colour = Taxon, shape = Seamounts)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = g.TA-se.TA, ymax = g.TA+se.TA)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~Phylum) + theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggplot(medium, aes(time.since.TC,g.TA, colour = Seamounts)) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = g.TA-se.TA, ymax = g.TA+se.TA)) +
  facet_wrap(~Taxon) + theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

#plot smd for all taxa with low recovery potential

ggplot(low, aes(time.since.TC,g.TA,colour = Taxon, shape = Seamounts)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = g.TA-se.TA, ymax = g.TA+se.TA)) +
  ylim(-1.25,1.25) + 
  geom_hline(yintercept = 0)  + theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggplot(low, aes(time.since.TC,g.TA, shape = Seamounts)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 1.5, colour = "brown3") +
  geom_errorbar(colour = "brown3", aes(ymin = g.TA-se.TA, ymax = g.TA+se.TA)) +
  facet_wrap(~Taxon) + theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

#plot all taxa, coded by recovery potential
ggplot(ma.data, aes(time.since.TC, g.TA, colour = Recovery.potential, shape = Seamounts)) + 
  geom_point(size = 2) + 
  ylim(-1,1) +
  geom_hline(yintercept = 0) + 
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(), 
    panel.grid.major.x = element_blank()
  )


