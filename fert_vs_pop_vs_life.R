# ***********************
# Author: Benjamin Tovar Cisneros
# Post: http://btovar.com/2015/03/mining-world-data/
# ***********************

# **********************
# load data
# **********************

# load fertility data
fertility <- read.delim("fertility.csv",header=TRUE,sep=",")
# load life expectancy data
life_expectancy <- read.delim("life_expectancy.csv",header=TRUE,sep=",")
# load annotation data
annotation <- read.delim("world_regions.txt",header=TRUE,sep="\t")
rownames(annotation) <- annotation$Country_Code
# load population data
population <- read.delim("world_population.txt",header=TRUE,sep="\t")

# **********************
# add annotation data
# **********************

# fertility data
fertility_annot <- annotation[as.character(fertility$Country_Code),c("Region","Region_ID")]
fertility <- cbind(fertility,fertility_annot)

# fertility data
life_expectancy_annot <- annotation[as.character(life_expectancy$Country_Code),c("Region","Region_ID")]
life_expectancy <- cbind(life_expectancy,life_expectancy_annot)

# population data
population_annot <- annotation[as.character(population$Country_Code),c("Region","Region_ID")]
population <- cbind(population,population_annot)


# **********************
# filter data
# **********************

# use country data only and only if it has all data
# (all years, in all databases)

fertility <- fertility[complete.cases(fertility),]
life_expectancy <- life_expectancy[complete.cases(life_expectancy),]
population <- population[complete.cases(population),]

# **********************
# reshape data (to be compatible with ggplot format)
# **********************

# example of melt function 
library(reshape)

# change the format of fertility data
fertility_m <- melt(fertility, id=c("Country_Name","Country_Code","Region","Region_ID"))
fertility_m$variable <- as.numeric(gsub("X","",fertility_m$variable))

# change the format of life_expectancy data
life_expectancy_m <- melt(life_expectancy, id=c("Country_Name","Country_Code","Region","Region_ID"))
life_expectancy_m$variable <- as.numeric(gsub("X","",life_expectancy_m$variable))

# change the format of population data
population_m <- melt(population, id=c("Country_Name","Country_Code","Region","Region_ID"))
population_m$variable <- as.numeric(gsub("X","",population_m$variable))


# **********************
# explore data: Values in all countries
# **********************

library(ggplot2)
library(gridExtra)

  ggplot(fertility_m) +
    aes(x=as.factor(variable),y=value) + 
    geom_boxplot(aes(colour=as.factor(variable),fill=as.factor(variable)),alpha=0.6) + 
    theme(legend.position="none",axis.text.x = element_text(angle=90, vjust=1)) + 
    labs(title="Fertility per year: all countries",x="Year",y="fertility")

  ggplot(life_expectancy_m) +
    aes(x=as.factor(variable),y=value) + 
    geom_boxplot(aes(colour=as.factor(variable),fill=as.factor(variable)),alpha=0.6) + 
    theme(legend.position="none",axis.text.x = element_text(angle=90, vjust=1)) + 
    labs(title="Life expectancy per year: all countries",x="Year",y="Life expectancy")


# **********************
# values in all regions
# **********************

  ggplot(fertility_m) +
    aes(x=Region_ID,y=value) + 
    geom_boxplot(aes(colour=Region,fill=Region),alpha=0.6) + 
    theme(legend.position="right",axis.text.x = element_text(angle=90, vjust=1)) + 
    labs(title="Fertility per region: all years",x="Region",y="Fertility")

  ggplot(life_expectancy_m) +
    aes(x=Region_ID,y=value) + 
    geom_boxplot(aes(colour=Region,fill=Region),alpha=0.6) + 
    theme(legend.position="right",axis.text.x = element_text(angle=90, vjust=1)) + 
    labs(title="Life expectancy per region: all years",x="Region",y="Life expectancy")

# **********************
# regressions and correlations 
# **********************

# merge the data into one dataset
# computing the mean value per year and per country

# extract shared data shared by the 3 databases
years <- paste("X",1961:2012,sep="")
# set the country names shared in the 3 databases
countries_all <-  c(as.character(fertility$Country_Code),
              as.character(life_expectancy$Country_Code),
              as.character(population$Country_Code))
countries_table <- table(countries_all)
index <- countries_table == 3
countries_unique <- names(countries_table)[index]

# > sum(table(countries)==3)
# [1] 186

# extract subdatabases
M <- list(fertility=fertility[countries_unique,years],
          life_expectancy=life_expectancy[countries_unique,years],
          population_norm=log10(population[countries_unique,years]))

# extract mean per country and per year
fertility_mean_per_country <- rowMeans(M$fertility)
life_expectancy_mean_per_country <- rowMeans(M$life_expectancy)
population_mean_per_country <- rowMeans(M$population)

mean_pear_country <- data.frame(fertility=fertility_mean_per_country,
                            life_expectancy=life_expectancy_mean_per_country,
                            population=population_mean_per_country)


# load library
library("GGally")

  ggpairs(mean_pear_country, title = "Pairwise comparison in all regions",
    lower=list(continuous="smooth",params=c(colour="#009688",alpha=0.7)),
    diag=list(continuous="density",params=c(colour="red",alpha=1)), 
    upper=list(params=list(size=10)), 
    axisLabels='show')

