# load r package
library(plyr)
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)
library(ggthemes)
library(knitr)

# set the work directory
setwd("/Users/Kay/Documents/datasci/sde/insight_demo")

# load the data
gDat <- read.delim("gapminderDataFiveYear.txt")

# basic exploration of the data
str(gDat)
levels(gDat$country)
levels(gDat$continent)

head(gDat,n = 15L)

# create funtion for linear regression
my.lm.int <- function(x) {
	jFit <- lm(lifeExp ~ I(year - min(x$year)), x)
	jCoef <- coef(jFit)
	names(jCoef) <- NULL
	return(c(intercept = jCoef[1],
					 slope = jCoef[2],
					 maxResid = max(abs(resid(jFit)))/summary(jFit)$sigma))
}

# data aggregation to get the coef of linear model for each country
gDat.lm<-ddply(gDat, ~ country + continent, my.lm.int)

head(gDat.lm)

# finde 12 "interesting" countries
# "interesting" is defined to be worst fitted by linear model based on maxResid
country.int<-gDat.lm %>%
  filter(min_rank(desc(maxResid)) < 13 ) %>%
  arrange(continent,maxResid)

# show the dim of the country.int
dim(country.int)

# make a table
kable(country.int)

# a look at these nine countries and linear fitting
ggplot(country.int)+
	geom_bar(aes(x=factor(1), fill=continent))+
	xlab("") + ylab("")+
	coord_polar(theta="y")

ggplot(subset(gDat, country %in% country.int$country), aes(x = year, y = lifeExp)) +
	geom_point(aes(color = continent))+
	stat_smooth(method = "lm", se = F) + 
	facet_wrap(~ continent+country, ncol = 3) + 
	geom_line()

# create function for linear spline
li.spline<-function(x, yr = 1952:2006){
  index<-floor((yr-1952)/5)
  remainder<-(yr-1952)%%5
  pop<-c((x$pop[(index+2)]-x$pop[(index+1)])/5*remainder+x$pop[(index+1)],
         x$pop[length(x$pop)])
  lifeExp<-c((x$lifeExp[(index+2)]-x$lifeExp[(index+1)])/5*remainder+x$lifeExp[(index+1)],
             x$lifeExp[length(x$lifeExp)])
  gdpPercap<-c((x$gdpPercap[(index+2)]-x$gdpPercap[(index+1)])/5*remainder+
                 x$gdpPercap[(index+1)],
               x$gdpPercap[length(x$gdpPercap)])
  return(data.frame(year = c(yr,2007),pop = pop, lifeExp = lifeExp, 
                    gdpPercap = gdpPercap))
}

# data aggregation to get linear spline for interesting countries
gDat.int<-gDat%>%
  subset(country %in% country.int$country)%>%
  droplevels%>%
  ddply( ~ country + continent, li.spline)

# add the abbreviation
abbr<-read.csv("abbr.csv",sep = ";")
gDat.int<-gDat.int%>%
	left_join(abbr)%>%
	droplevels
  
head(gDat.int)
str(gDat.int)

ggplot(gDat.int, aes(x = year, y = lifeExp)) + 
	geom_line(aes(color = continent)) + 
  facet_wrap(~ continent+country, ncol = 3)
  

jpeg("pic/foo%02d.jpg")
drawit=function(jYear){
  q <-ggplot(subset(gDat.int, year == jYear), aes(x = gdpPercap, y = lifeExp)) +
    scale_x_log10(limits = c(230, 63000)) +
    aes(fill = continent) 
  q+geom_point(aes(size = sqrt(pop/pi)), pch = 21, show_guide = FALSE)+
    scale_size_continuous(range=c(1,20))+
    ylim(10,83)+
    annotate("text", x=15000, y=20, label = jYear,size=30,color="grey")+
    geom_text(aes(label = Abbr),size = 5, aplha = 0.5, hjust = 1, vjust = 1)
}
finaldraw=function(year = 1952:2007){
  for (yr in year){
    print(drawit(yr))
  }
}
finaldraw()
dev.off()

make.mov <- function(){
  system("ffmpeg -r 4  -i pic/foo%02d.jpg data.mp4")
}


make.mov()
