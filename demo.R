# load r package
library(plyr)
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)
library(ggthemes)
library(knitr)


# load the package I write for this demo
# to install this package
# use: install_github("blacksde/demo")
library(demo)

library(help = demo)
ls("package:demo")

# set the work directory
setwd("/Users/Kay/Documents/datasci/sde/insight_demo")

# we will analyze built-in dataset gDat inside package demo
# basic exploration of the data
str(gDat)
levels(gDat$country)
levels(gDat$continent)

kable(head(gDat,n = 15L))

# data aggregation to get the coef of linear model for each country
# my_lm_int is the function for linear regression in package demo
# the regression model is lifeExp ~ year, which is default in function my_lm_int
gDat.lm<-ddply(gDat, ~ country + continent, my_lm_int)

kable(head(gDat.lm))

# find 12 "interesting" countries
# "interesting" is defined to be worst fitted by linear model based on maxResid
country.int<-gDat.lm %>%
  filter(min_rank(desc(maxResid)) < 13 ) %>%
  arrange(continent,maxResid)

# show the dim of the country.int
dim(country.int)

# make a table
kable(country.int)

# percentage of continents within these 12 countries
cont.perc<-ggplot(country.int)+
	geom_bar(aes(x=factor(1), fill=continent))+
	xlab("") + ylab("")+
	coord_polar(theta="y")
cont.perc

ggsave("percentage of continents.png",cont.perc)

# a look at these nine countries and linear fitting
li.int<-ggplot(subset(gDat, country %in% country.int$country),
			 aes(x = year, y = lifeExp)) +
	geom_point(aes(color = continent))+
	stat_smooth(method = "lm", se = F) +
	facet_wrap(~ continent+country, ncol = 3) +
	geom_line()

li.int

ggsave("linear fitting of interesting countries.png",li.int)


# data aggregation to get linear spline for interesting countries
# li_spline is the function of linear spline inside the package demo
gDat.int<-gDat%>%
  subset(country %in% country.int$country)%>%
  droplevels%>%
  ddply( ~ country + continent, li_spline)

# add the abbreviation using built-in data-set abbr
gDat.int<-gDat.int%>%
	left_join(abbr)%>%
	droplevels

kable(head(gDat.int))
str(gDat.int)

# a look at the result of linear spline
li.spl<-ggplot(gDat.int, aes(x = year, y = lifeExp)) +
	geom_line(aes(color = continent)) +
  facet_wrap(~ continent+country, ncol = 3)

li.spl

ggsave("linear spline of interesting countries.png",li.spl)


jpeg("pic/foo%02d.jpg")

# plot the data after linear spline
# x axis is gdpPercap
# y axis is lifeExp
# the volume of the ball is the pop
for (yr in 1952:2007){
	base <-ggplot(subset(gDat.int, year == yr), aes(x = gdpPercap, y = lifeExp)) +
		scale_x_log10(limits = c(230, 63000)) +
		aes(fill = continent)
	plot<-base+geom_point(aes(size = sqrt(pop/pi)), pch = 21, show_guide = FALSE)+
		scale_size_continuous(range=c(1,20))+
		ylim(10,83)+
		annotate("text", x=15000, y=20, label = yr,size=30,color="grey")+
		geom_text(aes(label = Abbr),size = 5, aplha = 0.5, hjust = 1, vjust = 1)
	print(plot)
}

dev.off()

make_mov("insight.mp4", loc = "pic", pic = "foo", frame = 6)
