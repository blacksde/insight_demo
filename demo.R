# load r package
library(plyr)
suppressPackageStartupMessages(library(dplyr))
library(ggplot2)
library(ggthemes)
library(knitr)

# load the data
gDat <- read.delim("gapminderDataFiveYear.txt")

# basic exploration of the data
str(gDat)
levels(gDat$country)
levels(gDat$continent)

head(gDat,n = 20L)

# create funtion for linear regression
my.lm.int <- function(x) {
  jFit <- lm(lifeExp ~ I(year - min(x$year)), x)
  jCoef <- coef(jFit)
  names(jCoef) <- NULL
  return(c(intercept = jCoef[1],
           slope = jCoef[2]))
}

# data aggregation to get the coef of linear model for each country
gDat.lm<-ddply(gDat, ~ country + continent, my.lm.int)

head(gDat.lm)

# finde "interesting" countries
# "interesting" is defined to be with top 2 slope within each continent
country.int<-gDat.lm %>%
  group_by(continent) %>%
  filter(min_rank(desc(slope)) < 3 ) %>%
  arrange(continent,slope)

# show the dim of the country.int
dim(country.int)

# make a table
kable(country.int)
  

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

# reoder the data and add the abbreviation
gDat.int<-gDat.int%>%
  mutate(country = reorder(country, continent))%>%
  mutate(abbr = revalue(country,
                               c("Tunisia" = "Tn",
                                 "Libya" = "Ly",
                                 "Honduras" = "Hn",
                                 "Nicaragua" = "Ni",
                                 "Vietnam" = "Vn",
                                 "Oman" = "Om",
                                 "Bosnia and Herzegovina" = "Ba",
                                 "Turkey" = "Tr",
                                 "New Zealand" = "NZ",
                                 "Australia" = "Au")))
head(gDat.int)
str(gDat.int)

ggplot(gDat.int, aes(x = year, y = lifeExp)) + 
  geom_point(aes(color = continent)) + 
  facet_wrap(~ continent+country, ncol = 2) + 
  geom_line()

jpeg("pic/foo%02d.jpg")
drawit=function(jYear){
  q <-ggplot(subset(gDat.int, year == jYear), aes(x = gdpPercap, y = lifeExp)) +
    scale_x_log10(limits = c(230, 63000)) +
    aes(fill = continent) 
  q+geom_point(aes(size = sqrt(pop/pi)), pch = 21, show_guide = FALSE)+
    scale_size_continuous(range=c(1,20))+
    ylim(10,83)+
    annotate("text", x=15000, y=20, label = jYear,size=30,color="grey")+
    geom_text(aes(label = abbr),size = 5, aplha = 0.5, hjust = 1, vjust = 1)
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
