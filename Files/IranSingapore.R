## @knitr dataSection
library(intsvy)
library(tidyverse)
library(highcharter)
library(plotly)
library(ISOcodes)
library(Hmisc)
library(reshape2)
library(WDI)

bsgTimss15 <- read_rds("../2015/bsg.rds")
asgTimss15 <- read_rds("../2015/asg.rds")
bsgTimss03 <- read_rds("../2003/bsg.rds")
asgTimss03 <- read_rds("../2003/asg.rds")

bsgTimss15 %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(mathScore = mean(bsmmat01:bsmmat05),scienceScore = mean(bsssci01:bsssci05)) %>% 
  select(idcntry,idschool,idstud,itsex,mathScore,scienceScore) -> student.stat8th15

asgTimss15 %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(mathScore = mean(asmmat01:asmmat05),scienceScore = mean(asssci01:asssci05)) %>% 
  select(idcntry,idschool,idstud,itsex,mathScore,scienceScore) -> student.stat4th15

bsgTimss03 %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(mathScore = mean(bsmmat01:bsmmat05),scienceScore = mean(bsssci01:bsssci05)) %>% 
  select(idcntry,idschool,idstud,itsex,mathScore,scienceScore) -> student.stat8th03

asgTimss03 %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(mathScore = mean(asmmat01:asmmat05),scienceScore = mean(asssci01:asssci05)) %>% 
  select(idcntry,idschool,idstud,itsex,mathScore,scienceScore) -> student.stat4th03


bsgTimss15 %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(Algebra = mean(bsmalg01:bsmalg05),`Data And Chance` = mean(bsmdat01:bsmdat05), 
         Number= mean(bsmnum01:bsmnum05), Geometry = mean(bsmgeo01:bsmgeo05), Chemistry = mean(bssche01:bssche05),
         `Earth Science` = mean(bssear01:bssear05), Biology = mean(bssbio01:bssbio05), Physics = mean(bssphy01:bssphy05)) %>% 
  select(idcntry,idschool,idstud,itsex,Algebra,`Data And Chance`,
         Number,Geometry,Chemistry,`Earth Science`,Biology,Physics) %>% 
  filter(idcntry %in% c(364,702)) -> Timss8thCompleteStat

asgTimss15 %>% 
  group_by(idcntry,idschool,idstud) %>% 
  mutate(`Data Display` = mean(asmdat01:asmdat05), Number= mean(asmnum01:asmnum05),
         Geometry = mean(asmgeo01:asmgeo05), `Earth Science` = mean(assear01:assear05),
         Physics = mean(assphy01:assphy05), `Life Science` = mean(asslif01:asslif05)) %>% 
  select(idcntry,idschool,idstud,itsex,`Data Display`,
         Number,Geometry,`Earth Science`,`Life Science`,Physics) %>% 
  filter(idcntry %in% c(364,702)) -> Timss4thCompleteStat

Timss4thCompleteStat %>% 
  ungroup() %>% 
  select(idcntry,`Data Display`:Geometry) %>% 
  gather("Subscale","Score",2:4) -> completeStat4thMath

Timss4thCompleteStat %>% 
  ungroup() %>% 
  select(idcntry,`Earth Science`:Physics) %>% 
  gather("Subscale","Score",2:4) -> completeStat4thScience

Timss8thCompleteStat %>% 
  ungroup() %>% 
  select(idcntry,Algebra:Geometry) %>% 
  gather("Subscale","Score",2:5) -> completeStat8thMath

Timss8thCompleteStat %>% 
  ungroup() %>% 
  select(idcntry,Chemistry:Physics) %>% 
  gather("Subscale","Score",2:5) -> completeStat8thScience

completeStat4thMath %>% 
  group_by(idcntry,Subscale) %>% 
  dplyr::summarise(Average = round(mean(Score),0)) -> completeStat4thMathAvg

completeStat4thScience %>% 
  group_by(idcntry,Subscale) %>% 
  dplyr::summarise(Average = round(mean(Score),0)) -> completeStat4thScienceAvg

completeStat8thMath %>% 
  group_by(idcntry,Subscale) %>% 
  dplyr::summarise(Average = round(mean(Score),0)) -> completeStat8thMathAvg

completeStat8thScience %>% 
  group_by(idcntry,Subscale) %>% 
  dplyr::summarise(Average = round(mean(Score),0)) -> completeStat8thScienceAvg

student.stat4th03 %>% 
  ungroup() %>% 
  filter(idcntry %in% c(364,702)) %>% 
  group_by(idcntry) %>% 
  dplyr::summarise(MathAverage = mean(mathScore), ScienceAverage = mean(scienceScore)) -> iran4th03
iran4th03$year <- 2003

student.stat4th15 %>% 
  ungroup() %>% 
  filter(idcntry %in% c(364,702)) %>% 
  group_by(idcntry) %>% 
  dplyr::summarise(MathAverage = mean(mathScore), ScienceAverage = mean(scienceScore)) -> iran4th15
iran4th15$year <- 2015


student.stat8th03 %>% 
  ungroup() %>% 
  filter(idcntry %in% c(364,702)) %>% 
  group_by(idcntry) %>% 
  dplyr::summarise(MathAverage = mean(mathScore), ScienceAverage = mean(scienceScore)) -> iran8th03
iran8th03$year <- 2003

student.stat8th15 %>% 
  ungroup() %>% 
  filter(idcntry %in% c(364,702)) %>% 
  group_by(idcntry) %>% 
  dplyr::summarise(MathAverage = mean(mathScore), ScienceAverage = mean(scienceScore)) -> iran8th15
iran8th15$year <- 2015


rbind(iran4th03,iran4th15) %>% 
  round() -> scoreChange4th

rbind(iran8th03,iran8th15) %>% 
  round() -> scoreChange8th

scoreChange4th %>% 
  gather("Subject","Average",2:3) %>% 
  arrange(idcntry) -> scoreChange4th

scoreChange8th %>% 
  gather("Subject","Average",2:3) %>% 
  arrange(idcntry) -> scoreChange8th

student.stat4th15 %>% 
  mutate(mathScore = round(mathScore),scienceScore = round(scienceScore)) -> student.stat4th15

student.stat8th15 %>% 
  mutate(mathScore = round(mathScore),scienceScore = round(scienceScore)) -> student.stat8th15


urbanPop <- WDI(country = "all", indicator = "SP.URB.TOTL.IN.ZS",start = 2011,end = 2014) %>% 
  filter(iso2c %in% c("IR","SG")) %>% 
  select(Country = country, urbanPop = SP.URB.TOTL.IN.ZS, year)

urbanPop %>% mutate(urbanPop = round(urbanPop)) -> urbanPop


population <- WDI(country = "all", indicator = "SP.POP.TOTL",start = 2011,end = 2014) %>% 
  filter(iso2c %in% c("IR","SG")) %>% 
  select(Country = country, Population = SP.POP.TOTL, year)

population %>%  mutate(Population = round(Population/1000000,1)) -> population

#####


popDensity <- WDI(country = "all", indicator = "EN.POP.DNST",start = 2011,end = 2014) %>% 
  filter(iso2c %in% c("IR","SG")) %>% 
  select(Country = country, popDensity = EN.POP.DNST, year)

popDensity %>%  mutate(popDensity = round(popDensity)) -> popDensity

#####
LE <- WDI(country = "all", indicator = "SP.DYN.LE00.IN",start = 2011,end = 2014) %>% 
  filter(iso2c %in% c("IR","SG")) %>% 
  select(Country = country, LE = SP.DYN.LE00.IN, year)

LE %>%  mutate(LE = round(LE,1)) -> LE

#####
infantMR <- WDI(country = "all", indicator = "SP.DYN.IMRT.IN",start = 2011,end = 2014) %>% 
  filter(iso2c %in% c("IR","SG")) %>% 
  select(Country = country, infantMR = SP.DYN.IMRT.IN, year)

infantMR %>%  mutate(infantMR = round(infantMR,1)) -> infantMR

#####
educationExpenditure <- WDI(country = "all", indicator = "SE.XPD.TOTL.GB.ZS",start = 2011,end = 2014) %>% 
  filter(iso2c %in% c("IR","SG")) %>% 
  select(Country = country, educationExpenditure = SE.XPD.TOTL.GB.ZS, year)

educationExpenditure %>%  mutate(educationExpenditure = round(educationExpenditure,1)) -> educationExpenditure
##total (% of government expenditure)

#####
LR <- WDI(country = "all", indicator = "SE.ADT.1524.LT.ZS",start = 2012,end = 2014) %>% 
  filter(iso2c %in% c("IR","SG")) %>% 
  select(Country = country, LR = SE.ADT.1524.LT.ZS, year)

LR %>%  mutate(LR = round(LR,1)) -> LR
#Literacy rate, youth total (% of people ages 15-24)



## @knitr lr
LR %>% 
  hchart(type = "line", hcaes(x = year , y = LR, group = Country)) %>% 
  hc_title(text = "Literacy Rate, Youth Total (% Of People Ages 15-24)") %>% 
  hc_xAxis(categories = urbanPop$year, title = list(text = "Year"),tickLength = 20,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Percentage"),tickLength = 5,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_flat())

## @knitr le
LE %>% 
  hchart(type = "line", hcaes(x = year , y = LE, group = Country)) %>% 
  hc_title(text = "Life Expectancy At Birth (Years)") %>% 
  hc_xAxis(categories = urbanPop$year, title = list(text = "Year"),tickLength = 20,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = ""),tickLength = 5,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_flat())

## @knitr urbanPop
urbanPop %>% 
  hchart(type = "line", hcaes(x = year , y = urbanPop, group = Country)) %>% 
  hc_title(text = "Urban Population (% Of Total)") %>% 
  hc_xAxis(categories = urbanPop$year, title = list(text = "Year",style = list(color = "yellow")),tickColor = "white",tickLength = 20,tickWidth = 2,
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Percentage",style = list(color = "yellow")),tickColor = "White",tickLength = 5,tickWidth = 2,
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_gridlight())

## @knitr population
population %>% 
  hchart(type = "line", hcaes(x = year , y = Population, group = Country)) %>% 
  hc_title(text = "Population (In Millions)") %>% 
  hc_xAxis(categories = urbanPop$year, title = list(text = "Year"),tickLength = 20,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Millions"),tickLength = 5,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_elementary())


## @knitr educationExpenditure
educationExpenditure %>% 
  hchart(type = "line", hcaes(x = year , y = educationExpenditure, group = Country)) %>% 
  hc_title(text = "Government Expenditure On Education(% Of Government Expenditure)") %>% 
  hc_xAxis(categories = urbanPop$year, title = list(text = "Year"),tickLength = 20,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Millions"),tickLength = 5,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_elementary())

## @knitr popDensity
popDensity %>% 
  hchart(type = "line", hcaes(x = year , y = popDensity, group = Country)) %>% 
  hc_title(text = "Population Density (People Per Square Kilometre)") %>% 
  hc_xAxis(categories = urbanPop$year, title = list(text = "Year"),tickLength = 20,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Density"),tickLength = 5,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_elementary())


## @knitr infantMR
infantMR %>% 
  hchart(type = "line", hcaes(x = year , y = infantMR, group = Country)) %>% 
  hc_title(text = "Infant Mortality Rate (Per 1,000 Live Births)") %>% 
  hc_xAxis(categories = urbanPop$year, title = list(text = "Year"),tickLength = 20,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = ""),tickLength = 5,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_gridlight())


## @knitr iranvsSingapore
highchart() %>% 
  hc_chart(polar = TRUE, type = "line") %>% 
  hc_title(text = "Iran vs. Singapore: 4th Grade Math Subscales") %>% 
  hc_xAxis(categories = completeStat4thMathAvg$Subscale,
           tickmarkPlacement = "on",
           lineWidth = 0) %>% 
  hc_yAxis(gridLineInterpolation = "polygon",
           lineWidth = 0,
           min = 0) %>% 
  hc_series(
    list(
      name = "Iran Performance",
      data = completeStat4thMathAvg %>% filter(idcntry == 364) %>% .$Average,
      pointPlacement = "on"
    ),
    list(
      name = "Singapore Performance",
      data = completeStat4thMathAvg %>% filter(idcntry == 702) %>% .$Average,
      pointPlacement = "on"
    )
  ) %>% 
  hc_add_theme(hc_theme_flat())

## @knitr iranvsSingapore2
highchart() %>% 
  hc_chart(polar = TRUE, type = "line") %>% 
  hc_title(text = "Iran vs. Singapore: 4th Grade Science Subscales") %>% 
  hc_xAxis(categories = completeStat4thScienceAvg$Subscale,
           tickmarkPlacement = "on",
           lineWidth = 0) %>% 
  hc_yAxis(gridLineInterpolation = "polygon",
           lineWidth = 0,
           min = 0,max = 600,tickInterval = 100) %>% 
  hc_series(
    list(
      name = "Iran Performance",
      data = completeStat4thScienceAvg %>% filter(idcntry == 364) %>% .$Average,
      pointPlacement = "on"
    ),
    list(
      name = "Singapore Performance",
      data = completeStat4thScienceAvg %>% filter(idcntry == 702) %>% .$Average,
      pointPlacement = "on"
    )
  ) %>% 
  hc_add_theme(hc_theme_elementary())

## @knitr iranvsSingapore3
highchart() %>% 
  hc_chart(polar = TRUE, type = "line") %>% 
  hc_title(text = "Iran vs. Singapore: 8th Grade Math Subscales") %>% 
  hc_xAxis(categories = completeStat8thMathAvg$Subscale,
           tickmarkPlacement = "on",
           lineWidth = 0) %>% 
  hc_yAxis(gridLineInterpolation = "polygon",
           lineWidth = 0,
           min = 0) %>% 
  hc_series(
    list(
      name = "Iran Performance",
      data = completeStat8thMathAvg %>% filter(idcntry == 364) %>% .$Average,
      pointPlacement = "on"
    ),
    list(
      name = "Singapore Performance",
      data = completeStat8thMathAvg %>% filter(idcntry == 702) %>% .$Average,
      pointPlacement = "on"
    )
  ) %>% 
  hc_add_theme(hc_theme_flat())


## @knitr iranvsSingapore4
highchart() %>% 
  hc_chart(polar = TRUE, type = "line") %>% 
  hc_title(text = "Iran vs. Singapore: 8th Grade Science Subscales") %>% 
  hc_xAxis(categories = completeStat8thScienceAvg$Subscale,
           tickmarkPlacement = "on",
           lineWidth = 0) %>% 
  hc_yAxis(gridLineInterpolation = "polygon",
           lineWidth = 0,
           min = 0) %>% 
  hc_series(
    list(
      name = "Iran Performance",
      data = completeStat8thScienceAvg %>% filter(idcntry == 364) %>% .$Average,
      pointPlacement = "on"
    ),
    list(
      name = "Singapore Performance",
      data = completeStat8thScienceAvg %>% filter(idcntry == 702) %>% .$Average,
      pointPlacement = "On"
    )
  ) %>% 
  hc_add_theme(hc_theme_elementary())

## @knitr avgChange
highchart() %>% 
  hc_chart(inverted = TRUE, type = "columnrange") %>% 
  hc_title(text = "Average Score Change Between 2003 And 2015 In 4th Grade") %>% 
  hc_xAxis(categories = c("Iran Math","Singapore Math","Iran Science","Singapore Science")) %>% 
  hc_yAxis(title = list(text = "Average Score")) %>% 
  hc_series(
    list(
      name = "Average Score",
      data = list(c(412,437),c(593,612),c(435,439),c(563,586)))
  ) %>% 
  hc_plotOptions(columnrange = list(dataLabels = list(enabled = TRUE))) %>%
  hc_add_theme(hc_theme_flat()) %>% 
  hc_legend(enabled = F)


## @knitr avgChange2
highchart() %>% 
  hc_chart(inverted = TRUE, type = "columnrange") %>% 
  hc_title(text = "Average Score Change Between 2003 And 2015 In 8th Grade") %>% 
  hc_xAxis(categories = c("Iran Math","Singapore Math","Iran Science","Singapore Science")) %>% 
  hc_yAxis(title = list(text = "Average Score")) %>% 
  hc_series(
    list(
      name = "Average Score",
      data = list(c(414,443),c(602,616),c(456,463),c(574,592)))
  ) %>% 
  hc_plotOptions(columnrange = list(dataLabels = list(enabled = TRUE))) %>%
  hc_add_theme(hc_theme_google()) %>% 
  hc_legend(enabled = F)

## @knitr rangeScores
highchart() %>% 
  hc_chart(inverted = TRUE, type = "columnrange") %>% 
  hc_title(text = "Range Of Scores In 2015 In 4th Grade") %>% 
  hc_xAxis(categories = c("Iran 4th Math","Iran 4th Science","Singapore 4th Math","Singapore 4th Science")) %>% 
  hc_yAxis(title = list(text = "Score")) %>% 
  hc_series(
    list(
      name = "Score",
      data = list(c(min(student.stat4th15 %>% filter(idcntry == 364) %>% .$mathScore),max(student.stat4th15 %>% filter(idcntry == 364) %>% .$mathScore)),
                  c(min(student.stat4th15 %>% filter(idcntry == 364) %>% .$scienceScore),max(student.stat4th15 %>% filter(idcntry == 364) %>% .$scienceScore)),
                  c(min(student.stat4th15 %>% filter(idcntry == 702) %>% .$mathScore),max(student.stat4th15 %>% filter(idcntry == 702) %>% .$mathScore)),
                  c(min(student.stat4th15 %>% filter(idcntry == 702) %>% .$scienceScore),max(student.stat4th15 %>% filter(idcntry == 702) %>% .$scienceScore))))
  ) %>% 
  hc_plotOptions(columnrange = list(dataLabels = list(enabled = TRUE))) %>%
  hc_add_theme(hc_theme_flat()) %>% 
  hc_legend(enabled = F)

## @knitr rangeScores2

highchart() %>% 
  hc_chart(inverted = TRUE, type = "columnrange") %>% 
  hc_title(text = "Range Of Scores In 2015 In 8th Grade") %>% 
  hc_xAxis(categories = c("Iran 8th Math","Iran 8th Science","Singapore 8th Math","Singapore 8th Science")) %>% 
  hc_yAxis(title = list(text = "Score")) %>% 
  hc_series(
    list(
      name = "Score",
      data = list(c(min(student.stat8th15 %>% filter(idcntry == 364) %>% .$mathScore),max(student.stat8th15 %>% filter(idcntry == 364) %>% .$mathScore)),
                  c(min(student.stat8th15 %>% filter(idcntry == 364) %>% .$scienceScore),max(student.stat8th15 %>% filter(idcntry == 364) %>% .$scienceScore)),
                  c(min(student.stat8th15 %>% filter(idcntry == 702) %>% .$mathScore),max(student.stat8th15 %>% filter(idcntry == 702) %>% .$mathScore)),
                  c(min(student.stat8th15 %>% filter(idcntry == 702) %>% .$scienceScore),max(student.stat8th15 %>% filter(idcntry == 702) %>% .$scienceScore))))
  ) %>% 
  hc_plotOptions(columnrange = list(dataLabels = list(enabled = TRUE))) %>%
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_legend(enabled = F)

