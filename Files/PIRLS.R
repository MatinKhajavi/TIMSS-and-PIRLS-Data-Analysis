## @knitr dataSection
library(jsonlite)
library(tidyverse)
library(highcharter)
library(plotly)
library(ISOcodes)
library(ggthemr)
### PIRLS 

## WORDWIDE
## 2016

acgPirls16 <- read_rds("../Pirls2016/ACG_Pirls16.rds")
asgPirls16 <- read_rds("../Pirls2016/ASG_Pirls16.rds")
ashPirls16 <- read_rds("../Pirls2016/ASH_Pirls16.rds")
atgPirls16 <- read_rds("../Pirls2016/ATG_Pirls16.rds")

student.statPirls16 <- read_rds("../Pirls2016/ASA_Pirls16.rds") %>% 
  group_by(IDCNTRY,IDSTUD,IDSCHOOL) %>% 
  mutate(score = mean(ASRREA01:ASRREA05)) %>% 
  select(IDCNTRY,IDSTUD,IDSCHOOL,score)

student.statPirls16 %>% 
  group_by(IDCNTRY,IDSCHOOL) %>% 
  summarise(score = mean(score)) -> schoolPerformancePirls16 

read_rds("../Pirls2016/AST_Pirls16.rds") %>% 
  mutate(overall = (ASRREA01 + ASRREA02 + ASRREA03 + ASRREA04 + ASRREA05)/5) %>%
  select(IDCNTRY, IDSTUD, IDSCHOOL,IDTEACH,overall) %>%
  group_by(IDCNTRY, IDSCHOOL,IDTEACH) %>% 
  summarise(score = mean(overall))-> teacherPerformancePirls16

student.statPirls16 %>% 
  group_by(IDCNTRY) %>% 
  summarise(score = mean(score)) %>% 
  arrange(desc(score)) -> countriesPerformancePirls16

ISO_3166_1 %>% 
  select(Country = Name , IDCNTRY = Numeric) -> countryCodes
c("England","Northern Ireland","Belgium (Flemish)","Dubai","Belgium (French)") -> Country
c(928,926,956,7841,957) -> IDCNTRY
data.frame(Country,IDCNTRY) -> df
rbind(countryCodes,df)->countryCodes
countryCodes$IDCNTRY <- as.double(countryCodes$IDCNTRY)

left_join(countriesPerformancePirls16,countryCodes) -> countriesPerformancePirls16

countriesPerformancePirls16 %>% 
  filter(!is.na(Country)) %>% 
  arrange(desc(score)) -> countriesPerformancePirls16

countriesPerformancePirls16 %>% 
  slice(1:5) -> Top5Pirls16
#####
student.statPirls16 %>% 
  mutate(benchmark = ifelse(score >= 625 ,"Advanced",
                            ifelse(score >= 550 , "High",
                                   ifelse(score >= 475 , "Intermediate","Low")))) -> student.BenchmarkStatPirls16

student.BenchmarkStatPirls16 %>% 
  group_by(IDCNTRY,benchmark) %>% 
  summarise(count = n()) -> countriesBenchmarksPirls16

student.statPirls16 %>% 
  group_by(IDCNTRY) %>% 
  summarise(all = n()) -> countries

left_join(countriesBenchmarksPirls16,countries) -> countriesBenchmarksPirls16
left_join(countriesBenchmarksPirls16,countryCodes) ->countriesBenchmarksPirls16
countriesBenchmarksPirls16 %>% 
  filter(!is.na(Country)) -> countriesBenchmarksPirls16


countriesBenchmarksPirls16 %>% 
  group_by(IDCNTRY,benchmark) %>% 
  mutate(percentage = round((count / all) * 100,0)) -> countriesBenchmarksPirls16



## @knitr resource
asgPirls16 %>% 
  mutate(resources = ASBG04 + 2-ASBG05A + 2-ASBG05B + 2-ASBG05C + 2-ASBG05D) %>%
  mutate(resources = ifelse(resources >= 6 ,"Many Resources",
                            ifelse(resources >=  4,"Some Resources","Few Resources"))) %>% 
  filter(!is.na(resources)) %>% 
  select(IDCNTRY,IDSTUD,IDSCHOOL,resources) -> studentHomePirls16


left_join(studentHomePirls16, student.statPirls16) -> studentHomePirls16

studentHomePirls16 %>% 
  group_by(resources) %>% 
  summarise(percentage = round(n()/nrow(studentHomePirls16) * 100,1), AvgScore = round(mean(score),0)) %>% 
  arrange(AvgScore)-> studentHomePirls16Summary

highchart() %>% 
  hc_add_series(studentHomePirls16Summary, "column", hcaes(x = resources, y = AvgScore),name = "Average Score") %>%
  hc_add_series(studentHomePirls16Summary, "pie", hcaes(name = resources, y = percentage), name = "Percentage") %>%
  hc_plotOptions(
    column = list(
      showInLegend = FALSE,
      colorByPoint = TRUE,pointFormat = "{point.y}",dataLabels = list(enabled = TRUE)
    ),
    pie = list(
      colorByPoint = TRUE, center = c('25%', '20%'),pointFormat = "{point.y}%",
      size = 230,dataLabels = list(format = "{point.name}<br>{point.percentage:.1f} %",distance = -10)
    )) %>%
  hc_yAxis(
    title = list(text = "Average Score"),
    labels = list(format = "{value}"), max = 850
  ) %>% 
  hc_xAxis(categories = studentHomePirls16Summary$resources) %>%
  hc_title(text = "Home Resources for Learning" ) %>%
  hc_tooltip(valueSuffix = " %") %>% 
  hc_add_theme(hc_theme_538())

rm(studentHomePirls16,studentHomePirls16Summary)


## @knitr earlyStudy
ashPirls16 %>% 
  mutate(preprimary = ASBH05B) %>%
  mutate(preprimary = ifelse(preprimary == 1  ,"Did Not Attend",
                             ifelse(preprimary == 2,"1 Year Or Less",
                                    ifelse(preprimary == 3,"2 Years","3 Years Or More")))) %>% 
  filter(!is.na(preprimary)) %>% 
  select(IDCNTRY,IDSTUD,IDSCHOOL,preprimary) -> studentPreprimaryPirls


left_join(studentPreprimaryPirls, student.statPirls16) -> studentPreprimaryPirls

studentPreprimaryPirls %>% 
  group_by(preprimary) %>% 
  summarise(percentage = round(n()/nrow(studentPreprimaryPirls) * 100,1), AvgScore = round(mean(score),0)) %>% 
  arrange(AvgScore)-> studentPreprimaryPirlsSummary

highchart() %>% 
  hc_add_series(studentPreprimaryPirlsSummary, "column", hcaes(x = preprimary, y = AvgScore),name = "Average Score") %>%
  hc_add_series(studentPreprimaryPirlsSummary, "pie", hcaes(name = preprimary, y = percentage), name = "Percentage") %>%
  hc_plotOptions(
    column = list(
      showInLegend = FALSE,
      colorByPoint = TRUE,pointFormat = "{point.y}",dataLabels = list(enabled = TRUE)
    ),
    pie = list(
      colorByPoint = TRUE, center = c('25%', '20%'),pointFormat = "{point.y}%",
      size = 230,dataLabels = list(format = "{point.name}<br>{point.percentage:.1f} %",distance = -15)
    )) %>%
  hc_yAxis(
    title = list(text = "Average Score"),
    labels = list(format = "{value}"), max = 850
  ) %>% 
  hc_xAxis(categories = studentPreprimaryPirlsSummary$preprimary) %>%
  hc_title(text = "An Early Start in School" ) %>%
  hc_tooltip(valueSuffix = " %") %>% 
  hc_add_theme(hc_theme_flat())


## @knitr homeResources
######## Home Resources

asgPirls16 %>% 
  mutate(resources = ASBG04 + 2-ASBG05A + 2-ASBG05B + 2-ASBG05C + 2-ASBG05D) %>%
  mutate(resources = ifelse(resources >= 6 ,"Many Resources",
                            ifelse(resources >=  4,"Some Resources","Few Resources"))) %>% 
  filter(!is.na(resources)) %>% 
  select(IDCNTRY,IDSTUD,IDSCHOOL,resources) -> studentHomePirls16


left_join(studentHomePirls16, student.statPirls16) -> studentHomePirls16

studentHomePirls16.fit <- aov(score ~ as.factor(resources), data = studentHomePirls16)
summary(studentHomePirls16.fit)
rm(studentHomePirls16.fit)

studentHomePirls16 %>% 
  group_by(resources) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentHomePirls16.avg

studentHomePirls16.avg %>% 
  hchart(type = "column", hcaes(x = resources, y = avgScore),color= "#8FBC8F")%>% 
  hc_title(text = "Home Resources") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())

studentHomePirls16$resources <- factor(studentHomePirls16$resources,levels = c("Many Resources","Some Resources","Few Resources"))
hcboxplot(x = studentHomePirls16$score,var = studentHomePirls16$resources,outliers = F,name = "Score",color= "#8FBC8F") %>%
  hc_title(text = "BoxPlot of Home Resources") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())   
rm(studentHomePirls16.avg,studentHomePirls16)


## @knitr earlyLiteracy
########### Early Literacy Activities Before Beginning Primary School


ashPirls16 %>% 
  mutate(literacy = ASBH02A + ASBH02B+ ASBH02C+ ASBH02D+ ASBH02E+ ASBH02F+ ASBH02G+ ASBH02H+ ASBH02I) %>%
  mutate(literacy = ifelse(literacy <= 13  ,"Often",
                           ifelse(literacy <= 22,"Sometimes","Never Or Almost Never"))) %>% 
  filter(!is.na(literacy)) %>% 
  select(IDCNTRY,IDSTUD,IDSCHOOL,literacy) -> studentEarlyLiteracyPirls16


left_join(studentEarlyLiteracyPirls16, student.statPirls16) -> studentEarlyLiteracyPirls16

studentEarlyLiteracyPirls16.fit <- aov(score ~ as.factor(literacy), data = studentEarlyLiteracyPirls16)
summary(studentEarlyLiteracyPirls16.fit)
rm(studentEarlyLiteracyPirls16.fit)

studentEarlyLiteracyPirls16 %>% 
  group_by(literacy) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentEarlyLiteracyPirls16.avg

studentEarlyLiteracyPirls16.avg %>% 
  hchart(type = "column", hcaes(x = literacy, y = avgScore),color= "#8FBC8F")%>% 
  hc_title(text = "Early Literacy Activities") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 

studentEarlyLiteracyPirls16$literacy <- factor(studentEarlyLiteracyPirls16$literacy,levels = c("Often","Sometimes","Never Or Almost Never"))

hcboxplot(x = studentEarlyLiteracyPirls16$score,var = studentEarlyLiteracyPirls16$literacy,outliers = F,name = "Score",color= "red") %>%
  hc_chart(type="column") %>% 
  hc_title(text = "BoxPlot of Early Literacy Activities") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())    


## @knitr preprimary 
############## Students Attended Preprimary Education
ashPirls16 %>% 
  mutate(preprimary = ASBH05B) %>%
  mutate(preprimary = ifelse(preprimary == 1  ,"Did Not Attend",
                             ifelse(preprimary == 2,"1 Year Or Less",
                                    ifelse(preprimary == 3,"2 Years","3 Years Or More")))) %>% 
  filter(!is.na(preprimary)) %>% 
  select(IDCNTRY,IDSTUD,IDSCHOOL,preprimary) -> studentPreprimaryPirls


left_join(studentPreprimaryPirls, student.statPirls16) -> studentPreprimaryPirls

studentPreprimaryPirls.fit <- aov(score ~ as.factor(preprimary), data = studentPreprimaryPirls)
summary(studentPreprimaryPirls.fit)
rm(studentPreprimaryPirls.fit)

studentPreprimaryPirls %>% 
  group_by(preprimary) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentPreprimaryPirls.avg

studentPreprimaryPirls.avg %>% 
  hchart(type = "bar", hcaes(x = preprimary, y = avgScore),color= "#F08080")%>% 
  hc_title(text = "Students Attended Preprimary Education") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 

studentPreprimaryPirls$preprimary <- factor(studentPreprimaryPirls$preprimary,levels = c("Did Not Attend","1 Year Or Less","2 Years","3 Years Or More"))
hcboxplot(x = studentPreprimaryPirls$score,var = studentPreprimaryPirls$preprimary,outliers = F,name = "Score",color= "#F08080") %>%
  hc_title(text = "BoxPlot of Students Attended Preprimary Education") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())     


## @knitr literacy
############ Could Do Literacy Tasks When Beginning Primary School
ashPirls16 %>% 
  mutate(literacy = ASBH07A +ASBH07B + ASBH07C + ASBH07D + ASBH07E + ASBH07F) %>%
  mutate(literacy = ifelse(literacy <= 7  ,"Very Well",
                           ifelse(literacy <= 13,"Moderately Well",
                                  ifelse(literacy <= 19,"Not Well","Not at all")))) %>% 
  filter(!is.na(literacy)) %>% 
  select(IDCNTRY,IDSTUD,IDSCHOOL,literacy) -> studentLiteracyPirls16


left_join(studentLiteracyPirls16, student.statPirls16) -> studentLiteracyPirls16

studentLiteracyPirls16.fit <- aov(score ~ as.factor(literacy), data = studentLiteracyPirls16)
summary(studentLiteracyPirls16.fit)
rm(studentLiteracyPirls16.fit)
studentLiteracyPirls16 %>% 
  group_by(literacy) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentLiteracyPirls16.avg

studentLiteracyPirls16.avg %>% 
  hchart(type = "column", hcaes(x = literacy, y = avgScore),color= "#FdA500")%>% 
  hc_title(text = "Literacy Tasks When Beginning Primary School") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 


studentLiteracyPirls16$literacy <- factor(studentLiteracyPirls16$literacy,levels = c("Very Well","Moderately Well","Not Well","Not at all"))
hcboxplot(x = studentLiteracyPirls16$score,var = studentLiteracyPirls16$literacy,outliers = F,name = "Score",color= "#FFA500") %>%
  hc_title(text = "BoxPlot of Literacy Tasks When Beginning Primary School") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())  

rm(studentLiteracyPirls16,studentLiteracyPirls16.avg)


## @knitr school
acgPirls16 %>% 
  mutate(economicState = ifelse(ACBG03B >=3,
                                ifelse(ACBG03A <= 2,"More Affluent","Neither More Affluent Nor More Disadvantaged"),
                                ifelse(ACBG03A >= 3,"More Disadvantaged","Neither More Affluent Nor More Disadvantaged"))) %>% 
  filter(!is.na(economicState)) %>% 
  select(IDCNTRY,IDSCHOOL,economicState) -> EconomicBackgroundPirls


left_join(EconomicBackgroundPirls, schoolPerformancePirls16) -> EconomicBackgroundPirls
EconomicBackgroundPirls %>% 
  group_by(economicState) %>% 
  summarise(percentage = round(n()/nrow(EconomicBackgroundPirls) * 100,1), AvgScore = round(mean(score),0)) %>% 
  arrange(AvgScore)-> EconomicBackgroundPirlsSummary

highchart() %>% 
  hc_add_series(EconomicBackgroundPirlsSummary, "column", hcaes(x = economicState, y = AvgScore),name = "Average Score") %>%
  hc_add_series(EconomicBackgroundPirlsSummary, "pie", hcaes(name = economicState, y = percentage), name = "Percentage") %>%
  hc_plotOptions(
    column = list(
      showInLegend = FALSE,
      colorByPoint = TRUE,pointFormat = "{point.y}",dataLabels = list(enabled = TRUE)
    ),
    pie = list(
      colorByPoint = TRUE, center = c('25%', '20%'),pointFormat = "{point.y}%",
      size = 210,dataLabels = list(format = "{point.name}<br>{point.percentage:.1f} %",distance = -15)
    )) %>%
  hc_yAxis(
    title = list(text = "Average Score"),
    labels = list(format = "{value}"), max = 850
  ) %>% 
  hc_xAxis(categories = EconomicBackgroundPirlsSummary$economicState) %>%
  hc_title(text = "Socioeconomic Background of the Students" ) %>%
  hc_tooltip(valueSuffix = " %") %>% 
  hc_add_theme(hc_theme_flat())

rm(EconomicBackgroundPirls,EconomicBackgroundPirlsSummary)

## @knitr schoolResources
########################## SCHOOL COMPOSiTiON AND RESOURCES

###### School Composition by Socioeconomic Background of the Student Body

acgPirls16 %>% 
  mutate(economicState = ifelse(ACBG03B >=3,
                                ifelse(ACBG03A <= 2,"More Affluent","Neither More Affluent Nor More Disadvantaged"),
                                ifelse(ACBG03A >= 3,"More Disadvantaged","Neither More Affluent Nor More Disadvantaged"))) %>% 
  filter(!is.na(economicState)) %>% 
  select(IDCNTRY,IDSCHOOL,economicState) -> EconomicBackgroundPirls


left_join(EconomicBackgroundPirls, schoolPerformancePirls16) -> EconomicBackgroundPirls

EconomicBackgroundPirls.fit <- aov(score ~ as.factor(economicState), data = EconomicBackgroundPirls)
summary(EconomicBackgroundPirls.fit)
rm(EconomicBackgroundPirls.fit)
EconomicBackgroundPirls %>% 
  group_by(economicState) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> EconomicBackgroundPirls.avg

EconomicBackgroundPirls.avg %>% 
  hchart(type = "column", hcaes(x = economicState, y = avgScore),color= "#FdA500")%>% 
  hc_title(text = "School Composition by Socioeconomic Background of the Student Body") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())      


EconomicBackgroundPirls$economicState <- factor(EconomicBackgroundPirls$economicState,levels = c("More Affluent","Neither More Affluent Nor More Disadvantaged","More Disadvantaged"))

hcboxplot(x = EconomicBackgroundPirls$score,var = EconomicBackgroundPirls$economicState,outliers = F,name = "Score",color= "#FFA500") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "School Composition by Socioeconomic Background of the Student Body") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 



## @knitr shortage
############  : Instruction Affected by Reading Resource Shortages

acgPirls16 %>% 
  mutate(resources = ACBG12AA + ACBG12AB +ACBG12AC + ACBG12AD + ACBG12AE +ACBG12AF +ACBG12AG + ACBG12AH +
           ACBG12AI + ACBG12BA + ACBG12BB + ACBG12BC + ACBG12BD) %>%
  mutate(resources = ifelse(resources < 19 ,"Not Affected",
                            ifelse(resources < 33 ,"Affected", "Affected A Lot"))) %>% 
  filter(!is.na(resources)) %>% 
  select(IDCNTRY,IDSCHOOL,resources) -> resourceShortagePirls


left_join(resourceShortagePirls, schoolPerformancePirls16) -> resourceShortagePirls

resourceShortagePirls.fit <- aov(score ~ as.factor(resources), data = resourceShortagePirls)
summary(resourceShortagePirls.fit)
rm(resourceShortagePirls.fit)
resourceShortagePirls %>% 
  group_by(resources) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> resourceShortagePirls.avg

resourceShortagePirls.avg %>% 
  hchart(type = "bar", hcaes(x = resources, y = avgScore),color= "#87CEFA")%>% 
  hc_title(text = "Instruction Affected by Reading Resource Shortages") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())      


resourceShortagePirls$resources <- factor(resourceShortagePirls$resources,levels = c("Not Affected","Affected", "Affected A Lot"))
hcboxplot(x = resourceShortagePirls$score,var = resourceShortagePirls$resources,outliers = F,name = "Score",color= "#87CEFA") %>%
  hc_title(text = "Instruction Affected by Reading Resource Shortages") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 

## @knitr library
########### Size of School Library

acgPirls16 %>% 
  mutate(books = ACBG09A) %>%
  mutate(books = ifelse(ACBG09 == 2 ,"No School Library",
                        ifelse(books >= 5 ,"More than 5,000 Book Title", 
                               ifelse(books >= 3,"501-5,000 Book Titles","500 Book Titles or Fewer")))) %>% 
  filter(!is.na(books) & !is.na(ACBG09)) %>% 
  select(IDCNTRY,IDSCHOOL,books) -> schoolLibraryPirls


left_join(schoolLibraryPirls, schoolPerformancePirls16) -> schoolLibraryPirls

schoolLibraryPirls.fit <- aov(score ~ as.factor(books), data = schoolLibraryPirls)
summary(schoolLibraryPirls.fit)
rm(schoolLibraryPirls.fit)
schoolLibraryPirls %>% 
  group_by(books) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolLibraryPirls.avg

schoolLibraryPirls.avg %>% 
  hchart(type = "column", hcaes(x = books, y = avgScore),color= "#8A2BE2")%>% 
  hc_title(text = "Size of School Library") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())      

schoolLibraryPirls$books <- factor(schoolLibraryPirls$books,levels = c("More than 5,000 Book Title","501-5,000 Book Titles","500 Book Titles or Fewer","No School Library"))
hcboxplot(x = schoolLibraryPirls$score,var = schoolLibraryPirls$books,outliers = F,name = "Score",color= "#8A2BE2") %>%
  hc_title(text = "Size of School Library") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  

rm(schoolLibraryPirls,schoolLibraryPirls.avg)

## @knitr emphasis
acgPirls16 %>% 
  mutate(emphasis = ACBG13A + ACBG13B + ACBG13C+ ACBG13D+ ACBG13E+ ACBG13F+ ACBG13G+ ACBG13H+ ACBG13I) %>%
  mutate(emphasis = ifelse(emphasis < 18 ,"Very High Emphasis",
                           ifelse(emphasis < 30 ,"High Emphasis",
                                  ifelse(emphasis < 40 , "Medium Emphasis" , "Low Emphasis")))) %>% 
  filter(!is.na(emphasis)) %>% 
  select(IDCNTRY,IDSCHOOL,emphasis) -> schoolEmphasisPirls


left_join(schoolEmphasisPirls, schoolPerformancePirls16) -> schoolEmphasisPirls
schoolEmphasisPirls %>% 
  group_by(emphasis) %>% 
  summarise(percentage = round(n()/nrow(schoolEmphasisPirls) * 100,3), AvgScore = round(mean(score),0)) %>% 
  arrange(AvgScore)-> schoolEmphasisPirlsSummary

highchart() %>% 
  hc_add_series(schoolEmphasisPirlsSummary, "column", hcaes(x = emphasis, y = AvgScore),name = "Average Score") %>%
  hc_add_series(schoolEmphasisPirlsSummary, "pie", hcaes(name = emphasis, y = percentage), name = "Percentage") %>%
  hc_plotOptions(
    column = list(
      showInLegend = FALSE,
      colorByPoint = TRUE,pointFormat = "{point.y}",dataLabels = list(enabled = TRUE)
    ),
    pie = list(
      colorByPoint = TRUE, center = c('25%', '20%'),pointFormat = "{point.y}%",
      size = 210,dataLabels = list(format = "{point.name}<br>{point.percentage:.3f} %",distance = 20)
    )) %>%
  hc_yAxis(
    title = list(text = "Average Score"),
    labels = list(format = "{value}"), max = 850
  ) %>% 
  hc_xAxis(categories = schoolEmphasisPirlsSummary$emphasis) %>%
  hc_title(text = "School Emphasis on Academic Success" ) %>%
  hc_tooltip(valueSuffix = " %") %>% 
  hc_add_theme(hc_theme_elementary())
rm(schoolEmphasisPirls,schoolEmphasisPirlsSummary)

## @knitr parents
######## Parents' Perceptions of Their Child's School

ashPirls16 %>% 
  mutate(perception = ASBH09A +ASBH09B + ASBH09C + ASBH09D + ASBH09E + ASBH09F) %>%
  mutate(perception = ifelse(perception <= 9 ,"Very Satisfied",
                             ifelse(perception <= 15,"Somewhat Satisfied","Less Than Satisfied"))) %>% 
  filter(!is.na(perception)) %>% 
  select(IDCNTRY,IDSTUD,IDSCHOOL,perception) -> parentsPerceptionPirls


left_join(parentsPerceptionPirls, student.statPirls16) -> parentsPerceptionPirls

parentsPerceptionPirls.fit <- aov(score ~ as.factor(perception), data = parentsPerceptionPirls)
summary(parentsPerceptionPirls.fit)
rm(parentsPerceptionPirls.fit)
parentsPerceptionPirls %>% 
  group_by(perception) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> parentsPerceptionPirls.avg

parentsPerceptionPirls.avg %>% 
  hchart(type = "column", hcaes(x = perception, y = avgScore),color= "#B0C4DE")%>% 
  hc_title(text = "Parents' Perceptions of Their Child's School") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())      

parentsPerceptionPirls$perception <- factor(parentsPerceptionPirls$perception,levels = c("Very Satisfied","Somewhat Satisfied","Less Than Satisfied"))
hcboxplot(x = parentsPerceptionPirls$score,var = parentsPerceptionPirls$perception,outliers = F,name = "Score",color= "#B0C4DE") %>%
  hc_title(text = "Parents' Perceptions of Their Child's School") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  
rm(parentsPerceptionPirls,parentsPerceptionPirls.avg)

## @knitr schoolEmphasis
#######  School Emphasis on Academic Success 
acgPirls16 %>% 
  mutate(emphasis = ACBG13A + ACBG13B + ACBG13C+ ACBG13D+ ACBG13E+ ACBG13F+ ACBG13G+ ACBG13H+ ACBG13I) %>%
  mutate(emphasis = ifelse(emphasis < 18 ,"Very High Emphasis",
                           ifelse(emphasis < 30 ,"High Emphasis",
                                  ifelse(emphasis < 40 , "Medium Emphasis" , "Low Emphasis")))) %>% 
  filter(!is.na(emphasis)) %>% 
  select(IDCNTRY,IDSCHOOL,emphasis) -> schoolEmphasisPirls


left_join(schoolEmphasisPirls, schoolPerformancePirls16) -> schoolEmphasisPirls

schoolEmphasisPirls.fit <- aov(score ~ as.factor(emphasis), data = schoolEmphasisPirls)
summary(schoolEmphasisPirls.fit)
rm(schoolEmphasisPirls.fit)
schoolEmphasisPirls %>% 
  group_by(emphasis) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolEmphasisPirls.avg

schoolEmphasisPirls.avg %>% 
  hchart(type = "column", hcaes(x = emphasis, y = avgScore),color= "#4169E1")%>% 
  hc_title(text = "School Emphasis on Academic Success") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())      

schoolEmphasisPirls$emphasis <- factor(schoolEmphasisPirls$emphasis,levels = c("Very High Emphasis","High Emphasis","Medium Emphasis" , "Low Emphasis"))
hcboxplot(x = schoolEmphasisPirls$score,var = schoolEmphasisPirls$emphasis,outliers = F,name = "Score",color= "#4169E1") %>%
  hc_title(text = "School Emphasis on Academic Success") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  
rm(schoolEmphasisPirls,schoolEmphasisPirlsa.vg)

## @knitr bullying
#########

asgPirls16 %>% 
  mutate(bullying = ASBG13A + ASBG13B + ASBG13C + ASBG13D + ASBG13E + ASBG13F + ASBG13G + ASBG13H ) %>%
  mutate(bullying = ifelse(bullying > 27 ,"Almost Never",
                           ifelse(bullying > 19 ,"About Monthly","About Weekly"))) %>% 
  filter(!is.na(bullying)) %>% 
  select(IDCNTRY,IDSCHOOL,IDSTUD,bullying) -> studentBullyingPirls


left_join(studentBullyingPirls, student.statPirls16) -> studentBullyingPirls
studentBullyingPirls %>% 
  group_by(bullying) %>% 
  summarise(percentage = round(n()/nrow(studentBullyingPirls) * 100,0), AvgScore = round(mean(score),0)) %>% 
  arrange(AvgScore)-> studentBullyingPirlsSummary

highchart() %>% 
  hc_add_series(studentBullyingPirlsSummary, "column", hcaes(x = bullying, y = AvgScore),name = "Average Score") %>%
  hc_add_series(studentBullyingPirlsSummary, "pie", hcaes(name = bullying, y = percentage), name = "Percentage") %>%
  hc_plotOptions(
    column = list(
      showInLegend = FALSE,
      colorByPoint = TRUE,pointFormat = "{point.y}",dataLabels = list(enabled = TRUE)
    ),
    pie = list(
      colorByPoint = TRUE, center = c('25%', '20%'),pointFormat = "{point.y}%",
      size = 210,dataLabels = list(format = "{point.name}<br>{point.percentage:.1f} %",distance = 10)
    )) %>%
  hc_yAxis(
    title = list(text = "Average Score"),
    labels = list(format = "{value}"), max = 850
  ) %>% 
  hc_xAxis(categories = studentBullyingPirlsSummary$bullying) %>%
  hc_title(text = "Student Bullying" ) %>%
  hc_tooltip(valueSuffix = " %") %>% 
  hc_add_theme(hc_theme_smpl())
rm(studentBullyingPirls,studentBullyingPirlsSummary)
########

## @knitr discipline
######### School Discipline - Principals' Reports
acgPirls16 %>% 
  mutate(discipline = ACBG14A + ACBG14B + ACBG14C+ ACBG14D+ ACBG14E+ ACBG14F+ ACBG14G+ ACBG14H+ ACBG14I + ACBG14J) %>%
  mutate(discipline = ifelse(discipline < 15 ,"Hardly Any Problems",
                             ifelse(discipline < 26 ,"Minor Problems","Moderate to Severe Problems"))) %>% 
  filter(!is.na(discipline)) %>% 
  select(IDCNTRY,IDSCHOOL,discipline) -> schoolDisciplinePirls


left_join(schoolDisciplinePirls, schoolPerformancePirls16) -> schoolDisciplinePirls

schoolDisciplinePirls.fit <- aov(score ~ as.factor(discipline), data = schoolDisciplinePirls)
summary(schoolDisciplinePirls.fit)
rm(schoolDisciplinePirls.fit)
schoolDisciplinePirls %>% 
  group_by(discipline) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolDisciplinePirls.avg

schoolDisciplinePirls.avg %>% 
  hchart(type = "bar", hcaes(x = discipline, y = avgScore),color= "#F0F8FF")%>% 
  hc_title(text = "School Discipline - Principals' Reports") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())      

schoolDisciplinePirls$discipline <- factor(schoolDisciplinePirls$discipline,levels = c("Hardly Any Problems","Minor Problems","Moderate to Severe Problems"))
hcboxplot(x = schoolDisciplinePirls$score,var = schoolDisciplinePirls$discipline,outliers = F,name = "Score",color= "#483D8B") %>%
  hc_title(text = "School Discipline - Principals' Reports") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  
rm(schoolDisciplinePirls,schoolDisciplinePirls.avg)

## @knitr orderly
############## Safe and Orderly School - Teachers' Reports

atgPirls16 %>% 
  mutate(safe = ATBG08A + ATBG08F + ATBG08B + ATBG08C + ATBG08D + ATBG08E + ATBG08G + ATBG08H ) %>%
  mutate(safe = ifelse(safe < 13 ,"Very Safe",
                       ifelse(safe < 22 ,"Somewhat Safe","Less Than Safe"))) %>% 
  filter(!is.na(safe)) %>% 
  select(IDCNTRY,IDSCHOOL,IDTEACH,safe) -> schoolSafePirls


left_join(schoolSafePirls, teacherPerformancePirls16) -> schoolSafePirls

schoolSafePirls.fit <- aov(score ~ as.factor(safe), data = schoolSafePirls)
summary(schoolSafePirls.fit)
rm(schoolSafePirls.fit)
schoolSafePirls %>% 
  group_by(safe) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolSafePirls.avg

schoolSafePirls.avg %>% 
  hchart(type = "bar", hcaes(x = safe, y = avgScore),color= "#1E90FF")%>% 
  hc_title(text = "Safe and Orderly School - Teachers' Reports") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  


schoolSafePirls$safe <- factor(schoolSafePirls$safe,levels = c("Very Safe","Somewhat Safe","Less Than Safe"))
hcboxplot(x = schoolSafePirls$score,var = schoolSafePirls$safe,outliers = F,name = "Score",color= "#1E90FF") %>%
  hc_title(text = "Safe and Orderly School - Teachers' Reports") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white")))  %>% 
  hc_add_theme(hc_theme_flatdark())      
rm(schoolSafePirls,schoolSafePirls.avg)

## @knitr studentBullying
##########  Student Bullying
asgPirls16 %>% 
  mutate(bullying = ASBG13A + ASBG13B + ASBG13C + ASBG13D + ASBG13E + ASBG13F + ASBG13G + ASBG13H ) %>%
  mutate(bullying = ifelse(bullying > 27 ,"Almost Never",
                           ifelse(bullying > 19 ,"About Monthly","About Weekly"))) %>% 
  filter(!is.na(bullying)) %>% 
  select(IDCNTRY,IDSCHOOL,IDSTUD,bullying) -> studentBullyingPirls


left_join(studentBullyingPirls, student.statPirls16) -> studentBullyingPirls

studentBullyingPirls.fit <- aov(score ~ as.factor(bullying), data = studentBullyingPirls)
summary(studentBullyingPirls.fit)
rm(studentBullyingPirls.fit)
studentBullyingPirls %>% 
  group_by(bullying) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentBullyingPirls.avg

studentBullyingPirls.avg %>% 
  hchart(type = "column", hcaes(x = bullying, y = avgScore),color= "#00BFFF")%>% 
  hc_title(text = "Student Bullying") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  


studentBullyingPirls$bullying <- factor(studentBullyingPirls$bullying,levels = c("Almost Never","About Monthly","About Weekly"))
hcboxplot(x = studentBullyingPirls$score,var = studentBullyingPirls$bullying,outliers = F,name = "Score",color= "#00BFFF") %>%
  hc_title(text = "Student Bullying") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white")))  %>% 
  hc_add_theme(hc_theme_flatdark())      


## @knitr organizing
######  Organizing Students for Reading Instruction :
## teach reading as Whole-class 

atgPirls16 %>% 
  mutate(wholeClass = ATBR08A ) %>%
  mutate(wholeClass = ifelse(wholeClass == 1 ,"Always Or Almost Always",
                             ifelse(wholeClass == 2 ,"Often",ifelse(wholeClass == 3 ,"Sometimes","Never")))) %>% 
  filter(!is.na(wholeClass)) %>% 
  select(IDCNTRY,IDSCHOOL,IDTEACH,wholeClass) -> teachWholeClassPirls


left_join(teachWholeClassPirls, teacherPerformancePirls16) -> teachWholeClassPirls

teachWholeClassPirls.fit <- aov(score ~ as.factor(wholeClass), data = teachWholeClassPirls)
summary(teachWholeClassPirls.fit)
rm(teachWholeClassPirls.fit)
teachWholeClassPirls %>% 
  group_by(wholeClass) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> teachWholeClassPirls.avg

teachWholeClassPirls.avg %>% 
  hchart(type = "column", hcaes(x = wholeClass, y = avgScore),color= "#FFFACD")%>% 
  hc_title(text = "Teach Reading as Whole-Class ") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  

teachWholeClassPirls$wholeClass <- factor(teachWholeClassPirls$wholeClass,levels = c("Always Or Almost Always","Often","Sometimes","Never"))
hcboxplot(x = teachWholeClassPirls$score,var = teachWholeClassPirls$wholeClass,outliers = F,name = "Score",color= "#CCCC00") %>%
  hc_title(text = "Teach Reading as Whole-Class ") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white")))  %>% 
  hc_add_theme(hc_theme_flatdark())    
rm(teachWholeClassPirls,teachWholeClassPirls.avg)

## @knitr mixedGroups
## mixed-ability groups
atgPirls16 %>% 
  mutate(mixedGroups = ATBR08A ) %>%
  mutate(mixedGroups = ifelse(mixedGroups == 1 ,"Always Or Almost Always",
                              ifelse(mixedGroups == 2 ,"Often",ifelse(mixedGroups == 3 ,"Sometimes","Never")))) %>% 
  filter(!is.na(mixedGroups)) %>% 
  select(IDCNTRY,IDSCHOOL,IDTEACH,mixedGroups) -> teachMixedGroupsPirls


left_join(teachMixedGroupsPirls, teacherPerformancePirls16) -> teachMixedGroupsPirls

teachMixedGroupsPirls.fit <- aov(score ~ as.factor(mixedGroups), data = teachMixedGroupsPirls)
summary(teachMixedGroupsPirls.fit)
rm(teachMixedGroupsPirls.fit)
teachMixedGroupsPirls %>% 
  group_by(mixedGroups) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> teachMixedGroupsPirls.avg

teachMixedGroupsPirls.avg %>% 
  hchart(type = "column", hcaes(x = mixedGroups, y = avgScore),color= "#708090")%>% 
  hc_title(text = "Teach Reading as Mixed-Ability Groups") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  

teachMixedGroupsPirls$mixedGroups <- factor(teachMixedGroupsPirls$mixedGroups,levels = c("Always Or Almost Always","Often","Sometimes","Never"))
hcboxplot(x = teachMixedGroupsPirls$score,var = teachMixedGroupsPirls$mixedGroups,outliers = F,name = "Score",color= "#DCDCDC") %>%
  hc_title(text = "Teach Reading as Mixed-Ability Groups") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white")))  %>% 
  hc_add_theme(hc_theme_flatdark())      
rm(teachMixedGroupsPirls,teachMixedGroupsPirls.avg)

## @knitr engaged
#########  Students Engaged in Reading Lessons

asgPirls16 %>% 
  mutate(engagement = ASBR01A + ASBR01B + ASBR01C + ASBR01D + ASBR01E + ASBR01F + ASBR01G + ASBR01H + ASBR01I ) %>%
  mutate(engagement = ifelse(engagement <= 13 ,"Very Engaged",
                             ifelse(engagement <= 23 ,"Somewhat Engaged","Less Than Engaged"))) %>% 
  filter(!is.na(engagement)) %>% 
  select(IDCNTRY,IDSCHOOL,IDSTUD,engagement) -> studentEngagementPirls


left_join(studentEngagementPirls, student.statPirls16) -> studentEngagementPirls

studentEngagementPirls.fit <- aov(score ~ as.factor(engagement), data = studentEngagementPirls)
summary(studentEngagementPirls.fit)
rm(studentEngagementPirls.fit)
studentEngagementPirls %>% 
  group_by(engagement) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentEngagementPirls.avg

studentEngagementPirls.avg %>% 
  hchart(type = "column", hcaes(x = engagement, y = avgScore),color= "#FFC0CB")%>% 
  hc_title(text = "Students Engaged in Reading Lessons") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  

studentEngagementPirls$engagement <- factor(studentEngagementPirls$engagement,levels = c("Very Engaged","Somewhat Engaged","Less Than Engaged"))
hcboxplot(x = studentEngagementPirls$score,var = studentEngagementPirls$engagement,outliers = F,name = "Score",color= "#FFC0CB") %>%
  hc_title(text = "Students Engaged in Reading Lessons") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white")))  %>% 
  hc_add_theme(hc_theme_flatdark())      
rm(studentEngagementPirls,studentEngagementPirls.avg)

## @knitr confidence
######## Students Confident in Reading

asgPirls16 %>% 
  mutate(confident = ASBR07A + ASBR07B + ASBR07C + ASBR07D + ASBR07E + ASBR07F ) %>%
  mutate(confident = ifelse(confident <= 10 ,"Not Confident",
                            ifelse(confident <= 16 ,"Somewhat Confident","Very Confident"))) %>% 
  filter(!is.na(confident)) %>% 
  select(IDCNTRY,IDSCHOOL,IDSTUD,confident) -> studentConfidentPirls


left_join(studentConfidentPirls, student.statPirls16) -> studentConfidentPirls

studentConfidentPirls.fit <- aov(score ~ as.factor(confident), data = studentConfidentPirls)
summary(studentConfidentPirls.fit)
rm(studentConfidentPirls.fit)
studentConfidentPirls %>% 
  group_by(confident) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentConfidentPirls.avg

studentConfidentPirls.avg %>% 
  hchart(type = "column", hcaes(x = confident, y = avgScore),color= "#7FFFD4")%>% 
  hc_title(text = "Students Confident in Reading") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black")))  

studentConfidentPirls$confident <- factor(studentConfidentPirls$confident,levels = c("Very Confident","Somewhat Confident","Not Confident"))
hcboxplot(x = studentConfidentPirls$score,var = studentConfidentPirls$confident,outliers = F,name = "Score",color= "#008B8B") %>%
  hc_title(text = "Students Confident in Reading") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white")))  %>% 
  hc_add_theme(hc_theme_flatdark())      

rm(studentConfidentPirls,studentConfidentPirls.avg)

