## @knitr dataSection
library(jsonlite)

library(tidyverse)
library(highcharter)
library(plotly)
library(ISOcodes)
library(ggthemr)
####################################################################################
##########################   TIMSS  ################################################
####################################################################################

## WORDWIDE


######### 2015 TIMSS ########
bcg15 <- read_rds("../2015/bcg.rds")
bsa15 <- read_rds("../2015/bsa.rds")
bsg15 <- read_rds("../2015/bsg.rds")
#bsr15 <- read_rds("../2015/bsr.rds")
bst15 <- read_rds("../2015/bst.rds")
btm15 <- read_rds("../2015/btm.rds")
bts15 <- read_rds("../2015/bts.rds")

######### STUDENT SCORES / TEACHERS/ SCHOOLS #####

## WORDWIDE 

## 2015 TIMSS 8TH GRADE

## STUDENTS
bscores <- bsa15 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5) %>%
  mutate(overall = (mat + sci) / 2) %>%
  mutate(mat.state = ifelse(mat >= 625, "Advanced", 
                            ifelse(mat >= 550, "High", 
                                   ifelse(mat >= 475, "Intermediate", "Low"))), 
         sci.state = ifelse(sci >= 625, "Advanced", 
                            ifelse(sci >= 550, "High", 
                                   ifelse(sci >= 475, "Intermediate", "Low"))), 
         over.state = ifelse(overall >= 625, "Advanced", 
                             ifelse(overall >= 550, "High", 
                                    ifelse(overall >= 475, "Intermediate", "Low"))))
## TEACHERS
bteacher <- bst15 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 
         sci = (bsssci01 + bsssci02 + bsssci03 + bsssci04 + bsssci05) / 5) %>%
  mutate(overall = (mat + sci) / 2) %>%
  group_by(idcntry, idschool, idteach) %>%
  summarise(mat.avg = mean(mat), 
            sci.avg = mean(sci), 
            over.avg = mean(overall))

## SCHOOLS
bschool <- bscores %>% 
  group_by(idcntry,idschool) %>% 
  summarise(over.avg = mean(overall), 
            mat.avg = mean(mat), 
            sci.avg = mean(sci))

student.stat8th <- read_rds("../2015/bsa.rds") %>%
  group_by(idcntry, idstud, idschool) %>%
  mutate(mat = mean(bsmmat01:bsmmat05), 
         sci = mean(bsssci01:bsssci05)) %>%
  select(idcntry, idstud, idschool, mat, sci) %>%
  mutate(overall = (mat + sci) / 2)

student.stat8th %>% 
  group_by(idcntry,idschool) %>% 
  summarise(score = mean(overall)) -> schoolPerformance8th
bst15 %>% 
  mutate(mat = (bsmmat01+bsmmat02+ bsmmat03+bsmmat04+bsmmat05)/5, 
         sci = (bsssci01+bsssci02+bsssci03+bsssci04+bsssci05)/5) %>%
  select(idcntry, idstud, idschool,idteach, mat, sci) %>%
  mutate(overall = (mat + sci) / 2) %>% 
  group_by(idcntry,idschool,idteach) %>% 
  summarise(score = mean(overall))-> teacherPerformance8th


## @knitr physicalResources
### PHYSICAL RESOURCES ###

### 8TH GRADE
bsg15.res <- bsg15 %>%
  mutate(desk = ifelse(bsbg06c == 1, 1, 0)) %>%
  mutate(room = ifelse(bsbg06d == 1, 1, 0)) %>%
  mutate(internet = ifelse(bsbg06e == 1, 1, 0)) %>%
  mutate(resources = desk + room + internet + bsbg04 + bsbg05) %>%
  filter(!is.na(resources)) %>%
  mutate(res.cat = ifelse(resources >= 10, "Many", 
                          ifelse(resources >=7, "Some", "Few"))) %>%
  select(idcntry, idstud, idschool, res.cat)

home15.stat8th <- left_join(bsg15.res, 
                            bscores, 
                            by = c("idcntry", "idstud", "idschool"))
home15.stat8th$res.cat <- factor(home15.stat8th$res.cat,
                                 levels = c("Many",
                                            "Some", 
                                            "Few"))

home15.stat8th.summery <- home15.stat8th %>%
  group_by(over.state, res.cat) %>%
  summarise(freq = n()) %>%
  mutate(category = paste("Overall Score = ", over.state, 
                          ", Physical Resources = ", res.cat))

home15.stat8th.avg <- home15.stat8th %>%
  group_by(res.cat) %>%
  summarise(avg = mean(overall))

# ANOVA
home15.stat8th.fit <- aov(overall ~ as.factor(res.cat), data = home15.stat8th)
summary(home15.stat8th.fit)
rm(home15.stat8th.fit)
# Chi-square

chisq.test(matrix(as.numeric(home15.stat8th.summery$freq), 
                  nrow = 3, ncol = 3, byrow = T), 
           simulate.p.value = T)
# avg histogram 

home15.stat8th.avg %>% 
  hchart(type = "column", hcaes(x = res.cat, y = avg %>% round(3)), color = "#20B2AA") %>%
  hc_title(text = "Average Score of Each Group",
           align = 'center', 
           style = list(fontWeight = "italic", fontSize = "20px")) %>%
  hc_subtitle(text = "Based on Resource Categoreis", 
              align = 'center', 
              style = list(fontSize = "15px")) %>%
  hc_xAxis(title = list(text = "Resources"), 
           style = list(color = "black"), tickColor = "black",
           tickLength = 20, lineColor = 'black',
           labels = list(style = list(color = "black", fontSize = "20px"))) %>%
  hc_yAxis(title = list(text = "Average Score"), 
           lineColor = 'black', lineWidth = 1) %>%
  hc_add_theme(hc_theme_538())

home15.stat8th.summery %>%
  group_by(res.cat) %>%
  summarise(freq = sum(freq)) %>%
  hchart(type = "pie", 
         hcaes(x = res.cat, y = freq), name = "Number of Participants") %>%
  hc_title(text = "Number of Participants in Each Category") %>%
  hc_add_theme(hc_theme_google())


## @knitr parentalEducation
### PARENTAL EDUCATION ### 

## 8TH GRADE
bsg15.pedu <- bsg15 %>%
  mutate(bsbg07a = ifelse(is.na(bsbg07a), 0, bsbg07a), 
         bsbg07b = ifelse(is.na(bsbg07b), 0, bsbg07b)) %>%
  mutate(bsbg07a = ifelse(bsbg07a == 8, bsbg07b, bsbg07a)) %>%
  mutate(bsbg07b = ifelse(bsbg07b == 8, bsbg07a, bsbg07b)) %>%
  mutate(hedu = ifelse(bsbg07a > bsbg07b, bsbg07a, bsbg07b)) %>%
  mutate(hedu = ifelse(hedu == 1, "did not go to school", 
                       ifelse(hedu == 2, "Lower secondary", 
                              ifelse(hedu == 3, "Upper secondary",
                                     ifelse(hedu == 4, "Post secondary, non-tertiary", 
                                            ifelse(hedu == 5, "Short-cycle tertiary", 
                                                   ifelse(hedu == 6, "Bachelor’s or equivalent",
                                                          "Postgraduate degree"))))))) %>%
  filter(hedu != 8) %>%
  select(idcntry, idstud, idschool, hedu)

parent15.stat8th <- left_join(bsg15.pedu, 
                              bscores, 
                              by = c("idcntry", "idstud", "idschool"))

parent15.stat8th$hedu <- factor(parent15.stat8th$hedu,
                                levels = c("did not go to school",
                                           "Lower secondary", 
                                           "Upper secondary", 
                                           "Post secondary, non-tertiary", 
                                           "Short-cycle tertiary", 
                                           "Bachelor’s or equivalent", 
                                           "Postgraduate degree"))

parent15.stat8th.summery <- parent15.stat8th %>%
  group_by(over.state, hedu) %>%
  summarise(freq = n()) %>%
  mutate(category = paste("Overall Score = ", over.state, 
                          ", Highest Parental Education = ", hedu))


parent15.stat8th.avg <- parent15.stat8th %>%
  group_by(hedu) %>%
  summarise(avg = mean(overall))

# ANOVA
parent15.stat8th.fit <- aov(overall ~ as.factor(hedu),
                            data = parent15.stat8th)
summary(parent15.stat8th.fit)
rm(parent15.stat8th.fit)
# Chi-square

chisq.test(matrix(as.numeric(parent15.stat8th.summery$freq), 
                  ncol = 7, byrow = T), simulate.p.value = T)

# avg histogram 

parent15.stat8th.avg %>% 
  hchart(type = "column", hcaes(x = hedu, y = round(avg, 2)), 
         name = "Average Score", 
         color = "skyblue") %>%
  hc_title(text = "Educate Yourself Before Educating Your Children",
           align = 'center', 
           style = list(fontWeight = "italic", fontSize = "20px")) %>%
  hc_xAxis(title = list(text = "Highest Level of Education of Parents", color = 'white'), 
           style = list(color = "white"), tickColor = "black",
           tickLength = 20, lineColor = 'black',
           labels = list(style = list(color = "white"))) %>%
  hc_yAxis(title = list(text = "Average Score of Children", color = 'white'), 
           labels = list(style = list(color = "white"))) %>%
  hc_add_theme(hc_theme_db())

# box plot
ggthemr('dust')
ggplot(parent15.stat8th) + 
  geom_boxplot(aes(y = overall, x = hedu, fill = hedu)) +
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text = element_blank(), 
        legend.position = "bottom") + 
  labs(title = "Boxplot of Children's Overall Score", 
       subtitle = "Based on Highest Level of Education by Parents", 
       x = "Overall Score", y = "Highest Level of Education of Parents") + 
  coord_flip() + 
  scale_y_continuous()


## @knitr Lang
### SPEAK LANGUAGE AT HOME ### 

## 8TH GRADE
bsg15.lang <- bsg15 %>%
  filter(!is.na(bsbg03)) %>%
  mutate(language = ifelse(bsbg03 == 1, "Always", 
                           ifelse(bsbg03 == 2, "Almost Always", 
                                  ifelse(bsbg03 == 3, "Sometimes", "Never"))))

language15.stat8th <- left_join(bsg15.lang, bscores, 
                                by = c("idcntry", "idstud", "idschool"))

language15.stat8th$language <- factor(language15.stat8th$language,
                                      levels = c("Always",
                                                 "Almost Always", 
                                                 "Sometimes", 
                                                 "Never"))

language15.stat8th.summary <- language15.stat8th %>%
  group_by(over.state, language) %>%
  summarise(freq = n())

language15.stat8th.avg <- language15.stat8th %>%
  group_by(language) %>%
  summarise(avg = mean(overall))

# ANOVA
language15.stat8th.fit <- aov(overall ~ as.factor(language),
                              data = language15.stat8th)
summary(language15.stat8th.fit)
rm(language15.stat8th.fit)
# Chi-square

chisq.test(matrix(as.numeric(language15.stat8th.summary$freq), 
                  nrow = 4, byrow = T))

# avg histogram 

language15.stat8th.avg %>% 
  hchart(type = "column", hcaes(x = language, y = avg %>% round(2)), 
         name = "Average Score", 
         color = "pink") %>%
  hc_title(text = "More English, Less Mother Langugae",
           align = 'center', 
           style = list(fontWeight = "italic", fontSize = "20px")) %>%
  hc_xAxis(title = list(text = "How Frequent They Speak Language of Test at Home", 
                        color = 'white'), 
           style = list(color = "white"), tickColor = "black",
           tickLength = 20, lineColor = 'black',
           labels = list(style = list(color = "white"))) %>%
  hc_yAxis(title = list(text = "Average Score", color = 'white'), 
           labels = list(style = list(color = "white"))) %>%
  hc_add_theme(hc_theme_db())

# box plot
ggthemr('fresh')
ggplot(language15.stat8th) + 
  geom_boxplot(aes(y = overall, x = language, fill = language)) +
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text = element_blank(), 
        legend.position = "bottom") + 
  labs(title = "Boxplot of Children's Overall Score", 
       subtitle = "Based on How Much They Speak The Language of Test at Home", 
       x = "Overall Score", y = "How Frequent They Speak Language of Test at Home") + 
  coord_flip() + 
  scale_y_continuous()



## @knitr economic
#### Economic background of students
##8th grade
bcg15 %>% 
  mutate(economicState = ifelse(bcbg03b >=3,
                                ifelse(bcbg03a <= 2,"More Affluent","Neither More Affluent Nor More Disadvantaged"),
                                ifelse(bcbg03a >= 3,"More Disadvantaged","Neither More Affluent Nor More Disadvantaged"))) %>% 
  filter(!is.na(economicState)) %>% 
  select(idcntry,idschool,economicState) -> EconomicBackground8th


left_join(EconomicBackground8th, schoolPerformance8th, by = c("idcntry","idschool")) -> EconomicBackground8th

EconomicBackground8th.fit <- aov(score ~ as.factor(economicState), data = EconomicBackground8th)
summary(EconomicBackground8th.fit)
rm(EconomicBackground8th.fit)
EconomicBackground8th %>% 
  group_by(economicState) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> EconomicBackground8th.avg

EconomicBackground8th.avg %>% 
  hchart(type = "column", hcaes(x = economicState, y = avgScore),name = "Average Score") %>% 
  hc_title(text = "Economic Background Of Students") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "Economic State")) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",tickLength = 5,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_google())

EconomicBackground8th$economicState <- factor(EconomicBackground8th$economicState,levels = c("More Affluent","Neither More Affluent Nor More Disadvantaged","More Disadvantaged"))
hcboxplot(x = EconomicBackground8th$score,var = EconomicBackground8th$economicState,outliers = F,name = "Score") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot of the Economic Background") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "Economic State")) %>% 
  hc_add_theme(hc_theme_google())


## @knitr shortage
####### Instruction Affected by Resource Shortages

### 8th grade

bcg15 %>% 
  mutate(resources = bcbg13aa+bcbg13ab +bcbg13ac + bcbg13ad + bcbg13ae +bcbg13af +bcbg13ag + bcbg13ah + 
           bcbg13ai + bcbg13ba + bcbg13bb + bcbg13bc + bcbg13bd + bcbg13be + bcbg13ca + bcbg13cb +bcbg13cc+ bcbg13cd) %>%
  mutate(resources = ifelse(resources < 25 ,"Not Affected",
                            ifelse(resources < 60 ,"Affected", "Affected A Lot"))) %>% 
  filter(!is.na(resources)) %>% 
  select(idcntry,idschool,resources) -> resourceShortage8th


left_join(resourceShortage8th, schoolPerformance8th, by = c("idcntry","idschool")) -> resourceShortage8th

resourceShortage8th.fit <- aov(score ~ as.factor(resources), data = resourceShortage8th)
summary(resourceShortage8th.fit)
rm(resourceShortage8th.fit)
resourceShortage8th %>% 
  group_by(resources) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> resourceShortage8th.avg

resourceShortage8th.avg %>% 
  hchart(type = "column", hcaes(x = resources, y = avgScore))%>% 
  hc_title(text = "Instruction Affected by Resource Shortages") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_sandsignika())

resourceShortage8th$resources <- factor(resourceShortage8th$resources,levels = c("Not Affected","Affected", "Affected A Lot"))
hcboxplot(x = resourceShortage8th$score,var = resourceShortage8th$resources,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot of Instruction Affected by Resource Shortages") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_sandsignika()) 



## @knitr conditions
########### Problems with School Conditions and Resources

##8th grade

btm15 %>% 
  mutate(condition = btbg08a + btbg08b + btbg08c + btbg08d + btbg08e + btbg08f + btbg08g) %>%
  mutate(condition = ifelse(condition < 8 ,"Hardly Any Problems",
                            ifelse(condition < 17 ,"Minor Problems", "Moderate to Severe Problemm"))) %>% 
  filter(!is.na(condition)) %>% 
  select(idcntry,idschool,idteach,condition) -> schoolcondition8th


left_join(schoolcondition8th, teacherPerformance8th, by = c("idcntry","idschool","idteach")) -> schoolcondition8th

schoolcondition8th.fit <- aov(score ~ as.factor(condition), data = schoolcondition8th)
summary(schoolcondition8th.fit)
rm(schoolcondition8th.fit)
schoolcondition8th %>% 
  group_by(condition) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolcondition8th.avg

schoolcondition8th.avg %>% 
  hchart(type = "column", hcaes(x = condition, y = avgScore),color = "#59f0b9")%>% 
  hc_title(text = "Problems with School Conditions and Resources") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 

schoolcondition8th$condition <- factor(schoolcondition8th$condition,levels = c("Hardly Any Problems","Minor Problems", "Moderate to Severe Problemm"))
hcboxplot(x = schoolcondition8th$score,var = schoolcondition8th$condition,outliers = T,name = "Score",color = "#59f0b9") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot Problems with School Conditions and Resources") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 

## @knitr emphasis
####### School Emphasis on Academic Success

## 8th grade

bcg15 %>% 
  mutate(emphasis = bcbg14a + bcbg14b + bcbg14c+ bcbg14d+ bcbg14e+ bcbg14f+ bcbg14g+ bcbg14h+ bcbg14i+
           bcbg14j + bcbg14k +bcbg14l + bcbg14m) %>%
  mutate(emphasis = ifelse(emphasis < 15 ,"Very High Emphasis",
                           ifelse(emphasis < 28 ,"High Emphasis",
                                  ifelse(emphasis < 41 , "Medium Emphasis" , "Low Emphasis")))) %>% 
  filter(!is.na(emphasis)) %>% 
  select(idcntry,idschool,emphasis) -> schoolEmphasis8th


left_join(schoolEmphasis8th, schoolPerformance8th, by = c("idcntry","idschool")) -> schoolEmphasis8th

schoolEmphasis8th.fit <- aov(score ~ as.factor(emphasis), data = schoolEmphasis8th)
summary(schoolEmphasis8th.fit)
rm(schoolEmphasis8th.fit)
schoolEmphasis8th %>% 
  group_by(emphasis) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolEmphasis8th.avg

schoolEmphasis8th.avg %>% 
  hchart(type = "column", hcaes(x = emphasis, y = avgScore))%>% 
  hc_title(text = "School Emphasis on Academic Success") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_538())

schoolEmphasis8th$emphasis <- factor(schoolEmphasis8th$emphasis,levels = c("Very High Emphasis","High Emphasis","Medium Emphasis" , "Low Emphasis"))
hcboxplot(x = schoolEmphasis8th$score,var = schoolEmphasis8th$emphasis,outliers = F,name = "Score") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot of School Emphasis on Academic Success") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_538())

## @knitr belonging
######  Students' Sense of School Belonging

## 8th grade
bsg15 %>% 
  mutate(belonging = bsbg15a + bsbg15b + bsbg15c + bsbg15d + bsbg15e + bsbg15f + bsbg15g) %>%
  mutate(belonging = ifelse(belonging < 15 ,"High Sense of Belonging",
                            ifelse(belonging < 25 ,"Sense of Belonging", "Little Sense of Belonging"))) %>% 
  filter(!is.na(belonging)) %>% 
  select(idcntry,idschool,belonging) -> schoolBelonging8th


left_join(schoolBelonging8th, schoolPerformance8th, by = c("idcntry","idschool")) -> schoolBelonging8th

schoolBelonging8th.fit <- aov(score ~ as.factor(belonging), data = schoolBelonging8th)
summary(schoolBelonging8th.fit)
rm(schoolBelonging8th.fit)
schoolBelonging8th %>% 
  group_by(belonging) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolBelonging8th.avg

schoolBelonging8th.avg %>% 
  hchart(type = "column", hcaes(x = belonging, y = avgScore))%>% 
  hc_title(text = "Students' Sense of School Belonging") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx())

schoolBelonging8th$belonging <- factor(schoolBelonging8th$belonging,levels = c("High Sense of Belonging","Sense of Belonging", "Little Sense of Belonging"))
hcboxplot(x = schoolBelonging8th$score,var = schoolBelonging8th$belonging,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot of Students' Sense of School Belonging") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx()) 



## @knitr principal
# PRINCIPAL'S POV

## 8TH GRADE

princ15.stat8th <- bcg15 %>%
  select(idcntry, idschool, bcbg15a:bcbg15k) %>%
  mutate(safety = bcbg15a + bcbg15b + bcbg15c + bcbg15d + bcbg15e + 
           bcbg15f + bcbg15g + bcbg15h + bcbg15i + bcbg15j + bcbg15k) %>%
  filter(!is.na(safety)) %>%
  select(idcntry, idschool, safety) %>%
  mutate(safety.cat = ifelse(safety <= 12, "Not A Problem", 
                             ifelse(safety <= 24, "Minor Problem", 
                                    ifelse(safety <= 36, "Moderate Problem", 
                                           "Serious Problem"))))


princ15.stat8th <- left_join(princ15.stat8th, bschool, 
                             by = c("idcntry", "idschool"))
princ15.stat8th$safety.cat <- factor(princ15.stat8th$safety.cat,
                                     levels = c("Not A Problem",
                                                "Minor Problem", 
                                                "Moderate Problem", 
                                                "Serious Problem"))

princ15.stat8th.avg <- princ15.stat8th %>%
  group_by(safety.cat) %>%
  summarise(avg = mean(over.avg))

# ANOVA
princ15.stat8th.fit <- aov(over.avg ~ as.factor(safety.cat),
                           data = princ15.stat8th)
summary(princ15.stat8th.fit)
rm(princ15.stat8th.fit)

# avg histogram 


princ15.stat8th.avg %>% 
  hchart(type = "column", hcaes(x = safety.cat, y = avg %>% round(2)), 
         name = "Average Score", 
         color = "bisque") %>%
  hc_title(text = "Get A Hold of Your School, Principal",
           align = 'center', 
           style = list(fontWeight = "italic", fontSize = "20px")) %>%
  hc_xAxis(title = list(text = "School Discipline Problems", 
                        color = 'white'), 
           style = list(color = "white"), tickColor = "black",
           tickLength = 20, lineColor = 'black',
           labels = list(style = list(color = "white"))) %>%
  hc_yAxis(title = list(text = "Average Score", color = 'white'), 
           labels = list(style = list(color = "white"))) %>%
  hc_add_theme(hc_theme_db())

# box plot

hcboxplot(x = princ15.stat8th$over.avg %>% round(3),
          var = princ15.stat8th$safety.cat,
          outliers = F,
          name = "Score",
          color = "darkmagenta") %>%
  hc_title(text = "BoxPlot of School Discipline Problems") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),
           tickColor = "black",
           labels = list(style = list(color = "black", fontSize = "20px")), 
           lineColor = 'black', lineWidth = 1) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),
           tickColor = "black",
           labels = list(style = list(color = "black")), 
           lineColor = 'black', lineWidth = 1) %>% 
  hc_add_theme(hc_theme_538())



## @knitr teachers
# TEACHER'S POV

# 8TH GRADE
btm15.safe <- btm15 %>%
  select(idcntry:idlink, btbg07a:btbg07h) %>%
  mutate(safety = btbg07a + btbg07b + btbg07c + btbg07d + 
           btbg07e + btbg07f + btbg07g + btbg07h) %>%
  filter(!is.na(safety)) %>%
  select(idcntry, idschool, idteach, safety) %>%
  mutate(safety.cat = ifelse(safety <= 14, "Not A Problem", 
                             ifelse(safety <= 20, "Minor Problem", 
                                    ifelse(safety <= 26, "Moderate Problem", 
                                           "Serious Problem"))))
mteachsafe15.stat8th <- left_join(btm15.safe, bteacher, 
                                  by = c("idcntry", "idschool", "idteach")) %>%
  select(idcntry, idschool, idteach, avg = mat.avg, safety.cat)

bts15.safe <- bts15 %>%
  select(idcntry:idlink, btbg07a:btbg07h) %>%
  mutate(safety = btbg07a + btbg07b + btbg07c + btbg07d + 
           btbg07e + btbg07f + btbg07g + btbg07h) %>%
  filter(!is.na(safety)) %>%
  select(idcntry, idschool, idteach, safety) %>%
  mutate(safety.cat = ifelse(safety <= 14, "Not A Problem", 
                             ifelse(safety <= 20, "Minor Problem", 
                                    ifelse(safety <= 26, "Moderate Problem", 
                                           "Serious Problem"))))
steachsafe15.stat8th <- left_join(bts15.safe, bteacher, 
                                  by = c("idcntry", "idschool", "idteach")) %>%
  select(idcntry, idschool, idteach, avg = sci.avg, safety.cat)

teachsafe15.stat8th <- rbind(steachsafe15.stat8th, mteachsafe15.stat8th)

teachsafe15.stat8th$safety.cat <- factor(teachsafe15.stat8th$safety.cat,
                                         levels = c("Not A Problem",
                                                    "Minor Problem", 
                                                    "Moderate Problem", 
                                                    "Serious Problem"))

teachsafe15.stat8th.avg <- teachsafe15.stat8th %>%
  group_by(safety.cat) %>%
  summarise(avg = mean(avg)) 

# ANOVA
teachsafe15.stat8th.fit <- aov(avg ~ as.factor(safety.cat),
                               data = teachsafe15.stat8th)
summary(teachsafe15.stat8th.fit)
rm(teachsafe15.stat8th.fit)

# avg histogram 

teachsafe15.stat8th.avg %>% 
  hchart(type = "column", hcaes(x = safety.cat, y = avg %>% round(2)), 
         name = "Average Score", 
         color = "#9C7FDF") %>%
  hc_title(text = "The Teachers Have Spoken",
           align = 'center', 
           style = list(fontWeight = "italic", fontSize = "20px")) %>%
  hc_subtitle(text = "School Discipline is Apparently Important", 
              align = 'center', 
              style = list(color = 'white')) %>%
  hc_xAxis(title = list(text = "School Discipline Problems"), 
           style = list(color = "white"), tickColor = "black",
           tickLength = 20, lineColor = 'black',
           labels = list(style = list(color = "white"))) %>%
  hc_yAxis(title = list(text = "Average Score"), 
           style = list(color = "white"),
           labels = list(style = list(color = "white"))) %>%
  hc_add_theme(hc_theme_db())

# box plot

hcboxplot(x = teachsafe15.stat8th$avg %>% round(3),
          var = teachsafe15.stat8th$safety.cat,
          outliers = F,
          name = "Score",
          color = "#99004C") %>%
  hc_title(text = "BoxPlot of School Discipline Problems - Teachers' POV") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),
           tickColor = "black",
           labels = list(style = list(color = "black", fontSize = "20px")), 
           lineColor = 'black', lineWidth = 1) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),
           tickColor = "black",
           labels = list(style = list(color = "black")), 
           lineColor = 'black', lineWidth = 1) %>% 
  hc_add_theme(hc_theme_538())

## @knitr students
# STUDENTS' POV

# 8TH GRADE
stud15.stat8th <- bsg15 %>%
  mutate(bullying = bsbg16a + bsbg16b + bsbg16c + bsbg16d + bsbg16e + 
           bsbg16f + bsbg16g + bsbg16h + bsbg16i) %>%
  select(idcntry, idschool, idclass, idstud, bullying) %>%
  filter(!is.na(bullying)) %>%
  mutate(bullying.cat = ifelse(bullying <= 16, "At Least Once A Week", 
                               ifelse(bullying <= 23, "Once Or Twice A Month", 
                                      ifelse(bullying <= 30, "A Few Times A Year", 
                                             "Never"))))

stud15.stat8th <- left_join(stud15.stat8th, bscores, 
                            by = c("idcntry", "idstud", "idschool"))
stud15.stat8th$bullying.cat <- factor(stud15.stat8th$bullying.cat,
                                      levels = c("At Least Once A Week",
                                                 "Once Or Twice A Month", 
                                                 "A Few Times A Year", 
                                                 "Never"))

stud15.stat8th.summary <- stud15.stat8th %>%
  group_by(over.state, bullying.cat) %>%
  summarise(freq = n())
stud15.stat8th.avg <- stud15.stat8th %>%
  group_by(bullying.cat) %>%
  summarise(avg = mean(overall))

# ANOVA
stud15.stat8th.fit <- aov(overall ~ as.factor(bullying.cat),
                          data = stud15.stat8th)
summary(stud15.stat8th.fit)
rm(stud15.stat8th.fit)
# Chi-square

chisq.test(matrix(as.numeric(stud15.stat8th.summary$freq), 
                  nrow = 4, ncol = 4, byrow = T))

# avg histogram 

stud15.stat8th.avg %>% 
  hchart(type = "column", hcaes(x = bullying.cat, y = avg %>% round(2)), 
         name = "Average Score", 
         color = "#0AABE5") %>%
  hc_title(text = "Kids Who Are Bullied Are More Likely to Experience Decreased Academic Achievement",
           align = 'center', 
           style = list(fontWeight = "italic", fontSize = "20px")) %>%
  hc_xAxis(title = list(text = "How Often They're Bullied"), 
           style = list(color = "white"), tickColor = "black",
           tickLength = 20, lineColor = 'black',
           labels = list(style = list(color = "white"))) %>%
  hc_yAxis(title = list(text = "Average Score"), 
           style = list(color = "white"),
           labels = list(style = list(color = "white"))) %>%
  hc_add_theme(hc_theme_db())

# box plot


ggthemr('dust')
ggplot(stud15.stat8th) + 
  geom_boxplot(aes(y = overall, x = bullying.cat, fill = bullying.cat)) +
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text = element_blank(), 
        legend.position = "bottom") + 
  labs(title = "Boxplot of Children's Overall Score", 
       subtitle = "Based on Bullying", 
       x = "Overall Score", y = "How Frequently Students Are Bullied") + 
  coord_flip() + 
  scale_y_continuous()


## @knitr principalExperience
##### Principal Experience 

## 8th grade
bcg15 %>% 
  mutate(experience = bcbg19) %>%
  mutate(experience = ifelse(experience <= 5  & experience >= 0 ,"Less Than 5 Years",
                             ifelse(experience <= 15  ,"5-15 Years",
                                    ifelse(experience <= 30 , "15-30 Years" , "More Than 30 Years")))) %>% 
  filter(!is.na(experience)) %>% 
  select(idcntry,idschool,experience) -> principalExperience8th


left_join(principalExperience8th, schoolPerformance8th, by = c("idcntry","idschool")) -> principalExperience8th

principalExperience8th.fit <- aov(score ~ as.factor(experience), data = principalExperience8th)
summary(principalExperience8th.fit)
rm(principalExperience8th.fit)

principalExperience8th %>% 
  group_by(experience) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> principalExperience8th.avg

principalExperience8th.avg %>% 
  hchart(type = "column", hcaes(x = experience, y = avgScore),color = "#20B2AA")%>% 
  hc_title(text = "Principal Experience") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_google())

principalExperience8th$experience <- factor(principalExperience8th$experience,levels = c("Less Than 5 Years","5-15 Years", "15-30 Years" , "More Than 30 Years"))
hcboxplot(x = principalExperience8th$score,var = principalExperience8th$experience,outliers = F,name = "Score",color = "#20B2AA") %>%
  hc_title(text = "BoxPlot of Principal Experience") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx())   


## @knitr principalEducation
#### Principal Education

## 8th grade
bcg15 %>% 
  mutate(education = bcbg21) %>%
  mutate(education = ifelse(education == 1 ,"Didn't Complete Bachelor's",
                            ifelse(education == 2 ,"Bachelor's or Equivalent",
                                   ifelse(education == 3 , "Master's or Equivalent" , "Doctor or Equivalent")))) %>% 
  filter(!is.na(education)) %>% 
  select(idcntry,idschool,education) -> principalEducation8th


left_join(principalEducation8th, schoolPerformance8th, by = c("idcntry","idschool")) -> principalEducation8th

principalEducation8th.fit <- aov(score ~ as.factor(education), data = principalEducation8th)
summary(principalEducation8th.fit)
rm(principalEducation8th.fit)

principalEducation8th %>% 
  group_by(education) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> principalEducation8th.avg

principalEducation8th.avg %>% 
  hchart(type = "column", hcaes(x = education, y = avgScore),color = "#FF6347")%>% 
  hc_title(text = "Principal Education") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())

principalEducation8th$education <- factor(principalEducation8th$education,levels = c("Didn't Complete Bachelor's","Bachelor's or Equivalent", "Master's or Equivalent" , "Doctor or Equivalent"))
hcboxplot(x = principalEducation8th$score,var = principalEducation8th$education,outliers = F,name = "Score",color = "#FF6347") %>%
  hc_title(text = "BoxPlot of Principal Education") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())   



## @knitr teachersExperience
##### Teachers Experience

## 8th grade
btm15 %>% 
  mutate(experience = btbg01) %>%
  mutate(experience = ifelse(experience <= 5  & experience >= 0 ,"Less Than 5 Years",
                             ifelse(experience <= 15  ,"5-15 Years",
                                    ifelse(experience <= 30 , "15-30 Years" , "More Than 30 Years")))) %>% 
  filter(!is.na(experience)) %>% 
  select(idcntry,idschool,idteach,experience) -> teacherExperience8th



left_join(teacherExperience8th, teacherPerformance8th, by = c("idcntry","idschool","idteach")) -> teacherExperience8th

teacherExperience8th.fit <- aov(score ~ as.factor(experience), data = teacherExperience8th)
summary(teacherExperience8th.fit)
rm(teacherExperience8th.fit)

teacherExperience8th %>% 
  group_by(experience) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> teacherExperience8th.avg

teacherExperience8th.avg %>% 
  hchart(type = "bar", hcaes(x = experience, y = avgScore),color = "#800000")%>% 
  hc_title(text = "Teachers Experience") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())

teacherExperience8th$experience <- factor(teacherExperience8th$experience,levels = c("Less Than 5 Years","5-15 Years", "15-30 Years" , "More Than 30 Years"))
hcboxplot(x = teacherExperience8th$score,var = teacherExperience8th$experience,outliers = F,name = "Score",color = "#800000") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot of Teachers Experience") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())   


## @knitr teachersEducation
#### Teacher Education
## 8th grade
btm15 %>% 
  mutate(education = btbg04) %>%
  mutate(education = ifelse(education == 1 ,"Didn't Complete Bachelor's",
                            ifelse(education == 2 ,"Bachelor's or Equivalent",
                                   ifelse(education == 3 , "Master's or Equivalent" , "Doctor or Equivalent")))) %>% 
  filter(!is.na(education)) %>% 
  select(idcntry,idschool,idteach,education) -> teacherEducation8th


left_join(teacherEducation8th, teacherPerformance8th, by = c("idcntry","idschool","idteach")) -> teacherEducation8th

teacherEducation8th.fit <- aov(score ~ as.factor(education), data = teacherEducation8th)
summary(teacherEducation8th.fit)
rm(teacherEducation8th.fit)

teacherEducation8th %>% 
  group_by(education) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> teacherEducation8th.avg

teacherEducation8th.avg %>% 
  hchart(type = "bar", hcaes(x = education, y = avgScore),color = "#5F9EA0")%>% 
  hc_title(text = "Teachers Education") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx())


teacherEducation8th$education <- factor(teacherEducation8th$education,levels = c("Didn't Complete Bachelor's","Bachelor's or Equivalent", "Master's or Equivalent" , "Doctor or Equivalent"))
hcboxplot(x = teacherEducation8th$score,var = teacherEducation8th$education,outliers = F,name = "Score",color = "#5F9EA0") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot of Teachers Education") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx())   



## @knitr homework
#### Amount of Homework
## 8 th grade

bsg15 %>%
  mutate(homework = bsbm25aa + bsbs25ab)  %>% 
  mutate(homework = ifelse(homework <= 2,"Everyday",
                           ifelse(homework <= 4,"3 or 4 Times a Week",
                                  ifelse(homework <= 6, "1 or 2 Times a Week",
                                         ifelse(homework <= 8,"Less Than Once a Week","Never"))))) %>% 
  filter(!is.na(homework)) %>% 
  select(idcntry,idschool,homework) -> homework8th

left_join(homework8th, schoolPerformance8th, by = c("idcntry","idschool")) -> homework8th

homework8th.fit <- aov(score ~ as.factor(homework), data = homework8th)
summary(homework8th.fit)
rm(homework8th.fit)
homework8th %>% 
  group_by(homework) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> homework8th.avg

homework8th.avg %>% 
  hchart(type = "column", hcaes(x = homework, y = avgScore))%>% 
  hc_title(text = "Amount of Homework") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_google())

homework8th$homework <- factor(homework8th$homework,levels = c("Never","Less Than Once 
                                                               a Week","1 or 2 Times a Week",
                                                               "3 or 4 Times a Week","Everyday"))
hcboxplot(x = homework8th$score,var = homework8th$homework,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot of Amount of Homework") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx()) 


## @knitr teachingLimited
####  Teaching Limited by Student Needs

## 8th grade
btm15 %>% 
  mutate(limit = btbg15a + btbg15b+ btbg15c+ btbg15d+ btbg15e+ btbg15f+ btbg15g) %>%
  mutate(limit = ifelse(limit <= 10 ,"Not At All",
                        ifelse(limit <= 17 ,"Some","A Lot"))) %>% 
  filter(!is.na(limit)) %>% 
  select(idcntry,idschool,idteach,limit) -> teacherLimit8th


left_join(teacherLimit8th, teacherPerformance8th, by = c("idcntry","idschool","idteach")) -> teacherLimit8th

teacherLimit8th.fit <- aov(score ~ as.factor(limit), data = teacherLimit8th)
summary(teacherLimit8th.fit)
rm(teacherLimit8th.fit)
teacherLimit8th %>% 
  group_by(limit) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore)) -> teacherLimit8th.avg

teacherLimit8th.avg %>% 
  hchart(type = "column", hcaes(x = limit, y = avgScore))%>% 
  hc_title(text = "Teaching Limited by Student Needs") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_elementary())


teacherLimit8th$limit <- factor(teacherLimit8th$limit,levels = c("Not At All","Some","A Lot"))
hcboxplot(x = teacherLimit8th$score,var = teacherLimit8th$limit,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot of Teaching Limited by Student Needs") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_elementary()) 

## @knitr absences
####  Students Absences

## 8th grade
bsg15 %>% 
  mutate(absent = bsbg11) %>%
  mutate(absent = ifelse(absent == 1 ,"Once A Week Or More",
                         ifelse(absent == 2 ,"Once Every Two Weeks",
                                ifelse(absent == 3,"Once A Month","Never Or Almost Never")))) %>% 
  filter(!is.na(absent)) %>% 
  select(idcntry,idschool,absent) -> studentAbsences8th


left_join(studentAbsences8th, schoolPerformance8th, by = c("idcntry","idschool")) -> studentAbsences8th

studentAbsences8th.fit <- aov(score ~ as.factor(absent), data = studentAbsences8th)
summary(studentAbsences8th.fit)
rm(studentAbsences8th.fit)
studentAbsences8th %>% 
  group_by(absent) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentAbsences8th.avg

studentAbsences8th.avg %>% 
  hchart(type = "bar", hcaes(x = absent, y = avgScore),color= "#B22222")%>% 
  hc_title(text = "Students Absences") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())

studentAbsences8th$absent <- factor(studentAbsences8th$absent,levels = c("Once A Week Or More","Once Every Two Weeks","Once A Month","Never Or Almost Never"))
hcboxplot(x = studentAbsences8th$score,var = studentAbsences8th$absent,outliers = F,name = "Score",color= "#B22222") %>%
  hc_title(text = "BoxPlot of Students Absences") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())  


## @knitr engagement
# ENGAGEMENT

# 8TH GRADE
mstudles15.stat8th <- bsg15 %>%
  mutate(lessons = bsbm18a + bsbm18b + bsbm18c + bsbm18d + bsbm18e + 
           bsbm18f + bsbm18g + bsbm18h + bsbm18i + bsbm18j) %>%
  select(idcntry, idschool , idstud, lessons) %>%
  filter(!is.na(lessons)) %>%
  mutate(lessons.cat = ifelse(lessons <= 17, "Very Engaging Teaching", 
                              ifelse(lessons <= 25, "Engaging Teaching",
                                     ifelse(lessons <= 32, "Less Than Engaging Teaching", 
                                            "Not Engaging Teaching"))))
mstudles15.stat8th$lessons.cat <- factor(mstudles15.stat8th$lessons.cat,
                                         levels = c("Very Engaging Teaching",
                                                    "Engaging Teaching", 
                                                    "Less Than Engaging Teaching", 
                                                    "Not Engaging Teaching"))

mstudles15.stat8th <- left_join(mstudles15.stat8th, bscores, 
                                by = c("idcntry", "idstud", "idschool"))

mstudles15.stat8th.summary <- mstudles15.stat8th %>%
  group_by(mat.state, lessons.cat) %>%
  summarise(freq = n())
mstudles15.stat8th.avg <- mstudles15.stat8th %>%
  group_by(lessons.cat) %>%
  summarise(avg = mean(mat))


# ANOVA
mstudles15.stat8th.fit <- aov(overall ~ as.factor(lessons.cat),
                              data = mstudles15.stat8th)
summary(mstudles15.stat8th.fit)
rm(mstudles15.stat8th.fit)
# Chi-square

chisq.test(matrix(as.numeric(mstudles15.stat8th.summary$freq), 
                  nrow = 4, ncol = 4, byrow = T))

# avg histogram 

mstudles15.stat8th.avg %>% 
  hchart(type = "column", hcaes(x = lessons.cat, y = avg %>% round(2)), 
         name = "Average Score", 
         color = "#982929", 
         borderColor = 'black') %>%
  hc_title(text = "Apparently Student Engagement in Class is NOT That Important",
           align = 'center', 
           style = list(fontWeight = "italic", fontSize = "20px")) %>%
  hc_xAxis(title = list(text = "Teacher' Engagement Level"), 
           style = list(color = "white"), tickColor = "black",
           tickLength = 20, lineColor = 'black',
           labels = list(style = list(color = "white"))) %>%
  hc_yAxis(title = list(text = "Average Score"), 
           style = list(color = "white"),
           labels = list(style = list(color = "white"))) %>%
  hc_add_theme(hc_theme_db())

# box plot
ggthemr('dust')
ggplot(mstudles15.stat8th) + 
  geom_boxplot(aes(y = mat, x = lessons.cat, fill = lessons.cat)) +
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text = element_blank(), 
        legend.position = "bottom") + 
  labs(title = "Boxplot of Children's Overall Score", 
       subtitle = "Based on Engagement in Class", 
       x = "Overall Score", y = "Engagement Level of Teachers") + 
  coord_flip() + 
  scale_y_continuous()


## @knitr attitude
# LEARNING ATTITUDE

## 8TH GRADE
mstudlearn15.stat8th <- bsg15 %>%
  mutate(learning = bsbm17a + bsbm17b + bsbm17c + bsbm17d + bsbm17e + 
           bsbm17f + bsbm17g + bsbm17h + bsbm17i) %>%
  select(idcntry, idschool, idstud, learning) %>%
  filter(!is.na(learning)) %>%
  mutate(learning.cat = ifelse(learning <= 16, "Love Learning", 
                               ifelse(learning <= 23, "Like Learning", 
                                      ifelse(learning <= 30, "Dislike Learning", 
                                             "Hate Learning"))))
mstudlearn15.stat8th$learning.cat <- factor(mstudlearn15.stat8th$learning.cat,
                                            levels = c("Love Learning",
                                                       "Like Learning", 
                                                       "Dislike Learning", 
                                                       "Hate Learning"))

mstudlearn15.stat8th <- left_join(mstudlearn15.stat8th, bscores, 
                                  by = c("idcntry", "idstud", "idschool"))

mstudlearn15.stat8th.summary <- mstudlearn15.stat8th %>%
  group_by(mat.state, learning.cat) %>%
  summarise(freq = n())
mstudlearn15.stat8th.avg <- mstudlearn15.stat8th %>%
  group_by(learning.cat) %>%
  summarise(avg = mean(mat))

# ANOVA
mstudlearn15.stat8th.fit <- aov(overall ~ as.factor(learning.cat),
                                data = mstudlearn15.stat8th)
summary(mstudlearn15.stat8th.fit)
rm(mstudlearn15.stat8th.fit)
# Chi-square

chisq.test(matrix(as.numeric(mstudlearn15.stat8th.summary$freq), 
                  nrow = 4, ncol = 4, byrow = T))

# avg histogram 

mstudlearn15.stat8th.avg %>% 
  hchart(type = "column", hcaes(x = learning.cat, y = avg))

mstudlearn15.stat8th.avg %>% 
  hchart(type = "column", hcaes(x = learning.cat, y = avg %>% round(2)), 
         name = "Average Score", 
         color = "#287635", 
         borderColor = 'black') %>%
  hc_title(text = "Watch That Attitude 😠",
           align = 'center', 
           style = list(fontWeight = "italic", fontSize = "20px")) %>%
  hc_xAxis(title = list(text = "Attitude Towards Learning"), 
           style = list(color = "white"), tickColor = "black",
           tickLength = 20, lineColor = 'black',
           labels = list(style = list(color = "white"))) %>%
  hc_yAxis(title = list(text = "Average Score"), 
           style = list(color = "white"),
           labels = list(style = list(color = "white"))) %>%
  hc_add_theme(hc_theme_db())

# box plot
ggthemr('dust')
ggplot(mstudlearn15.stat8th) + 
  geom_boxplot(aes(y = mat, x = learning.cat, fill = learning.cat)) +
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text = element_blank(), 
        legend.position = "bottom") + 
  labs(title = "Boxplot of Children's Overall Score", 
       subtitle = "Based on Attitude Towards Mathematics", 
       x = "Overall Score", y = "") + 
  guides(fill = guide_legend(title = "")) + 
  coord_flip() + 
  scale_y_continuous()

## @knitr confidence
# CONFIDENCE

## 8TH GRADE
mstudconf15.stat8th <- bsg15 %>%
  mutate(confidence = bsbm19a + bsbm19b + bsbm19c + bsbm19d + bsbm19e + 
           bsbm19f + bsbm19g + bsbm19h + bsbm19i) %>%
  select(idcntry, idschool, idstud, confidence) %>%
  filter(!is.na(confidence)) %>%
  mutate(confidence.cat = ifelse(confidence <= 18, "Very Confident", 
                                 ifelse(confidence <= 27, "Confident", 
                                        "Not Confident")))
mstudconf15.stat8th$confidence.cat <- factor(mstudconf15.stat8th$confidence.cat,
                                             levels = c("Very Confident",
                                                        "Confident", 
                                                        "Not Confident"))

mstudconf15.stat8th <- left_join(mstudconf15.stat8th, bscores, 
                                 by = c("idcntry", "idstud", "idschool"))
mstudconf15.stat8th.summary <- mstudconf15.stat8th %>%
  group_by(mat.state, confidence.cat) %>%
  summarise(freq = n()) 

mstudconf15.stat8th.avg <- mstudconf15.stat8th %>%
  group_by(confidence.cat) %>%
  summarise(avg = mean(mat))

# ANOVA
mstudconf15.stat8th.fit <- aov(overall ~ as.factor(confidence.cat),
                               data = mstudconf15.stat8th)
summary(mstudconf15.stat8th.fit)
rm(mstudconf15.stat8th.fit)
# Chi-square

chisq.test(matrix(as.numeric(mstudconf15.stat8th.summary$freq), 
                  ncol = 3, byrow = T))



# avg histogram 

mstudconf15.stat8th.avg %>% 
  hchart(type = "column", hcaes(x = confidence.cat, y = avg %>% round(2)), 
         name = "Average Score", 
         color = "#05C0EF", 
         borderColor = 'black') %>%
  hc_title(text = "Be Confident, Not Cocky",
           align = 'center', 
           style = list(fontWeight = "italic", fontSize = "20px")) %>%
  hc_xAxis(title = list(text = "Confidence in Math"), 
           style = list(color = "white"), tickColor = "black",
           tickLength = 20, lineColor = 'black',
           labels = list(style = list(color = "white"))) %>%
  hc_yAxis(title = list(text = "Average Score"), 
           style = list(color = "white"),
           labels = list(style = list(color = "white"))) %>%
  hc_add_theme(hc_theme_db())

# box plot
ggthemr('dust')
ggplot(mstudconf15.stat8th) + 
  geom_boxplot(aes(y = mat, x = confidence.cat, fill = confidence.cat)) +
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text = element_blank(), 
        legend.position = "bottom") + 
  labs(title = "Boxplot of Children's Overall Score", 
       subtitle = "Based on Their Confidence in Mathematics", 
       x = "Overall Score", y = "") + 
  guides(fill = guide_legend(title = "")) + 
  coord_flip() + 
  scale_y_continuous()

## @knitr value
# VALUE 
mstudval15.stat8th <- bsg15 %>%
  mutate(value = bsbm20a + bsbm20b + bsbm20c + bsbm20d + bsbm20e + 
           bsbm20f + bsbm20g + bsbm20h + bsbm20i) %>%
  select(idcntry, idschool, idstud, value) %>%
  filter(!is.na(value)) %>%
  mutate(value.cat = ifelse(value <= 18, "Strongly Value", 
                            ifelse(value <= 27, "Moderately Value", 
                                   "Do Not Value")))
mstudval15.stat8th$value.cat <- factor(mstudval15.stat8th$value.cat,
                                       levels = c("Strongly Value",
                                                  "Moderately Value", 
                                                  "Do Not Value"))

mstudval15.stat8th <- left_join(mstudval15.stat8th, bscores, 
                                by = c("idcntry", "idstud", "idschool"))
mstudval15.stat8th.summary <- mstudval15.stat8th %>%
  group_by(mat.state, value.cat) %>%
  summarise(freq = n()) 
mstudval15.stat8th.avg <- mstudval15.stat8th %>%
  group_by(value.cat) %>%
  summarise(avg = mean(mat))

# ANOVA
mstudval15.stat8th.fit <- aov(overall ~ as.factor(value.cat),
                              data = mstudval15.stat8th)
summary(mstudval15.stat8th.fit)
rm(mstudval15.stat8th.fit)
# Chi-square

chisq.test(matrix(as.numeric(mstudval15.stat8th.summary$freq), 
                  ncol = 3, byrow = T))


# avg histogram 


mstudval15.stat8th.avg %>% 
  hchart(type = "column", hcaes(x = value.cat, y = avg %>% round(2)), 
         name = "Average Score", 
         color = "#71237D", 
         borderColor = 'black') %>%
  hc_title(text = "Relationship Between Valuing Math and Performance in Math",
           align = 'center', 
           style = list(fontWeight = "italic", fontSize = "20px")) %>%
  hc_xAxis(title = list(text = "How Much Students Value Math"), 
           style = list(color = "white"), tickColor = "black",
           tickLength = 20, lineColor = 'black',
           labels = list(style = list(color = "white"))) %>%
  hc_yAxis(title = list(text = "Average Score"), 
           style = list(color = "white"),
           labels = list(style = list(color = "white"))) %>%
  hc_add_theme(hc_theme_db())

# box plot
ggthemr('dust')
ggplot(mstudval15.stat8th) + 
  geom_boxplot(aes(y = mat, x = value.cat, fill = value.cat)) +
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text = element_blank(), 
        legend.position = "bottom") + 
  labs(title = "Boxplot of Children's Overall Score", 
       subtitle = "Based on How Much They Value Mathematics", 
       x = "Overall Score", y = "") + 
  guides(fill = guide_legend(title = "")) + 
  coord_flip() + 
  scale_y_continuous()
