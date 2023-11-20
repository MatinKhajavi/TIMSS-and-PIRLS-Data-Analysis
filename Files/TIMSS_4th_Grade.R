## @knitr dataSection
library(jsonlite)
library(tidyverse)
library(highcharter)
library(plotly)
library(ggthemr)
####################################################################################
##########################   TIMSS  ################################################
####################################################################################

## WORDWIDE


######### 2015 TIMSS ########

acg15 <- read_rds("../2015/acg.rds")
asa15 <- read_rds("../2015/asa.rds")
asg15 <- read_rds("../2015/asg.rds")
ash15 <- read_rds("../2015/ash.rds")
#asr15 <- read_rds("../2015/asr.rds")
ast15 <- read_rds("../2015/ast.rds")
atg15 <- read_rds("../2015/atg.rds")

## 2015 TIMSS 4TH GRADE

## STUDENTS

ascore <- asa15 %>%
  mutate(mat = (asmmat01+ asmmat02 + asmmat03 + asmmat04 + asmmat05) / 5, 
         sci = (asssci01 + asssci02 + asssci03 + asssci04 + asssci05) / 5) %>%
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
ateacher <- ast15 %>%
  mutate(mat = (asmmat01 + asmmat02 + asmmat03 + asmmat04 + asmmat05) / 5, 
         sci = (asssci01 + asssci02 + asssci03 + asssci04 + asssci05) / 5) %>%
  mutate(overall = (mat + sci) / 2) %>%
  group_by(idcntry, idschool, idteach) %>%
  summarise(mat.avg = mean(mat), 
            sci.avg = mean(sci), 
            over.avg = mean(overall))

## SCHOOLS
aschool <- ascore %>% 
  group_by(idcntry,idschool) %>% 
  summarise(over.avg = mean(overall), 
            mat.avg = mean(mat), 
            sci.avg = mean(sci))

student.stat4th <- read_rds("../2015/asa.rds") %>%
  group_by(idcntry, idstud, idschool) %>%
  mutate(mat = mean(asmmat01:asmmat05), 
         sci = mean(asssci01:asssci05)) %>%
  select(idcntry, idstud, idschool, mat, sci) %>%
  mutate(overall = (mat + sci) / 2)

student.stat4th %>% 
  group_by(idcntry,idschool) %>% 
  summarise(score = mean(overall)) -> schoolPerformance4th

ast15 %>% 
  mutate(mat = (asmmat01+asmmat02+ asmmat03+asmmat04+asmmat05)/5, 
         sci = (asssci01+asssci02+asssci03+asssci04+asssci05)/5) %>%
  select(idcntry, idstud, idschool,idteach, mat, sci) %>%
  mutate(overall = (mat + sci) / 2) %>% 
  group_by(idcntry,idschool,idteach) %>% 
  summarise(score = mean(overall))-> teacherPerformance4th


## @knitr physicalResources
### PHYSICAL RESOURCES ###

### 4TH GRADE
asg15.res <- asg15 %>%
  mutate(desk = ifelse(asbg05c == 1, 1, 0), 
         room = ifelse(asbg05d == 1, 1, 0), 
         computer = ifelse(asbg05a == 1, 1, 0), 
         internet = ifelse(asbg05e == 1, 1, 0)) %>%
  mutate(resources = desk + room + computer + internet + asbg04) %>%
  filter(!is.na(resources)) %>%
  mutate(res.cat = ifelse(resources >= 7, "Many", 
                          ifelse(resources >= 4, "Some", "Few"))) %>%
  select(idcntry, idstud, idschool, res.cat)

home15.stat4th <- left_join(asg15.res, 
                            ascore, 
                            by = c("idcntry", "idstud", "idschool"))

home15.stat4th$res.cat <- factor(home15.stat4th$res.cat,
                                 levels = c("Many",
                                            "Some", 
                                            "Few"))

home15.stat4th.summery <- home15.stat4th %>%
  group_by(over.state, res.cat) %>%
  summarise(freq = n()) %>%
  mutate(category = paste("Overall Score = ", over.state, 
                          ", Physical Resources = ", res.cat))

home15.stat4th.avg <- home15.stat4th %>%
  group_by(res.cat) %>%
  summarise(avg = mean(overall))

# ANOVA
home15.stat4th.fit <- aov(overall ~ as.factor(res.cat), data = home15.stat4th)
summary(home15.stat4th.fit)
rm(home15.stat4th.fit)

# Chi-square

chisq.test(matrix(as.numeric(home15.stat4th.summery$freq), nrow = 3), 
           simulate.p.value = T)

## @knitr avg
home15.stat4th.avg %>% 
  hchart(type = "column", hcaes(x = res.cat, y = avg %>% round(2)), color = 'purple') %>%
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


# box plot
ggthemr('pale')
ggplot(home15.stat4th) + 
  geom_boxplot(aes(y = overall, x = res.cat, fill = res.cat)) + 
  theme_minimal() + 
  labs(title = "Boxplot of Student Scores", 
       x = "Resources", y = "Overall Score") +
  guides(fill = guide_legend(title = "Resources")) + 
  scale_y_continuous()

rm(home15.stat4th, home15.stat4th.avg, home15.stat4th.summery, 
   asg15.res)

## @knitr parentalEducation
### PARENTAL EDUCATION ### 

## 4TH GRADE
ash15.pedu <- ash15 %>%
  mutate(asbh20a = ifelse(is.na(asbh20a) | (asbh20a == 9), 0, asbh20a), 
         asbh20b = ifelse(is.na(asbh20b) | (asbh20b == 9), 0, asbh20b)) %>%
  mutate(asbh20a = ifelse(asbh20a == 8, asbh20b, asbh20a)) %>%
  mutate(asbh20b = ifelse(asbh20b == 8, asbh20a, asbh20b)) %>%
  mutate(hedu = ifelse(asbh20a > asbh20b, asbh20a, asbh20b)) %>%
  mutate(hedu = ifelse(hedu == 1, "did not go to school", 
                       ifelse(hedu == 2, "Lower secondary", 
                              ifelse(hedu == 3, "Upper secondary",
                                     ifelse(hedu == 4, "Post secondary, non-tertiary", 
                                            ifelse(hedu == 5, "Short-cycle tertiary", 
                                                   ifelse(hedu == 6, "Bachelor’s or equivalent",
                                                          "Postgraduate degree"))))))) %>%
  filter(hedu != 8) %>%
  select(idcntry, idstud, idschool, hedu)

parent15.stat4th <- left_join(ash15.pedu, 
                              ascore, 
                              by = c("idcntry", "idstud", "idschool"))

parent15.stat4th$hedu <- factor(parent15.stat4th$hedu,
                                levels = c("did not go to school",
                                           "Lower secondary", 
                                           "Upper secondary", 
                                           "Post secondary, non-tertiary", 
                                           "Short-cycle tertiary", 
                                           "Bachelor’s or equivalent", 
                                           "Postgraduate degree"))

parent15.stat4th.summery <- parent15.stat4th %>%
  group_by(over.state, hedu) %>%
  summarise(freq = n()) %>%
  mutate(category = paste("Overall Score = ", over.state, 
                          ", Highest Parental Education = ", hedu))


parent15.stat4th.avg <- parent15.stat4th %>%
  group_by(hedu) %>%
  summarise(avg = mean(overall))

# ANOVA
parent15.stat4th.fit <- aov(overall ~ as.factor(hedu),
                            data = parent15.stat4th)
summary(parent15.stat4th.fit)
rm(parent15.stat4th.fit)

# Chi-square

chisq.test(matrix(as.numeric(parent15.stat4th.summery$freq), 
                  ncol = 7, byrow = T), simulate.p.value = T)

## @knitr avg1
# avg histogram 

parent15.stat4th.avg %>% 
  hchart(type = "column", hcaes(x = hedu, y = round(avg, 2)), 
         name = "Average Score", 
         color = "gold") %>%
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
ggthemr('flat')
ggplot(parent15.stat4th) + 
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

rm(parent15.stat4th, parent15.stat4th.avg, parent15.stat4th.summery, ash15.pedu)

## @knitr speakLang
### SPEAK LANGUAGE AT HOME ### 

## 4TH GRADE 

asg15.lang <- asg15 %>%
  filter(!is.na(asbg03)) %>%
  mutate(language = ifelse(asbg03 == 1, "Always", 
                           ifelse(asbg03 == 2, "Almost Always", 
                                  ifelse(asbg03 == 3, "Sometimes", "Never"))))

language15.stat4th <- left_join(asg15.lang, ascore, 
                                by = c("idcntry", "idstud", "idschool"))

language15.stat4th$language <- factor(language15.stat4th$language,
                                      levels = c("Always",
                                                 "Almost Always", 
                                                 "Sometimes", 
                                                 "Never"))

language15.stat4th.summary <- language15.stat4th %>%
  group_by(over.state, language) %>%
  summarise(freq = n())

language15.stat4th.avg <- language15.stat4th %>%
  group_by(language) %>%
  summarise(avg = mean(overall))

# ANOVA
language15.stat4th.fit <- aov(overall ~ as.factor(language),
                              data = language15.stat4th)
summary(language15.stat4th.fit)

rm(language15.stat4th.fit)
# Chi-square

chisq.test(matrix(as.numeric(language15.stat4th.summary$freq), 
                  nrow = 4, byrow = T))


## @knitr avg2
# avg histogram 


language15.stat4th.avg %>% 
  hchart(type = "column", hcaes(x = language, y = avg %>% round(2)), 
         name = "Average Score", 
         color = "#20B2AA") %>%
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
ggplot(language15.stat4th) + 
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

rm(language15.stat4th.avg, language15.stat4th, language15.stat4th.summary, asg15.lang)


## @knitr economic
#### Economic background of students
## 4th grade
acg15 %>% 
  mutate(economicState = ifelse(acbg03b >=3,
                                ifelse(acbg03a <= 2,"More Affluent","Neither More Affluent Nor More Disadvantaged"),
                                ifelse(acbg03a >= 3,"More Disadvantaged","Neither More Affluent Nor More Disadvantaged"))) %>% 
  filter(!is.na(economicState)) %>% 
  select(idcntry,idschool,economicState) -> EconomicBackground4th


left_join(EconomicBackground4th, schoolPerformance4th, by = c("idcntry","idschool")) -> EconomicBackground4th

EconomicBackground.fit <- aov(score ~ as.factor(economicState), data = EconomicBackground4th)
summary(EconomicBackground.fit)
rm(EconomicBackground.fit)

EconomicBackground4th %>% 
  group_by(economicState) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> EconomicBackground4th.avg

EconomicBackground4th.avg %>% 
  hchart(type = "column", hcaes(x = economicState, y = avgScore),name = 'Average Score') %>% 
  hc_title(text = "Economic Background Of Students") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "Economic State")) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",tickLength = 5,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_flat())

EconomicBackground4th$economicState <- factor(EconomicBackground4th$economicState,levels = c("More Affluent","Neither More Affluent Nor More Disadvantaged","More Disadvantaged"))
hcboxplot(x = EconomicBackground4th$score,var = EconomicBackground4th$economicState) %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot of the Economic Background") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "Economic State")) %>% 
  hc_add_theme(hc_theme_flat())

## @knitr skills
######## Schools Where Students Enter the Primary Grades with Literacy and Numeracy Skills
##4th rade
acg15 %>% 
  mutate(skillscore = acbg18a+acbg18b +acbg18c + acbg18d + acbg18e +acbg18f +acbg18g + acbg18h + acbg18i + acbg18j + acbg18k) %>%
  mutate(skillscore = ifelse(skillscore > 38 ,"More than 75% Enter with Skills",
                             ifelse(skillscore > 17,"25-75% Enter with Skills", "Less than 25% Enter with Skills"))) %>% 
  filter(!is.na(skillscore)) %>% 
  select(idcntry,idschool,skillscore) -> studentBackgroundSkill4th


left_join(studentBackgroundSkill4th, schoolPerformance4th, by = c("idcntry","idschool")) -> studentBackgroundSkill4th

studentBackgroundSkill4th.fit <- aov(score ~ as.factor(skillscore), data = studentBackgroundSkill4th)
summary(studentBackgroundSkill4th.fit)
rm(studentBackgroundSkill4th.fit)

studentBackgroundSkill4th %>% 
  group_by(skillscore) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentBackgroundSkill4th.avg

studentBackgroundSkill4th.avg %>% 
  hchart(type = "column", hcaes(x = skillscore, y = avgScore),name = "Average Score") %>% 
  hc_title(text = "Entering the Primary Grades with Literacy and Numeracy Skills") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",tickLength = 5,tickWidth = 2,
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_elementary())


studentBackgroundSkill4th$skillscore <- factor(studentBackgroundSkill4th$skillscore,levels = c("More than 75% Enter with Skills","25-75% Enter with Skills","Less than 25% Enter with Skills"))
hcboxplot(x = studentBackgroundSkill4th$score,var = studentBackgroundSkill4th$skillscore,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "")) %>% 
  hc_add_theme(hc_theme_elementary()) 

## @knitr shortage
####### Instruction Affected by Resource Shortages

### 4th grade

acg15 %>% 
  mutate(resources = acbg14aa+acbg14ab +acbg14ac + acbg14ad + acbg14ae +acbg14af +acbg14ag + acbg14ah + 
           acbg14ai + acbg14ba + acbg14bb + acbg14bc + acbg14bd + acbg14be + acbg14ca + acbg14cb + acbg14cc+ acbg14cd) %>%
  mutate(resources = ifelse(resources < 25 ,"Not Affected",
                            ifelse(resources < 60 ,"Affected", "Affected A Lot"))) %>% 
  filter(!is.na(resources)) %>% 
  select(idcntry,idschool,resources) -> resourceShortage4th


left_join(resourceShortage4th, schoolPerformance4th, by = c("idcntry","idschool")) -> resourceShortage4th

resourceShortage4th.fit <- aov(score ~ as.factor(resources), data = resourceShortage4th)
summary(resourceShortage4th.fit)
rm(resourceShortage4th.fit)
resourceShortage4th %>% 
  group_by(resources) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> resourceShortage4th.avg

resourceShortage4th.avg %>% 
  hchart(type = "column", hcaes(x = resources, y = avgScore)) %>% 
  hc_title(text = "Instruction Affected by Resource Shortages") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_db())

resourceShortage4th$resources <- factor(resourceShortage4th$resources,levels = c("Not Affected","Affected", "Affected A Lot"))
hcboxplot(x = resourceShortage4th$score,var = resourceShortage4th$resources,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot of Instruction Affected by Resource Shortages") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_db()) 


## @knitr conditions
########### Problems with School Conditions and Resources
### 4th grade
atg15 %>% 
  mutate(condition = atbg08a + atbg08b + atbg08c + atbg08d + atbg08e + atbg08f + atbg08g) %>%
  mutate(condition = ifelse(condition < 8 ,"Hardly Any Problems",
                            ifelse(condition < 17 ,"Minor Problems", "Moderate to Severe Problemm"))) %>% 
  filter(!is.na(condition)) %>% 
  select(idcntry,idschool,idteach,condition) -> schoolcondition4th


left_join(schoolcondition4th, teacherPerformance4th, by = c("idcntry","idschool","idteach")) -> schoolcondition4th

schoolcondition4th.fit <- aov(score ~ as.factor(condition), data = schoolcondition4th)
summary(schoolcondition4th.fit)
rm(schoolcondition4th.fit)
schoolcondition4th %>% 
  group_by(condition) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolcondition4th.avg

schoolcondition4th.avg %>% 
  hchart(type = "column", hcaes(x = condition, y = avgScore),color = "#2980b9")%>% 
  hc_title(text = "Problems with School Conditions and Resources") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 

schoolcondition4th$condition <- factor(schoolcondition4th$condition,levels = c("Hardly Any Problems","Minor Problems", "Moderate to Severe Problemm"))
hcboxplot(x = schoolcondition4th$score,var = schoolcondition4th$condition,outliers = T,name = "Score",color = "#2980b9") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot Problems with School Conditions and Resources") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 

## @knitr emphasis
####### School Emphasis on Academic Success
## 4th grade
acg15 %>% 
  mutate(emphasis = acbg15a + acbg15b + acbg15c+ acbg15d+ acbg15e+ acbg15f+ acbg15g+ acbg15h+ acbg15i+
           acbg15j + acbg15k + acbg15l + acbg15m) %>%
  mutate(emphasis = ifelse(emphasis < 15 ,"Very High Emphasis",
                           ifelse(emphasis < 28 ,"High Emphasis",
                                  ifelse(emphasis < 41 , "Medium Emphasis" , "Low Emphasis")))) %>% 
  filter(!is.na(emphasis)) %>% 
  select(idcntry,idschool,emphasis) -> schoolEmphasis4th


left_join(schoolEmphasis4th, schoolPerformance4th, by = c("idcntry","idschool")) -> schoolEmphasis4th

schoolEmphasis4th.fit <- aov(score ~ as.factor(emphasis), data = schoolEmphasis4th)
summary(schoolEmphasis4th.fit)
rm(schoolEmphasis4th.fit)
schoolEmphasis4th %>% 
  group_by(emphasis) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolEmphasis4th.avg

schoolEmphasis4th.avg %>% 
  hchart(type = "column", hcaes(x = emphasis, y = avgScore),color = "#393029")%>% 
  hc_title(text = "School Emphasis on Academic Success") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 

schoolEmphasis4th$emphasis <- factor(schoolEmphasis4th$emphasis,levels = c("Very High Emphasis","High Emphasis","Medium Emphasis" , "Low Emphasis"))
hcboxplot(x = schoolEmphasis4th$score,var = schoolEmphasis4th$emphasis,outliers = T,name = "Score",color = "#393029") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot of School Emphasis on Academic Success") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) 



## @knitr belonging
######  Students' Sense of School Belonging
## 4th grade
asg15 %>% 
  mutate(belonging = asbg11a + asbg11b + asbg11c + asbg11d + asbg11e + asbg11f + asbg11g) %>%
  mutate(belonging = ifelse(belonging < 15 ,"High Sense of Belonging",
                            ifelse(belonging < 25 ,"Sense of Belonging", "Little Sense of Belonging"))) %>% 
  filter(!is.na(belonging)) %>% 
  select(idcntry,idschool,belonging) -> schoolBelonging4th


left_join(schoolBelonging4th, schoolPerformance4th, by = c("idcntry","idschool")) -> schoolBelonging4th

schoolBelonging4th.fit <- aov(score ~ as.factor(belonging), data = schoolBelonging4th)
summary(schoolBelonging4th.fit)
rm(schoolBelonging4th.fit)
schoolBelonging4th %>% 
  group_by(belonging) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> schoolBelonging4th.avg

schoolBelonging4th.avg %>% 
  hchart(type = "column", hcaes(x = belonging, y = avgScore))%>% 
  hc_title(text = "Students' Sense of School Belonging") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_economist())


schoolBelonging4th$belonging <- factor(schoolBelonging4th$belonging,levels = c("High Sense of Belonging","Sense of Belonging", "Little Sense of Belonging"))
hcboxplot(x = schoolBelonging4th$score,var = schoolBelonging4th$belonging,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot of Students' Sense of School Belonging") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_economist()) 



## @knitr principal
# PRINCIPAL'S POV

princ15.stat4th <- acg15 %>%
  select(idcntry, idschool, acbg16a:acbg16j) %>%
  mutate(safety = acbg16a + acbg16b + acbg16c + acbg16d + acbg16e + 
           acbg16f + acbg16g + acbg16h + acbg16i + acbg16j) %>%
  filter(!is.na(safety)) %>%
  select(idcntry, idschool, safety) %>%
  mutate(safety.cat = ifelse(safety <= 18, "Not A Problem", 
                             ifelse(safety <= 25, "Minor Problem", 
                                    ifelse(safety <= 32, "Moderate Problem", 
                                           "Serious Problem"))))

princ15.stat4th <- left_join(princ15.stat4th, aschool, 
                             by = c("idcntry", "idschool"))

princ15.stat4th$safety.cat <- factor(princ15.stat4th$safety.cat,
                                     levels = c("Not A Problem",
                                                "Minor Problem", 
                                                "Moderate Problem", 
                                                "Serious Problem"))

princ15.stat4th.avg <- princ15.stat4th %>%
  group_by(safety.cat) %>%
  summarise(avg = mean(over.avg))

# ANOVA
princ15.stat4th.fit <- aov(over.avg ~ as.factor(safety.cat),
                           data = princ15.stat4th)
summary(princ15.stat4th.fit)
rm(princ15.stat4th.fit)

# avg histogram 


princ15.stat4th.avg %>% 
  hchart(type = "column", hcaes(x = safety.cat, y = avg %>% round(2)), 
         name = "Average Score", 
         color = "#F5A327") %>%
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
hcboxplot(x = princ15.stat4th$over.avg %>% round(3),
          var = princ15.stat4th$safety.cat,
          outliers = F,
          name = "Score",
          color = "firebrick") %>%
  hc_title(text = "BoxPlot of School Discipline Problems") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),
           tickColor = "black",
           labels = list(style = list(color = "black", fontSize = "20px")), 
           lineColor = 'black', lineWidth = 1) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),
           tickColor = "black",
           labels = list(style = list(color = "black")), 
           lineColor = 'black', lineWidth = 1) %>% 
  hc_add_theme(hc_theme_538())

## @knitr teacher
# TEACHER'S POV

atg15.safe <- atg15 %>%
  select(idcntry:idlink, atbg07a:atbg07h) %>%
  mutate(safety = atbg07a + atbg07b + atbg07c + atbg07d + 
           atbg07e + atbg07f + atbg07g + atbg07h) %>%
  filter(!is.na(safety)) %>%
  select(idcntry, idschool, idteach, safety) %>%
  mutate(safety.cat = ifelse(safety <= 14, "Not A Problem", 
                             ifelse(safety <= 20, "Minor Problem", 
                                    ifelse(safety <= 26, "Moderate Problem", 
                                           "Serious Problem"))))

teachsafe15.stat4th <- left_join(atg15.safe, ateacher, 
                                 by = c("idcntry", "idschool", "idteach"))

teachsafe15.stat4th$safety.cat <- factor(teachsafe15.stat4th$safety.cat,
                                         levels = c("Not A Problem",
                                                    "Minor Problem", 
                                                    "Moderate Problem", 
                                                    "Serious Problem"))

teachsafe15.stat4th.avg <- teachsafe15.stat4th %>%
  group_by(safety.cat) %>%
  summarise(avg = mean(over.avg, na.rm = T))

# ANOVA
teachsafe15.stat4th.fit <- aov(over.avg ~ as.factor(safety.cat),
                               data = teachsafe15.stat4th)
summary(teachsafe15.stat4th.fit)
rm(teachsafe15.stat4th.fit)


# avg histogram 


teachsafe15.stat4th.avg %>% 
  hchart(type = "column", hcaes(x = safety.cat, y = avg %>% round(2)), 
         name = "Average Score", 
         color = "#FFA8A8") %>%
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

hcboxplot(x = teachsafe15.stat4th$over.avg %>% round(3),
          var = teachsafe15.stat4th$safety.cat,
          outliers = F,
          name = "Score",
          color = "#9E2121") %>%
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


## @knitr student
# STUDENTS' POV

# 4TH GRADE
stud15.stat4th <- asg15 %>%
  mutate(bullying = asbg12a + asbg12b + asbg12c + asbg12d + asbg12e + 
           asbg12f + asbg12g + asbg12h) %>%
  select(idcntry, idschool, idclass, idstud, bullying) %>%
  filter(!is.na(bullying)) %>%
  mutate(bullying.cat = ifelse(bullying <= 14, "At Least Once A Week", 
                               ifelse(bullying <= 20, "Once Or Twice A Month", 
                                      ifelse(bullying <= 26, "A Few Times A Year", 
                                             "Never"))))
stud15.stat4th$bullying.cat <- factor(stud15.stat4th$bullying.cat,
                                      levels = c("At Least Once A Week",
                                                 "Once Or Twice A Month", 
                                                 "A Few Times A Year", 
                                                 "Never"))

stud15.stat4th <- left_join(stud15.stat4th, ascore, 
                            by = c("idcntry", "idstud", "idschool"))

stud15.stat4th.summary <- stud15.stat4th %>%
  group_by(over.state, bullying.cat) %>%
  summarise(freq = n())
stud15.stat4th.avg <- stud15.stat4th %>%
  group_by(bullying.cat) %>%
  summarise(avg = mean(overall))

# ANOVA
stud15.stat4th.fit <- aov(overall ~ as.factor(bullying.cat),
                          data = stud15.stat4th)
summary(stud15.stat4th.fit)
rm(stud15.stat4th.fit)

# Chi-square
chisq.test(matrix(as.numeric(stud15.stat4th.summary$freq), 
                  nrow = 4, ncol = 4, byrow = T))

# avg histogram 


stud15.stat4th.avg %>% 
  hchart(type = "column", hcaes(x = bullying.cat, y = avg %>% round(2)), 
         name = "Average Score", 
         color = "#BC9BB3") %>%
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
ggplot(stud15.stat4th) + 
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


## @knitr experience
##### Principal Experience 
## 4th grade
acg15 %>% 
  mutate(experience = acbg19) %>%
  mutate(experience = ifelse(experience <= 5  & experience >= 0 ,"Less Than 5 Years",
                             ifelse(experience <= 15  ,"5-15 Years",
                                    ifelse(experience <= 30 , "15-30 Years" , "More Than 30 Years")))) %>% 
  filter(!is.na(experience)) %>% 
  select(idcntry,idschool,experience) -> principalExperience4th


left_join(principalExperience4th, schoolPerformance4th, by = c("idcntry","idschool")) -> principalExperience4th

principalExperience4th.fit <- aov(score ~ as.factor(experience), data = principalExperience4th)
summary(principalExperience4th.fit)
rm(principalExperience4th.fit)

principalExperience4th %>% 
  group_by(experience) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> principalExperience4th.avg

principalExperience4th.avg %>% 
  hchart(type = "column", hcaes(x = experience, y = avgScore),color = "#FF9500")%>% 
  hc_title(text = "Principal Experience") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx())

principalExperience4th$experience <- factor(principalExperience4th$experience,levels = c("Less Than 5 Years","5-15 Years", "15-30 Years" , "More Than 30 Years"))
hcboxplot(x = principalExperience4th$score,var = principalExperience4th$experience,outliers = F,name = "Score",color = "#FF9500") %>%
  hc_title(text = "BoxPlot of Principal Experience") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx())  



## @knitr principalEducation
#### Principal Education
## 4th grade
acg15 %>% 
  mutate(education = acbg21) %>%
  mutate(education = ifelse(education == 1 ,"Didn't Complete Bachelor's",
                            ifelse(education == 2 ,"Bachelor's or Equivalent",
                                   ifelse(education == 3 , "Master's or Equivalent" , "Doctor or Equivalent")))) %>% 
  filter(!is.na(education)) %>% 
  select(idcntry,idschool,education) -> principalEducation4th


left_join(principalEducation4th, schoolPerformance4th, by = c("idcntry","idschool")) -> principalEducation4th

principalEducation4th.fit <- aov(score ~ as.factor(education), data = principalEducation4th)
summary(principalEducation4th.fit)
rm(principalEducation4th.fit)
principalEducation4th %>% 
  group_by(education) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> principalEducation4th.avg

principalEducation4th.avg %>% 
  hchart(type = "column", hcaes(x = education, y = avgScore),color = "#FFE4B5")%>% 
  hc_title(text = "Principal Education") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_flat())

principalEducation4th$education <- factor(principalEducation4th$education,levels = c("Didn't Complete Bachelor's","Bachelor's or Equivalent", "Master's or Equivalent" , "Doctor or Equivalent"))
hcboxplot(x = principalEducation4th$score,var = principalEducation4th$education,outliers = F,name = "Score",color = "#FFE4B5") %>%
  hc_title(text = "BoxPlot of Principal Education") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())   



## @knitr teacherExperience
##### Teachers Experience
## 4th grade
atg15 %>% 
  mutate(experience = atbg01) %>%
  mutate(experience = ifelse(experience <= 5  & experience >= 0 ,"Less Than 5 Years",
                             ifelse(experience <= 15  ,"5-15 Years",
                                    ifelse(experience <= 30 , "15-30 Years" , "More Than 30 Years")))) %>% 
  filter(!is.na(experience)) %>% 
  select(idcntry,idschool,idteach,experience) -> teacherExperience4th



left_join(teacherExperience4th, teacherPerformance4th, by = c("idcntry","idschool","idteach")) -> teacherExperience4th

teacherExperience4th.fit <- aov(score ~ as.factor(experience), data = teacherExperience4th)
summary(teacherExperience4th.fit)
rm(teacherExperience4th.fit)
teacherExperience4th %>% 
  group_by(experience) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> teacherExperience4th.avg

teacherExperience4th.avg %>% 
  hchart(type = "bar", hcaes(x = experience, y = avgScore),color = "#E0FFFF")%>% 
  hc_title(text = "Teachers Experience") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark())


teacherExperience4th$experience <- factor(teacherExperience4th$experience,levels = c("Less Than 5 Years","5-15 Years", "15-30 Years" , "More Than 30 Years"))
hcboxplot(x = teacherExperience4th$score,var = teacherExperience4th$experience,outliers = F,name = "Score",color = "#008B8B") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot of Teachers Experience") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_flatdark()) 

## @knitr teacherEducation
#### Teacher Education
## 4th grade
atg15 %>% 
  mutate(education = atbg04) %>%
  mutate(education = ifelse(education == 1 ,"Didn't Complete Bachelor's",
                            ifelse(education == 2 ,"Bachelor's or Equivalent",
                                   ifelse(education == 3 , "Master's or Equivalent" , "Doctor or Equivalent")))) %>% 
  filter(!is.na(education)) %>% 
  select(idcntry,idschool,idteach,education) -> teacherEducation4th


left_join(teacherEducation4th, teacherPerformance4th, by = c("idcntry","idschool","idteach")) -> teacherEducation4th

teacherEducation4th.fit <- aov(score ~ as.factor(education), data = teacherEducation4th)
summary(teacherEducation4th.fit)
rm(teacherEducation4th.fit)
teacherEducation4th %>% 
  group_by(education) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> teacherEducation4th.avg

teacherEducation4th.avg %>% 
  hchart(type = "bar", hcaes(x = education, y = avgScore),color = "#FF7F50")%>% 
  hc_title(text = "Teachers Education") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx())

teacherEducation4th$education <- factor(teacherEducation4th$education,levels = c("Didn't Complete Bachelor's","Bachelor's or Equivalent", "Master's or Equivalent" , "Doctor or Equivalent"))
hcboxplot(x = teacherEducation4th$score,var = teacherEducation4th$education,outliers = F,name = "Score",color = "#FF7F50") %>%
  hc_chart(type = "column") %>% 
  hc_title(text = "BoxPlot of Teachers Education") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_ffx())   


## @knitr teacherLimited
####  Teaching Limited by Student Needs
## 4th grade
atg15 %>% 
  mutate(limit = atbg15a + atbg15b+ atbg15c+ atbg15d+ atbg15e+ atbg15f+ atbg15g) %>%
  mutate(limit = ifelse(limit <= 10 ,"Not At All",
                        ifelse(limit <= 17 ,"Some","A Lot"))) %>% 
  filter(!is.na(limit)) %>% 
  select(idcntry,idschool,idteach,limit) -> teacherLimit4th


left_join(teacherLimit4th, teacherPerformance4th, by = c("idcntry","idschool","idteach")) -> teacherLimit4th

teacherLimit4th.fit <- aov(score ~ as.factor(limit), data = teacherLimit4th)
summary(teacherLimit4th.fit)
rm(teacherLimit4th.fit)
teacherLimit4th %>% 
  group_by(limit) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> teacherLimit4th.avg

teacherLimit4th.avg %>% 
  hchart(type = "column", hcaes(x = limit, y = avgScore))%>% 
  hc_title(text = "Teaching Limited by Student Needs") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_flat())

teacherLimit4th$limit <- factor(teacherLimit4th$limit,levels = c("Not At All","Some","A Lot"))
hcboxplot(x = teacherLimit4th$score,var = teacherLimit4th$limit,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot of Teaching Limited by Student Needs") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "black")),tickColor = "black",
           labels = list(style = list(color = "black"))) %>% 
  hc_add_theme(hc_theme_flat())

## @knitr studentsAbsences
####  Students Absences
## 4th grade
asg15 %>% 
  mutate(absent = asbg08) %>%
  mutate(absent = ifelse(absent == 1 ,"Once A Week Or More",
                         ifelse(absent == 2 ,"Once Every Two Weeks",
                                ifelse(absent == 3,"Once A Month","Never Or Almost Never")))) %>% 
  filter(!is.na(absent)) %>% 
  select(idcntry,idschool,absent) -> studentAbsences4th


left_join(studentAbsences4th, schoolPerformance4th, by = c("idcntry","idschool")) -> studentAbsences4th

studentAbsences4th.fit <- aov(score ~ as.factor(absent), data = studentAbsences4th)
summary(studentAbsences4th.fit)
rm(studentAbsences4th.fit)
studentAbsences4th %>% 
  group_by(absent) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> studentAbsences4th.avg

studentAbsences4th.avg %>% 
  hchart(type = "column", hcaes(x = absent, y = avgScore))%>% 
  hc_title(text = "Students Absences") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())


studentAbsences4th$absent <- factor(studentAbsences4th$absent,levels = c("Once A Week Or More","Once Every Two Weeks","Once A Month","Never Or Almost Never"))
hcboxplot(x = studentAbsences4th$score,var = studentAbsences4th$absent,outliers = F,name = "Score") %>%
  hc_title(text = "BoxPlot of Students Absences") %>% 
  hc_subtitle(text = "4th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica()) 

## @knitr timespent
######## Instructional Time Spent
## 4th grade
acg15 %>% 
  mutate(hours = acbg08a * acbg08b/60 ) %>%
  mutate(hours = ifelse(hours <= 1000 ,"450 - 1000 Instructional Hours",
                        ifelse(hours <= 1500 ,"1000 - 1500 Instructional Hours",
                               ifelse(hours <= 2500,"1500-2500 Instructional Hours","2500 - 3000 Instructional Hours")))) %>% 
  filter(!is.na(hours)) %>% 
  select(idcntry,idschool,hours) -> instructionHours4th


left_join(instructionHours4th, schoolPerformance4th, by = c("idcntry","idschool")) -> instructionHours4th

instructionHours4th.fit <- aov(score ~ as.factor(hours), data = instructionHours4th)
summary(instructionHours4th.fit)
rm(instructionHours4th.fit)
instructionHours4th %>% 
  group_by(hours) %>% 
  summarise(avgScore = mean(score)) %>% 
  arrange(desc(avgScore))-> instructionHours4th.avg

instructionHours4th.avg %>% 
  hchart(type = "bar", hcaes(x = hours, y = avgScore),color= "#20B2AA")%>% 
  hc_title(text = "Instructional Time Spent") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Average Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())


instructionHours4th$hours <- factor(instructionHours4th$hours,levels = c("450 - 1000 Instructional Hours","1000 - 1500 Instructional Hours","1500-2500 Instructional Hours","2500 - 3000 Instructional Hours"))
hcboxplot(x = instructionHours4th$score,var = instructionHours4th$hours,outliers = F,name = "Score",color= "#20B2AA") %>%
  hc_title(text = "BoxPlot of Instructional Time Spent") %>% 
  hc_subtitle(text = "8th Grade") %>% 
  hc_xAxis(title = list(text = "",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_yAxis(title = list(text = "Score",style = list(color = "white")),tickColor = "white",
           labels = list(style = list(color = "white"))) %>% 
  hc_add_theme(hc_theme_darkunica())  

## @knitr engagement
# ENGAGEMENT

# 4TH GRADE

mstudles15.stat4th <- asg15 %>%
  mutate(lessons = asbm02a + asbm02b + asbm02c + asbm02d + asbm02e + 
           asbm02f + asbm02g + asbm02h + asbm02i + asbm02j) %>%
  select(idcntry, idschool , idstud, lessons) %>%
  filter(!is.na(lessons)) %>%
  mutate(lessons.cat = ifelse(lessons <= 17, "Very Engaging Teaching", 
                              ifelse(lessons <= 25, "Engaging Teaching",
                                     ifelse(lessons <= 32, "Less Than Engaging Teaching", 
                                            "Not Engaging Teaching"))))
mstudles15.stat4th$lessons.cat <- factor(mstudles15.stat4th$lessons.cat,
                                         levels = c("Very Engaging Teaching",
                                                    "Engaging Teaching", 
                                                    "Less Than Engaging Teaching", 
                                                    "Not Engaging Teaching"))

mstudles15.stat4th <- left_join(mstudles15.stat4th, ascore, 
                                by = c("idcntry", "idstud", "idschool"))

mstudles15.stat4th.summary <- mstudles15.stat4th %>%
  group_by(mat.state, lessons.cat) %>%
  summarise(freq = n())
mstudles15.stat4th.avg <- mstudles15.stat4th %>%
  group_by(lessons.cat) %>%
  summarise(avg = mean(mat))


# ANOVA
mstudles15.stat4th.fit <- aov(overall ~ as.factor(lessons.cat),
                              data = mstudles15.stat4th)
summary(mstudles15.stat4th.fit)
rm(mstudles15.stat4th.fit)
# Chi-square


chisq.test(matrix(as.numeric(mstudles15.stat4th.summary$freq), 
                  nrow = 4, ncol = 4, byrow = T))

# avg histogram 

mstudles15.stat4th.avg %>% 
  hchart(type = "column", hcaes(x = lessons.cat, y = avg %>% round(2)), 
         name = "Average Score", 
         color = "#3049AC", 
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
ggplot(mstudles15.stat4th) + 
  geom_boxplot(aes(y = mat, x = lessons.cat, fill = lessons.cat)) +
  theme_minimal() + 
  theme(axis.text.x = element_blank(),
        axis.text = element_blank(), 
        legend.position = "bottom") + 
  labs(title = "Boxplot of Children's Overall Score", 
       subtitle = "Based on Engagement in Class", 
       x = "Overall Score", y = "Engagement Level of Teachers") + 
  guides(fill = guide_legend(title = "")) + 
  coord_flip() + 
  scale_y_continuous()



## @knitr learning
# LEARNING ATTITUDE

## 4TH GRADE 

mstudlearn15.stat4th <- asg15 %>%
  mutate(learning = asbs04a + asbs04b + asbs04c + asbs04d + asbs04e + 
           asbs04f + asbs04g + asbs04h + asbs04i) %>%
  select(idcntry, idschool, idstud, learning) %>%
  filter(!is.na(learning)) %>%
  mutate(learning.cat = ifelse(learning <= 16, "Love Learning", 
                               ifelse(learning <= 23, "Like Learning", 
                                      ifelse(learning <= 30, "Dislike Learning", 
                                             "Hate Learning"))))
mstudlearn15.stat4th$learning.cat <- factor(mstudlearn15.stat4th$learning.cat,
                                            levels = c("Love Learning",
                                                       "Like Learning", 
                                                       "Dislike Learning", 
                                                       "Hate Learning"))

mstudlearn15.stat4th <- left_join(mstudlearn15.stat4th, ascore, 
                                  by = c("idcntry", "idstud", "idschool"))

mstudlearn15.stat4th.summary <- mstudlearn15.stat4th %>%
  group_by(mat.state, learning.cat) %>%
  summarise(freq = n()) 
mstudlearn15.stat4th.avg <- mstudlearn15.stat4th %>%
  group_by(learning.cat) %>%
  summarise(avg = mean(mat))

# ANOVA
mstudlearn15.stat4th.fit <- aov(overall ~ as.factor(learning.cat),
                                data = mstudlearn15.stat4th)
summary(mstudlearn15.stat4th.fit)
rm(mstudlearn15.stat4th.fit)
# Chi-square

chisq.test(matrix(as.numeric(mstudlearn15.stat4th.summary$freq), 
                  nrow = 4, ncol = 4, byrow = T))

# avg histogram 


mstudlearn15.stat4th.avg %>% 
  hchart(type = "column", hcaes(x = learning.cat, y = avg %>% round(2)), 
         name = "Average Score", 
         color = "#C7148B", 
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
ggplot(mstudlearn15.stat4th) + 
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

## 4TH GRADE
mstudconf15.stat4th <- asg15 %>%
  mutate(confidence = asbm03a + asbm03b + asbm03c + asbm03d + asbm03e + 
           asbm03f + asbm03g + asbm03h + asbm03i) %>%
  select(idcntry, idschool, idstud, confidence) %>%
  filter(!is.na(confidence)) %>%
  mutate(confidence.cat = ifelse(confidence <= 18, "Very Confident", 
                                 ifelse(confidence <= 27, "Confident", 
                                        "Not Confident")))
mstudconf15.stat4th$confidence.cat <- factor(mstudconf15.stat4th$confidence.cat,
                                             levels = c("Very Confident",
                                                        "Confident", 
                                                        "Not Confident"))

mstudconf15.stat4th <- left_join(mstudconf15.stat4th, ascore, 
                                 by = c("idcntry", "idstud", "idschool"))
mstudconf15.stat4th.summary <- mstudconf15.stat4th %>%
  group_by(mat.state, confidence.cat) %>%
  summarise(freq = n()) 

mstudconf15.stat4th.avg <- mstudconf15.stat4th %>%
  group_by(confidence.cat) %>%
  summarise(avg = mean(mat))

# ANOVA
mstudconf15.stat4th.fit <- aov(overall ~ as.factor(confidence.cat),
                               data = mstudconf15.stat4th)
summary(mstudconf15.stat4th.fit)
rm(mstudconf15.stat4th.fit)
# Chi-square

chisq.test(matrix(as.numeric(mstudconf15.stat4th.summary$freq), 
                  ncol = 3, byrow = T))


# avg histogram 

mstudconf15.stat4th.avg %>% 
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
ggplot(mstudconf15.stat4th) + 
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

