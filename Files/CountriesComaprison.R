


## @knitr dataSection
library(tidyverse)
library(highcharter)
library(plotly)
library(shiny)

bsg15 <- read_rds("../2015/bsg.rds")
bsg11 <- read_rds("../2011/bsg.rds")
bsg07 <- read_rds("../2007/bsg.rds")
bsg03 <- read_rds("../2003/bsg.rds")

asg11 <- read_rds("../2011/asg.rds")

library(ISOcodes)
codes <- ISO_3166_1 %>%
  select(idcntry = Numeric, country = Alpha_2, Name) %>%
  mutate(idcntry = as.character(idcntry))

## @knitr avgResult
res15total <- bsg15 %>%
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5) %>% 
  group_by(idcntry) %>% 
  mutate(mavg = mean(mat, na.rm = T)) %>% 
  arrange(desc(mavg)) %>% select(idcntry, mat, mavg) %>% 
  apply(2, function(x) x %>% as.numeric() %>% round(3)) %>% 
  data.frame() %>% 
  mutate(idcntry = idcntry %>% as.character()) %>% 
  left_join(codes, by = "idcntry") %>%
  filter(!is.na(country)) %>% 
  select(country = Name, `Math Average` = mavg, `Math Individual` = mat)
# Average Results
res15avg <- res15total %>% select(country, `Math Average`) %>% unique() %>% 
  mutate(status = ifelse(`Math Average` > 525, "High", 
                         ifelse(`Math Average` > 450, "Medium", "Low")))
## VISUAL DEMONSTRATION of Average of Results
res15avg %>%
  arrange(desc(`Math Average`)) %>%
  hchart(type = 'column', hcaes(x = country, y = `Math Average` %>% round(2), group = status), 
         borderColor = 'black', color = c("#1C179B", "#B0DBEB", "#6EA2EF")) %>%
  hc_legend(align = 'center') %>% 
  hc_plotOptions(column = list(pointWidth = 10)) %>%
  hc_title(text = 'Average Score in Mathematics',
           align = 'center') %>% 
  hc_subtitle(text = 'Timss 2015', align = 'center') %>%
  hc_xAxis(title = list(text = ""),tickColor = "black",
           labels = list(style = list(color = "black"), rotation = -90), 
           lineColor = "black") %>%
  hc_yAxis(title = list(text = ""),
           tickColor = "black",
           labels = list(style = list(color = "black")), 
           lineColor = "black", lineWidth = 1) %>% 
  hc_add_theme(hc_theme_elementary())

## @knitr countries2v2
countries <- res15avg %>% arrange(desc(`Math Average`)) %>% 
  select(country) %>% unlist() %>% unname()

sample <- sample(countries, size = 10)
plot <- ggplot(res15total %>% filter(country %in% sample)) + 
  geom_density(aes(`Math Individual`, fill = country, color = country), alpha = 0.5) + 
  theme_minimal() + 
  labs(x = "Mathematics Score", y = "", title = "Individual Mathematics Score Distribution of 10 Sample Countries") + 
  guides(fill = guide_legend(title = ""))

plot %>% ggplotly() %>% layout(showlegend = F) %>% div(align = 'center')



## @knitr countries
temp <- res15avg %>% mutate(dummy = 0) %>% select(country, dummy) %>% spread(country, dummy)
temp <- temp[, countries]
compare15 <- cbind(res15avg, temp)
rownames(compare15) <- compare15$country
compare15 <- compare15 %>% select(-country, -status)
save <- compare15$`Math Average`

n <- length(countries)
threshhold <- 2.2 * (10 ^ -16)
compare15 <- compare15 %>% select(-`Math Average`)
for(i in 1:n){
  first <- res15total %>% filter(country == countries[i]) %>%
    select(`Math Individual`) %>% 
    unlist() %>% unname()
  for(j in (i + 1):n){
    second <- res15total %>% filter(country == countries[j]) %>% 
      select(`Math Individual`) %>%
      unlist() %>% unname()
    if(j <= n){
      hypo <- t.test(first, second)
      if (hypo$p.value < threshhold){
        compare15[i, j] <- "Significantly Greater"
        compare15[j, i] <- "Significantly Less"
      }
      else{
        compare15[i, j] <- "Not Significantly Different"
        compare15[j, i] <- "Not Significantly Different"
      }
    }
    
  }
}
compare15$`Math Average1` <- save

results.matrix <- compare15 %>% ungroup()
results.matrix$country1 <- rownames(results.matrix)
results.matrix <- results.matrix %>% gather(key = "country2", value = "status", Singapore:`Saudi Arabia`)
results.matrix <- results.matrix %>% mutate(status = ifelse(status == 0, "Not Significantly Different", status))
results.matrix <- results.matrix %>% mutate(temp = rank(-`Math Average1`, ties.method = "first")) %>% arrange(temp)
results.matrix$`Math Average2` <- 0
n <- nrow(results.matrix) 
for(i in 1:n){
  curr.country <- results.matrix[i, "country2"]
  curr.country.score <- results.matrix %>% filter(country1 == curr.country) %>% select(`Math Average1`) %>% unique() %>% unlist() %>% unname()
  results.matrix[i, "Math Average2"] <- curr.country.score
}
results.matrix <- results.matrix %>% mutate(result = paste(`Math Average1`, `Math Average2`, sep = " - "))


p <- ggplot(results.matrix, aes(x = reorder(country1, results.matrix$temp), 
                                y = reorder(country2, results.matrix$temp), 
                                text = result)) + 
  geom_tile(aes(fill = status), 
            colour= "white", 
            size = 0.5, 
            stat = "identity", 
            height = 1, width = 1) +
  ggtitle("Comparing Participants Population Mean") + 
  labs(x = "", y = "", subtitle = "Timss 2015 Mathematics Results") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill=NA, color="white", size=0.5, linetype="solid"),
    axis.line = element_blank(),
    axis.ticks = element_blank(), 
    axis.text.x  = element_text(angle=270, vjust=0.5, hjust=0),
    axis.text = element_text(size=rel(1), face = "bold"),
    legend.title = element_text(size=rel(1.5)), 
    legend.text = element_text(size=rel(1)), 
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "top"
  ) + 
  coord_flip() +
  guides(fill = guide_legend(title="Left Axis Compared to Bottom Axis", 
                             default.unit="inch", 
                             title.position = "top")) + 
  scale_fill_manual(labels = c("Not Significantly Different", 
                               "Significantly Greater", 
                               "Significantly Less"), 
                    values = c("Not Significantly Different"= "#979393",
                               "Significantly Greater"= "#108B52",
                               "Significantly Less" = "#8B1919"))
p


## @knitr countries3
get_average_info <- function(x){
  x %>%
    mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5) %>% 
    group_by(idcntry) %>% 
    summarise(mavg = mean(mat, na.rm = T)) %>% 
    apply(2, function(x) round(as.numeric(x), 3)) %>% 
    data.frame() %>% 
    mutate(idcntry = as.character(idcntry)) %>%
    left_join(codes, by = "idcntry") %>% 
    filter(!is.na(country)) %>% 
    select(country = Name, mavg) %>% 
    return()
}
res15 <- get_average_info(bsg15) %>% mutate(year = 2015)
res11 <- get_average_info(bsg11) %>% mutate(year = 2011) 
res07 <- get_average_info(bsg07) %>% mutate(year = 2007)
res03 <- get_average_info(bsg03) %>% mutate(year = 2003)

total <- res15 %>% full_join(res11) %>% full_join(res07) %>% full_join(res03) %>% 
  spread(year, mavg)

highchart() %>% 
  hc_chart(type = "bar", showLegend = F) %>% 
  hc_plotOptions(bar = list(borderColor = "black", 
                            colors = c("#2E7226", "#519C48", "#5DCA51", "#98CD92"), 
                            colorByPoint = T)) %>% 
  hc_title(text = "The World Throughout The Years",
           align = 'center', 
           style = list(fontFamily = 'B-Nazanin', 
                        fontWeight = "italic", fontSize = "35px")) %>%
  hc_subtitle(text = "Average Score in Mathematics", 
              style = list(fontSize = "20px")) %>% 
  hc_xAxis(categories = c(2015, 2011, 2007, 2003),
           gridLineColor = "transparent", lineColor = 'black', 
           tickColor = "black", tickLength = 30, 
           labels = list(style = list(fontSize="20px"))) %>%
  hc_add_series(name = "Score",
                data = list(
                  list(sequence = total$`2015`),
                  list(sequence = total$`2011`),
                  list(sequence = total$`2007`), 
                  list(sequence = total$`2003`)
                )
  ) %>% 
  hc_motion(enabled = TRUE,
            labels = total$country,
            series = c(1,0),
            updateIterval = 1000,
            autoPlay = T) %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_plotOptions(
    series = list(
      showInLegend =F    
    ))

## @knitr coutriesChange
res4th.11 <- asg11 %>% 
  mutate(mat = (asmmat01 + asmmat02 + asmmat03 + asmmat04 + asmmat05) / 5) %>% 
  select(idcntry, mat) %>% 
  group_by(idcntry) %>% 
  mutate(identity = row_number(idcntry)) %>% 
  apply(2, function(x) x %>% as.numeric() %>% round(3))  %>% 
  data.frame() %>% 
  mutate(idcntry = as.character(idcntry)) %>%
  left_join(codes, by = "idcntry") %>% 
  filter(!is.na(country)) %>% 
  select(country = Name, mat11 = mat, identity)
res4th.11.avg <- res4th.11 %>% group_by(country) %>% dplyr::summarise(avg11 = mean(mat11, na.rm = T))

res8th.15 <- bsg15 %>% 
  mutate(mat = (bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5) %>%
  select(idcntry, mat) %>% 
  group_by(idcntry) %>% 
  mutate(identity = row_number(idcntry)) %>% 
  apply(2, function(x) x %>% as.numeric() %>% round(3))  %>% 
  data.frame() %>% 
  mutate(idcntry = as.character(idcntry)) %>%
  left_join(codes, by = "idcntry") %>% 
  filter(!is.na(country)) %>% 
  select(country = Name, mat15 = mat, identity)
res8th.15.avg <- res8th.15 %>% group_by(country) %>% dplyr::summarise(avg15 = mean(mat15, na.rm = T))


compare.same <- left_join(res8th.15, res4th.11, by = c("country", "identity")) %>%
  drop_na() %>% select(-identity)
countries <- compare.same %>% select(country) %>% unique() %>% unlist() %>% unname()
timedata <- compare.same %>% select(country) %>% unique()
timedata$compare15and11 <- 0
rownames(timedata) <- timedata$country
n <- nrow(timedata)
threshhold <- 2.2 * (10 ^ -16)
temp <- c()
for(i in 1:n){
  current <- compare.same %>% filter(country == countries[i])
  first <- current$mat11
  second <- current$mat15
  hypo.greater <- wilcox.test(first, second, alternative = "greater")
  hypo.less <- wilcox.test(first, second, alternative = "less")
  if (hypo.greater$p.value < threshhold){
    timedata[countries[i], 2] <- "Year 2015 Was Significantly Less Than 2011"
  } else if (hypo.less$p.value < threshhold){
    timedata[countries[i], 2] <- "Year 2015 Was Significantly Greater Than 2011"
  } else {
    timedata[countries[i], 2] <- "Year 2015 Was Not Significantly Different From 2011"
  }
}
timedata <- timedata %>% left_join(res4th.11.avg) %>% left_join(res8th.15.avg) %>% 
  gather(key = "year", value = "score", avg11:avg15)


timedata %>% 
  arrange(desc(score)) %>%
  mutate(year = ifelse(year == "avg15", "2015", "2011"), 
         score = score %>% round(3)) %>% 
  hchart(type = 'scatter', hcaes(x = country %>% as.character(), y = score, group = compare15and11), color = c("#979393", "#108B52", "#8B1919")) %>% 
  hc_legend(align = "left") %>%
  hc_tooltip(pointFormat = "{point.group}<br> Year: {point.year}<br> Average: {point.score}") %>%
  hc_xAxis(title = "", labels = list(rotation = -90)) %>% hc_yAxis(title = "") %>% 
  hc_title(text = "Comparing The Same Students Between 2015 and 2011") %>%
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_size(height = 700)

## @knitr improvement 
compare.perc <- timedata %>% 
  group_by(country) %>% 
  dplyr::mutate(dif = lead(score) - score) %>%
  drop_na() %>% 
  mutate(perc = dif / score * 100) %>% select(country, perc) %>% 
  mutate(status = ifelse(perc < 0, "Decrease", "Increase")) %>%
  arrange(perc)

compare.perc %>%
  hchart(type = 'column', hcaes(x = country %>% as.character(), y = perc %>% round(3), group = status), 
         color = c("#C33E3E", "#169C50"), borderColor = 'black', showInLegend = F) %>% 
  hc_plotOptions(column = list(pointWidth = 10, tooltip = list(pointFormat = '{point.y}%'))) %>%
  hc_title(text = 'Improvement or Decline of Countries',
           align = 'center', style = list(fontSize = "30px")) %>% 
  hc_subtitle(text = 'Timss 2015 - 2011', align = 'center', style = list(fontSize = "20px")) %>%
  hc_xAxis(title = list(text = ""),tickColor = "black",
           labels = list(style = list(color = "black"), rotation = -90), 
           lineColor = "black") %>%
  hc_yAxis(title = list(text = ""),
           tickColor = "black",
           labels = list(style = list(color = "black"), 
                         format = '{value}%'),
           lineColor = "black", lineWidth = 1) %>% 
  hc_add_theme(hc_theme_elementary())
