
## @knitr dataSection
library(tidyverse)
library(highcharter)
library(plotly)
library(shiny)
library(ggbiplot)
library(kableExtra)
library(dendextend)
library(factoextra)

bsg15 <- read_rds("../2015/bsg.rds")
bsg11 <- read_rds("../2011/bsg.rds")
bsg07 <- read_rds("../2007/bsg.rds")
library(ISOcodes)
codes <- ISO_3166_1 %>%
  select(idcntry = Numeric, country = Alpha_2, Name) %>%
  mutate(idcntry = as.character(idcntry))

set.seed(19)

## GATHERING THE DATA
status15 <- bsg15 %>% 
  select(idcntry, bsmmat01:bssrea05)
status11 <- bsg11 %>% 
  select(idcntry, bsmmat01:bssrea05)
status07 <- bsg07 %>% 
  select(idcntry, bsmmat01:bssrea05)

status <- rbind(status15, status11, status07) 
status <- data.frame(apply(status15, 2, function(x) round(as.numeric(x), 3))) %>%
  mutate(idcntry = as.character(idcntry)) %>% left_join(codes, by = "idcntry") %>% filter(!is.na(country))
status <- status %>% select(country = Name, bsmmat01:bssrea05)

## @knitr table
status_avg <- status %>% group_by(country) %>% summarise_all(mean)
rownames(status_avg) <- status_avg$country
status_avg <- status_avg %>% select(-country)

ni <- ncol(status_avg)
nj <- nrow(status_avg)
i <- 1
columns <- colnames(status_avg)

while (i < ni) {
  for(j in 1:nj){
    status_avg[j, ni + (i %/% 5 + 1)] <- status_avg[j, i:(i + 4)] %>% unlist() %>% unname() %>% mean(na.rm = T)
  }
  temp <- colnames(status_avg)
  colnames(status_avg) <- c(temp[-length(temp)], c(columns[i] %>% substr(3, 6)))
  i <- i + 5
}
status_avg$country <- rownames(status_avg)
status_avg <- status_avg %>% select(country, mmat:srea)
rownames(status_avg) <- 1:nrow(status_avg)

status_avg %>% 
  filter((country == "Singapore") | (country == "Iran, Islamic Republic of")) %>% 
  mutate(ifelse(country == "Iran, Islamic Republic o", "Iran", country)) %>%
  select(Country = country, Mathematics =  mmat, Science = ssci, Algebra = malg, Geometry = mgeo, Chemistry = sche, Biology = sbio, Physics = sphy) %>%
  knitr::kable() %>% kable_styling()

## @knitr clustering1
k.timss <- eclust(status_avg %>% dplyr::select(-country), "kmeans", k = 3, nstart = 25, graph = FALSE)

status_avg$cluster <- k.timss$cluster
rownames(status_avg) <- status_avg$country

status_avg %>% 
  filter(cluster == 1) %>% 
  select(country) %>%
  plyr::rename(c("country" = "First K-means Cluster")) %>%
  knitr::kable(format = "html") %>% kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
  row_spec(4, bold = T, color = "white", background = "#D7261E")


## @knitr clustering2
status_avg %>% 
  filter(cluster == 2) %>% 
  select(country) %>%
  plyr::rename(c("country" = "Second K-means Cluster")) %>%
  knitr::kable(format = "html") %>% kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left")

## @knitr clustering3
status_avg %>% 
  filter(cluster == 3) %>% 
  select(country) %>%
  plyr::rename(c("country" = "Third K-means Cluster")) %>%
  knitr::kable(format = "html") %>% kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left")

## @knitr pca
status_avg.pca = prcomp(status_avg %>% dplyr::select(-country, -cluster), center = T, scale. = T)

ggbiplot(status_avg.pca, groups = k.timss$cluster %>% as.factor(), ellipse = T) + 
  scale_color_discrete(name = '') +
  theme_minimal()

## @knitr pcaHC
hchart(status_avg.pca, cor = TRUE)

## @knitr kmeans
library(fpc)
test <- kmeansruns(status_avg %>% dplyr::select(-country), krange = 1:10, criterion = "asw")


## @knitr avgSW
highchart() %>%
  hc_add_series(name = "ASW", data = test$crit, color = "#B72424", showInLegend = F) %>%
  hc_title(text = 'Average Silhoette Width',
           align = 'center') %>% 
  hc_subtitle(text = 'Based on Different Values of K', align = 'center') %>%
  hc_xAxis(title = list(text = ""),tickColor = "black",
           labels = list(style = list(color = "black"), rotation = -90), 
           lineColor = "black") %>%
  hc_yAxis(title = list(text = ""),
           tickColor = "black",
           labels = list(style = list(color = "black")), 
           lineColor = "black", lineWidth = 1) %>% 
  hc_add_theme(hc_theme_elementary())

## @knitr SW
si <- k.timss$silinfo$widths
si$country = status_avg$country
si %>%
  hchart(type = 'column', hcaes(x = country, y = sil_width, group = cluster)) %>%
  hc_title(text = 'Silhoette Width of Each Country',
           align = 'center') %>% 
  hc_subtitle(text = 'Colored based on cluster', align = 'center') %>%
  hc_xAxis(title = list(text = ""),tickColor = "black",
           labels = list(style = list(color = "black"), rotation = -90), 
           lineColor = "black") %>%
  hc_yAxis(title = list(text = ""),
           tickColor = "black",
           labels = list(style = list(color = "black")), 
           lineColor = "black", lineWidth = 1) %>% 
  hc_add_theme(hc_theme_elementary())


## @knitr H_clustering
dend <- status_avg %>% select(-country, -cluster) %>% dist(method = "euclidean") %>% 
  hclust()
clusters <- cutree(dend, k = 3)
status_avg$cluster_dend <- clusters
status_avg %>% 
  filter(cluster_dend == 1) %>% 
  select(country) %>% 
  plyr::rename(c("country" = "First Dendrogram Cluster")) %>%
  knitr::kable(format = "html") %>% kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left")


## @knitr H_clus2
status_avg %>% 
  filter(cluster_dend == 2) %>% 
  select(country) %>% 
  plyr::rename(c("country" = "Second Dendrogram Cluster")) %>%
  knitr::kable(format = "html") %>% kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left") %>%
  row_spec(3, bold = T, color = "white", background = "#D7261E")

## @knitr H_clus3
status_avg %>% 
  filter(cluster_dend == 3) %>% 
  select(country) %>%
  plyr::rename(c("country" = "Third Dendrogram Cluster")) %>%
  knitr::kable(format = "html") %>% kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left")


## @knitr dendrogram

par(mar = c(9,0,0,0))
plot.new()
dend %>%
  as.dendrogram() %>%
  set("labels_col", value = c("#169C50", "#CB7E09", "#B30808"), k = 3) %>%
  set("branches_k_color", value = c("#169C50", "#CB7E09", "#B30808"), k = 3) %>%
  plot(axes = FALSE) 
