library(haven)


files <- list.files("../PL16_SPSSData/")

ACGPirls16 <- c()
acg <- files[1:63]
for(i in acg){
  data <- read_sav(i)
  ACGPirls16 <- rbind(ACGPirls16, data)
}
saveRDS(ACGPirls16, file = "ACG_Pirls16.rds")

ASAPirls16 <- c()
asa <- files[64:126]
for(i in asa){
  data <- read_sav(i) %>% 
    select(IDCNTRY,IDBOOK,IDSCHOOL,IDCLASS,IDSTUD,IDPOP:ASRIBM05)
  ASAPirls16 <- rbind(ASAPirls16, data)
}
saveRDS(ASAPirls16, file = "ASA_Pirls16.rds")


ASGPirls16 <- c()
asg <- files[127:189]
for(i in asg){
  data <- read_sav(i)
  ASGPirls16 <- rbind(ASGPirls16, data)
}
saveRDS(ASGPirls16, file = "ASG_Pirls16.rds")


ASHPirls16 <- c()
ash <- files[190:252]
for(i in ash){
  data <- read_sav(i)
  ASHPirls16 <- rbind(ASHPirls16, data)
}
saveRDS(ASHPirls16, file = "ASH_Pirls16.rds")



ASTPirls16 <- c()
ast <- files[316:378]
for(i in ast){
  data <- read_sav(i)
  ASTPirls16 <- rbind(ASTPirls16, data)
}
saveRDS(ASTPirls16, file = "AST_Pirls16.rds")

ATGPirls16 <- c()
atg <- files[379:441]
for(i in atg){
  data <- read_sav(i)
  ATGPirls16 <- rbind(ATGPirls16, data)
}
saveRDS(ATGPirls16, file = "ATG_Pirls16.rds")
