## 2013-2020 social science Citation datasets

###############
#2014

analyze.citation.2015.1 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2015-1.txt")
analyze.citation.2015.2 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2015-2.txt")
analyze.citation.2015.3 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2015-3.txt")
analyze.citation.2015.4 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2015-4.txt")
Citation_world_2014_test <- full_join(analyze.citation.2014.1, analyze.citation.2014.2, by = "Countries.Regions")
Citation_world_2014_test <- full_join(Citation_world_2014_test, analyze.citation.2014.3, by = "Countries.Regions")

Citation_world_2014_test2 <- Citation_world_2014_test %>% 
  mutate(Record.Count.sum = Citation_world_2014_test$Record.Count.x + Citation_world_2014_test$Record.Count.y + Citation_world_2014_test$Record.Count)
Citation_world_2014_test2 <- Citation_world_2014_test2 %>% 
  mutate(Record.Count.prozent = Record.Count.sum/(70365 + 97799 +26939))
Citation_world_2014_test2$Record.Count.prozent <- 
  round(Citation_world_2014_test2$Record.Count.prozent, 3)
Citation_world_2014_test3 <- Citation_world_2014_test2 %>% 
  select(Countries.Regions, Record.Count.sum, Record.Count.prozent)

x <- Citation_world_2014_test3[c(2, 21, 41),]
UK <- c("UK", sum(x$Record.Count.sum), sum(x$Record.Count.prozent))
as.data.frame(UK)
Citation_world_2014_test3<- rbind(Citation_world_2014_test3, UK)
Citation_world_2014_test3 <- Citation_world_2014_test3[c(-2, -21, -41),]

Citation_world_2014_test3$Record.Count.prozent <- as.numeric(Citation_world_2014_test3$Record.Count.prozent)
Citation_world_2014_test3$Record.Count.sum <- as.integer(Citation_world_2014_test3$Record.Count.sum)
Citation_world_2014_test3 <- Citation_world_2014_test3 %>% arrange(desc(Record.Count.sum))

write_xlsx(Citation_world_2014_test3 ,"Citation_world_2014.xlsx")

#######################################################################

#2015

analyze.citation.2015.1 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2015-1.txt")
analyze.citation.2015.2 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2015-2.txt")
analyze.citation.2015.3 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2015-3.txt")
analyze.citation.2015.4 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2015-4.txt")
Citation_world_2015 <- inner_join(analyze.citation.2015.1, analyze.citation.2015.2, by = "Countries.Regions")
Citation_world_2015 <- inner_join(Citation_world_2015, analyze.citation.2015.3, by = "Countries.Regions")
Citation_world_2015 <- inner_join(Citation_world_2015, analyze.citation.2015.4, by = "Countries.Regions")

Citation_world_2015 <- Citation_world_2015 %>% 
  mutate(Record.Count.sum = Citation_world_2015$Record.Count.x + Citation_world_2015$Record.Count.y + Citation_world_2015$Record.Count.x.x + Citation_world_2015$Record.Count.y.y)
Citation_world_2015 <- Citation_world_2015%>% 
  mutate(Record.Count.prozent = Record.Count.sum/(72810 + 96995 + 15263 + 24315))
Citation_world_2015$Record.Count.prozent <- 
  round(Citation_world_2015$Record.Count.prozent, 3)
Citation_world_2015 <- Citation_world_2015 %>% 
  select(Countries.Regions, Record.Count.sum, Record.Count.prozent)

x <- Citation_world_2015[c(2, 20, 41),]
UK <- c("UK", sum(x$Record.Count.sum), sum(x$Record.Count.prozent))
as.data.frame(UK)
Citation_world_2015<- rbind(Citation_world_2015, UK)
Citation_world_2015 <- Citation_world_2015[c(-2, -20, -41),]

Citation_world_2015$Record.Count.prozent <- as.numeric(Citation_world_2015$Record.Count.prozent)
Citation_world_2015$Record.Count.sum <- as.integer(Citation_world_2015$Record.Count.sum)
Citation_world_2015 <- Citation_world_2015 %>% arrange(desc(Record.Count.sum))

write_xlsx(Citation_world_2015 ,"Citation_world_2015.xlsx")

#######################################################################

#2016

analyze.citation.2016.1 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2016-1.txt")
analyze.citation.2016.2 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2016-2.txt")
analyze.citation.2016.3 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2016-3.txt")
analyze.citation.2016.4 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2016-4.txt")
Citation_world_2016 <- inner_join(analyze.citation.2016.1, analyze.citation.2016.2, by = "Countries.Regions")
Citation_world_2016 <- inner_join(Citation_world_2016, analyze.citation.2016.3, by = "Countries.Regions")
Citation_world_2016 <- inner_join(Citation_world_2016, analyze.citation.2016.4, by = "Countries.Regions")

Citation_world_2016 <- Citation_world_2016 %>% 
  mutate(Record.Count.sum = Citation_world_2016$Record.Count.x + Citation_world_2016$Record.Count.y + Citation_world_2016$Record.Count.x.x + Citation_world_2016$Record.Count.y.y)
Citation_world_2016 <- Citation_world_2016%>% 
  mutate(Record.Count.prozent = Record.Count.sum/(71935 + 63253 + 26712 + 19007))
Citation_world_2016$Record.Count.prozent <- 
  round(Citation_world_2016$Record.Count.prozent, 3)
Citation_world_2016 <- Citation_world_2016 %>% 
  select(Countries.Regions, Record.Count.sum, Record.Count.prozent)

x <- Citation_world_2016[c(2, 17, 34),]
UK <- c("UK", sum(x$Record.Count.sum), sum(x$Record.Count.prozent))
as.data.frame(UK)
Citation_world_2016<- rbind(Citation_world_2016, UK)
Citation_world_2016 <- Citation_world_2016[c(-2, -17, -34),]

Citation_world_2016$Record.Count.prozent <- as.numeric(Citation_world_2016$Record.Count.prozent)
Citation_world_2016$Record.Count.sum <- as.integer(Citation_world_2016$Record.Count.sum)
Citation_world_2016 <- Citation_world_2016 %>% arrange(desc(Record.Count.sum))

write_xlsx(Citation_world_2016 ,"Citation_world_2016.xlsx")

#######################################################################

#2017

analyze.citation.2017.1 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2017-1.txt")
analyze.citation.2017.2 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2017-2.txt")
analyze.citation.2017.3 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2017-3.txt")
analyze.citation.2017.4 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2017-4.txt")
Citation_world_2017 <- inner_join(analyze.citation.2017.1, analyze.citation.2017.2, by = "Countries.Regions")
Citation_world_2017 <- inner_join(Citation_world_2017, analyze.citation.2017.3, by = "Countries.Regions")
Citation_world_2017 <- inner_join(Citation_world_2017, analyze.citation.2017.4, by = "Countries.Regions")

Citation_world_2017 <- Citation_world_2017 %>% 
  mutate(Record.Count.sum = Citation_world_2017$Record.Count.x + Citation_world_2017$Record.Count.y + Citation_world_2017$Record.Count.x.x + Citation_world_2017$Record.Count.y.y)
Citation_world_2017 <- Citation_world_2017%>% 
  mutate(Record.Count.prozent = Record.Count.sum/(60510 + 52139 + 24209 + 15679))
Citation_world_2017$Record.Count.prozent <- 
  round(Citation_world_2017$Record.Count.prozent, 3)
Citation_world_2017 <- Citation_world_2017 %>% 
  select(Countries.Regions, Record.Count.sum, Record.Count.prozent)
View(Citation_world_2017)

x <- Citation_world_2017[c(2, 16, 37),]
UK <- c("UK", sum(x$Record.Count.sum), sum(x$Record.Count.prozent))
as.data.frame(UK)
Citation_world_2017<- rbind(Citation_world_2017, UK)
Citation_world_2017 <- Citation_world_2017[c(-2, -16, -37),]

Citation_world_2017$Record.Count.prozent <- as.numeric(Citation_world_2017$Record.Count.prozent)
Citation_world_2017$Record.Count.sum <- as.integer(Citation_world_2017$Record.Count.sum)
Citation_world_2017 <- Citation_world_2017 %>% arrange(desc(Record.Count.sum))

write_xlsx(Citation_world_2017 ,"Citation_world_2017.xlsx")

#######################################################################

#2018

analyze.citation.2018.1 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2018-1.txt")
analyze.citation.2018.2 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2018-2.txt")
analyze.citation.2018.3 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2018-3.txt")
analyze.citation.2018.4 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2018-4.txt")
Citation_world_2018 <- inner_join(analyze.citation.2018.1, analyze.citation.2018.2, by = "Countries.Regions")
Citation_world_2018 <- inner_join(Citation_world_2018, analyze.citation.2018.3, by = "Countries.Regions")
Citation_world_2018 <- inner_join(Citation_world_2018, analyze.citation.2018.4, by = "Countries.Regions")

Citation_world_2018 <- Citation_world_2018 %>% 
  mutate(Record.Count.sum = Citation_world_2018$Record.Count.x + Citation_world_2018$Record.Count.y + Citation_world_2018$Record.Count.x.x + Citation_world_2018$Record.Count.y.y)
Citation_world_2018 <- Citation_world_2018%>% 
  mutate(Record.Count.prozent = Record.Count.sum/(47119 + 42085 + 18237 + 12952))
Citation_world_2018$Record.Count.prozent <- 
  round(Citation_world_2018$Record.Count.prozent, 3)
Citation_world_2018 <- Citation_world_2018 %>% 
  select(Countries.Regions, Record.Count.sum, Record.Count.prozent)
View(Citation_world_2018)

x <- Citation_world_2018[c(3, 20, 35),]
UK <- c("UK", sum(x$Record.Count.sum), sum(x$Record.Count.prozent))
as.data.frame(UK)
Citation_world_2018<- rbind(Citation_world_2018, UK)
Citation_world_2018 <- Citation_world_2018[c(-3, -20, -35),]

Citation_world_2018$Record.Count.prozent <- as.numeric(Citation_world_2018$Record.Count.prozent)
Citation_world_2018$Record.Count.sum <- as.integer(Citation_world_2018$Record.Count.sum)
Citation_world_2018 <- Citation_world_2018 %>% arrange(desc(Record.Count.sum))

write_xlsx(Citation_world_2018 ,"Citation_world_2018.xlsx")

#######################################################################

#2019

analyze.citation.2019.1 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2019-1.txt")
analyze.citation.2019.2 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2019-2.txt")
analyze.citation.2019.3 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2019-3.txt")
analyze.citation.2019.4 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2019-4.txt")
analyze.citation.2019.5 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2019-5.txt")

Citation_world_2019 <- inner_join(analyze.citation.2019.1, analyze.citation.2019.2, by = "Countries.Regions")
Citation_world_2019 <- inner_join(Citation_world_2019, analyze.citation.2019.3, by = "Countries.Regions")
Citation_world_2019 <- inner_join(Citation_world_2019, analyze.citation.2019.4, by = "Countries.Regions")
Citation_world_2019 <- inner_join(Citation_world_2019, analyze.citation.2019.5, by = "Countries.Regions")


Citation_world_2019 <- Citation_world_2019 %>% 
  mutate(Record.Count.sum = Citation_world_2019$Record.Count.x + Citation_world_2019$Record.Count.y + Citation_world_2019$Record.Count.x.x + Citation_world_2019$Record.Count.y.y + Citation_world_2019$Record.Count)
Citation_world_2019 <- Citation_world_2019%>% 
  mutate(Record.Count.prozent = Record.Count.sum/(19905 + 27017 + 29808 + 3775 + 7662))
Citation_world_2019$Record.Count.prozent <- 
  round(Citation_world_2019$Record.Count.prozent, 3)
Citation_world_2019 <- Citation_world_2019 %>% 
  select(Countries.Regions, Record.Count.sum, Record.Count.prozent)
View(Citation_world_2019)

x <- Citation_world_2019[c(2, 20, 39),]
UK <- c("UK", sum(x$Record.Count.sum), sum(x$Record.Count.prozent))
as.data.frame(UK)
Citation_world_2019<- rbind(Citation_world_2019, UK)
Citation_world_2019 <- Citation_world_2019[c(-2, -20, -39),]

Citation_world_2019$Record.Count.prozent <- as.numeric(Citation_world_2019$Record.Count.prozent)
Citation_world_2019$Record.Count.sum <- as.integer(Citation_world_2019$Record.Count.sum)
Citation_world_2019 <- Citation_world_2019 %>% arrange(desc(Record.Count.sum))

write_xlsx(Citation_world_2019 ,"Citation_world_2019.xlsx")

#######################################################################

#2020

analyze.citation.2020.1 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2020-1.txt")
analyze.citation.2020.2 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2020-2.txt")
analyze.citation.2020.3 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2020-3.txt")
analyze.citation.2020.4 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2020-4.txt")
analyze.citation.2020.5 <- read.delim("~/Documents/Master/FIW:MA/analyze citation WOS/analyze citation 2020-5.txt")

Citation_world_2020 <- inner_join(analyze.citation.2020.1, analyze.citation.2020.2, by = "Countries.Regions")
Citation_world_2020 <- inner_join(Citation_world_2020, analyze.citation.2020.3, by = "Countries.Regions")
Citation_world_2020 <- inner_join(Citation_world_2020, analyze.citation.2020.4, by = "Countries.Regions")
Citation_world_2020 <- inner_join(Citation_world_2020, analyze.citation.2020.5, by = "Countries.Regions")


Citation_world_2020 <- Citation_world_2020 %>% 
  mutate(Record.Count.sum = Citation_world_2020$Record.Count.x + Citation_world_2020$Record.Count.y + Citation_world_2020$Record.Count.x.x + Citation_world_2020$Record.Count.y.y + Citation_world_2020$Record.Count)
Citation_world_2020 <- Citation_world_2020%>% 
  mutate(Record.Count.prozent = Record.Count.sum/(10276 + 17071 + 9131 + 2028 + 3834))
Citation_world_2020$Record.Count.prozent <- 
  round(Citation_world_2020$Record.Count.prozent, 3)
Citation_world_2020 <- Citation_world_2020 %>% 
  select(Countries.Regions, Record.Count.sum, Record.Count.prozent)
View(Citation_world_2020)

x <- Citation_world_2020[c(2, 24, 47),]
UK <- c("UK", sum(x$Record.Count.sum), sum(x$Record.Count.prozent))
as.data.frame(UK)
Citation_world_2020<- rbind(Citation_world_2020, UK)
Citation_world_2020 <- Citation_world_2020[c(-2, -24, -47),]

Citation_world_2020$Record.Count.prozent <- as.numeric(Citation_world_2020$Record.Count.prozent)
Citation_world_2020$Record.Count.sum <- as.integer(Citation_world_2020$Record.Count.sum)
Citation_world_2020 <- Citation_world_2020 %>% arrange(desc(Record.Count.sum))

write_xlsx(Citation_world_2020 ,"Citation_world_2020.xlsx")

## Descriptive analysis FIW publications

# Datasets importing

FIW_Publikationen_disk_alle <- read_excel("FIW_Publikationen_disk_r.xlsx", sheet = 1)
str(FIW_Publikationen_disk_alle)
FIW_Publikationen_disk_Datenbank <- read_excel("FIW_Publikationen_disk_r.xlsx", sheet = 2)
str(FIW_Publikationen_disk_Datenbank)
FIW_Publikationen_disk_wichtig <- read_excel("FIW_Publikationen_disk_r.xlsx", sheet = 3)
str(FIW_Publikationen_disk_wichtig)

FIW_Publikationen_disk <- left_join(FIW_Publikationen_disk_alle, FIW_Publikationen_disk_Datenbank, by ="Jahr")
FIW_Publikationen_disk <- left_join(FIW_Publikationen_disk, FIW_Publikationen_disk_wichtig, by ="Jahr")
FIW_Publikationen_disk <- round(FIW_Publikationen_disk, 3)


# Rename the columns and factor Jahr
FIW_Publikationen_disk <- FIW_Publikationen_disk %>%
  rename(Jahr = Jahr,
         per_Englisch.alle = per_Englisch.x,
         per_internationale_Kollaboration.alle = per_internationale_Kollaboration.x,
         per_Google_Scholar.alle = per_Google_Scholar.x,
         per_Scopus.alle =  per_Scopus.x,
         per_WOS.alle = per_WOS.x,
         
         per_Englisch.Datenbank = per_Englisch.y,
         per_internationale_Kollaboration.Datenbank = per_internationale_Kollaboration.y,
         per_Google_Scholar.Datenbank = per_Google_Scholar.y,
         per_Scopus.Datenbank =  per_Scopus.y,
         per_WOS.Datenbank = per_WOS.y,
         
         per_Englisch.wichtig = per_Englisch,
         per_internationale_Kollaboration.wichtig = per_internationale_Kollaboration,
         per_Google_Scholar.wichtig = per_Google_Scholar,
         per_Scopus.wichtig =  per_Scopus,
         per_WOS.wichtig = per_WOS
  )
FIW_Publikationen_disk <- FIW_Publikationen_disk %>%
  mutate(Jahr = as.factor(Jahr))

# std 

FIW_Publikationen_disk_std <- scale(FIW_Publikationen_disk[,2:16])

original_cols <- colnames(FIW_Publikationen_disk_std)
colnames(FIW_Publikationen_disk_std) <- paste("std" ,original_cols,sep="_")

FIW_Publikationen_disk_std <- as.data.frame(FIW_Publikationen_disk_std)

FIW_Publikationen_disk_std <- FIW_Publikationen_disk_std %>%
  add_column(Jahr = as.integer(c(2013:2020)),
             .before = "std_per_Englisch.alle") 

# test of correlation between independent values
cor_independent_values <- cor(FIW_Publikationen_disk_std$std_per_Englisch.alle, FIW_Publikationen_disk_std$std_per_internationale_Kollaboration.alle)

ggplot(FIW_Publikationen_disk_std, aes(std_per_Englisch.alle, std_per_internationale_Kollaboration.alle))+
  geom_point()+
  geom_smooth(se = FALSE, method = "lm")

ggplot(FIW_Publikationen_disk_std, aes(Jahr, std_per_Englisch.alle , group = 1))+
  geom_line(color = "blue")+
  geom_point()+
  geom_line(aes(Jahr, std_per_internationale_Kollaboration.alle, group = 1), color = "red")+
  geom_point(aes(Jahr, std_per_internationale_Kollaboration.alle, group = 1), color = "yellow")+
  ylab("Standarized percentage of englisch/cooperatvie works")

# independent values to dependent values in total data bank
lm_goog_eng <- lm(std_per_Google_Scholar.alle ~ std_per_Englisch.alle, data =  FIW_Publikationen_disk_std)
lm_Scopus_eng <- lm(std_per_Scopus.alle ~ std_per_Englisch.alle, data =  FIW_Publikationen_disk_std)
tidy(lm_Scopus_eng)
lm_WOS_eng <- lm(std_per_WOS.alle ~ std_per_Englisch.alle, data =  FIW_Publikationen_disk_std)
tidy(lm_WOS_eng)

lm_goog_int_Kol <- lm(std_per_Google_Scholar.alle ~ std_per_internationale_Kollaboration.alle, data =  FIW_Publikationen_disk_std)
tidy(lm_goog_int_Kol)
lm_Scopus_int_Kol <- lm(std_per_Scopus.alle ~ std_per_internationale_Kollaboration.alle, data =  FIW_Publikationen_disk_std)
tidy(lm_Scopus_int_Kol)
lm_WOS_int_Kol <- lm(std_per_WOS.alle ~ std_per_internationale_Kollaboration.alle, data =  FIW_Publikationen_disk_std)
tidy(lm_WOS_int_Kol)

# only lm_WOS_int_Kol significant (p-value: 0.01859, Adjusted R-squared:  0.5691)

# Improvement of English and cooperative works by years

lm_eng_Jahr <- lm(std_per_Englisch.alle ~ Jahr, data =  FIW_Publikationen_disk_std)
summary(lm_eng_Jahr)
lm_int_Kol_Jahr <- lm(std_per_internationale_Kollaboration.alle ~ Jahr, data =  FIW_Publikationen_disk_std)
summary(lm_int_Kol_Jahr)

# only lm_eng_Jahr significant (p-value: 0.0187, Adjusted R-squared:  0.5682)

# Small sample test

shapiro.test(FIW_Publikationen_disk$per_Englisch.wichtig)
shapiro.test(FIW_Publikationen_disk$per_internationale_Kollaboration.wichtig)
shapiro.test(FIW_Publikationen_disk$per_Scopus.wichtig)
shapiro.test(FIW_Publikationen_disk$per_Google_Scholar.wichtig)
shapiro.test(FIW_Publikationen_disk$per_WOS.wichtig)
shapiro.test(FIW_Publikationen_disk$per_Englisch.Datenbank)
shapiro.test(FIW_Publikationen_disk$per_internationale_Kollaboration.Datenbank)



# T test for wichtig vs. alle
t.test(FIW_Publikationen_disk$per_Englisch.wichtig, mu = mean(FIW_Publikationen_disk$per_Englisch.alle), alternative="greater")
wilcox.test(FIW_Publikationen_disk$per_internationale_Kollaboration.wichtig, mu = mean(FIW_Publikationen_disk$per_internationale_Kollaboration.alle), alternative="greater", exact = F)
t.test(FIW_Publikationen_disk$per_Scopus.wichtig , mu = mean(FIW_Publikationen_disk$per_Scopus.alle), alternative="greater")
wilcox.test(FIW_Publikationen_disk$per_Google_Scholar.wichtig , mu = mean(FIW_Publikationen_disk$per_Google_Scholar.alle), alternative="greater", exact = F)
t.test(FIW_Publikationen_disk$per_WOS.wichtig , mu = mean(FIW_Publikationen_disk$per_WOS.alle), alternative="greater")

# per_WOS.wichtig is significant different from per_WOS.alle

# T test for Datenbank vs. alle
t.test(FIW_Publikationen_disk$per_Englisch.Datenbank, mu = mean(FIW_Publikationen_disk$per_Englisch.alle), alternative="greater")
t.test(FIW_Publikationen_disk$per_internationale_Kollaboration.Datenbank, mu = mean(FIW_Publikationen_disk$per_internationale_Kollaboration.alle), alternative="greater")
t.test(FIW_Publikationen_disk$per_Scopus.Datenbank , mu = mean(FIW_Publikationen_disk$per_Scopus.alle), alternative="greater")
t.test(FIW_Publikationen_disk$per_Google_Scholar.Datenbank , mu = mean(FIW_Publikationen_disk$per_Google_Scholar.alle), alternative="greater")
t.test(FIW_Publikationen_disk$per_WOS.Datenbank , mu = mean(FIW_Publikationen_disk$per_WOS.alle), alternative="greater", var.equal = FALSE)

# per_Englisch.Datenbank is significant different from per_Englisch.alle

# count N from each year

FIW_Publikationen_N_Jahr <- read_excel("FIW_Publikationen.xlsx", sheet = 1)%>%
  count(Jahr)%>%
  filter(Jahr%in%c("2013","2014","2015","2016","2017","2018","2019","2020"))
FIW_Publikationen_N_Jahr$Jahr <- as.integer(FIW_Publikationen_N_Jahr$Jahr)
cor(FIW_Publikationen_N_Jahr$Jahr, FIW_Publikationen_N_Jahr$n)
lm_FIW_Publikationen_N_Jahr <-lm(n ~ Jahr, data = FIW_Publikationen_N_Jahr)
summary(lm_FIW_Publikationen_N_Jahr)

# Growth significant (Adjusted R-squared:  0.725, p-value: 0.004515)

# Share of wichtig/Datenbanken Subgruppe to Grundkorpus

x <- c(17,
       19,
       31,
       36,
       30,
       45,
       45,
       39)
y <- c(3,
       0,
       4,
       4,
       3,
       8,
       9,
       7)
z <- c(4,
       4,
       9,
       6,
       5,
       14,
       13,
       7)

total_anteil_wichtig <- round(y / x, 3)
total_anteil_datenbank <- round(z / x, 3)

FIW_Publikationen_disk$total_anteil_wichtig <- total_anteil_wichtig
FIW_Publikationen_disk$total_anteil_datenbank <- total_anteil_datenbank

# Improvement of percentage of wichtig/Datenbanken Subgruppe by years

lm_at_wichtig_Jahr <- lm(total_anteil_wichtig ~ Jahr, data =  FIW_Publikationen_disk)
summary(lm_at_wichtig_Jahr)
lm_at_datenbank_Jahr <- lm(total_anteil_datenbank ~ Jahr, data =  FIW_Publikationen_disk)
summary(lm_at_datenbank_Jahr)

# No significant

## Bibliometric analysis FIW publications

# Datasets importing

FIW_Publikationen_biblio_by_jahr <- read_excel("FIW_Publikationen_biblio_r.xlsx", sheet = 1)
str(FIW_Publikationen_biblio_by_jahr)
FIW_Publikationen_biblio_origindaten <- read.xlsx("FIW_Publikationen_biblio_r.xlsx", sheet = 2)
str(FIW_Publikationen_biblio_origindaten)

FIW_Publikationen_biblio_origindaten$Margalefs_richness_index_Zitate_Scopus <- as.numeric(FIW_Publikationen_biblio_origindaten$Margalefs_richness_index_Zitate_Scopus)
FIW_Publikationen_biblio_origindaten$Margalefs_richness_index_Zitate_WOS <- as.numeric(FIW_Publikationen_biblio_origindaten$Margalefs_richness_index_Zitate_WOS)
FIW_Publikationen_biblio_origindaten$Margalefs_richness_index_Autoren <- as.numeric(FIW_Publikationen_biblio_origindaten$Margalefs_richness_index_Autoren)

FIW_Publikationen_biblio_origindaten_name <- FIW_Publikationen_biblio_origindaten[,2]
FIW_Publikationen_biblio_origindaten_name

FIW_Publikationen_biblio_origindaten_round <- round(FIW_Publikationen_biblio_origindaten[,-2], 3)
str(FIW_Publikationen_biblio_origindaten_round)
FIW_Publikationen_biblio_origindaten_round[is.na(FIW_Publikationen_biblio_origindaten_round)]<-0


FIW_Publikationen_biblio_origindaten <- FIW_Publikationen_biblio_origindaten_round%>%
  add_column(Publikationen = FIW_Publikationen_biblio_origindaten_name, 
             .before = "richness_Zitate_Scopus" )


# cluster analysis by kmeans

set.seed(235)
km <- kmeans(FIW_Publikationen_biblio_origindaten_round, centers = 3, nstart = 25)
fviz_cluster(km, data = FIW_Publikationen_biblio_origindaten_round)
str(km)

FIW_Publikationen_biblio_origindaten <- FIW_Publikationen_biblio_origindaten%>%
  mutate(cluster = as_factor(km$cluster))

str(FIW_Publikationen_biblio_origindaten_round)

#ggplot by richness_Zitate_WOS, Margalefs_richness_index_Autoren

ggplot(FIW_Publikationen_biblio_origindaten, aes(richness_Zitate_WOS, Margalefs_richness_index_Autoren, shape = cluster, color = as.factor(Jahr)))+
  geom_point(position="jitter")+
  scale_shape_manual(values=c(0, 1, 2))
ggplot(FIW_Publikationen_biblio_origindaten, aes(richness_Zitate_WOS, Margalefs_richness_index_Autoren, shape = cluster))+
  geom_point(position="jitter")


# 3 clusters is the best  

png(
  filename = "Elbow-Methode in K-means.png",
  res = 300, # 300ppi 
  width = 1650, height = 965,
  bg = "transparent" 
)

fviz_nbclust(FIW_Publikationen_biblio_origindaten_round, 
             FUNcluster = hcut,  # hierarchical clustering
             method = "wss",     # total within sum of square
             k.max = 12          # max number of clusters to consider
) + 
  
  labs(title = "Diagramm 1:",
       subtitle ="Elbow-Methode in K-means",
       x = "Anzahl der K Clusters",
       y = "Streuung innerhalb der K Cluster") +
  theme(text=element_text(family="Times New Roman", size=10),
        plot.title = element_text(family="Times New Roman", size=10, face = "bold"),
        plot.subtitle = element_text(family="Times New Roman", size=10, face = "italic"))+
  
  
  geom_vline(xintercept = 3,     
             linetype = 2)

dev.off()



# PCA



pca<-prcomp(FIW_Publikationen_biblio_origindaten_round[,-1], scale = TRUE)

str(pca)


plot(pca,         
     type="line") 

abline(h=1, lty = 2) 


pca_rotations.df <- as.data.frame(pca$rotation)

pca_rotation_sum <- pca_rotations.df %>%
  colSums() %>%
  as.vector()

str(pca_rotation_sum)
pca_rotation_sum <- data.frame(PC= paste0("PC",1:14),
                               rotation = pca_rotation_sum)

### Export

png(
  filename = "Screeplot.png",
  res = 300, # 300ppi 
  width = 1650, height = 965,
  bg = "transparent" 
)


ggplot(pca_rotation_sum, aes(x=PC,y=rotation, group=1))+
  geom_point(size=2, shape = 1)+
  geom_line()+
  geom_hline(yintercept = 1,     
             linetype = 2)+
  labs(title = "Diagramm 2:",
       subtitle = "Screeplot für die Hauptkomponentenanalyse",
       
       x = "Faktor",
       y = "Eigenwert")+
  theme(text=element_text(family="Times New Roman", size=10),
        plot.title = element_text(family="Times New Roman", size=10, face = "bold"),
        plot.subtitle = element_text(family="Times New Roman", size=10, face = "italic"))

dev.off( )


labs(title="Scree plot: PCA on scaled data")


## plot pc1 and pc2
plot(pca$x[,1], pca$x[,2])

## make a scree plot
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")


# find out which 2 original variables match principal components best

plot(pca$x[,1], pca$x[,2])

pca.data <- data.frame(Jahr=FIW_Publikationen_biblio_origindaten_round[,1],
                       Sample=rownames(pca$x),
                       X=pca$x[,1],
                       Y=pca$x[,2],
                       cluster = km$cluster)
str(pca.data)

ggplot(data=pca.data, aes(x=X, y=Y, shape = as.factor(cluster))) +
  geom_point(position = "jitter", alpha = 0.5, size = 5) +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("Hauptkomponentenanalyse der bibliometrischen Daten mit dem Clustering durch K-means")+
  labs(shape = "Clusters")+
  theme(text=element_text(family="Times New Roman", size=10))

## Export


png(
  filename = "Hauptkomponentenanalyse.png",
  res = 300, # 300ppi 
  width = 1650, height = 965,
  bg = "transparent" 
)

ggplot(data=pca.data, aes(x=X, y=Y, shape = as.factor(cluster))) +
  geom_point(position = "jitter", alpha = 0.5, size = 5) +
  labs(title = "Diagramm 3:",
       subtitle = "Hauptkomponentenanalyse der bibliometrischen Daten mit dem Clustering durch K-means",
       shape = "Clusters",
       x = paste("Hauptkomponente 1 - ", pca.var.per[1], "%", sep=""),
       y = paste("Hauptkomponente 2 - ", pca.var.per[2], "%", sep=""))+
  theme(text=element_text(family="Times New Roman", size=10),
        plot.title = element_text(family="Times New Roman", size=10, face = "bold"),
        plot.subtitle = element_text(family="Times New Roman", size=10, face = "italic"))

dev.off( )

########################

# PCA top 10 genes

loading_scores1 <- pca$rotation[,1]
gene_scores1 <- abs(loading_scores1) 
gene_score_ranked1 <- sort(gene_scores1, decreasing=TRUE)
top_10_genes1 <- names(gene_score_ranked1[1:10])

top_10_genes1 

pca_rotation1 <- pca$rotation[top_10_genes1,1] 
str(pca_rotation1)

pca_rotation1.df <- as.data.frame(pca_rotation1)

loading_scores2 <- pca$rotation[,2]
gene_scores2 <- abs(loading_scores2) 
gene_score_ranked2 <- sort(gene_scores2, decreasing=TRUE)
top_10_genes2 <- names(gene_score_ranked2[1:10])


top_10_genes2

pca_rotation2 <- pca$rotation[top_10_genes2,2] 
str(pca_rotation2)

pca_rotation2.df <- as.data.frame(pca_rotation2)




# Yearly development of bibliometric values



FIW_Publikationen_biblio_by_jahr_name <- FIW_Publikationen_biblio_by_jahr[,1]

FIW_Publikationen_biblio_by_jahr_round <- round(FIW_Publikationen_biblio_by_jahr[,-1],3)
FIW_Publikationen_biblio_by_jahr_round[is.na(FIW_Publikationen_biblio_by_jahr_round)]<- 0


# scale between 0 to 1
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

richness_Zitate_Scopus <- range01(FIW_Publikationen_biblio_by_jahr_round$richness_Zitate_Scopus)
richness_Zitate_WOS <- range01(FIW_Publikationen_biblio_by_jahr_round$richness_Zitate_WOS)
richness_Autoren <- range01(FIW_Publikationen_biblio_by_jahr_round$richness_Autoren)
diversity_Zitate_Scopus <- range01(FIW_Publikationen_biblio_by_jahr_round$diversity_Zitate_Scopus)
diversity_Zitate_WOS <- range01(FIW_Publikationen_biblio_by_jahr_round$diversity_Zitate_WOS)
diversity_Autoren <- range01(FIW_Publikationen_biblio_by_jahr_round$diversity_Autoren)
evenness_Zitate_Scopus <- range01(FIW_Publikationen_biblio_by_jahr_round$evenness_Zitate_Scopus)
evenness_Zitate_WOS <- range01(FIW_Publikationen_biblio_by_jahr_round$evenness_Zitate_WOS)
evenness_Autoren <- range01(FIW_Publikationen_biblio_by_jahr_round$evenness_Autoren)
RI_EUCLID_Zitate_WOS <- range01(FIW_Publikationen_biblio_by_jahr_round$RI_EUCLID_Zitate_WOS)
RI_EUCLID_Autoren <- range01(FIW_Publikationen_biblio_by_jahr_round$RI_EUCLID_Autoren)
Margalefs_richness_index_Zitate_Scopus <- range01(FIW_Publikationen_biblio_by_jahr_round$Margalefs_richness_index_Zitate_Scopus)
Margalefs_richness_index_Zitate_WOS <- range01(FIW_Publikationen_biblio_by_jahr_round$Margalefs_richness_index_Zitate_WOS)
Margalefs_richness_index_Autoren <- range01(FIW_Publikationen_biblio_by_jahr_round$Margalefs_richness_index_Autoren)

FIW_Publikationen_biblio_by_jahr_scale <- as.data.frame(
  round( 
    cbind(richness_Zitate_Scopus, richness_Zitate_WOS, richness_Autoren,
          diversity_Zitate_Scopus, diversity_Zitate_WOS, diversity_Autoren,
          evenness_Zitate_Scopus, evenness_Zitate_WOS, evenness_Autoren,
          RI_EUCLID_Zitate_WOS, RI_EUCLID_Autoren,
          Margalefs_richness_index_Zitate_Scopus, Margalefs_richness_index_Zitate_WOS, Margalefs_richness_index_Autoren)
    , 3)
)




FIW_Publikationen_biblio_by_jahr_scale <- cbind(FIW_Publikationen_biblio_by_jahr_name, FIW_Publikationen_biblio_by_jahr_scale)  
colnames(FIW_Publikationen_biblio_by_jahr_scale)[1]<- "Jahr"

str(FIW_Publikationen_biblio_by_jahr)

# Regression analysis

lm1 <- lm(richness_Zitate_Scopus ~ Jahr, data = FIW_Publikationen_biblio_by_jahr)
tidy(lm1)
lm2 <- lm(richness_Zitate_WOS ~ Jahr, data = FIW_Publikationen_biblio_by_jahr)
tidy(lm2)
lm3 <- lm(richness_Autoren ~ Jahr, data = FIW_Publikationen_biblio_by_jahr)
tidy(lm3)
lm4 <- lm(diversity_Zitate_Scopus ~ Jahr, data = FIW_Publikationen_biblio_by_jahr)
tidy(lm4)
lm5 <- lm(diversity_Zitate_WOS ~ Jahr, data = FIW_Publikationen_biblio_by_jahr)
tidy(lm5)
lm6 <- lm(diversity_Autoren ~ Jahr, data = FIW_Publikationen_biblio_by_jahr)
tidy(lm6)
lm7 <- lm(evenness_Zitate_Scopus ~ Jahr, data = FIW_Publikationen_biblio_by_jahr)
tidy(lm7)
lm8 <- lm(evenness_Zitate_WOS ~ Jahr, data = FIW_Publikationen_biblio_by_jahr)
tidy(lm8)
lm9 <- lm(evenness_Autoren ~ Jahr, data = FIW_Publikationen_biblio_by_jahr)
tidy(lm9)
lm10 <- lm(RI_EUCLID_Zitate_WOS ~ Jahr, data = FIW_Publikationen_biblio_by_jahr)
tidy(lm10)
lm11 <- lm(RI_EUCLID_Autoren ~ Jahr, data = FIW_Publikationen_biblio_by_jahr)
tidy(lm11)
lm12 <- lm(Margalefs_richness_index_Zitate_Scopus ~ Jahr, data = FIW_Publikationen_biblio_by_jahr)
tidy(lm12)
lm13 <- lm(Margalefs_richness_index_Zitate_WOS ~ Jahr, data = FIW_Publikationen_biblio_by_jahr)
tidy(lm13)
lm14 <- lm(Margalefs_richness_index_Autoren ~ Jahr, data = FIW_Publikationen_biblio_by_jahr)
tidy(lm14)

# all insignificant, failed
# means that FIW members and publications do not change 
# in terms of internationality after the passage of time