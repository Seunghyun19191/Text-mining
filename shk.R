install.packages("readxl")
library(readxl)
df_exam<- read_excel("csv.exam.xlsx")
url <- "https://raw.githubusercontent.com/youngwoos/Doit_R/master/Data/csv_exam.csv"
download.file(url, destfile = "csv_exam.csv", mode = "wb")
df_exam <- read.csv("csv_exam.csv")
install.packages("readr")   # 처음만 설치
library(readr)

url <- "https://raw.githubusercontent.com/youngwoos/Doit_R/master/Data/csv_exam.csv"
df_exam <- read_csv(url)

head(df_exam)   # 데이터 잘 들어왔는지 확인
df_exam <- read.csv("csv_exam.csv")
url <- "https://raw.githubusercontent.com/youngwoos/Doit_R/master/Data/csv_exam.csv"

# CSV 불러오기
df_exam <- read_csv(url)

# 데이터 확인
head(df_exam)

df_exam <- read.csv("csv_exam.csv")

# 2. 데이터 전체 출력 (20행 다 보임)
df_exam

mean(df_exam$english)
mean(df_exam$science)

read.csv("csv_exam.csv")

df_exam<-read.csv("csv_exam.csv, stringsAsFactors=F")

mpg
displ4<-mpg %>% filter(displ<=4)
displ4
displ5<-mpg %>% filter(displ>=5)
displ5
mean(displ4$hwy)
mean(displ5$hwy)

maudi<-mpg %>% filter(manufacturer=="audi")
maudi
mtoyota<-mpg %>% filter(manufacturer=="toyota")
mtoyota
mean(maudi$cty)
mean(mtoyota$cty)

mcfh<-mpg %>%  filter(manufacturer%in%c("chevrolet","ford","honda"))
mcfh
mean(mcfh$hwy)              

exam %>% select(math)
exam %>% filter(class==1)
exam %>% filter(math>60)
exam %>% select(math)
exam %>% select(math)
exam %>% select(class,math,english)
exam %>% select(-math,-english)
exam %>% 
  filter(class==1) %>% 
  select(english)


mpg %>% select(class,cty)
suv_cty<-mpg %>% filter(class=="suv")
suv_cty

compact_cty<-mpg %>% filter(class=="compact") 
compact_cty

mean(suv_cty$cty)


exam %>% arrange(desc(math))
exam %>% arrange(class,math)
mpg
mpg %>% filter(manufacturer=="audi") %>% arrange(desc(hwy)) %>% head(5)

