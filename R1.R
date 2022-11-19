library("tidyverse")
library("magrittr")
library('lubridate')
library('zoo')
commerce=read.csv("C:\\Users\\segli\\Downloads\\archive\\data.csv")
summary(commerce)
str(commerce)
# Cleanup -----------------------------------------------------------------
commerce <- commerce %>%
  filter(!is.na(CustomerID)) %>% #remove entries without customer information
  distinct() #remove duplicates
# Removing outliers---------------------------------------------------------
par(mfrow=c(1,1))
boxplot(commerce$Quantity, main ="Boxplot- Quantity") #Quantity boxplot to see if there are any outliers in column 
boxplot(commerce$UnitPrice, main = "Boxplot - Unit Price") #Unit Price boxplot to see if there are any outliers in column 
outliers1 <- boxplot(commerce$Quantity, plot=FALSE)$out #saving Outliers from Quantity in a vector
commerce<- commerce[-which(commerce$Quantity %in% outliers1),] #removing outliers vector from Quantity
outliers2 <- boxplot(commerce$UnitPrice, plot=FALSE)$out #saving Outliers from UnitPrice in a vector
commerce<- commerce[-which(commerce$UnitPrice %in% outliers2),]#removing outliers vector from UnitPrice
par(mfrow=c(1,1))
boxplot(commerce$Quantity, main ="Boxplot- Quantity") #Quantity boxplot to see if there are any outliers left
boxplot(commerce$UnitPrice, main = "Boxplot - Unit Price")#Unit Price boxplot to see if there are any outliers left
outliers3 <- boxplot(commerce$UnitPrice, plot=FALSE)$out #saving Outliers from UnitPrice in a vector
commerce<- commerce[-which(commerce$UnitPrice %in% outliers3),] #removing outliers vector from UnitPrice
boxplot(commerce$UnitPrice, main = "Boxplot - Unit Price") #Unit Price boxplot to see if there are any outliers left
#Task1-------------------------------------------------------------------------
commerce <- commerce %>% mutate(lineTotal = Quantity * UnitPrice) 
str(commerce)

task1 <- commerce %>% 
  group_by(CustomerID,InvoiceNo,InvoiceDate) %>%
  summarise(Total_Price = sum(lineTotal))

task1 <- task1 %>%
  group_by(CustomerID) %>%
  mutate(cum_sum = cumsum(Total_Price))


task1$Date <- sapply(task1$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})
task1$InvoiceDate <- as.POSIXct(task1$InvoiceDate, tryFormats = '%m/%d/%Y %H:%M')
task1 <- task1 %>%
  group_by(CustomerID) %>%
  arrange(InvoiceDate)%>%
  mutate(numbering = row_number())%>%
  ungroup()

task1 <- task1 %>%
  group_by(CustomerID) %>%
  mutate("prev_value" = lag(InvoiceNo, order_by = InvoiceDate))%>%
  ungroup()
  
task1result <- task1 %>% arrange(CustomerID,InvoiceDate)
task1result <- task1result[,-c(6)]
View(task1result)

#Task2-------------------------------------------------------------------------
commerce$Date <- sapply(commerce$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})

task <- commerce %>%
  group_by(Date)%>%
  summarise(Invoice_Total = sum(lineTotal),
            transactions = n_distinct(InvoiceNo))
  
task21 <- commerce %>%
  group_by(Date) %>%
  count(InvoiceNo, name = "Invoices")


task21 <- task21 %>%
  group_by(Date) %>%
  summarise(InvoiceCount = sum(Invoices))


task22 <- commerce %>%
  group_by(InvoiceNo,Date) %>%
  summarise(Invoice_Q = sum(Quantity))%>%
  group_by(Date)%>%
  filter(Invoice_Q > 1 | Invoice_Q < -1)%>%
  count(InvoiceNo, name = "Invoices_2plus_Items")

task22 <- task22 %>%
  group_by(Date) %>%
  summarise(Invoices_2plus = sum(Invoices_2plus_Items))

plus <- merge(x=task22, y=task21, by="Date") 

plus <- plus%>%
  group_by(Date)%>%
  mutate(Invoices_2plus_perc = Invoices_2plus/InvoiceCount * 100)

plus <- plus[,-c(2:3)]
task2 <- merge(x=task, y=plus, by="Date")

task23 <- commerce %>%
  group_by(Date) %>%
  filter(Quantity < 0)%>%
  summarise(refunds_num=sum(n_distinct(InvoiceNo)))

task221 <- merge(x=task2, y=task23, by="Date",all.x = TRUE)

task221 <- task221 %>%
  group_by(Date) %>%
  mutate(refund_rate_payments = refunds_num/transactions *100)

task24 <- commerce %>%
  group_by(Date) %>%
  filter(Quantity < 0)%>%
  summarise(refund_price=sum(lineTotal))

task222 <- merge(x=task221, y=task24, by="Date",all.x = TRUE)

task222 <- task222 %>%
  group_by(Date) %>%
  mutate(refund_sum_perc= refund_price/Invoice_Total)

task222 <- task222 %>%
  mutate(Date = mdy(Date))

task21 <- task %>%
  mutate(Date = mdy(Date))%>%
  complete(Date = full_seq(Date, period = 1),fill = list(Invoice_Total=0)) %>%
  mutate(cum_rolling7 = rollapplyr(Invoice_Total, width = 7, FUN = sum, partial = TRUE)) %>%
  mutate(cum_rolling30 = rollapplyr(Invoice_Total, width = 30, FUN = sum, partial = TRUE))

task2_result <- merge(x=task21, y=task222, by="Date", all.x = TRUE)

task2_result <- task2_result[,-c(6,7,11)]

head(task2_result, n=15)
head(task1result,n=15)

