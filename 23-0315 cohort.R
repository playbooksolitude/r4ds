#23-0315 wedn 09:25

#
library(tidyverse)
#install.packages("kohorts")
library(cohorts)

#
cohorts::gamelaunch
cohorts::online_cohorts

gamelaunch
online_cohorts

online_cohorts |> group_by(CustomerID) |> arrange(InvoiceDate)
online_cohorts |> group_by(CustomerID) |> summarise(n = n()) |> arrange(desc(n))

#ggplot
online_cohorts |> filter(CustomerID == "14911") |> 
  ggplot(aes(InvoiceDate)) + geom_point(stat = "count")

#dataset
online_cohorts |> group_by(CustomerID) |> 
  summarise(n = n()) |> arrange(desc(n)) |> slice(2:10) -> cohorts1
cohorts1[,1] -> cohorts2

  #ggplot #CustomerID #matching ----------------------------------- 변수값 비교
online_cohorts |> #filter(InvoiceDate < "2011-01-01") |> 
  filter(CustomerID == cohorts2$CustomerID) |> 
  ggplot(aes(InvoiceDate)) + 
  geom_point(stat = "count")

#all that users
online_cohorts |> ggplot(aes(InvoiceDate)) + 
  geom_point(stat = "count")

online_cohorts |> cohort_table_month(CustomerID, InvoiceDate) |> 
  pivot_longer(!cohort, 
               names_to = "period",
               values_to = "values") -> cohorts3_pivot
table(cohorts3_pivot$period)
online_cohorts
  #facet_grid
ggplot(cohorts3_pivot, aes(x = period, y = values)) + 
  geom_point() +
  facet_grid(period~cohort)

online_cohorts |> 
  separate(InvoiceDate, into = c("year", "month", "day"), 
           remove = F, convert = T, sep = "-") -> online_cohorts2
  #check
online_cohorts |> cohort_table_day(CustomerID, InvoiceDate) #없는 날짜는 안나온다
online_cohorts |> cohort_table_day(CustomerID, InvoiceDate) |> colnames()
(online_cohorts2 |> filter(year == "2010", month == "12", day == "1") -> temp1) #99
(online_cohorts2 |> filter(year == "2010", month == "12", day == "2") -> temp2) #10
(online_cohorts2 |> filter(year == "2010", month == "12", day == "3") -> temp3) #5
(online_cohorts2 |> filter(year == "2010", month == "12", day == "4") -> temp4) #0
(online_cohorts2 |> filter(year == "2010", month == "12", day == "5") -> temp5) #7

online_cohorts2 |> print(n = 1000) #1204 0건
cohort_table_day()
#
(semi_join(temp1, temp2, by = "CustomerID") -> a1_2)
(semi_join(temp1, temp3, by = "CustomerID") -> a1_3)
(semi_join(temp2, temp3, by = "CustomerID") -> a2_3)
(semi_join(temp3, temp2, by = "CustomerID") -> a3_2)

semi_join(temp3, a1_2, by = "CustomerID")
semi_join(temp2, temp5, by = "CustomerID") #2+1명 #12748 #13767 #NA
semi_join(temp5, temp2, by = "CustomerID")
temp3

online_cohorts2 |> filter(year == "2010", month == "12") |> 
  distinct(day)
  
online_cohorts2 |> filter(year == "2010", month == "12") |> 
  filter(day %in% c(1,2,3,4,5)) |> 
  filter(CustomerID %in% c("12748", "13767"))

online_cohorts2 |> filter(year == "2010", month == "12") |> 
  filter(day %in% c(3)) -> p

online_cohorts2 |> filter(year == "2010", month == "12")




p[,1] #1203 고객 56명
online_cohorts2 |> filter(year == "2010", month == "12", #217
                          day %in% c("2")) |> select(1) |> 
  semi_join(p[,1]) #10명 겹친다
#1일 2일 <-> 3일 10명 겹친다

cohort_table_day

anti_join(
  online_cohorts2 |> filter(year == "2010", month == "12", day == "1") |> select(1),
  online_cohorts2 |> filter(year == "2010", month == "12", day == "2") |> select(1)
)

anti_join(
  online_cohorts2 |> filter(year == "2010", month == "12", day == "1") |> select(1),
  online_cohorts2 |> filter(year == "2010", month == "12", day == "3") |> select(1)
)

anti_join(
  online_cohorts2 |> filter(year == "2010", month == "12", day == "5") |> select(1),
  online_cohorts2 |> filter(year == "2010", month == "12", day == "2") |> select(1)
)






























