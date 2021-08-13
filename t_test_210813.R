#starbucks <- read.csv("c:/users/keunb/OneDrive/바탕 화면/starbucks_clustering.csv")
#str(starbucks)
#starbucks_z <- as.data.frame(lapply(starbucks[2:3], scale))
#starbucks_cluster5 <- kmeans(starbucks_z, 5)
#starbucks_cluster4 <- kmeans(starbucks_z, 4)
#starbucks_after <- read.csv("c:/users/keunb/OneDrive/바탕 화면/starbucks_after_cluster.csv")
#aggregate(data=starbucks_after, 유동인구_평균~cluster, mean)
#aggregate(data=starbucks_after, 차선수~cluster, mean)

#group0
t_test_group0 <- read.csv("G:/내 드라이브/2021 공공빅데이터 청년인턴/프로젝트/데이터/t_test_group0.csv")
t.test(t_test_group0$일반, t_test_group0$종암DT, alternative='greater')

#group1
t_test_group1 <- read.csv("G:/내 드라이브/2021 공공빅데이터 청년인턴/프로젝트/데이터/t_test_group1.csv")
t.test(t_test_group1$일반, t_test_group1$송파나루역DT, alternative='greater')
t.test(t_test_group1$일반, t_test_group1$신정DT, alternative='greater')
t.test(t_test_group1$일반, t_test_group1$DT, alternative='greater')

#group5
t_test_group5 <- read.csv("G:/내 드라이브/2021 공공빅데이터 청년인턴/프로젝트/데이터/t_test_group5.csv")
t.test(t_test_group5$일반, t_test_group5$강동구청DT, alternative='greater')
t.test(t_test_group5$일반, t_test_group5$공릉DT, alternative='greater')
t.test(t_test_group5$일반, t_test_group5$송파마천사거리DT, alternative='greater')
t.test(t_test_group5$일반, t_test_group5$연희DT, alternative='greater')
t.test(t_test_group5$일반, t_test_group5$화곡DT, alternative='greater')
t.test(t_test_group5$일반, t_test_group5$DT, alternative='greater')

#group6
t_test_group6 <- read.csv("G:/내 드라이브/2021 공공빅데이터 청년인턴/프로젝트/데이터/t_test_group6.csv")
t.test(t_test_group6$일반, t_test_group6$구의DT, alternative='greater')
t.test(t_test_group6$일반, t_test_group6$송파방이DT, alternative='greater')
t.test(t_test_group6$일반, t_test_group6$DT, alternative='greater')


#group7
t_test_group7 <- read.csv("G:/내 드라이브/2021 공공빅데이터 청년인턴/프로젝트/데이터/t_test_group7.csv")
t.test(t_test_group7$일반, t_test_group7$낙성대DT, alternative='greater')
t.test(t_test_group7$일반, t_test_group7$신림DT, alternative='greater')
t.test(t_test_group7$일반, t_test_group7$신월동DT, alternative='greater')
t.test(t_test_group7$일반, t_test_group7$DT, alternative='greater')





#group0 8시 + 9시
#DT 매장이 1개밖에 없어서 8시9시 합쳤습니다.
t_test0 <- read.csv("G:/내 드라이브/2021 공공빅데이터 청년인턴/프로젝트/데이터/t_test_group0_8시.csv")
var.test(t_test0$일반8, t_test0$DT8)

t.test(t_test0$일반8, t_test0$DT8, alternative='greater')
t.test(t_test0$일반8, t_test0$DT8, alternative='greater', var.equal=T)

#group1 8시, 9시
t_test1 <- read.csv("G:/내 드라이브/2021 공공빅데이터 청년인턴/프로젝트/데이터/t_test_group1_8시.csv")
var.test(t_test1$일반8, t_test1$DT8)
var.test(t_test1$일반9, t_test1$DT9)

t.test(t_test1$일반8, t_test1$DT8, alternative='greater') #8시 결과
t.test(t_test1$일반8, t_test1$DT8, alternative='greater', var.equal=T) #8시 결과-분산동일

t.test(t_test1$일반9, t_test1$DT9, alternative='greater') #9시 결과
t.test(t_test1$일반9, t_test1$DT9, alternative='greater', var.equal=T) #9시 결과-분산 동일

#group5 8시
t_test5 <- read.csv("G:/내 드라이브/2021 공공빅데이터 청년인턴/프로젝트/데이터/t_test_group5_8시.csv")
var.test(t_test5$일반, t_test5$DT)

t.test(t_test5$일반, t_test5$DT, alternative='greater', var.equal=T)

#group6 8시, 9시
t_test6 <- read.csv("G:/내 드라이브/2021 공공빅데이터 청년인턴/프로젝트/데이터/t_test_group6_8시.csv")
var.test(t_test6$일반8, t_test6$DT8)
var.test(t_test6$일반9, t_test6$DT9)

t.test(t_test6$일반8, t_test6$DT8, alternative='greater', var.equal=T)
t.test(t_test6$일반9, t_test6$DT9, alternative='greater', var.equal=T)


#group7 8시, 9시
t_test7 <- read.csv("G:/내 드라이브/2021 공공빅데이터 청년인턴/프로젝트/데이터/t_test_group7_8시.csv")
var.test(t_test7$일반8, t_test7$DT8)
var.test(t_test7$일반9, t_test7$DT9)

t.test(t_test7$일반8, t_test7$DT8, alternative='greater', var.equal=T)
t.test(t_test6$일반9, t_test6$DT9, alternative='greater', var.equal=T)


#cluster0_weekday----------
cluster0 <- read.csv("G:/내 드라이브/2021 공공빅데이터 청년인턴/프로젝트/데이터/agg_group0_weekday_ttest.csv")
cluster0

#07시
var.test(cluster0$일반07, cluster0$신림dt07)
t.test(cluster0$일반07, cluster0$신림dt07, alternative='greater')

var.test(cluster0$일반07, cluster0$신월동dt07)
t.test(cluster0$일반07, cluster0$신월동dt07, alternative='greater')

var.test(cluster0$일반07, cluster0$신정dt07)
t.test(cluster0$일반07, cluster0$신정dt07, alternative='greater')

var.test(cluster0$일반07, cluster0$종암dt07)
t.test(cluster0$일반07, cluster0$종암dt07, alternative='greater')

#09시
var.test(cluster0$일반09, cluster0$신림dt09)
t.test(cluster0$일반09, cluster0$신림dt09, alternative='greater')
t.test(cluster0$일반09, cluster0$신림dt09, alternative='greater', var.equal=T)

var.test(cluster0$일반09, cluster0$신월동dt09)
t.test(cluster0$일반09, cluster0$신월동dt09, alternative='greater', var.equal=T)

var.test(cluster0$일반09, cluster0$신정dt09)
t.test(cluster0$일반09, cluster0$신정dt09, alternative='greater')

var.test(cluster0$일반09, cluster0$종암dt09)
t.test(cluster0$일반09, cluster0$종암dt09, alternative='greater')

#12시
var.test(cluster0$일반12, cluster0$신림dt12)
t.test(cluster0$일반12, cluster0$신림dt12, alternative='greater')
t.test(cluster0$일반12, cluster0$신림dt12, alternative='greater', var.equal=T)

var.test(cluster0$일반12, cluster0$신월동dt12)
t.test(cluster0$일반12, cluster0$신월동dt12, alternative='greater')
t.test(cluster0$일반12, cluster0$신월동dt12, alternative='greater', var.equal=T)

var.test(cluster0$일반12, cluster0$신정dt12)
t.test(cluster0$일반12, cluster0$신정dt12, alternative='greater')

var.test(cluster0$일반12, cluster0$종암dt12)
t.test(cluster0$일반12, cluster0$종암dt12, alternative='greater')

#18시
var.test(cluster0$일반18, cluster0$신림dt18)
t.test(cluster0$일반18, cluster0$신림dt18, alternative='greater')

var.test(cluster0$일반18, cluster0$신월동dt18)
t.test(cluster0$일반18, cluster0$신월동dt18, alternative='greater')

var.test(cluster0$일반18, cluster0$신정dt18)
t.test(cluster0$일반18, cluster0$신정dt18, alternative='greater')

var.test(cluster0$일반18, cluster0$종암dt18)
t.test(cluster0$일반18, cluster0$종암dt18, alternative='greater')


#cluster1_weekday ----------
cluster1_weekday <- read.csv("G:/내 드라이브/2021 공공빅데이터 청년인턴/프로젝트/데이터/agg_group1_weekday_ttest.csv")
cluster1_weekday

#07시
var.test(cluster1_weekday$일반07, cluster1_weekday$강동구청dt07)
t.test(cluster1_weekday$일반07, cluster1_weekday$강동구청dt07, alternative='greater')

var.test(cluster1_weekday$일반07, cluster1_weekday$구의dt07)
t.test(cluster1_weekday$일반07, cluster1_weekday$구의dt07, alternative='greater')

var.test(cluster1_weekday$일반07, cluster1_weekday$송파마천사거리dt07)
t.test(cluster1_weekday$일반07, cluster1_weekday$송파마천사거리dt07, alternative='greater')

var.test(cluster1_weekday$일반07, cluster1_weekday$송파방이dt07)
t.test(cluster1_weekday$일반07, cluster1_weekday$송파방이dt07, alternative='greater')

var.test(cluster1_weekday$일반07, cluster1_weekday$신월icdt07)
t.test(cluster1_weekday$일반07, cluster1_weekday$신월icdt07, alternative='greater')

var.test(cluster1_weekday$일반07, cluster1_weekday$연희dt07)
t.test(cluster1_weekday$일반07, cluster1_weekday$연희dt07, alternative='greater')

var.test(cluster1_weekday$일반07, cluster1_weekday$화곡dt07)
t.test(cluster1_weekday$일반07, cluster1_weekday$화곡dt07, alternative='greater')

#09시
var.test(cluster1_weekday$일반09, cluster1_weekday$강동구청dt09)
t.test(cluster1_weekday$일반09, cluster1_weekday$강동구청dt09, alternative='greater')

var.test(cluster1_weekday$일반09, cluster1_weekday$구의dt09)
t.test(cluster1_weekday$일반09, cluster1_weekday$구의dt09, alternative='greater')

var.test(cluster1_weekday$일반09, cluster1_weekday$송파마천사거리dt09)
t.test(cluster1_weekday$일반09, cluster1_weekday$송파마천사거리dt09, alternative='greater')

var.test(cluster1_weekday$일반09, cluster1_weekday$송파방이dt09)
t.test(cluster1_weekday$일반09, cluster1_weekday$송파방이dt09, alternative='greater')

var.test(cluster1_weekday$일반09, cluster1_weekday$신월icdt09)
t.test(cluster1_weekday$일반09, cluster1_weekday$신월icdt09, alternative='greater')

var.test(cluster1_weekday$일반09, cluster1_weekday$연희dt07)
t.test(cluster1_weekday$일반09, cluster1_weekday$연희dt07, alternative='greater')

var.test(cluster1_weekday$일반09, cluster1_weekday$화곡dt07)
t.test(cluster1_weekday$일반09, cluster1_weekday$화곡dt07, alternative='greater')

#12시
var.test(cluster1_weekday$일반12, cluster1_weekday$강동구청dt12)
t.test(cluster1_weekday$일반12, cluster1_weekday$강동구청dt12, alternative='greater', var.equal=T)

var.test(cluster1_weekday$일반12, cluster1_weekday$구의dt112)
t.test(cluster1_weekday$일반12, cluster1_weekday$구의dt12, alternative='greater', var.equal=T)

var.test(cluster1_weekday$일반12, cluster1_weekday$송파마천사거리dt12)
t.test(cluster1_weekday$일반12, cluster1_weekday$송파마천사거리dt12, alternative='greater')

var.test(cluster1_weekday$일반12, cluster1_weekday$송파방이dt12)
t.test(cluster1_weekday$일반12, cluster1_weekday$송파방이dt12, alternative='greater', var.equal=T)

var.test(cluster1_weekday$일반12, cluster1_weekday$신월icdt12)
t.test(cluster1_weekday$일반12, cluster1_weekday$신월icdt12, alternative='greater')

var.test(cluster1_weekday$일반12, cluster1_weekday$연희dt12)
t.test(cluster1_weekday$일반12, cluster1_weekday$연희dt12, alternative='greater')

var.test(cluster1_weekday$일반12, cluster1_weekday$화곡dt12)
t.test(cluster1_weekday$일반12, cluster1_weekday$화곡dt12, alternative='greater')

#18시
var.test(cluster1_weekday$일반18, cluster1_weekday$강동구청dt18)
t.test(cluster1_weekday$일반18, cluster1_weekday$강동구청dt18, alternative='greater')

var.test(cluster1_weekday$일반18, cluster1_weekday$구의dt18)
t.test(cluster1_weekday$일반18, cluster1_weekday$구의dt18, alternative='greater', var.equal=T)

var.test(cluster1_weekday$일반18, cluster1_weekday$송파마천사거리dt18)
t.test(cluster1_weekday$일반18, cluster1_weekday$송파마천사거리dt18, alternative='greater')

var.test(cluster1_weekday$일반18, cluster1_weekday$송파방이dt18)
t.test(cluster1_weekday$일반18, cluster1_weekday$송파방이dt18, alternative='greater')

var.test(cluster1_weekday$일반18, cluster1_weekday$신월icdt18)
t.test(cluster1_weekday$일반18, cluster1_weekday$신월icdt18, alternative='greater', var.equal=T)

var.test(cluster1_weekday$일반18, cluster1_weekday$연희dt18)
t.test(cluster1_weekday$일반18, cluster1_weekday$연희dt18, alternative='greater')

var.test(cluster1_weekday$일반18, cluster1_weekday$화곡dt18)
t.test(cluster1_weekday$일반18, cluster1_weekday$화곡dt18, alternative='greater')



#cluster0_weekend----------
cluster0_weekend <- read.csv("G:/내 드라이브/2021 공공빅데이터 청년인턴/프로젝트/데이터/agg_group0_weekend_ttest.csv")
cluster0_weekend

#07시
var.test(cluster0_weekend$일반07, cluster0_weekend$신림dt07)
t.test(cluster0_weekend$일반07, cluster0_weekend$신림dt07, alternative='greater')

var.test(cluster0_weekend$일반07, cluster0_weekend$신월동dt07)
t.test(cluster0_weekend$일반07, cluster0_weekend$신월동dt07, alternative='greater')

var.test(cluster0_weekend$일반07, cluster0_weekend$신정dt07)
t.test(cluster0_weekend$일반07, cluster0_weekend$신정dt07, alternative='greater')

var.test(cluster0$일반07, cluster0$종암dt07)
t.test(cluster0_weekend$일반07, cluster0_weekend$종암dt07, alternative='greater')

#09시
var.test(cluster0_weekend$일반09, cluster0_weekend$신림dt09)
t.test(cluster0_weekend$일반09, cluster0_weekend$신림dt09, alternative='greater')

var.test(cluster0_weekend$일반09, cluster0_weekend$신월동dt09)
t.test(cluster0_weekend$일반09, cluster0_weekend$신월동dt09, alternative='greater', var.equal=T)

var.test(cluster0_weekend$일반09, cluster0_weekend$신정dt09)
t.test(cluster0_weekend$일반09, cluster0_weekend$신정dt09, alternative='greater')

var.test(cluster0_weekend$일반09, cluster0_weekend$종암dt09)
t.test(cluster0_weekend$일반09, cluster0_weekend$종암dt09, alternative='greater')

#12시
var.test(cluster0_weekend$일반12, cluster0_weekend$신림dt12)
t.test(cluster0_weekend$일반12, cluster0_weekend$신림dt12, alternative='greater')
t.test(cluster0_weekend$일반12, cluster0_weekend$신림dt12, alternative='greater', var.equal=T)

var.test(cluster0_weekend$일반12, cluster0_weekend$신월동dt12)
t.test(cluster0_weekend$일반12, cluster0_weekend$신월동dt12, alternative='greater')
t.test(cluster0_weekend$일반12, cluster0_weekend$신월동dt12, alternative='greater', var.equal=T)

var.test(cluster0_weekend$일반12, cluster0_weekend$신정dt12)
t.test(cluster0_weekend$일반12, cluster0_weekend$신정dt12, alternative='greater')

var.test(cluster0_weekend$일반12, cluster0_weekend$종암dt12)
t.test(cluster0_weekend$일반12, cluster0_weekend$종암dt12, alternative='greater')

#18시
var.test(cluster0_weekend$일반18, cluster0_weekend$신림dt18)
t.test(cluster0_weekend$일반18, cluster0_weekend$신림dt18, alternative='greater')

var.test(cluster0_weekend$일반18, cluster0_weekend$신월동dt18)
t.test(cluster0_weekend$일반18, cluster0_weekend$신월동dt18, alternative='greater')

var.test(cluster0_weekend$일반18, cluster0_weekend$신정dt18)
t.test(cluster0_weekend$일반18, cluster0_weekend$신정dt18, alternative='greater')

var.test(cluster0_weekend$일반18, cluster0_weekend$종암dt18)
t.test(cluster0_weekend$일반18, cluster0_weekend$종암dt18, alternative='greater')


#cluster1_weekend----------
cluster1_weekend <- read.csv("G:/내 드라이브/2021 공공빅데이터 청년인턴/프로젝트/데이터/agg_group1_weekend_ttest.csv")
cluster1_weekend

#07시
var.test(cluster1_weekend$일반07, cluster1_weekend$강동구청dt07)
t.test(cluster1_weekend$일반07, cluster1_weekend$강동구청dt07, alternative='greater')

var.test(cluster1_weekend$일반07, cluster1_weekend$구의dt07)
t.test(cluster1_weekend$일반07, cluster1_weekend$구의dt07, alternative='greater')

var.test(cluster1_weekend$일반07, cluster1_weekend$송파마천사거리dt07)
t.test(cluster1_weekend$일반07, cluster1_weekend$송파마천사거리dt07, alternative='greater')

var.test(cluster1_weekend$일반07, cluster1_weekend$송파방이dt07)
t.test(cluster1_weekend$일반07, cluster1_weekend$송파방이dt07, alternative='greater')

var.test(cluster1_weekend$일반07, cluster1_weekend$신월icdt07)
t.test(cluster1_weekend$일반07, cluster1_weekend$신월icdt07, alternative='greater')

var.test(cluster1_weekend$일반07, cluster1_weekend$연희dt07)
t.test(cluster1_weekend$일반07, cluster1_weekend$연희dt07, alternative='greater')

var.test(cluster1_weekend$일반07, cluster1_weekend$화곡dt07)
t.test(cluster1_weekend$일반07, cluster1_weekend$화곡dt07, alternative='greater')

#09시
var.test(cluster1_weekend$일반09, cluster1_weekend$강동구청dt09)
t.test(cluster1_weekend$일반09, cluster1_weekend$강동구청dt09, alternative='greater')

var.test(cluster1_weekend$일반09, cluster1_weekend$구의dt09)
t.test(cluster1_weekend$일반09, cluster1_weekend$구의dt09, alternative='greater')

var.test(cluster1_weekend$일반09, cluster1_weekend$송파마천사거리dt09)
t.test(cluster1_weekend$일반09, cluster1_weekend$송파마천사거리dt09, alternative='greater')

var.test(cluster1_weekend$일반09, cluster1_weekend$송파방이dt09)
t.test(cluster1_weekend$일반09, cluster1_weekend$송파방이dt09, alternative='greater')

var.test(cluster1_weekend$일반09, cluster1_weekend$신월icdt09)
t.test(cluster1_weekend$일반09, cluster1_weekend$신월icdt09, alternative='greater')

var.test(cluster1_weekend$일반09, cluster1_weekend$연희dt07)
t.test(cluster1_weekend$일반09, cluster1_weekend$연희dt07, alternative='greater')

var.test(cluster1_weekend$일반09, cluster1_weekend$화곡dt07)
t.test(cluster1_weekend$일반09, cluster1_weekend$화곡dt07, alternative='greater')

#12시
var.test(cluster1_weekend$일반12, cluster1_weekend$강동구청dt12)
t.test(cluster1_weekend$일반12, cluster1_weekend$강동구청dt12, alternative='greater', var.equal=T)

var.test(cluster1_weekend$일반12, cluster1_weekend$구의dt12)
t.test(cluster1_weekend$일반12, cluster1_weekend$구의dt12, alternative='greater', var.equal=T)

var.test(cluster1_weekend$일반12, cluster1_weekend$송파마천사거리dt12)
t.test(cluster1_weekend$일반12, cluster1_weekend$송파마천사거리dt12, alternative='greater')

var.test(cluster1_weekend$일반12, cluster1_weekend$송파방이dt12)
t.test(cluster1_weekend$일반12, cluster1_weekend$송파방이dt12, alternative='greater', var.equal=T)

var.test(cluster1_weekend$일반12, cluster1_weekend$신월icdt12)
t.test(cluster1_weekend$일반12, cluster1_weekend$신월icdt12, alternative='greater')

var.test(cluster1_weekend$일반12, cluster1_weekend$연희dt12)
t.test(cluster1_weekend$일반12, cluster1_weekend$연희dt12, alternative='greater')

var.test(cluster1_weekend$일반12, cluster1_weekend$화곡dt12)
t.test(cluster1_weekend$일반12, cluster1_weekend$화곡dt12, alternative='greater')

#18시
var.test(cluster1_weekend$일반18, cluster1_weekend$강동구청dt18)
t.test(cluster1_weekend$일반18, cluster1_weekend$강동구청dt18, alternative='greater')

var.test(cluster1_weekend$일반18, cluster1_weekend$구의dt18)
t.test(cluster1_weekend$일반18, cluster1_weekend$구의dt18, alternative='greater', var.equal=T)

var.test(cluster1_weekend$일반18, cluster1_weekend$송파마천사거리dt18)
t.test(cluster1_weekend$일반18, cluster1_weekend$송파마천사거리dt18, alternative='greater')

var.test(cluster1_weekend$일반18, cluster1_weekend$송파방이dt18)
t.test(cluster1_weekend$일반18, cluster1_weekend$송파방이dt18, alternative='greater')

var.test(cluster1_weekend$일반18, cluster1_weekend$신월icdt18)
t.test(cluster1_weekend$일반18, cluster1_weekend$신월icdt18, alternative='greater', var.equal=T)

var.test(cluster1_weekend$일반18, cluster1_weekend$연희dt18)
t.test(cluster1_weekend$일반18, cluster1_weekend$연희dt18, alternative='greater')

var.test(cluster1_weekend$일반18, cluster1_weekend$화곡dt18)
t.test(cluster1_weekend$일반18, cluster1_weekend$화곡dt18, alternative='greater')
