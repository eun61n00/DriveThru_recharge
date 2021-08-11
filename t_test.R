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

t_test_test <- read.csv("G:/내 드라이브/2021 공공빅데이터 청년인턴/프로젝트/데이터/testtest.csv")
t.test(t_test_test$일반, t_test_test$공릉DT, alternative='greater')
t.test(t_test_test$일반, t_test_test$연희DT, alternative='greater')
t.test(t_test_test$일반, t_test_test$강동구청DT, alternative='greater')
