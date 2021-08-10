# DriveThru
(공빅데 프로젝트) 드라이브스루 매장에 대한 교통유발계수를 보정하여 교통유발부담금을 재산정한다.

# 2021-08-09
(분석 1일차)

- 스타벅스 도로명주소, 위경도 크롤링해오기 (완료)
- 행정구역별 유동인구 평균 산출하기 (완료)
- 통행속도 데이터에서의 도로를 기준으로 스타벅스 앞 도로 추출하기! (진행중..)

# 2021-08-10
### 오늘 한 것들
- 스타벅스 매장 데이터 + 행정동 주소 + 유동인구 join
- 통해속도 데이터에 있는 시점, 종점 위치를 openAPI로 좌표값 받아오기
- 받아온 좌표값을 q-gis에 point로 찍기
- point를 vertex 순서로 이어서 각각 도로 객체 line으로 만들기 (여기까지 오늘 완료!)
### 내일부터
- 앞으로는... 만든 line과 스타벅스 point 앞에 있는 도로 line만 추출하기(상행/하행 구분)
- 따온 line과 통행속도 데이터의 차선 수 join
- clustering : 매장 + 유동인구 + 차선으로 매장들의 입점 환경을 클러스터링
