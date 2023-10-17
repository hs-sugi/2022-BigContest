options(encoding = 'UTF-8')

library(DT)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(fresh)
library(showtext)


require(showtext)

font_add_google(name="Nanum Gothic", regular.wt = 400, bold.wt=700)

showtext_auto()
showtext_opts(dpi=122)


############# 데이터 ###############################

house_data <- read.csv("final_amo.csv", header=TRUE, fileEncoding = "UTF-8")
food_data <- read.csv("FOOD_ITEM.csv", header=TRUE, fileEncoding = "UTF-8")
trans_data <- read.csv("TRANS_CO2.csv", header=TRUE, fileEncoding = "UTF-8")
tour_data <- read.csv("landmark_final.csv", header=TRUE, fileEncoding = "UTF-8")
menu_data <- read.csv("menu_df.csv", header = TRUE, fileEncoding = "UTF-8")
cafe_data <- read.csv("cafemenu_df.csv", header = TRUE, fileEncoding = "UTF-8")

################# 교통 ########################
airport = c("김포", "김해", "광주", "청주", "대구", "군산", "울산", "양양", "여수","None")
air_dist = c(449,291,182,367,330,266,349,542,180,0)
air_data = data.frame(airport, air_dist)

air_ftn <- function(arr, dep){
  air_carbon = trans_data[which(trans_data$MD_CAT == "항공"), 8]
  
  dist1 = air_data[which(air_data$airport == arr),2]
  dist2 = air_data[which(air_data$airport == dep),2]
  
  carbon1 = as.numeric(dist1) * air_carbon/189
  carbon2 = as.numeric(dist2) * air_carbon/189
  
  carbon = carbon1 + carbon2
  return(carbon)
}

port = c("목포", "완도", "고흥", "여수", "부산", "해남", "장흥","None")
ship_dist = c(145,104,129,203,294,116,109,0)
ship_data = data.frame(port, ship_dist)

ship_ftn <- function(arr, dep){
  ship_carbon = trans_data[which(trans_data$MD_CAT == "선박"), 8]
  
  dist1 = ship_data[which(ship_data$port == arr),2]
  dist2 = ship_data[which(ship_data$port == dep),2]
  
  carbon1 = as.numeric(dist1) * ship_carbon/30
  carbon2 = as.numeric(dist2) * ship_carbon/30
  
  carbon = carbon1 + carbon2
  return(carbon)
}


bus_ftn <- function(type, dist){
  bus_carbon = mean(trans_data[which(trans_data$SM_CAT == type), 8])
  
  carbon = as.numeric(dist) * bus_carbon/30
  return(carbon)
}

bikic_ftn <- function(type, dist){
  if (type == "자전거"){
    carbon = 0
  }
  else{
    carbon = as.numeric(dist)/6.9 * 0.004367
  }
  return(carbon)
}


car_ftn <- function(brand, dist, num){
  car = trans_data[which(trans_data$FUEL_TYPE != "Electric" & trans_data$LG_CAT=="자동차"), ]
  car_carbon = mean(car[which(car$MD_CAT == brand), 8])
  car_fuelcost = mean(car[which(car$MD_CAT == brand), 6])
  
  if (num>0){
    carbon = as.numeric(dist)/car_fuelcost * car_carbon/ (as.numeric(num))
  }
  else{
    carbon = 0
  }
  return(carbon)
}


elect_ftn <- function(brand, dist, num){
  elect = trans_data[which(trans_data$FUEL_TYPE == "Electric" & trans_data$LG_CAT=="자동차"), ]
  elect_carbon = mean(elect[which(elect$MD_CAT == brand), 8])
  elect_fuelcost = mean(elect[which(elect$MD_CAT == brand), 6])
  
  if (num > 0){
    carbon = as.numeric(dist)/elect_fuelcost * elect_carbon/(as.numeric(num))
  }
  else{
    carbon=0
  }
  
  return(carbon)
}

cars_brand = unique(trans_data[which(trans_data$FUEL_TYPE != "Electric" & trans_data$LG_CAT=="자동차"), 3])
elect_brand = unique(trans_data[which(trans_data$FUEL_TYPE == "Electric" & trans_data$LG_CAT=="자동차"), 3])
#############################################################

############ 식당 ##########################
menu_name = menu_data[,2]
menu_ftn <- function(name,num){
  if (is.null(name) == TRUE){
    carbon = 0
  }
  else{
    carbon = 0
    
    for (i in 1:length(name)){
      carbon2 = menu_data[which(menu_data$menu == name[i]), 3]
      carbon = carbon + carbon2
    }
    carbon = carbon/length(name)*num
  }
  
  return(carbon)
}

############ 카페 ########################
cafe_name = cafe_data[,2]
cafe_ftn <- function(name,num){
  if (is.null(name) == TRUE){
    carbon = 0
  }
  else{
    carbon = 0
    
    for (i in 1:length(name)){
      carbon2 = cafe_data[which(cafe_data$menu == name[i]), 3]
      carbon = carbon + carbon2
    }
    carbon = carbon/length(name) * num
  }
  
  return(carbon)
}



########### 숙소 ###################################
house_name = house_data[,4]
house_ftn <- function(house_name, num, room_num){
  if (house_name =="선택하세요"){
    carbon=0
  }
  else{
    house_carbon = house_data[which(house_data$TRRSRT_NM==house_name),10]
    carbon = house_carbon * as.numeric(num) * as.numeric(room_num)
  }
  return(carbon)
}

#########################################################

########### 관광 ########################
tour_name = tour_data[,2]
tour_ftn <- function(tour_name){
  if (is.null(tour_name) == TRUE){
    carbon = 0
  }
  
  else{
    carbon=0
    
    for (i in 1:length(tour_name)){
      carbon2 = tour_data[which(tour_data$landmark == tour_name[i]),7]
      carbon = carbon + carbon2
    }
  }
  return(carbon)
}

##########################################
# shop - food1(food)
dess_list = c("떡", "빵", "초콜릿", "잼", "번&비스킷", "케이크&푸딩", "과일", "기타 디저트")
dess_co2 = c(0.073,0.073,0.310,0.310,0.564,0.564,0.061,0.310)
dess_data = data.frame(dess_list, dess_co2)

dess_ftn <- function(price){
  food_infla = 1.059 * 1.044 * 1 * 1.028
  carbon = 0
  for (i in 1:8){
    cb = dess_co2[i]/1456.14 * as.numeric(price[i])/food_infla
    carbon = cb + carbon
  }

  return(carbon)
}

drink_list = c("물", "주스", "커피", "탄산음료", "유제품", "차")
drink_co2 = c(0.275,0.184,0.275,0.207,0.204,0.275)
drink_data = data.frame(drink_list, drink_co2)

drink_ftn <- function(price){
  food_infla = 1.059 * 1.044 * 1 * 1.028
  
  carbon = 0
  for(i in 1:6){
    cb = drink_co2[i]/1456.14 * as.numeric(price[i])/food_infla
    carbon = carbon + cb
  }
  return(carbon)
}

else_list = c("오일", "허브")
else_co2 = c(0.247,0.31)
else_data = data.frame(else_list, else_co2)

else_ftn <- function(price1, price2){
  food_infla = 1.059 * 1.044 * 1 * 1.028
  cb1 = else_co2[1]/1456.14 * as.numeric(price1)/food_infla
  cb2 = else_co2[2]/1456.14 * as.numeric(price2)/food_infla
  
  carbon = sum(cb1,cb2)
  return(carbon)
}


# shop - food2(alcohol)
alc_data <- food_data[which(startsWith(food_data[,1], "2.1"),),]
alc_co2_mean <- mean(alc_data[,2])

alc_type = c("소주", "맥주", "막걸리", "와인", "샴페인")
alc_co2 = c(0.208/360, 0.131/500, alc_co2_mean/750, 1.28/750, 2/750)
alc_price = c(1596, 2326, 1800, 40000, 50000)
alc_data = data.frame(alc_type, alc_co2, alc_price)

alc_ftn <- function(type, num, size){
  alc_infla = 1.004*1.003*1.006*1.003
  co2 = alc_data[which(alc_data$alc_type == type), 2]
  price = alc_data[which(alc_data$alc_type == type), 3]
  
  if (type == "막걸리"){
    carbon = co2/1456.14 *price/alc_infla*num * as.numeric(size)
  }
  else {
    carbon =  co2 * num * as.numeric(size)
  }
  return(carbon)
}



# shop - cigar
cigar_co2 = 0.014*20

cigar_ftn <- function(num){
  carbon = cigar_co2 * num
  return(carbon)
}



# shop - food3(clothes)
clothes_type = c("상의", "하의", "아우터", "원피스", "모자")
clo_price = c(39200,33900,73960,57600,28900)
clothes_co2 = c(0.311,0.311,0.311,0.311,0.174)
clothes_data = data.frame(clothes_type, clo_price, clothes_co2)

clothes_ftn <- function(num){
  clothes_infla = 1.006*1.007*1.001*1.011
  
  carbon = 0
  for (i in 1:5){
    cb = clothes_co2[i]/1456.14 * num[i] * clo_price[i]/clothes_infla
    carbon = cb + carbon
  }

  return(carbon)
}


# shop - shoes
shoes_price = c(44000,58650,69000,109800,48890)
shoes_co2 = 0.311
shoes_ftn <- function(num){
  clothes_infla = 1.006*1.007*1.001*1.011
  carbon = 0
  for (i in 1:5){
    cb = shoes_co2/1456.14 * num[i] * shoes_price[i]/clothes_infla
    carbon = cb + carbon
  }
  return(carbon)
}




# shop - food5(goods)
goods_type = c("장식품", "침구류", "식기류", "가죽 제품", "화장품", "책", "목욕 용품",  "스포츠 용품", "보석")
goods_co2 = c(0.603,1.111,1.733,0.444,0.027,0.221,0.027,0.488, 0.444)
goods_data = data.frame(goods_type, goods_co2)

goods_ftn <- function(price){
  goods_infla = 1.02*1.02*1.016*1.006
  
  carbon = 0
  for(i in 1:9){
    cb = goods_co2[i]/1456.14 * as.numeric(price[i])/goods_infla
    carbon = cb + carbon
  }
  
  return(carbon)
}


# shop - box(co2 per kg)
box1 = 1.85 * 0.178

box_ftn<-function(b1){
  carbon = box1 * b1
  return(carbon)
}

#########################################################

jeju_theme <- create_theme(
  adminlte_color(
    light_blue = "#3D8361"
  )
)


#########################################################
################## ui ###################################

header <- dashboardHeader(title = span("탄소 계산기", style = "font-size:24px; font-weight: bold"))

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("교통", tabName = "trans", icon = icon("fa-solid fa-car")),
    menuItem("숙소", tabName = "house", icon = icon("fa-solid fa-house")),
    menuItem("관광", tabName = "tour", icon = icon("fa-solid fa-umbrella-beach")),
    menuItem("F&B - 식사", tabName = "food", icon = icon("fa-solid fa-utensils")),
    menuItem("F&B - 카페", tabName = "cafe", icon = icon("fa-solid fa-mug-hot")),
    menuItem("쇼핑 - 식음료 및 기호식품", tabName = "shop", icon = icon("fa-solid fa-champagne-glasses")),
    menuItem("쇼핑 - 의류 및 상품", tabName = "shop2", icon = icon("fa-solid fa-shirt")),
    menuItem("결과", tabName = "res", icon = icon("fa-solid fa-chart-line"))),
  
  numericInput("person", h5("여행 인원"), value = 1),
  dateRangeInput("dates", h5("여행기간")))


body <- dashboardBody(
  use_theme(jeju_theme),
  
  tags$head(
    tags$style(HTML("

      .selectize-input {
        height: 30px;
        width: 200px;
        font-size: 10pt;
        padding-top: 5px;
      }

    "))
  ),
  
  tabItems(
    tabItem(tabName = "trans",
            fluidRow(
              column(width = 6,
                     box(width=NULL, height = 200, title = "항공", status = "warning",
                         selectInput("air_dep", label = "출발지(->제주)",
                                     choices = c("김포", "김해", "광주", "청주", "대구", "군산", "울산", "양양", "여수", "None"), selected = "None"),
                         
                         selectInput("air_arr", label = "도착지(제주->)",
                                     choices = c("김포", "김해", "광주", "청주", "대구", "군산", "울산", "양양", "여수", "None"), selected = "None")
                         
                     )),
              column(width = 6,
                     box(width=NULL, height = 200, title = "선박", status = "warning",
                         selectInput("ship_dep", label = "출발지(->제주)",
                                     choices = c("목포", "완도", "고흥", "여수", "부산", "해남", "장흥", "None"), selected = "None"),
                         selectInput("ship_arr", label = "도착지(제주->)",
                                     choices = c("목포", "완도", "고흥", "여수", "부산", "해남", "장흥", "None"), selected = "None"))         
              )
            ),
            
            fluidRow(
              column(width = 6,
                     box(width=NULL, height = 300, title = "자동차", status = "warning",
                         selectInput("car_brand", label = "자동차 브랜드",
                                     choices = cars_brand),
                         textInput("car_dist", label = "이동거리(km)",
                                   value = "0"),
                         numericInput("car_num", "동승인원", value=1, min=0))),
              
              column(width = 6,
                     box(width = NULL, height = 300, title = "전기차", status = "warning",
                         selectInput("electric_brand", label = "전기차 브랜드",
                                     choices = elect_brand),
                         textInput("electric_dist", label = "이동거리(km)",
                                   value = "0"),
                         numericInput("electric_num", "동승인원", value=1, min=0)))
            ),
            
            fluidRow(
              column(width = 6,
                     box(width=NULL, height = 200, title = "버스", status = "warning",
                         selectInput("bus_type", label = "버스 종류",
                                     choices = c("시내버스", "시외버스", "고속버스")),
                         textInput("bus_dist", label = "이동거리(km)",
                                   value = "0"))),
                     
               column(width = 6,
                      box(width=NULL, height = 200, title = "자전거/킥보드", status = "warning",
                          selectInput("bikic", label = "자전거/킥보드",
                                      choices = c("자전거", "킥보드")),
                          textInput("bikic_dist", label = "이동거리(km)",
                                    value = "0")))
            )),
    
    tabItem(tabName = "house",
            fluidRow(
              column(width = 6,
                     box(width = NULL, height= 300, title = "첫번째 숙박지", status = "warning",
                         selectInput("house1", "House", choices = c("선택하세요",house_name)),
                         numericInput("house_day1", "숙박일(1박 2일->1을 입력)", value=0, min=0),
                         numericInput("house_room1", "객실 수", value=0, min=0)
                     )),
              column(width = 6,
                     box(width = NULL, height= 300, title = "두번째 숙박지", status = "warning",
                         selectInput("house2", "House", choices = c("선택하세요",house_name)),
                         numericInput("house_day2", "숙박일(1박 2일->1을 입력)", value=0, min=0),
                         numericInput("house_room2", "객실 수", value=0, min=0)
                     ))
            ),
            fluidRow(
              column(width = 6,
                     box(width = NULL, height= 300, title = "세번째 숙박지", status = "warning",
                         selectInput("house3", "House", choices = c("선택하세요",house_name)),
                         numericInput("house_day3", "숙박일(1박 2일->1을 입력)", value=0, min=0),
                         numericInput("house_room3", "객실 수", value=0, min=0)
                     )),
              column(width = 6,
                     box(width = NULL, height= 300, title = "네번째 숙박지", status = "warning",
                         selectInput("house4", "House", choices = c("선택하세요",house_name)),
                         numericInput("house_day4", "숙박일(1박 2일->1을 입력)", value=0, min=0),
                         numericInput("house_room4", "객실 수", value=0, min=0)
                     ))
            )),
    
    
    
    tabItem(tabName = "tour",
            fluidRow(
              column(width = 4,
                     box(id = "tour.1", width = NULL, height = 300, title = "Day1", status = "warning",
                         helpText("방문하신 관광지를 모두 선택해주세요."),
                         selectizeInput("tour1", "관광지1", choices = tour_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                         textOutput('tour1_text'))),
              
              column(width = 4,
                     box(id = "tour.2", width = NULL, height = 300, title = "Day2", status = "warning",
                         helpText("방문하신 관광지를 모두 선택해주세요."),
                         selectizeInput("tour2", "관광지2", choices = tour_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                         textOutput('tour2_text'))),
              
              column(width = 4,
                     box(id = "tour.3",width = NULL, height = 300, title = "Day3", status = "warning",
                         helpText("방문하신 관광지를 모두 선택해주세요."),
                         selectizeInput("tour3", "관광지3", choices = tour_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                         textOutput('tour3_text')))
              
            ),
            
            fluidRow(
              column(width = 4,
                     box(id = "tour.4", width = NULL, height = 300, title = "Day4", status = "warning",
                         helpText("방문하신 관광지를 모두 선택해주세요."),
                         selectizeInput("tour4", "관광지4", choices = tour_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                         textOutput('tour4_text'))),
              
              column(width = 4,
                     box(id = "tour.5", width = NULL, height = 300, title = "Day5", status = "warning",
                         helpText("방문하신 관광지를 모두 선택해주세요."),
                         selectizeInput("tour5", "관광지5", choices = tour_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                         textOutput('tour5_text'))),
              
              column(width = 4,
                     box(id = "tour.6", width = NULL, height = 300, title = "Day6", status = "warning",
                         helpText("방문하신 관광지를 모두 선택해주세요."),
                         selectizeInput("tour6", "관광지6", choices = tour_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                         textOutput('tour6_text')))
            )),
    
    tabItem(tabName = "food",
            fluidRow(
              column(width = 4,
                     box(width = NULL, height = 400, title = "Day1", status = "warning",
                         fluidRow(
                           column(width = 6,
                                  selectizeInput("food11", "아침", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food11_text'),
                                  selectizeInput("food12", "점심", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food12_text'),
                                  selectizeInput("food13", "저녁", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food13_text')),
                           column(width = 6,
                                  numericInput("food11_num", "음식 개수", value = 0, min = 0),
                                  numericInput("food12_num", "음식 개수", value = 0, min = 0),
                                  numericInput("food13_num", "음식 개수", value = 0, min = 0))
                         )
                         
                     )),
              
              column(width = 4,
                     box(width = NULL, height = 400, title = "Day2", status = "warning",
                         fluidRow(
                           column(width = 6,
                                  selectizeInput("food21", "아침", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food21_text'),
                                  selectizeInput("food22", "점심", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food22_text'),
                                  selectizeInput("food23", "저녁", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food23_text')),
                           column(width = 6,
                                  numericInput("food21_num", "음식 개수", value = 0, min = 0),
                                  numericInput("food22_num", "음식 개수", value = 0, min = 0),
                                  numericInput("food23_num", "음식 개수", value = 0, min = 0))
                         )
                         
                         )),
              
              column(width = 4,
                     box(width = NULL, height = 400, title = "Day3", status = "warning",
                         fluidRow(
                           column(width =6,
                                  selectizeInput("food31", "아침", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food31_text'),
                                  selectizeInput("food32", "점심", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food32_text'),
                                  selectizeInput("food33", "저녁", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food33_text')),
                           column(width =6,
                                  numericInput("food31_num", "음식 개수", value = 0, min = 0),
                                  numericInput("food32_num", "음식 개수", value = 0, min = 0),
                                  numericInput("food33_num", "음식 개수", value = 0, min = 0))
                         )
                         
                         ))
              
              
            ),
            
            fluidRow(
              column(width = 4,
                     box(width = NULL, height = 400, title = "Day4", status = "warning",
                         fluidRow(
                           column(width = 6,
                                  selectizeInput("food41", "아침", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food41_text'),
                                  selectizeInput("food42", "점심", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food42_text'),
                                  selectizeInput("food43", "저녁", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food43_text')),
                           
                           column(width =6,
                                  numericInput("food41_num", "음식 개수", value = 0, min = 0),
                                  numericInput("food42_num", "음식 개수", value = 0, min = 0),
                                  numericInput("food43_num", "음식 개수", value = 0, min = 0))
                         )
                         
                         
                     )),
              
              column(width = 4,
                     box(width = NULL, height = 400, title = "Day5", status = "warning",
                         fluidRow(
                           column(width = 6,
                                  selectizeInput("food51", "아침", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food51_text'),
                                  selectizeInput("food52", "점심", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food52_text'),
                                  selectizeInput("food53", "저녁", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food53_text')),
                           
                           column(width=6,
                                  numericInput("food51_num", "음식 개수", value = 0, min = 0),
                                  numericInput("food52_num", "음식 개수", value = 0, min = 0),
                                  numericInput("food53_num", "음식 개수", value = 0, min = 0))
                         )
                         
                         )),
              
              column(width = 4,
                     box(width = NULL, height = 400, title = "Day6", status = "warning",
                         fluidRow(
                           column(width=6,
                                  selectizeInput("food61", "아침", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food61_text'),
                                  selectizeInput("food62", "점심", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food62_text'),
                                  selectizeInput("food63", "저녁", choices = menu_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('food63_text')),
                           
                           column(width = 6,
                                  numericInput("food61_num", "음식 개수", value = 0, min = 0),
                                  numericInput("food62_num", "음식 개수", value = 0, min = 0),
                                  numericInput("food63_num", "음식 개수", value = 0, min = 0))
                         )
                         
                         ))
              
              
            )
    ),
    
    tabItem(tabName = "cafe",
            fluidRow(
              column(width = 4,
                     box(width = NULL, height = 300, title = "Day1", status = "warning",
                         fluidRow(
                           column(width=6,
                                  selectizeInput("cafe11", "카페 메뉴", choices = cafe_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('cafe11_text'),
                                  selectizeInput("cafe12", "카페 메뉴", choices = cafe_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('cafe12_text')),
                           
                           column(width = 6,
                                  numericInput("cafe11_num", "음식 개수", value = 0, min = 0),
                                  numericInput("cafe12_num", "음식 개수", value = 0, min = 0))
                         )
                         
                         
                     )),
              
              column(width = 4,
                     box(width = NULL, height = 300, title = "Day2", status = "warning",
                         fluidRow(
                           column(width=6,
                                  selectizeInput("cafe21", "카페 메뉴", choices = cafe_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('cafe21_text'),
                                  selectizeInput("cafe22", "카페 메뉴", choices = cafe_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('cafe22_text')),
                           column(width = 6,
                                  numericInput("cafe21_num", "음식 개수", value = 0, min = 0),
                                  numericInput("cafe22_num", "음식 개수", value = 0, min = 0))
                         )
                         
                         )),
              
              column(width = 4,
                     box(width = NULL, height = 300, title = "Day3", status = "warning",
                         fluidRow(
                           column(width = 6,
                                  selectizeInput("cafe31", "카페 메뉴", choices = cafe_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('cafe31_text'),
                                  selectizeInput("cafe32", "카페 메뉴", choices = cafe_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('cafe32_text')),
                           
                           column(width = 6,
                                  numericInput("cafe31_num", "음식 개수", value = 0, min = 0),
                                  numericInput("cafe32_num", "음식 개수", value = 0, min = 0))
                         )
                         
                         ))
              
              
            ),
            
            fluidRow(
              column(width = 4,
                     box(width = NULL, height = 300, title = "Day4", status = "warning",
                         fluidRow(
                           column(width=6,
                                  selectizeInput("cafe41", "카페 메뉴", choices = cafe_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('cafe41_text'),
                                  selectizeInput("cafe42", "카페 메뉴", choices = cafe_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('cafe42_text')),
                           
                           column(width=6,
                                  numericInput("cafe41_num", "음식 개수", value = 0, min = 0),
                                  numericInput("cafe42_num", "음식 개수", value = 0, min = 0))
                         )
                         
                         
                     )),
              
              column(width = 4,
                     box(width = NULL, height = 300, title = "Day5", status = "warning",
                         fluidRow(
                           column(width=6,
                                  selectizeInput("cafe51", "카페 메뉴", choices = cafe_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('cafe51_text'),
                                  selectizeInput("cafe52", "카페 메뉴", choices = cafe_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('cafe52_text')),
                           
                           column(width=6,
                                  numericInput("cafe51_num", "음식 개수", value = 0, min = 0),
                                  numericInput("cafe52_num", "음식 개수", value = 0, min = 0))
                         )
                         
                         )),
              
              column(width = 4,
                     box(width = NULL, height = 300, title = "Day6", status = "warning",
                         fluidRow(
                           column(width=6,
                                  selectizeInput("cafe61", "카페 메뉴", choices = cafe_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('cafe61_text'),
                                  selectizeInput("cafe62", "카페 메뉴", choices = cafe_name, multiple = TRUE, options = list(plugins= list('remove_button'))),
                                  textOutput('cafe62_text')),
                           
                           column(width=6,
                                  numericInput("cafe61_num", "음식 개수", value = 0, min = 0),
                                  numericInput("cafe62_num", "음식 개수", value = 0, min = 0))
                         )
                         
                         ))
              
              
            )
    ),
    
    
    
   


    
    
    tabItem(tabName = "shop2",
            fluidRow(
              column(width = 4,
                   box(width = NULL, height = 500, title = "의류", status = "warning",
                       helpText("구매하신 개수를 입력하세요"),
                       numericInput("clo1", "상의", value=0, min=0),
                       numericInput("clo2", "하의", value=0, min=0),
                       numericInput("clo3", "아우터", value=0, min=0),
                       numericInput("clo4", "원피스", value=0, min=0),
                       numericInput("clo5", "모자", value=0), min=0)
                   ),
              
              column(width = 4,
                   box(width=NULL, height = 500, title = "신발", status = "warning",
                       helpText("구매하신 개수를 입력하세요"),
                       numericInput("shoes1", "슬리퍼", value=0, min=0),
                       numericInput("shoes2", "구두", value=0, min=0),
                       numericInput("shoes3", "스니커즈", value=0, min=0),
                       numericInput("shoes4", "로퍼", value=0, min=0),
                       numericInput("shoes5", "샌들", value=0, min=0))),
            
              column(width = 4,
                     box(width=NULL, height = 800, title = "상품", status = "warning",
                         helpText("구매하신 가격을 입력하세요"),
                         textInput("goods1", "장식품", value=0),
                         textInput("goods2", "침구류", value=0),
                         textInput("goods3", "식기류", value=0),
                         textInput("goods4", "가죽 제품", value=0),
                         textInput("goods5", "화장품", value=0),
                         textInput("goods6", "책", value=0),
                         textInput("goods7", "목욕 용품", value=0),
                         textInput("goods8", "스포츠 용품", value=0),
                         textInput("goods9", "보석", value=0))))

  ),
  
  tabItem(tabName = "shop",
          fluidRow(
            column(width = 3,
                   box(width = NULL, height=650, title = "디저트(구매하신 금액)", status = "warning",
                       textInput("dess1", "떡", value=0),
                       textInput("dess2", "빵", value=0),
                       textInput("dess3", "초콜릿", value=0),
                       textInput("dess4", "잼", value=0),
                       textInput("dess5", "번&비스킷", value=0),
                       textInput("dess6", "케이크&푸딩", value=0),
                       textInput("dess7", "과일", value=0),
                       textInput("dess8", "기타 디저트", value=0))),
            
            column(width = 3,
                   box(width = NULL, height = 500, title = "음료(구매하신 금액)", status = "warning",
                       textInput("drink1", "물", value=0),
                       textInput("drink2", "주스", value=0),
                       textInput("drink3", "커피", value=0),
                       textInput("drink4", "탄산음료", value=0),
                       textInput("drink5", "유제품", value=0),
                       textInput("drink6", "차", value=0)),
                   
                   box(width = NULL, height = 200, title = "기타", status = "warning",
                       textInput("else1", "오일", value=0),
                       textInput("else2", "허브", value=0))
                   
                   ),
            
            column(width = 3,
                   box(width = NULL, height= 300, title = "주류1", status = "warning",
                       selectInput("alc1", "주종1", choices = alc_type),
                       numericInput("alc1_num", "개수", value=0, min=0),
                       textInput("alc1_size", "용량(ml)", value=0)
                   ),
                   box(width = NULL, height= 300, title = "주류2", status = "warning",
                       selectInput("alc2", "주종2", choices = alc_type),
                       numericInput("alc2_num", "개수", value=0, min=0),
                       textInput("alc2_size", "용량(ml)", value=0)
                   )),
            column(width = 3,
                   box(width = NULL, height= 300, title = "주류3", status = "warning",
                       selectInput("alc3", "주종3", choices = alc_type),
                       numericInput("alc3_num", "개수", value=0, min=0),
                       textInput("alc3_size", "용량(ml)", value=0)),
                   
                   box(width = NULL, height= 150, title = "담배", status = "warning",
                       numericInput("cigar", "담배(한 갑 기준)", value=0, min=0)
                   ),
                   box(width = NULL, height = 150, title = "상자 개수", status = "warning",
                       numericInput("box1", "개수", value=0, min=0)))
          )),
  
  tabItem(tabName = "res",
          fluidRow(
            valueBoxOutput("res_trans"),
            valueBoxOutput("res_house"),
            valueBoxOutput("res_tour")),
          
          fluidRow(
            valueBoxOutput("res_food"),
            valueBoxOutput("res_shop"),
            valueBoxOutput("res_shop2")),
          
          fluidRow(
            column(width = 8,
                   box(width =NULL, height = 500, title = "분야별 탄소(%)", status = "warning",
                       plotOutput('res_plot')
                       )),
            valueBoxOutput("res7")
            
            )
            
          ))
  )



ui <- dashboardPage(header, sidebar, body)


server <- function(input, output) {
  
  output$res_trans <- renderInfoBox({
    infoBox("교통의 CO2 배출량",
      value = round((air_ftn(input$air_dep, input$air_arr)+
                 ship_ftn(input$ship_dep, input$ship_arr)+
                 bus_ftn(input$bus_type, input$bus_dist)+
                 car_ftn(input$car_brand, input$car_dist, input$car_num)+
                   elect_ftn(input$electric_brand, input$electric_dist, input$electric_num) + bikic_ftn(input$bikic, input$bikic_dist)) * input$person, 4),
      icon = icon("fa-solid fa-car"),
      fill = TRUE,
      color = "purple"
    )
  })
  
  output$res_house <- renderInfoBox({
    infoBox("숙소의 CO2 배출량",
      value = round((house_ftn(input$house1, input$house_day1, input$house_room1) +
                       house_ftn(input$house2, input$house_day2, input$house_room2) +
                       house_ftn(input$house3, input$house_day3, input$house_room3) +
                       house_ftn(input$house4, input$house_day4, input$house_room4)), 4), icon = icon("fa-solid fa-house"), fill = TRUE, color = "yellow"
    )
  })
  
  
  
  output$res_tour <- renderInfoBox({
    infoBox("관광의 CO2 배출량",
      value = round((tour_ftn(input$tour1) + tour_ftn(input$tour2)+tour_ftn(input$tour3)+tour_ftn(input$tour4)+tour_ftn(input$tour5)+tour_ftn(input$tour6)),4),
      icon = icon("fa-solid fa-umbrella-beach"), fill = TRUE, color = "orange"
      
    )
  })
  
  
  
  output$res_food <- renderInfoBox({
    infoBox("F&B의 CO2 배출량",
            value = round(((menu_ftn(input$food11, input$food11_num) + menu_ftn(input$food12, input$food12_num) + menu_ftn(input$food13, input$food13_num) +
                             menu_ftn(input$food21, input$food21_num) + menu_ftn(input$food22, input$food22_num) + menu_ftn(input$food23, input$food23_num) +
                             menu_ftn(input$food31, input$food31_num) + menu_ftn(input$food32, input$food32_num) + menu_ftn(input$food33, input$food33_num) +
                             menu_ftn(input$food41, input$food41_num) + menu_ftn(input$food42, input$food42_num) + menu_ftn(input$food43, input$food43_num) +
                             menu_ftn(input$food51, input$food51_num) + menu_ftn(input$food52, input$food52_num) + menu_ftn(input$food53, input$food53_num) + 
                             menu_ftn(input$food61, input$food61_num) + menu_ftn(input$food62, input$food62_num) + menu_ftn(input$food63, input$food63_num)) +
                            
                            (cafe_ftn(input$cafe11, input$cafe11_num) + cafe_ftn(input$cafe12, input$cafe12_num) +
                               cafe_ftn(input$cafe21, input$cafe21_num) + cafe_ftn(input$cafe22, input$cafe22_num) +
                               cafe_ftn(input$cafe31, input$cafe31_num) + cafe_ftn(input$cafe32, input$cafe32_num)+
                               cafe_ftn(input$cafe41, input$cafe41_num) + cafe_ftn(input$cafe42, input$cafe42_num)+
                               cafe_ftn(input$cafe51, input$cafe51_num) + cafe_ftn(input$cafe52, input$cafe52_num)+
                               cafe_ftn(input$cafe61, input$cafe61_num) + cafe_ftn(input$cafe62, input$cafe62_num))) * input$person ,4),
            icon = icon("fa-solid fa-utensils"),
            fill = TRUE, color = "olive")
  })
  
  

  
  
  
  output$res_shop <- renderInfoBox({
    infoBox("식음료 및 기호식품의 CO2(쇼핑)",
      value = round((dess_ftn(c(input$dess1,input$dess2,input$dess3,input$dess4,input$dess5,input$dess6,input$dess7,input$dess8)) +
                      drink_ftn(c(input$drink1,input$drink2,input$drink3,input$drink4,input$drink5,input$drink6)) +
                       else_ftn(input$else1, input$else2)+
                       cigar_ftn(input$cigar) +
                       box_ftn(input$box1) +
                       alc_ftn(input$alc1, input$alc1_num, input$alc1_size)+
                       alc_ftn(input$alc2, input$alc2_num, input$alc2_size)+
                     alc_ftn(input$alc3, input$alc3_num, input$alc3_size)), 4),
      icon = icon("fa-solid fa-champagne-glasses"), fill = TRUE, color = "fuchsia"
    )
  })
  
  output$res_shop2 <- renderInfoBox({
    infoBox("의류 및 상품의 CO2(쇼핑)",
      value = round((goods_ftn(c(input$goods1, input$goods2, input$goods3, input$goods4, input$goods5, input$goods6, input$goods7, input$goods8, input$goods9)) + 
                      clothes_ftn(c(input$clo1,input$clo2,input$clo3,input$clo4,input$clo5)) +
                      shoes_ftn(c(input$shoes1, input$shoes2, input$shoes3, input$shoes4, input$shoes5))),4),
      icon = icon("fa-solid fa-shirt"), fill = TRUE, color = "green"
      
    )
  })
  
  
  output$res7 <- renderInfoBox({
    infoBox("탄소 배출 총량",
            value = round((air_ftn(input$air_dep, input$air_arr)+
                             ship_ftn(input$ship_dep, input$ship_arr)+
                             bus_ftn(input$bus_type, input$bus_dist)+
                             car_ftn(input$car_brand, input$car_dist, input$car_num)+
                             elect_ftn(input$electric_brand, input$electric_dist, input$electric_num) + bikic_ftn(input$bikic, input$bikic_dist)) * input$person, 4)+round((house_ftn(input$house1, input$house_day1, input$house_room1) +
                                                                                                                  house_ftn(input$house2, input$house_day2, input$house_room2) +
                                                                                                                  house_ftn(input$house3, input$house_day3, input$house_room3) +
                                                                                                                  house_ftn(input$house4, input$house_day4, input$house_room4)), 4)+
              round((tour_ftn(input$tour1) + tour_ftn(input$tour2)+tour_ftn(input$tour3)+tour_ftn(input$tour4)+tour_ftn(input$tour5)+tour_ftn(input$tour6)),4)+
              round((dess_ftn(c(input$dess1,input$dess2,input$dess3,input$dess4,input$dess5,input$dess6,input$dess7,input$dess8)) +
                       drink_ftn(c(input$drink1,input$drink2,input$drink3,input$drink4,input$drink5,input$drink6)) +
                       else_ftn(input$else1, input$else2)+
                       cigar_ftn(input$cigar) +
                       box_ftn(input$box1) +
                       alc_ftn(input$alc1, input$alc1_num, input$alc1_size)+
                       alc_ftn(input$alc2, input$alc2_num, input$alc2_size)+
                       alc_ftn(input$alc3, input$alc3_num, input$alc3_size)), 4)+
              round((goods_ftn(c(input$goods1, input$goods2, input$goods3, input$goods4, input$goods5, input$goods6, input$goods7, input$goods8, input$goods9)) + 
                       clothes_ftn(c(input$clo1,input$clo2,input$clo3,input$clo4,input$clo5)) +
                       shoes_ftn(c(input$shoes1, input$shoes2, input$shoes3, input$shoes4, input$shoes5))),4) +round(((menu_ftn(input$food11, input$food11_num) + menu_ftn(input$food12, input$food12_num) + menu_ftn(input$food13, input$food13_num) +
                                                                                                                         menu_ftn(input$food21, input$food21_num) + menu_ftn(input$food22, input$food22_num) + menu_ftn(input$food23, input$food23_num) +
                                                                                                                         menu_ftn(input$food31, input$food31_num) + menu_ftn(input$food32, input$food32_num) + menu_ftn(input$food33, input$food33_num) +
                                                                                                                         menu_ftn(input$food41, input$food41_num) + menu_ftn(input$food42, input$food42_num) + menu_ftn(input$food43, input$food43_num) +
                                                                                                                         menu_ftn(input$food51, input$food51_num) + menu_ftn(input$food52, input$food52_num) + menu_ftn(input$food53, input$food53_num) + 
                                                                                                                         menu_ftn(input$food61, input$food61_num) + menu_ftn(input$food62, input$food62_num) + menu_ftn(input$food63, input$food63_num)) +
                                                                                                                        
                                                                                                                        (cafe_ftn(input$cafe11, input$cafe11_num) + cafe_ftn(input$cafe12, input$cafe12_num) +
                                                                                                                           cafe_ftn(input$cafe21, input$cafe21_num) + cafe_ftn(input$cafe22, input$cafe22_num) +
                                                                                                                           cafe_ftn(input$cafe31, input$cafe31_num) + cafe_ftn(input$cafe32, input$cafe32_num)+
                                                                                                                           cafe_ftn(input$cafe41, input$cafe41_num) + cafe_ftn(input$cafe42, input$cafe42_num)+
                                                                                                                           cafe_ftn(input$cafe51, input$cafe51_num) + cafe_ftn(input$cafe52, input$cafe52_num)+
                                                                                                                           cafe_ftn(input$cafe61, input$cafe61_num) + cafe_ftn(input$cafe62, input$cafe62_num))) * input$person ,4), icon = icon("fa-solid fa-chart-line"), fill=TRUE, color = "red")
  })
  
  
  trans <- reactive(round((air_ftn(input$air_dep, input$air_arr)+
                             ship_ftn(input$ship_dep, input$ship_arr)+
                             bus_ftn(input$bus_type, input$bus_dist)+
                             car_ftn(input$car_brand, input$car_dist, input$car_num)+
                             elect_ftn(input$electric_brand, input$electric_dist, input$electric_num) + bikic_ftn(input$bikic, input$bikic_dist)) * input$person, 4))
  house <- reactive(round((house_ftn(input$house1, input$house_day1, input$house_room1) +
                             house_ftn(input$house2, input$house_day2, input$house_room2) +
                             house_ftn(input$house3, input$house_day3, input$house_room3) +
                             house_ftn(input$house4, input$house_day4, input$house_room4)), 4))
  
  tour <- reactive(round((tour_ftn(input$tour1) + tour_ftn(input$tour2)+tour_ftn(input$tour3)+tour_ftn(input$tour4)+tour_ftn(input$tour5)+tour_ftn(input$tour6)),4))
  
  
  food <-reactive(round(((menu_ftn(input$food11, input$food11_num) + menu_ftn(input$food12, input$food12_num) + menu_ftn(input$food13, input$food13_num) +
                            menu_ftn(input$food21, input$food21_num) + menu_ftn(input$food22, input$food22_num) + menu_ftn(input$food23, input$food23_num) +
                            menu_ftn(input$food31, input$food31_num) + menu_ftn(input$food32, input$food32_num) + menu_ftn(input$food33, input$food33_num) +
                            menu_ftn(input$food41, input$food41_num) + menu_ftn(input$food42, input$food42_num) + menu_ftn(input$food43, input$food43_num) +
                            menu_ftn(input$food51, input$food51_num) + menu_ftn(input$food52, input$food52_num) + menu_ftn(input$food53, input$food53_num) + 
                            menu_ftn(input$food61, input$food61_num) + menu_ftn(input$food62, input$food62_num) + menu_ftn(input$food63, input$food63_num)) +
                           
                           (cafe_ftn(input$cafe11, input$cafe11_num) + cafe_ftn(input$cafe12, input$cafe12_num) +
                              cafe_ftn(input$cafe21, input$cafe21_num) + cafe_ftn(input$cafe22, input$cafe22_num) +
                              cafe_ftn(input$cafe31, input$cafe31_num) + cafe_ftn(input$cafe32, input$cafe32_num)+
                              cafe_ftn(input$cafe41, input$cafe41_num) + cafe_ftn(input$cafe42, input$cafe42_num)+
                              cafe_ftn(input$cafe51, input$cafe51_num) + cafe_ftn(input$cafe52, input$cafe52_num)+
                              cafe_ftn(input$cafe61, input$cafe61_num) + cafe_ftn(input$cafe62, input$cafe62_num))) * input$person ,4))

  
  
  shop1 <- reactive(round((dess_ftn(c(input$dess1,input$dess2,input$dess3,input$dess4,input$dess5,input$dess6,input$dess7,input$dess8)) +
                             drink_ftn(c(input$drink1,input$drink2,input$drink3,input$drink4,input$drink5,input$drink6)) +
                             else_ftn(input$else1, input$else2)+
                             cigar_ftn(input$cigar) +
                             box_ftn(input$box1) +
                             alc_ftn(input$alc1, input$alc1_num, input$alc1_size)+
                             alc_ftn(input$alc2, input$alc2_num, input$alc2_size)+
                             alc_ftn(input$alc3, input$alc3_num, input$alc3_size)), 4))
  
  shop2 <- reactive(round((goods_ftn(c(input$goods1, input$goods2, input$goods3, input$goods4, input$goods5, input$goods6, input$goods7, input$goods8, input$goods9)) + 
                             clothes_ftn(c(input$clo1,input$clo2,input$clo3,input$clo4,input$clo5)) +
                             shoes_ftn(c(input$shoes1, input$shoes2, input$shoes3, input$shoes4, input$shoes5))),4))
  
  
  res_sum = reactive(round((air_ftn(input$air_dep, input$air_arr)+
                              ship_ftn(input$ship_dep, input$ship_arr)+
                              bus_ftn(input$bus_type, input$bus_dist)+
                              car_ftn(input$car_brand, input$car_dist, input$car_num)+elect_ftn(input$electric_brand, input$electric_dist, input$electric_num) + bikic_ftn(input$bikic, input$bikic_dist)) * input$person, 4)+round((house_ftn(input$house1, input$house_day1, input$house_room1) +
                                                                                                                   house_ftn(input$house2, input$house_day2, input$house_room2) +
                                                                                                                   house_ftn(input$house3, input$house_day3, input$house_room3) +
                                                                                                                   house_ftn(input$house4, input$house_day4, input$house_room4)), 4)+
                       round((tour_ftn(input$tour1) + tour_ftn(input$tour2)+tour_ftn(input$tour3)+tour_ftn(input$tour4)+tour_ftn(input$tour5)+tour_ftn(input$tour6)),4)+
                       round((dess_ftn(c(input$dess1,input$dess2,input$dess3,input$dess4,input$dess5,input$dess6,input$dess7,input$dess8)) +
                                drink_ftn(c(input$drink1,input$drink2,input$drink3,input$drink4,input$drink5,input$drink6)) +
                                else_ftn(input$else1, input$else2)+
                                cigar_ftn(input$cigar) +
                                box_ftn(input$box1) +
                                alc_ftn(input$alc1, input$alc1_num, input$alc1_size)+
                                alc_ftn(input$alc2, input$alc2_num, input$alc2_size)+
                                alc_ftn(input$alc3, input$alc3_num, input$alc3_size)), 4)+
                       round((goods_ftn(c(input$goods1, input$goods2, input$goods3, input$goods4, input$goods5, input$goods6, input$goods7, input$goods8, input$goods9)) + 
                                clothes_ftn(c(input$clo1,input$clo2,input$clo3,input$clo4,input$clo5)) +
                                shoes_ftn(c(input$shoes1, input$shoes2, input$shoes3, input$shoes4, input$shoes5))),4))
  
  
  
  output$res_plot <- renderPlot({
    b = ggplot(data.frame('type' = c("교통", "숙소", "F&B", "관광", "쇼핑1", "쇼핑2"),
                          'ratio'=c(trans(), house(), food(), tour(), shop1(), shop2())),
               aes(x='', y=ratio, fill=type)) + geom_bar(stat = 'identity')
    b + coord_polar('y', start=0) + scale_fill_brewer(palette="Dark2") + theme_minimal()
  })
  

  
  
  output$tour1_text <- renderText({
    paste(input$tour1)
  })
  
  output$tour2_text <- renderText({
    paste(input$tour2)
  })
  
  output$tour3_text <- renderText({
    paste(input$tour3)
  })
  
  output$tour4_text <- renderText({
    paste(input$tour4)
  })
  
  output$tour5_text <- renderText({
    paste(input$tour5)
  })
  
  output$tour6_text <- renderText({
    paste(input$tour6)
  })
  
  
  output$food11_text <- renderText({
    paste(input$food11)
  })
  output$food12_text <- renderText({
    paste(input$food12)
  })
  output$food13_text <- renderText({
    paste(input$food13)
  })
  output$food21_text <- renderText({
    paste(input$food21)
  })
  output$food22_text <- renderText({
    paste(input$food22)
  })
  output$food23_text <- renderText({
    paste(input$food23)
  })
  output$food31_text <- renderText({
    paste(input$food31)
  })
  output$food32_text <- renderText({
    paste(input$food32)
  })
  output$food33_text <- renderText({
    paste(input$food33)
  })
  output$food41_text <- renderText({
    paste(input$food41)
  })
  output$food42_text <- renderText({
    paste(input$food42)
  })
  output$food43_text <- renderText({
    paste(input$food43)
  })
  output$food51_text <- renderText({
    paste(input$food51)
  })
  output$food52_text <- renderText({
    paste(input$food52)
  })
  output$food53_text <- renderText({
    paste(input$food53)
  })
  output$food61_text <- renderText({
    paste(input$food61)
  })
  output$food62_text <- renderText({
    paste(input$food62)
  })
  output$food63_text <- renderText({
    paste(input$food63)
  })
  
  
  
  output$cafe11_text <- renderText({
    paste(input$cafe11)
  })
  output$cafe12_text <- renderText({
    paste(input$cafe12)
  })
  output$cafe21_text <- renderText({
    paste(input$cafe21)
  })
  output$cafe22_text <- renderText({
    paste(input$cafe22)
  })
  output$cafe31_text <- renderText({
    paste(input$cafe31)
  })
  output$cafe32_text <- renderText({
    paste(input$cafe32)
  })
  output$cafe41_text <- renderText({
    paste(input$cafe41)
  })
  output$cafe42_text <- renderText({
    paste(input$cafe42)
  })
  output$cafe51_text <- renderText({
    paste(input$cafe51)
  })
  output$cafe52_text <- renderText({
    paste(input$cafe52)
  })
  output$cafe61_text <- renderText({
    paste(input$cafe61)
  })
  output$cafe62_text <- renderText({
    paste(input$cafe62)
  })
  
}


shinyApp(ui = ui, server = server)
