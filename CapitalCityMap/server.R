################################################################################################################################################
############### 世界の首都マップ -- ui.R #######################################################################################################
################################################################################################################################################

library(shiny)
library(leaflet)
library(dplyr)

# アプリをブラウザ上で立ち上げる
options(shiny.launch.browser = T)
# 緯度経度データの読み込み
load("Data.RData")


# shinyサーバー
shinyServer(function(input, output) {
  
  # 国名のプルダウンリストのUI 
  output$pulldownUI <- renderUI({
    countryList <- unique(as.character(Data$name_jps))
    
    selectInput(inputId = "theCountries", label =  "国名を選択してください", 
                choices = countryList, selected = countryList[1], multiple = F)
  })
  
  # プルダウンリストの選択によって変化するデータ
  passData <- reactive({
    firstData <- Data %>% filter(name_jps == input$theCountries)
    return(firstData)
  })
  
  # リーフレットの出力
  output$leaflet <- renderLeaflet({
    if (input$theCountries == "not set") {
      # 日本の経度
      lng_jp <- Data %>% filter(name_jps == "日本") %>% select(lon)
      # 日本の緯度
      lat_jp <- Data %>% filter(name_jps == "日本") %>% select(lat) 
      
      leaflet() %>% addTiles() %>% setView(lng = lng_jp, lat = lat_jp, zoom = 2)
    } else {
      # 選択された国の経度
      lng <- passData()$lon
      # 選択された国の緯度
      lat <- passData()$lat
      
      leaflet() %>% addTiles() %>% 
        addMarkers(lng = lng, lat = lat, 
                   popup = as.character(passData()$capital_jp)) %>% 
        setView(lng = lng, lat = lat, zoom = 2)
        
    }
  })
  
})
