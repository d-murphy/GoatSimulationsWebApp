
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)

GOATData <- read.csv("GOATData.csv",stringsAsFactors = TRUE)
GOATData <- arrange(GOATData, Year, Team)


GOATdataTable <- read.csv("GOATDataForTable.csv",stringsAsFactors = FALSE)

GOATdataTable$ThreeManLadderFirstPlace <- ifelse(GOATdataTable$ThreeManLadderFirstPlace==0,NA,GOATdataTable$ThreeManLadderFirstPlace)
GOATdataTable$BLadderFirstPlace <- ifelse(GOATdataTable$BLadderFirstPlace==0,NA,GOATdataTable$BLadderFirstPlace)
GOATdataTable$CLadderFirstPlace <- ifelse(GOATdataTable$CLadderFirstPlace==0,NA,GOATdataTable$CLadderFirstPlace)
GOATdataTable$CHoseFirstPlace <- ifelse(GOATdataTable$CHoseFirstPlace==0,NA,GOATdataTable$CHoseFirstPlace)
GOATdataTable$BHoseFirstPlace <- ifelse(GOATdataTable$BHoseFirstPlace==0,NA,GOATdataTable$BHoseFirstPlace)
GOATdataTable$EfficiencyFirstPlace <- ifelse(GOATdataTable$EfficiencyFirstPlace==0,NA,GOATdataTable$EfficiencyFirstPlace)
GOATdataTable$MotorPumpFirstPlace <- ifelse(GOATdataTable$MotorPumpFirstPlace==0,NA,GOATdataTable$MotorPumpFirstPlace)
GOATdataTable$BucketsFirstPlace <- ifelse(GOATdataTable$BucketsFirstPlace==0,NA,GOATdataTable$BucketsFirstPlace)
GOATdataTable$ThreeManLadderTop5 <- ifelse(GOATdataTable$ThreeManLadderTop5==0,NA,GOATdataTable$ThreeManLadderTop5)
GOATdataTable$BLadderTop5 <- ifelse(GOATdataTable$BLadderTop5==0,NA,GOATdataTable$BLadderTop5)
GOATdataTable$CLadderTop5 <- ifelse(GOATdataTable$CLadderTop5==0,NA,GOATdataTable$CLadderTop5)
GOATdataTable$CHoseTop5 <- ifelse(GOATdataTable$CHoseTop5==0,NA,GOATdataTable$CHoseTop5)
GOATdataTable$BHoseTop5 <- ifelse(GOATdataTable$BHoseTop5==0,NA,GOATdataTable$BHoseTop5)
GOATdataTable$EfficiencyTop5 <- ifelse(GOATdataTable$EfficiencyTop5==0,NA,GOATdataTable$EfficiencyTop5)
GOATdataTable$MotorPumpTop5 <- ifelse(GOATdataTable$MotorPumpTop5==0,NA,GOATdataTable$MotorPumpTop5)
GOATdataTable$BucketsTop5 <- ifelse(GOATdataTable$BucketsTop5==0,NA,GOATdataTable$BucketsTop5)





shinyServer(function(input, output) {
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- GOATdataTable
    
    data <- data %>% select(-AveragePoints,-OverallResult,-ThreeManLadderAverage,-BLadderAverage,
                            -CLadderAverage,-CHoseAverage,-BHoseAverage,-EfficiencyAverage,
                            -MotorPumpAverage,-BucketsAverage)  %>%
      filter(ThreeManLadderTop5>0 | BLadderTop5>0 | CLadderTop5 > 0 | CHoseTop5 > 0 | 
               BHoseTop5 > 0 | EfficiencyTop5 > 0 | MotorPumpTop5 > 0 | BucketsTop5 > 0)
    
    #Filters on Year
    if(!(input$year == 0)){
      data <- dplyr::filter(data, Year==input$year)
    }
    ##Removes ThreeMan
    if(!(1 %in% input$checkGroup) | !(1 %in% input$checkGroup2)){
      data <- dplyr::select(data,-ThreeManLadderFirstPlace) 
      #        data <- dplyr::select(data,-`Three Man First Place %`)
    }
    if(!(1 %in% input$checkGroup) | !(2 %in% input$checkGroup2)){
      data <- dplyr::select(data,-ThreeManLadderTop5) 
      #        data <- dplyr::select(data,-`Three Man Top 5 %`)
    }
    #BLadder
    if(!(2 %in% input$checkGroup) | !(1 %in% input$checkGroup2)){
      data <- dplyr::select(data,-BLadderFirstPlace) 
      #        data <- dplyr::select(data,`B Ladder First Place %`) 
    }
    if(!(2 %in% input$checkGroup) | !(2 %in% input$checkGroup2)){
      data <- dplyr::select(data,-BLadderTop5) 
      #        data <- dplyr::select(data,-`B Ladder Top 5 %`) 
    }
    #CLadder
    if(!(3 %in% input$checkGroup) | !(1 %in% input$checkGroup2)){
      data <- dplyr::select(data,-CLadderFirstPlace) 
      #        data <- dplyr::select(data,-`C Ladder First Place %`) 
    }
    if(!(3 %in% input$checkGroup) | !(2 %in% input$checkGroup2)){
      data <- dplyr::select(data,-CLadderTop5) 
      #        data <- dplyr::select(data,-`C Ladder Top 5 %`) 
    }
    #CHose
    if(!(4 %in% input$checkGroup) | !(1 %in% input$checkGroup2)){
      data <- dplyr::select(data,-CHoseFirstPlace) 
      #        data <- dplyr::select(data,-`C Hose First Place %`) 
    }
    if(!(4 %in% input$checkGroup) | !(2 %in% input$checkGroup2)){
      data <- dplyr::select(data,-CHoseTop5) 
      #        data <- dplyr::select(data,-`C Hose Top 5 %`) 
    }
    #BHose
    if(!(5 %in% input$checkGroup) | !(1 %in% input$checkGroup2)){
      data <- dplyr::select(data,-BHoseFirstPlace) 
      #        data <- dplyr::select(data,-`B Hose First Place %`) 
    }
    if(!(5 %in% input$checkGroup) | !(2 %in% input$checkGroup2)){
      data <- dplyr::select(data,-BHoseTop5) 
      #        data <- dplyr::select(data,-`B Hose Top 5 %`) 
    }
    #Efficiency
    if(!(6 %in% input$checkGroup) | !(1 %in% input$checkGroup2)){
      data <- dplyr::select(data,-EfficiencyFirstPlace) 
      #        data <- dplyr::select(data,-`Efficiency First Place %`) 
    }
    if(!(6 %in% input$checkGroup) | !(2 %in% input$checkGroup2)){
      data <- dplyr::select(data,-EfficiencyTop5) 
      #        data <- dplyr::select(data,-`Efficiency Top 5 %`) 
    }
    #MotorPump
    if(!(7 %in% input$checkGroup) | !(1 %in% input$checkGroup2)){
      data <- dplyr::select(data,-MotorPumpFirstPlace) 
      #        data <- dplyr::select(data,-`Motor Pump First Place %`) 
    }
    if(!(7 %in% input$checkGroup) | !(2 %in% input$checkGroup2)){
      data <- dplyr::select(data,-MotorPumpTop5) 
      #        data <- dplyr::select(data,-`Motor Pump Top 5 %`) 
    }
    #Buckets
    if(!(8 %in% input$checkGroup) | !(1 %in% input$checkGroup2)){
      data <- dplyr::select(data,-BucketsFirstPlace) 
      #        data <- dplyr::select(data,-`Buckets First Place %`) 
    }
    if(!(8 %in% input$checkGroup) | !(2 %in% input$checkGroup2)){
      data <- dplyr::select(data,-BucketsTop5) 
      #        data <- dplyr::select(data,-`Buckets Top 5 %`) 
    }
    #Overall
    if(!(9 %in% input$checkGroup) | !(1 %in% input$checkGroup2)){
      data <- dplyr::select(data,-OverallFirstPlace) 
      #        data <- dplyr::select(data,-`Overall First Place %`) 
    }
    if(!(9 %in% input$checkGroup) | !(2 %in% input$checkGroup2)){
      data <- dplyr::select(data,-OverallTop5) 
      #        data <- dplyr::select(data,-`Overall Top 5 %`) 
    }
    data
    
  },rownames=FALSE))
  
  
  
  output$Plot <- renderPlot({
    
    #Overall First Years
    if(input$contest=="Overall" & input$Top5v1st=="FirstPlace" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="Overall" & FirstPct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) + 
              labs(title ="Number of Teams in Year with Chance for First > 1%", x = "Years", y = "Count")
      )
    }
    #Overall First Teams
    if(input$contest=="Overall" & input$Top5v1st=="FirstPlace" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="Overall" & FirstPct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) + 
              labs(title ="Number of Teams from Town with Chance for First > 1%", x = "Team", y = "Count")
      )
    }
    
    #Overall Top5 Years
    if(input$contest=="Overall" & input$Top5v1st=="Top5" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="Overall" & Top5Pct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) +  
              labs(title ="Number of Teams in Year with Chance for Top 5 > 1%", x = "Years", y = "Count")
      )
    }
    #Overall Top5 Teams
    if(input$contest=="Overall" & input$Top5v1st=="Top5" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="Overall" & Top5Pct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) +  
              labs(title ="Number of Teams from Town with Chance for Top 5 > 1%", x = "Team", y = "Count")
      )
    }
    #Three Man First Years
    if(input$contest=="Three Man Ladder" & input$Top5v1st=="FirstPlace" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="Three Man Ladder" & FirstPct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) +  
              labs(title ="Number of Teams in Year with Chance for First > 1%", x = "Years", y = "Count")
      )
    }
    #Three Man First Teams
    if(input$contest=="Three Man Ladder" & input$Top5v1st=="FirstPlace" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="Three Man Ladder" & FirstPct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) +  
              labs(title ="Number of Teams from Town with Chance for First > 1%", x = "Team", y = "Count")
      )
    }
    
    #Three Man Top5 Years
    if(input$contest=="Three Man Ladder" & input$Top5v1st=="Top5" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="Three Man Ladder" & Top5Pct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) +  
              labs(title ="Number of Teams in Year with Chance for Top 5 > 1%", x = "Years", y = "Count")
      )
    }
    #Three Man Top5 Teams
    if(input$contest=="Three Man Ladder" & input$Top5v1st=="Top5" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="Three Man Ladder" & Top5Pct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) +  
              labs(title ="Number of Teams from Town with Chance for Top 5 > 1%", x = "Team", y = "Count")
      )
    }

    #B Ladder First Years
    if(input$contest=="B Ladder" & input$Top5v1st=="FirstPlace" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="B Ladder" & FirstPct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) +  
              labs(title ="Number of Teams in Year with Chance for First > 1%", x = "Years", y = "Count")
      )
    }
    #B Ladder First Teams
    if(input$contest=="B Ladder" & input$Top5v1st=="FirstPlace" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="B Ladder" & FirstPct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) +  
              labs(title ="Number of Teams from Town with Chance for First > 1%", x = "Team", y = "Count")
      )
    }
    
    #B Ladder Top5 Years
    if(input$contest=="B Ladder" & input$Top5v1st=="Top5" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="B Ladder" & Top5Pct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) +  
              labs(title ="Number of Teams in Year with Chance for Top 5 > 1%", x = "Years", y = "Count")
      )
    }
    #B Ladder Top5 Teams
    if(input$contest=="B Ladder" & input$Top5v1st=="Top5" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="B Ladder" & Top5Pct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) +  
              labs(title ="Number of Teams from Town with Chance for Top 5 > 1%", x = "Team", y = "Count")
      )
    }
    
    #C Ladder First Years
    if(input$contest=="C Ladder" & input$Top5v1st=="FirstPlace" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="C Ladder" & FirstPct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) +  
              labs(title ="Number of Teams in Year with Chance for First > 1%", x = "Years", y = "Count")
      )
    }
    #C Ladder First Teams
    if(input$contest=="C Ladder" & input$Top5v1st=="FirstPlace" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="C Ladder" & FirstPct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) +  
              labs(title ="Number of Teams from Town with Chance for First > 1%", x = "Team", y = "Count")
      )
    }
    
    #C Ladder Top5 Years
    if(input$contest=="C Ladder" & input$Top5v1st=="Top5" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="C Ladder" & Top5Pct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) +  
              labs(title ="Number of Teams in Year with Chance for Top 5 > 1%", x = "Years", y = "Count")
      )
    }
    #C Ladder Top5 Teams
    if(input$contest=="C Ladder" & input$Top5v1st=="Top5" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="C Ladder" & Top5Pct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) +  
              labs(title ="Number of Teams from Town with Chance for Top 5 > 1%", x = "Team", y = "Count")
      )
    }
    
    #C Hose First Years
    if(input$contest=="C Hose" & input$Top5v1st=="FirstPlace" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="C Hose" & FirstPct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) +  
              labs(title ="Number of Teams in Year with Chance for First > 1%", x = "Years", y = "Count")
      )
    }
    #C Hose First Teams
    if(input$contest=="C Hose" & input$Top5v1st=="FirstPlace" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="C Hose" & FirstPct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) +  
              labs(title ="Number of Teams from Town with Chance for First > 1%", x = "Team", y = "Count")
      )
    }
    
    #C Hose Top5 Years
    if(input$contest=="C Hose" & input$Top5v1st=="Top5" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="C Hose" & Top5Pct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) +  
              labs(title ="Number of Teams in Year with Chance for Top 5 > 1%", x = "Years", y = "Count")
      )
    }
    #C Hose Top5 Teams
    if(input$contest=="C Hose" & input$Top5v1st=="Top5" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="C Hose" & Top5Pct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) +  
              labs(title ="Number of Teams from Town with Chance for Top 5 > 1%", x = "Team", y = "Count")
      )
    }
    
    #B Hose First Years
    if(input$contest=="B Hose" & input$Top5v1st=="FirstPlace" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="B Hose" & FirstPct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) +  
              labs(title ="Number of Teams in Year with Chance for First > 1%", x = "Years", y = "Count")
      )
    }
    #B Hose First Teams
    if(input$contest=="B Hose" & input$Top5v1st=="FirstPlace" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="B Hose" & FirstPct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) +  
              labs(title ="Number of Teams from Town with Chance for First > 1%", x = "Team", y = "Count")
      )
    }
    
    #B Hose Top5 Years
    if(input$contest=="B Hose" & input$Top5v1st=="Top5" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="B Hose" & Top5Pct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) +  
              labs(title ="Number of Teams in Year with Chance for Top 5 > 1%", x = "Years", y = "Count")
      )
    }
    #B Hose Top5 Teams
    if(input$contest=="B Hose" & input$Top5v1st=="Top5" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="B Hose" & Top5Pct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) +  
              labs(title ="Number of Teams from Town with Chance for Top 5 > 1%", x = "Team", y = "Count")
      )
    }
    
    #Efficiency First Years
    if(input$contest=="Efficiency" & input$Top5v1st=="FirstPlace" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="Efficiency" & FirstPct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) +  
              labs(title ="Number of Teams in Year with Chance for First > 1%", x = "Years", y = "Count")
      )
    }
    #Efficiency First Teams
    if(input$contest=="Efficiency" & input$Top5v1st=="FirstPlace" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="Efficiency" & FirstPct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) +  
              labs(title ="Number of Teams from Town with Chance for First > 1%", x = "Team", y = "Count")
      )
    }
    
    #Efficiency Top5 Years
    if(input$contest=="Efficiency" & input$Top5v1st=="Top5" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="Efficiency" & Top5Pct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) +  
              labs(title ="Number of Teams in Year with Chance for Top 5 > 1%", x = "Years", y = "Count")
      )
    }
    #Efficiency Top5 Teams
    if(input$contest=="Efficiency" & input$Top5v1st=="Top5" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="Efficiency" & Top5Pct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) +  
              labs(title ="Number of Teams from Town with Chance for Top 5 > 1%", x = "Team", y = "Count")
      )
    }
    
    #Motor Pump First Years
    if(input$contest=="Motor Pump" & input$Top5v1st=="FirstPlace" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="Motor Pump" & FirstPct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) +  
              labs(title ="Number of Teams in Year with Chance for First > 1%", x = "Years", y = "Count")
      )
    }
    #Motor Pump First Teams
    if(input$contest=="Motor Pump" & input$Top5v1st=="FirstPlace" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="Motor Pump" & FirstPct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) +  
              labs(title ="Number of Teams from Town with Chance for First > 1%", x = "Team", y = "Count")
      )
    }
    
    #Motor Pump Top5 Years
    if(input$contest=="Motor Pump" & input$Top5v1st=="Top5" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="Motor Pump" & Top5Pct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) +  
              labs(title ="Number of Teams in Year with Chance for Top 5 > 1%", x = "Years", y = "Count")
      )
    }
    #Motor Pump Top5 Teams
    if(input$contest=="Motor Pump" & input$Top5v1st=="Top5" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="Motor Pump" & Top5Pct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) +  
              labs(title ="Number of Teams from Town with Chance for Top 5 > 1%", x = "Team", y = "Count")
      )
    }

    #Buckets First Years
    if(input$contest=="Buckets" & input$Top5v1st=="FirstPlace" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="Buckets" & FirstPct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) +  
              labs(title ="Number of Teams in Year with Chance for First > 1%", x = "Years", y = "Count")
      )
    }
    #Buckets First Teams
    if(input$contest=="Buckets" & input$Top5v1st=="FirstPlace" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="Buckets" & FirstPct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) +  
              labs(title ="Number of Teams from Town with Chance for First > 1%", x = "Team", y = "Count")
      )
    }
    
    #Buckets Top5 Years
    if(input$contest=="Buckets" & input$Top5v1st=="Top5" & input$x_axis=="Years"){
      print(GOATData %>% 
              filter(Contest..First.=="Buckets" & Top5Pct > 1) %>%
              ggplot(aes(x=Year)) + geom_bar(fill="dark blue") + theme_classic()   +  
              coord_cartesian(xlim = c(1996, 2015)) + 
              scale_x_continuous(breaks=c(1996,2000,2004,2008,2012,2015)) +  
              labs(title ="Number of Teams in Year with Chance for Top 5 > 1%", x = "Years", y = "Count")
      )
    }
    #Buckets Top5 Teams
    if(input$contest=="Buckets" & input$Top5v1st=="Top5" & input$x_axis=="Teams"){
      print(GOATData %>% 
              filter(Contest..First.=="Buckets" & Top5Pct > 1) %>%
              ggplot(aes(x=Team)) + geom_bar(fill="dark blue") + theme_classic()   +  
              theme(axis.text.x = element_text(face="bold", 
                                               size=10, angle=60,hjust=1)) +  
              labs(title ="Number of Teams from Town with Chance for Top 5 > 1%", x = "Team", y = "Count")
      )
    }
    
            
  })
  
  
  
  
})
