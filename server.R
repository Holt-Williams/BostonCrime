#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("library.R")
source("load_and_clean.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    output$minDate <- renderText({
        format(input$daterange[1])
    })
    
    output$maxDate <- renderText({
        format(input$daterange[2])
    })
    
    output$district <- renderText({
        input$district
    })
    
    output$district2 <- renderText({
        input$district
    })
    
    output$streetName <- renderText({
        input$topstreets
    })
    
    output$nRows <- renderText({ 
        df%>%filter(DISTRICT==input$district)%>%nrow()
    })
    
    output$nRowsUnique <- renderText({
        data<-df%>%filter(DISTRICT==input$district)
        as.character(round(length(unique(data$INCIDENT_NUMBER))/length(data$INCIDENT_NUMBER),digits=3))
    })

    output$countDistricts <- renderText({
        as.character(nlevels(df$DISTRICT))
    })
    
    output$dangerStreets <- renderPlot({
        data2<-df%>%select(DISTRICT,STREET,OCCURRED_ON_DATE)%>%filter(DISTRICT==input$district,STREET != "", OCCURRED_ON_DATE>input$daterange[1], OCCURRED_ON_DATE<input$daterange[2])
        
        top_n(as.data.frame(table(data2$STREET)), n=10)%>%ggplot(aes(x=Var1, y=Freq))+
            geom_col(aes(fill=Var1))+coord_flip()+
            theme_fivethirtyeight()+theme(legend.position = "none")+
            ggtitle(paste(input$district,"'s ","streets With the Most Incidences", sep=""))+
            scale_fill_brewer(palette = "Paired")
    })
    
    choices_streets <- reactive({
        data2<-df%>%select(DISTRICT,STREET,OCCURRED_ON_DATE)%>%
            filter(DISTRICT==input$district,STREET != "", OCCURRED_ON_DATE>"2015-06-15", OCCURRED_ON_DATE<"2020-06-16")%>%
            select(STREET)%>%table()%>%as.data.frame()%>%top_n(n=10)
    })
    
    observe({
        updateSelectInput(session = session, inputId = "topstreets", choices = choices_streets()$.)
    })
    
    
    output$offensegroupsStreets <- renderPlot({
        data<- df%>%filter(DISTRICT==input$district,STREET != "", OFFENSE_CODE_GROUP != "",STREET == input$topstreets,OCCURRED_ON_DATE>input$daterange[1], OCCURRED_ON_DATE<input$daterange[2])%>%
            select(OFFENSE_CODE_GROUP)%>%table()%>%as.data.frame()%>%top_n(n=10)
        
        data%>%ggplot(aes(x=reorder(.,Freq), y=Freq))+
            geom_col(aes(fill="#FF7F00"))+coord_flip()+
            theme_fivethirtyeight()+theme(legend.position = "none")+
            ggtitle(paste("Top Ten Crimes on ",input$topstreets))
    })
    
    
    choices_district <- reactive({
        data2<-df%>%select(DISTRICT)%>%
            filter(DISTRICT != "External",DISTRICT != "Blank" )%>%
            unique()
    })
    
    observe({
        updateSelectInput(session = session, inputId = "district", choices = choices_district())
    })
    
    output$mymap <- renderPlot({
        
        mapdata<- df%>%filter(DISTRICT==input$district,STREET != "",STREET == input$topstreets,OCCURRED_ON_DATE>input$daterange[1], OCCURRED_ON_DATE<input$daterange[2])%>%
            filter(!is.na(Lat),!is.na(Long))
        Bos_map <- qmap(c(lon=mean(mapdata$Long), lat=mean(mapdata$Lat)), zoom=12)
        g <- Bos_map + geom_point(aes(x=Long, y=Lat), data=mapdata, size=3, alpha=0.2, color="darkorange")+
            theme_void()
        plot(g, height = 400, width = 600 )
        
        })
    
    output$timePlot <- renderPlot({
        timedata<- df%>%
            filter(DISTRICT==input$district,STREET != "",STREET == input$topstreets,OCCURRED_ON_DATE>input$daterange[1], OCCURRED_ON_DATE<input$daterange[2])
        
        
        timedata%>%ggplot(aes(x=HOUR, fill=DAY_OF_WEEK))+geom_bar()
       
    })
    
    
    
    
    
    
## Below this is not used/Broken    
    output$districtTable <- renderTable({
        df%>%
            select(DISTRICT,INCIDENT_NUMBER,YEAR)%>%
            group_by(DISTRICT,YEAR)%>%
            summarise(n=n())%>%
            spread(key=YEAR, value=n)
    })
    
    output$incidentsYear <- renderPlot({
       data1<- df%>%
            select(OCCURRED_ON_DATE,INCIDENT_NUMBER,YEAR)%>%
            distinct()%>%
            group_by(OCCURRED_ON_DATE)%>%
            mutate(count=length(OCCURRED_ON_DATE))
       
        ggplot(data=data1,aes(y=count,x=OCCURRED_ON_DATE))+
            geom_line(aes(color=YEAR))+geom_smooth(color="Orange")+
            theme_fivethirtyeight()+theme(axis.text.x=element_blank(),
                                          axis.ticks.x=element_blank())+
            xlab("Incidents")+ylab("Months")+ggtitle("Incidents by Year")+
            xlim(input$daterange[1],input$daterange[2])
    })
    
    output$shootingPlot <- renderPlot({
        data2<-df%>%
            select(SHOOTING,OCCURRED_ON_DATE,INCIDENT_NUMBER,YEAR)%>%
            filter(SHOOTING=='Yes')%>%
            distinct()%>%
            group_by(OCCURRED_ON_DATE)%>%
            mutate(count=length(OCCURRED_ON_DATE))
        
            ggplot(data=data2,aes(x=OCCURRED_ON_DATE,y=count, color=YEAR))+
            geom_point()+stat_smooth()+
            theme_fivethirtyeight()+
            ggtitle("Total Shootings per Day: 2015 - 2020")+
                xlim(input$daterange2[1],input$daterange2[2])
    })
    
    output$shootingPlotBar <- renderHighchart({
        data3<-df%>%
            select(SHOOTING,OCCURRED_ON_DATE,INCIDENT_NUMBER,OFFENSE_DESCRIPTION)%>%
            filter(SHOOTING=='Yes')%>%
            distinct()%>%mutate(qtr=as.yearqtr(OCCURRED_ON_DATE))%>%
            group_by(OCCURRED_ON_DATE)%>%
            mutate(grouping=ifelse(OFFENSE_DESCRIPTION=="INVESTIGATE PROPERTY" | OFFENSE_DESCRIPTION=="BALLISTICS EVIDENCE/FOUND","BALLISTICS EVIDENCE/FOUND & INVESTIGATE PROPERTY", "All Other"))
        data3$qtr<-format(data3$qtr, format = "%y/0%q")
        data4<-data3%>%ungroup()%>%select(qtr,grouping)%>%
            group_by(qtr,grouping)%>%
            mutate(count=as.integer(length(qtr)))%>%
            arrange(qtr)%>%slice(1)%>%spread(grouping, count)%>%
            mutate(total=sum(c(`All Other`,`BALLISTICS EVIDENCE/FOUND & INVESTIGATE PROPERTY`)))%>%drop_na()
        
    if(input$radio == "1") 
        {highchart() %>% 
            hc_chart(type = "column") %>%
            hc_plotOptions(column = list(stacking = "normal")) %>%
            hc_xAxis(categories = data4$qtr) %>%
            hc_add_series(name="All Shooting Tagged Incidences",
                          data = data4$total,
                          color="Gray")
          
        } else  {
            highchart() %>% 
                hc_chart(type = "column") %>%
                hc_plotOptions(column = list(stacking = "normal")) %>%
                hc_xAxis(categories = data4$qtr) %>%
                hc_add_series(name="BALLISTICS EVIDENCE/FOUND & INVESTIGATE PROPERTY",
                              data = data4$`BALLISTICS EVIDENCE/FOUND & INVESTIGATE PROPERTY`,
                              color="Orange")%>%
                hc_add_series(name="All Other",
                              data = data4$`All Other`,
                              color="Gray")
            }
        
    })
    
    
})
