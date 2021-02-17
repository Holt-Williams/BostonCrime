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
    ##Note First API Use and it works, need to fully integrate at all junctions
    ##Q: whats the best way to do this as this is hacked together?
    output$nRows <- renderText({ 
        #df%>%filter(DISTRICT==input$district)%>%nrow()
        nrows<-dplyr::filter(my_tbl, DISTRICT == local(input$district))%>%summarise(nrows=n())%>%collect()
        as.character(nrows[1])
    })
    
    output$nRowsUnique <- renderText({
        data<-dplyr::filter(my_tbl, DISTRICT == local(input$district))%>%summarise(distinc= n_distinct(INCIDENT_NUMBER), total=n())%>%collect()
        as.character(round(as.integer(data$distinc)/as.integer(data$total),digits=3))
        
    })

    output$countDistricts <- renderText({
        nuniq<-dplyr::filter(my_tbl)%>%summarise(n=n_distinct(DISTRICT))%>%collect()
        as.character(nuniq)
    })
    
    output$dangerStreets <- renderPlot({
      data2<-dplyr::filter(my_tbl, DISTRICT == local(input$district),STREET != "", OCCURRED_ON_DATE>local(input$daterange[1]), OCCURRED_ON_DATE<local(input$daterange[2]))%>%
        collect()
      
        top_n(as.data.frame(table(data2$STREET)), n=10)%>%ggplot(aes(x=Var1, y=Freq))+
            geom_col(aes(fill=Var1))+coord_flip()+
            theme_fivethirtyeight()+theme(legend.position = "none")+
            ggtitle(paste(input$district,"'s ","streets With the Most Incidences", sep=""))+
            scale_fill_brewer(palette = "Paired")
    })
    
    choices_streets <- reactive({
        data2<-dplyr::filter(my_tbl, DISTRICT == local(input$district),STREET != "", OCCURRED_ON_DATE>local(input$daterange[1]), OCCURRED_ON_DATE<local(input$daterange[2]))%>%
            select(STREET)%>%collect()%>%table()%>%as.data.frame()%>%top_n(n=10)
    })
    
    observe({
        updateSelectInput(session = session, inputId = "topstreets", choices = choices_streets()$.)
    })
    
    
    output$offensegroupsStreets <- renderPlot({
        data<- dplyr::filter(my_tbl,DISTRICT==local(input$district),STREET != "", OFFENSE_CODE_GROUP != "",STREET == local(input$topstreets),OCCURRED_ON_DATE>local(input$daterange[1]), OCCURRED_ON_DATE<local(input$daterange[2]))%>%
            select(OFFENSE_CODE_GROUP)%>%collect()%>%table()%>%as.data.frame()%>%top_n(n=10)
        
        data%>%ggplot(aes(x=reorder(.,Freq), y=Freq))+
            geom_col(aes(fill="#FF7F00"))+coord_flip()+
            theme_fivethirtyeight()+theme(legend.position = "none")+
            ggtitle(paste("Top Ten Crimes on ",input$topstreets))
    })
    
    
    choices_district <- reactive({
      
      data2<-dplyr::filter(my_tbl, DISTRICT != "External",DISTRICT != "Blank" )%>%select(DISTRICT)%>%collect()%>%unique()
       
    })
    
    observe({
        updateSelectInput(session = session, inputId = "district", choices = choices_district())
    })
    
    ## why not leaflet? idk, it's broken when I try to use html things with a template
    ## Map doesn't cenetr as it throws erros and slows down
    output$mymap <- renderPlot({
        
        mapdata<- dplyr::filter(my_tbl,DISTRICT==local(input$district),STREET != "",STREET == local(input$topstreets),OCCURRED_ON_DATE>local(input$daterange[1]), OCCURRED_ON_DATE<local(input$daterange[2]))%>%
            filter(!is.na(Lat),!is.na(Long))

         g<-Bos_map+geom_point(aes(x=as.numeric(Long), y=as.numeric(Lat)), data=mapdata, size=3, alpha=.5, color="darkorange")+
            theme_void()
        plot(g)
        
        }) 
    
    output$timePlot <- renderPlot({
        timedata<- dplyr::filter(my_tbl,DISTRICT==local(input$district),STREET != "",STREET == local(input$topstreets),OCCURRED_ON_DATE>local(input$daterange[1]), OCCURRED_ON_DATE<local(input$daterange[2]))
        
        
        timedata%>%ggplot(aes(x=as.numeric(HOUR), fill=DAY_OF_WEEK))+geom_bar()+xlab("Hour")+ylab("Count")+labs(fill = "Day of Week")
       
    })
    
    
    
    
    
    
## Below this is not used/Broken    
  
    
    # output$incidentsYear <- renderPlot({
    #    data1<- df%>%
    #         select(OCCURRED_ON_DATE,INCIDENT_NUMBER,YEAR)%>%
    #         distinct()%>%
    #         group_by(OCCURRED_ON_DATE)%>%
    #         mutate(count=length(OCCURRED_ON_DATE))
    #    
    #     ggplot(data=data1,aes(y=count,x=OCCURRED_ON_DATE))+
    #         geom_line(aes(color=YEAR))+geom_smooth(color="Orange")+
    #         theme_fivethirtyeight()+theme(axis.text.x=element_blank(),
    #                                       axis.ticks.x=element_blank())+
    #         xlab("Incidents")+ylab("Months")+ggtitle("Incidents by Year")+
    #         xlim(input$daterange[1],input$daterange[2])
    # })
    
    # output$shootingPlot <- renderPlot({
    #     data2<-df%>%
    #         select(SHOOTING,OCCURRED_ON_DATE,INCIDENT_NUMBER,YEAR)%>%
    #         filter(SHOOTING=='Yes')%>%
    #         distinct()%>%
    #         group_by(OCCURRED_ON_DATE)%>%
    #         mutate(count=length(OCCURRED_ON_DATE))
    #     
    #         ggplot(data=data2,aes(x=OCCURRED_ON_DATE,y=count, color=YEAR))+
    #         geom_point()+stat_smooth()+
    #         theme_fivethirtyeight()+
    #         ggtitle("Total Shootings per Day: 2015 - 2020")+
    #             xlim(input$daterange2[1],input$daterange2[2])
    # })
   
    
    
})
