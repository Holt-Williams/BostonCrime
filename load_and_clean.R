### Load data

#df<- read.csv("data/crime.csv")


### Check for issues

#head(df)
#str(df)
#cols<-colnames(df)[c(2,6,9:10,12)]
#df<-df%>%mutate_at(cols, funs(factor(.)))
#df$OCCURRED_ON_DATE<-as.Date(df$OCCURRED_ON_DATE)
# Order Day of Week Correctly
#df$DAY_OF_WEEK <- ordered(df$DAY_OF_WEEK, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                       #  "Friday", "Saturday", "Sunday"))
#df$INCIDENT_NUMBER<-as.character(df$INCIDENT_NUMBER)
#levels(df$SHOOTING)<-c("No","No","Yes","Yes")
### https://bpdnews.com/districts, lists the names of the districts and corresponding short hand
#names<-c('Blank',
         # 'Downtown',
         # 'Charlestown',
         # 'East Boston',
         # 'Roxbury',
         # 'Mattapan',
         # 'Dorchester',
         # 'South Boston',
         # 'Brighton',
         # 'South End',
         # 'Jamaica Plain',
         # 'Hyde Park',
         # 'West Roxbury',
         # 'External'
         # )
#levels(df$DISTRICT)<-names
## table(df$DISTRICT), about 2500 blank. Should these ebe Dropped?

## Fix Offense CODE Group Issues misspelling
#df$OFFENSE_CODE_GROUP<-fct_recode(df$OFFENSE_CODE_GROUP,'INVESTIGATE PERSON'= "Investigate Person")

## Universal Crime Reporting Number?
## https://en.wikipedia.org/wiki/Uniform_Crime_Reports#UCR_crime_categories
#UCR<-c("Blank",
#       "Other",
#       "Index Crimes",
#       "Part 3",
#       "Property Crimes")

register_google() # removed api key
map.center <- geocode("Boston, MA")
Bos_map <- qmap(c(lon=map.center$lon, lat=map.center$lat), zoom=11)
#table(df$UCR_PART)


#test<-df%>%drop_na()%>%filter(Long<0,Lat>0)



# Time Series for Forecasting
## select(Count=count)%>%ts(frequency=7)
#TS_forecast<-forecast(auto.arima(TS), h=14)
print(2)


## API BAse Calls
## Speed could be gained through optimizing this per inputs
my_ckan <- src_ckan("https://data.boston.gov/")
my_tbl <- tbl(src = my_ckan, name = "12cb3883-56f5-47de-afa5-3b1cf61b257b")

