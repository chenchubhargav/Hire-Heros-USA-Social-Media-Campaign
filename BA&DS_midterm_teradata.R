#Importing Data Files
getwd()
donor_day<-read.csv('donor_Final_2015.csv') #Donor Daywise December
fbpostdec<-read.csv("fbpostdec15withoutpermlink.csv")  #FB post Daywise
fbpagedec<-read.csv("FacebookPageDataAfterNov2015.csv") #FB page Daywise
twitterDec<-read.csv("Tweeter_dec15_jan16.csv")  #Twitter Daywise
linkedInDec<-read.csv("linkindatadec2015.csv") #LinkedIn Daywise December

fb_m<-read.csv("fb_m.csv") #FB Monthwise

d2014 <- read.csv("donor_2014_M.csv") #Donor month wise 2014
d2015 <- read.csv("donor_2015_M.csv") #Donor monthwise 2015

d2014<-mutate(d2014,Year="2014")
d2015<-mutate(d2015,Year="2015")
d14m_15m<-rbind(d2014,d2015) #Donor 2014 & 2015

#Attaching index in data frame corresponding to month for ordering
fb_m$ordered_Month <- with(data=fb_m,
                                ifelse ((Month == "Jan"),1,
                                ifelse ((Month == "Feb"),2,
                                ifelse ((Month == "Mar"),3,
                                ifelse ((Month == "Apr"),4,
                                ifelse ((Month == "May"),5,
                                ifelse ((Month == "Jun"),6,
                                ifelse ((Month == "Jul"),7,
                                ifelse ((Month == "Aug"),8,
                                ifelse ((Month == "Sep"),9,
                                ifelse ((Month == "Oct"),10,
                                ifelse ((Month == "Nov"),11,
                                        12))))))))))))


d14m_15m$ordered_Month <- with(data=d14m_15m,
                               ifelse ((Month == "Jan"),1,
                                       ifelse ((Month == "Feb"),2,
                                               ifelse ((Month == "Mar"),3,
                                                       ifelse ((Month == "Apr"),4,
                                                               ifelse ((Month == "May"),5,
                                                                       ifelse ((Month == "Jun"),6,
                                                                               ifelse ((Month == "Jul"),7,
                                                                                       ifelse ((Month == "Aug"),8,
                                                                                               ifelse ((Month == "Sep"),9,
                                                                                                       ifelse ((Month == "Oct"),10,
                                                                                                               ifelse ((Month == "Nov"),11,
                                                                                                                       12))))))))))))

#Facebook Month Wise Plot
ggplot(data = fb_m, aes(x = reorder(ordered_Month,Year), y = Engaged.Users, fill = factor(Year))) +
  geom_bar(stat = "identity", position='stack')+
  coord_flip() +
  labs(y = "total donations", x = NULL)

#Facebook Month Wise Plot Yearly Dodged
ggplot(data = fb_m, aes(x = reorder(Month, Year), y = Engaged.Users)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = factor(Year))) +
  labs(y = "\ntotal donations", x = NULL)


#plot for month vs amount donated for 2014

#Donor 2014-2015 Month Wise Plot
ggplot(data = d14m_15m, aes(x = reorder(Month, Year), y = Amount)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = Year,color=Year)) +
  coord_flip() +
  labs(y = "\ntotal donations", x = NULL)

#Donor Month Wise Plot Yearly Dodged
ggplot(data = d14m_15m, aes(x = reorder(Month, Year), y = Amount)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = Year)) +
  labs(y = "total donations", x = NULL)

#LinkedIn Day Wise Data Plot
ggplot(data = linkedInDec, aes(x =DATE,y=company_update_clicks,group=1))+geom_line()

#Donor Day Wise Data Plot
ggplot(data = donor_day, aes(x = Date, y = Amount)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = Date)) +
  labs(y = "total donations", x = NULL)


