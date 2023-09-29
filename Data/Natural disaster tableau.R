"C:\Users\edmee\OneDrive - De Vinci\A4\S8\Simulation\Natural disaster.csv"
nd <- read.csv("C:/Users/edmee/OneDrive - De Vinci/A4/S8/Simulation/Natural disaster.csv", skip = 6,header=T,sep=";")
unique(nd$Disaster.Type)
colnames(nd)
keep=c("Disaster.Type","Year","ISO","Start.Month","Total.Deaths")
natural_dis=nd[,keep]
epidemic=natural_dis[natural_dis$Disaster.Type=='Epidemic',]

natural_dis=subset(natural_dis,natural_dis$Disaster.Type!='Epidemic' )

natural_dis2 = natural_dis %>% group_by(Year, ISO,Disaster.Type)  %>%
  summarise(Nombre=length(Disaster.Type),
            Mort=sum(Total.Deaths),
            .groups = 'drop')

epidemic2=epidemic %>% group_by(Year, ISO,Disaster.Type)  %>%
  summarise(Nombre=length(Disaster.Type),
            Mort=sum(Total.Deaths),
            .groups = 'drop')

df

ne <- dcast(natural_dis2,Year+ISO~Disaster.Type,value.var = "Nombre")
ne[is.na(ne)]<-0
type=unique((nd$Disaster.Type))
type=subset(type,type!="Epidemic")
ne$Death_nd=rowSums(ne[, type])

mp<-left_join(df,ne,by=c("Year","ISO"))
names(df)[8] <- "ISO"
