setwd("F:/DATA/SLBE/R scripts/ROG CPUE/")#set directory

####TESTING AGAIN FOR FUN!!!!!!#####

library(doBy)
library(reshape)
library("ggplot2")
library(vegan)
library(reshape2)
library(ggplot2)
library(scales)
library(lubridate)
library(extrafont)
library(ggplot2)
library(plyr)
library(outliers)
library(forams)
library(FSA)
#if you don't have FSA installed it is a little tricky...ask TRT for help

source("compiledataforCPUE.R",echo=TRUE, max.deparse.length=10000)
source("F:/DATA/SLBE/R scripts/General Scripts/GobyThemes.R") #Load GGPLOT themes
setwd("F:/DATA/SLBE/R scripts/ROG CPUE/")#set directory
######################################################################################################
#Subsetting the data#

table(FinalFish$Is.this.data.OK.)
Good <- FinalFish[!FinalFish$Is.this.data.OK. %in%  c("NO","CHECK"),]
Good$DayNum<- yday(Good$Date.In) #Add daynumber for easy plotting
Good$Month <- month(Good$Date.In) #Add month for easy plotting

MT <- Good[which(Good$GEAR =="MT"),] #Minnow Traps only
MT$ROG.CPUE.trap.adj<- as.numeric(MT$ROG.CPUE.trap.adj)
GN <- Good[which(Good$GEAR =="GN"),] #Gill nets only

##################################PROBLEMS WITH THE DATA#########################################
#More gobies measured than caught???

#get the rawfishlog
db <- "F:/DATA/SLBE/AVBOT Database.accdb"
con2 <- odbcConnectAccess2007(db)
RawFishLog <- sqlFetch(con2, "Inventory Control Log - Fish")
FieldL<- sqlFetch(con2,"Fish Field Lengths")
close(con2)
colnames(RawFishLog) <- make.names(colnames(RawFishLog ), unique = TRUE)
colnames(FieldL) <- make.names(colnames(FieldL), unique = TRUE)   

Inv.fish.spp <- as.data.frame.matrix(table(RawFishLog$YearSER, RawFishLog$Species))
FieldL.fish.spp <- as.data.frame.matrix(table(FieldL$YearSER, FieldL$Spp.CODE))

Inv.fish.spp$sum <- rowSums(Inv.fish.spp)
FieldL.fish.spp$sum <- rowSums(FieldL.fish.spp)

colnames(FieldL.fish.spp) <- paste("FieldL", colnames(FieldL.fish.spp), sep = "_")
colnames(Inv.fish.spp) <- paste("Inv", colnames(Inv.fish.spp), sep = "_")

Inv.fish.spp$YearSER <- rownames(Inv.fish.spp)
FieldL.fish.spp$YearSER <- rownames(FieldL.fish.spp)


CPUEfish <- FinalFish[,which(names(FinalFish) %in% c("YearSER","Total.Catch", "ALE",                                             
                                                     "BLO","BRS", "BUT","CAP" , "CHS","COS" , "Crayfish","DAR" ,"DWS,EMS" ,
                                                     "JOD"  , "LAT","LMB" , "LNS","LWF"  , "MOS","NSS" , "PUS","PWF"  , 
                                                     "RAS","ROB", "ROG","RWF", "SCL","SLS"  , "SMB","STS" , "Sunfish","TRP", "WAE","WHS" , "YEP"
))]

counts <- join_all(list(CPUEfish, FieldL.fish.spp,  Inv.fish.spp ), by="YearSER", type="left")
write.csv(counts, "counts.csv")


FinalFish$YearSER[FinalFish$Propn.ROG.meas.vs.caught > 1 & FinalFish$Propn.ROG.meas.vs.caught < 100 | FinalFish$Propn.ROG.meas.vs.caught == Inf]

FinalFish$No.ROG.measured[FinalFish$YearSER=="2010-102"]
FinalFish$ROG[FinalFish$YearSER=="2010-102"]
FinalFish$Choice[FinalFish$YearSER== "2010-102"]

FinalFish$No.ROG.measured[FinalFish$YearSER=="2010-116"]
FinalFish$ROG[FinalFish$YearSER=="2010-116"]

FinalFish$Choice[FinalFish$YearSER== "2012-627"]
FinalFish$No.ROG.measured[FinalFish$YearSER=="2012-627"]
FinalFish$ROG[FinalFish$YearSER=="2012-627"]

#2010-101  155/108  #LAB - asked steve, too many problems - change code to make this a field length
#2010-103  153/109  #LAB - asked steve, too many problems - change code to make this a field length
#2010-335  74/45    #LAB - asked steve, too many problems. may need to scrap this one.

##FIXED###
#2010-307  3/2      #LAB - DID TEMPORARY FIX - No indication there was EVER a third goby - change code to make this a field length 
#2011-154  11/10    #FIELD - FIXED ROG -> MOS IN FIELD LENGTHS
#2012-111  36/34    #LAB - FIXED - LAB NUMBER IS ACTUALLY CORRECT. FIXED CATCH LOG.
#2012-148  35/18    #FIELD - FIXED - FISH FROM ANOTHER SERIAL WERE ADDED INTO THE FIELD LENGTHS FOR SOME REASON
#2012-181  20/19    #FIELD - FIXED - ERROR IN NOTES
#2012-627  4/1      #FIELD - ERROR IN NOTES, ROG to STS
#2011-366  #0 -> actually 42 gobies
#2011-365  #0 -> actually 20 gobies
#2010-211 #just missing crayfish count, okay otherwsie


######################################################################################################
####MAKE SOME PLOTS OF CPUE######

####PLOT MINNOW TRAPS ####
####Plot MT CPUE (adj by trap) by year and Dep Non Dep###
ggplot(data=MT, aes(x=DayNum, y=ROG.CPUE.trap.adj, colour=All.DepNonDep)) + geom_point() + Goby_theme + 
  facet_grid(Year ~ .) + scale_x_continuous(breaks=pretty_breaks(n=20))+
  scale_y_continuous(breaks=pretty_breaks(n=5), limits=c(0, 60)) + 
  labs(x="Julian Day", y="Round goby CPUE adjusted by number of traps")

####Plot MT CPUE (adj by trap) by year and GeneralLoc###
ggplot(data=MT, aes(x=DayNum, y=ROG.CPUE.trap.adj, colour=GeneralLoc)) + geom_point() + Goby_theme + facet_grid(Year ~ .) + scale_x_continuous(breaks=pretty_breaks(n=20))+
  scale_y_continuous(breaks=pretty_breaks(n=5), limits=c(0, 60))+ 
  labs(x="Julian Day", y="Round goby CPUE adjusted by number of traps")

####Plot MT CPUE (adj by trap) by year and Depth###
ggplot(data=MT, aes(x=DayNum, y=ROG.CPUE.trap.adj, colour=Avg.trap.depth.m)) + geom_point() + Goby_theme + facet_grid(Year ~ .) + scale_x_continuous(breaks=pretty_breaks(n=20))+
  scale_y_continuous(breaks=pretty_breaks(n=5), limits=c(0, 60))+ 
  labs(x="Julian Day", y="Round goby CPUE adjusted by number of traps")

####Plot MT CPUE (adj by trap) by year and Dep Non Dep AND General Loc for 2012 and 2013 ###
years12.13 <- MT[MT$Year %in%  c("2012","2013"),]
MT$All.DepNonDep<- as.factor(MT$All.DepNonDep)
ggplot(data=years12.13, aes(x=DayNum, y=ROG.CPUE.trap.adj, shape=All.DepNonDep, colour=All.DepNonDep, size=All.DepNonDep)) + geom_point() + Goby_theme + 
  facet_grid(Year ~ GeneralLoc) + scale_x_continuous(breaks=pretty_breaks(n=5))+
  scale_y_continuous(breaks=pretty_breaks(n=5), limits=c(0, 30)) + 
  labs(x="Julian Day", y="Round goby CPUE adjusted by number of traps")+
  scale_colour_manual(values=c("black", "darkgray"), 
                      name="Site Condition",
                      breaks=c("DEP", "NONDEP"),
                      labels=c("Depositional", "Non-depositional"))+
  scale_shape_manual(values=c(17, 20), 
                     name="Site Condition",
                     breaks=c("DEP", "NONDEP"),
                     labels=c("Depositional", "Non-depositional"))+
  scale_size_manual(values=c(3, 4), 
                    name="Site Condition",
                    breaks=c("DEP", "NONDEP"),
                    labels=c("Depositional", "Non-depositional"))

####Plot MT species richness by year and Dep Non Dep###
ggplot(data=MT, aes(x=DayNum, y=Species.Richness, colour=All.DepNonDep)) + geom_point() + Goby_theme + 
  facet_grid(Year ~ .) + scale_x_continuous(breaks=pretty_breaks(n=20))+
  scale_y_continuous(breaks=pretty_breaks(n=5), limits=c(0, 10)) + 
  labs(x="Julian Day", y="Species Richness")

####Plot MT species richness by year and GeneralLoc###
ggplot(data=MT, aes(x=DayNum, y=Species.Richness, colour=GeneralLoc)) + geom_point() + Goby_theme + 
  facet_grid(Year ~ .) + scale_x_continuous(breaks=pretty_breaks(n=20))+
  scale_y_continuous(breaks=pretty_breaks(n=5), limits=c(0, 10)) + 
  labs(x="Julian Day", y="Species Richness")

####Plot MT propn gobies by year and All.DepNonDep for s manitou and g harbor###
ggplot(Good[which(Good$GeneralLoc %in% c("GoodHarbor","SouthManitou")),] , aes(x=DayNum, y=Propn.ROG.vs.Total.Catch, fill=All.DepNonDep)) + geom_bar( position=position_dodge(),stat="identity") + Goby_theme +
  facet_grid(Year ~ GeneralLoc)+ labs(x="Julian Day", y="Proportion of round goby vs. total catch")

####Plot MT + GN propn gobies by year for all sites###
Good$Month<-month(Good$Date.Out)
monthprops<- aggregate(Propn.ROG.vs.Total.Catch ~ Year+Month+GEAR, data = Good, mean)
monthprops[,c(1:3)] <- lapply(monthprops, factor)
monthprops2 <- with(monthprops, expand.grid(Year = levels(Year), Month = levels(Month), GEAR=levels(GEAR)))
newdat <- merge(monthprops, monthprops2, all.y = TRUE)
newdat$Propn.ROG.vs.Total.Catch[is.na(newdat$Propn.ROG.vs.Total.Catch)]<- 0
ggplot(newdat, aes(x=Month, y=Propn.ROG.vs.Total.Catch, fill=GEAR)) + geom_bar( position=position_dodge(),stat="identity") + Goby_pres_theme +
  facet_grid(Year ~ .,scales="free_x")+ labs(x="Month", y="Proportion of round goby vs. total catch")

########PLOT GILL NETS#######
####Plot GN CPUE by year and Dep Non Dep###
ggplot(data=GN, aes(x=DayNum, y=ROG.CPUE, colour=All.DepNonDep)) + geom_point() + Goby_theme + 
  facet_grid(Year ~ .) + scale_x_continuous(breaks=pretty_breaks(n=20))+
  scale_y_continuous(breaks=pretty_breaks(n=5), limits=c(0, 6)) + 
  labs(x="Julian Day", y="Round goby CPUE")

####Plot GN CPUE (adj by trap) by year and GeneralLoc###
ggplot(data=GN, aes(x=DayNum, y=ROG.CPUE, colour=GeneralLoc)) + geom_point() + Goby_theme + facet_grid(Year ~ .) + scale_x_continuous(breaks=pretty_breaks(n=20))+
  scale_y_continuous(breaks=pretty_breaks(n=5), limits=c(0, 6))+ 
  labs(x="Julian Day", y="Round goby CPUE")


######################################################################################################
###########GET SUMMARIES OF GOBY LENGTH ##################

#####FIRST COMPILE THE GOBY LENGTH DATA
##CHOICE.ROG.LENGTHS contains subset YearSER data with EITHER lab or field data
##ALL.ROG.LENGTHS contains all lab AND field data for the yearSER

#make backups
All.ROG.Lengths.1 <- All.ROG.Lengths
All.ROG.Lengths.1 <- Choice.ROG.Lengths

Lengthcols <- c("Standardized.Site.ID", "Event", "Year", "YearSER",
                "SedRate", "MussRate","CladRate","All.DepNonDep",                                   
                "All.SiteCondition","Predom.Simp.Ben.Class","Depth.m.standard","Date.Out","GeneralLoc", "GEAR")

All.ROG.Lengths <- join(All.ROG.Lengths, FinalFish[,Lengthcols], by= "YearSER", type="left")
Choice.ROG.Lengths <- join(Choice.ROG.Lengths, FinalFish[,Lengthcols], by= "YearSER", type="left")

Choice.ROG.Lengths$Month <-month(Choice.ROG.Lengths$Date.Out)
Choice.ROG.Lengths$DayNum <- yday(Choice.ROG.Lengths$Date.Out)
Choice.ROG.Lengths$Year <- as.factor(Choice.ROG.Lengths$Year)

All.ROG.Lengths$Month<-month(All.ROG.Lengths$Date.Out)
All.ROG.Lengths$DayNum <- yday(All.ROG.Lengths$Date.Out)
All.ROG.Lengths$Year <- as.factor(All.ROG.Lengths$Year)
All.ROG.Lengths$Month <- as.factor(All.ROG.Lengths$Month)

#Remove the bad data, where final fish doesn't exist due to bad data#
Choice.ROG.Lengths <- Choice.ROG.Lengths[!is.na(Choice.ROG.Lengths$GEAR),]
All.ROG.Lengths <- All.ROG.Lengths[!is.na(All.ROG.Lengths$GEAR),]

#####Histogram of lengths####
histogram(~Lengthmm|Month,data=Choice.ROG.Lengths[Choice.ROG.Lengths$GeneralLoc=="GoodHarbor" 
                                                  & Choice.ROG.Lengths$GEAR=="MT",],
          type="density",
          xlab="Size (mm)",
          main="Choice.ROG.Lengths")

histogram(~Lengthmm|Month,data=Choice.ROG.Lengths[Choice.ROG.Lengths$GEAR=="MT",],
          type="density",
          xlab="Size (mm)",
          main="Choice.ROG.Lengths")

histogram(~Lengthmm|GeneralLoc,data=Choice.ROG.Lengths[Choice.ROG.Lengths$GEAR=="MT",],
          type="density",
          xlab="Size (mm)",
          main="Choice.ROG.Lengths")

histogram(~Lengthmm|Year,data=Choice.ROG.Lengths[Choice.ROG.Lengths$GEAR=="MT",],
          type="density",
          xlab="Size (mm)",
          main="Choice.ROG.Lengths")

summaryBy(Lengthmm ~ GEAR,data=Choice.ROG.Lengths, FUN=c(mean,sd,min,max,length))
##By dep non dep, MT only
summaryBy(Lengthmm ~ All.DepNonDep,data=Choice.ROG.Lengths[Choice.ROG.Lengths$GEAR=="MT",], FUN=c(mean,sd,min,max,length))
##By GeneralLoc, MT only
summaryBy(Lengthmm ~ GeneralLoc,data=Choice.ROG.Lengths[Choice.ROG.Lengths$GEAR=="MT",], FUN=c(mean,sd,min,max,length))

#PLOT GOBY LENGTHS histograms by GEARS
qplot(Lengthmm, data = Choice.ROG.Lengths, geom = "freqpoly", binwidth = 5, colour = GEAR, xlab="Length (mm)",ylab="Count")+
  Goby_theme + scale_color_manual(values=c("darkgray", "#000000"),labels=c("Gill Net", "Minnow Trap"))
#PLOT GOBY LENGTHS histograms by All.DepNonDep, minnow traps only
qplot(Lengthmm, data = Choice.ROG.Lengths[Choice.ROG.Lengths$GEAR=="MT",], geom = "freqpoly", binwidth = 5, colour = All.DepNonDep, xlab="Length (mm)",ylab="Count")+
  Goby_theme + scale_color_manual(name="Site Condition",values=c("darkgray", "#000000"),labels=c("Depositional", "Non-depositional"))
#PLOT GOBY LENGTHS histograms by General Loc, minnow traps only
qplot(Lengthmm, data = Choice.ROG.Lengths[Choice.ROG.Lengths$GEAR=="MT",], geom = "freqpoly", binwidth = 5, colour = GeneralLoc, xlab="Length (mm)",ylab="Count")+
  Goby_theme 

#Difference in goby lengths between GEAR types?

#Very non-normal data, need to use non-par test
shapiro.test(Choice.ROG.Lengths$Lengthmm[Choice.ROG.Lengths$GEAR=="MT"])
shapiro.test(Choice.ROG.Lengths$Lengthmm[Choice.ROG.Lengths$GEAR=="GN"])
wilcox.test(Choice.ROG.Lengths$Lengthmm[Choice.ROG.Lengths$GEAR=="MT"],Choice.ROG.Lengths$Lengthmm[Choice.ROG.Lengths$GEAR=="GN"] )
##VERY DIFFERENT!

#how about between dep non dep
wilcox.test(Choice.ROG.Lengths$Lengthmm[c(Choice.ROG.Lengths$All.DepNonDep=="DEP",Choice.ROG.Lengths$GEAR=="MT")],Choice.ROG.Lengths$Lengthmm[c(Choice.ROG.Lengths$All.DepNonDep=="NONDEP",Choice.ROG.Lengths$GEAR=="MT")] )
#between generallocs SM and GH?
wilcox.test(Choice.ROG.Lengths$Lengthmm[c(Choice.ROG.Lengths$GEAR=="MT",Choice.ROG.Lengths$GeneralLoc=="SouthManitou")],Choice.ROG.Lengths$Lengthmm[c(Choice.ROG.Lengths$GeneralLoc=="GoodHarbor",Choice.ROG.Lengths$GEAR=="MT")] )
#how about species richness between dep non dep - Minnow traps?
wilcox.test(MT$Species.Richness[MT$All.DepNonDep=="DEP"],MT$Species.Richness[MT$All.DepNonDep=="NONDEP"] )
#how about species richness between dep non dep - Gill nets?
wilcox.test(GN$Species.Richness[GN$All.DepNonDep=="DEP"],GN$Species.Richness[GN$All.DepNonDep=="NONDEP"] )
#how about species richness between GeneralLoc - MT?
wilcox.test(MT$Species.Richness[MT$GeneralLoc=="SouthManitou"],MT$Species.Richness[MT$GeneralLoc=="GoodHarbor"] )

#CPUE between GeneralLoc - MT?
wilcox.test(MT$ROG.CPUE.trap.adj[MT$GeneralLoc=="SouthManitou"],MT$ROG.CPUE.trap.adj[MT$GeneralLoc=="GoodHarbor"] )

###Correlation CPUE vs. Cladrate
shapiro.test(MT$ROG.CPUE.trap.adj)
shapiro.test(MT$CladRate)
cor.test(MT$ROG.CPUE.trap.adj,  MT$CladRate, method="spearman")
MT$CladRate <- as.factor(MT$CladRate)
boxplot(MT$ROG.CPUE.trap.adj~  MT$CladRate)

###Correlation CPUE vs. trap.depth
shapiro.test(MT$ROG.CPUE.trap.adj)
shapiro.test(MT$Avg.trap.depth.m)
cor.test(MT$ROG.CPUE.trap.adj,  MT$Avg.trap.depth.m, method="spearman")
plot(  MT$Avg.trap.depth.m, MT$ROG.CPUE.trap.adj)

##Plot goby lengths over time by depth
FinalFish$DayNum <- yday(FinalFish$Date.Out)

ggplot(data=FinalFish, aes(x=DayNum, y=ROG.Lengthmm.mean)) + geom_point() + Goby_theme + 
  facet_grid(Year ~ Depth.m.standard) + scale_x_continuous(breaks=pretty_breaks(n=20))+
  scale_y_continuous(breaks=pretty_breaks(n=5), limits=c(0, 150)) + 
  labs(x="Julian Day", y="Length (mm)")

#plot goby length by weight with exponential line
ggplot(Choice.ROG.Lengths, aes(x = Lengthmm, y = Weightg)) + geom_point() + geom_smooth(method = "glm", family = gaussian(link="log"),se = TRUE, colour="black") + 
  Goby_theme +labs(x="Round goby total length (mm)", y="Round goby mass (g)", title="All years")+
  scale_x_continuous(breaks=pretty_breaks(n=10)) + scale_y_continuous(breaks=pretty_breaks(n=10), limits=c(0, 150))
#to fit regualar loess line, use geom_smooth() by itself
#####################################################################################

#Prep sample for Bray-curtis

freq2 <- GN[,c(88, 5, 26:58)]

freq2$Month <- month(freq2$Date.Out)

#freq2 <- freq[,2:35]/freq[,36]

#freq2<- cbind(freq2, freq[,c(1,37,39)])

#Remove NA values you don't want
#freq<- freq[!(is.na(freq$All.DepNonDep)), ]

###MAKE TREATMENTS FOR LATER
Month<- freq2$Month
Depth <- freq2$Depth.m.standard
#All.DepNonDep<-freq$All.DepNonDep

#Combine year and site to make sample name
freq2$Sample <- paste(freq2[,1], freq2[,36]) 

row.names(freq2)<-freq2$Sample

###Choose cols/animals to remove
freq2 <- freq2[ ,-which(names(freq2) %in% c("Month", "Date.Out", "Sample", "Depth.m.standard"))]


freq2 <- freq2[, colSums(freq2 == 0) != nrow(freq2)] #Remove any columns that are all zeroes
freq2 = freq2[ rowSums(freq2)!=0, ]  #Remove any rows that are all zeroes

#######VERY IMPORTANT#########
######CONVERT DATAFRAME TO MATRIX!!!!!#######
freq_mat<- data.matrix(freq2)

#shannon diversity
diversity(freq_mat, index = "shannon")

####BRAY
#binary=FALSE means you look at the number of individuals.  
#TRUE would give the result for presence-absence (Sorenson's index)
bc<-vegdist(freq_mat, method="bray", binary=FALSE)

####NMDS####
#using Bray-Curtis ordination
freq.mds<-metaMDS(freq_mat, distance = "bray", k = 2) 
stressplot(freq.mds)

plot(freq.mds) #plots the ordination axes
points(freq.mds, display = c("sites", "species"))#displays both sites and species on the same plot.  
text(freq.mds, display = c("sites", "species"))

treat <- GeneralLoc
ordiplot(freq.mds,type="n")
ordihull(freq.mds,groups=treat,draw="polygon",col="grey90", label=T)
orditorp(freq.mds,display="species",col="red",air=0.01)
orditorp(freq.mds,display="sites",col=c(rep("green",5),rep("blue",5)),
         air=0.01,cex=1.25)

#################################Does mean depth of gobies change over time??######################
###METHODS PROPOSED BY JEAN
#run after running updated Fishcpue dismo

#write.table(MT2, "MT.txt" , sep = "\t")
#Isolate all the MT sets (those that match/go together/form a complete set)

setwd("F:/DATA/SLBE/R scripts/ROG CPUE")
library(reshape2)
library(mgcv)

## Here, we add a "." between upper case letters and numbers
split <- colsplit(gsub("([A-Z])([0-9])", "\\1\\.\\2", MT$Site), 
                  "\\.", c("GenSite", "SiteDep"))

MT2 <- cbind(MT, split)
MT2$GenSite <- as.factor(MT2$GenSite)
MT2$SiteDep <- as.factor(MT2$SiteDep)
MT2$LocYear <- do.call(paste, c(MT2[c("GenSite", "Year")], sep = ".")) #Create LocYear variable

#Keep only what we need
MT2 <- MT2[,which(names(MT2) %in% c("GenSite","SiteDep", "YearSER","Depth.m.standard",
                                    "ROG.CPUE.trap.adj","Year", "LocYear","DayNum","Avg.trap.depth.m","GeneralLoc","All.DepNonDep"))]

sets<- read.csv("sets.csv")

MT3<- join(MT2, sets, by="YearSER")

MT3 <- MT3[!(is.na(MT3$Set)), ]  #Remove ones without a full set

MT3$Set <- as.factor(MT3$Set)

#MT3$ROG.CPUE.trap.adj <- as.numeric(levels(MT3$ROG.CPUE.trap.adj))[MT3$ROG.CPUE.trap.adj]

######Re-categorize sites ########
library(rpart)
# explore depth ####
MT3nona <- MT3[!is.na(MT3$Avg.trap.depth.m), ]
MT3nona <- MT3nona[order(MT3nona$Avg.trap.depth.m), ]
with(MT3nona, {
  hc <- hclust(dist(Avg.trap.depth.m), method="ward.D2")
  group <- cutree(hc, 3)
  par(mar=c(4, 4, 1, 1), cex=1.2, las=1)
  plot(jitter(Depth.m.standard), Avg.trap.depth.m, col=group, lwd=2,
       xlab="Depth categ.  (m)", ylab="Avg. trap depth  (m)")
  print(MT3nona[Depth.m.standard/10 != group, ])
})

#######################2013 sets alone######################
sets2013 <- MT3[MT3$Year=="2013",]
sets2013$Set <- factor(sets2013$Set)

sets2013l <- list()
for(set in levels(sets2013$Set)) {
  sets2013l[[set]] <- weighted.mean(sets2013$Depth.m.standard[sets2013$Set == set], sets2013$ROG.CPUE.trap.adj[sets2013$Set == set]) 
}

wmeans2013 <- do.call(rbind, sets2013l)
wmeans2013<- cbind(wmeans2013, levels(sets2013$Set))
wmeans2013 <- data.frame(wmeans2013)
names(wmeans2013) <- c("W.mean","Set")

Setinfo2013 <- join(wmeans2013, sets2013, by="Set", type = "left", match = "first")

Setinfo2013$W.mean <- as.numeric(levels(Setinfo2013$W.mean))[Setinfo2013$W.mean]

min(Setinfo2013$DayNum)
max(Setinfo2013$DayNum)
min(Setinfo2013$W.mean)
max(Setinfo2013$W.mean)

##SIMPLE PLOT
g<-ggplot(data=Setinfo2013, aes(x=DayNum, y=-(W.mean), colour=LocYear)) + geom_point(size = 3) + Goby_theme + 
  scale_y_continuous(limits = c(-25, -10))+ labs(x="Day Number", y="Weighted mean depth of capture", title="2013 Minnow Trap CPUE, 10-30m")+
  scale_x_continuous(limits = c(190, 320))
g + stat_smooth(method="lm", se=FALSE)

####GAM MODEL####


model2013 <- gam(W.mean ~ s(DayNum) + LocYear + Avg.trap.depth.m, data=Setinfo2013)
summary(model2013)

##SIMPLE PLOT
g<-ggplot(data=Setinfo2013, aes(x=DayNum, y=-(W.mean), colour=LocYear)) + geom_point(size = 3) + Goby_theme + 
  scale_y_continuous(limits = c(-20, -10))+ labs(x="Day Number", y="Weighted mean depth of capture", title="2013 Minnow Trap CPUE, 10-30m")+
  scale_x_continuous(limits = c(190, 275))
g + stat_smooth(method="lm", se=FALSE)

####GAM MODEL####
model2013r <- gam(W.mean ~ s(DayNum) + LocYear, data=Setinfo2013)
summary(model2013r)

############################2010-2013, 10-20 m sites only###############################
TenTwenty<- MT3[!(MT3$Depth.m.standard==30),] #remove 30m depth sites

setlist<- list()
for(set in levels(TenTwenty$Set)) {
  setlist[[set]] <- weighted.mean(TenTwenty$Depth.m.standard[TenTwenty$Set == set], TenTwenty$ROG.CPUE.trap.adj[TenTwenty$Set == set]) 
}

wmeans <- do.call(rbind, setlist)

Sets<- cbind(wmeans, levels(TenTwenty$Set))
Sets <- data.frame(Sets)
names(Sets) <- c("W.mean","Set")

Setinfo <- join(Sets, TenTwenty, by="Set", type = "left", match = "first")

Setinfo$W.mean <- as.numeric(levels(Setinfo$W.mean))[Setinfo$W.mean]

min(Setinfo$DayNum)
max(Setinfo$DayNum)
min(Setinfo$W.mean)
max(Setinfo$W.mean)

##Re-run after removing the crappy sites
#In this case, need to remove: GH.2012
Setinfo<- Setinfo[!(Setinfo$LocYear=="GH.2012"),]
Setinfo<- Setinfo[!(Setinfo$LocYear=="C.2010"),]
Setinfo<- Setinfo[!(Setinfo$LocYear=="D.2010"),]
Setinfo<- Setinfo[!is.nan(Setinfo$W.mean),]


hist(Setinfo$W.mean)
#let's compute some fits...
require(propagate)
fits <- fitDistr(Setinfo$W.mean)
fits$aic

##SIMPLE PLOT #
##colored by year
g<-ggplot(data=Setinfo, aes(x=DayNum, y=-(W.mean), colour=Year)) + geom_point(size = 3) + Goby_theme + 
  scale_y_continuous(limits = c(-20, -10))+ labs(x="Day Number", y="Weighted mean depth of capture", title="2010-2013 Minnow Trap CPUE, 10-20m")
g + stat_smooth(method="lm", se=FALSE)

##colored by locyear, all years
g<-ggplot(data=Setinfo, aes(x=DayNum, y=-(W.mean), colour=LocYear)) + geom_point(size = 3) + Goby_theme + 
  scale_y_continuous(limits = c(-20, -10))+ labs(x="Day Number", y="Weighted mean depth of capture", title="2010-2013 Minnow Trap CPUE, 10-20m")
g + stat_smooth(method="lm", se=FALSE)

##colored by locyear, change the year
g<-ggplot(data=Setinfo[Setinfo$Year=="2010",], aes(x=DayNum, y=-(W.mean), colour=LocYear)) + geom_point(size = 3) + Goby_theme + 
  scale_y_continuous(limits = c(-20, -10))+ labs(x="Day Number", y="Weighted mean depth of capture", title="2013 Minnow Trap CPUE, 10-20m")
g + stat_smooth(method="lm", se=FALSE)

####GAM MODEL####
allyears <- gam(W.mean ~ s(DayNum, by=Year) + GeneralLoc + All.DepNonDep, data=Setinfo)
gam.check(allyears)
summary(allyears)
plot(allyears, residuals=TRUE, pch=1, las=1)
plot(Setinfo$DayNum,fitted(allyears))

####ANCOVA MODEL
model.1=aov(W.mean ~ DayNum + LocYear, data=Setinfo)
Anova(model.1, type="III") 
summary.lm(model.1) 

###########################################################################
########Compare length distributions of lab ROG and field ROG##############

t.test(All.ROG.Lengths$Lengthmm[All.ROG.Lengths$Choice=="Lab"],All.ROG.Lengths$Lengthmm[All.ROG.Lengths$Choice=="Field"])
wilcox.test(All.ROG.Lengths$Lengthmm[All.ROG.Lengths$Choice=="Lab"],All.ROG.Lengths$Lengthmm[All.ROG.Lengths$Choice=="Field"])
ksTest(All.ROG.Lengths$Lengthmm[All.ROG.Lengths$Choice=="Lab"],All.ROG.Lengths$Lengthmm[All.ROG.Lengths$Choice=="Field"])

p1 <- hist(All.ROG.Lengths$Lengthmm[All.ROG.Lengths$Choice=="Lab"], breaks = 10)                     
p2 <- hist(All.ROG.Lengths$Lengthmm[All.ROG.Lengths$Choice=="Field"], breaks = 10)                 
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,200), ylim=c(0,3000))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,200), ylim=c(0,3000), add=T) 

summary(All.ROG.Lengths$Lengthmm[All.ROG.Lengths$Choice=="Lab"])
summary(All.ROG.Lengths$Lengthmm[All.ROG.Lengths$Choice=="Field"])

###########################################################################
##############Are more gobies found in dep in the fall?####################

depan <- MT[MT$Year =="2012"  |  MT$Year =="2013", ]
## Here, we add a "." between upper case letters and numbers
split <- colsplit(gsub("([A-Z])([0-9])", "\\1\\.\\2", depan$Site), 
                  "\\.", c("GenSite", "SiteDep"))

depan <- cbind(depan, split)
depan$GenSite <- as.factor(depan$GenSite)
depan$SiteDep <- as.factor(depan$SiteDep)
depan <- depan[!is.na(depan$All.DepNonDep),]
depan$Season<- ifelse(depan$DayNum <200, "Summer", 
                      ifelse(depan$DayNum>268, "Fall", NA))
depan <- depan[!is.na(depan$Season),]
depanvars  <- names(depan) %in% c("ROG.CPUE.trap.adj", "All.DepNonDep", "GenSite", "Year", "Season", "SiteDep") 
depan <- depan[depanvars]
depan$ROG.CPUE.trap.adj <- as.numeric(levels(depan$ROG.CPUE.trap.adj))[depan$ROG.CPUE.trap.adj]

#write.csv(depan,"depan.csv")

#testin <- cast(depan, All.DepNonDep + GenSite + Year + SiteDep ~ Season, value='ROG.CPUE.trap.adj', mean)

testy <- gam(ROG.CPUE.trap.adj ~  Year + Season + All.DepNonDep + SiteDep, data=depan)
gam.check(testy)
summary(testy)


###########################################################################
###############what are clad rates of benthic classes?#####################

aggregate(MT$CladRate, by=list(MT$Predominant.Benthic.Class),  FUN=mean, na.rm=TRUE)
MT$Season<- ifelse(MT$DayNum <200, "Summer", ifelse(MT$DayNum>268, "Fall", NA))
cast(MT, Predominant.Benthic.Class ~ Season, value='CladRate', mean)
aggregate(MT$CladRate, by=list(MT$Season), FUN=mean, na.rm=TRUE)

