# Purpose: Birds-of-paradise sound analysis project-- 
#       1. Data-check Raven selection tables
#       2. PCA cluster analysis of continuous sound measurements  
#       3. Agglomerative hierarchical clustering based on categorical signal descriptions to determine call types
#       4. Visually inspect results (with grid of spectrograms)


#Run on Mac in ML Common Area
setwd(dir<-"/BoPmedia/BOP sound analysis/Completed_SOUND")
directories<-list.dirs("/BoPmedia/BOP sound analysis/Completed_SOUND/Completed")[-1] #Gets directories (not parent directory)
species.names<-list.dirs("/BoPmedia/BOP sound analysis/Completed_SOUND/Completed",full.names=FALSE)[-1]

#Run on Rusty's
# setwd(dir<-"C:/Users/Rusty/Amazon Drive/BOP/SOUND")
# directories<-list.dirs("C:/Users/Rusty/Amazon Drive/BOP/Sound/Completed")[-1] #Gets directories (not parent directory)
# species.names<-list.dirs("C:/Users/Rusty/Amazon Drive/BOP/Sound/Completed",full.names=FALSE)[-1]

#Run on Janelle's laptop
# setwd(dir<-"/Users/jlm394/Desktop/R_working/BoP")
# directories<-list.dirs("/Users/jlm394/Desktop/R_working/BoP/Completed")[-1] #Gets directories (not parent directory)
# species.names<-list.dirs("/Users/jlm394/Desktop/R_working/BoP/Completed",full.names=FALSE)[-1]




################################################
##### 1. Data-check Raven selection tables #####
################################################

mega<-c()
naindex<-0  #N of files for which special data assignements are missing
naughtyindex<-0 #N of files for which special data assignements are not 0/1
checkindex<-c("Freq_Modulation","Harmonics","Non.Harmonic_Struc","Impulsive","Stochastic")
missingcols<-0
#Loops through all files and reads them into COMPOSITE Dataframe "Mega", omitting (and identifying)
# elements that are missing/wrong values for the 5 binary, human-classified variables
for(xx in 1:length(species.names)){ #Loop through each folder
  
  file.names<-list.files(directories[xx],full.names=TRUE,pattern=".txt")
  print(species.names[xx])
  
  for(yy in 1:length(file.names)){ #Loop through text files for each species
    
    print(yy)
    
    out <- tryCatch({read.delim(file.names[yy],sep="\t",header=T,row.names=NULL,colClasses=c("Freq_Modulation"="factor","Harmonics"="factor","Non.Harmonic_Struc"="factor","Impulsive"="factor","Stochastic"="factor"),fill=TRUE)},
                    warning = function(w) {cat('In warning handler\n');print(w);w},error = function(e) { cat('In error handler\n'); print(e); e })#function checks if nls model returns error
    
    
    

    
    
    if(any(class(out) == "warning")){ 
      
      print(paste("THIS FILE ",file.names[yy]," IS MISSING KEY COLUMNS"))
      
      if(missingcols==0){
        MCErrors<-c(species.names[xx],file.names[yy])
      } else {
        MCErrors<-rbind(MCErrors,c(species.names[xx],file.names[yy]))
      }
      missingcols<-missingcols+1 
      } else {
    #read in table, and provide specific classes for specific columns
    table<-read.delim(file.names[yy],sep="\t",header=T,row.names=NULL,colClasses=c("Freq_Modulation"="factor","Harmonics"="factor","Non.Harmonic_Struc"="factor","Impulsive"="factor","Stochastic"="factor"),fill=TRUE)
    #read.csv('test.csv', colClasses=c("time"="character"))
  #This line condenses the raw output of Raven from the Spectrogram and Wavelength windows into one table
#  data.frame(table[seq(1,nrow(table),2),c(grep("Selection",names(table)),grep("Begin",names(table)),grep("End",names(table)),grep("Delta",names(table)),grep("Low",names(table)),grep("High",names(table)),grep("Delta.Freq",names(table)),grep("File",names(table)),grep("Note",names(table)),grep("Song",names(table)),grep("Quality",names(table)))],na.omit(table$Peak.Freq..Hz.),na.omit(table[,grep("RMS",colnames(table))]))->table

  
#     data.frame(table[seq(2,nrow(table),2),c(grep("Selection",names(table)),grep("Begin",names(table)),grep("End",names(table)),
#                                           grep("BW.90...Hz.",names(table)),grep("Freq.5...Hz.",names(table)),grep("Freq.95...Hz.",names(table)),
#                                           grep("Time.5...s.",names(table)),grep("Time.95...s.",names(table)),grep("Dur.90...s.",names(table)),
#                                           grep("File",names(table)),grep("Note",names(table)),
#                                           grep("Song",names(table)),grep("Quality",names(table)))],na.omit(table$Peak.Freq..Hz.),
#                                           na.omit(table[,grep("RMS",colnames(table))]))->table
  
    table$recording<-sub("C:/Users/Rusty/Amazon Drive/BOP/Sound/Completed/", "", (file.names[yy]))
    #sub("C:/Users/Rusty/Amazon Drive/BOP/Sound/Janelle/", "", substring(as.character(file.names[yy]),61))
    table$species<-species.names[xx]
    chuck<-ncol(table)
    table<-table[,c(chuck,chuck-1,1:(chuck-2))]
    
    
    
   
    
    mini<-table[,c(checkindex)] # mini[4,4]<-NA  mini[1,3]<-"01" mini[8,2]<-3
    
    table$hasNAs<-rowSums(is.na(mini))
    
   # summary(table[,c(checkindex)])  
   # table[,c(checkindex)]<-factor(table[,c(checkindex)])
   # table[,c(checkindex)]<-lapply(table[,c(checkindex)], function(x) as.factor((x)))
  
    if(sum(table$hasNAs>0)){
      print("NA DETECTED IN:")
      print(table[which(table$hasNAs==1),c("species","recording","Selection")])
      
      if(naindex==0){
      NaErrors<-table[which(table$hasNAs==1),c("species","recording","Selection")]
      } else {
        NaErrors<-rbind(NaErrors,table[which(table$hasNAs==1),c("species","recording","Selection")])
      }
      naindex<-naindex+1 
      
      table<-table[-which(table$hasNAs==1),]
    }           
    
    if(length(which(apply(mini, 1, function(r) any(r != "0" & r !="1"))))>0){
      print("ISSUE DETECTED IN:")
      print(table[ which(apply(mini, 1, function(r) any(r != 0 & r !=1))),c("species","recording","Selection")])
      
      
      if(naughtyindex==0){
        NaughtyErrors<-table[ which(apply(mini, 1, function(r) any(r != "0" & r !="1"))),c("species","recording","Selection")]
      } else {
        NaughtyErrors<-rbind(NaughtyErrors,table[ which(apply(mini, 1, function(r) any(r != "0" & r !="1"))),c("species","recording","Selection")])
      }
      naughtyindex<-naughtyindex+1 
      
      table<-table[-which(apply(mini, 1, function(r) any(r != 0 & r !=1))),]
    }
    
    
    subtable<-table[,c("species","recording","Selection","Begin.Time..s.","End.Time..s.",
                       
                       
                       
                       
                       "Delta.Time..s.",
                       
                       "Avg.Entropy..bits.",
                       "Agg.Entropy..bits.",
                       "BW.90...Hz.",
                       
                       "Min.Entropy..bits.",
                       "Max.Entropy..bits.",
                       
                       "Peak.Freq..Hz.",
                       "Dur.90...s.",
                       "Freq.5...Hz.",
                       "Freq.95...Hz.",
                       
                     #  "Peak.Freq.Contour..Hz.",
                       "PFC.Avg.Slope..Hz.ms.",
                       "PFC.Max.Freq..Hz.",
                       "PFC.Max.Slope..Hz.ms.",
                       "PFC.Min.Freq..Hz.",
                       "PFC.Min.Slope..Hz.ms.",
                       
                       "Freq_Modulation","Harmonics","Non.Harmonic_Struc","Impulsive","Stochastic",
                        "Clean")]
    
    
    
    
    
    if (xx==1 & yy==1) {
      Mega<-subtable
    } else {
      Mega<-rbind(Mega,subtable)
    }


      } #else, for non-warnings in table read in
  }
}


#print(NaErrors)
print(NaughtyErrors) #Prints full df of errors with identifying info

print(MCErrors)

# Include only "Clean" selections in results
alldirtyclean<-Mega

Mega$Clean<-factor(Mega$Clean)

Mega<-Mega[which(Mega$Clean==1),]
Mega<-Mega[,-26]

# Prepare Mega for use with warbleR
# Add .wav file name to Mega
library(stringr)
Mega$sound.files<-paste(str_sub(Mega$recording,-10,-5),".wav",sep='')
Mega<-Mega[,c(1,2,26,3:25)]

# Rename to selec.file, selec, start, end in Mega
colnames(Mega)[which(colnames(Mega) %in% c("recording", "Selection", "Begin.Time..s.", "End.Time..s."))]<- c("selec.file","selec", "start", "end")



#####################################################################
##### 2. PCA cluster analysis of continuous sound measurements  #####
#####################################################################

#Color-scale species names
Mega$species<-factor(Mega$species)
nspecies<-length(unique(Mega$species))
#Create a custom color scale
library(RColorBrewer)
myColors <- colorRampPalette(brewer.pal(9,"Set3"))(50)
names(myColors) <- levels(Mega$species)

#Grab basic selection info and the Categories info 
info<-colnames(Mega)[1:6]
Category1<-Mega[,c(info,checkindex)]
Category1$combo<-paste(Category1[,checkindex[1]],Category1[,checkindex[2]],Category1[,checkindex[3]],Category1[,checkindex[4]],Category1[,checkindex[5]],sep=".")
Category1$combo<-factor(Category1$combo)

rawData<-Mega[,-c(1:6)]
rawData<-rawData[,-which(colnames(rawData) %in% checkindex)] #Gets rid of binary variables---we don't want them in our PCA


#rawData[checkindex] <- lapply(rawData[checkindex], function(x) as.numeric(as.character(x))) #uses checkindex for binary huaman assigned variables 
summary(rawData)
scaled<-scale(rawData)

# s.pca<-princomp(scaled)
# summary(s.pca)
# s.pca$loadings
# summary(s.pca$scores)
# plot(s.pca)
# print(s.pca)
# 
# head(s.pca$x)
# 
# library(rgl)
# limitvalues<-c(min(s.pca$scores),max(s.pca$scores))
# 
# plot3d(s.pca$scores[,c(1,4,14)], col=myColors[Mega$species],xlim=limitvalues,ylim=limitvalues,zlim=limitvalues)
# 
# hist(s.pca$scores[,1],xlim=limitvalues)
# hist(s.pca$scores[,4],xlim=limitvalues)
# hist(s.pca$scores[,14],xlim=limitvalues)
# 
# 
# 
# #text3d(s.pca$scores[,1:3],texts=Mega$species)
# text3d(10*s.pca$loadings[,1:3], texts=rownames(s.pca$loadings), col="red")
# coords <- NULL
# for (i in 1:nrow(s.pca$loadings)) {
#   coords <- rbind(coords, rbind(c(0,0,0),s.pca$loadings[i,1:3]))
# }
# lines3d(10*coords, col="red", lwd=4)


# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 




library(rgl)

#prcomp is preferred https://stats.stackexchange.com/questions/20101/what-is-the-difference-between-r-functions-prcomp-and-princomp
sound.pca <- prcomp(scaled,center = TRUE,scale. = TRUE) 
print(sound.pca)
plot(sound.pca)
#If Error in plot.new() : figure margins too large prints, check par("mar")
#par("mar")
##[1] 5.1 4.1 4.1 2.1     #If these are the values, change to,
#par(mar=c(1,1,1,1))
summary(sound.pca)
diff<-apply(sound.pca$x,2,function(x) range(x)[2]-range(x)[1])
biplot(sound.pca)

head(sound.pca$x)
summary(sound.pca$x)



limitvalues<-c(min(sound.pca$x),max(sound.pca$x))
plot3d(sound.pca$x[,c(1:3)], col=myColors[Mega$species],xlim=limitvalues,ylim=limitvalues,zlim=limitvalues)




#####################################################
##### 3. Agglomerative hierarchical clustering  #####
#####################################################

### A. DEFINE CLUSTERING THRESHOLD BASED ON 5% VARIATION TO CLUSTER POINTS IN COLOR SPACE
####################

dS22<-sound.pca$x[,1:3]

dist.matrix<-dist(dS22)
hc <-fastcluster::hclust(dist.matrix, method="ward.D2", members=NULL) #Expects squared distance values
plot(hc)

cutted<-cutree(hc,h=4)
plot(cutted)

for(j in 1:250){
  cutie<-cutree(hc,h=j)
  plot(length(unique(cutie))~j,xlim=c(0,250),ylim=c(0,1000))
  par(new=TRUE)
}

### B. DEFINE CLUSTERING THRESHOLD BASED ON % VARIATION DIFFERENCE YOU WANT BETWEEN CLUSTERED GROUPS
####################

s1<-summary(sound.pca)$importance
s2<-s1[3,3]
cutmark<-max(c(dist(sound.pca$x[,c(1:3)])))*(0.75/s2) #Find maximum euclidean distance between two points (among PCs 1-3), 
#and multiplies this by the prop variation threshold (0.05) divided by the prop variation explained by these 3 PCs

clusterid<-cutree(hc, h = cutmark) 
plot(clusterid)
clusters<-as.vector(unique(clusterid))
l.clusters<-length(clusters[clusters !=0])

Mega$notetype<- factor(clusterid) #

SUPER<-cbind(Mega,sound.pca$x)
looksie <- SUPER[order(SUPER[,"PC3"]),] 




myColors <- colorRampPalette(brewer.pal(8,"Set1"))(nspecies)
names(myColors) <- levels(Mega$species)
plot3d(sound.pca$x[,1:3], col=myColors[Mega$species],xlim=limitvalues,ylim=limitvalues,zlim=limitvalues,size=5)
idColors <- colorRampPalette(brewer.pal(9,"Set3"))(l.clusters)
names(idColors) <- levels(Mega$notetype)
plot3d(sound.pca$x[,1:3], col=idColors[Mega$notetype],xlim=limitvalues,ylim=limitvalues,zlim=limitvalues,size=10)



Category2<-cbind(Category1[,c(1:4,12)],sound.pca$x[,c(1:3)])

alltogether<-merge(Category2,Mega,by=c("species","selec.file","selec"))
alltogether$noteID<-factor(paste(alltogether$combo,alltogether$notetype,sep="."))

# Write all.together, labeling as last date Raven selection tables were updated
write.csv(alltogether, "alltogether_June30.csv")

## Why are there 2 sound.files columns?
# For now, change one of them
colnames(alltogether)[which(colnames(alltogether) %in% c("sound.files.x"))]<- c("sound.files")


length(unique(alltogether$species))
length(unique(alltogether$noteID))




#######################################
##### 4. Visually inspect results #####
#######################################

# First, install warbleR from github if Marcelo hasn't updated the package; last update 7/24
library(devtools)
#install_github("marce10/warbleR")
#Load Libraries for this script
library(warbleR)


# View spectrograms on a grid
#catalog(alltogether[1:10,], #subset by rows
sort1 <- alltogether[order(alltogether$noteID, alltogether$species),]    #sort by noteID, then by species
sort2 <- alltogether[order(alltogether$species, alltogether$noteID),]    #sort by species, then by noteID

# Testing coloring
# catalog(sort1[sort1$noteID == '0.0.0.0.0.4',],
#         flim = c(0, 5),
#         nrow = 10,
#         ncol = 10,
#         cex = 1,
#         leg.wd = 10,
#         same.time.scale = TRUE,
#         mar = 0.1,
#         hatching = 1,
#         spec.mar = .25,
#         wl = 512,
#         legend = 1,
#         orientation = "h",
#         #tag.pal = list(temp.colors, heat.colors),
#         tags = c("noteID"),
#         group.tag = c("species")
# )

catalog(alltogether,
        flim = c(0, 5),
        nrow = 5,
        ncol = 10,
        cex = 1,
        leg.wd = 10,
        same.time.scale = FALSE,
        mar = 0.1,
        hatching = 1,
        spec.mar = .25,
        wl = 512,
        legend = 1,
        orientation = "h",
        #tag.pal = list(temp.colors, heat.colors),
        tags = c("noteID"),
        group.tag = c("species")
)


################################################################################




# Change common names to scientific Names 

setwd("C:/Users/Rusty/Amazon Drive/BOP/tree")
source("NameFixing.R", chdir = F)


# BOPnamefixer(dataframe, 1 to use an existing column (any other value=use rownames),abc=numeric index of column containing orig species name)
rightnames<-BOPnamefixer(alltogether,COL=1,abc=1,oldnames='common') #BOPnamefixer is a function defined at the bottom that changes rownames to correct species names


setwd("C:/Users/Rusty/Amazon Drive/BOP/Video_analysis/Species")
#source("ProcessBOP_Mar27.R", chdir = F)
##############################################################################################################
IDtable<-read.csv(file="Files-and-IDs(ES)2.csv",header=TRUE,sep=",")
IDtable$File<-gsub("C:/Users/Rusty/Google Drive/BOP/Video_analysis/Species/","",IDtable$File)
IDtable$File<-gsub("_2000-01-01T000000_000.csv","",IDtable$File)
IDtable$File<-gsub("(^.+/)","",IDtable$File) #anything up to the first / in the string
IDtable[which(IDtable$File=="458124_ind1"),"File"]<-"458124"
IDtable[which(IDtable$File=="471973 Combined.csv"),"File"]<-"471973"
IDtable[which(IDtable$File=="468919_2016-07-05T120712_289.csv"),"File"]<-"468919"
IDtable[which(IDtable$File=="469250_part1"),"File"]<-"469250"
IDtable[which(IDtable$File=="469251_part1"),"File"]<-"469251"
IDtable[which(IDtable$File=="456767_ind1"),"File"]<-"456767"
IDtable[which(IDtable$File=="465571_imm"),"File"]<-"465571"
IDtable[which(IDtable$File=="465337_ind1"),"File"]<-"465337"
IDtable[which(IDtable$File=="465338_ind1"),"File"]<-"465338"
IDtable[which(IDtable$File=="465339_ind1"),"File"]<-"465339"
IDtable[which(IDtable$File=="469160_ind1"),"File"]<-"469160"
IDtable[which(IDtable$File=="469174_ind1"),"File"]<-"469174"
IDtable[which(IDtable$File=="469214_ind1"),"File"]<-"469214"


rightnames$recording<-sub(".txt", "", rightnames$recording)
rightnames$recording<-stringr::str_sub(rightnames$recording,-6,-1)


rightnames$ID<-IDtable[match(rightnames$recording,IDtable$File),"ID"]



rightnames<-rightnames[,c(2,32,3,4,1,31,5:30)]

unique(paste(rightnames$species,rightnames$ID,sep="-"))
unique(rightnames$combo)
unique(rightnames$notetype)
unique(rightnames$noteID)
unique(rightnames$recording)

rightnames$uniquebirds<-paste(rightnames$species,rightnames$ID,sep="")
unique(rightnames$uniquebirds)




setwd(dir<-"C:/Users/Rusty/Amazon Drive/BOP/SOUND")

BASE<-rightnames
save(BASE,file="CleanSounds_June30.RData")

ALLrecordingslist<-list()

for(sequence in 1:length(unique(rightnames$recording))){
  
  thisrecording<-unique(rightnames$recording)[sequence]
  thisrecording.data<-rightnames[which(rightnames$recording==thisrecording),]
  thisrecording.data<-thisrecording.data[order(thisrecording.data[,"Begin.Time..s."]),] 
  
  
  newlength<-round(max(thisrecording.data$End.Time..s.)/0.01)+2 
  
  if(newlength<1000)
    newlength<-1000 #makes a 10 second (1000 deciseconds) frame
  
  completebehaviormatrix<-matrix(data=NA,nrow=1,ncol=newlength)
  xx<-seq(0:(newlength-1))-1
  completebehaviormatrix<-data.frame(completebehaviormatrix)
  colnames(completebehaviormatrix)<-xx
  #completebehaviormatrix starts as an empty, 1 row matrix with slots for every 1/100 of time during the recording
  
  
  for (ooo in 1:nrow(thisrecording.data))  { #This loop fills the matrix with 1s for every 1/10 sec where the behavior is occuring
    
    thisone<-thisrecording.data[ooo,"noteID"]
    columnstofill<-100* (seq(round(thisrecording.data[ooo,"Begin.Time..s."],digits=2),
                             round(thisrecording.data[ooo,"End.Time..s."],digits=2),by=0.01)   )
    columnstofill<-as.character(columnstofill)
    completebehaviormatrix[1,columnstofill]<-as.character(thisone) #fills columns while behavior is active
  }
  
  completebehaviormatrix[1,which(is.na(completebehaviormatrix[1,]))] <-"nosound"
  spacematrix<-completebehaviormatrix
  #Averages PCA location for each unique NOTE TYPE

  for(www in 1:length(unique(thisrecording.data$noteID))){
    piano<-as.character(unique(thisrecording.data$noteID)[www])
    spacey<-colMeans(thisrecording.data[which(thisrecording.data$noteID==unique(thisrecording.data$noteID)[www]),c("PC1","PC2","PC3")])
    spacematrix[c(2:4),which(spacematrix[1,]==piano)]<-spacey
  }
  
  spacematrix<-spacematrix[-1,]
  spacematrix[c(1:3),which(is.na(spacematrix[1,]))]<-c(0,0,0)
  
  
  
  
  unq<-thisrecording.data$uniquebirds[1]
  bird<-thisrecording.data$ID[1]
  species<-thisrecording.data$species[1]
  recording<-thisrecording.data$recording[1]
  
  
  recordings.list<-list()
  recordings.list[[1]]<-unq
  recordings.list[[2]]<-bird
  recordings.list[[3]]<-species
  recordings.list[[4]]<-recording
  recordings.list[[5]]<-completebehaviormatrix
  recordings.list[[6]]<-spacematrix
  names(recordings.list)<-c("uniqueID","bird","species","recording","soundsequence","PCs")
  
  ALLrecordingslist[[sequence]]<-recordings.list
}
beepr::beep(4)

save(ALLrecordingslist,file="CleanSoundSeqData_PCAs_June30.RData")


summaryofvocalpatterns<-matrix(data=NA,nrow=length(unique(rightnames$uniquebirds)),ncol=1)
summaryofvocalpatterns<-data.frame(summaryofvocalpatterns)
summaryofvocalpatterns$uniquebird<-unique(rightnames$uniquebirds)
summaryofvocalpatterns[,1]<-stringr::str_sub(summaryofvocalpatterns$uniquebird,0,-2)
colnames(summaryofvocalpatterns)[1]<-"species"
  

windowsizes<-c(500,1000,1500,2000,2500,3000,3500,4000,4500,5000) # x/100 sec (e.g. 1000/100 = 10 seconds)
#Loop through all recordings
library(zoo)


for(RORY in 1:length(ALLrecordingslist)){
  
  uniquebird<-ALLrecordingslist[[RORY]][[1]]
  recording<-ALLrecordingslist[[RORY]][[4]]  
  datasequence<-t(ALLrecordingslist[[RORY]][[5]])
  
  spacesequence<-t(ALLrecordingslist[[RORY]][[6]])
  
  for(TEDDY in 1:length(windowsizes)){
    
    windowsize<-windowsizes[TEDDY]
  
    x<-rollapply(c(datasequence),width=windowsize,by=50,networksummarizer.BOPSOUND,by.column=FALSE,fill = NA, partial = FALSE,align="left")
    y<-rollapply(spacesequence,width=windowsize,by=50,PCAsummarizer.BOPSOUND,by.column=FALSE,fill = NA, partial = FALSE,align="left")
    
    
      if(length(x)==length(datasequence)){ #These lengths will only be equal if the window size is too big
        xx<-na.omit(x) #This doesn't work because super small networks (e.g. behaves =2) return NA for particular variables
      } else {
        x<-cbind(x,y)
        x<-cbind(seq(1,(nrow(x)),by=1),windowsize/100,x)
        x[,1]<-x[,1]/100
        colnames(x)[1]<-"Tstart"
        colnames(x)[2]<-"windowsize"
        xx<-x[which(rowSums(x[,-c(1:2)],na.rm=TRUE)!=0),,drop=FALSE]
        colnames(xx)[13]<-"PCAvolume"
      }
  
      if(length(xx)!=0){
        xyz.sliding<-as.matrix(xx)
        xyz.sliding<-cbind(ALLrecordingslist[[RORY]]$species,ALLrecordingslist[[RORY]]$uniqueID,
                           ALLrecordingslist[[RORY]]$bird,ALLrecordingslist[[RORY]]$recording,xyz.sliding)
        
        colnames(xyz.sliding)[c(1:4)]<-c("species","uniquebird","ID","Recording")
        
        if(windowsize>windowsizes[1]){
          CurveData<-rbind(CurveData,xyz.sliding)
        } else {
          CurveData<-xyz.sliding
        }  
      } else {
        
        empty<-matrix(rep(NA,17),nrow=1)
        colnames(empty)<-colnames(xyz.sliding)
        empty[1,1]<-ALLrecordingslist[[RORY]]$species
        empty[1,2]<-ALLrecordingslist[[RORY]]$uniqueID
        empty[1,3]<-ALLrecordingslist[[RORY]]$bird
        empty[1,4]<-ALLrecordingslist[[RORY]]$recording
        if(windowsize>windowsizes[1]){
          CurveData<-rbind(CurveData,empty)
        } else {
          CurveData<-empty
        }  
      } 
  }
  print(paste(RORY," out of ", length(ALLrecordingslist),sep=""))
    if(RORY==1){
      AllWindowsCurves<-CurveData
    } else {
      AllWindowsCurves<-rbind(AllWindowsCurves,CurveData)
    }
}




beepr::beep(4)

bk<-AllWindowsCurves

AllWindowsCurves<-data.frame(AllWindowsCurves)
AllWindowsCurves[,c(5:17)]<-lapply(AllWindowsCurves[,c(5:17)], function(x) as.numeric(as.character(x)))

looksie.pca <- AllWindowsCurves[which(AllWindowsCurves[,"PCAvolume"]>(quantile(AllWindowsCurves[,"PCAvolume"],0.95,na.rm=TRUE))),]
looksie.pca <-looksie.pca[order(looksie.pca$PCAvolume),]
looksie.pca <-AllWindowsCurves[which(AllWindowsCurves[,"windowsize"]==10),]
looksie.pca <-looksie.pca[order(looksie.pca$PCAvolume),]
looksie.TS <- AllWindowsCurves[which(AllWindowsCurves[,"trueShannon"]>(quantile(AllWindowsCurves[,"trueShannon"],0.999,na.rm=TRUE))),]
looksie.TS<-looksie.TS[order(looksie.TS$trueShannon),]



AllWindowsCurves$SongComplexity<-AllWindowsCurves$uniquebehaviors*AllWindowsCurves$uniquetransitions
AllWindowsCurves$trueSongComplex<-AllWindowsCurves$trueShannon*AllWindowsCurves$trueShannon.T
save(AllWindowsCurves,file="AcousticCurveData_June30.RData")



load("AcousticCurveData_June30.RData")


for(xj in 1:2){
  if(xj==1)
    TIMEWINDOW<-10
  
  if(xj==2)
    TIMEWINDOW<-50
  
  JustRightTime<-subset(AllWindowsCurves,windowsize==TIMEWINDOW)
  #This loop subsets the data (which is already time-specific by window size (TIMEWINDOW))
  # by each bird, and keeps the maximally complex (defined by 'keeper' line) data for that individual
  for(ty in 1:length(unique(JustRightTime$uniquebird))){
    disbird<-JustRightTime[which(JustRightTime$uniquebird==unique(JustRightTime$uniquebird)[ty]),]
    keeper<-disbird[which.max(disbird$SongComplexity),]
    if(ty>1){
      indmaxes<-rbind(indmaxes,keeper)
    } else {
      indmaxes<-keeper
    }
  }
  if(TIMEWINDOW==50)
  FiftySong<-indmaxes
  
  if(TIMEWINDOW==10)
  TenSong<-indmaxes
 
}
ACOUSTIC<-list(TenSong,FiftySong)
save(ACOUSTIC,file="ACOUSTIC_clean_June30.RData")

indmaxes<-TenSong

indmaxes<-indmaxes[order(indmaxes$SongComplexity),]

###################################################################################################################

#Makes PDF plot of accumulation curves for each IND/for each species (for SDC.true.x)
colnames(AllWindowsCurves)
metricstocompare<-c("uniquebehaviors", "uniquetransitions", "trueShannon", "trueShannon.T", "PCAvolume", "SongComplexity", "trueSongComplex")


pdf("SONGcurves_allsamescale_June30.pdf",width= 30, height= 24,family="NimbusRom")
par(mfrow=c(5,6))# rows, columns
par(mar=c(6,5,0,1)) #Margines of each plot (bottom, left, top, right)
par(oma=c(0,0,10,0)) #Outer Margins of "entire" plot

  
for(varinquestion in 1:length(metricstocompare)){
  complexityvariable<-metricstocompare[varinquestion]
  for(abc in 1:(length(unique(AllWindowsCurves$species)))){ 
    sp<-unique(AllWindowsCurves[,1])[abc]
    
    if(abc>31){
      plot.new()
    } else {
    
      
      
    currentspecies<-AllWindowsCurves[which(AllWindowsCurves$species==sp),]
    currentbehav<-currentspecies[,c("ID","windowsize",complexityvariable)]
    #currentbehav[c(2:3)]<-lapply(currentbehav[c(2:3)], function(x) as.numeric(as.character(x)))
    #currentbehav <- currentbehav[is.finite(currentbehav[,efg]),] #gets rid of rows with Inf/-Inf values
    currentbehav<-na.omit(currentbehav)
    maxbehavvalue<-max(AllWindowsCurves[,complexityvariable],na.rm=TRUE)
    par(new=FALSE)
    
    for(def in 1:length(unique(currentbehav$ID))){
      chacha<-unique(currentbehav$ID)[def]
      currentindividual<-currentbehav[which(currentbehav$ID==chacha),]
      empties<-currentindividual[1,]
      empties[,c(2:3)]<-c(0,0)
      
      for(ghi in 1:length(windowsizes)){
        yuba<-currentindividual[which(currentindividual[,2]==(windowsizes[ghi]/100)),]
        yuba<-yuba[which.max(yuba[,3]),]
        if(ghi==1){
          ci<-yuba
        } else {
          ci<-rbind(ci,yuba)
        }
      }
      
      
      currentindividual<-rbind(empties,ci)
      
      individualnumber<-as.numeric(as.character(currentindividual$ID[1]))
      Greencolors<-rev(colorRampPalette(brewer.pal(9,"Greens"))(10))
      Greencolor<-Greencolors[individualnumber]
      
      Bluecolors<-rev(colorRampPalette(brewer.pal(9,"Blues"))(10))
      Bluecolor<-Bluecolors[individualnumber]
      
      Redcolors<-rev(colorRampPalette(brewer.pal(9,"Reds"))(10))
      Redcolor<-Redcolors[individualnumber]
      
      if(def==1){
        xlab2="window"
        ylab2=currentspecies[1,1]
        
        
      } else {
        par(new=TRUE)
        xlab2=NA
        ylab2=NA
        
      }
      
      
      formula.plot <- as.formula(paste (complexityvariable, "~ SSasymp(windowsize, Asym, R0, lrc)",sep=""))
      out <- tryCatch(nls( formula.plot , data = currentindividual),error = function(e) { cat('In error handler\n'); print(e); e })#function checks if nls model returns error
      print(paste(as.character(currentspecies$species[1]),currentindividual$ID[1],sep=" "))
      
      #Only plot if there are at least 2 time windows to use (100 & 200)
      if(length(c(na.omit(unique(currentindividual$windowsize))))>2){
        if(any(class(out) == "error")){  
          use.Xs<-currentindividual[,2]
          use.Ys<-currentindividual[,3]
          line.Xs<-currentindividual[,2]
          line.Ys<-currentindividual[,3]
          relevantcolor<-Bluecolor
          dottype<-1
          linetype<-2
          
        } else {
          
          fm<-nls( formula.plot , data = currentindividual)
          predictedYs<-predict(fm)
          use.Xs<-currentindividual[,2]
          use.Ys<-currentindividual[,3]
          line.Xs<-currentindividual[,2]
          line.Ys<-predictedYs
          relevantcolor<-Greencolor
          linetype<-1
          dottype<-20
        }  
        
        
        
      }
      if(length(c(na.omit(unique(currentindividual$windowsize))))==2){
        use.Xs<-currentindividual[,2]
        use.Ys<-currentindividual[,3]
        line.Xs<-currentindividual[,2]
        line.Ys<-currentindividual[,3]
        relevantcolor<-Redcolor
        linetype<-2
        dottype<-0
      }
      
      if(def>1)
        par(new=TRUE)
      plotted<-{plot(use.Xs,use.Ys,xlim=c(0,50),ylim=c(0,maxbehavvalue),pch=dottype,xlab=xlab2,ylab=ylab2,col=relevantcolor,cex.axis = 1.5, cex.lab = 2); lines(line.Xs,line.Ys,lty=linetype,lwd=2,col=relevantcolor,cex.axis = 1.5, cex.lab = 2)}
    }
    
    }
  }
  mtext(complexityvariable, outer = TRUE,side = 3, cex = 4, line = 1) #Adds species' name to top of plot
}



dev.off()










library(igraph)
library(vegan)
library(geometry)
#Uses "sequence of sounds to generate summary stats of acoustic complexity, for use in sliding window analysis
networksummarizer.BOPSOUND<-function(z){
  summarytable<-table(z)
  proportiontable<-prop.table(summarytable)
  uniquebehaviors<-length(summarytable)
  
  #TRUE SHANNON DIVERSITY
  dftable<-t(as.data.frame(summarytable))
  colnames(dftable)<-dftable[1,]
  dftable<-dftable[-1,,drop=FALSE]
  rownames(dftable) <- c()
  dftable<-data.frame(dftable)
  indx<-c(1:uniquebehaviors)
  dftable[indx]<- lapply(dftable[indx], function(x) as.numeric(as.character(x)))
  Shannon<-vegan::diversity(dftable,index = "shannon")
  trueShannon<-exp(Shannon)
  
  
  #TRANSITIONSSSSSSSS  
  ##########################################################################################################
  c.no.OFFs<-as.character(z)
  
  #uses "createSequenceMatrix" function (from *markovchain*) to calculate transition matrix
  TransitionMatrix.cum<-markovchain::createSequenceMatrix(c.no.OFFs,sanitize=FALSE)
  
  
  noselfs<-TransitionMatrix.cum
  diag(noselfs)<-NA
  
  prop.table.excludeNAs<- function(x) {x/sum(x, na.rm=TRUE)}
  proportiontable.trans.Simpson<-prop.table.excludeNAs(noselfs) #get rid of diagonal/self-transitions for Simpson Analyses
  
  proportiontable.trans<-prop.table.excludeNAs(TransitionMatrix.cum) 
  
  ###########TRUE SHANNON DIVERSITY FOR TRANSITIONS
  if(uniquebehaviors>1){
    val3.s<-matrix(nrow=(((uniquebehaviors^2)-uniquebehaviors)/2),ncol=2)
    rownum<-1
    for (u in 1:uniquebehaviors){
      for(v in u:uniquebehaviors){#iteratively reduces columns analyzed in next loop to avoid double counting transitions 
        if (u!=v){
          val3.s[rownum,2]<-as.numeric(TransitionMatrix.cum[u,v]+TransitionMatrix.cum[v,u])
          val3.s[rownum,1]<-paste(row.names(TransitionMatrix.cum)[u],colnames(TransitionMatrix.cum)[v],sep="-")
          rownum<-rownum+1
        }
      }
    }
    val4.s<-as.data.frame(val3.s)
    val4.s[,2]<-as.numeric(as.character(val4.s[,2])) 
    
    dftable.T<-t(val4.s)
    colnames(dftable.T)<-dftable.T[1,]
    dftable.T<-dftable.T[-1,,drop=FALSE]
    rownames(dftable.T) <- c()
    dftable.T<-data.frame(dftable.T)
    indx<-c(1:length(dftable.T))
    dftable.T[indx]<- lapply(dftable.T[indx], function(x) as.numeric(as.character(x)))
    Shannon.T<-vegan::diversity(dftable.T,index = "shannon")
    trueShannon.T<-exp(Shannon.T)
    # propShannon.T<-Shannon.T/log(specnumber(dftable.T))
    # redundancy.T<-1-propShannon.T
    uniquetransitions<-length(val3.s)
    
  } else {
    trueShannon.T<-NA
    propShannon.T<-NA
    redundancy.T<-NA
    uniquetransitions<-1
  }
  
  
  #########################################################
  # NETWORK SUMMARY VARIABLES
  showme.looped<- graph_from_adjacency_matrix(proportiontable.trans,mode="directed",weighted=TRUE,diag=TRUE,add.colnames=TRUE)
  
  #EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
  #6.1 Density
  
  #The proportion of present edges from all possible edges in the network.
  
  #  densityscore<-ecount(showme.looped)/(vcount(showme.looped)*(vcount(showme.looped)-1)) #for a directed network
  densityscore<-ecount(showme.looped)/(vcount(showme.looped)^2)#for a directed network WITH self-loops
  
  
  #Mean degree
  deg <- degree(showme.looped,loops=FALSE)
  mean.degree<-mean(deg)
  
  #Mean path length
  mean_path_length<-mean_distance(showme.looped, directed=T)
  
  #network diameter is the longest geodesic distance (length of the shortest path between two nodes) in the network.
  network.diameter<-diameter(showme.looped, directed=T,weights=NA)
  
  #average clustering coefficient
  avg.cluster.coeff<-transitivity(showme.looped)
  
  ############################################
  #SMALL WORLDNESS --- video-wise, no filter
  #number of nodes/vertices in graph
  vertices<- uniquebehaviors
  #number of edges in G(n,m) graph
  edges<- sum(TransitionMatrix.cum!=0)
  
  rando.network<-sample_gnm(vertices, edges, directed = TRUE, loops = TRUE)
  
  Trobserved<-avg.cluster.coeff
  mean.Trrandom<-transitivity(rando.network)
  
  SPobserved<-mean_path_length
  mean.SPrandom<-mean_distance(rando.network, directed=T)  
  
  Smallworldness<- (Trobserved/mean.Trrandom)/(SPobserved/mean.SPrandom)
  ############################################
  
  printme<-data.frame(uniquebehaviors,uniquetransitions,
                      trueShannon,
                      trueShannon.T, 
                      densityscore,mean.degree,mean_path_length,network.diameter,avg.cluster.coeff,
                      Smallworldness)
  
  
  #CONNECTIVITY measures reflect degree to which network differs from complete network
  #Edge density: % of edges compared to maximum
  #Average degree: Avg. number of links
  #Average path length: avg of shortest pathes between reachable nodes
  #Network diameter: longest of shortest paths
  
  #CENTRALITY measures quantify heterogeneity in network structure
  #Average clustering coefficient:
  #Components
  
  
  
  return(printme)
  
}

#uses sequence of PCA locations to generate sound space occupancy, for use in sliding window analysis
PCAsummarizer.BOPSOUND<-function(zz){
  
  zz<-apply(zz,2,function(x) as.numeric(x))

  #if all points are zero, return 0 as volume
  if(sum(colSums(zz))==0){
    dis<-0
  } else {
    dis <- tryCatch({convhulln(zz, "FA")$vol},
                  warning = function(w) {0},error = function(e) { 0})#function checks if convhull returns error
  }
return(dis)
}
