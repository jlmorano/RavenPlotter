# Purpose: Birds-of-paradise sound analysis project-- 
#       1. Data-check Raven selection tables
#       2. PCA cluster analysis of sounds to determine call types 


#Run on Janelle's laptop
setwd(dir<-"/Users/jlm394/Desktop/R_working/BoP")

directories<-list.dirs("/Users/jlm394/Desktop/R_working/BoP/Completed")[-1] #Gets directories (not parent directory)
species.names<-list.dirs("/Users/jlm394/Desktop/R_working/BoP/Completed",full.names=FALSE)[-1]

#Run on Rusty's
#setwd(dir<-"C:/Users/Rusty/Amazon Drive/BOP/SOUND")

#directories<-list.dirs("C:/Users/Rusty/Amazon Drive/BOP/Sound/Janelle")[-1] #Gets directories (not parent directory)
#species.names<-list.dirs("C:/Users/Rusty/Amazon Drive/BOP/Sound/Janelle",full.names=FALSE)[-1]


mega<-c()
naindex<-0  #N of files for which special data assignements are missing
naughtyindex<-0 #N of files for which special data assignements are not 0/1
checkindex<-c("Freq_Modulation","Harmonics","Non.Harmonic_Struc","Impulsive","Stochastic")
#Loops through all files and reads them into COMPOSITE Dataframe "Mega", omitting (and identifying)
# elements that are missing/wrong values for the 5 binary, human-classified variables
for(xx in 1:length(species.names)){ #Loop through each folder
  
  file.names<-list.files(directories[xx],full.names=TRUE,pattern=".txt")
  print(species.names[xx])
  
  for(yy in 1:length(file.names)){ #Loop through text files for each species
    
    
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
  
    table$recording<-sub("/Users/jlm394/Desktop/R_working/BoP", "", (file.names[yy]))
    #sub("/Users/jlm394/Desktop/R_working/BoP", "", substring(as.character(file.names[yy]),61))
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
                       
                       "Freq_Modulation","Harmonics","Non.Harmonic_Struc","Impulsive","Stochastic")]
    
    
    
    
    
    if (xx==1 & yy==1) {
      Mega<-subtable
    } else {
      Mega<-rbind(Mega,subtable)
    }


  
  }
}


#print(NaErrors)
print(NaughtyErrors) #Prints full df of errors with identifying info



#######################################################






PCA STUFF BELOW











#######################################################
Mega$species<-factor(Mega$species)
nspecies<-length(unique(Mega$species))
#Create a custom color scale
library(RColorBrewer)
myColors <- colorRampPalette(brewer.pal(9,"Set3"))(50)
names(myColors) <- levels(Mega$species)

info<-colnames(Mega)[1:5]
Category1<-Mega[,c(info,checkindex)]
Category1$combo<-paste(Category1[,checkindex[1]],Category1[,checkindex[2]],Category1[,checkindex[3]],Category1[,checkindex[4]],Category1[,checkindex[5]],sep=".")
Category1$combo<-factor(Category1$combo)

rawData<-Mega[,-c(1:5)]
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
summary(sound.pca)
diff<-apply(sound.pca$x,2,function(x) range(x)[2]-range(x)[1])
biplot(sound.pca)

head(sound.pca$x)
summary(sound.pca$x)



limitvalues<-c(min(sound.pca$x),max(sound.pca$x))
plot3d(sound.pca$x[,c(1:3)], col=myColors[Mega$species],xlim=limitvalues,ylim=limitvalues,zlim=limitvalues)











#Uses agglomerative hierarchical clustering with a cut-off of 5% variation to cluster points in colorspace 
dS22<-sound.pca$x[,1:3]

dist.matrix<-dist(dS22)
hc <-fastcluster::hclust(dist.matrix, method="ward.D2", members=NULL) #Expects squared distance values
plot(hc)




#DEFINE CLUSTERING THRESHOLD BASED ON % VARIATION DIFFERENCE YOU WANT BETWEEN CLUSTERED GROUPS
####################
s1<-summary(sound.pca)$importance
xyz<-0.25
divisor<-s1[2,1]/xyz 
diff<-apply(sound.pca$x,2,function(x) range(x)[2]-range(x)[1])
cutmark<-diff[1]/divisor
#Set xyz=% of variation you want as your clustering distance 
#(e.g. 0.05 = corresonds to 10% of variation explained by all PCs)


clusterid<-cutree(hc, h = cutmark) 
clusters<-as.vector(unique(clusterid))
l.clusters<-length(clusters[clusters !=0])

Mega$notetype<- factor(clusterid) #



myColors <- colorRampPalette(brewer.pal(8,"Set1"))(nspecies)
names(myColors) <- levels(Mega$species)
plot3d(sound.pca$x[,1:3], col=myColors[Mega$species],xlim=limitvalues,ylim=limitvalues,zlim=limitvalues,size=5)
idColors <- colorRampPalette(brewer.pal(9,"Set3"))(l.clusters)
names(idColors) <- levels(Mega$notetype)
plot3d(sound.pca$x[,1:3], col=idColors[Mega$notetype],xlim=limitvalues,ylim=limitvalues,zlim=limitvalues,size=10)



alltogether<-merge(Category1[,c(1:3,11)],Mega,by=c("species","recording","Selection"))
alltogether$noteID<-factor(paste(alltogether$combo,alltogether$notetype,sep="."))

