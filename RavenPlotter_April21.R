













setwd(dir<-"C:/Users/Rusty/Amazon Drive/BOP/SOUND")


directories<-list.dirs("C:/Users/Rusty/Amazon Drive/BOP/Sound/Janelle")[-1] #Gets directories (not parent directory)
species.names<-list.dirs("C:/Users/Rusty/Amazon Drive/BOP/Sound/Janelle",full.names=FALSE)[-1]


mega<-c()
naindex<-0  #N of files for which special data assignements are missing
naughtyindex<-0 #N of files for which special data assignements are not 0/1

#Loops through all files and reads them into COMPOSITE Dataframe "Mega", omitting (and identifying)
# elements that are missing/wrong values for the 5 binary, human-classified variables
for(xx in 1:length(species.names)){ #Loop through each folder
  
  file.names<-list.files(directories[xx],full.names=TRUE,pattern=".txt")
  
  for(yy in 1:length(file.names)){ #Loop through text files for each species
    
    
    #read in table, and provide specific classes for specific columns
    table<-read.table(file.names[yy],sep="\t",header=T,row.names=NULL,colClasses=c("Freq_Modulation"="factor","Harmonics"="factor","Non.Harmonic_Struc"="factor","Impulsive"="factor","Stochastic"="factor"))
    #read.csv('test.csv', colClasses=c("time"="character"))
  #This line condenses the raw output of Raven from the Spectrogram and Wavelength windows into one table
#  data.frame(table[seq(1,nrow(table),2),c(grep("Selection",names(table)),grep("Begin",names(table)),grep("End",names(table)),grep("Delta",names(table)),grep("Low",names(table)),grep("High",names(table)),grep("Delta.Freq",names(table)),grep("File",names(table)),grep("Note",names(table)),grep("Song",names(table)),grep("Quality",names(table)))],na.omit(table$Peak.Freq..Hz.),na.omit(table[,grep("RMS",colnames(table))]))->table

  
#     data.frame(table[seq(2,nrow(table),2),c(grep("Selection",names(table)),grep("Begin",names(table)),grep("End",names(table)),
#                                           grep("BW.90...Hz.",names(table)),grep("Freq.5...Hz.",names(table)),grep("Freq.95...Hz.",names(table)),
#                                           grep("Time.5...s.",names(table)),grep("Time.95...s.",names(table)),grep("Dur.90...s.",names(table)),
#                                           grep("File",names(table)),grep("Note",names(table)),
#                                           grep("Song",names(table)),grep("Quality",names(table)))],na.omit(table$Peak.Freq..Hz.),
#                                           na.omit(table[,grep("RMS",colnames(table))]))->table
  
    table$recording<-sub("C:/Users/Rusty/Amazon Drive/BOP/Sound/Janelle/", "", (file.names[yy]))
    #sub("C:/Users/Rusty/Amazon Drive/BOP/Sound/Janelle/", "", substring(as.character(file.names[yy]),61))
    table$species<-species.names[xx]
    chuck<-ncol(table)
    table<-table[,c(chuck,chuck-1,1:(chuck-2))]
    
    
    
    checkindex<-c("Freq_Modulation","Harmonics","Non.Harmonic_Struc","Impulsive","Stochastic")
    
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
    
    
    subtable<-table[,c("species","recording","Selection",
                       
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
Mega$species<-factor(Mega$species)
#Create a custom color scale
library(RColorBrewer)
myColors <- brewer.pal(9,"Set1")
names(myColors) <- levels(Mega$species)

rawData<-Mega[,-c(1:3)]
rawData[checkindex] <- lapply(rawData[checkindex], function(x) as.numeric(as.character(x))) #uses checkindex 
summary(rawData)


s.pca<-princomp(rawData)
summary(s.pca)
s.pca$loadings
#summary(s.pca$scores)
plot(s.pca)
print(s.pca)

head(s.pca$X)

library(rgl)
plot3d(s.pca$scores[,1:3], col=myColors[Mega$species])



# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 

sound.pca <- prcomp(rawData,center = TRUE,scale. = TRUE) 
print(sound.pca)
plot(sound.pca)
summary(sound.pca)

biplot(sound.pca)

head(sound.pca$x)
summary(sound.pca$x)

diff<-apply(sound.pca$x,2,function(x) range(x)[2]-range(x)[1])
diff<-diff[-20]
asds<-apply(sound.pca$x,2,function(x) sd(x))
asds<-asds[-20]

PropVar<-summary(sound.pca)$importance[2,]
PropVar<-PropVar[-20]

plot(diff~PropVar)

plot(asds~PropVar)

plot((asds)~log(PropVar))

plot(log(asds)~(PropVar))

plot(log(asds)~log(PropVar))

summary(lm(log(asds)~log(PropVar)))











#Uses agglomerative hierarchical clustering with a cut-off of 3 JNDs to cluster points in colorspace 
dS22<-s.pca$scores[,1:3]

dist.matrix<-dist(dS22)
hc <-fastcluster::hclust(dist.matrix, method="ward.D2", members=NULL) #Expects squared distance values
clusterid<-cutree(hc, h = 15000) 
clusters<-as.vector(unique(clusterid))
l.clusters<-length(clusters[clusters !=0])

clusterid2<-cutree(hc, h = 25000) 
clusters2<-as.vector(unique(clusterid2))
l.clusters2<-length(clusters2[clusters2 !=0])

Mega$notetype<- clusterid2 #

plot3d(s.pca$scores[,1:3], col=myColors[Mega$species])
plot3d(s.pca$scores[,1:3], col=myColors[Mega$notetype])

cutree.h <- function(tree,h) {
  # this line adapted from cutree(...) code
  k <- nrow(tree$merge) + 2L - apply(outer(c(hc$height, Inf), h, ">"), 2, which.max)
  return(cutree(tree,k=k))
}


