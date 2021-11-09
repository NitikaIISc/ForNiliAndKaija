## this function is called by the main one ReadMoveBankDataFormat
## it loops on all time groups
## for each one it updates the numbers of co-occuring vultures, including self 
## also the updates the counters of simulatnounsly tracked vultures 
## note that each dyad, including self is counted twice A--A and B--A or if self A--A and A--A
#takes ~3.5 hours
createDirectedMatrices<-function(Dataset, DistThreshold){
  
  
ColumToSelect=c("ID","coords.x2","coords.x1","Easting","Northing","timegroup","group")
#Nitika - this time group has multiple non-self overlap rows; 
#timgrpind=6 #(=timegroup)
#subset(Dataset,timegroup == 6)
#101488 - group

for (timgrpind in 1: max(Dataset$timegroup)){#loop on all time groups ## timegroup comes from analysis
  ## extract current time group (#18458 has a good example)
  #subset(Dataset, timegroup==timgrpind,select=c("ID","coords.x2","coords.x1"))
  timegroupDF=subset(Dataset, timegroup==timgrpind,select=ColumToSelect)
  #print(timegroupDF[order(timegroupDF$group),])#,'timegroupDF$ID'#plot it
  
  ## working within this time group: dyads and distances: now dont need timegroup but 'group' (spatiotemporal) 
  timegroupDF=subset(timegroupDF, timegroup==timgrpind,select=c("ID","coords.x2","coords.x1","group")) #subset columns of interest further
 
##these vultures were observed around the same time on the same day
  
  DT <- expand.grid.df(timegroupDF,timegroupDF);  #Nitika - entire column written twice laterally (next to each other)
  #Each vulture with all others so that their locations can be compared to see who was close to each other
  #nrow(DT)
  #head(DT)
  names(DT)[5:7] <- c('ID2',"lat_secondInd","long_secondInd") 
  ##making a template: ID lat long  group (spatial)   ID2 lat2 long2  group2 (spatial)
  
  
  
  #?expand.grid.df = Creates new data frame containing all combination of rows from data.frames in 
  #distances:
  setDT(DT)[ , dist_km := distGeo(matrix(c(coords.x1, coords.x2), ncol = 2), 
                                  matrix(c(long_secondInd, lat_secondInd), ncol = 2))/1000];
  
  
  #distance between all dyads
 # ?distGeo  = Vector of distances in meters (which is why divided by 1000)
  #?setDT = instead of as.data.table because big lists/dataframes make copies first and then perform a function
  #         and can thus take a long time and memory
  
  #dist_km find the shortest distance between two points i.e. latlong of ID and ID2 so those with 
  # self would obviously give dist_km=0 
  # ?distGeo = Highly accurate estimate of the shortest distance between two points on an ellipsoid (default is WGS84 ellipsoid). The shortest path between two points on an ellipsoid is called the geodesic.
  ## create all possible dyads of vultures in a long format:
  PresentVultures=subset(DT, dist_km==0&as.character(ID)==as.character(ID2),select=c(1,5))
  ##Identify all self-association dyads with zero inter-location distance and same IDs
  #these are the vultures present in this time group - selecting only ID and ID2 i.e. columns 1 & 5
  PresentVultures=as.data.frame(unique(PresentVultures$ID))#PresentVultures=unique(PresentVultures$ID);
  #all self-associating IDs
  PresentVultures=expand.grid.df(PresentVultures,PresentVultures,unique=F)
  #transform back to rows of ID1=ID2
  
  #these are the dyads that has concurrent time point
  names(PresentVultures)=c('ID','ID2') #these are all the vultures whose location was recorded around the same time including self
  
  #expand.grid.df(PresentVultures,PresentVultures,unique=F)
  #               ID      ID2       ID      ID2
  #            Y69.T13W Y69.T13W Y69.T13W Y69.T13W
  
  ## loop on current dyads (including self) to update co-occurances:
  #dyadcnt=2 - checking with second row
  for (dyadcnt in 1: dim(PresentVultures)[1]){#length just half since each dyad is counted twice AB and BA
    Dyadind=which(SimlDataPntCnt$ID==PresentVultures$ID[dyadcnt]&SimlDataPntCnt$ID2==PresentVultures$ID2[dyadcnt])
    #In above line, just identifying which row in the empty SimlDataPntCnt ID1 and ID2 dyads are the same as PresentVulture for one timgroup at a time 
    
    
    #identified all rows of self AND non-self association in same timegroup and 0 distance in SimlDataPntCnt
    ##gives which row number in SimlDataPntCnt has the same dyad
    #Nitika - Identified all dyads from PresentVultures by subsetting all rows that had dist_km=0 and self-association
    
    SimlDataPntCnt$counter[Dyadind]=SimlDataPntCnt$counter[Dyadind]+1; 
    
    #Dyadind is the row number with that dyad
    #adding one to the frequency such that this dyad could've hung out close to each other because they were around at the same time
    
   
    #add another (1) tallymark to counter in front of the dyad every time the pair co-occurs in time 
  }#for loop on current dyads 
  
  #if(CountTwice){
  ## since self dyads appear only once: #Nitika - they should also be counted twice like ab & ba
  SelfDyad=which(PresentVultures$ID==PresentVultures$ID2);#since self dyads appear only once, another loop on them, so diag will be counted twice like the rest
  
  for (dyadcnt in (SelfDyad)){#count dyad twice? A-B and B-A
    Dyadind=which(SimlDataPntCnt$ID==PresentVultures$ID[dyadcnt]&SimlDataPntCnt$ID2==PresentVultures$ID2[dyadcnt])
  ##Nitika::avoiding counting self dyads twice because we aren't keeping directed edges for non-self  
    
    #SimlDataPntCnt$counter[Dyadind]=SimlDataPntCnt$counter[Dyadind]+1;#
  
    
    }#count dyad twice? A-B and B-A
  #so the diagonal, self is counted twice since its A-A and A-A later. others are A-B and B-A, 
  
  ## now setting interacting dyads
  #if (dim(timegroupDF)[1]>=1){#more than one vulture in this time group?
  InteractingSelf=subset(DT, dist_km==0 & (as.character(ID)==as.character(ID2))) # just including self interactions, once, not multiple times
  InteractingSelf=InteractingSelf[!duplicated(InteractingSelf$ID),]# just including self interactions, once, not multiple times
  InteractingDyads=subset(DT,(dist_km<=DistThreshold/1000 & (as.character(ID)!=as.character(ID2)))) # not including self interactions
  #only here, in the Interacting dyads did we cheack if a dyad was spatially proximate
  
  #subset data table such that non self-overlapping IDs as well as within a certain distance from each other
  InteractingDyads=InteractingDyads[!duplicated(InteractingDyads[,c("ID","ID2")])];
  
  if(dim(PresentVultures)[1]<dim(InteractingDyads)[1])      {
    break
  }#for debugging
  
  #InteractingDyads=rbind(InteractingDyads,InteractingSelf,InteractingSelf)#counting the self twice to keep it like the twice of the non-self dyads
  
  ##
  InteractingDyads=rbind(InteractingDyads,InteractingSelf)
  #rbind non-self overlapping rows with self-overlapping rows
  
  ## a loop on interacting dyads, non-self, for updating the CoocurCountr storge
  for (dyadcnt in 1: dim(InteractingDyads)[1]){#
    Dyadind=which(CoocurCountr$ID==InteractingDyads$ID[dyadcnt]&CoocurCountr$ID2==InteractingDyads$ID2[dyadcnt])
    #Identifying in the empty dataframe where SPATIO-temporal proximity is recorded, which dyads are in which rows
    #Spatial proximity between dyads was calculated after SimlDataPntCnt (temporal overlap) in the step where self and non-self (<2km distthreshold)
    #was calculated as InteractingDyads
    CoocurCountr$counter[Dyadind]=CoocurCountr$counter[Dyadind]+1;#
  }#loop 
  
  # }#if making sure Ids match
  
  ## debugging:
  #print(InteractingDyads[order(InteractingDyads$group,InteractingDyads$ID),])#,'timegroupDF$ID'#plot it
  #conditions for debugging:
  #(dim(InteractingSelf)[1]*2<dim(InteractingDyads)[1])
  #if(length(unique(PresentVultures$ID))>1)
  #if(dim(InteractingSelf)[1]*2<dim(InteractingDyads)[1]){#read dyads interact
  #  print(timgrpind)
  #  print('timgrpind')
  #}
  
  rm(list=c("DT","InteractingDyads","InteractingSelf","SelfDyad","Dyadind","dyadcnt","timegroupDF"))
  
}#loop on time groups
return(list(SimlDataPntCnt,CoocurCountr))
}
