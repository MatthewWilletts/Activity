#Script to carry out 'leave one out' RF and HMM analysis on lots of data

#with various approaches to balanced classes!!!


#load packages
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterPackages.R')
library(plyr)
library(caret)
library(HMM)

#In this script we will be doing 'leave one out' analysis for our participants


source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterFunctions.R')
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterHmmFunctions.R')


#Take in arguements from command line
#This script will calculate carry out HMM analysis using k space data

#parse inputs
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterInputs.R')

#Read in settings

#we are trying 4 variations, thus we have 2^4 options. thus we can code this in as taking a number from 0 to 15 in binary

  
  
  ncores<-16
  
  set.seed(ncores)
  
  registerDoMC(ncores)
  
  #define data directories
  source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterDirectories.R')
  
  
  settings<-number2binary(input_number,4)
  
  
  DataDir<-'/data/dph-ukbaccworkgroup/magd4534/Leave_one_out'
  
  RFoutput<-file.path(DataDir,'RFs',paste(settings,collapse = ''))
  HMMoutput<-file.path(DataDir,'HMMs',paste(settings,collapse = ''))
  
  ResultsOutput<-file.path(DataDir,'Results',paste(settings,collapse = ''))
  
  
  AllData<-fread(input = file.path('/data/dph-ukbaccworkgroup/datasets/capture-24/accelerometer/curated/free-30sec-sleep-more.csv'))
  #AllData<-fread(input = '/Users/Matthew/Downloads/free-30sec-sleep-more.csv')
  
  
  #Delete unnecessary columns from data set
  
  
  AllData$time_in_hours<-as.numeric(difftime(as.POSIXct(AllData$Time),as.POSIXct(trunc(as.POSIXct(AllData$Time), units="days")),unit='hours'))
    
  AllData$identifier<-AllData$userId
  
  AllData<-AllData[which(AllData$sexStr!='0'),]
  
  AllData$behavior<-AllData$label
  
  AllData[, c("V1","Unnamed: 0",'annotation',
              'ID','idx','dataErrors',
              'clipsAfterCalibr','clipsBeforeCalibr',
              'samples','rawSamples','woreAcc',
              'userId','label','sex','day'):=NULL]
  
  #Change char columns to factors
  AllData$collectionDay<-as.factor(AllData$collectionDay)
  AllData$sexStr<-as.factor(AllData$sexStr)
  
  participants<-unique(AllData$identifier)
  
  leave_out<-chunkID
  
  participant<-participants[leave_out]
  
  #Reduce labels
  
  AllData<-reduceLabels(AllData,list(c('sitting','standing'),c('sports','manual-work','household-chores')),c('sitstand','mixed-activity'))
  
  iq<-which(AllData$identifier==participant)
  


  #settings[1] is whether or not we use the hour of day and sage sex info
  
  #settings[2] is whether or not we use xyz fft features
  
  #settings[3] is whether or not we use fft features
  
  #settings[4] is how we split our training and test:
  #0 if normal leave one out
  #1 if we subdivide our training set so we can use our trained RF to create our emission model 
  
  #settings[5] is whether or not we OVA analysis
  
  
  #settings[6] is whether or not we use our number of trees prop to number of inputs
  
  
  #Do settings[1] - true has features, false deletes features
  
  if(settings[1]==FALSE){
    AllData[, c("collectionDay","sexStr",'age',
                'time_in_hours'):=NULL]
    
  }
  
  #Do settings[2] - true has features, false deletes features
  
  if(settings[2]==FALSE){
    
    AllData[, c("xfft0","xfft1",'xfft2',
                "xfft3","xfft4","xfft5",
                "xfft6","xfft7",'xfft8',
                "xfft9","xfft10","xfft11",
                "xfft12","xfft13",'xfft14',
                "yfft0","yfft1",'yfft2',
                "yfft3","yfft4","yfft5",
                "yfft6","yfft7",'yfft8',
                "yfft9","yfft10","yfft11",
                "yfft12","yfft13",'yfft14',
                "zfft0","zfft1",'zfft2',
                "zfft3","zfft4","zfft5",
                "zfft6","zfft7",'zfft8',
                "zfft9","zfft10","zfft11",
                "zfft12","zfft13",'zfft14'):=NULL]
  }
  
  #Do settings[3] - true has features, false deletes features
  
  if(settings[3]==FALSE){
    
    AllData[, c("fft0","fft1",'fft2',
                "fft3","fft4","fft5",
                "fft6","fft7",'fft8',
                "fft9","fft10","fft11",
                "fft12","fft13",'fft14'):=NULL]
  }
  
  
  train_set <- AllData[-iq,]
  test_set <- AllData[iq,]
  test_behavior<-as.factor(AllData$behavior)
  test_behavior<-test_behavior[iq]

  
  
  #Do settings[4] - true makes two different training sets, false has one training set
  
if(settings[4]==TRUE){
  list_of_participants_in_training<-unique(train_set$identifier)
  number_in_RF_training<-floor(length(list_of_participants_in_training)*0.75)
  number_in_HMM_training<-ceiling(length(list_of_participants_in_training)*0.25)
  
  participants_in_RF_training<-sample(list_of_participants_in_training,number_in_RF_training)
  participants_in_HMM_training<-setdiff(list_of_participants_in_training,participants_in_RF_training)
  
  RF_train_set<-train_set[train_set$identifier %in% participants_in_RF_training,]
  HMM_train_set<-train_set[train_set$identifier %in% participants_in_HMM_training,]
} else if(settings[2]==FALSE){
  RF_train_set<-train_set
  HMM_train_set<-train_set
}
  states<-RF_train_set$behavior[-iq]
  
  
  count_of_classes<-table(train_set$behavior)
  count_of_classes<-count_of_classes[order(dimnames(count_of_classes)[[1]])]
  
  sample_size<-min(count_of_classes)
  scaled_ntrees<-ntrees*nrow(RF_train_set)/sample_size
  scaled_ntrees_by_class<-ntrees*count_of_classes/sum(count_of_classes)
  
  
  list_of_behaviors<-unique(RF_train_set$behavior)
  list_of_behaviors<-list_of_behaviors[order(list_of_behaviors)]
  
  #do settings[5] - true does OVA analysis, false does normal multiclass. OVA always has balancing of samples
  #do settings[6] - for settings[5] T, [6] true does ntrees prop to number of inputs, false does equal no of trees per class
  #                 for settings[5] F, [6] true does sampling of inputs prop to population, false does equal samples per class
  if(settings[5]==0){
    
    if(settings[6]==0){
      
      sampsize<-nrow(RF_train_set)
      ntrees<-1600
      }else if (settings[6]==1){
        
      sampsize<-c(rep(sample_size,length(unique(RF_train_set$behavior))))
      ntrees<-3200}
    #do multiclass
    rf<-RF_analysis(ntrees=ntrees,
                                  ncores=ncores,
                                  train_set=RF_train_set,
                                  test_set=test_set,
                                  test_behavior=test_behavior,
                                  RFoutput=file.path(RFoutput),
                                  ResultsOutput=file.path(ResultsOutput),
                                  output_token=paste0(leave_out,'_',paste(settings,collapse = '')),
                                  sampsize=sampsize)
    
    
    #Now we do HMM stuff
    
    EmissionProbs<-computeEmissionProbs_mod(rf=rf,behavior_data=states)
    Testing_predictions<-predict(rf,test_set[,2:(ncol(test_set)-2),with=FALSE])
    hmm<-HMM_analysis(states=states,EmissionProbs=EmissionProbs,Testing_predictions=Testing_predictions,
                      test_behavior=test_behavior,save_vals=FALSE,run_preds=FALSE)
    
    if(settings[4]==TRUE){
      
      #now run EM using HMM_train_set
      update_hmm<-baumWelch(hmm, HMM_train_set$behavior, maxIterations=100, delta=1E-9, pseudoCount=0)
    
      #then run viterbi on updated HMM
      update_hmm<-HMM_analysis(test_behavior=test_behavior,output_token=paste0(leave_out,'_',paste(settings,collapse = '')),
                               save_vals=TRUE,run_preds=TRUE,create_hmm=FALSE,HMM=update_hmm)
      
    }else{
      hmm<-HMM_analysis(test_behavior=test_behavior,output_token=paste0(leave_out,'_',paste(settings,collapse = '')),
                               save_vals=TRUE,run_preds=TRUE,create_hmm=FALSE,HMM=hmm)
    }
      
    } else if(settings[5]==1){
   
  rf_list<-list()
  
  if(settings[6]==1){
  ntrees_vect<-round(ntrees*count_of_classes/sum(1/count_of_classes))
  samp_size<-lapply(count_of_classes,function(x) rep(x,2))
  } else if (settings[6]==0){
    ntrees_vect<-rep(ntrees/length(list_of_behaviors),length(list_of_behaviors))
    samp_size<-list(rep(sample_size,2))[rep(1,length(list_of_behaviors))]
  }
  
  for(i in 1:length(list_of_behaviors)){
    
    ie<-which(train_set$behavior!=list_of_behaviors[i])
    
    temp_train_set<-RF_train_set
    temp_train_set$behavior[ie]<-'else'
    
    
    ir<-which(test_set$behavior!=list_of_behaviors[i])
    temp_test_set<-test_set
    temp_test_set$behavior[ir]<-'else'
    
    temp_test_behavior<-as.character(test_behavior)
    temp_test_behavior[ir]<-'else'
    temp_test_behavior<-as.factor(temp_test_behavior)
    
    temp_samp_size<-sum(temp_train_set$behavior!='else')
    
    rf_list[[i]]<-RF_analysis(ntrees=ntrees_vect[i],
                                  ncores=ncores,
                                  train_set=temp_train_set,
                                  test_set=temp_test_set,
                                  test_behavior=temp_test_behavior,
                                  RFoutput=file.path(RFoutput),
                                  ResultsOutput=file.path(ResultsOutput),
                                  output_token=paste0(leave_out,'_',paste(settings,collapse = '')),
                                  replace=TRUE,
                                  sampsize=samp_size[[i]],
                                  save_vals = FALSE,
                                  run_preds = FALSE)
    
  }
  
  rf_preds_ova<-predict(rf_list,test_set[,2:(ncol(train_set)-2),with=FALSE],type='prob')
  
  rf_preds_ova<-do.call(cbind,rf_preds_ova)
  rf_preds_ova<-rf_preds_ova[,colnames(rf_preds_ova)!='else']
  
  max_vals<-unlist(apply(rf_preds_ova, 1, max) )
  
  max_labels<-apply(rf_preds_ova, 1, which.max )
  
  max_labels<-colnames(rf_preds_ova)[max_labels]
  
  
  rf_conf<-confusionMatrix(max_labels, test_behavior)
  
  dir.create(ResultsOutput, showWarnings = FALSE)
  dir.create(RFoutput, showWarnings = FALSE)
  
  
  save(RF_conf,file=file.path(ResultsOutput,paste0('RF_conf',leave_out,'_',paste(settings,collapse = ''),'.RData')))
  save(rf_list,file=file.path(RFoutput,paste0('RF_',leave_out,'_',paste(settings,collapse = ''),'.RData')))
  
  
  
  
  #Now we do HMM stuff
  
  # get the list of unique states
  sort_states = sort(unique(RF_train_set$behavior))
  S = length(sort_states)
  # set up the transition Probability matrix
  emissionProbs = matrix(0, nrow=S, ncol=S)
  rownames(emissionProbs) = sort_states
  colnames(emissionProbs) = sort_states
  
  
  all_votes<-do.call(cbind,lapply(rf_list, function(x) x$votes))
  
  # loop through state sequence and count transitions
  for (k in 1:length(sort_states)) {
    emissionProbs[k, ] = colSums(all_votes[behavior_data == sort_states[k], ])
    emissionProbs[k, ] = emissionProbs[k, ]/sum(emissionProbs[k, ])
  }
  
  
  
  Testing_predictions<-predict(rf,test_set[,2:(ncol(test_set)-2),with=FALSE])
  hmm<-HMM_analysis(states=states,EmissionProbs=emissionProbs,Testing_predictions=Testing_predictions,
                    test_behavior=test_behavior,save_vals=FALSE,run_preds=FALSE)
  
  
  if(settings[4]==TRUE){
    
    #now run EM using HMM_train_set
    update_hmm<-baumWelch(hmm, HMM_train_set$behavior, maxIterations=100, delta=1E-9, pseudoCount=0)
    
    #then run viterbi on updated HMM
    update_hmm<-HMM_analysis(test_behavior=test_behavior,output_token=paste0(leave_out,'_',paste(settings,collapse = '')),
                             save_vals=TRUE,run_preds=TRUE,create_hmm=FALSE,HMM=update_hmm)
    
  }else{
    hmm<-HMM_analysis(test_behavior=test_behavior,output_token=paste0(leave_out,'_',paste(settings,collapse = '')),
                      save_vals=TRUE,run_preds=TRUE,create_hmm=FALSE,HMM=hmm)
  }
  
  
  }
  
  