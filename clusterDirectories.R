
#First, define data directories
dataDirectory<-'/data/dph-ukbaccworkgroup/phpc0595/data/capture24/epoch30sec'
labelDirectory<-'/data/dph-ukbaccworkgroup/npeu0203/label-data/label-dictionary-9-classes'
instanceLabelDirectory<-'/data/dph-ukbaccworkgroup/magd4534/label-data/instance-label-dictionary-9-classes'
outputDataDirectory<-'/data/dph-ukbaccworkgroup/magd4534/capture-processed/30sec'
resultsDataDirectory<-'/data/dph-ukbaccworkgroup/magd4534/results/30sec'

# dataDirectory<-'/Users/Matthew/Documents/Oxford/Activity/FeatureData'
# labelDirectory<-'/Users/Matthew/Documents/Oxford/Activity/LabelData'
# instanceLabelDirectory<-'/Users/Matthew/Documents/Oxford/Activity/InstanceLabelData'
# outputDataDirectory<-dataDirectory
# resultsDataDirectory<-outputDataDirectory


#Directories for RF and HMM models
RFoutput<-paste0(resultsDataDirectory,'/RFoutput')
HMMoutput<-paste0(resultsDataDirectory,'/HMMoutput')
ProxOutput<-paste0(resultsDataDirectory,'/RFoutput/ProxTrain')
