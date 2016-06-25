#Cluster parse variable inputs

option_list <- list(
  make_option(c("-n", "--nchunks"), type="integer",
              help="number of chunks data is divided into",
              metavar="number"),
  make_option(c("-c", "--chunk"), type="integer",
              help="which chunk of data we are analysing",
              metavar="number"),
  make_option(c("-p", "--participant"), type="integer", default=1,
              help="which participant we are leaving out of analysis",
              metavar="number"),
  make_option(c("-t", "--trees"), type="integer", default=1600,
              help="number of trees for random forest",
              metavar="number"),
  make_option(c("-d", "--duration"), type="integer", default=30,
              help="size of window for analysis",
              metavar="number")
)

opt <- parse_args(OptionParser(option_list=option_list))

nchunks<-opt$nchunks

chunkID<-opt$chunk

leave_out<-opt$participant

ntrees<-opt$trees

duration<-opt$duration