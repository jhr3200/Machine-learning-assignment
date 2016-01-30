preprocesssdat <- function(rawdata){
  
  #Function to Preprocessing data 
  #To identify non viable columns
  #======================================
  
  # Remove 1st 7 factor columns these are not useful
  print(names(rawdata))
  rawdata<-rawdata[,c(-1:-7)]

  #Now preprocess remaining numeric data
  numcols<-length(rawdata[1,])
  colname<-names(rawdata)
  num_tresh<-0.70
  name_indx <- 0
  col_names<-c()
  col_indx<-c()
  lastcol<-length(rawdata[1,])
  
  #extract outcome column before processing
  outcome<-data.frame(classe=as.factor(rawdata[,lastcol]))
  
  for (i in 1:numcols){#loop through all columns
    
    #convert column format to number 
    convertnumform<-as.numeric(as.character(rawdata[,i]))
    #IF numeric=1 if not=0
    convertlogicform<-as.logical(!is.na(convertnumform))
    
    #number of numeric vals in column
    num_vals<-sum(convertlogicform)
    num_cols<-length(rawdata[,i])
    frac_num<-num_vals/num_cols
    print(paste("fract=",frac_num))
    
    if(!is.na(frac_num)) { #If fraction of numeric content is < num_tresh remove column 
      frac_num 
      num_tresh
      if(frac_num >= num_tresh){
        print(paste("***kept col ", i))
        name_indx<-name_indx+1
        col_names[name_indx] <- colname[i]
        col_indx[name_indx] <- i
      }
    }
    
  }
  
  #surviving columns  
  viablenumericdata<-data.frame(rawdata[,col_indx])
  viablenumericdata
  
  #Reconstruct data frame with viable variables and factors omitting rows with NA's
  processeddata<-(data.frame(outcome,viablenumericdata))


return(processeddata)
}
