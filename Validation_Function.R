# code to validate NPDES data
#check data in AWQMS through data pull function
#output an excel sheet that contains validation results

#having issues with factors, convert to numeric
#function to do so
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

  #x is dataset
  #checks to run

  #1. QL checks
  ql<-function(x){
    require(openxlsx)
    require(dplyr)
    source("E:/GitHub/ShinyNPDES_AWQMS/NameandFraction.R")

    #read in table with QLs
    qls<-read.xlsx("2_21_19_QLs.xlsx",colNames=TRUE,na.string="NA")
    
    #need to do unit conversions to compare data and ql list, all need to be in ug/l, except for Alkalinity, which should be in mg/L
    #checked AWQMS database, all alkalinity is reported in mg/l or mg/l CaCO3, no need for conversion
    #get list of char names (just use names in the ql table, don't need to compare if the analyte isn't in that table)
    names<-unique(qls$Char_Name)
    #remove alkalinity, that needs to stay as mg/l
    names<-names[names !="Alkalinity, total"]
    
    x<-unit_conv(x,names,"mg/l","ug/l")
    x<-unit_conv(x,names,"ng/l","ug/l")
    
    #do a join of qls and x based on Char_Name (need to combine metals and fractions)
    x<-namefrac(x)
    
    x<-left_join(x,qls,by="Char_Name")
    
    #compare QL to MRL (got rid of unit mismatch code, was causing lot of problems and we've got the basic units covered)
    x$issue<-ifelse(
      x$MRLValue>x$QL & (x$Result_Numeric<x$MRLValue | x$Result=="ND"| (x$Result_Numeric==x$MRLValue & is.na(x$MDLValue))),
      "QL not met, Result below QL",
      NA)
    
    return(subset(x,!(is.na(issue)),
           select=c("Char_Name","CASNumber","act_id","Result","Result_Unit","MDLValue","MRLValue","MRLUnit","QL","QL_Unit","Result_Type","Result_Comment","issue")))
    
    
  }
  
  #2. Dissolved vs Total/Total Recoverable
  dvst<-function(x){
    y<-subset(x,x$Sample_Fraction %in% c("Total Recoverable","Dissolved","Total") & !(x$Char_Name %in% c("Organic carbon","Alkalinity, total")))
    
    #need to make this conditional, returns error if there are no analytes that have the right sample fractions
    if(nrow(y)!=0) {
    
      #create new identifier out of date and characteristic name
      #(used to use activity ID, but a lot of the labs have been using multiple activity IDs per batch 
      #(e.g. unfilterd sample is xx-001A and filtered sample is xx-001B))
      #use date and location since it is extremely rare for a permittee to be taking more than one sample a day at each location for metals data
    y$comb<-paste0(y$SampleStartDate,",",y$MLocID,",",y$Char_Name)
    
    #get unique identifiers, put into new dataset
    new<-unique(subset(y,select=c("act_id","SampleStartDate","MLocID","Char_Name","comb")))
    
    #select all dissolved
    dis<-subset(y,y$Sample_Fraction %in% "Dissolved")
    
    #select all total recoverable/total
    tot<-subset(y,y$Sample_Fraction %in% c("Total Recoverable","Total"))
    
    #add dissolved and total columns to new dataset
    new$dissolved<-dis$Result_Numeric[match(new$comb,dis$comb,nomatch=NA)]
    new$total<-tot$Result_Numeric[match(new$comb,tot$comb,nomatch=NA)]
    
    #add MDL value (take lower value if one is lower than the other (makes for more conservative test))
    new$totMDL<-tot$MDLValue[match(new$comb,tot$comb,nomatch=NA)]
    new$disMDL<-dis$MDLValue[match(new$comb,dis$comb,nomatch=NA)]
    new$MDL<-ifelse(new$totMDL<new$disMDL,new$totMDL,new$disMDL)
    
    #calculate the difference, round to 3 figures
    new$diff<-round(new$total-new$dissolved,3)
    
    #calculate the RPD
    new$RPD<-abs(new$diff)/((new$total+new$dissolved)/2)*100
    
    #round the RPD value
    new$RPD<-round(new$RPD,2)
    
    #if difference is negative, and if it is larger than the MDL, then it is an issue
    issues<-subset(new,new$diff<0 & abs(new$diff)>new$MDL)
    cont<-merge(issues,y, by="comb")
    
    final<-unique(subset(cont,select=c("Char_Name.x","Sample_Fraction","Result","MRLValue","MDLValue","Result_Unit","diff","RPD","SampleStartDate.x",
                                "SampleStartTime","OrganizationID","MLocID.x","Project1","Result_status","Result_Comment")))
    }
    
    #create empty data frame to ensure code continues to run smoothly
    
    else {final<-data.frame(matrix(ncol=15,nrow=0))
    names<-c("Char_Name.x","Sample_Fraction","Result","MRLValue","MDLValue","Result_Unit","diff","RPD","SampleStartDate.x",
    "SampleStartTime","OrganizationID","MLocID.x","Project1","Result_status","Result_Comment")
    colnames(final)<-names
    }
    
    return(final)
    
  }
  
  
  #3. Method Checks- get list of methods used and list of characteristics
  #will want to significantly expand this to be able to check against the CFR
  mc<-function(x){
    #x is dataset
    require(openxlsx)
    require(dplyr)
   
    
    #read in AWQMS-compatible CFR method list- NOTE THAT THIS LIST IS NOT EXHAUSTIVE YET-BULDING AS WE GO
    cfr<-read.xlsx("3_13_2019_CFR_Methods.xlsx",colNames=TRUE,na.string="NA")
    
    #join datasets on characteristic name
    x<-left_join(x,cfr,by=c("Char_Name","Method_Code","Method_Context"))
    
    
    
    
    #note that EPA method 625 and 624 were valid CFR methods up until the 2017 revision of CFR 136, where they were replaced with 
    #625.1 and 624.1, make sure this is noted in the code so we don't flag pre 2017 data as out of compliance when it was actually fine
    x$CFR_Method<-ifelse(x$Method_Code %in% c("624","625","608") & x$SampleStartDate<"2017-09-27",
                         paste0("Yes"),
                         x$CFR_Method)
    
    x$CFR_Method<-ifelse(x$Method_Code %in% c("624","625","608") & x$SampleStartDate>"2017-09-27",
                         paste0("Out of Date (9/27/2017)"),
                         x$CFR_Method)
    
    #no methods for tributyl phosphate, salinity, Demeton (8065-48-3), trivalent chromium, or inorganic arsenic in CFR 136
    #also no method specified in CFR 136 for a number of purgeables
    x$CFR_Method<-ifelse(x$Char_Name %in% c("Tributyl phosphate","Salinity","Demeton","Arsenic, Inorganic",
                                            ".alpha.-Terpineol", "1,2,4,5-Tetrachlorobenzene","1-Methylnaphthalene",
                                            "2,3,4,6-Tetrachlorophenol","2,3-Dichloroaniline","2,4,5-Trichlorophenol",
                                            '2,6-Dichlorophenol','2-Methylnaphthalene','2-Naphthalenamine','Acetophenone',
                                            'Aniline','Benzaldehyde','Benzoic acid','Benzyl alcohol','Biphenyl',
                                            'Butyl 2-ethylhexyl phthalate','Methylmercury(1+)',
                                            'Caprolactam','Carbazole','Decane','Dibenzofuran','Dimethoate',
                                            'Diphenylamine','Heptadecane','m,p-Cresol','m-Nitroaniline','N-Nitrosodiethylamine',
                                            'N-Nitrosodi-n-butylamine','N-Nitrosopyrrolidine', 'o-Cresol',
                                            'Octadecane','o-Nitroaniline','p-Chloroaniline','Pentachlorobenzene',
                                            'p-Nitroaniline','Pyridine','Chromium(III)','Endrin ketone','1,2-Diphenylhydrazine','Nitrogen'),
                     paste0("No CFR method for pollutant, check permit"),
                     x$CFR_Method)
    
    x$CFR_Method<-ifelse(is.na(x$CFR_Method),
                         paste0("No, check CFR 136"),
                         x$CFR_Method)
    
    #return method results
    sub<-subset(x, CFR_Method %in% c("No, check CFR 136","Out of Date (9/27/2017)","No CFR method for pollutant, check permit"),
                select=c(Char_Name,Method_Code,Method_Context,CFR_Method))
    sub<-unique(sub)
    
    return(sub)
  }
  
  #estimated data (want to remove any rejected data (will be covered by rejected function), or J flag data),
  #but do want to keep data that is J flag but with a QC issue (result comment will be filled in)
  estm<-function(x){
    source("E:/GitHub/ShinyNPDES_AWQMS/NameandFraction.R")
    x<-namefrac(x)
    
    sub<- subset(x,x$Result_Type=="Estimated" & x$Result_status!="Rejected" & 
                   (x$Result_Numeric>=x$MRLValue | (x$Result_Numeric<=x$MDLValue & x$Result_Operator=="<") | (is.na(x$MDLValue) & is.na(x$MRLValue)) | !(is.na(x$Result_Comment))),
                  select=c(MLocID,act_id,SampleStartDate,SampleStartTime,Char_Name,Char_Speciation,
                           CASNumber,Result,Result_Unit,Method_Code,Result_Comment,Result_Type,Result_status))
  }
  
  #rejected data
  rej<-function(x){
    #x is dataset
    x<-namefrac(x)
    sub<-subset(x,Result_status %in% c("Rejected"),
                select=c(MLocID,act_id,SampleStartDate,SampleStartTime,Char_Name,Char_Speciation,
                         CASNumber,Result,Result_Unit,Method_Code,Result_Comment,Result_status))
  }
  


#x<-AWQMS_Data(startdate = "2015-05-01", enddate = "2019-07-17" , org = "SFPP_INC(NOSTORETID)")
