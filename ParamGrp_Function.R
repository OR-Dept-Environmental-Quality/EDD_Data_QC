##create a function that groups analytes into categories by creating a column that identifies the analyte type
#this will help organize data in charts and tables

param_grp<-function(x){
  #x is dataframe with list of analytes from AWQMS data pull
  #note that this will need to be done before namefrac function is used
  require(dplyr)
  
  #groupings
  
  #need to add all total and dissolved cases to metals that result from namefrac, or else they won't show up
  metals<-c("Cyanide","Cyanides amenable to chlorination (HCN & CN)","Aluminum","Iron","Lead","Mercury","Nickel","Silver","Thallium","Antimony","Arsenic","Arsenic, Inorganic",
            "Beryllium","Cadmium","Chromium","Copper","Zinc","Selenium","Chromium(III)","Chromium(VI)","Arsenic ion (3+)","Arsenic ion (3+), Dissolved","Arsenic ion (3+), Total Recoverable",
            "Arsenic ion (5+)","Arsenic ion (5+), Total Recoverable","Arsenic ion (5+), Dissolved","Calcium","Cobalt",
            "Lithium","Beryllium","Manganese","Magnesium","Molybdenum","Potassium","Sodium","Uranium","Vanadium","Barium","Boron",
            "Aluminum, Dissolved","Aluminum, Total Recoverable","Antimony, Dissolved", "Arsenic, Dissolved","Barium, Dissolved",
            "Beryllium, Dissolved","Boron, Dissolved","Boron, Total Recoverable","Cadmium, Dissolved","Calcium, Total Recoverable",
            "Chromium, Dissolved","Cobalt, Dissolved","Copper, Dissolved","Iron, Total Recoverable","Lead, Dissolved","Lithium, Dissolved",
            "Lithium, Total Recoverable","Magnesium, Total Recoverable", "Manganese, Total Recoverable","Molybdenum, Dissolved",
            "Molybdenum, Total Recoverable","Nickel, Dissolved","Potassium, Total Recoverable", "Selenium, Dissolved",
            "Silica, Dissolved","Silica, Total Recoverable","Silver, Dissolved","Sodium, Total Recoverable","Thallium, Dissolved",
            "Uranium, Dissolved", "Uranium, Total Recoverable","Vanadium, Dissolved","Zinc, Dissolved", "Antimony, Total Recoverable",
            "Arsenic, Total Recoverable","Barium, Total Recoverable","Beryllium, Total Recoverable","Boron, Total Recoverable",
            "Cadmium, Total Recoverable","Calcium, Dissolved", "Chromium, Total Recoverable","Cobalt, Total Recoverable",
            "Copper, Total Recoverable","Iron, Dissolved","Lead, Total Recoverable","Magnesium, Dissolved","Manganese, Dissolved",
            "Molybdenum, Dissolved","Nickel, Total Recoverable","Potassium, Dissolved", "Selenium, Total Recoverable",
            "Silver, Total Recoverable","Sodium, Dissolved","Thallium, Total Recoverable", "Vanadium, Total Recoverable",
            "Zinc, Total Recoverable", "Mercury, Total Recoverable", "Cyanide, Total","Chromium(III), Total Recoverable","Chromium(VI), Total Recoverable",
            "Chromium(III), Dissolved","Chromium(VI), Dissolved","Arsenic, Inorganic, Dissolved","Arsenic, Inorganic, Total Recoverable","Weak, dissociable cyanide",
            "Mercury, Dissolved", "Cyanide, Dissolved")
  
  #volatile organic carbons
  voc<-c("1,1,1,2-Tetrachloroethane", "1,1,1-Trichloroethane", "1,1,2,2-Tetrachloroethane", "1,1,2-Trichloroethane", "1,1-Dichloroethane", 
         "1,1-Dichloroethylene", "1,1-Dichloropropene", "1,2,3-Trichlorobenzene","1,2,3-Trichloropropane", "1,2,4-Trimethylbenzene", 
         "1,2-Dibromo-3-chloropropane", "1,2-Dichloroethane", "1,2-Dichloropropane","1,3-Dichlorobenzene", "1,3-Dichloropropane",
         "1,3,5-Trimethylbenzene", "2,2-Dichloropropane","cis-1,2-Dichloroethylene","trans-1,2-Dichloroethylene", "Acetone","Acrylonitrile", 
         "Benzene", "Bromobenzene", "Carbon disulfide", "Carbon tetrachloride", "Chlorobenzene", "Chlorodibromomethane", "Chloroethane", 
         "Chloroform", "Chloromethane", "cis-1,3-Dichloropropene", "Cumene", "Dichlorobromomethane", "CFC-12",
         "Dibromomethane", "Ethylbenzene", "p-Cymene","tert-Butylbenzene","Vinyl chloride","Tribromomethane","Tetrachloroethene",
         "Ethylene dibromide","CFC-11", "Halon 1011", "Hexachlorobutadiene", "m-Dichlorobenzene", "Methyl bromide", "Methyl ethyl ketone", 
         "Methyl isobutyl ketone", "Methyl tert-butyl ether", "Methylene chloride", "m-Xylene", "Naphthalene", "n-Butylbenzene","1,3-Dichloropropene","trans-1,3-Dichloropropene",
         "n-Propylbenzene", "o-Chlorotoluene", "o-Dichlorobenzene", "o-Xylene", "p-Chlorotoluene", "p-Dichlorobenzene", "p-Xylene", 
         "sec-Butylbenzene","Styrene","Toluene","Trichloroethene (TCE)", "o-Xylene, mixt. with m-xylene and p-xylene","Xylene","m,p-Xylene",
         "Tetrachloroethylene","Trichloroethylene","2-Chloroethyl vinyl ether","Bromoethane")
  
  #semivolatiles
  semiv<-c("1,2,4,5-Tetrachlorobenzene", "1,2,4-Trichlorobenzene", "1-Methylphenanthrene", "1-Methylpyrene", "2,3,4,6-Tetrachlorophenol", 
           "2,3,5,6-Tetrachlorophenol", "2,4,5-Trichlorophenol", "2,4,6-Trichlorophenol", "2,4-Dichlorophenol", "2,4-Dimethylphenol", 
           "2,4-Dinitrophenol", "2,4-Dinitrotoluene", "2,6-Dichlorophenol", "2,6-Dinitrotoluene", "2-Chloronaphthalene", 
           "3,3'-Dichlorobenzidine", "4,6-Dichloro-o-cresol", "Acenaphthene", "Acenaphthylene","Acrolein", "Anthracene", "Azobenzene", "Benz[a]anthracene", 
           "Benzidine", "Benzo(b)fluoranthene", "Benzo[a]pyrene", "Benzo[ghi]perylene", "Benzo[k]fluoranthene", "Bis(2-chloroethoxy)methane", 
           "Bis(2-chloroethyl) ether", "Bis(2-chloroisopropyl) ether", "Butyl benzyl phthalate", "Chrysene", "Di(2-ethylhexyl) adipate", 
           "Di(2-ethylhexyl) phthalate", "Dibenz[a,h]anthracene", "Dibenzofuran", "Dibutyl phthalate", "Diethyl phthalate", "Dimethyl phthalate",
           "Di-n-octyl phthalate", "Fluoranthene", "Fluorene", "Hexachlorobenzene",  "Hexachlorobutadiene", "Hexachlorocyclopentadiene", 
           "Hexachloroethane", "Indeno[1,2,3-cd]pyrene", "Isophorone", "m-Dichlorobenzene", "Naphthalene",'o-Chlorophenol' ,"o-Cresol",
           "o-Nitrophenol" ,'4,6-Dinitro-o-cresol' , 'BDE-003' ,'p-Chloro-m-cresol','p-Chlorophenyl phenyl ether','p-Nitrophenol' ,
           'Bis(2-chloro-1-methylethyl) ether','Nitrobenzene' ,'N-Nitrosodi-n-propylamine','N-Nitrosodiphenylamine',
           'Pentachlorophenol' ,'Phenanthrene' ,'Phenol' ,'Pyrene',"1,2-Diphenylhydrazine",'N-Nitrosodimethylamine',"Pentachlorobenzene",
           '2-Methylnaphthalene','Benzyl alcohol','Carbazole','m-Nitroaniline','o-Nitroaniline','p-Chloroaniline','p-Nitroaniline',"Pyridine","m,p-Cresol",
           "Butyl 2-ethylhexyl phthalate", "Benzo[j]fluoranthene", "Benzo[j]fluoranthene", "Dibenz[a,h]acridine", "Dibenz[a,j]acridine", "Dibenzo[a,e]pyrene",
           "Dibenzo[a,h]pyrene", "Dibenzo[a,i]pyrene", "Perylene", "3-Methylcholanthrene","Phenols")
  
  #physical chemistry parameters (a little bit of a grab bag)
  phys<-c("Conductivity",	"Depth, bottom","Depth, from ground surface to well water level","Dissolved oxygen (DO)",
          "Oxidation reduction potential (ORP)", "pH","Temperature, water","Nitrate + Nitrite","Ammonium","Organic carbon, Total", 
          "Organic carbon, Dissolved","Chemical oxygen demand","Alkalinity, total","Ammonia","Total dissolved solids","Total suspended solids",
          "Total hardness","Hardness, Ca, Mg","Sulfate","Chloride","Silica", "Silica, Dissolved","Silica, Total Recoverable", "Sulfide",
          "Biochemical oxygen demand, standard conditions","Tannin and Lignin", "Hardness, Ca, Mg, Total Recoverable", "Ammonia, Total",
          "Hardness, Ca, Mg, Total","Hydrogen sulfide","Phosphate-phosphorus", "Phosphorus","Hardness, Ca, Mg, Dissolved","Alkalinity, bicarbonate",
          "Alkalinity, carbonate","Alkalinity, Hydroxide", "Chlorine, Total Residual", "Total Kjeldahl nitrogen", "Nitrite", "Nitrate",
          "Hardness, total by calculation", "Hydroxide", "Alkalinity")
  
  #pesticides, PCBs, and other contaminants
  pest<-c("N,N-Diethyl-m-toluamide","p,p'-DDT","Parathion","Chlordane","Lindane","Dieldrin","Endrin","Methoxychlor","p,p'-DDD","p,p'-DDE","Heptachlor",
          "Azinphos-methyl","Malathion","Aldrin",".alpha.-Hexachlorocyclohexane",".beta.-Hexachlorocyclohexane",
          "Benzene Hexachloride, Beta (BHC)","1,2,3,4,5,6-Hexachlorocyclohexane",".alpha.-Endosulfan","Heptachlor epoxide",
          "Endosulfan sulfate","Mirex","Chlorpyrifos","Endrin aldehyde","Toxaphene","Demeton","Aroclor 1260","Aroclor 1254",
          "Aroclor 1221","Aroclor 1232","Aroclor 1248","Aroclor 1016",".beta.-Endosulfan","Aroclor 1242","Total PCBs","N-Nitrosodiethylamine",
          "N-Nitrosodi-n-butylamine","N-Nitrosopyrrolidine","N-Nitrosomethylethylamine","N-Nitrosomorpholine","N-Nitrosopiperidine","Silvex",
          "Picloram","Dinoseb","Dicamba","Dalapon","2,4-D",".delta.-Hexachlorocyclohexane", "Carbophenothion", "Chlorothalonil", "Coumaphos", "Diazinon",
          "Dichlorvos", "Dicrotophos", "Dimethoate", "Dioxathion","Disulfoton", "Endrin ketone","Ethion", "Ethoprop", "Famphur", "Fenamiphos",
          "Fensulfothion", "Fenthion", "Leptophos", "Merphos", "Methyl parathion", "Mevinphos", "Monocrotophos", "O-Ethyl O-(p-nitrophenyl) phenylphosphonothioate",
          "O,O,O-Triethyl phosphorothioate", "Phorate", "Phosmet", "Prothiofos", "Ronnel", "Sulfotep", "Sulprofos", "Terbufos", "Tetrachlorvinphos",
          "Tetraethyl pyrophosphate", "Trichloronate", "Triethyl phosphate", "2,3,7,8-Tetrachlorodibenzo-p-dioxin","Chlorfenvinphos", "Chlordane, technical",
          "2,2,4,5,6,7,8,8-Octachloro-2,3,3a,4,7,7a-hexahydro-4,7-methano-1H-indene", "cis-Chlordane", "Demeton-S", "Sum of alpha-, beta-, gamma- and delta-BHC",
          "m-Dinitrobenzene","O-Dinitrobenzene","p-Dinitrobenzene","Aniline","1-Methylnaphthalene")
  
  #create group column in dataframe
  x$param_grp<-case_when(x$Char_Name %in% metals ~"Metals",
                         x$Char_Name %in% voc ~ "VOCs",
                         x$Char_Name %in% phys ~ "Physical Chem",
                         x$Char_Name %in% semiv ~ "Semivolatiles",
                         x$Char_Name %in% pest ~ "Pesticides")
  
  return(x)
}

#library(AWQMSdata)
#test<-AWQMS_Data(startdate='2018-05-01', enddate='2018-05-03',org=c("OREGONDEQ","RVBND_LF(NOSTORETID)"),project = "Landfill Monitoring")

#try<-param_grp(test)

#unsort<-unique(subset(try,is.na(param_grp),select=c(Char_Name,param_grp,Method_Code)))

#library(openxlsx)
#write.xlsx(unsort,"E:/UnSort.xlsx")