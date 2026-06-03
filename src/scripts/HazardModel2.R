#_______________________________________________________________________________
#
# Hazard Model 2.0
# 
# AUTHOR: Brenda Hanley
# ERROR HANDLING: Brenda Hanley
# MODEL LOGGING: Brenda Hanley
# QAQC: 
#
# Location: Cornell Wildlife Health Lab
# Licence: MIT
#
# This code corresponds to the work by Thompson et al.
# 
# This code was written under: 
# R version 4.4.1 (2024-06-14 ucrt) -- "Race for Your Life"
# Copyright (C) 2024 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64
# 
#_______________________________________________________________________________

# Load packages. ------------ 
	library(dplyr)

# Reusable Functions
html_tag_line <- function(text, tag = "p") {
	# Given a string and a simple string representing an html tag, this
	# function surrounds the text with the tag.
	paste0("<", tag, ">", text, "</", tag, ">")
}

add_item_to_json_array=function(file_path, new_item) {
    # This is a bespoke function that adds a string representing a JavaScript
    # Object to the attachments.json file containing an array listing the model
    # outputs. Although this function has error handling for a missing file and
    # improperly formed file, the existence of the file and a list enclosed in
    # brackets in that file are expected.

    # Check if the file exists.
    if (!file.exists(file_path)) {
        # Write to error log and exit script with an error.
        line=paste0("<h4>ERROR</h4><p>Error: File '", file_path, "' not found.</p>")
        write(line,file=model_log_filepath,append=TRUE)
        quit(status=1)}

    # Read the file content.
    file_content=readChar(file_path, file.info(file_path)$size)

    # Check if the file is empty.
    if (nchar(file_content) == 0) {
        line=paste0("<h4>ERROR</h4><p>Error: File '", file_path, "' is empty.</p>")
        write(line,file=model_log_filepath,append=TRUE)
        quit(status=1)}

    # Remove the last closing bracket.
    file_content=substr(file_content, 1, nchar(file_content) - 1)

    # Create a function for adding double quotes around text.
    double_quote=function(x) {paste0('"', x, '"')}

    # Create the new item as a JSON string using shQuote with double_quote.
    new_item_json=paste0(
        "{",
        paste(
            sapply(names(new_item), double_quote),
            sapply(as.character(new_item), double_quote),
            sep=":", collapse=","
        ),
        "}"
    )

    # Add comma, new item, and closing bracket to the file content
    file_content=paste0(file_content, ",", new_item_json, "]")

    # Write the updated data back to the file
    writeLines(file_content, file_path, sep="")
}

# Model log file started with Python data processing script. -------
	model_log_filepath=file.path("", "data","attachments","info.html")
	json_attachment_file=file.path("", "data", "attachments.json")

# Continue the log started with the python script. ----------
	line='<h4>Model Execution</h4>'
	write(line,file=model_log_filepath,append=TRUE)

# Info about Required Data. ------------
# There are required files that will always be imported:
# 1. A csv file of selected set of risk factors and weights.
# 2. A csv file of sub-administrative areas (to get the standard data frame). 
# 3. A csv file of data totals (to get data totals on all user-selected factors). 
# 4. A csv file of distances to closest known infection. 
# 5. A csv file of the average movements. 

# WEIGHTS TABLE -----------------------------------------------
	
# Read in the (Required) Weights file. 
	Weights_filepath=file.path("","data","Weights.csv")
	Weights=readr::read_csv(Weights_filepath,show_col_types = FALSE)
	#Weights=readr::read_csv("Weights.csv") 

	# Formatting the weights table.  
	Weights=as.data.frame(cbind(Weights$RiskFactor,Weights$UserSelection,
	Weights$MeanWeightNear,Weights$MeanWeightFar,Weights$SDWeightNear,Weights$SDWeightFar))
  colnames(Weights)=c("RiskFactor","UserSelection","MeanWeightNear","MeanWeightFar","SDWeightNear","SDWeightFar")
  
  # Get the number of risk factors selected. 
  NumberRisksSelected=sum(as.numeric(Weights$UserSelection))

  # Zero out weights of risk factors that are irrelevant in model.
	Weights[Weights$UserSelection=="0",]=0
	
	# Check. If the user selects only one risk factor, and that risk factor 
	# has a NA for a weight, then stop the code. 
	if (NumberRisksSelected==1){
	  which=Weights$UserSelection[Weights$UserSelection==1]
	    if (is.na(Weights$MeanWeightNear[which])|is.na(Weights$MeanWeightFar[which])){
	      # Terminate the code. 
	      line="We're sorry. You selected a single risk factor, and that factor does not 
	      have a valid weight. Return to the CWD Data Warehouse and alter the weights, or select a new set of risk factors."
	      write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
	      # Quit the session.
	      quit(status=70)}}
	
	# Check. If the user selects only one risk factor, and that risk factor 
	# has a 0 for a weight, then stop the code. 
	if (NumberRisksSelected==1){
	  which=as.numeric(Weights$UserSelection[Weights$UserSelection==1])
	  if (Weights$MeanWeightNear[which]==0|Weights$MeanWeightFar[which]==0){
	    # Terminate the code. 
	    line="We're sorry. You selected a single risk factor, and that factor does not 
	      have a non-zero weight. Return to the CWD Data Warehouse and alter the 
	    weights, or select a new set of risk factors."
	    write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
	    # Quit the session.
	    quit(status=70)}}

  # At this point, the risk factors table and the weights table should only have 
  # weights and SDs for relevant risk factors (and no weights where irrelevant). 
	# At least one relevant risk factor has non-zero weight. 
	
  # Check. Make sure that there are no NAs in the weights table as it could cause problems in shadow math. 
	Weights[is.na(Weights)]=0

  # Check. User could have any assortment of risk factors in there, but make sure at 
	# least one risk factor exists with non-zero weights.
	NotZero=Weights[rowSums(Weights!=0)>0,]
	NotZeroDim=as.numeric(nrow(NotZero))
	# If all are zeros, terminate the script. 
	if (NotZeroDim==0){
	    line="We're sorry. This map would be boring because there are no weights!"
	    write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
	    line="Please return to the CWD Data Warehouse and select a different set of weights."
	    write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
	    # Quit the session.
	    quit(status=70)
	  } # End if weights do not exist.
	
	Cleaned_Weights=Weights
	
	# The final version of the weights is now named Cleaned_Weights.
	
# DISTANCE FILE ------------------------------------------------
	
	# Read in the (Required) Distance file. 	
	Distance_filepath=file.path("","data","Distance.csv")
	Distance=readr::read_csv(Distance_filepath,show_col_types = FALSE) 	
	#Distance=readr::read_csv("Distance.csv") 
	
	# Formatting the distance file. 
	Cleaned_Distance=as.data.frame(cbind(Distance$SubAdminID,Distance$FullName,Distance$DistanceToNearestPositive,Distance$DistanceAdjustedWithMigration))
	colnames(Cleaned_Distance)=c("SubAdminID","FullName","DistanceToNearestPositive","DistanceAdjustedWithMigration")
	
	# Check. Make sure that there are no NAs in the distance file. 
	Cleaned_Distance[is.na(Cleaned_Distance)]=0
	
	# Check. Make sure all distances are positive. 
	Cleaned_Distance[Cleaned_Distance<0]=0
	
	# The cleaned distance file is now named Cleaned_Distance.
	
# SUBADMIN -----------------------------------------------------
	
	# Read in the (Required) SubAdmin file. 
	Subadmin_filepath=file.path("","data","Subadmin.csv")
	Subadmin=readr::read_csv(Subadmin_filepath,show_col_types = FALSE) 
	#Subadmin=readr::read_csv("Subadmin.csv") 
	
	# Formatting the subadmin file. 
	Subadmin=as.data.frame(cbind(Subadmin$SubAdminID,Subadmin$FullName))
	colnames(Subadmin)=c("SubAdminID","FullName")
	
	# Get the number of subadmin units. 
	FullNumberAreas=as.numeric(nrow(Subadmin))
	
# DATA TOTALS FILE ---------------------------------------------
	
	# Read in the (Required) Data Totals file. 
	DataTotals_filepath=file.path("","data","DataTotals.csv")
	DataTotals=readr::read_csv(DataTotals_filepath,show_col_types = FALSE) 
	#DataTotals=readr::read_csv("DataTotals.csv") 

	# Formatting the data totals. 
	Cleaned_DataTotals=as.data.frame(cbind(
	  DataTotals$SubAdminID,
	  DataTotals$FullName,
	  DataTotals$MineralLicks,
	  DataTotals$WaterBodies,
	  DataTotals$Feedgrounds,
	  DataTotals$Guzzlers,
	  DataTotals$BaitingStations,
	  DataTotals$AgriculturePractices,
	  DataTotals$CaptiveCervidFacilities,
	  DataTotals$RehabilitationFacilities,
	  DataTotals$Taxidermists,
	  DataTotals$Processors,
	  DataTotals$RenderingFacilities,
	  DataTotals$Incinerators,
	  DataTotals$FoodBanks,
	  DataTotals$Landfill,
	  DataTotals$Dumping,
	  DataTotals$Sheds,
	  DataTotals$Roadkill,
	  DataTotals$LocalPractices,
	  DataTotals$FreeRangingCervidDispersal,
	  DataTotals$SeasonalMigration))
	  colnames(Cleaned_DataTotals)=
	    c("SubAdminID",
	    "FullName",
	    "MineralLicks",
	    "WaterBodies",
	    "Feedgrounds",
	    "Guzzlers",
	    "BaitingStations",
	    "AgriculturePractices",
	    "CaptiveCervidFacilities",
	    "RehabilitationFacilities",
	    "Taxidermists",
	    "Processors",
	    "RenderingFacilities",
	    "Incinerators",
	    "FoodBanks",
	    "Landfill",
	    "Dumping",
	    "Sheds",
	    "Roadkill",
	    "LocalPractices",
	    "FreeRangingCervidDispersal",
	    "SeasonalMigration")
	  
	# Get the number of subamin areas with data. 
	NumSubAreas=nrow(Cleaned_DataTotals)
	
	# At this point, if the risk factor is not relevant, the weight is zero. 
	# However, there could be instances where NA exists in the data of an irrelevant risk factor. 
	# We need to take care of those to ensure that NA is used only for missing data within relevant risk factors. 
	for (i in 1:20){if (Cleaned_Weights$UserSelection[i]==0){Cleaned_DataTotals[,i+2]=rep(0,dim(DataTotals)[1])}}
	
	# Check. If the user selects only one risk factor, and that risk factor 
	# has a NA-valued data in all subamin areas, then stop the code. 
	if (NumberRisksSelected==1){
	  NumberOfNAs=as.numeric(sum(is.na(Cleaned_DataTotals)))
	  if (NumberOfNAs==NumSubAreas){
	    # Terminate the code. 
	    line="We're sorry. You selected a single risk factor, but that risk factor does not 
	      have data for any sub-admin area. Return to the CWD Data Warehouse and input 
	    additional data, or select a new set of risk factors."
	    write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
	    # Quit the session.
	    quit(status=70)}}
	
	# Check. If the user selects many risk factors, but all selected risk factors
	# only have NAs for data, then terminate the code. 
	if (NumberRisksSelected>1){
	  NumberOfNAs=as.numeric(sum(is.na(Cleaned_DataTotals)))
	  if (NumberOfNAs==NumberRisksSelected*NumSubAreas){
	    # Terminate the code. 
	    line="We're sorry. You selected a set of risk factors, but those risk factors do not 
	   have data for any sub-admin area. Return to the CWD Data Warehouse and input 
	    additional data, or select a new set of risk factors."
	    write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
	    # Quit the session.
	    quit(status=70)}}
	
	# At this point, an NA only exists if there is missing data in a relevant risk factor.
	# However, at least one sub-admin area has non-NA data, and not all sub-admin areas 
	# have NAs in every risk factor. At least one sub-admin area can be mapped. 
	
	# Now we want to scale the count factors. Our goal is to have all count columns with non-zero entries sum to one. 
	# Note that these count risk factors start at column 3 and run through column 20. 
	# Note: We will deal with the last two (#21 dispersal and #22 seasonal migration) next. 
	Cleaned_DataTotals=as.data.frame(Cleaned_DataTotals)
	for (h in 3:20){
	  x=sum(as.numeric(Cleaned_DataTotals[,h]),na.rm=TRUE)
	  if (x!=0) {
	    Cleaned_DataTotals[,h]=as.numeric(Cleaned_DataTotals[,h])/sum(as.numeric(Cleaned_DataTotals[,h]),na.rm=TRUE)}}
	
	# Join distance data to the data to make the last two comparisons. 	
	Data_Distance=left_join(Cleaned_DataTotals,Cleaned_Distance,by=c("SubAdminID","FullName"))	
	
	# We will now scale dispersal, which is column 21. 
	# Dispersal matters fully (i.e., has a value of 1) if a single animal can arrive from the nearest known infection. 
	# Thus, if dispersal is greater than or equal to the distance to the nearest positive, then the value will be scaled to 1.
	# Whereas if dispersal is less than the distance to the nearest positive, then the value will be scaled to the proportion of the distance. 
	for (k in 1:NumSubAreas){
	  y=as.numeric(Cleaned_DataTotals[k,21])
	  if (is.na(y)){Cleaned_DataTotals[k,21]=NA}else{
	    if (y>=as.numeric(Data_Distance$DistanceToNearestPositive[k])){Cleaned_DataTotals[k,21]=1
	    }else{Cleaned_DataTotals[k,21]=as.numeric(Cleaned_DataTotals[k,21])/as.numeric(Data_Distance$DistanceToNearestPositive[k])}
	  }}
	# Now that the units are consistent, we will scale the entire vector to one. 
	Cleaned_DataTotals[,21]=as.numeric(Cleaned_DataTotals[,21])/sum(as.numeric(Cleaned_DataTotals[,21]),na.rm=TRUE)
	
	# Finally, we will scale seasonal migration, which is column 22. 
	# Seasonal migration matters fully (i.e., has a value of 1) if the herd's movements touch the nearest known infection.
	# Thus, if the seasonal migration is greater than the distance to the nearest positive, then the value will be scaled to 1. 
	# Whereas if seasonal migration is less than the distance to the nearest positive, then the value will be scaled to the proportion of the distance. 
	for (k in 1:NumSubAreas){
	  y=as.numeric(Cleaned_DataTotals[k,22])
	  if (is.na(y)){Cleaned_DataTotals[k,22]=NA}else{
	    if (y>=as.numeric(Data_Distance$DistanceToNearestPositive[k])){Cleaned_DataTotals[k,22]=1
	    }else{Cleaned_DataTotals[k,22]=as.numeric(Cleaned_DataTotals[k,22])/as.numeric(Data_Distance$DistanceToNearestPositive[k])}
	  }}
	# Now that the units are consistent, we will scale the entire vector to one. 
	Cleaned_DataTotals[,22]=as.numeric(Cleaned_DataTotals[,22])/sum(as.numeric(Cleaned_DataTotals[,22]),na.rm=TRUE)
	
	# The complete scaled data table is now named Cleaned_DataTotals. 
	
# MOVEMENT FILE --------------------------------------
	
	# Read in the (Required) Movements file. 	
	Movement_filepath=file.path("","data","AverageMovement.csv")
	Movement=readr::read_csv(Movement_filepath,show_col_types = FALSE) 
	#Movement=readr::read_csv("Movement.csv")
	
	# Formatting the movement file. 
	Cleaned_Movement=as.data.frame(cbind(Movement$SubAdminID,Movement$FullName,Movement$AverageMovement))
	colnames(Cleaned_Movement)=c("SubAdminID","FullName","AverageMovement")
	
	# Check. Make sure that there are no NAs in the movement file. 
	Cleaned_Movement[is.na(Cleaned_Movement)]=0
	
	# Check. Make sure all movements are positive. 
	Cleaned_Movement[Cleaned_Movement<0]=0
	
	# The cleaned movement file is now named Cleaned_Movement.

# FOLD DATA STREAMS TOGETHER ------------------------------------------------
	
# Join cleaned data total table to SubAdmin ID. 
	SubAdmin_Data=left_join(Subadmin,Cleaned_DataTotals,by=c("SubAdminID","FullName"))

# Join cleaned distance data to SubAdmin ID. 	
	SubAdmin_Distance=left_join(Subadmin,Cleaned_Distance,by=c("SubAdminID","FullName"))	
	
	# Join cleaned movement data to SubAdmin ID. 	
	SubAdmin_Movement=left_join(Subadmin,Cleaned_Movement,by=c("SubAdminID","FullName"))	
	
# COMPUTE QUANTITIES FOR EACH OF THE i SPATIAL CELLS. ------------------

	# Initialize data storage for the i spatial cells. 
	BaselinePerCell=rep(0,FullNumberAreas)
	TotalPerCell=rep(0,FullNumberAreas)
	HazardPerCell=rep(0,FullNumberAreas)
	
	# Initialize data storage for the j risk factors in the i cells.
	# Means. 
	eBPerCell=rep(0,FullNumberAreas)
	eRFPerCell=matrix(0,FullNumberAreas,20)
	eWPerCell=matrix(0,FullNumberAreas,20)
	
	# Mins and Maxes. 
	eRFPerCell_MAX=matrix(0,FullNumberAreas,20)
	eWPerCell_MAX=matrix(0,FullNumberAreas,20)
	eRFPerCell_MIN=matrix(0,FullNumberAreas,20)
	eWPerCell_MIN=matrix(0,FullNumberAreas,20)
	
	# Means. 
	# Loop through all i spatial cells to find the means. 
	for (i in 1:FullNumberAreas){
	  # Compute the Baseline Per Spatial Cell. 
	  Distance=as.numeric(SubAdmin_Distance$DistanceAdjustedWithMigration)
	  FRAME=read.csv("/data/CWD_Transition_Probability.csv")
	  model=lm(transition_prob~dist_1,data=FRAME)
	  distance=data.frame(dist_1=Distance[i])
	  BaselinePerCell[i]=as.numeric(max(0,predict(model,newdata=distance)))
	  # Compute the Total Risk Per Spatial Cell. 
	  Distance=as.numeric(SubAdmin_Distance$DistanceAdjustedWithMigration)
	  AverageMovement=as.numeric(SubAdmin_Movement$AverageMovement)
	  DataCountMatrix=SubAdmin_Data[,3:22]
	  WeightVectorNear=as.numeric(Cleaned_Weights$MeanWeightNear) # 20 element vector with all numbers. 
	  WeightVectorFar=as.numeric(Cleaned_Weights$MeanWeightFar) # 20 element vector with all numbers.
	      # Nearer scenario. 
	      if(Distance[i]<=AverageMovement[i]) {
	        WeightVector = WeightVectorNear
	        if (any(is.na(DataCountMatrix[i,]))){TotalPerCell[i]=NA}else{
	          TotalPerCell[i]=as.numeric(sum(WeightVectorNear*DataCountMatrix[i,]))}}
	      # Farther Scenario. 
	      if(Distance[i]>AverageMovement[i]) {
	        WeightVector = WeightVectorFar
	        if (any(is.na(DataCountMatrix[i,]))){TotalPerCell[i]=NA}else{
	          TotalPerCell[i]=sum(WeightVectorFar*as.numeric(DataCountMatrix[i,]))}}
	  # Compute the Total Hazard Per Cell. 
	  if (is.na(TotalPerCell[i])){HazardPerCell[i]=NA}else{HazardPerCell[i]=BaselinePerCell[i]+BaselinePerCell[i]*TotalPerCell[i]}
    # Compute the elasticity of the baseline. 
	  if(is.na(HazardPerCell[i])){eBPerCell[i]=NA}else{
	    if(HazardPerCell[i]==0){eBPerCell[i]=0}else{eBPerCell[i]=(BaselinePerCell[i]/HazardPerCell[i])*(1+sum(WeightVector*DataCountMatrix[i,]))}}
    # Compute the elasticity of each of the j risk factors in cell i.
	  for (j in 1:20){
	    if (is.na(HazardPerCell[i])){eRFPerCell[i,j]=NA}else{
	    if(HazardPerCell[i]==0){eRFPerCell[i,j]=0}else{eRFPerCell[i,j]=(BaselinePerCell[i]*WeightVector[j]*DataCountMatrix[i,j])/HazardPerCell[i]}}
	  } # End j. 
    # Compute the elasticity of each of the j weights in cell j. 
	  for (j in 1:20){ 
	    if(is.na(HazardPerCell[i])){eWPerCell[i,j]=NA}else{
	    if(HazardPerCell[i]==0){eWPerCell[i,j]=0}else{eWPerCell[i,j]=(BaselinePerCell[i]*WeightVector[j]*DataCountMatrix[i,j])/HazardPerCell[i]}}
	  } # End j. 
  } # End i areas. 
	
	# Maximums.  
	# Loop through all i spatial cells to find the maxes. 
	for (i in 1:FullNumberAreas){
	  # Compute the Baseline Per Spatial Cell. 
	  Distance=as.numeric(SubAdmin_Distance$DistanceAdjustedWithMigration)
	  FRAME=read.csv("/data/CWD_Transition_Probability.csv")
	  model=lm(transition_prob~dist_1,data=FRAME)
	  distance=data.frame(dist_1=Distance[i])
	  BaselinePerCell[i]=as.numeric(max(0,predict(model,newdata=distance)))
	  # Compute the Total Risk Per Spatial Cell. 
	  Distance=as.numeric(SubAdmin_Distance$DistanceAdjustedWithMigration)
	  AverageMovement=as.numeric(SubAdmin_Movement$AverageMovement)
	  DataCountMatrix=SubAdmin_Data[,3:22]
	  WeightVectorNear=as.numeric(Cleaned_Weights$MeanWeightNear) # 20 element vector with all numbers. 
	  WeightVectorFar=as.numeric(Cleaned_Weights$MeanWeightFar) # 20 element vector with all numbers.
	  # Adjust to the maximum weight. 
	  WeightVectorNear=WeightVectorNear+2*as.numeric(Cleaned_Weights$SDWeightNear)
	  WeightVectorFar=WeightVectorFar+2*as.numeric(Cleaned_Weights$SDWeightFar)
	  # Nearer scenario. 
	  if(Distance[i]<=AverageMovement[i]) {
	    WeightVector = WeightVectorNear
	    if (any(is.na(DataCountMatrix[i,]))){TotalPerCell[i]=NA}else{
	      TotalPerCell[i]=as.numeric(sum(WeightVectorNear*DataCountMatrix[i,]))}}
	  # Farther Scenario. 
	  if(Distance[i]>AverageMovement[i]) {
	    WeightVector = WeightVectorFar
	    if (any(is.na(DataCountMatrix[i,]))){TotalPerCell[i]=NA}else{
	      TotalPerCell[i]=sum(WeightVectorFar*as.numeric(DataCountMatrix[i,]))}}
	  # Compute the Total Hazard Per Cell. 
	  if (is.na(TotalPerCell[i])){HazardPerCell[i]=NA}else{HazardPerCell[i]=BaselinePerCell[i]+BaselinePerCell[i]*TotalPerCell[i]}
	  # Compute the elasticity of the baseline. 
	  if(is.na(HazardPerCell[i])){eBPerCell[i]=NA}else{
	    if(HazardPerCell[i]==0){eBPerCell[i]=0}else{eBPerCell[i]=(BaselinePerCell[i]/HazardPerCell[i])*(1+sum(WeightVector*DataCountMatrix[i,]))}}
	    # Compute the elasticity of each of the j risk factors in cell i.
	    for (j in 1:20){
	      if (is.na(HazardPerCell[i])){eRFPerCell_MAX[i,j]=NA}else{
	        if(HazardPerCell[i]==0){eRFPerCell_MAX[i,j]=0}else{eRFPerCell_MAX[i,j]=(BaselinePerCell[i]*WeightVector[j]*DataCountMatrix[i,j])/HazardPerCell[i]}}
	    } # End j. 
	  # Compute the elasticity of each of the j weights in cell j. 
	  for (j in 1:20){ 
	    if(is.na(HazardPerCell[i])){eWPerCell_MAX[i,j]=NA}else{
	      if(HazardPerCell[i]==0){eWPerCell_MAX[i,j]=0}else{eWPerCell_MAX[i,j]=(BaselinePerCell[i]*WeightVector[j]*DataCountMatrix[i,j])/HazardPerCell[i]}}
	  } # End j. 
	} # End i areas.
	
	# Minimums. 
	# Loop through all i spatial cells to find the mins. 
	for (i in 1:FullNumberAreas){
	  # Compute the Baseline Per Spatial Cell. 
	  Distance=as.numeric(SubAdmin_Distance$DistanceAdjustedWithMigration)
	  FRAME=read.csv("/data/CWD_Transition_Probability.csv")
	  model=lm(transition_prob~dist_1,data=FRAME)
	  distance=data.frame(dist_1=Distance[i])
	  BaselinePerCell[i]=as.numeric(max(0,predict(model,newdata=distance)))
	  # Compute the Total Risk Per Spatial Cell. 
	  Distance=as.numeric(SubAdmin_Distance$DistanceAdjustedWithMigration)
	  AverageMovement=as.numeric(SubAdmin_Movement$AverageMovement)
	  DataCountMatrix=SubAdmin_Data[,3:22]
	  WeightVectorNear=as.numeric(Cleaned_Weights$MeanWeightNear) # 20 element vector with all numbers. 
	  WeightVectorFar=as.numeric(Cleaned_Weights$MeanWeightFar) # 20 element vector with all numbers.
	  # Adjust to the maximum weight. 
	  WeightVectorNear=max(0,WeightVectorNear-2*as.numeric(Cleaned_Weights$SDWeightNear))
	  WeightVectorFar=max(0,WeightVectorFar-2*as.numeric(Cleaned_Weights$SDWeightFar))
	  # Nearer scenario. 
	  if(Distance[i]<=AverageMovement[i]) {
	    WeightVector = WeightVectorNear #nh added
	    if (any(is.na(DataCountMatrix[i,]))){TotalPerCell[i]=NA}else{
	      TotalPerCell[i]=as.numeric(sum(WeightVectorNear*DataCountMatrix[i,]))}}
	  # Farther Scenario. 
	  if(Distance[i]>AverageMovement[i]) {
	    WeightVector = WeightVectorFar #nh added
	    if (any(is.na(DataCountMatrix[i,]))){TotalPerCell[i]=NA}else{
	      TotalPerCell[i]=sum(WeightVectorFar*as.numeric(DataCountMatrix[i,]))}}
	  # Compute the Total Hazard Per Cell. 
	  if (is.na(TotalPerCell[i])){HazardPerCell[i]=NA}else{HazardPerCell[i]=BaselinePerCell[i]+BaselinePerCell[i]*TotalPerCell[i]}
	  # Compute the elasticity of the baseline. 
	  if(is.na(HazardPerCell[i])){eBPerCell[i]=NA}else{
	    if(HazardPerCell[i]==0){eBPerCell[i]=0}else{eBPerCell[i]=(BaselinePerCell[i]/HazardPerCell[i])*(1+sum(WeightVector*DataCountMatrix[i,]))}}
	    # Compute the elasticity of each of the j risk factors in cell i.
	    for (j in 1:20){
	      if (is.na(HazardPerCell[i])){eRFPerCell_MIN[i,j]=NA}else{
	        if(HazardPerCell[i]==0){eRFPerCell_MIN[i,j]=0}else{eRFPerCell_MIN[i,j]=(BaselinePerCell[i]*WeightVector[j]*DataCountMatrix[i,j])/HazardPerCell[i]}}
	    } # End j. 
	  # Compute the elasticity of each of the j weights in cell j. 
	  for (j in 1:20){ 
	    if(is.na(HazardPerCell[i])){eWPerCell_MIN[i,j]=NA}else{
	      if(HazardPerCell[i]==0){eWPerCell_MIN[i,j]=0}else{eWPerCell_MIN[i,j]=(BaselinePerCell[i]*WeightVector[j]*DataCountMatrix[i,j])/HazardPerCell[i]}}
	  } # End j. 
	} # End i areas.
	
# PREPARE THE OUTPUT FRAME. -------------------------------
	OutputHazards=cbind(Subadmin,BaselinePerCell,TotalPerCell,HazardPerCell,eBPerCell,eRFPerCell,eWPerCell)
	colnames(OutputHazards)=c("SubAdminID","FullName","BaselineRisk","TotalRisk","TotalHazard",
	                               "e_Baseline",
	                               "e_MineralLicks","e_WaterBodies",
	                               "e_Feedgrounds","e_Guzzlers","e_BaitingStations",
	                               "e_AgriculturePractices","e_CaptiveCervidFacilities",
	                               "e_RehabilitationFacilities","e_Taxidermists","e_Processors",
	                               "e_RenderingFacilities","e_Incinerators","e_FoodBanks",
	                               "e_Landfill","e_Dumping","e_Sheds","e_Roadkill","e_LocalPractices",
	                          "e_FreeRangingCervidDispersal","e_SeasonalMigration",
	                               "ew_MineralLicks","ew_WaterBodies",
	                               "ew_Feedgrounds","ew_Guzzlers","ew_BaitingStations",
	                               "ew_AgriculturePractices","ew_CaptiveCervidFacilities",
	                               "ew_RehabilitationFacilities","ew_Taxidermists","ew_Processors",
	                               "ew_RenderingFacilities","ew_Incinerators","ew_FoodBanks",
	                               "ew_Landfill","ew_Dumping","ew_Sheds","ew_Roadkill","ew_LocalPractices","ew_FreeRangingCervidDispersal","ew_SeasonalMigration")
	
	# Determine which sub-admin areas had insufficient data.
	omitted_areas=OutputHazards[!complete.cases(OutputHazards),]

	# If only a few had insufficient data, report it out. 
	if (dim(omitted_areas)[1]>=1){
	  # Let user know that data frame generation was successful. 
	  line="The dataframe was successfully generated with the set of subadministrative areas that met the data requirements."
	  write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
	  line="However, some subadministrative areas had insufficient data and were therefore removed from this analysis."
	  write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
	  line="Specifically, this analysis did not consider:"
	  write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
	  line=print(omitted_areas$FullName)
	  write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)
	  line="You can proceed with the partial outputs, or you can return to the CWD Data Warehouse and input more data, or select a new set of risk factors."
	  write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)}
	
	# Check to see that at least one subadmin area has sufficient data. If not, kill the code. 
	if (dim(omitted_areas)[1]==FullNumberAreas){
	  line="We're sorry. All subadministative areas had insufficient data for this analysis. Return to the CWD Data Warehouse and input more data, or select a new set of risk factors."
	  write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE) 
	  # Quit the session.
	  quit(status=70)}
	
	# Check to see if all areas were eligible for analysis. 
	if (dim(omitted_areas)[1]==0){
	  # Let user know that data frame generation was successful. 
	  line="The data frame was successfully generated for all subadministrative areas."
	  write(html_tag_line(line, "p"),file=model_log_filepath,append=TRUE)}
	
# WRITE THE FILES BACK TO THE WAREHOUSE. -----------------
	OutputHazards_CompleteCases=OutputHazards[complete.cases(OutputHazards),]
	  # Write the file to the attachments directory so it can be picked up by the next script
	  output_csv_path = file.path("", "data", "attachments", "OutputHazards.csv")
	  write.csv(OutputHazards_CompleteCases, output_csv_path, row.names = FALSE)
	  
	  # Register the attachment in the JSON manifest
	  attachment_metadata = c(filename="OutputHazards.csv", content_type="text/csv", role="downloadable")
	  add_item_to_json_array(json_attachment_file, attachment_metadata)