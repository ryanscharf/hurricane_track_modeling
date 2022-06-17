#find all T:/ files
#convert data folder to temp directory 




#todo - add best track to the tail end of each cluster 

#####https://ftp.nhc.noaa.gov/atcf/aid_public/?C=M;O=D
#aal_file <- "Z:/Catastrophe Modeling/Cat_Claims/CatPlan/2017/Scripts/RScripts/Clustering/Data/aal142016.dat/aal142016.dat"
#aal_file <- "T:/Private/R_ai/OperationalClusterLeaflet/aal052019.dat"
#aal_dir <-  "T:/Private/R_ai/OperationalClusterLeaflet/"

#getURL(url = 'ftp://ftp.nhc.noaa.gov/atcf/aid_public', userpwd = 'anonymous:rscharf@aiicfl.com',dirlistonly = T, .opts = list(timeout = 1, maxredirs = 2, verbose = TRUE))

#https://www.ecmwf.int/en/forecasts/datasets/set-iii#III-viii
#https://apps.sfwmd.gov/sfwmd/common/images/weather/ecmwf_ens.txt

# Month-Day-Hour to work with
Hour_to_process  <-  "06"            ###UPDATE 00, 06, 12, or 18
Day_to_process   <-  "27"            ###UPDATE
Month_to_process <-  "08"            ###UPDATE
`Storm#`         <-  "09"            ###UPDATE
Year             <-  '2021' 
RMAX             <- 29                #from https://rammb-data.cira.colostate.edu/tc_realtime/
intensity_mod <- 1.4
fcst_hr <- 120
only_current_models <- F
decay_windspeeds <- T

# Number of Clusters
nclusters <- 4

#data folder for modeling files
data_folder <- 'data/'

source('utils.R')

necessary_packages <-
  c(
    "sf",
    "htmltools",
    "htmlwidgets",
    "RColorBrewer",
    "leaflet",
    "tidyverse",
    'maps',
    'mapdata',
    'maptools',
    "rgeos",
    'lubridate',
    'zoo',
    'AIpkg',
    'colorspace'
  )
lapply(necessary_packages, require, character.only = TRUE)

download.file(paste0("ftp://ftp.nhc.noaa.gov/atcf/aid_public/aal",`Storm#`,Year,".dat.gz"), 
              paste0("T:/Private/R_ai/OperationalClusterLeaflet/aal",`Storm#`, Year, ".dat.gz"),
              method="libcurl")
#bAL file
download.file(paste0("ftp.nhc.noaa.gov/atcf/btk/bal",`Storm#`,Year,".dat"), 
              paste0("T:/Private/R_ai/OperationalClusterLeaflet/bal",`Storm#`, Year, ".dat"),
              method="libcurl")


aal_file <- paste0("T:/Private/R_ai/OperationalClusterLeaflet/aal",`Storm#`, Year, ".dat.gz")

#https://www.nrlmry.navy.mil/atcf_web/docs/database/new/abdeck.txt
bal_file <- f_Read_ATCF_AAL(paste0("https://ftp.nhc.noaa.gov/atcf/btk/bal",`Storm#`,Year,".dat"))

preliminary_best_track_url <- paste0('https://www.nhc.noaa.gov/gis/best_track/al',`Storm#`, Year, '_best_track.zip')
download.file(preliminary_best_track_url, 
              paste0("T:/Private/R_ai/OperationalClusterLeaflet/best_track",`Storm#`, Year, ".zip"),
              method="libcurl")
tmp_file <- tempfile()

unzip(
  zipfile = paste0("T:/Private/R_ai/OperationalClusterLeaflet/best_track",`Storm#`, Year, ".zip"),
  exdir = tmp_file)
best_track_shp <- list.files(tmp_file, pattern = 'lin.shp$', full.names = T)
best_track_shp <- read_sf(best_track_shp) %>% st_transform(4326)

#tempfn <-  str_split(aal_file, "/")[[1]][5] %>% str_sub(0,9)
#Year <- str_sub(tempfn, 6, 9)

# Model Description
#https://web.uwm.edu/hurricane-models/models/models.html
Model_description <- "T:/Private/Risk2/CatPlan/2018/Scripts/RScripts/Forcasting/Model_List.csv"

# Colors For Clusters
col_clusters <- c("deeppink","darkseagreen", "deepskyblue", "magenta", "orange")
col_meanclusters <- c("darkred","darkgreen", "darkblue", "darkmagenta", "darkorange")
#--------------------------------
# FUNCTIONS
#--------------------------------
print(" Loading the functions")
print("-------------------------")
source("//aiig/risk/Private/R_ai/R_SQL/Functions/f_Read_ATCF_AAL.R")

#----------------------------------------------------------------------------
## BEGINNING OF PROGRAM
#----------------------------------------------------------------------------
daModel <- read.csv(Model_description, as.is = TRUE)
print("Now loading AAL file")
daATCF <- f_Read_ATCF_AAL(aal_file) 

# %>% mutate(
#   Year = as.integer(Year),
#   Month = as.integer(Month),
#   Day = as.integer(Day),
#   Hour = as.integer(Hour)
# )

euro_tracks <- get_euro_tracks() %>% filter(model_type == 'ENSEMBLE' & storm == `Storm#`)

euro_tracks <- euro_tracks %>% mutate(
  Basin = 'AL',
  Storm_Number_in_Year = as.integer(storm),
  #StormName = ,
  Year = str_sub(rundate, start = 1, end = 4),
  Month = str_sub(rundate, start = 5, end = 6),
  Day = str_sub(rundate, 7,8),
  Hour = init_time,
  ModelName = paste0(model,ensemble_member),
  TAU,
  Status = NA,
  Lat,
  Lon,
  Max_winds_kts = NA,
  Max_winds_mph = NA,
  Min_SLP_mb = NA
) %>% 
  select(-model, -model_type, -rundate, -init_time, -storm, -ensemble_member)

daATCF <- bind_rows(daATCF, euro_tracks) %>% mutate(
  Max_winds_kts = na_if(Max_winds_kts, 0),
  Max_winds_mph = na_if(Max_winds_mph, 0),
  Min_SLP_mb = na_if(Min_SLP_mb, 0)
)

NHCintensityModels <- c('FSSE', 'HCCA','GFEX','RVCN','ICON','IVCN','NVGM','NVGI','AVNO','AVNI','GFSO','GFSI','EMX','EMXI','EMX2','EGRR','EGRI','EGR2','CMC','CMCI','HWRF','HWFI','CTCX','CTCI','HMON','HMNI','CLP5','SHF5','DSF5','TCLP','SHIP','DSHP','LGEM')

Sys.time()
models_per_z <- daATCF %>% filter(Day == Day_to_process) %>% count(Hour)

Name <- daATCF$Name
# for now will hard-wire to forecast time of 2016092500
# inc_hardtime <- which((daATCF$Month == Month_to_process) & (daATCF$Day == Day_to_process) & (daATCF$Hour == Hour_to_process))
# daATCF <- daATCF[inc_hardtime,]

latest_run <-
  daATCF %>% filter(Month == Month_to_process, Day == Day_to_process) %>% 
  summarize(Hour = max(Hour),
  time = ymd_h(paste0(Year, Month, Day, Hour, sep = '-'))
  ) %>% distinct()

daATCF <- daATCF %>% 
  group_by(ModelName) %>% 
  filter(Month == max(Month)) %>% 
  filter(Day == max(Day)) %>% 
  filter(Hour == max(Hour)) %>% ungroup() %>%
  #distinct(ModelName, Month, Day, Hour) %>%
  mutate(run_vintage = case_when(
    Month == Month_to_process &  Day == Day_to_process &
    Hour == latest_run$Hour ~ 'current',
    T ~ 'old'
    ),
    time = ymd_h(paste0(Year, Month, Day, Hour, sep = '-')),
    forecast_time = time + hours(TAU)  ,
    TAU = as.integer(difftime(forecast_time, latest_run$time ,units = 'hours'))
    )

#for old runs, filter out forecast times that are earlier than the most current run times
daATCF <- daATCF %>% filter(forecast_time >= latest_run$time)

if (only_current_models == T){
  daATCF <- daATCF %>% filter(run_vintage == 'current')
}


#-----------------------------
# 1) Mainly clean up the data
#-----------------------------
print("Cleaning the data: removing duplicate, intensity models, ...")

# List of models to keep for now
inc_XTRP <- which(daATCF$ModelName == "XTRP")
daATCF <- daATCF[-inc_XTRP,]

# remove intensity models
list_model_names <- unique(daATCF$ModelName)
inc_list_intensity_models <- NULL
for (i in 1:length(list_model_names)) {
  inc <- which(daATCF$ModelName == list_model_names[i])
  if (sum(daATCF$Lat[inc],na.rm = T) ==0)
    inc_list_intensity_models <- c(inc_list_intensity_models,inc)
}

daATCF <- daATCF[-inc_list_intensity_models,]
# remove duplicated rows
daATCF <- daATCF[!duplicated(daATCF),]

#get rid of TAUs that revert to lat/long origin
daATCF <- daATCF %>% filter(!Lat  == 0 & !Lon == 0) 

#------------------------------------
# Print the list of remaining models
#------------------------------------
inc_modelmatch <- match(daATCF$ModelName, daModel$Model_Abbreviation)
daATCF$ModelDescription <- daModel$Model_Description[inc_modelmatch]
temp_model_list <- cbind(daATCF$ModelName,daATCF$ModelDescription)
temp_model_list <- temp_model_list[!duplicated(temp_model_list),]

daATCF_OFCL <- daATCF[which( (daATCF$ModelName == "OFPI") & (daATCF$TAU == 0)), ]
if (nrow(daATCF_OFCL) == 0) {
  daATCF_OFCL <- daATCF[which( (daATCF$ModelName == "CARQ") & (daATCF$TAU == 0)), ]
}

# Add hour 0 to model who start at some other hour
for (imod in 1:nrow(temp_model_list)) {
  inc_mod <- which(daATCF$ModelName == temp_model_list[imod,1])
  daTemp <- daATCF[inc_mod,]
  
  if ( (min(daTemp$TAU >0)) | (is.na(daTemp$TAU) == TRUE) ) {
    
    daATCF <- daATCF[-inc_mod,]
    daTemp <- rbind(daATCF_OFCL,daTemp)
    daTemp$ModelName[1] <- daTemp$ModelName[2]
    
    daATCF <-  rbind(daATCF,daTemp)
    
  }
}

#print(temp_model_list) 
#hist(daATCF$TAU)
print(paste("You have ", nrow(temp_model_list), "unique models after clean up") )

#------------------------------------
# keep a version with only models that go to fcsthr hours forecast with a cap at fcsthr
print("Now filetering only the model that extend to the desired forecast hour")
# daATCF_fcsthr <- daATCF[1,]
# 
# for (i in 1:nrow(temp_model_list)) {
#   # i=i+1;print(i)
#   inc <- which(daATCF$ModelName == temp_model_list[i,1])
#   
#   if (max(daATCF$TAU[inc]) >=fcst_hr) {
#     inc_fcsthr <- which(daATCF$TAU[inc] == fcst_hr)
#     daATCF_fcsthr <- rbind(daATCF_fcsthr,daATCF[inc[1:inc_fcsthr],])
#   }
# }
# daATCF_fcsthr <- daATCF_fcsthr[-1,]
# daATCF <- daATCF_fcsthr
daATCF <- daATCF %>% group_by(ModelName) %>% filter(max(TAU) >= fcst_hr) %>%
  ungroup() 

daATCF <- daATCF %>% filter(!is.na(ModelName))

temp_model_list <- cbind(daATCF$ModelName,daATCF$ModelDescription)
temp_model_list <- temp_model_list[!duplicated(temp_model_list),]
#print(temp_model_list) 

#hist(daATCF$TAU)
print(paste("You have ", nrow(temp_model_list), "unique models after the hour filtering"))

models_to_process <- unique(daATCF$ModelName)
nmodels_to_process <- length(unique(daATCF$ModelName))

# Interpolate all the tracks to the same time
unique_hrs <- sort(unique(daATCF$TAU))

daATCFI <- NULL


for (imod in 1:nmodels_to_process) {
  
  inc_model       <- which(daATCF$ModelName == models_to_process[imod])
  temp_model_name <- daATCF$ModelName[inc_model[1]]
  temp_TAU        <- daATCF$TAU[inc_model] 
  temp_Lat        <- daATCF$Lat[inc_model] 
  temp_Lon        <- daATCF$Lon[inc_model] 
  temp_Max_winds_kts        <- daATCF$Max_winds_kts[inc_model]
  temp_Max_winds_mph        <- daATCF$Max_winds_mph[inc_model]
  temp_Min_SLP_mb           <- daATCF$Min_SLP_mb[inc_model]
  temp_ModelDescription           <- daATCF$ModelDescription[inc_model[1]]
  
  daTempI        <- data.frame(cbind(temp_model_name,temp_TAU,temp_Lat,temp_Lon,temp_Max_winds_kts,
                                     temp_Max_winds_mph,temp_Min_SLP_mb,temp_ModelDescription), stringsAsFactors = FALSE)
  names(daTempI) <- c("ModelName" , "TAU" ,"Lat" , "Lon" ,"Max_winds_kts" ,"Max_winds_mph" ,"Min_SLP_mb", "ModelDescription")
  
  if (length(temp_TAU) != length(unique_hrs) ) {
    #print(forecach)
    temp_LatI        <- approx(temp_TAU,temp_Lat , unique_hrs)$y
    temp_LonI        <- approx(temp_TAU,temp_Lon , unique_hrs)$y
    temp_Max_winds_ktsI        <- approx(temp_TAU,replace_na(temp_Max_winds_kts,0) , unique_hrs)$y
    temp_Max_winds_mphI        <- approx(temp_TAU, replace_na(temp_Max_winds_mph,0) , unique_hrs)$y
    temp_Min_SLP_mbI        <- approx(temp_TAU, replace_na(temp_Min_SLP_mb,0) , unique_hrs)$y
    
    daTempI        <- data.frame(cbind(temp_model_name[1],unique_hrs,temp_LatI,temp_LonI,temp_Max_winds_ktsI,temp_Max_winds_mphI,
                                       temp_Min_SLP_mbI,temp_ModelDescription[1]), stringsAsFactors = FALSE)
    names(daTempI) <- c("ModelName" , "TAU" ,"Lat" , "Lon" ,"Max_winds_kts" ,"Max_winds_mph" ,"Min_SLP_mb", "ModelDescription")
  } 
  
  daATCFI <- rbind(daATCFI,daTempI)
  
}

daATCFI$ModelName <- as.character(daATCFI$ModelName)
daATCFI$TAU       <- as.integer(daATCFI$TAU)
daATCFI$Lat       <- as.numeric(daATCFI$Lat)
daATCFI$Lon       <- as.numeric(daATCFI$Lon)
daATCFI$Max_winds_kts       <- as.numeric(daATCFI$Max_winds_kts)
daATCFI$Max_winds_mph       <- as.numeric(daATCFI$Max_winds_mph)
daATCFI$Min_SLP_mb       <- as.numeric(daATCFI$Min_SLP_mb)
daATCFI$ModelDescription <- as.character(daATCFI$ModelDescription)


daATCF <- daATCFI

daATCF <- daATCF %>% filter(TAU <= fcst_hr)
#-----------------------------------
# Start the clustering
#-----------------------------------
# Add an Sid
daATCF$Sid <- 0
daATCF$Mid <- 0

for (i in 1:nmodels_to_process) {
  inc <- which(daATCF$ModelName == models_to_process[i])
  daATCF$Sid[inc] <- i
  daATCF$Mid[inc] <- round(length(inc)/2)
}

# splits the data by IDs to look at one storm at the time - this return a list
daATCF.split = split(daATCF, daATCF$Sid)

# for each track only display, beginning, end location, max wind speed and location of max wind speed
daATCF.c = data.frame(matrix(0, nrow=nmodels_to_process, ncol =7))
colnames(daATCF.c) = c("FirstLon", "FirstLat",
                       "LastLon" , "LastLat", "WmaxS", "MidLon", "MidLat")

for (i in 1:nmodels_to_process) {
  inc <- which(daATCF$ModelName == models_to_process[i])
  temp<- max(round(length(inc)/2),1)
  daATCF.c$FirstLon[i] <- daATCF$Lon[inc[1]]
  daATCF.c$FirstLat[i] <- daATCF$Lat[inc[1]]
  
  daATCF.c$LastLon[i] <- daATCF$Lon[inc[length(inc)]]
  daATCF.c$LastLat[i] <- daATCF$Lat[inc[length(inc)]]
  
  daATCF.c$MidLon[i] <- daATCF$Lon[inc[temp]]
  daATCF.c$MidLat[i] <- daATCF$Lat[inc[temp]]
  daATCF.c$WmaxS[i] <- daATCF$Max_winds_kts[inc[temp]]
}

mean_wmax <- daATCF.c %>% filter(WmaxS > 0) %>% summarize(m = mean(WmaxS)) %>% pull(m)
####replace 0 windspeedS?
daATCF.c <- daATCF.c %>% mutate(
  WmaxS = case_when(WmaxS == 0 ~ mean_wmax,
  T ~ WmaxS)
)
  

# before clustering you check the feature variances by applying the "var" function
sapply(daATCF.c, var)

# > sapply(daATCF.c, var)
# FirstLon  FirstLat   LastLon   LastLat     WmaxS    MidLon    MidLat 
# 139.51615   7.44032 316.16006 170.93274 968.31181 285.08989  69.56073 

#if the first lat/longs are all identical, then the variance and the sd will be 0 and there will be an error
#because of this, we'll add noise to the first lats and longs

daATCF.c$FirstLon <- jitter(daATCF.c$FirstLon, .1)
daATCF.c$FirstLat <- jitter(daATCF.c$FirstLat, .1)


daATCF.cs = scale(daATCF.c) 
m = attr(daATCF.cs, "scaled:center")
s = attr(daATCF.cs, "scaled:scale")

# the function "scale" centers and scales the columns of your numeric data frame. The center and scale values are saved as attributes in the data frame. 
# Here you save them to rescale the centroids after the cluster analysis 
k = nclusters 
set.seed(5426)
ct = kmeans(daATCF.cs, centers= k)
summary(ct)

a <- 0.00
b <- 0.00
kmeans_rpt <- data.frame()
for(i in 1:10){
  c <- a
  d <- b
  set.seed(5426)
  res <- kmeans(daATCF.cs, centers= i, iter.max = 50, nstart = 10)

  a <- res$betweenss
  kmeans_rpt[i, 'k'] <- i
  kmeans_rpt[i, 'betwenness'] <- a
  b <- res$totss
  kmeans_rpt[i, 'total_sum_of_squares'] <- b
  kmeans_rpt[i, 'information_retained'] <- round(a/b,2)
  kmeans_rpt[i, 'chg_IR'] <- round((a/b)-(c/d),2)
  #cat(paste0('K: ', round(i,2),'\t', round(a/b,2), '\t', round((a/b)-(c/d),2), '\n'))
  if(i == 10){print(select(kmeans_rpt, information_retained, chg_IR))}
}
par(mfrow = c(2,1))
plot(y = kmeans_rpt$information_retained, x = kmeans_rpt$k, pch = 16, lwd = 2, type = 'b')
plot(x = kmeans_rpt$k, y = kmeans_rpt$chg_IR, lwd = 2, lty = 1, type = 'b')
# k-means clsuter analysis setting the number of clsuters to 3
# > ct$betweenss/ct$totss
# [1] 0.7133559
# this ratio will increase with the number of clusters, but at the expense of having clusters that may not be interpretable
#With 4 clusters, the increase in this ratio is smaller than the increase going from 2 to 3 clusters SO you are content with 3 cluster results

ctrs = ct$center[ct$cluster,]      # add cluster membership to original data frame
cln  = ct$cluster                  # shows to which cluster each track belongs
dist = rowMeans((daATCF.cs - ctrs)^2) # check the distance between the cluster and the original data frame
id = 1:length(dist)

daATCF.c = data.frame(daATCF.c , id = id, dist = dist, cln = cln)
daATCF.c.split = split(daATCF.c, daATCF.c$cln)                # split the data by cluster member

daATCF$ClusterNumber <- 0
for (i in 1:nmodels_to_process) {
  cid = which(daATCF$ModelName == models_to_process[i])
  daATCF$ClusterNumber[cid] <- cln[i]
}


# Next you subset your clsuter data based on the tracks tahat come closest to the cluster centroids. 
# That closeness occurs in feature space that includes latitude,longitude, and intensity features. 
# Here you choose 9 tracks for each centroid

te =5
daATCFid = unlist(lapply(daATCF.c.split, function(x)
  x$id[order(x$dist)[1:te]]))

cinfo = subset(daATCF.c, id %in% daATCFid)

cyclones = daATCF.split
#uselat = range(unlist(lapply(cyclones[cinfo$id],
#                             function(x) x$Lat)))
#uselon = range(unlist(lapply(cyclones[cinfo$id],
#                             function(x) x$Lon)))

#cinfo = cinfo[order(cinfo$cln, cinfo$dist),]

#create a line for each cluster
daOutTrack <- NULL
for (icl in 1:nclusters) {
  
  inc        <- which(daATCF$ClusterNumber ==icl)
  daTemp     <- daATCF[inc,]
  ninc       <- length(unique(daTemp$Sid))
  unique_Sid <- unique(daTemp$Sid)
  unique_hrs <- sort(unique(daTemp$TAU ))
  
  #create daMidTrack from scratch
  daMidTrack        <- data.frame(matrix(0,ncol = 8, nrow = length(unique_hrs)))
  names(daMidTrack) <- c("TAU", "Lon", "Lat","Max_winds_kts","Min_SLP_mb", "Ntracks", "NWinds", "NSLP")
  daMidTrack$TAU    <- unique_hrs
  
  for(i in 1:ninc) {
    cid = which(daTemp$Sid == unique_Sid[i])
    cyclone = daTemp[cid,]
    #find the index of matching TAUs, should be the same as cid?
    inc_match_hours <- match(daMidTrack$TAU,cyclone$TAU)
    # if (is.na(sum(inc_match_hours)) == TRUE) {print(forbreak)}
    
    #get the longs for the ith model
    temp_lon <- cyclone$Lon[inc_match_hours];
    #get the lats for the ith model
    temp_lat <- cyclone$Lat[inc_match_hours];
    #ge the max winds for the ith model
    temp_winds <- cyclone$Max_winds_kts[inc_match_hours];
    #get the surface pressure for the ith model
    temp_slp <- cyclone$Min_SLP_mb[inc_match_hours];
    #temp object to check for NAs
    inc_na   <- which(is.na(temp_lon) == TRUE);
    #assign NAs to 0s for all these temp objects
    temp_lon[inc_na] <- 0;
    temp_lat[inc_na] <- 0;
    temp_winds[inc_na] <- 0;
    temp_slp[inc_na] <- 0;
    #print out 1 for the nrows of the model?
    N <- rep(1,length(temp_lon));
    N[inc_na] <- 0
    
    daMidTrack$Lon  <- daMidTrack$Lon + temp_lon
    daMidTrack$Lat  <- daMidTrack$Lat + temp_lat
    daMidTrack$Ntracks  <- daMidTrack$Ntracks + N
    
    #update winds and pressure. if value exists, increment count
    if (sum(cyclone$Max_winds_kts) >0 ) {
      daMidTrack$Max_winds_kts  <- daMidTrack$Max_winds_kts + temp_winds
      daMidTrack$NWinds  <- daMidTrack$NWinds + N
    }
    if (sum(cyclone$Min_SLP_mb) >0 ) {
      daMidTrack$Min_SLP_mb  <- daMidTrack$Min_SLP_mb + temp_slp
      daMidTrack$NSLP        <-   daMidTrack$NSLP + N
    }
  }
  
  #average the lat,lon, wind, pressure
  daMidTrack$Lon  <- daMidTrack$Lon/daMidTrack$Ntracks
  daMidTrack$Lat  <- daMidTrack$Lat/daMidTrack$Ntracks
  daMidTrack$Max_winds_kts  <- daMidTrack$Max_winds_kts/daMidTrack$NWinds * intensity_mod
  daMidTrack$Min_SLP_mb  <- daMidTrack$Min_SLP_mb/daMidTrack$NSLP
  
  #  lines(daMidTrack$Lon, daMidTrack$Lat, lwd=5,
  #        col= col_meanclusters[icl])
  
  daMidTrack$ClusterNumber <- icl
  daOutTrack <- rbind(daOutTrack,daMidTrack)
  #daOutTrack <- daOutTrack %>% filter(TAU <= 150)
}


########fancydoodads
cyclonesdf <- bind_rows(cyclones)



#take the list of models, subset to those which we use, assign unique colors to them
subdaModel <- daModel %>% filter(Model_Abbreviation %in% cyclonesdf$ModelName) %>% 
  mutate(color = var_to_color(., Model_Parent))
#make a df to be the circleMarkers then join it with model_parent for the colors
markers <- cyclonesdf %>% left_join(subdaModel, by = c("ModelName" = "Model_Abbreviation"))
#create end-point lables for marker
markerlabels <- markers %>% filter( TAU == max(TAU))
markerlabelshtml <- paste0("<font color =\"", markerlabels$color, "\">", markerlabels$ModelName, "</font>") %>% lapply(htmltools::HTML)
#take the df of coords and cast it to a sf object, then cast to a LINESTRINGs, then join
#on the Model_Parent & color data
modellines <- cyclonesdf %>% st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>% group_by(ModelName) %>% 
  summarise(do_union = F) %>% st_cast("LINESTRING") %>% left_join(subdaModel, by = c("ModelName" = "Model_Abbreviation"))

daSum        <- data.frame(matrix(0, ncol = 3, nrow= nclusters))
names(daSum) <- c("Ntracks", "ClusterNumber", "Color" )

for (j in 1:nclusters) {
  inc <- which(daOutTrack$ClusterNumber ==j)
  daSum$Ntracks[j]       <- daOutTrack$Ntracks[inc[1]]
  daSum$ClusterNumber[j] <- daOutTrack$ClusterNumber[inc[1]]
  daSum$Color[j] <- col_meanclusters[j]
}

daSum$Probability <- 100*daSum$Ntracks/sum(daSum$Ntracks)
daOutTrack <- daOutTrack %>% left_join(daSum, by = "ClusterNumber")
daOutTrack <- mutate(daOutTrack, Max_winds_mph = Max_winds_kts * 1.15077945)




#cast cluster poitns to sf object then linestrings
clusterLines <- daOutTrack %>% st_as_sf(coords = c("Lon", "Lat"), crs = 4326) %>% group_by(ClusterNumber) %>%
  dplyr::summarize(do_union = F, Color = unique(Color)) %>% st_cast("LINESTRING")

daoutlabels <- filter(daOutTrack, TAU == max(TAU))
daoutlabels <- paste0("<strong><font color =\"",daoutlabels$Color, "\">Cluster ", daoutlabels$ClusterNumber, "<br/>Members: ", daoutlabels$Ntracks.x, 
                      "<br/>Probability : ", trunc(daoutlabels$Probability), "%</font></strong>") %>% lapply(htmltools::HTML)

leaflet_title <- paste0('<strong>AL', `Storm#`, ' Valid for:', ' at ', Year, '/', Month_to_process, '/', Day_to_process, '-', Hour_to_process, 'z</strong><br>',
                        Sys.time())
#making the map
mcleaflet <-
  leaflet() %>% addTiles(group = "OSM") %>%  addProviderTiles(group = "Stamen.TonerLite", "Stamen.TonerLite") %>%
  addProviderTiles(group = "Esri.WorldImagery", "Esri.WorldImagery") %>% addProviderTiles(group = "Esri.OceanBasemap", "Esri.OceanBasemap") %>%
  addGraticule(
    group = "BaseGroup",
    interval = 5,
    style = list(weight = 1, opacity = .5)
  ) %>%
  
  #change markers to cyclonesdf, rejoin in the other data?
  addLabelOnlyMarkers(
    group = "Models",
    data = markerlabels,
    label = markerlabelshtml,
    labelOptions = labelOptions(
      noHide = T,
      textOnly = T,
      style = list("opacity" = ".2")
    )
  ) %>%
  
  addCircleMarkers(
    group = "Models",
    data = markers,
    lat = ~ Lat,
    lng = ~ Lon,
    radius = 1,
    color = ~ color,
    opacity = .4,
    popup = paste0(
      "<strong>Model: ",
      markers$ModelName,
      "</strong><br/>lat: ",
      markers$Lat,
      ", lon: ",
      markers$Lon,
      "<br/>Max Winds (mph): ",
      round(markers$Max_winds_mph),
      "<br/>Forecast Hr: ",
      markers$TAU
    )
  ) %>%
  
  addPolylines(
    data = modellines,
    group = "Models",
    weight = 2,
    opacity = .4,
    color = ~ color,
    options = pathOptions(clickable = FALSE)
  ) %>%
  
  addLabelOnlyMarkers(
    group = "Clusters",
    data = filter(daOutTrack, TAU == max(TAU)),
    label = daoutlabels,
    labelOptions = labelOptions(
      noHide = T,
      textOnly = T,
      direction = "top",
      offset = c(30, 0)
    )
  ) %>%
  
  addCircleMarkers(
    group = "Clusters",
    data = daOutTrack,
    lat = ~ Lat,
    lng = ~ Lon,
    radius = 2,
    opacity = 1,
    color = ~ Color,
    popup = paste0(
      "<strong>Cluster: ",
      daOutTrack$ClusterNumber,
      "</strong><br/>Forecast Hour: ",
      daOutTrack$TAU,
      "<br/>lat: ",
      round(daOutTrack$Lat, 1),
      ", lon: ",
      round(daOutTrack$Lon, 1),
      "<br/>Max Winds (mph): ",
      round(daOutTrack$Max_winds_mph)
    )
  ) %>%
  
  addPolylines(
    data = clusterLines,
    group = "Clusters",
    color = ~ Color,
    weight = 5,
    opacity = 1,
    options = pathOptions(clickable = FALSE)
  ) %>%
  
  addLayersControl(
    overlayGroups = c(
      "Models",
      "Clusters",
      "OSM",
      "Stamen.TonerLite",
      "Esri.WorldImagery",
      "Esri.OceanBasemap"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  #addPolylines(data =  best_track_shp) %>%
  addControl(leaflet_title, position = "topleft" )

#addLegend("bottomleft", pal = markers$color)

#RVCN not present in daModel

#unnamed/early storms will not have a best track shape to map
if(is_empty(best_track_shp)){
  mcleaflet} else {
  mcleaflet %>% addPolylines(data =  best_track_shp,
                             color = 'black',
                             opacity = 1) }



#saveWidget(mcleaflet,"mcleaflet.html")
cat(
  'Number of total Contributors: \t',
  daOutTrack %>% 
    distinct(ClusterNumber, Ntracks.x) %>% 
    summarize(sum(Ntracks.x)) %>% 
    pull()
)

distinct(daATCF, ModelName, ClusterNumber) %>% arrange(ClusterNumber) %>% print()

daATCF %>% distinct(ModelName, ClusterNumber) %>% count(ClusterNumber) %>% print()

print(models_per_z)
print(paste0('Intensity Modifier: ', intensity_mod))
# Save Cluster Tracks -----------------------------------------------------


#the forecast hours will have to be converted to days and hours
# fcsthr2days <- function(x){
#   x$Day <- as.numeric(x$Day) + trunc(x$Hour/24)
#   x$Hour <- x$Hour %% 24
#   x$dirtdat <- as.Date(x$Day - 1, paste0(x$Year,"-",x$Month, "-01"))
#   x$Day <- str_sub(x$dirtdat, 9,10)
#   x$Month <- str_sub(x$dirtdat, 6,7)
#   x$Year <- str_sub(x$dirtdat, 1,4)
#   return(x)
# }
# 
# #change insensity for a cluster
# daOutTrack <-
#   daOutTrack %>%
#   mutate(Max_winds_mph =
#            case_when(
#              ClusterNumber == 1 ~ Max_winds_mph * .6,
#              T ~ Max_winds_mph))
# 
# #copy cluster intensity
# wanted_intensity <-
#   daOutTrack %>%
#   filter(ClusterNumber == 3) %>% pull(Max_winds_mph)
# 
# daOutTrack %>% mutate(Max_winds_mph = 
#                         case_when(
#                           ClusterNumber == 1 ~ wanted_intensity
#                         )
#                       )



#values for kcc clipboard import
#SeasonYear, Storm#, Name, Year, Month, Day, Hour, Latitude, Longitude, WindMax, Rmax, CP


# diagnostic plots --------------------------------------------------------

#check windspeeds over time for a cluster
daATCF %>% #filter(ClusterNumber == 2) %>% 
  group_by(TAU, ClusterNumber) %>% 
  summarize(mean(Max_winds_mph), 
            median(Max_winds_mph), 
            n()) %>%
  mutate(ClusterNumber = as.factor(ClusterNumber))%>%
  ungroup() %>% pivot_longer(
  cols = c('mean(Max_winds_mph)', 
           'median(Max_winds_mph)'),
  names_to = 'values'
) %>% 
  #group_by(ClusterNumber, values) %>% 
  ggplot() + 
  geom_line(aes(
    x = TAU,
    y = value,
    color = ClusterNumber,
    linetype = values,
    group = interaction(as.factor(ClusterNumber), values)
  ), lwd = 2) + labs( y = 'Max Winds (mph)', title = 'Max Winds Across Cluster Members')


