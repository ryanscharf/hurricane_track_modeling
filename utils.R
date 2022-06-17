
#' make_dfs
#' splits a dataframe into multiple dataframes by row indices
#'
#' @param df dataframe to be split
#' @param x starting index
#' @param y ending index
#'
#' @return a dataframe
#' @export
#'
#' @examples
make_dfs <- function(df, x, y){
  from <- x
  to <- y
  df <- data.frame(
    df[from:to,]
  )
  storm_info <- df[1,1]
  df <- df %>% mutate(storminfo = storm_info) %>%
    slice(-1)
  
  return(df)
}

#' get_euro_tracks
#' Pulls some track information that is hidden on SFWMD's website.
#' This only will populate for storms that are currently active.
#' 
#' @return a dataframe of ensemble members & coordiantes
#' @export
#'
#' @examples \NOTRUN get_euro_tracks()
get_euro_tracks <- function(){
  ECMWF_sfwmd <- read_delim('https://apps.sfwmd.gov/sfwmd/common/images/weather/ecmwf_ens.txt', delim = '\t')
  
  #need to  find index of each run vintage
  data_details <- names(ECMWF_sfwmd)
  names(ECMWF_sfwmd) <- 'data'
  
  index_mvintage_x <- which(
    str_detect(ECMWF_sfwmd[[1]], 'ECMWF')
  )
  index_mvintage_y <- index_mvintage_x[-1] - 1
  index_mvintage_y <- append(index_mvintage_y, nrow(ECMWF_sfwmd))
  indicies_mvintage <- data.frame(from = index_mvintage_x, 
                                  to = index_mvintage_y,
                                  run = ECMWF_sfwmd[index_mvintage_x,])
  ECMWF_sfwmd[,'run'] <- NA_character_
  
  for(i in 1:nrow(indicies_mvintage)){
    ECMWF_sfwmd[indicies_mvintage[i, 'from'] : indicies_mvintage[i, 'to'], 'run'] <- indicies_mvintage[i, 'data']
  }
  
  ECMWF_sfwmd<- ECMWF_sfwmd[-index_mvintage_x,]
  
  index_members <- which(
    str_detect(ECMWF_sfwmd[[1]], 'STORM')
  )
  index_y <- index_members[-1] -1
  last_index <- nrow(ECMWF_sfwmd)
  index_y <- append(index_y, last_index)
  
  ecmwf_search_params <- data.frame(x = index_members,
                                    y = index_y)
  
  euro_df <- 
    purrr::map2(ecmwf_search_params$x, 
                ecmwf_search_params$y, 
                ~make_dfs(df = ECMWF_sfwmd, .x, .y)) %>% 
    bind_rows() 
  
  
  euro_df <- euro_df %>%
    mutate(splits = str_split(data, ' ', 3, simplify = T),
           TAU = as.integer(splits[,1]), 
           model = 'ECMWF',
           Lat = as.numeric(splits[,2]), 
           Lon = as.numeric(splits[,3]),
           model_type = str_split(run, ' ',simplify = T)[,2],
           rundate = str_extract(run, '[:digit:]{10}'),
           init_time = str_sub(rundate, start = -2),
           rundate = str_sub(rundate, 1, 8),
           storm = str_extract(storminfo, '(?<=STORM_)[:digit:]{2}'),
           ensemble_member = str_pad(str_extract(storminfo, '(?<=_)[:digit:]+$'), 2, 'left', 0)
           
    ) %>%
    select(-splits, -data, -storminfo, -run)
  
  return(euro_df)
  
}


#' decay_tracks
#' decays windspeeds of storm while over land.
#' does NOT properly consider multiple-landfalling tracks
#'
#' @param df_decay dataframe of track coordinates to decay
#'
#' @return dataframe of decayed tracks
#' @export
#'
#' @examples
decay_tracks <- function(df_decay = .) {
  
  if (dplyr::is_grouped_df(df_decay)) {
    indices <- group_indices(df_decay) 
    df_decay      <- do(df_decay, decay_tracks(.)) 
    df_decay      <- df_decay[order(order(indices)), , drop = FALSE]
    return(df_decay)
  } 
  
  fl_shape <- read_sf(here::here('data/Florida_Shoreline.geojson'))
  #df_decay <- df_decay %>% filter(ClusterNumber == ClusterNumber)
  
  df_decay_expand <- df_decay %>% expand(TAU = seq(0, fcst_hr, 1)) %>%
    left_join(df_decay, by = 'TAU') %>% mutate(
      Lon = na.approx(Lon),
      Lat = na.approx(Lat),
      Max_winds_kts = na.approx(Max_winds_kts),
      Min_SLP_mb = na.approx(Min_SLP_mb),
      Max_winds_mph = na.approx(Max_winds_mph),
      NWinds = unique(df_decay$NWinds),
      NSLP = unique(df_decay$NSLP),
      Ntracks.x = unique(df_decay$Ntracks.x),
      Ntracks.y = unique(df_decay$Ntracks.y),
      ClusterNumber = unique(df_decay$ClusterNumber),
      Color = unique(df_decay$Color),
      Probability = unique(df_decay$Probability)
    )
  
  df_decay_coords <- df_decay_expand %>% select(TAU, Lon, Lat)
  overlands <-
    df_decay_expand %>% st_as_sf(coords = c('Lon', 'Lat'), crs = st_crs(fl_shape)) %>%
    st_intersects(fl_shape, sparse = F)
  
  df_decay_expand <-
    df_decay_expand %>% bind_cols(overland = overlands) %>%
    mutate(time_oland = case_when(
      overland == T ~ row_number(overland) - min_rank(overland),
      T ~ NA_integer_
    ))
  
  landfall_ws <-
    df_decay_expand %>% filter(!is.na(time_oland)) %>% slice(1) %>% pull(Max_winds_kts)
  if(is_empty(landfall_ws)){
    landfall_ws <- NA
  }
  
  df_decay_expand <- df_decay_expand %>%
    mutate(
      df_wind = case_when(
        !is.na(time_oland) ~ decay_windspeed(V0 = landfall_ws, t = time_oland),
        T ~ Max_winds_kts
      ),
      df_wind_mph = df_wind * 1.15078
    )
  
  df_decay_expand <- df_decay_expand %>% filter(TAU %in% df_decay$TAU)
  return(df_decay_expand)
}

# if(decay_windspeeds == T){
#   daOutTrack <- daOutTrack %>% 
#     group_by(ClusterNumber) %>% 
#     group_map(~decay_tracks(df_decay = .), .keep = T) %>% 
#     bind_rows()
#   
#   daOutTrack <- daOutTrack %>% mutate(Max_winds_kts = df_wind, Max_winds_mph = df_wind_mph) %>%
#     select(-overland, -time_oland, -df_wind, -df_wind_mph)
# }

#' decay_windspeed
#'
#' @param V0 starting velocity
#' @param t time unit
#'
#' @return decayed value
#' @export
#'
#' @examples
decay_windspeed <- function(V0, t) {
  R <- .9
  a <- .095# h ^-1 decay constant aka 1/h
  Vb <- 26.7
  Vt <- Vb + (R * V0 - Vb)*(exp(1)^-(a*t))
  return(Vt)
}

#' var_to_color
#' create fuction oo make color palette from variable name
#'
#' @param df dataframe
#' @param var variable to convert to color
#' @param pal Palette to use. defaults to colorspace::qualitative_hcl(pal = 'Dark2)
#'
#' @return returns color palette
#' @export
#'
#' @examples
var_to_color <- function(df, var, pal = 'Dark2') {
  var <- enquo(var)
  df %>% 
    mutate(Colors = #brewer.pal(n_distinct(!!var), pal)
             qualitative_hcl(n_distinct(!!var), pal)[match(!!var, unique(!!var))]) %>%
    pull(Colors)
}



#' f_Read_ATCF_AAL
#' converts operational track .dat file into a usable dataframe
#' reference: https://www.nrlmry.navy.mil/atcf_web/docs/database/new/abdeck.txt
#' 
#' @param atcf_aal_datafile 
#'
#' @return dataframe
#' @export
#'
#' @examples
f_Read_ATCF_AAL <- function(atcf_aal_datafile) {
  
  #-----------
  # CONSTANTS
  #-----------
  kts_2_mph <- 1.15077945 
  
  ATCF_AAL_header   <- c("Basin", "Storm_Number_in_Year", "Storm_Name" ,"Year", "Month", "Day", "Hour",
                         "ModelName", "TAU", "Status", "Lat", "Lon","Max_winds_kts","Max_winds_mph",
                         "Min_SLP_mb")
  
  
  ATCF_AAL_temp_header   <- c("Basin", "CY", "YYYYMMDDHH ","TECHNUM_MIN" ,"TECH","TAU", "LatN_S", "LonE_W", "VMAX",
                              "MSLP", "TY", "RAD", "WINDCODE", "RAD1","RAD2","RAD3","RAD4",
                              "POUTER","ROUTER", "RMW","GUSTS","EYE","SUBREGION","MAXSEAS",  "INITIALS",
                              "DIR","STORMNAME","DEPTH","SEAS","SEASCODE","SEAS1","SEAS2","SEAS3", "SEAS4",
                              "USERDEFINE1","userdata1","USERDEFINE2","userdata2",
                              "USERDEFINE3","userdata3","USERDEFINE4","userdata4",
                              "USERDEFINE5","userdata5")
  
  ncol <- max(count.fields(atcf_aal_datafile, sep = ","))
  
  daATCF_AAL <- read.table(atcf_aal_datafile, header=FALSE, as.is = TRUE, fill = TRUE, sep=",",
                           strip.white = TRUE,col.names = ATCF_AAL_temp_header)
  
  #S.name <<- daATCF_AAL %>% filter(is.na(DEPTH) == F & !DEPTH == "") %>% .[nrow(.), c("DEPTH")]
  
  #--------------------------
  # Add the name of storms
  # in data.frame
  # Reformat the lat/lon
  #--------------------------
  daATCF_AAL$Year                <- substr(daATCF_AAL[,3], 1,4)
  daATCF_AAL$Month               <- substr(daATCF_AAL[,3], 5,6)
  daATCF_AAL$Day                 <- substr(daATCF_AAL[,3], 7,8)
  daATCF_AAL$Hour                <- substr(daATCF_AAL[,3], 9,10)
  daATCF_AAL$Max_winds_mph       <- as.numeric(daATCF_AAL$VMAX)*kts_2_mph
  daATCF_AAL$LatN_S              <- as.numeric(sub("N","",daATCF_AAL$LatN_S))/10
  daATCF_AAL$LonE_W              <- (-1)*as.numeric(sub("W","",daATCF_AAL$LonE_W))/10
  #-----------------------
  # Remove and Re-order
  # some columns
  #-----------------------
  daATCF_AAL <- daATCF_AAL[,-c(3,4,seq(12,26,1), seq(28,40,1)),drop=FALSE] 
  daATCF_AAL <- daATCF_AAL[,c(1,2,10,15,16,17,18,3,4,9,5,6,7,19,8)]
  names(daATCF_AAL) <- ATCF_AAL_header
  
  # # Quality Check
  daATCF_AAL <- daATCF_AAL %>% mutate(
    Max_winds_kts = na_if(Max_winds_kts, Max_winds_kts < 0),
    Max_winds_mph = na_if(Max_winds_mph, Max_winds_mph < 0),
    Min_SLP_mb = na_if(Min_SLP_mb, Min_SLP_mb < 0)
  )
  
  return(daATCF_AAL)
}



#' download_atcf
#' downloads hte operational model data from NOAA
#' 
#' @param directory defaults to a temporary directory
#' @param Year Storm Year
#' @param StormNum The storm number of the year.Invests work differently than named storms
#'
#' @return dataframe
#' @export
#'
#' @examples
download_atcf <- function(directory = temp_file, Year, StormNum){
  
  StormNum <- str_pad(StormNum, 2, 'left', 0)
  
  if(Year < lubridate::year(today())){
    noaa_ftp <- paste0('ftp://ftp.nhc.noaa.gov/atcf/archive/',Year,'/aal')
  } else {
    noaa_ftp <- "ftp://ftp.nhc.noaa.gov/atcf/aid_public/aal"
  }

  
  aal_file <- paste0(directory,StormNum, Year, "aal.dat.gz")
  
  download.file(paste0(noaa_ftp,StormNum,Year,".dat.gz"), 
                aal_file,
                method="libcurl")
  # #bAL file
  # download.file(paste0("ftp.nhc.noaa.gov/atcf/btk/bal",StormNum,Year,".dat"), 
  #               paste0(temp_file,StormNum, Year, "bal.dat"),
  #               method="libcurl")
  
  
  aal_file <- f_Read_ATCF_AAL(aal_file)
  return(aal_file)
}



#' download_best_track
#' downloads the best track / preliminary best track data
#' 
#' @param directory defaults to a temporary directory
#' @param Year Storm Year
#' @param StormNum The storm number of the year.Invests work differently than named storms
#'
#' @return dataframe
#' @export
#'
#' @examples
download_best_track <- function(directory = temp_file, Year, StormNum){
  # #https://www.nrlmry.navy.mil/atcf_web/docs/database/new/abdeck.txt
  # bal_file <- f_Read_ATCF_AAL(
  #   paste0("https://ftp.nhc.noaa.gov/atcf/btk/bal",StormNum,Year,".dat")
  #   )
  
  StormNum <- str_pad(StormNum, 2, 'left', 0)
  
  preliminary_best_track_url <- paste0('https://www.nhc.noaa.gov/gis/best_track/al',StormNum, Year, '_best_track.zip')
  
  download.file(preliminary_best_track_url, 
                paste0(directory,StormNum, Year, ".zip"),
                method="libcurl")
  unzip(
    zipfile = paste0(directory,StormNum, Year, ".zip"),
    exdir = directory)
  
  best_track_shp <- list.files(directory, pattern = 'lin.shp$', full.names = T)
  best_track_shp <- read_sf(best_track_shp) %>% st_transform(4326)
  
  return(best_track_shp)
}

