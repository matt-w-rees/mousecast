# this script:
# (1) extracts tables from access database (two different approaches based on operating system)
# (2) recombines tables to how they should fit together based on access relationships
# (3) stitches together tables from southern (1 table) and northern (2 tables) regions (including so minor cleaning and error fixing)
# (4) returns list of two dataframe for targets pipeline)

extract_monitoring_database_rapid <- function(access_monitoring){
  
  
  # EXTRACT DATA FROM MICROSOFT ACCESS --------------------------------------
  
  # for testing
  # specify path to access database
  #access_monitoring <- "raw_data/ms_access/MouseMonitoring.accdb"
  
  # check if mac or windows
  x <- Sys.info()
  
  # if windows, use RODBC package (no further installation required)
  if (x[1] == "Windows") {
    
    # define path
    PATH <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", access_monitoring)
    
    # establish connection to access database
    channel <- odbcDriverConnect(PATH)
    
    ## Download tables from database
    # south
    tblDataSiteNameID    <- sqlFetch(channel, "tblDataSiteNameID")
    tblCropTypeID        <- sqlFetch(channel, "tblCropTypeID")
    tblCropStageID       <- sqlFetch(channel, "tblCropStageID")
    tbl1SiteDataSouth    <- sqlFetch(channel, "tbl1SiteDataSouth")
    tblSessionIDSouth    <- sqlFetch(channel, "tblSessionIDSouth")
    tbl2SessionSouth     <- sqlFetch(channel, "tbl2SessionSouth")
    tblRapidAssessSouth  <- sqlFetch(channel, "tblRapidAssessSouth")
    # north
    tbl1SiteDataNorth       <- sqlFetch(channel, "tbl1SiteDataNorth")
    tblSessionIDNorth       <- sqlFetch(channel, "tblSessionIDNorth")
    tblCropTypeNorthID      <- sqlFetch(channel, "tblCropTypeNorthID")
    tblCropStageNorthID     <- sqlFetch(channel, "tblCropStageNorthID")
    tblDataSiteNameNorthID  <- sqlFetch(channel, "tblDataSiteNameNorthID")
    tblSessionIDNorth       <- sqlFetch(channel, "tblSessionIDNorth")
    tbl2SessionNorth        <- sqlFetch(channel, "tbl2SessionNorth")
    tblRapidAssessNorth     <- sqlFetch(channel, "tblRapidAssessNorth")
    
    # close channel
    close(channel) 
    
    
    # or for mac, use Hmisc (need to install mdbtools on computer first)
  } else {
    
  ## Download tables from database
  # south
  tblDataSiteNameID    <- mdb.get(access_monitoring, "tblDataSiteNameID") %>% remove_all_labels()
  tblCropTypeID        <- mdb.get(access_monitoring, "tblCropTypeID") %>% remove_all_labels()
  tblCropStageID       <- mdb.get(access_monitoring, "tblCropStageID") %>% remove_all_labels()
  tbl1SiteDataSouth    <- mdb.get(access_monitoring, "tbl1SiteDataSouth") %>% remove_all_labels()
  tblSessionIDSouth    <- mdb.get(access_monitoring, "tblSessionIDSouth") %>% transform(Start.Date = ymd(Start.Date), End.Date = ymd(End.Date)) %>% remove_all_labels()
  tbl2SessionSouth     <- mdb.get(access_monitoring, "tbl2SessionSouth") %>% remove_all_labels()
  tblRapidAssessSouth  <- mdb.get(access_monitoring, "tblRapidAssessSouth") %>% transform(DateSet = ymd(DateSet), DateRecovered = ymd(DateRecovered)) %>% remove_all_labels()
  # north
  tbl1SiteDataNorth       <- mdb.get(access_monitoring, "tbl1SiteDataNorth") %>% remove_all_labels()
  tblSessionIDNorth       <- mdb.get(access_monitoring, "tblSessionIDNorth")  %>% transform(Start.Date = ymd(Start.Date), End.Date = ymd(End.Date)) %>% remove_all_labels()
  tblCropTypeNorthID      <- mdb.get(access_monitoring, "tblCropTypeNorthID") %>% remove_all_labels()
  tblCropStageNorthID     <- mdb.get(access_monitoring, "tblCropStageNorthID") %>% remove_all_labels()
  tblDataSiteNameNorthID  <- mdb.get(access_monitoring, "tblDataSiteNameNorthID") %>% remove_all_labels()
  tbl2SessionNorth        <- mdb.get(access_monitoring, "tbl2SessionNorth") %>% remove_all_labels()
  tblSessionIDNorth       <- mdb.get(access_monitoring, "tblSessionIDNorth") %>% transform(Start.Date = ymd(Start.Date), End.Date = ymd(End.Date)) %>% remove_all_labels()
  tblRapidAssessNorth     <- mdb.get(access_monitoring, "tblRapidAssessNorth") %>% transform(DateSet = ymd(DateSet), DateRecovered = ymd(DateRecovered)) %>% remove_all_labels()
  }
  
  
  # STITCH TOGETHER TABLES --------------------------------------------------
  
  ## SOUTH
  # renames first lot of tables - so match name of corresponding column in tbl2SessionSouth
  tblDataSiteNameID <- rename(tblDataSiteNameID, DataSiteNameOld = DataSiteName, DataSiteName = DataSiteNameID)
  tblCropTypeID <- rename(tblCropTypeID, CropType = CroptypeID)
  tblCropStageID <- rename(tblCropStageID, CropStageOld = CropStage, CropStage = CropStageID)

  # combine live trap base information about site / session
  base_south <- left_join(tbl1SiteDataSouth[,1:7], tbl2SessionSouth[,1:6], by = "SiteDataID") %>%
    left_join(., tblSessionIDSouth[,1:4], by = "SessionID") %>%
    left_join(., tblDataSiteNameID[,1:2], by = "DataSiteName") %>%
    left_join(., tblCropTypeID, by = "CropType") %>%
    left_join(., tblCropStageID, by = "CropStage") 
  
  # add rapid assessment monitoring data
  DataRASouth <- left_join(base_south, tblRapidAssessSouth, by = "SessionID") %>%
    # remove rows without rapid assessment data - this must be because these sites were only live trapped
    filter(!(is.na(RapidAssessmentID)))  %>%
    # remove unnecessary ID cols now as they're now joined
    dplyr::select(!(c(SiteDataID, SessionID, DataSiteName, CropType, CropStage))) 
  
  ## NORTH
  # renames columns so they match name of corresponding column in tbl2SessionNorth
  tblDataSiteNameNorthID <- rename(tblDataSiteNameNorthID, DataSiteNameNorthOld = DataSiteNameNorth, DataSiteNameNorth = DataSiteNameNorthID)
  tblCropTypeNorthID <- rename(tblCropTypeNorthID,  CropTypeOld = CropType, CropType = CropTypeID)
  tblCropStageNorthID <- rename(tblCropStageNorthID, CropStageOld = CropStage, CropStage = CropStageID)
  tblSessionIDNorth <- rename(tblSessionIDNorth, SessionID = SessionNorthID)
  
  # combine live trap base information about site / session
  base_north <- 
    left_join(tbl1SiteDataNorth[,1:7], tbl2SessionNorth[1:6], by = "SiteDataID") %>%
    left_join(., tblSessionIDNorth[,1:4], by = "SessionID") %>%
    left_join(., tblDataSiteNameNorthID[,1:2], by = "DataSiteNameNorth") %>%
    left_join(., tblCropTypeNorthID, by = "CropType") %>%
    left_join(., tblCropStageNorthID, by = "CropStage") 
  
  # add rapid assessment monitoring data
  DataRANorth <- left_join(base_north, tblRapidAssessNorth, by = "SessionID", relationship = "many-to-many") %>%
    # remove rows without rapid assessment data - this must be because these sites were only live trapped
    filter(!(is.na(RapidAssessmentNorthID))) %>%
    # clean base table - remove unnecessary ID cols now as they're now joined
    dplyr::select(!(c(SiteDataID, SessionID, DataSiteNameNorth, CropType, CropStage)))
  
  
  
  # fix inconsistent column names in south data for active burrow
  DataRASouth <- rename(DataRASouth, ActiveBurrow.225...250 = ActiveBurrow.225..250, ActiveBurrow.325...350 = ActiveBurrow325...350, ActiveBurrow.350...375 = ActiveBurrow.350..375)
  # remove blank column 
  DataRASouth$Total <- NULL 
  
  
  # COMBINE NORTHERN AND SOUTHERN DATA ---------------------------------------------

  ## MAKE COLUMNS THE SAME FOR NORTH AND SOUTH
  # remove "North" in col names
  names(DataRANorth) <- gsub("North", "", names(DataRANorth))
  
  # remove spaces / make lowercase for both datasets
  names(DataRANorth) <- gsub(' ', '', tolower(names(DataRANorth)))
  names(DataRASouth) <- gsub(' ', '', tolower(names(DataRASouth)))
  
  # in north, croptypeold is same as cropname in south 
  DataRANorth <- rename(DataRANorth, cropname = croptypeold) 
  
  # bind together
  DataRA <- suppressMessages(bind_rows(DataRANorth, DataRASouth))
  
  #return
  return(DataRA)
  
}