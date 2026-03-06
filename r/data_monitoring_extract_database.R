# this script:
# (1) extracts tables from access database (two different approaches based on operating system)
# (2) recombines tables to how they should fit together based on access relationships
# (3) stitches together tables from southern (1 table) and northern (2 tables) regions (including so minor cleaning and error fixing)

# note, ODBC drivers / R packages shift date origin to “days since 1970-01-01” -- see code below to account for this


data_monitoring_extract_database <- function(access_monitoring) {
  
  # ----------------------------------------------------------
  # Step 1: Determine OS
  # ----------------------------------------------------------
  is_windows <- Sys.info()[1] == "Windows"
  
  # ----------------------------------------------------------
  # Step 2: Extract tables from Access database
  # - Windows uses RODBC
  # - Mac uses Hmisc/mdbtools
  # ----------------------------------------------------------
  if (is_windows) {
    
    # Windows connection via RODBC
    PATH <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", access_monitoring)
    channel <- odbcDriverConnect(PATH)
    
    # South tables
    tblDataSiteNameID    <- sqlFetch(channel, "tblDataSiteNameID")
    tblCropTypeID        <- sqlFetch(channel, "tblCropTypeID")
    tblCropStageID       <- sqlFetch(channel, "tblCropStageID")
    tbl1SiteDataSouth    <- sqlFetch(channel, "tbl1SiteDataSouth")
    tblSessionIDSouth    <- sqlFetch(channel, "tblSessionIDSouth")
    tbl2SessionSouth     <- sqlFetch(channel, "tbl2SessionSouth")
    tbl3TrapInfoSouth    <- sqlFetch(channel, "tbl3TrapInfoSouth")
    tbl4CaptureDataSouth <- sqlFetch(channel, "tbl4CaptureDataSouth")
    tblRapidAssessSouth  <- sqlFetch(channel, "tblRapidAssessSouth")
    
    # North tables
    tbl1SiteDataNorth       <- sqlFetch(channel, "tbl1SiteDataNorth")
    tblSessionIDNorth       <- sqlFetch(channel, "tblSessionIDNorth")
    tblCropTypeNorthID      <- sqlFetch(channel, "tblCropTypeNorthID")
    tblCropStageNorthID     <- sqlFetch(channel, "tblCropStageNorthID")
    tblDataSiteNameNorthID  <- sqlFetch(channel, "tblDataSiteNameNorthID")
    tbl2SessionNorth        <- sqlFetch(channel, "tbl2SessionNorth")
    tbl3TrapInfoNorth       <- sqlFetch(channel, "tbl3TrapInfoNorth")
    tbl4CaptureDataNorth    <- sqlFetch(channel, "tbl4CaptureDataNorth")
    tbl5NewCaptureDataNorth <- sqlFetch(channel, "tbl5NewCaptureDataNorth")
    tblRapidAssessNorth     <- sqlFetch(channel, "tblRapidAssessNorth")
    
    close(channel)
    
  } else {
    
    # Mac extraction via mdb.get
    fetch <- function(tbl) mdb.get(access_monitoring, tbl) %>% remove_all_labels()
    
    tblDataSiteNameID    <- fetch("tblDataSiteNameID")
    tblCropTypeID        <- fetch("tblCropTypeID")
    tblCropStageID       <- fetch("tblCropStageID")
    tbl1SiteDataSouth    <- fetch("tbl1SiteDataSouth")
    tblSessionIDSouth    <- fetch("tblSessionIDSouth") %>% transform(Start.Date = as.Date(Start.Date, origin = "1970-01-01"), End.Date = as.Date(End.Date, origin = "1970-01-01"))
    tbl2SessionSouth     <- fetch("tbl2SessionSouth")
    tbl3TrapInfoSouth    <- fetch("tbl3TrapInfoSouth")
    tbl4CaptureDataSouth <- fetch("tbl4CaptureDataSouth")
    tblRapidAssessSouth  <- fetch("tblRapidAssessSouth") %>% transform(DateSet = as.Date(DateSet, origin = "1970-01-01"), DateRecovered = as.Date(DateRecovered, origin = "1970-01-01"))
    
    tblDataSiteNameNorthID  <- fetch("tblDataSiteNameNorthID")
    tbl1SiteDataNorth       <- fetch("tbl1SiteDataNorth")
    tblSessionIDNorth       <- fetch("tblSessionIDNorth") %>% transform(Start.Date = as.Date(Start.Date, origin = "1970-01-01"), End.Date = as.Date(End.Date, origin = "1970-01-01"))
    tblCropTypeNorthID      <- fetch("tblCropTypeNorthID")
    tblCropStageNorthID     <- fetch("tblCropStageNorthID")
    tbl2SessionNorth        <- fetch("tbl2SessionNorth")
    tbl3TrapInfoNorth       <- fetch("tbl3TrapInfoNorth")
    tbl4CaptureDataNorth    <- fetch("tbl4CaptureDataNorth")
    tbl5NewCaptureDataNorth <- fetch("tbl5NewCaptureDataNorth")
    tblRapidAssessNorth     <- fetch("tblRapidAssessNorth") %>% transform(DateSet = as.Date(DateSet, origin = "1970-01-01"), DateRecovered = as.Date(DateRecovered, origin = "1970-01-01"))
  }
  
  # ----------------------------------------------------------
  # Step 3: Stitch base site/session tables
  # ----------------------------------------------------------
  
  
  # SOUTH
  # name linking variables so tehey actually match
  tblDataSiteNameID    <- rename(tblDataSiteNameID, DataSiteNameOld = DataSiteName, DataSiteName = DataSiteNameID)
  tblCropTypeID        <- rename(tblCropTypeID, CropType = CroptypeID)
  tblCropStageID       <- rename(tblCropStageID, CropStageOld = CropStage, CropStage = CropStageID)
  tblSessionIDSouth    <- rename(tblSessionIDSouth, Session = SessionID)
  
  base_south <- left_join(tbl1SiteDataSouth[,1:6], # need to subset this to remove the higher / broader level (farm) coordinates in south data only
                          tbl2SessionSouth, by = "SiteDataID") %>%
    left_join(tblSessionIDSouth, by = "Session") %>%
    left_join(tblDataSiteNameID, by = "DataSiteName") %>%
    left_join(tblCropTypeID, by = "CropType") %>%
    left_join(tblCropStageID, by = "CropStage")
  
  # NORTH
  tblDataSiteNameNorthID <- rename(tblDataSiteNameNorthID, DataSiteNameNorthOld = DataSiteNameNorth, DataSiteNameNorth = DataSiteNameNorthID)
  tblCropTypeNorthID     <- rename(tblCropTypeNorthID, CropTypeOld = CropType, CropType = CropTypeID)
  tblCropStageNorthID    <- rename(tblCropStageNorthID, CropStageOld = CropStage, CropStage = CropStageID)
  tblSessionIDNorth      <- rename(tblSessionIDNorth, Session = SessionNorthID)
  tbl2SessionNorth       <- rename(tbl2SessionNorth, Session = SessionNorth)
  
  base_north <- left_join(tbl1SiteDataNorth, tbl2SessionNorth, by = "SiteDataID") %>%
    left_join(tblSessionIDNorth, by = "Session") %>%
    left_join(tblDataSiteNameNorthID, by = "DataSiteNameNorth") %>%
    left_join(tblCropTypeNorthID, by = "CropType") %>%
    left_join(tblCropStageNorthID, by = "CropStage")
  
  
  

  # ----------------------------------------------------------
  # Step 4: Add trap tables to base
  # ----------------------------------------------------------
  
  # SOUTH
  DataCHSouth <- left_join(tbl3TrapInfoSouth, tbl4CaptureDataSouth, by = "TrapInfoID") %>%
    left_join(base_south, ., by = "SessionID") %>%
    filter(!is.na(TrapInfoID)) %>%
    select(-c(SiteDataID, SessionID, DataSiteName, CropType, CropStage))
  
  # NORTH
  tbl5NewCaptureDataNorth <- rename(tbl5NewCaptureDataNorth, Testes = Testis)
  CaptureDataNorth <- bind_rows(tbl4CaptureDataNorth, tbl5NewCaptureDataNorth)
  DataCHNorth <- left_join(tbl3TrapInfoNorth, CaptureDataNorth, by = "TrapInfoID") %>%
    left_join(base_north, ., by = "SessionID") %>%
    filter(!is.na(TrapInfoID)) %>%
    select(-c(SiteDataID, SessionID, DataSiteNameNorth, CropType, CropStage))
  
  
  # ----------------------------------------------------------
  # Step 5: Join north and south datasets
  # ----------------------------------------------------------
  
  ## Harmonise column names
  # all lowercase 
  names(DataCHSouth) <- tolower(names(DataCHSouth))
  names(DataCHNorth) <- tolower(names(DataCHNorth))
  
  # remove north from column names in northern dataset
  names(DataCHNorth) <- gsub("north", "", names(DataCHNorth))
  # and because of the line^ need to fix this one manually:
  DataCHNorth <- rename(DataCHNorth, capturedataid = erncapturedataid)

  # change any variable classes in northern data to match southern (ready for binding)
  DataCHNorth <- transform(DataCHNorth, pregnant = as.character(pregnant), dnasampleno = as.character(dnasampleno), pittag = as.character(pittag))
  
  # add region variable denoting which table it came from
  DataCHSouth$region <- "south"
  DataCHNorth$region <- "north"
  
  # now join north and south datasets together
  DataCH <- bind_rows(DataCHSouth, DataCHNorth)
  
  # ----------------------------------------------------------
  # Step 5: Add rapid assessment tables to base
  # ----------------------------------------------------------
  
  DataRASouth <- left_join(base_south, tblRapidAssessSouth, by = "SessionID") %>%
    filter(!is.na(RapidAssessmentID)) %>%
    select(-c(SiteDataID, SessionID, DataSiteName, CropType, CropStage)) %>%
    rename(
      ActiveBurrow.225...250 = ActiveBurrow.225..250,
      ActiveBurrow.325...350 = ActiveBurrow325...350,
      ActiveBurrow.350...375 = ActiveBurrow.350..375
    )
  DataRASouth$Total <- NULL
  
  DataRANorth <- left_join(base_north, tblRapidAssessNorth, by = "SessionID", relationship = "many-to-many") %>%
    filter(!is.na(RapidAssessmentNorthID)) %>%
    select(-c(SiteDataID, SessionID, DataSiteNameNorth, CropType, CropStage))
  
  # Harmonise column names
  names(DataRANorth) <- gsub("North", "", names(DataRANorth))
  names(DataRANorth) <- gsub(' ', '', tolower(names(DataRANorth)))
  names(DataRASouth) <- gsub(' ', '', tolower(names(DataRASouth)))
  DataRANorth <- rename(DataRANorth, cropname = croptypeold)
  
  # bind together but dont print messages
  DataRA <- suppressMessages(bind_rows(DataRANorth, DataRASouth))

  # ----------------------------------------------------------
  # Step 7: Return as named list
  # ----------------------------------------------------------
  data_list <- list(DataCH, DataRA)
  names(data_list) <- c("DataCH", "DataRA")
  return(data_list)
}
