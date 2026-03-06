# this script:
# (1) extracts tables from access database (two different approaches based on operating system)
# (2) recombines tables to how they should fit together based on access relationships
# (3) stitches together tables from southern (1 table) and northern (2 tables) regions (including so minor cleaning and error fixing)

extract_monitoring_database_traps <- function(access_monitoring){
  
  # for testing
  # specify path to access database
  # access_monitoring <- "raw_data/ms_access/MouseMonitoring.accdb"
  
# EXTRACT DATA FROM MICROSOFT ACCESS AND COMBINE --------------------------------------
 
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
  tbl3TrapInfoSouth    <- sqlFetch(channel, "tbl3TrapInfoSouth")
  tbl4CaptureDataSouth <- sqlFetch(channel, "tbl4CaptureDataSouth")
  # north
  tbl1SiteDataNorth       <- sqlFetch(channel, "tbl1SiteDataNorth")
  tblSessionIDNorth       <- sqlFetch(channel, "tblSessionIDNorth")
  tblCropTypeNorthID      <- sqlFetch(channel, "tblCropTypeNorthID")
  tblCropStageNorthID     <- sqlFetch(channel, "tblCropStageNorthID")
  tblDataSiteNameNorthID  <- sqlFetch(channel, "tblDataSiteNameNorthID")
  tbl2SessionNorth        <- sqlFetch(channel, "tbl2SessionNorth")
  tbl3TrapInfoNorth       <- sqlFetch(channel, "tbl3TrapInfoNorth")
  tbl4CaptureDataNorth    <- sqlFetch(channel, "tbl4CaptureDataNorth")
  tbl5NewCaptureDataNorth <- sqlFetch(channel, "tbl5NewCaptureDataNorth")
  
  # close channel
  close(channel) 
  
  
  # or for mac, use Hmisc (need to install mdbtools on computer first)
  } else {

  # south
  tblDataSiteNameID    <- mdb.get(access_monitoring, "tblDataSiteNameID")          %>% remove_all_labels()
  tblCropTypeID        <- mdb.get(access_monitoring, "tblCropTypeID")              %>% remove_all_labels()
  tblCropStageID       <- mdb.get(access_monitoring, "tblCropStageID")             %>% remove_all_labels()
  tbl1SiteDataSouth    <- mdb.get(access_monitoring, "tbl1SiteDataSouth")          %>% remove_all_labels()
  tblSessionIDSouth    <- mdb.get(access_monitoring, "tblSessionIDSouth")          %>% transform(Start.Date = ymd(Start.Date), End.Date = ymd(End.Date)) %>% remove_all_labels()
  tbl2SessionSouth     <- mdb.get(access_monitoring, "tbl2SessionSouth")           %>% remove_all_labels()
  tbl3TrapInfoSouth    <- mdb.get(access_monitoring, "tbl3TrapInfoSouth")          %>% remove_all_labels()
  tbl4CaptureDataSouth <- mdb.get(access_monitoring, "tbl4CaptureDataSouth")       %>% remove_all_labels()
  # north
  tblDataSiteNameNorthID  <- mdb.get(access_monitoring, "tblDataSiteNameNorthID")  %>% remove_all_labels()
  tbl1SiteDataNorth       <- mdb.get(access_monitoring, "tbl1SiteDataNorth")       %>% remove_all_labels()
  tblSessionIDNorth       <- mdb.get(access_monitoring, "tblSessionIDNorth")       %>% transform(Start.Date = ymd(Start.Date), End.Date = ymd(End.Date)) %>% remove_all_labels()
  tblCropTypeNorthID      <- mdb.get(access_monitoring, "tblCropTypeNorthID")      %>% remove_all_labels()
  tblCropStageNorthID     <- mdb.get(access_monitoring, "tblCropStageNorthID")     %>% remove_all_labels()
  tbl2SessionNorth        <- mdb.get(access_monitoring, "tbl2SessionNorth")        %>% remove_all_labels()
  tbl3TrapInfoNorth       <- mdb.get(access_monitoring, "tbl3TrapInfoNorth")       %>% remove_all_labels()
  tbl4CaptureDataNorth    <- mdb.get(access_monitoring, "tbl4CaptureDataNorth")    %>% remove_all_labels()
  tbl5NewCaptureDataNorth <- mdb.get(access_monitoring, "tbl5NewCaptureDataNorth") %>% remove_all_labels()
  
  }
  
  
  # STITCH TOGETHER TABLES --------------------------------------------------
  
  ## SOUTH
  # renames columns so they match name of corresponding column in tbl2SessionSouth
  tblDataSiteNameID <- rename(tblDataSiteNameID, DataSiteNameOld = DataSiteName, DataSiteName = DataSiteNameID)
  tblCropTypeID <- rename(tblCropTypeID, CropType = CroptypeID)
  tblCropStageID <- rename(tblCropStageID, CropStageOld = CropStage, CropStage = CropStageID)
  tblSessionIDSouth <- rename(tblSessionIDSouth, Session = SessionID)
  
  # combine live trap base information about site / session
  base_south <- left_join(tbl1SiteDataSouth[,1:7], tbl2SessionSouth[,1:6], by = "SiteDataID") %>%
    left_join(., tblSessionIDSouth[,1:4], by = "Session") %>%
    left_join(., tblDataSiteNameID[,1:2], by = "DataSiteName") %>%
    left_join(., tblCropTypeID, by = "CropType") %>%
    left_join(., tblCropStageID, by = "CropStage") 
  
  # add live trapping data 
  DataCHSouth <- left_join(tbl3TrapInfoSouth[,1:7], tbl4CaptureDataSouth, by = "TrapInfoID") %>%
    left_join(base_south, ., by = "SessionID") %>%
    # remove rows without trapping data - this must be because these sites were only rapidly assessed
    filter(!(is.na(TrapInfoID))) %>%
    # remove unnecessary ID cols now they're joined
    dplyr::select(!(c(SiteDataID, SessionID, DataSiteName, CropType, CropStage))) 
  
  ## NORTH
  # renames columns so they match name of corresponding column in tbl2SessionNorth
  tblDataSiteNameNorthID <- rename(tblDataSiteNameNorthID, DataSiteNameNorthOld = DataSiteNameNorth, DataSiteNameNorth = DataSiteNameNorthID)
  tblCropTypeNorthID <- rename(tblCropTypeNorthID,  CropTypeOld = CropType, CropType = CropTypeID)
  tblCropStageNorthID <- rename(tblCropStageNorthID, CropStageOld = CropStage, CropStage = CropStageID)
  tblSessionIDNorth <- rename(tblSessionIDNorth, Session = SessionNorthID)
  tbl2SessionNorth <- rename(tbl2SessionNorth, Session = SessionNorth)
  
  # combine live trap base information about site / session
  base_north <- 
    left_join(tbl1SiteDataNorth[,1:7], tbl2SessionNorth[1:6], by = "SiteDataID") %>%
    left_join(., tblSessionIDNorth[,1:4], by = "Session") %>%
    left_join(., tblDataSiteNameNorthID[,1:2], by = "DataSiteNameNorth") %>%
    left_join(., tblCropTypeNorthID, by = "CropType") %>%
    left_join(., tblCropStageNorthID, by = "CropStage") 
  
  # join 'new' and 'old' trapping data tables (not sure what the difference is here):
  # (a) rename cols with different spelling
  tbl5NewCaptureDataNorth <- rename(tbl5NewCaptureDataNorth, Testes = Testis) #EmbryoLength =  "Embryo length",
  # (b) combine
  CaptureDataNorth <- bind_rows(tbl4CaptureDataNorth, tbl5NewCaptureDataNorth)
  
  # include at least one line for trapping grids which did not catch any mice
  DataCHNorth <- left_join(tbl3TrapInfoNorth, CaptureDataNorth, by = "TrapInfoID") %>%
    left_join(base_north, ., by = "SessionID") %>%
    # remove rows without trapping data - this must be because these sites were only rapidly assessed?
    filter(!(is.na(TrapInfoID))) %>%
    # clean base table - remove unnecessary ID cols now as they're now joined
    dplyr::select(!(c(SiteDataID, SessionID, DataSiteNameNorth, CropType, CropStage)))
  
  # COMBINE AND CLEAN NORTHERN AND SOUTHERN DATA ---------------------------------------------
  
  ## MAKE COLUMNS THE SAME FOR NORTH AND SOUTH
  # remove spaces / make lowercase
  names(DataCHNorth) <- tolower(names(DataCHNorth))
  names(DataCHSouth) <- tolower(names(DataCHSouth))
  # remove "north" in col names
  names(DataCHNorth) <- gsub("north", "", names(DataCHNorth))
  # fix this column name due to code ^ to fit with south data
  DataCHNorth <- rename(DataCHNorth, capturedataid = erncapturedataid)
  # don't need these
  DataCHSouth <- select(DataCHSouth, !(c("comments")))
  DataCHNorth <- select(DataCHNorth, !(c("sexcode")))
  # distinguish which region (for when combined)
  DataCHSouth$region <- "south"
  DataCHNorth$region <- "north"
  # fix different classes
  DataCHNorth <- transform(DataCHNorth, 
                           pregnant = as.character(pregnant),
                           dnasampleno = as.character(dnasampleno),
                           pittag = as.character(pittag))
  
  # bind together
  DataCH <- bind_rows(DataCHSouth, DataCHNorth)
  
  # return only trapping data
  return(DataCH)

}