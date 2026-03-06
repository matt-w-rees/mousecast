data_ecology_extract_database <- function(access_eco){
  
  # specify access datatype path 
  #access_eco <- "raw_data/survey_data/FreyaEco.accdb"
  
  # check if mac or windows
  x <- Sys.info()
  
    # if windows, use RODBC package (no further installation required)
    if (x[1] == "Windows") {
      
     # define path
     PATH <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};", "DBQ=", access_eco)
     
     ## Establish connection to access database
     channel <- odbcDriverConnect(PATH)
     
     ## Download tables from database
     tbl1SiteDataSouth <- sqlFetch(channel, "tbl1SiteDataSouth")
     tblSessionIDSouth       <- sqlFetch(channel, "tblSessionIDSouth")
     tblCropTypeID      <- sqlFetch(channel, "tblCropTypeID")
     tblCropStageID     <- sqlFetch(channel, "tblCropStageID")
     tbl2SessionSouth        <- sqlFetch(channel, "tbl2SessionSouth")
     tbl3TrapInfoSouth       <- sqlFetch(channel, "tbl3TrapInfoSouth")
     tbl4CaptureDataSouth    <- sqlFetch(channel, "tbl4CaptureDataSouth")
     
     # close channel
     close(channel)
     
     
     # or for mac, use Hmisc (need to install mdbtools on computer first)
    } else {
      
      
      # Mac extraction via mdb.get
      fetch <- function(tbl) mdb.get(access_eco, tbl) %>% remove_all_labels()
      
    ## Download tables from database
    tbl1SiteDataSouth    <- mdb.get(access_eco, "tbl1SiteDataSouth") 
    tblSessionIDSouth    <- mdb.get(access_eco, "tblSessionIDSouth")  %>% remove_all_labels() %>% transform(Start.Date = as.Date(Start.Date, origin = "1970-01-01"), End.Date = as.Date(End.Date, origin = "1970-01-01"), SessionID = as.integer(SessionID))
    tblCropTypeID        <- mdb.get(access_eco, "tblCropTypeID")
    tblCropStageID       <- mdb.get(access_eco, "tblCropStageID") 
    tbl2SessionSouth     <- mdb.get(access_eco, "tbl2SessionSouth") 
    tbl3TrapInfoSouth    <- mdb.get(access_eco, "tbl3TrapInfoSouth") 
    tbl4CaptureDataSouth <- mdb.get(access_eco, "tbl4CaptureDataSouth") %>% remove_all_labels() %>% transform(TrapInfoID = as.integer(TrapInfoID))
    
    }
  

  # REMOVE EMPTY ROWS -------------------------------------------------------
  # first remove NAs in tbl4CaptureDataSouth - these are just empty rows
  tbl4CaptureDataSouth <- filter(tbl4CaptureDataSouth, !(is.na(TrapInfoID)))
  
  
  # STITCH TABLES TOGETHER --------------------------------------------------
    data <- left_join(tbl1SiteDataSouth, tbl2SessionSouth, by = "SiteDataID") %>%
            left_join(., tblSessionIDSouth, join_by("Session" == "SessionID")) %>%
            left_join(., tblCropTypeID, join_by("CropType" == "CroptypeID")) %>%
            left_join(., tblCropStageID, join_by("CropStage" == "CropStageID")) %>%  
            left_join(., tbl3TrapInfoSouth, by = "SessionID")  %>%  
            left_join(., tbl4CaptureDataSouth, by = "TrapInfoID")
  
  # return as single dataframe
  return(data)


}
  

# END 
