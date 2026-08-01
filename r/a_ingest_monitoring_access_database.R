# this script:
# (1) extracts tables from access database (two different approaches based on operating system)
# (2) recombines tables to how they should fit together based on access relationships
# (3) stitches together tables from southern (1 table) and northern (2 tables) regions (including so minor cleaning and error fixing)

# note, ODBC drivers / R packages shift date origin to “days since 1970-01-01” -- see code below to account for this


ingest_monitoring_access_database <- function(access_monitoring, exclude_subsites = NULL) {
  
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
  
  base_south <- left_join(
                          # Drop Contact.Name/Comments (not used downstream) -- named select, not a
                          # positional [,1:6] slice, so this doesn't silently change if the Access
                          # table's own column order or count ever changes.
                          select(tbl1SiteDataSouth, SiteDataID, State, AreaName, NearestTown, SiteName, FarmerName),
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
  # Step 5: Join north and south trapping datasets
  # ----------------------------------------------------------
  
  ## Harmonise column names
  # all lowercase 
  names(DataCHSouth) <- tolower(names(DataCHSouth))
  names(DataCHNorth) <- tolower(names(DataCHNorth))
  
  # remove north from column names in northern dataset
  names(DataCHNorth) <- gsub("north", "", names(DataCHNorth))
  # and because of the line^ need to fix this one manually:
  DataCHNorth <- rename(DataCHNorth, capturedataid = erncapturedataid)
  
  # make the crop type name column same (change north to south convention)
  DataCHNorth <- rename(DataCHNorth, cropname = croptypeold)
  
  # change any variable classes in northern data to match southern (ready for binding)
  DataCHNorth <- transform(DataCHNorth, pregnant = as.character(pregnant), dnasampleno = as.character(dnasampleno), pittag = as.character(pittag))
  
  # now join north and south datasets together
  DataCH <- bind_rows(DataCHSouth, DataCHNorth)

  # Remove snapback traps -- these kill the animal, so they aren't part of
  # the live-capture (and potential future capture-recapture) dataset.
  # traptype is the raw numeric Access code at this stage (1 = longworth,
  # 2 = elliott, 3 = snapback); see clean_data_access_monitoring_traps() for
  # where this gets recoded to the labelled trap_type column.
  DataCH <- dplyr::filter(DataCH, traptype != 3)


  # ----------------------------------------------------------
  # Step 5: Add rapid assessment tables to base
  # ----------------------------------------------------------
  
  # relationship = "one-to-many": base_south/base_north have one row per
  # SessionID (verified against the live database); tblRapidAssessSouth/North
  # legitimately have several rows per SessionID (one per transect/card), so
  # this asserts the true cardinality rather than silently allowing the
  # base_*-side duplication (a many-to-many cross-product, spuriously pairing
  # transects with the wrong site/crop row) that "many-to-many" would permit
  # if a future Access database refresh ever introduced it unnoticed.
  DataRASouth <- left_join(base_south, tblRapidAssessSouth, by = "SessionID", relationship = "one-to-many") %>%
    filter(!is.na(RapidAssessmentID)) %>%
    select(-c(SiteDataID, SessionID, DataSiteName, CropType, CropStage)) %>%
    rename(
      ActiveBurrow.225...250 = ActiveBurrow.225..250,
      ActiveBurrow.325...350 = ActiveBurrow325...350,
      ActiveBurrow.350...375 = ActiveBurrow.350..375
    )
  DataRASouth$Total <- NULL

  DataRANorth <- left_join(base_north, tblRapidAssessNorth, by = "SessionID", relationship = "one-to-many") %>%
    filter(!is.na(RapidAssessmentNorthID)) %>%
    select(-c(SiteDataID, SessionID, DataSiteNameNorth, CropType, CropStage))
  
  # Harmonise column names
  names(DataRANorth) <- gsub("North", "", names(DataRANorth))
  names(DataRANorth) <- gsub(' ', '', tolower(names(DataRANorth)))
  names(DataRASouth) <- gsub(' ', '', tolower(names(DataRASouth)))
  DataRANorth <- rename(DataRANorth, cropname = croptypeold)
  
  # bind together but dont print messages
  DataRA <- suppressMessages(bind_rows(DataRANorth, DataRASouth))

  # Remove fenceline subsites (not true within-crop surveys) -- applied to
  # both DataCH and DataRA here, since they share the same datasitenameold
  # column from the same raw site table, rather than separately in each
  # downstream cleaning function. tolower()/str_squish() handle case and
  # whitespace on both sides of the comparison (this column isn't
  # standardised yet at this stage, and some raw values have stray double
  # spaces, e.g. "Grandview  19").
  if (!is.null(exclude_subsites)) {
    exclude_subsites <- tolower(stringr::str_squish(exclude_subsites))
    DataCH <- dplyr::filter(DataCH, !(tolower(stringr::str_squish(datasitenameold)) %in% exclude_subsites))
    DataRA <- dplyr::filter(DataRA, !(tolower(stringr::str_squish(datasitenameold)) %in% exclude_subsites))
  }

  # ----------------------------------------------------------
  # Step 8: Combine as named list
  # ----------------------------------------------------------
  data_list <- list(DataCH, DataRA)
  names(data_list) <- c("DataCH", "DataRA")
  return(data_list)
}
