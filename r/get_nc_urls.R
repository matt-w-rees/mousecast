
# ─────────────────────────────────────────────
# Function 1: Parse THREDDS catalog to get .nc URLs
# ─────────────────────────────────────────────
get_nc_urls <- function(catalog_url, base_url = "https://www.indraweb.io/thredds/fileServer/") {
  
  # Step 1: Read and parse the catalog XML
  catalog <- read_xml(catalog_url)
  
  # Step 2: Strip namespaces to simplify XPath matching
  catalog <- xml_ns_strip(catalog)
  
  # Step 3: Find all <dataset> nodes with a urlPath attribute
  datasets <- xml_find_all(catalog, ".//dataset[@urlPath]")
  
  # Step 4: Extract urlPaths (relative paths to resources)
  url_paths <- xml_attr(datasets, "urlPath")
  
  # Step 5: Filter to include only those ending with ".nc"
  nc_paths <- url_paths[grepl("\\.nc$", url_paths)]
  
  # Step 6: Construct full download URLs from base path
  full_urls <- paste0(base_url, nc_paths)
  
  return(full_urls)
  
}
