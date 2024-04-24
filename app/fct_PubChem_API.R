cas2cid <- function(CAS) {
    if (is.na(CAS)) return(NA)
    
    # Query NCBI
    similar.cid <- try(webchem::get_cid(query=CAS, domain="compound", from="cas", match="all"))
    if (inherits(similar.cid, "try-error") | length(similar.cid) == 0) return(NA)
    
    # Change type and check if a single CID is provided
	similar.cid <- as.integer(similar.cid$cid)
	if (length(similar.cid) == 1) return(similar.cid)
	
	# Refined conversion from CAS to CID using PubChem's Synonyms section
	df <- webchem::pc_sect(id=similar.cid, section="Depositor-Supplied Synonyms")
	CASex <- paste("^", CAS, "$", sep="")
	most_common <- sapply(split(df, df$CID), function(m) return(sum(grepl(CASex, unique(m$Result)))))
	most_common <- names(most_common[most_common == 1])
	if (identical(most_common, character(0))) return(similar.cid)
	most_common <- as.integer(most_common)
	return(most_common)
}

getPropertiesFromCID <- function(CID) {
    empty_dataframe <- data.frame("MolecularWeight"=NA, "CanonicalSMILES"=NA, "IUPACName"=NA, "XLogP"=NA)
    if (is.na(CID)) return(empty_dataframe)
  
    # Query NCBI
    columns = c("MolecularWeight", "CanonicalSMILES", "IUPACName", "XLogP")
    properties <- try(webchem::pc_prop(cid=CID, properties=columns))
    if (inherits(properties, "try-error") || length(properties) == 0) return(empty_dataframe)
    
    # Check if all columns are here, otherwise append column with NA
    missing_columns <- setdiff(columns, colnames(properties))
    if (length(missing_columns) > 0) {
      properties[ , missing_columns] <- NA
    }
    
    # Change types
    properties$CID <- toString(properties$CID)
    properties$MolecularWeight <- as.double(properties$MolecularWeight)
    properties$XLogP <- as.double(properties$XLogP)
    
    return(properties)
}

getCommonName <- function(CID) {
    if (is.na(CID)) return(NA)
    df <- try(webchem::pc_sect(id=CID, section="Synonyms"))
    if (inherits(df, "try-error") || length(df) == 0) return(NA)
    df <- df[1, "Name"]
    df <- toString(df)
    return(df)
}

getLRI <- function(CID, type=c("polar", "non-polar", "semi-standard non-polar"), canonicalSMILES=NA) {
    if (is.na(CID)) return(NA)
    
    # Check if compound is an n-alkane (linear)
    if (!is.na(canonicalSMILES)) {
        if (grepl("^c+$", tolower(canonicalSMILES))) return(nchar(canonicalSMILES) * 100)
    }
    
    # Attribute local var depending on type argument
    type <- tolower(type)
    if (type == "polar") name <- "Standard polar"
    if (type == "non-polar") name <- "Standard non-polar"
    if (type == "semi-standard non-polar") name <- "Semi-standard non-polar"
    
    # Query NCBI
    df <- try(PubChemR::get_pug_view(identifier=CID, domain="compound", heading="Kovats Retention Index", annotation = "Data"))
    if (inherits(df, "try-error") || length(df) == 0) return(NA)
    
    # Loop through LRI types: polar, non-polar, semi-standard non-polar or all column types
    lri.section <- df$Record$Section[[1]]$Section[[1]]$Section[[1]]$Information
    # If "All column types" type is the only type then return always these LRI
    if (length(lri.section) == 1 && lri.section[[1]]$Name == "All column types") {
        return(lri.section[[1]]$Value$Number)
    }
    for (i in 1:length(lri.section)) {
        current.section <- lri.section[[i]]
        if (current.section$Name == name) {
            return(current.section$Value$Number)
        }
    }
    return(NA)
}

getPubchemDescriptors <- function(CID, type=c("Odor", "Taste")) {
    if (is.na(CID)) return(NA)
    # Query NCBI
    df <- try(webchem::pc_sect(id=CID, section=type))
    if (inherits(df, "try-error") | length(df) == 0) return(NA)
    
    # Split and select unique descriptors
    df <- tolower(df$Result)
    df <- strsplit(x=df, split=", ")
    df <- unique(unlist(df))
    df <- paste(df, collapse="; ")
    return(df)
}

getFlavornetOdorDescriptors <- function(CID) {
    if (is.na(CID)) return(NA)
    db <- read.csv("./data/descriptor_db/flavornet_odor_descriptors.csv", header=TRUE, na.strings=c("NA", ""))
    CID <- as.numeric(CID)
    db <- db[db$CID == CID, "odor_flavornet"]
    if (identical(db, character(0))) return(NA)
    return(db)
}

getGoodScentsOdorDescriptors <- function(CAS) {
    db <- read.csv("./data/descriptor_db/goodscents_odor_descriptors.csv", header=TRUE, na.strings=c("NA", ""))
    db <- db[db$CAS == CAS, "odor_goodscents"]
    if (identical(db, character(0))) return(NA)
    return(db)
}