#' Convert a CAS registry number to PubChem CID.
#' 
#' This converts a single CAS registry number to its PubChem CID(s) via PubChem
#' API. It returns a single PubChem or several as vector of integers.
#' 
#' @param CAS A single CAS registry number as characters
#' @return PubChem CID(s) as integer(s)
#' @examples
#' cas2cid("64-17-5")
#' cas2cid("22564-99-4")
cas2cid <- function(CAS) {
    if (is.na(CAS)) return(NA)
    
    # Query PubChem
    convertee <- try(webchem::get_cid(query=CAS, domain="compound", from="cas", match="all"))
    if (inherits(convertee, "try-error") | length(convertee) == 0) return(NA)
    
    # Change type and check if a single CID is returned
	convertee <- as.integer(convertee$cid)
	if (length(convertee) == 1) return(convertee)
	
	# Refined conversion from CAS to CID using PubChem's Synonyms section
	df <- webchem::pc_sect(id=convertee, section="Depositor-Supplied Synonyms")
	CASregex <- paste("^", CAS, "$", sep="")
	mostCommonCAS <- sapply(split(df, df$CID), function(m) return(sum(grepl(CASregex, unique(m$Result)))))
	mostCommonCAS <- names(mostCommonCAS[mostCommonCAS == 1])
	if (identical(mostCommonCAS, character(0))) return(convertee)
	return(as.integer(mostCommonCAS))
}

#' Retrieve physico-chemical properties of a compound via its PubChem CID.
#' 
#' This retrieves physico-chemical properties of a compound via its PubChem CID
#' using PubChem API. Physico-chemical properties include molecular weight, 
#' canonical SMILES, IUPAC name, and XLogP; all organised as a dataframe.
#' 
#' @param CID A single PubChem CID as integer
#' @return A dataframe containing physico-chemical properties
#' @examples
#' getPropertiesFromCID(6549)
#' getPropertiesFromCID(702)
getPropertiesFromCID <- function(CID) {
    emptyDF <- data.frame("MolecularWeight"=NA, "CanonicalSMILES"=NA, "IUPACName"=NA, "XLogP"=NA)
    if (is.na(CID)) return(emptyDF)
  
    # Query Pubchem
    header <- colnames(emptyDF)
    propertiesDF <- try(webchem::pc_prop(cid=CID, properties=header))
    if (inherits(propertiesDF, "try-error") | length(propertiesDF) == 0) return(emptyDF)
    
    # Check if all columns are here, otherwise append column with NA
    missingColumns <- setdiff(header, colnames(propertiesDF))
    if (length(missingColumns) > 0) {
      propertiesDF[ , missingColumns] <- NA
    }
    
    # Change types
    propertiesDF$CID <- toString(propertiesDF$CID)
    propertiesDF$MolecularWeight <- as.double(propertiesDF$MolecularWeight)
    propertiesDF$XLogP <- as.double(propertiesDF$XLogP)
    
    return(propertiesDF)
}

#' Retrieve common name of a compound via its PubChem CID.
#' 
#' This retrieves the common name of a compound via its PubChem CID
#' using PubChem API. The common name corresponds to the its PubChem page title.
#' 
#' @param CID A single PubChem CID as integer
#' @return The common name of the compound as characters
#' @examples
#' getCommonName(6549)
#' getCommonName(702)
getCommonName <- function(CID) {
    if (is.na(CID)) return(NA)
    
    # Query PubChem
    df <- try(webchem::pc_sect(id=CID, section="Synonyms"))
    if (inherits(df, "try-error") | length(df) == 0) return(NA)
    
    name <- df[1, "Name"]
    name <- toString(name)
    return(name)
}

findKovatsSubsubsection <- function(query) {
    for (i in 1:length(query)) {
        if (query[[i]]$TOCHeading == "Kovats Retention Index") {
            return(i)
        }
    }
}

#' Retrieve LRI (Linear Retention Indices) of a compound via its PubChem CID.
#' 
#' This retrieves LRI (Linear Retention Indices) of a compound via its PubChem
#' CID using PubChem API. LRI of different types of columns are available
#' ("polar", "non-polar", "semi-standard non-polar"). When a canonical SMILES
#' is given as argument, the LRI for every type of column is calculated
#' automatically if the compound is an n-alkane (linear) as the number of
#' carbon atoms multiplied by 100.
#' 
#' @param CID A single PubChem CID as integer
#' @param type The type of column: "polar", "non-polar", "semi-standard non-polar"
#' @param canonicalSMILES Canonical SMILES of the compound
#' @return A vector of LRI values for the corresponding column
#' @examples
#' getLRI(6549, "polar")
#' getLRI(6549, "non-polar", "CC(=CCCC(C)(C=C)O)C")
#' getLRI(702, "polar")
#' getLRI(8182, "polar", "CCCCCCCCCCCC")
getLRI <- function(CID, type=c("polar", "non-polar", "semi-standard non-polar"), canonicalSMILES=NA) {
    if (is.na(CID)) return(NA)
    
    # Check if compound is an n-alkane (linear)
    if (!is.na(canonicalSMILES)) {
        if (grepl("^c+$", tolower(canonicalSMILES))) return(nchar(canonicalSMILES) * 100)
    }
    
    # Attribute local variable depending on type argument
    type <- tolower(type)
    if (type == "polar") name <- "Standard polar"
    if (type == "non-polar") name <- "Standard non-polar"
    if (type == "semi-standard non-polar") name <- "Semi-standard non-polar"
    
    # Query PubChem
    object <- try(PubChemR::get_pug_view(identifier=CID, domain="compound", annotation = "Data"))
    if (inherits(object, "try-error") | length(object) == 0) return(NA)
    section <- try(PubChemR::sectionList(object, .pattern="Chemical and Physical Properties"))
    if (inherits(section, "try-error") | length(section) == 0) return(NA)
    section <- section$SectionID
    df <- try(PubChemR::section(object, .id=section))
    if (inherits(df, "try-error") | length(df) == 0) return(NA)
    df <- df[["result"]][["Section"]][[2]][["Section"]]
    
    # Loop through LRI types: polar, non-polar, semi-standard non-polar or all column types
    subsubsection <- findKovatsSubsubsection(df)
    LRISection <- df[[subsubsection]][["Information"]]
    # If "All column types" type is the only type then return always these LRI
    if (length(LRISection) == 1 & LRISection[[1]]$Name == "All column types") {
        return(LRISection[[1]]$Value$Number)
    }
    for (i in 1:length(LRISection)) {
        currentSection <- LRISection[[i]]
        if (currentSection$Name == name) {
            return(currentSection$Value$Number)
        }
    }
    return(NA)
}

#' Retrieve descriptors (taste or odor) of a compound via its PubChem CID.
#' 
#' This retrieves descriptors (taste or odor) of a compound via its PubChem CID
#' using PubChem API.
#' 
#' @param CID A single PubChem CID as integer
#' @param type "Odor" or "Taste"
#' @return Descriptors of the corresponding type for the compound as characters
#' @examples
#' getPubchemDescriptors(6549, "Odor")
#' getPubchemDescriptors(6549, "Taste")
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

#' Retrieve descriptors (odor only) of a compound via its PubChem CID using
#' Flavornet database as flat file.
#'
#' @param CID A single PubChem CID as integer
#' @return Odor descriptors of the corresponding compound as characters
#' @examples
#' getFlavornetOdorDescriptors(263)
getFlavornetOdorDescriptors <- function(CID) {
    if (is.na(CID)) return(NA)
    db <- read.csv("./data/descriptor_db/flavornet_odor_descriptors.csv", header=TRUE, na.strings=c("NA", ""))
    CID <- as.numeric(CID)
    db <- db[db$CID == CID, "odor_flavornet"]
    if (identical(db, character(0))) return(NA)
    return(db)
}

#' Retrieve descriptors (odor only) of a compound via its CAS registry number using
#' TheGoodScentsCompany database as flat file.
#'
#' @param CAS A single CAS registry number as characters
#' @return Odor descriptors of the corresponding compound as characters
#' @examples
#' getGoodScentsOdorDescriptors("98-86-2")
getGoodScentsOdorDescriptors <- function(CAS) {
    db <- read.csv("./data/descriptor_db/goodscents_odor_descriptors.csv", header=TRUE, na.strings=c("NA", ""))
    db <- db[db$CAS == CAS, "odor_goodscents"]
    if (identical(db, character(0))) return(NA)
    return(db)
}