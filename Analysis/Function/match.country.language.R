#' @title Matching a country to it's language (present in the tree)
#'
#' @description Matches a country to it's spoken language if it's in the tree
#'
#' @param country A country (character)
#' @param country_language A list of two columns, first one being the countries and second one being the language(s) spoken in it
#' @param tree One language tree.
#'
#' @author Thomas Guillerme
#' 
match.country.language <- function(country, country_languages, languages) {
    #Find the corresponding language
    language_out <- country_languages[which(country_languages[,1] == country),2]

    #Check if the language is present
    if(length(language_out) == 1) {
        if(!is.na(match(language_out, languages))) {
            return(language_out)
        } else {
            return(NA)
        }
    } else {
        if(any(!is.na(match(language_out, languages)))) {
            #Check which language is present
            if(any(is.na(match(language_out, languages)))) {
                language_out <- language_out[-which(is.na(match(language_out, languages)))]
            } else {
                return(paste(language_out, collapse = "&"))
            }

            if(length(language_out) == 1) {
                return(language_out)
            } else {
                return(paste(language_out, collapse = "&"))
            }
        } else {
            return(NA)
        }
    }
}