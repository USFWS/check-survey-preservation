#' Generate report of projects and products for a refuge
#'
#' @param A string for a refuge cost center code
#' @param A Boolean for whether or not to make the ServCat API call secure
#'
#' @return A string of html code to paste as a text report
#'
#' @examples
#' getReport("FF07RAM000")
getReport <- function(id, internal = TRUE){
  if(length(id)==0){
    return("<i>No refuge selected</i>")
  }
  
  #Make call to get profiles of all current PRIMR surveys for selected refuge
  url <- paste0("https://ecos.fws.gov:443/primr/surveyApi/flatList?offset=0&max=100&order=id%2Casc&ccc=", id, "&status=Current")
  response <- GET(url = url, encode = "json", add_headers("Content-Type" = "application/json"), verbose())
  
  #Halt code if error
  if(http_error(response) == TRUE){
    stop("This request has failed.")
  }
  
  #Convert output from json for parsing
  json_output <- fromJSON((content(response, as = "text")))
  
  #Break if no projects exist
  if(json_output$count == 0){
    return("<i>No projects found.</i>")
  }
  
  #Store json content
  primr_data <- json_output$data
  surveyCount <- nrow(primr_data)
  
  #Start HTML survey report content with the total project count
  report <- paste("<br><b>Total Current Surveys:</b>", surveyCount,"<br>")
  
  #Instantiate vectors to hold ServCat information regarding each survey
  servcat_titles <- c()
  servcat_content <- c()
  
  #Iterate through surveys and pull product info from ServCat
  for (i in 1:surveyCount){
    #Add PRIMR info to report
    report <- paste0(report, "<h3>", i, ") ", primr_data$name[i], " <a href='https://ecos.fws.gov/primr/survey/edit/", primr_data$id[i], "' target='_blank'>(PRIMR link)</a></h3><b>Coordinator: </b>", primr_data$coordinatorName[i], "<br>")
    
    #Handle case of no linked ServCat project
    if(is.na(primr_data$products_servCatIds[i])){
      report <- paste(report, "<b>Project: </b><font color='red'>None linked</font><br>")
    }else{
      #Get reference codes for all corresponding ServCat projects
      projectCodes <- unlist(strsplit(primr_data$products_servCatIds[i], ", "))
      
      #Iterate through ServCat project/s
      for(val in projectCodes){
        #Make call for profile of each project, if exists
        if(internal == TRUE){
          url <- paste0("https://ecos.fws.gov/ServCatServices/servcat-secure/v4/rest/Profile?q=", val)
          response <- GET(url = url, config = authenticate(":",":","ntlm"), encode = "json", add_headers("Content-Type" = "application/json"), verbose())
        }else{
          url <- paste0("https://ecos.fws.gov/ServCatServices/servcat/v4/rest/Profile?q=", val)
          response <- GET(url = url, encode = "json", add_headers("Content-Type" = "application/json"), verbose())
        }
        
        #Halt if error triggered by API call
        if(http_error(response) == TRUE){
          stop("This request has failed.")
        }
        
        #Get reference profile as text
        json_output <- fromJSON((content(response, as = "text")))
        
        #Fix formatting for multiple projects
        if(length(projectCodes)>1){
          report <- paste(report, "<br>")
        }
        
        #Add ServCat project title and link to the report
        report <- paste(report, "<b>Project: </b>", json_output$bibliography$title, " <a href='https://ecos.fws.gov/ServCat/Reference/Profile/", val, "' target='_blank'>(ServCat link)</a><br>")
        
        #Capture product content: reference types, counts, and dates
        if(length(json_output$products)==0 || is.na(json_output$products)){
          #Handle case of no products
          results <- "<i><font color='gray'>None</font></i>"
        }else{
          #Handle case of products present
          products <- json_output$products[[1]]$referenceType
          dates <- substr(json_output$products[[1]]$dateOfIssue,1,4)
          types <- unique(json_output$products[[1]]$referenceType)
          type_counts <- c()
          type_dates <- c()
          for (element in types){
            type_counts <- append(type_counts, sum(str_count(element, products)))
            date_set <- sort(dates[which(products == element)])
            date_string <- ""
            for (i in 1:length(date_set)){
              if(length(date_set)==1){
                date_string <- paste0("(", date_set[i], ")")
              }else if(i==1){
                date_string <- paste0("(", date_set[i])
              }else if(i==length(date_set)){
                date_string <- paste0(date_string, ", ", date_set[i], ")")
              }else{
                date_string <- paste0(date_string, ", ", date_set[i])
              }
            }
            type_dates <- append(type_dates, date_string)
          }
          
          results <- "<br>"
          for (i in 1:length(types)){
            if(i == 1){
              results <- paste(results, paste(type_counts[i], "x", types[i], type_dates[i]), sep="")
            }else{
              results <- paste(results, paste("<br>", type_counts[i], "x", types[i], type_dates[i]), sep="")
            }
          }
        }
        
        #Add ServCat content to report
        report <- paste(report, "<b>Products: </b><font color='SlateBlue'>", results, "</font><br>")
      }
    }
  }
  
  #Return combined report
  return(report)
}

#' Get cost center code of a given refuge
#'
#' @param A string of the name of a refuge 
#'
#' @return A string of a cost center code
#'
#' @examples
#' getCCC("Alaska Maritime National Wildlife Refuge")
getCCC <- function(refuge){
  #Return cost center code corresponding to specified refuge
  return(regionTable$costCenterCode[which(regionTable$stationName == refuge)])
}

#' Get list of refuges for a given FWS region
#'
#' @param A number as an integer between 1 and 8 (inclusive) representing a region
#'
#' @return A list of strings representing refuge station names
#'
#' @examples
#' getRefugeList(7)
getRefugeList <- function(region){
  #Handle case of no region selected
  if(length(region)==0){
    return("")
  }
  
  #Return list of stations for specified region
  return(regionTable$stationName[which(regionTable$region == region)])
}

#' Get dataframe of all FWS refuge station names, their cost center code, and their region number
#'
#' @return A dataframe with 3 columns: 'stationName','costCenterCode', and 'region'
#'
#' @examples
#' getRegionList()
getRegionTable <- function(){
  #Make call
  url <- paste("https://ecos.fws.gov:443/primr/refugeApi/geo.json?offset=0&max=700&order=id%2Casc")
  response <- GET(url = url, encode = "json", add_headers("Content-Type" = "application/json"), verbose())
  
  #Halt code if error
  if(http_error(response) == TRUE){
    stop("This request has failed.")
  }
  
  #Convert output from json for parsing
  json_output <- fromJSON((content(response, as = "text")))
  refuges <- subset(json_output$features$properties, select = c(3,7))
  region <- substring(refuges$costCenterCode, 4, 4)
  refuges$region <- region
  
  #Return dataframe with refuge names, cost center codes, and region numbers
  return(refuges)
}