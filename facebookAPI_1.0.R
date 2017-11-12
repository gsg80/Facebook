library(Rfacebook)
library(jsonlite)

setwd("/Users/gsg/R Projects/Facebook")

#my_oauth <- fbOAuth(app_id = "112542622779401", app_secret = "663c2fec1724652679986a210da0d9cf") 
#save(my_oauth, file = "my_oauth")

load("my_oauth")
me <- getUsers("me", token=my_oauth)
#me$name

f <- "facebook.csv"
ver <- "1.0"

danecsv <- read.csv2(f)  

l <- length(danecsv[, "id_media_konto"])
likes <- ""


id <- ""
names <- ""
website <- ""
allLikes <- data.frame(id = id
                        , name = names
                        , website = website)

fbDane <- data.frame(id_media_konto = 0
                     , id = ""
                     , about = ""
                     , fan_count = 0
                     , link = ""
                     , name = ""
                     , phone = ""
                     , address = ""
                     , website = ""
                     , email = ""
                     , were_here = 0
                     , data = "")


for(i in 1:l){
  id <- ""
  d <- ""
  try({
    if(is.na(danecsv[i, "konto_id"])==TRUE){
      id <- danecsv[i, "konto_nazwa"] 
    } else {
      id <- danecsv[i, "konto_id"] 
    }
    likes <- getLikes(user=id, n=500, token=my_oauth)
    allLikes <- rbind(allLikes, data.frame(id = paste("'",likes[,"id"],"'",sep="")
                                     ,name = likes[,"names"]
                                     ,website = likes[, "website"]))
    
    url <- paste("https://graph.facebook.com/", id, "/?fields=about,category,fan_count,description,link,name,phone,rating_count,single_line_address,website,were_here_count,emails", "&access_token=112542622779401|663c2fec1724652679986a210da0d9cf", sep="")
    d <- fromJSON(url)
  }, silent=T) 
  
    if(length(d) == 1) {
      dataCrawling <- as.character(Sys.Date())	
      fbDane <- rbind(fbDane, data.frame(id_media_konto = danecsv[i, "id_media_konto"]
                                         , id = ""
                                         , about = ""
                                         , fan_count = 0
                                         , link = ""
                                         , name = ""
                                         , phone = ""
                                         , address = ""
                                         , website = ""
                                         , email = ""
                                         , were_here = 0
                                         , data = dataCrawling))      
    } else {
      #czyść znaki końca linii
      d$about <- gsub("\r", "", d$about, fixed = TRUE)	
      d$about <- gsub("\n", "", d$about, fixed = TRUE)		
      d$about <- gsub("\t", "", d$about, fixed = TRUE)
      d$about <- gsub('"', '', d$about, fixed = TRUE)
      
      d$address <- gsub("\r", "", d$address, fixed = TRUE)	
      d$address <- gsub("\n", "", d$address, fixed = TRUE)		
      d$address <- gsub("\t", "", d$address, fixed = TRUE)
      d$address <- gsub('"', '', d$address, fixed = TRUE)

      d$website <- gsub("\r", "", d$website, fixed = TRUE)	
      d$website <- gsub("\n", "", d$website, fixed = TRUE)		
      d$website <- gsub("\t", "", d$website, fixed = TRUE)
      d$website <- gsub('"', '', d$website, fixed = TRUE)

      d$name <- gsub("\r", "", d$name, fixed = TRUE)	
      d$name <- gsub("\n", "", d$name, fixed = TRUE)		
      d$name <- gsub("\t", "", d$name, fixed = TRUE)
      d$name <- gsub('"', '', d$name, fixed = TRUE)

      d$emails <- gsub("\r", "", d$emails, fixed = TRUE)	
      d$emails <- gsub("\n", "", d$emails, fixed = TRUE)		
      d$emails <- gsub("\t", "", d$emails, fixed = TRUE)
      d$emails <- gsub('"', '', d$emails, fixed = TRUE)
      
    #dodaj '' do id
    d$id <- paste("'", d$id, "'", sep="")
    dataCrawling <- as.character(Sys.Date())	
    fbDane <- rbind(fbDane, data.frame(id_media_konto = danecsv[i, "id_media_konto"]
                                       , id = if(is.null(d$id)==TRUE || identical(d$id, character(0))==TRUE) {""} else {d$id}
                                       , about = if(is.null(d$about)==TRUE || identical(d$about, character(0))==TRUE) {""} else {d$about}
                                       , fan_count = if(is.null(d$fan_count)==TRUE) {0} else {d$fan_count}
                                       , link = if(is.null(d$link)==TRUE || identical(d$link, character(0))==TRUE) {""} else {d$link}
                                       , name = if(is.null(d$name)==TRUE || identical(d$name, character(0))==TRUE) {""} else {d$name}
                                       , phone = if(is.null(d$phone)==TRUE || identical(d$phone, character(0))==TRUE) {""} else {d$phone}
                                       , address = if(is.null(d$single_line_address)==TRUE || identical(d$single_line_address, character(0))==TRUE) {""} else {d$single_line_address}
                                       , website = if(is.null(d$website)==TRUE || identical(d$website, character(0))==TRUE) {""} else {d$website}
                                       , email = if(is.null(d$emails)==TRUE || identical(d$emails, character(0))==TRUE) {""} else {d$emails}
                                       , were_here = if(is.null(d$were_here_count)==TRUE) {0} else {d$were_here_count}
                                       , data = dataCrawling))
    }

  print(i)
  if(i %% 100 == 0){

    write.csv(allLikes, "facebook_likes_out.csv", fileEncoding = "CP1250")
    write.csv(fbDane, "facebook_data_out.csv", fileEncoding = "CP1250")
  }
}

#allLikes
write.csv(allLikes, "facebook_likes_out.csv", fileEncoding = "CP1250")
write.csv(fbDane, "facebook_data_out.csv", fileEncoding = "CP1250")



#getpagedata <- getPage(324245387480, token=my_oauth, n=10)
#getpagedata <- getPage("377107222440637", token=my_oauth, n=500)
#View(getpagedata)
