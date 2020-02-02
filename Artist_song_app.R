#Setup
#Define package list
packagelist=c('shiny',"shinythemes",'httr','jsonlite','XML',
              'stringr','DT','plotly','stringi','parallel')
#Load packages, installing if required
sapply(packagelist,function(x){
  if(x %in% installed.packages()[,1]){
    library(x,character.only = TRUE)
  }else{
    install.packages(x)
    library(x,character.only = TRUE)
  }
})

#Function to extract the artist id given a name
get_artist_id=function(artist='coldplay'){
  artist=tolower(artist)
  #remove spaces
  artist=gsub(' ','%20',artist)
#Define API call
base_ulr='http://musicbrainz.org/ws/2/artist/?query='
tryCatch({
Get_artist <- GET(paste0(base_ulr,artist,'&fmt=xml'))
#Result
result=xmlParse(Get_artist)
#Extract id from XML, if non return NA
artist_id=xmlGetAttr(xmlRoot(result)[[1]][[1]],'id')
return(artist_id)
},error = function(e){
  return(NA)
  })
}

################
##################
#Extract first the list of works for artist and then pass to lyric api
#return labled data frame with songs and lengths
get_track_list_and_lengths=function(artist='coldplay',
  artist_id="cc197bad-dc9c-440d-a5b5-d52ba2e14234"){

#Get works associated with artist id
  #Request
base_ulr2='http://musicbrainz.org/ws/2/work?artist='
Get_songs <- GET(paste0(base_ulr2,artist_id,'&limit=100&fmt=json'))
#Extract
work<- fromJSON(rawToChar(Get_songs$content))$works
work_count<- fromJSON(rawToChar(Get_songs$content))$'work-count'

#If the number of works is >100 loop to get remaining works
if(nrow(work)<work_count){
  for(i in 1:ceiling((work_count-100)/100)){
  offset=i*100  
  Get_songs_loc <- GET(paste0(base_ulr2,artist_id,'&limit=100&fmt=json&offset=',offset))
  work_loc<- fromJSON(rawToChar(Get_songs_loc$content))$works
  work=unique(rbind(work,work_loc))
}}

#Remove works not labled as Song
songs=unique(work[which(work$type=='Song'),])
artist_loc=gsub(' ','%20',artist)

#In use case do we need to process all songs or would it be adaquate to use a random sample?


#Pass each title to the lyrics API
#This could be parrelised 
cl <- makeCluster(mc <- getOption("cl.cores", 4))
#clusterExport(cl, "base")
clusterEvalQ(cl, library(httr))
clusterEvalQ(cl, library(jsonlite))

songlengths=parLapply(cl,songs[,'title'],function(z){
  #Set base url
base_ulr3='https://api.lyrics.ovh/v1/'
#Process to remove special characters includign spaces
loc=gsub('â???T ',"'",z)
loc=gsub('â???T',"'",loc)
loc=gsub(' ','%20',loc)

#Handle request
Get_lyrics <- GET(paste0(base_ulr3,artist_loc,'/',loc))
#If request fails return NA
#There are a lot of edge cases here, 
#need to better distinguish where no lyrics are availble vs failure to load
if(Get_lyrics$status_code==200){
  #Extract lyrics
  lyrics=fromJSON(rawToChar(Get_lyrics$content))$lyrics
  #If Instrumental word count is 0
  # Fix to deal with 
  if(lyrics=="Instrumental"){return(0)
    #Count words
    }else{return(length(strsplit(lyrics,"\\s+")[[1]]))}
}else{return(NA)}
})
stopCluster(cl)

names(songlengths)=songs[,'title']
#output
out=cbind.data.frame(Artist=stri_trans_totitle(artist),title=names(songlengths),Length=unlist(songlengths))
return(out[order(out$title),c("Artist","title","Length")])
}


#Output visualisations
#Data table
make_DT_output<-function(
  df=get_track_list_and_lengths(artist = 'muse',artist_id = get_artist_id(artist = 'muse'))
){
  loc=df[which(df$Length>0),]
datatable(cbind.data.frame(
  'Measure'=c('Artist','Mean Word Count','SD Word Count','Longest Song:','Shortest Song'),
'Value'=c(stri_trans_totitle(unique(as.character(df$Artist))),
  mean=signif(mean(df$Length,na.rm=T),3), 
  sd=signif(sd(df$Length,na.rm=T),3), 
  paste0(paste0(loc$title[which(loc$Length==max(loc$Length,na.rm=T))],collapse = '/'),
       ' (',max(loc$Length,na.rm=T),')'),
  paste0(paste0(loc$title[which(loc$Length==min(loc$Length,na.rm=T))],collapse = '/'),
       ' (',min(loc$Length,na.rm=T),')')
)), rownames = FALSE)
}

#Histogram visualisation
make_Plot_output<-function( 
  df_1=get_track_list_and_lengths(artist = 'muse',artist_id = get_artist_id(artist = 'muse')),
  df_2=NA){
  if(!is.na(df_2)){plot_frame=rbind(df_1,df_2)}else{plot_frame=df_1}
  if(is.na(df_2)){title='Distribution of Song Lengths'
  }else{
    title='Distribution of Song Lengths'
    test=t.test(df_1$Length, df_2$Length, var.equal = FALSE)
    stats=paste0(' t=',signif(test$statistic,4),
           ' df=',signif(test$parameter,3),
           ' p=',signif(test$p.value,3))
    title=paste0(title,'\n',stats)
    }
  
p=plot_ly(x = ~Length,color = ~Artist,data=plot_frame, type = "histogram") %>%
  layout(title = title,
         yaxis = list(title = 'Count'))
return(p)
}