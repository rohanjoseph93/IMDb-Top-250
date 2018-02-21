#Scraping Code :  https://rpubs.com/esundeep/webscape_imdb_rvest

library(rvest)
library(XML)
library(xml2)
library(ggplot2)

url = "http://www.imdb.com/chart/top?ref_=nv_wl_img_3"

page = read_html(url)

movie.nodes <- html_nodes(page,'.titleColumn a')

movie.link = sapply(html_attrs(movie.nodes),`[[`,'href')
movie.link = paste0("http://www.imdb.com",movie.link)
movie.cast = sapply(html_attrs(movie.nodes),`[[`,'title')
movie.name = html_text(movie.nodes)

sec <- html_nodes(page,'.secondaryInfo')

year = as.numeric(gsub(")","",                          # Removing )
                       gsub("\\(","",                   # Removing (
                            html_text( sec )                 # get text of HTML node  
                       )))

rating.nodes = html_nodes(page,'.imdbRating')
# Check One node
xmlTreeParse(rating.nodes[[20]])

rating.nodes = html_nodes(page,'.imdbRating strong')
votes = as.numeric(gsub(',','',
                        gsub(' user ratings','',
                             gsub('.*?based on ','',
                                  sapply(html_attrs(rating.nodes),`[[`,'title')
                             ))))

rating = as.numeric(html_text(rating.nodes))

top250 <- data.frame(movie.name, movie.cast, movie.link,year,votes,rating)

#Actors and Directors Analysis
actors <- unlist(strsplit(as.character(top250$movie.cast),","))
A <- data.frame(table(actors))

B <- A[!grepl("(dir.)", A$actors),]
B <- B[order(-B$Freq),]
actors_10 <- B[1:12,]

C <- A[ grep("(dir.)", A$actors, invert = FALSE) , ]
C <- C[order(-C$Freq),]
directors_10 <- C[1:10,]

#Top actors plot
ggplot(data=actors_10, aes(x=reorder(actors,-Freq), y=Freq)) +
geom_bar(stat="identity",fill="steelblue") + theme_minimal()+ labs(x="Actors",y="Frequency",title="Top Actors")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Top directors plot
ggplot(data=directors_10, aes(x=reorder(actors,-Freq), y=Freq)) +
geom_bar(stat="identity",fill="steelblue") + theme_minimal()+ labs(x="Directors",y="Frequency",title="Top Directors")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
