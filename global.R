library(downloader)

# ozDASL data list
download("http://www.statsci.org/data/multiple.html", 'z.txt')

z <- paste(readLines('z.txt'), collapse = "")
startIX <- gregexpr(pattern = '<ul>',text = z)[[1]][1] # multiple regression datasets start
endIX <- gregexpr(pattern = '</ul>',text = z)[[1]][1] # multiple regression datasets end

dataMeterials <- substr(z, startIX, endIX-1)
DataList <-  strsplit(gsub(pattern = "<.*?>", '',dataMeterials), '[()]')[[1]]
Title <- DataList[seq(1, length(DataList), 2)]
Title <- sapply(Title, function(x) gsub('\\s{2}', '', x)) # remove surplus spaces in front of title
VarNu <- DataList[seq(2, length(DataList), 2)]

DataTable <- data.frame('title'=Title, 'var_nu'=VarNu)
row.names(DataTable) <- 1:dim(DataTable)[1]
DataTable <- DataTable[-c(7,9,10,16),]


# Description of Each datasets
para <- gregexpr(pattern = '<ul>',text = z)[[1]]
dataLinkS <- gregexpr(pattern = 'href="',text = z)[[1]]
dataLinkS <- dataLinkS[which(dataLinkS > para[1])]
dataLinkE <- gregexpr(pattern = '.html',text = z)[[1]]
dataLinkE <- dataLinkE[which(dataLinkE > para[1])]

dataLinkS <- dataLinkS[1:dim(DataTable)[1]]
dataLinkS <- dataLinkS[-c(7,9,10,16)]
dataLinkE <- dataLinkE[1:dim(DataTable)[1]]
dataLinkE <- dataLinkE[-c(7,9,10,16)]


# dataLinkS <- dataLinkS[-c(7,9,10,16,26,39,44,45,49,69,72:76)]
# dataLinkE <- dataLinkE[-c(7,9,10,16,26,39,44,45,49,69,72:78)]
urlS <- "http://www.statsci.org/data/"
urlE <- ".html"

