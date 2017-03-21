#' Sample skeleton for custom science - Main application function
#'
#' @import keboola.r.docker.application
#' @export
#' @param datadir Path to data directory.
doSomething <- function(datadir) {
 # read input
# Get the file from FTP

	# do something
	app <- DockerApplication$new(datadir)
	app$readConfig()
	
	UserName <- app$getParameters()$UserName
	PassWord <- app$getParameters()$PassWord
	MyTable <- app$getParameters()$Table
	attempts <- app$getParameters()$attempts
	delay <- app$getParameters()$delay
	
	# Authentication request
	library(RCurl)
	library(httr)
	library(XML)

	apiURL <- 'api6.silverpop.com/XMLAPI'


	body1 <- "<Envelope><Body>
	<Login>
	<USERNAME>UserName</USERNAME>
	<PASSWORD>PassWord</PASSWORD>
	</Login>
	</Body></Envelope>"

	body1 <- gsub("UserName", UserName, body1)
	body1 <- gsub("PassWord", PassWord, body1)

	test1 <- POST(url = apiURL, body = body1, 
				  verbose(), content_type("text/xml"))


	parsed <- htmlParse(test1)
	js <- xpathSApply(parsed, "//session_encoding", xmlValue)

	#Gather sessionid for further requests
	jsessionid <- gsub(";","?",js)


	## Date parameters: COULD WE PASS DATE PARAMETERS FROM THE ORCHESTRATION LAYER?
	body2 <- "<Envelope>
	<Body>   
	<ExportTable>    
	<TABLE_ID>MyTable</TABLE_ID>
	<EMAIL>Email</EMAIL> 
	<EXPORT_FORMAT>CSV</EXPORT_FORMAT>   
	</ExportTable>  
	</Body> 
	</Envelope>"

	# Trick to pass the parameters

	test2 <- POST(url = paste(apiURL,jsessionid,sep=""), body = body2
				  ,verbose(), content_type("text/xml"))


	xml_data <- xmlParse(test2)

	## 
	nodes <- getNodeSet(xml_data, "//FILE_PATH")

	## Parse response to Data Frame
	data <- xmlToDataFrame(nodes)

	fname <- as.character(data[[1]])
	fname<-substr(fname, 11, 70)

	fname_df <- data.frame(filename = fname)

	last_file <- fname_df[1,1]


	url <- "sftp://transfer6.silverpop.com/download/"
	userpwd <- paste0(UserName,":",PassWord)


	data_01 <- NULL
	attempt <- 1
	while(is.null(data_01) && attempt <= attempts ) {
	  attempt <- attempt + 1
	  Sys.sleep(delay)
	  try(
		
		data_01 <-getURL(paste(url,last_file,sep=""), userpwd = userpwd)
		)
	} 

	data <- read.csv(textConnection(data_01, encoding = "UTF-8"), encoding = "UTF-8")
#	head(data)

	write.csv(data, file = file.path(datadir, "out/tables/CRMi.csv"), row.names = FALSE)

}
