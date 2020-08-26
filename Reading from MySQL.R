## Connecting to a server and listing all the databases available.

library(RMySQL)
ucsc_db <- dbConnect(MySQL(), user = "genome", host = "genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucsc_db, "show databases;")
dbDisconnect(ucsc_db)

## Connecting to a server, accessing a database, and listing tables.

hg_19 <- dbConnect(MySQL(), user = "genome", db = "hg19",
                  host = "genome-mysql.cse.ucsc.edu")
all_tables <- dbListTables(hg_19)
length(all_tables)
all_tables[1:5]

## Getting dimensions of a specific table

dbListFields(hg_19, "affyU133Plus2") # Columns of the table
dbGetQuery(hg_19, "select count(*) from affyU133Plus2") # Row number of the table

## Reading tables from MySQL database

affy_data <- dbReadTable(hg_19, "affyU133Plus2")
write.csv(affy_data, "affy data.csv")

## Selecting a specific subset

query <- dbSendQuery(hg_19, "select * from affyU133Plus2 where misMatches between 1 and 3")
affy_mis <- fetch(query)
quantile(affy_mis$misMatches)
affy_mis_small <- fetch(query, n = 10) # To prevent getting gigantic data with wrong queries
dim(affy_mis_small)
dbClearResult(query) # You need to delete the query

## You need to disconnect from database after you are done

dbDisconnect(hg_19)
