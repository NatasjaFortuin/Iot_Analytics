install.packages("RMySQL")
library(RMySQL)

#### Learn SQL with iris ####
## Lists attributes contained in a table
dbCanConnect(iris)
iris <- dbConnect(MySQL(iris), ":memory:")
iris <- dbConnect(RMySQL::MySQL(), dbname = "iris")
dbListFields(iris)
irisALL <- dbGetQuery(con, "SELECT * FROM iris")


#To learn the attributes in that table, use the dbListFields command. 
#If you have waited too long since connecting to the database you may have to connect again.

```
## Lists attributes contained in a table
dbListFields(con,'iris')

We can query the database. We can download all of the data or choose the specific attributes we’re interested in.

```
## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")

## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")


#### Create a database connection ####
con = dbConnect(MySQL(),   
                user='deepAnalytics', 
                password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')
## List the tables contained in the database 
dbListTables(con)

#### Investigate dataframes ####
str(iris)
summary(iris)
head(iris)
tail(iris)

str(irisALL)
summary(irisALL)
head(irisALL)
tail(irisALL)

str(irisSELECT)
summary(irisSELECT)
head(irisSELECT)
tail(irisSELECT)

str(con)
summary(con)
head(con)
tail(con)

dim(irisALL)
date()
View(irisALL)
