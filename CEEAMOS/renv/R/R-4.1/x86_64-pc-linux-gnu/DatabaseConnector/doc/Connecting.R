## ---- echo = FALSE, message = FALSE-------------------------------------------
library(DatabaseConnector)


## ----eval=FALSE---------------------------------------------------------------
## Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "c:/temp/jdbcDrivers")


## ----eval=FALSE---------------------------------------------------------------
## install.packages("usethis")
## usethis::edit_r_environ()


## ----eval=FALSE---------------------------------------------------------------
## Sys.setenv("DATABASECONNECTOR_JAR_FOLDER" = "c:/temp/jdbcDrivers")


## ----eval=FALSE---------------------------------------------------------------
## downloadJdbcDrivers("postgresql")

## ----echo=FALSE---------------------------------------------------------------
writeLines("DatabaseConnector JDBC drivers downloaded to 'c:/temp/jdbcDrivers'.")


## ----eval=FALSE---------------------------------------------------------------
## install.packages("RSQLite")


## ----eval=FALSE---------------------------------------------------------------
## conn <- connect(dbms = "postgresql",
##                 server = "localhost/postgres",
##                 user = "joe",
##                 password = "secret")

## ----echo=FALSE---------------------------------------------------------------
writeLines("Connecting using PostgreSQL driver")


## ----eval=FALSE---------------------------------------------------------------
## disconnect(conn)


## ----eval=FALSE---------------------------------------------------------------
## conn <- connect(dbms = "postgresql",
##                 connectionString = "jdbc:postgresql://localhost:5432/postgres",
##                 user = "joe",
##                 password = "secret")

## ----echo=FALSE---------------------------------------------------------------
writeLines("Connecting using PostgreSQL driver")


## ----eval=FALSE---------------------------------------------------------------
## details <- createConnectionDetails(dbms = "postgresql",
##                                    server = "localhost/postgres",
##                                    user = "joe",
##                                    password = "secret")
## conn <- connect(details)

## ----echo=FALSE---------------------------------------------------------------
writeLines("Connecting using PostgreSQL driver")


## -----------------------------------------------------------------------------
conn <- connect(dbms = "sqlite", server = tempfile())

# Upload cars dataset as table:
insertTable(connection = conn, 
            tableName = "cars", 
            data = cars)

querySql(conn, "SELECT COUNT(*) FROM main.cars;")

disconnect(conn)

