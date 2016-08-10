# before running this script, first must create a mongod instance on localhost ip (127.0.0.1:27017)
# And ensure the data is uploaded to the database by runnign the bash script and setting indexes

library(rmongodb)
mongo <- mongo.create()
mongo.is.connected(mongo)

db <- "sotonproject"
coll.pat <- "sotonproject.patents"
coll.cit <- "sotonproject.citations"

mongo.findOne(mongo, coll.pat)
pat.count <- mongo.count(mongo, coll.pat)

# This aggregate system has exponential scaling, 1e5 takes 1.2s, 1.5e5 takes 2.4s, 2e5 takes 15.8s
time <- Sys.time()
pipe1 <- mongo.bson.from.JSON('{ "$limit": 200000 }')
pipe2 <- mongo.bson.from.JSON('{ "$group": { "_id": "$Patent"}  }')
pipe3 <- mongo.bson.from.JSON('{ "$group": { "_id": 1, "count": { "$sum": 1 } } }')
cmd_list <- list(pipe1, pipe2, pipe3)
(unique.patents <- mongo.aggregation(mongo, coll.pat, cmd_list))
(time <- Sys.time() - time)

mongo.destroy(mongo)


# Read into memory, is slower at first but scales approximately linearly
# 2.78, 3.95, 5.44
library(readr)
library(dplyr)
time <- Sys.time()
df <- read_csv("../DataFiles/Cleaned2/patent_cat.csv", n_max = 150000)
df <- df %>% select(Patent) %>% group_by(Patent)
counts <- df %>% summarise(count = n())
(time <- Sys.time() - time)

# MapReduce
# # 1.7s 2.7 4.3
## Map reduce seems to be the best performaing and also scales apprxomately 



