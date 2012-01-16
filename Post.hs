module Post where

import Database.HDBC
import Database.HDBC.Sqlite3


db :: IO Connection
db = connectSqlite3 "blog.sqlite3"

findPosts = [(String, SqlValue)]
findPosts = do
      conn <- db
      stmt <- prepare conn "select count(*) from posts"
      execute stmt []
      fetchRow stmt

