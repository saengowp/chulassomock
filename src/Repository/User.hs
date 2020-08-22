module Repository.User (
        add,
        get,
        initDb
        ) where

import Model.User
import Database.SQLite.Simple as SQL
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Data.Aeson

add :: SQL.Connection -> User -> Text -> IO ()
add con user ticket = SQL.execute con "INSERT INTO tickets (ticket, user) VALUES (?, ?);" (ticket, encode user)

get :: SQL.Connection -> Text -> IO (Maybe User)
get con ticket = do
        result <- SQL.query con "SELECT user FROM tickets WHERE ticket = ?;" (Only ticket) :: IO [ SQL.Only ByteString]
        let users = map (\(Only x) -> decode x) result :: [ Maybe User ]
        case users of
          [ (Just usr) ] -> return $ Just usr
          _ -> return $ Nothing

initDb :: SQL.Connection -> IO ()
initDb con = SQL.execute_ con "CREATE TABLE IF NOT EXISTS tickets (ticket TEXT PRIMARY KEY, user BLOB);"
