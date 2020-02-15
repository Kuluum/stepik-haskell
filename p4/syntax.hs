import Data.Time.Clock
import Data.Time.Format
-- import System.Locale

timeToString :: UTCTime -> String
timeToString _ = "as" --formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry { timestamp  :: UTCTime, logLevel :: LogLevel, message ::String }

logLevelToString :: LogLevel -> String
logLevelToString Error = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info = "Info"

logEntryToString :: LogEntry -> String
logEntryToString (LogEntry timestamp logLevel message) = (timeToString timestamp) ++ ": " ++ (logLevelToString logLevel) ++ ": " ++ message

data Person = Person { firstName :: String, lastName :: String, age :: Int }

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 {lastName = lastName p1}

abbrFirstName :: Person -> Person
abbrFirstName p@(Person{firstName = fn}) = if length fn > 2 then p {firstName = [(head fn), '.']} else p