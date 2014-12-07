-- Hugo configuration

module Config where

import qualified Database.Redis as R

server :: String
server = "chat.freenode.net"

port :: Integer
port = 6667

dbInfo :: R.ConnectInfo
dbInfo = R.defaultConnectInfo

chans :: [String]
chans = ["##hugo-test", "##hugo-test2"]

nick :: String
nick = "___hugo___"

realname :: String
realname = "Hugo the bot"

cmdChar :: Char
cmdChar = ':'

msgMaxLen :: Integer
msgMaxLen = 250

nsnsMaxLen :: Integer
nsnsMaxLen = 10

admin :: String
admin = "stefan_1"

password :: String
password = "hunter2"
