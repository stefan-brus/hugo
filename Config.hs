-- Hugo configuration

module Config where

import qualified Database.Redis as R

server = "chat.freenode.net"
port = 6667
dbInfo = R.defaultConnectInfo
chans = ["##hugo-test", "##hugo-test2"]
nick = "___hugo___"
realname = "Hugo the bot"
cmdChar = ':'
msgMaxLen = 250
nsnsMaxLen = 10
