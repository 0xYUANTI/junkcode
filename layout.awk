#!/usr/bin/mawk -f

# long lines
length() > 79

# trailing whitespace
/[ ]+$/

# tabs
/[\t]/

# file headers
/%%%%%$/ && length != 79

# topic headers
/=====$/ && length != 72
/-----$/ && length != 72

# eof