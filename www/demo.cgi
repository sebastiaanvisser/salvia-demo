#!/bin/bash
echo -en "Content-Type: text/html; charset=utf-8\r\n"
echo -en "\r\n"

echo "CGI demo"
echo "<hr>"

echo "<pre>"
cat `find src -name "*.hs"`
echo "</pre>"

