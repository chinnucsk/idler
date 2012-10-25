cd ~/src/idler/priv/
rm yflrss.xml
cat start.XML > yflrss.XML
echo "<pubDate>" >> yflrss.XML
echo $(date -u) >> yflrss.XML
echo "</pubDate>" >> yflrss.XML
echo >> yflrss.XML
for i in $(ls -1 -tr *.xml); do
   cat $i >> yflrss.XML
   echo >> yflrss.XML
done
echo >> yflrss.XML
cat end.XML >> yflrss.XML
mv yflrss.XML yflrss.xml
/home/gert/uploadrss.sh
find /home/gert/src/idler/priv/*.xml -mtime +7 -exec rm {} \;

