#!/bin/bash

ADS_HOSTS_FILE="/Users/d1egoaz/dotfiles/dnsmasq/dnsmasq.d/hosts/ads.hosts"
HOSTSTEMP=/tmp/ads.hosts.temp
HOSTSTEMP_SORT_UNIQUE=/tmp/ads.hosts.sort.temp

# http://blog.ksx4system.net/wp-content/uploads/blocklista.txt
ADS_SERVERS='http://jazz.tvtom.pl/download/hosts
http://adaway.org/hosts.txt
http://adblock.gjtech.net/?format=unix-hosts
http://hosts-file.net/ad_servers.txt
http://pgl.yoyo.org/as/serverlist.php?hostformat=hosts;showintro=0;mimetype=plaintext
http://someonewhocares.org/hosts/hosts
http://sysctl.org/cameleon/hosts
http://www.malekal.com/HOSTS_filtre/HOSTS.txt
http://www.malwaredomainlist.com/hostslist/hosts.txt
http://winhelp2002.mvps.org/hosts.txt'

WHITE_LIST='da.feedsportal.com'

if [ -f $HOSTSTEMP ] ; then
	rm $HOSTSTEMP
fi

# remueve retorno de carro -> tr -d '\r'
# encuentra 127.0.0.1 o 0.0.0.0 -> grep -E '^(127\.0\.0\.1|0\.0\.0\.0)'
# se deja solo la lista de servers -> awk '{print $2}'	
for url in $ADS_SERVERS ; do
	wget -O- $url | grep -E '^(127\.0\.0\.1|0\.0\.0\.0)' | tr -d '\r' | sed 's/[[:blank:]]\+/ /g' | awk '{print $2}' >> $HOSTSTEMP
done

for line in $WHITE_LIST; do 
  #sed --in-place "/$line/d" $HOSTSTEMP
  sed -i '' "/$line/d" $HOSTSTEMP
done

echo 'Sort + uniq + redirecting to 0.0.0.0'
sort $HOSTSTEMP | uniq > $HOSTSTEMP_SORT_UNIQUE
awk '{print "0.0.0.0 " $0}' $HOSTSTEMP_SORT_UNIQUE > $ADS_HOSTS_FILE
echo 'done!'
