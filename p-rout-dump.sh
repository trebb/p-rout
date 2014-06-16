#! /bin/bash

dumpdir=${1-'/var/lib/p-rout/'}
filename=dump$(date +%Y%m%d-%H%M).sql.gz

find $dumpdir/ -name dump*-*.sql.gz -mtime +14 -delete
pg_dump -Z 9 -U p-rout -f $dumpdir/$filename -w p_rout
