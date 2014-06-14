#! /bin/bash
filename=dump$(date +%Y%m%d-%H%M).sql.gz
pg_dump -Z 9 -U p-rout -f $filename -w p_rout


