#!/bin/sh

echo "Starting migrations"
MIGRATIONS=$(echo *.sql | sort -n -k1.2)
echo $MIGRATIONS
for i in $MIGRATIONS
do
	psql postgresql://postgres:postgres@db:5432 -f $i
done
echo "Migration complete"
