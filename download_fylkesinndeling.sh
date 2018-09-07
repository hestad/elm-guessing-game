#!/bin/bash
set -e

curl -o kommuner.json 'http://data.ssb.no/api/klass/v1/classifications/104/corresponds?targetClassificationId=131&from=2018-08-09' -H 'Accept: application/json; charset=UTF-8'
