#!/bin/bash
set -e

mkdir -p _data
mkdir -p _data/france
rm -rf _data/france/*

mkdir -p _data/france/prices

for i in {2012..2019}
do
   echo "$i"
   curl "https://opendata.reseaux-energies.fr/explore/dataset/eco2mix-national-cons-def/download/?format=csv&disjunctive.nature=true&refine.date_heure=$i" > "_data/france/$i.csv"
   sed -i -e 's/:00\+00:00;/:00\+0000;/g' "_data/france/$i.csv"

   curl "https://ewoken.github.io/epex-spot-data/data/$i.csv" > "_data/france/prices/$i.csv"
done

curl "https://opendata.reseaux-energies.fr/explore/dataset/parc-prod-par-filiere/download/?format=csv" > "_data/france/installed_capacities.csv"
curl "https://opendata.reseaux-energies.fr/explore/dataset/pic-journalier-consommation-brute/download/?format=csv&q=date%3C%3D%222019-12-31T22:59:59Z%22" > "_data/france/temperatures.csv"

rm -f _data/france/*.csv-e

mkdir -p figures
