#!/bin/bash

mkdir -p data
rm -rf data/*

for i in {2012..2018}
do
   echo "$i"
   curl "https://opendata.reseaux-energies.fr/explore/dataset/eco2mix-national-cons-def/download/?format=csv&disjunctive.nature=true&refine.date_heure=$i" > "data/$i.csv"
   sed -i -e 's/:00\+00:00;/:00\+0000;/g' "data/$i.csv"
done

curl "https://opendata.reseaux-energies.fr/explore/dataset/parc-prod-par-filiere/download/?format=csv" > "data/installed_capacities.csv"

rm -f data/*.csv-e

mkdir -p figures/linear_method
mkdir -p figures/multilinear_method
mkdir -p figures/season_distrib
