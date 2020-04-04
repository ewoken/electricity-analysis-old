#!/bin/bash

mkdir -p 0_data
rm -rf 0_data/*

for i in {2012..2019}
do
   echo "$i"
   curl "https://opendata.reseaux-energies.fr/explore/dataset/eco2mix-national-cons-def/download/?format=csv&disjunctive.nature=true&refine.date_heure=$i" > "data/$i.csv"
   sed -i -e 's/:00\+00:00;/:00\+0000;/g' "data/$i.csv"
done

curl "https://opendata.reseaux-energies.fr/explore/dataset/parc-prod-par-filiere/download/?format=csv" > "data/installed_capacities.csv"
curl "https://opendata.reseaux-energies.fr/explore/dataset/pic-journalier-consommation-brute/download/?format=csv&q=date%3C%3D%222019-12-31T22:59:59Z%22" > "data/temperatures.csv"

rm -f 0_data/*.csv-e

mkdir -p 1_co2_impact_renewables/figures/multilinear_method
mkdir -p 2_heating_analysis/figures
mkdir -p 3_trade/figures
