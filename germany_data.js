const fs = require('fs');
const fetch = require('node-fetch');
const { range, merge } = require('lodash');
const moment = require('moment-timezone');

const TYPES = [
  'trade',
  'hydro',
  'biomass',
  'nuclear',
  'brown_coal',
  'hard_coal',
  'oil',
  'gas',
  'others',
  'pumped_storage',
  'seasonal_storage',
  'wind',
  'solar'
]
const FORMAT = "YYYY-MM-DDTHH:mm:ss+0000"

async function fetchYearData(year) {
  const res = await fetch(`https://www.energy-charts.de/power/year_${year}.json`);
  const d = await res.json();

  const f = d.map((typeData, i) => {
    const type = (year == 2010 && i >= 8) ? TYPES[i + 1] : TYPES[i];
    const values = typeData.values.map(t => ({ [type]: Math.round(t[1] * 1000) }));

    return values;
  });
  const g = f[0].map((_, i) => f.map(row => row[i])).map(items => merge(...items));

  const startYear = moment().year(year).startOf('year');
  const unit = year == 2019 ? 'minutes' : 'hours';
  const step = year == 2019 ? 15 : 1;
  return g.map((item, i) => ({
    startDate: moment(startYear).add(i * step, unit).format(FORMAT),
    endDate: moment(startYear).add((i + 1) * step, unit).format(FORMAT),
    others: 0,
    ...item,
  }));
}

function toCSV(array, keys) {
  keys = keys || Object.keys(array[0]);

  const headers = keys.join(',');
  const lines = array.map(value => {
    return keys.map(key => value[key]).join(',');
  });
  const content = [headers, ...lines, '\n'];
  return content.join('\n');
}

async function main() {
  const years = range(2010, 2020);
  const yearData = await Promise.all(years.map(year => fetchYearData(year)));

  years.forEach((year, i) => {
    fs.writeFileSync(`./0_germany_data/${year}.csv`, toCSV(yearData[i]));
  });
}

main()



