# MILQMAT
This repository contains the code for predicting raw milk sales from vending machines in Germany.

| Variable name            | Variable description                                                                             |
|--------------------------|--------------------------------------------------------------------------------------------------|
| ID                       | Sequential ID number                                                                             |
| ja_2019                  | Annual raw milk sales, 2019 [l]                                                                  |
| alter_BL                 | Age of the farmer                                                                                |
| kuehe                    | Number of cows                                                                                   |
| bio                      | Organic production [dummy]                                                                       |
| umsatzteil               | Share of milk production in total farm revenue [%]                                               |
| modell                   | Vending machine model                                                                            |
| inbetrieb                | Year of commissioning                                                                            |
| preis                    | Raw milk selling price [€/l]                                                                     |
| produkte                 | Additional product range [dummy]                                                                 |
| automaten                | Additional product sale via vending machines [dummy]                                             |
| hofladen                 | Additional product sale via farm store [dummy]                                                   |
| vertrauen                | Additional product sale via trust box [dummy]                                                    |
| immerauf                 | Opening hours: 24/7 [dummy]                                                                      |
| s_touris                 | Location in tourist area [dummy]                                                                 |
| s_nah                    | Location in recreational area [dummy]                                                            |
| s_stadt                  | Location near a town or city [dummy]                                                             |
| s_pendel                 | Location with commuter traffic [dummy]                                                           |
| strasse                  | Location near (federal) highway [dummy]                                                          |
| gmaps                    | Listed in Google Maps [dummy]                                                                    |
| online                   | Online advertising                                                                               |
| zeitung                  | Newspaper advertising [dummy]                                                                    |
| schild                   | Sign advertising [dummy]                                                                         |
| fb                       | Facebook page [dummy]                                                                            |
| hpage                    | Website [dummy]                                                                                  |
| automatendichte          | Raw milk vending machines in the county, 2019 [machines per 100 km²]                             |
| dichte_G                 | Population density in the municipality, 2019 [population/km²]                                    |
| Ländlichkeit_2016g       | Rurality in the municipality, 2016a                                                              |
| Fahrzeit_Smarkt_2017     | Accessibility of a supermarket in the municipality, 2017 [min driving time]                      |
| Milchviehbetriebe_2016   | Number of dairy farms in the county, 2016                                                        |
| Durchschnittsalter_2019  | Average age in the county, 2019                                                                  |
| Pachtentgelt_2010        | Lease fee in the county, 2010 [€/ha]                                                             |
| Sek1_2019                | Share of employees in the agricultural sector in the county, 2019 [%]                            |
| bquali_unifh_2020        | Proportion of university graduates in the county, 2020 [%]                                       |
| Milchpreis_2019_B        | Average milk price in the state (2011-2021) [€/100 kg]                                           |
| EinkommenpP              | Annual income per capita in the municipality, 2016 [€ 000]                                       |
