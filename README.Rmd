# Heatwave

Below is the metadata for the experimental data files associated with 'Heatwave eliminates the benefit of adaptation to high temperatures in an experimental model system'

#### **General**

\- `heatwave`: the presence (1) or absence (0) of a 5 day heatwave of 42°C

\- `adapted_temp`: the temperature in °C under which beetles evolved for 49 months (25, 30, or 35)

\- `population_id`: replicate population (1-10 for each combination of `heatwave` and `adapted_temp`)

\- `sex`: female (f) or male (m)

\- `weight`: dry weight of beetle in grams

#### **heatwavepopulation.csv**

\- `weeks_since_heatwave`: weeks elapsed since the end of the heatwave at the time of data collection (0, 2, 6, 12, or 18)

\- `alive`: number of live beetles counted

#### **preheatwavebodysize.csv**

\- `replicate`: replicate individual beetle (1-10 for each `adapted_temp` and `sex` combination)

#### **postheatwavebodysize.csv**

\- `replicate`: replicate individual beetle (1-5 for each `population_id` and `sex` combination)

#### **heatwavefecundity.csv**

\- `replicate`: replicate individual female beetle (1-3 for each `population_id`)

\- `egg_count`: number of eggs laid by each female beetle in 48 hours

\- `notes`: fecundity tubes that had no beetle (nb) or a dead beetle (db) at the time of data collection (NA for tubes with living beetles)

#### **bodysizeexperiment.csv**

\- `size`: size category according to visual assessment (a, b, c, d, or e from smallest to largest)

\- `replicate`: replicate individual beetle (1-6 for each `size` and `sex` combination)

\- `survival_twoweeks`: the survival (s) or death (d) of beetles two weeks after a heatwave 