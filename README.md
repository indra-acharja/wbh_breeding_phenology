# Data and Code for: Increasing variability and declining breeding success: A major challenge in the conservation of the critically endangered *\ Ardea insignis* (White-bellied Heron)

## General Information

**Journal:** *Ornithology* (American Ornithological Society), Oxford Academic.

**Authors:** Indra P. Acharja¹*, M. Clay Green¹, Yuji Okahisa², Thinley Phuntsho³, Tshering Tobgay³, Pema Khandu¹, Satoshi Shimano⁴

¹ Department of Biology, Texas State University, San Marcos, TX, 78666, USA   
² Faculty of Environment, University of Human Environments, Okazaki, Aichi Prefecture, 444-3505, Japan   
³ Species Conservation Division, Royal Society for Protection of Nature, Thimphu, Bhutan   
⁴ Science Research Center, Hosei University, Tokyo, 102-8160 Japan

*\*Corresponding Author* : indra.acharja@gmail.com

**Repository:** Texas State University Digital Repository (Dataverse)

---

## Project Description

# White-bellied Heron (*Ardea insignis*)

The White-bellied Heron is a Critically Endangered species restricted to the eastern Himalayas, occurring in Bhutan, northeast India, Myanmar, and parts of China. It is the second-largest heron in the world and among the rarest, with fewer than 250 mature individuals estimated globally. However, fewer than 60 individuals and only four to six breeding pairs are currently known.

This repository contains breeding data collected from 2016 to 2025, which were used in the analysis for the article titled "Increasing variability and declining breeding success: A major challenge in the conservation of the critically endangered *Ardea insignis* (White-bellied Heron)" published in Ornithology. The data include records of first egg-laying dates, clutch sizes, hatchling numbers, fledgling numbers, and nest outcomes (success or failure) across multiple breeding seasons. 


The data were collected through annual field surveys coordinated by the Royal Society for the Protection of Nature (RSPN) in collaboration with the Department of Forests and Park Services, Local Conservation Support Groups (LCSGs), volunteers, birdwatchers, and local communities.

---

## File Inventory

1. **`wbh_2016_to_2025_breedingdata.csv`**: The primary dataset containing breeding records for 2016–2025 used in analysis.

4. **`breeding_analysis_2016_2025.R`**: R script for data processing, statistical summaries, and visualization.

---

## Data Dictionary (`wbh_2016_to_2025_breedingdata.csv`)

| Column Name | Description |
| :--- | :--- |
| `location` | Name of the nest site location. |
| `river` | Name of the river associated with the nest site. |
| `first_egg_day` | Date the first egg was laid in the nest (MM/DD/YYYY). |
| `clutch_no` | Order of nesting attempt (1 = first attempt; 2 = re-nesting attempt). |
| `clutch_size` | Total number of eggs recorded in the nest. |
| `no_hatchling` | Total number of chicks that successfully hatched. |
| `no_fledgling` | Total number of chicks that successfully fledged from the nest. |
| `outcome` | Breeding outcome of the nest attempt (Success or Fail). |

*Note: Success is defined as at least one chick successfully fledging from the nest.*
---

## Usage

### Requirements
The analysis requires **R** and the following libraries:
* `tidyverse`
* `lubridate`
* `patchwork`
* `gt`

### Execution
1. Place the `wbh_2016_to_2025_breedingdata.csv` data file and the `breeding_analysis_2016_2025.R` script in the same working directory.
2. Run the script.
3. The script will automatically create a folder named `plots_maps` containing:
    * **Figure 01**: Monthly success and productivity.
    * **Figure 02**: Nest outcomes by location.
    * **Figure 03**: Annual variation in breeding timing.
    * **Table 01**: Yearly summary of first egg dates.
    * **Performance Table**: Transposed comparison of overall vs. clutch-specific metrics.

---

## Citations

**Article citation:**  
Acharja, I. P., Green, M. C., Okahisa, Y., Phuntsho, T., Tobgay, T., Khandu, P., & Shimano, S. (2026). Increasing variability and declining breeding success: A major challenge in the conservation of the critically endangered *Ardea insignis* (White-bellied Heron). *Journal of Ornithology*. https://doi.org/[article DOI] (to be updated upon publication)

**Data and code citation:**  
Acharja, I. P., Green, M. C., Okahisa, Y., Phuntsho, T., Tobgay, T., Khandu, P., & Shimano, S. (2026). Data and code for: Increasing variability and declining breeding success: A major challenge in the conservation of the critically endangered *Ardea insignis* (White-bellied Heron). [Data set and software]. Texas State University Digital Repository. https://doi.org/10.18738/T8/L8JRSM

---


## License
This dataset and code are licensed under a **Creative Commons Attribution 4.0 International (CC BY 4.0)** License.