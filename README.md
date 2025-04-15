# trees-energy

This repository includes replication material for:
"Quasi-experimental evidence on the effect of the urban tree canopy on energy consumption"
by Fatemah Ravazdezh and Nicholas Rivers

To approximately replicate all results in the paper:
1. Clone the repository

2. Obtain non-confidential data from the Mendely repository at the following URL:
https://data.mendeley.com/preview/6g72mndtys?a=7eac386e-07ec-49cd-8437-434592e066ea
These data should be added to the "intermediate_data/" folder.

3. Execute the code "code/Master_script". Ensure that use_rounded_data is set to 1.

Note that the Mendeley data is a non-confidential version of the original data. Addresses have been removed and tree canopy data has been rounded to preserve anonymity. As a result, statistical results may not exactly replicate the results in the paper. Geographic information has been jittered to preserve anonymity.

In order to build the files in intermediate_data/, use the script code/Data_preparation.R.  To be able to execute this file requires the following confidential files in the raw_data/ directory:
- U_Ottawa_ClimateResiliency.gdb
- data_FINAL_2022_12_12.csv
- data_random_tree.csv
- LEAF.txt
- LEAF2.txt
- Municipal_address_points.csv
