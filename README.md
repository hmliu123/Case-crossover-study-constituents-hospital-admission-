# Example Codes for "Hospital admissions attributable to reduced air pollution due to clean-air policies in China"
## Overview
Example codes and test data for the main analysis of the paper "Hospital admissions attributable to reduced air pollution due to clean-air policies in China"
Note: The health insurance data analyzed in this study are regulated by governmental policies and cannot be made available to the public due to privacy reasons. Thus, we generate a fake dataset as  an example, the results are not consistent with the main findings of the paper.

## Software details
R Software, version 4.0.3

## Instructions to use data
1. "test_data.csv" is the fake dataset of hospital admissions for LRI from 5 cities during 2017-01-01 - 2017-12-31.
2. "test_data_pol.csv" is the dataset for daily concentrations of PM2.5 and its major constituents in the example 5 cities in 2017.
3. "BC_annual_AF.csv" contains annual attributable fractions for major cause-specific hospital admissions associated with black carbon during 2013-2017, which is used for the Theil-Sen median slope estimator and Mann-Kendall trend test.
4. The file folder "Result" provides the necessary data to calculate annual attributable fractions and total avoid numbers.
5. LRI in the code presents for low respiratory infections.
## Instructions to use code
1. Necessary packages are provided in the codes, please first install the packages.
2. The shared codes are used to generate the main analysis of the paper, codes for drawing pictures are not provided.
3. The codes are generated to estimate the risk and burden of one cause-specific hospital admission (LRI in the example data) associated with short-term exposure to PM2.5 and its major constituents. However, the same code can be used (by changing the dataset of other cause-specific hospital admissions) to develop the results for other outcomes (i.e., hospital admissions for CHD and CKD)
4. "funccmake.R" is the source code used to generate case-crossover dataframes.
