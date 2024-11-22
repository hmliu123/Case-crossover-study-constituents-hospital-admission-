# Example Code for "Reduced Fine Particulate Air Pollution with Cause-specific Hospital Admissions "
## Overview
Example code and test data for the main analysis in the paper "Reduced Fine Particulate Air Pollution with Cause-specific Hospital Admissions"3
Note: The health insurance data analyzed in this study are regulated by governmental policies and cannot be made available to the public due to privacy reasons. Thus, we generate a fake dataset as  an example, the results are not consistent with the paper.

## Software details
R Software, version 4.0.3

## Instructions to use data
1. "test_data.csv" is the fake dataset of hospital admission for LRI from 5 cities from 2017-01-01 to 2017-12-31.
2. "test_data_pol.csv" is the dataset for daily concentration on PM2.5 and its major constituents in the 5 cities.
3. File folder "Result" provides necessary results to further calculate annual attributable fractions and total avoid numbers.

## Instructions to use code
1. Necessary packages are provided in the codes, please first install the packages.
2. The shared codes are used to generate the main analysis of the paper, codes for drawing pictures are not shown.
3. The codes are generated for estimating the risk and burden of one cause-specific hospital admission (LRI in the example data) associated with short-term exposure to PM2.5 and its major constituents. However, the same code can be used (by changing the data of other cause-specific hospital admissions) to develop the results for other outcomes (i.e., hospital admissions for CHD and CKD)
