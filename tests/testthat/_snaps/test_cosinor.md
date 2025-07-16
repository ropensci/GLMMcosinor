# script works and warnings are displayed appropriately

    Code
      test_object
    Output
      Test Details: 
      Parameter being tested:
      Amplitude
      
      Comparison type:
      levels
      
      Grouping variable used for comparison between groups: X
      Reference group: control
      Comparator group: test
      
      cglmm model only has a single component and to compare
                between groups.
      
      
      
      Global test: 
      Statistic: 
      2.19
      
      P-value: 
      0.1385
      
      
      Individual tests:
      Statistic: 
      1.48
      
      P-value: 
      0.1385
      
      Estimate and 95% confidence interval:
      1.68 (-0.54 to 3.9)

# multi-component comparison works, print functions work

    Code
      print(object, digits = 2)
    Output
      
       Conditional Model 
      
       Raw formula: 
      Y ~ group + group:main_rrr1 + group:main_sss1 + group:main_rrr2 +      group:main_sss2 
      
       Raw Coefficients: 
                       Estimate
      (Intercept)           1.0
      group1               -0.5
      group0:main_rrr1     -2.0
      group1:main_rrr1      0.9
      group0:main_sss1      0.3
      group1:main_sss1      0.3
      group0:main_rrr2     -2.1
      group1:main_rrr2      1.0
      group0:main_sss2      0.2
      group1:main_sss2      0.3
      
       Transformed Coefficients: 
                     Estimate
      (Intercept)         1.0
      [group=1]          -0.5
      [group=0]:amp1      2.0
      [group=1]:amp1      0.9
      [group=0]:amp2      2.1
      [group=1]:amp2      1.1
      [group=0]:acr1      3.0
      [group=1]:acr1      0.3
      [group=0]:acr2      3.0
      [group=1]:acr2      0.3

