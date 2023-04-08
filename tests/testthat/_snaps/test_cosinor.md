# multi-component comparison works and print functions work

    
     Conditional Model 
    
     Raw formula: 
    Y ~ group + group:main_rrr1 + group:main_sss1 + group:main_rrr2 +      group:main_sss2 
    
     Raw Coefficients: 
         (Intercept)           group1 group0:main_rrr1 group1:main_rrr1 
           0.9911170       -0.4795597       -1.9309625        0.9272635 
    group0:main_sss1 group1:main_sss1 group0:main_rrr2 group1:main_rrr2 
          -0.2314901       -0.3386632       -2.0734429        0.9937285 
    group0:main_sss2 group1:main_sss2 
          -0.3049441       -0.2648390 
    
     Transformed Coefficients: 
       (Intercept)      [group=1] [group=0]:amp1 [group=1]:amp1 [group=0]:amp2 
         0.9911170     -0.4795597      1.9447889      0.9871729      2.0957472 
    [group=1]:amp2 [group=0]:acr1 [group=1]:acr1 [group=0]:acr2 [group=1]:acr2 
         1.0284143      3.0222788      0.3501766      2.9955681      0.2604565 

