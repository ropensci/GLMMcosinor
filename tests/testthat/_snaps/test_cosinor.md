# multi-component comparison works, print functions work

    
     Conditional Model 
    
     Raw formula: 
    Y ~ group + group:main_rrr1 + group:main_sss1 + group:main_rrr2 +      group:main_sss2 
    
     Raw Coefficients: 
         (Intercept)           group1 group0:main_rrr1 group1:main_rrr1 
                0.99            -0.48            -1.93             0.93 
    group0:main_sss1 group1:main_sss1 group0:main_rrr2 group1:main_rrr2 
               -0.23            -0.34            -2.07             0.99 
    group0:main_sss2 group1:main_sss2 
               -0.30            -0.26 
    
     Transformed Coefficients: 
       (Intercept)      [group=1] [group=0]:amp1 [group=1]:amp1 [group=0]:amp2 
              0.99          -0.48           1.94           0.99           2.10 
    [group=1]:amp2 [group=0]:acr1 [group=1]:acr1 [group=0]:acr2 [group=1]:acr2 
              1.03           3.02           0.35           3.00           0.26 

