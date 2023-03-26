# multi-component comparison works and print functions work

    
     Conditional Model 
    
     Raw formula: 
    Y ~ group + group:main_rrr1 + group:main_sss1 + group:main_rrr2 +      group:main_sss2 
    
     Raw Coefficients: 
         (Intercept)           group1 group0:main_rrr1 group1:main_rrr1 
           0.9911137       -0.4795481       -1.9309710        0.9272435 
    group0:main_sss1 group1:main_sss1 group0:main_rrr2 group1:main_rrr2 
          -0.2314863       -0.3386861       -2.0734439        0.9937582 
    group0:main_sss2 group1:main_sss2 
          -0.3049397       -0.2648372 
    
     Transformed Coefficients: 
       (Intercept)      [group=1] [group=0]:amp1 [group=1]:amp1 [group=0]:amp2 
         0.9911137     -0.4795481      1.9447969      0.9871619      2.0957476 
    [group=1]:amp2 [group=0]:acr1 [group=1]:acr1 [group=0]:acr2 [group=1]:acr2 
         1.0284425      3.0222812      0.3502054      2.9955703      0.2604474 

