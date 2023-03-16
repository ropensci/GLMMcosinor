# model output is class cosinor.glmm

    
     Conditional Model 
    
     Raw formula: 
    Y ~ X + X:main_rrr1 + X:main_sss1 
    
     Raw Coefficients: 
     (Intercept)           X1 X0:main_rrr1 X1:main_rrr1 X0:main_sss1 X1:main_sss1 
      29.6732064    1.9174922    0.7543715    6.4934437    6.1046411    4.7931343 
    
     Transformed Coefficients: 
    (Intercept)       [X=1]   [X=0]:amp   [X=1]:amp   [X=0]:acr   [X=1]:acr 
     29.6732064   1.9174922   6.1510746   8.0708703  -1.4478462  -0.6358738 
    
    ***********************
    
     Dispersion Model 
    
     Raw  Formula: 
    ~X:disp_rrr1 + X:disp_sss1 - 1 
    
     Raw  Coefficients: 
    X0:disp_rrr1 X1:disp_rrr1 X0:disp_sss1 X1:disp_sss1 
     0.506103139 -0.106644819  0.003898437 -0.060841337 
    
     Transformed  Coefficients: 
       [X=0]:amp    [X=1]:amp    [X=0]:acr    [X=1]:acr 
     0.506118153  0.122779419 -0.007702698  2.623143509 
    
    ***********************
    
     Zero-Inflation Model 
    
     Raw  Formula: 
    ~X:zi_rrr1 + X:zi_sss1 - 1 
    
     Raw  Coefficients: 
    X0:zi_rrr1 X1:zi_rrr1 X0:zi_sss1 X1:zi_sss1 
    -0.2066606  0.0540041  0.1367328  0.2403406 
    
     Transformed  Coefficients: 
     [X=0]:amp  [X=1]:amp  [X=0]:acr  [X=1]:acr 
     0.2477992  0.2463332 -2.5570851 -1.3497692 

