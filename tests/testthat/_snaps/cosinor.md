# model output is class cosinor.glmm

    
     Conditional Model 
    
     Raw formula: 
    Y ~ X + X:main_rrr1 + X:main_sss1 
    
     Raw Coefficients: 
     (Intercept)           X1 X0:main_rrr1 X1:main_rrr1 X0:main_sss1 X1:main_sss1 
           29.67         1.92         0.75         6.49         6.10         4.79 
    
     Transformed Coefficients: 
    (Intercept)       [X=1]   [X=0]:amp   [X=1]:amp   [X=0]:acr   [X=1]:acr 
          29.67        1.92        6.15        8.07       -1.45       -0.64 
    
    ***********************
    
     Dispersion Model 
    
     Raw  Formula: 
    ~X:disp_rrr1 + X:disp_sss1 - 1 
    
     Raw  Coefficients: 
    X0:disp_rrr1 X1:disp_rrr1 X0:disp_sss1 X1:disp_sss1 
            0.51        -0.11         0.00        -0.06 
    
     Transformed  Coefficients: 
    [X=0]:amp [X=1]:amp [X=0]:acr [X=1]:acr 
         0.51      0.12     -0.01      2.62 
    
    ***********************
    
     Zero-Inflation Model 
    
     Raw  Formula: 
    ~X:zi_rrr1 + X:zi_sss1 - 1 
    
     Raw  Coefficients: 
    X0:zi_rrr1 X1:zi_rrr1 X0:zi_sss1 X1:zi_sss1 
         -0.21       0.05       0.14       0.24 
    
     Transformed  Coefficients: 
    [X=0]:amp [X=1]:amp [X=0]:acr [X=1]:acr 
         0.25      0.25     -2.56     -1.35 

---

    
     Conditional Model 
    
     Raw formula: 
    Y ~ group + group:main_rrr1 + group:main_sss1 + group:main_rrr2 +      group:main_sss2 + (main_rrr1 + main_sss1 | 1) + (0 + main_rrr2 +      main_sss2 | group) 
    
     Raw Coefficients: 
         (Intercept)           group1 group0:main_rrr1 group1:main_rrr1 
                5.06            -3.06            -0.01             0.03 
    group0:main_sss1 group1:main_sss1 group0:main_rrr2 group1:main_rrr2 
               -1.04            -0.93             1.17             1.13 
    group0:main_sss2 group1:main_sss2 
               -1.77            -1.69 
    
     Transformed Coefficients: 
       (Intercept)      [group=1] [group=0]:amp1 [group=1]:amp1 [group=0]:amp2 
              5.06          -3.06           1.04           0.93           2.12 
    [group=1]:amp2 [group=0]:acr1 [group=1]:acr1 [group=0]:acr2 [group=1]:acr2 
              2.03           1.58           1.54           0.98           0.98 

