# model output is class cosinor.glmm

    
     Conditional Model 
    
     Raw formula: 
    Y ~ X + X:main_rrr1 + X:main_sss1 
    
     Raw Coefficients: 
                 Estimate
    (Intercept)      29.7
    X1                1.9
    X0:main_rrr1      0.8
    X1:main_rrr1      6.5
    X0:main_sss1      6.1
    X1:main_sss1      4.8
    
     Transformed Coefficients: 
                Estimate
    (Intercept)     29.7
    [X=1]            1.9
    [X=0]:amp        6.2
    [X=1]:amp        8.1
    [X=0]:acr        1.4
    [X=1]:acr        0.6
    
    ***********************
    
     Dispersion Model 
    
     Raw  Formula: 
    ~X:disp_rrr1 + X:disp_sss1 - 1 
    
     Raw  Coefficients: 
                 Estimate
    X0:disp_rrr1      0.5
    X1:disp_rrr1     -0.1
    X0:disp_sss1      0.0
    X1:disp_sss1     -0.1
    
     Transformed  Coefficients: 
              Estimate
    [X=0]:amp      0.5
    [X=1]:amp      0.1
    [X=0]:acr      0.0
    [X=1]:acr     -2.6
    
    ***********************
    
     Zero-Inflation Model 
    
     Raw  Formula: 
    ~X:zi_rrr1 + X:zi_sss1 - 1 
    
     Raw  Coefficients: 
               Estimate
    X0:zi_rrr1     -0.2
    X1:zi_rrr1      0.1
    X0:zi_sss1      0.1
    X1:zi_sss1      0.2
    
     Transformed  Coefficients: 
              Estimate
    [X=0]:amp      0.2
    [X=1]:amp      0.2
    [X=0]:acr      2.6
    [X=1]:acr      1.3

---

    
     Conditional Model 
    
     Raw formula: 
    Y ~ group + group:main_rrr1 + group:main_sss1 + group:main_rrr2 +      group:main_sss2 + (0 + main_rrr2 + main_sss2 | group) 
    
     Raw Coefficients: 
                     Estimate
    (Intercept)           5.0
    group1               -3.0
    group0:main_rrr1      0.1
    group1:main_rrr1      0.0
    group0:main_sss1      0.9
    group1:main_sss1      1.0
    group0:main_rrr2      1.1
    group1:main_rrr2      1.1
    group0:main_sss2      1.7
    group1:main_sss2      1.7
    
     Transformed Coefficients: 
                   Estimate
    (Intercept)         5.0
    [group=1]          -3.0
    [group=0]:amp1      0.9
    [group=1]:amp1      1.0
    [group=0]:amp2      2.0
    [group=1]:amp2      2.0
    [group=0]:acr1      1.5
    [group=1]:acr1      1.6
    [group=0]:acr2      1.0
    [group=1]:acr2      1.0

