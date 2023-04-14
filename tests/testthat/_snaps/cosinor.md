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
    Y ~ X + X:main_rrr1 + X:main_sss1 + X:main_rrr2 + X:main_sss2 +      X:main_rrr3 + X:main_sss3 + (1 | main_rrr1 + main_sss1) +      (X | main_rrr2 + main_sss2) 
    
     Raw Coefficients: 
     (Intercept)           X1 X0:main_rrr1 X1:main_rrr1 X0:main_sss1 X1:main_sss1 
           29.76         5.01         0.79        25.80         2.18         4.21 
    X0:main_rrr2 X1:main_rrr2 X0:main_sss2 X1:main_sss2 X0:main_rrr3 X1:main_rrr3 
            8.36        -0.48        -3.30        24.30        -8.63       -20.51 
    X0:main_sss3 X1:main_sss3 
            8.28       -34.38 
    
     Transformed Coefficients: 
    (Intercept)       [X=1]  [X=0]:amp1  [X=1]:amp1  [X=0]:amp2  [X=1]:amp2 
          29.76        5.01        2.32       26.14        8.99       24.30 
     [X=0]:amp3  [X=1]:amp3  [X=0]:acr1  [X=1]:acr1  [X=0]:acr2  [X=1]:acr2 
          11.96       40.03       -1.23       -0.16        0.38       -1.59 
     [X=0]:acr3  [X=1]:acr3 
          -2.38        2.11 

