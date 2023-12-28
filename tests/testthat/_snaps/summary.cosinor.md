# summary print works

    
     Conditional Model 
    Raw model coefficients:
                 estimate standard.error lower.CI upper.CI p.value    
    (Intercept)     30.33           0.38    29.58     31.1  <2e-16 ***
    X0:main_rrr1     0.87           0.63    -0.38      2.1     0.2    
    X1:main_rrr1     6.48           0.94     4.63      8.3   7e-12 ***
    X0:main_sss1     6.24           0.68     4.91      7.6  <2e-16 ***
    X1:main_sss1     4.67           0.91     2.89      6.4   3e-07 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
    Transformed coefficients:
                estimate standard.error lower.CI upper.CI p.value    
    (Intercept)    30.33           0.38    29.58     31.1  <2e-16 ***
    [X=0]:amp1      6.30           0.68     4.97      7.6  <2e-16 ***
    [X=1]:amp1      7.98           0.91     6.20      9.8  <2e-16 ***
    [X=0]:acr1      1.43           0.10     1.24      1.6  <2e-16 ***
    [X=1]:acr1      0.62           0.12     0.39      0.9   1e-07 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

---

    
     Conditional Model 
    Raw model coefficients:
                 estimate standard.error lower.CI upper.CI p.value    
    (Intercept)     30.33           0.38    29.59     31.1  <2e-16 ***
    X0:main_rrr1     0.95           0.62    -0.27      2.2     0.1    
    X1:main_rrr1     6.60           0.98     4.67      8.5   2e-11 ***
    X0:main_sss1     6.28           0.70     4.91      7.7  <2e-16 ***
    X1:main_sss1     4.58           0.93     2.75      6.4   1e-06 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
    Transformed coefficients:
                estimate standard.error lower.CI upper.CI p.value    
    (Intercept)   30.328          0.375   29.593     31.1  <2e-16 ***
    [X=0]:amp1     6.351          0.697    4.985      7.7  <2e-16 ***
    [X=1]:amp1     8.030          0.926    6.214      9.8  <2e-16 ***
    [X=0]:acr1     1.420          0.098    1.227      1.6  <2e-16 ***
    [X=1]:acr1     0.607          0.123    0.365      0.8   8e-07 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
    ***********************
    
     Dispersion Model 
    Raw model coefficients:
                 estimate standard.error lower.CI upper.CI p.value    
    (Intercept)     3.313          0.100    3.116      3.5  <2e-16 ***
    X0:disp_rrr1    0.377          0.180    0.024      0.7    0.04 *  
    X1:disp_rrr1   -0.082          0.245   -0.562      0.4    0.74    
    X0:disp_sss1    0.104          0.197   -0.283      0.5    0.60    
    X1:disp_sss1    0.056          0.234   -0.402      0.5    0.81    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
    Transformed coefficients:
                estimate standard.error lower.CI upper.CI p.value    
    (Intercept)    3.313          0.100    3.116      3.5  <2e-16 ***
    [X=0]:amp1     0.392          0.173    0.053      0.7    0.02 *  
    [X=1]:amp1     0.100          0.243   -0.376      0.6    0.68    
    [X=0]:acr1     0.269          0.521   -0.753      1.3    0.61    
    [X=1]:acr1     2.539          2.379   -2.125      7.2    0.29    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
    ***********************
    
     Zero-Inflation Model 
    Raw model coefficients:
                estimate standard.error lower.CI upper.CI p.value
    (Intercept) -2.4e+01        1.3e+04 -2.6e+04    26409       1
    X0:zi_rrr1  -3.4e+00        1.5e+04 -2.9e+04    28995       1
    X1:zi_rrr1   1.5e-01        2.7e+04 -5.3e+04    52825       1
    X0:zi_sss1   1.6e+00        1.1e+04 -2.2e+04    21801       1
    X1:zi_sss1   4.4e-01        2.6e+04 -5.1e+04    50935       1
    
    Transformed coefficients:
                estimate standard.error lower.CI upper.CI p.value
    (Intercept) -2.4e+01        1.3e+04 -2.6e+04    26409       1
    [X=0]:amp1   3.7e+00        1.6e+04 -3.1e+04    30621       1
    [X=1]:amp1   4.7e-01        2.6e+04 -5.0e+04    50158       1
    [X=0]:acr1   2.7e+00        2.7e+03 -5.2e+03     5228       1
    [X=1]:acr1   1.2e+00        5.8e+04 -1.1e+05   114240       1

