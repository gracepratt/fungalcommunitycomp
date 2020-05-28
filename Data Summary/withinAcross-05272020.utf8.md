---
title: "Within and Across"
author: "Aidee Guzman"
date: "5/27/2020"
output: pdf_document
---








# ALPHA

**Data summary** 

```
##       FarmType variable    mean    SE     min     max
## 1  Monoculture  div_all   3.582 0.035   2.211   4.313
## 2  Polyculture  div_all   3.525 0.036   1.596   4.309
## 3  Monoculture  div_amf   1.046 0.068   0.000   2.263
## 4  Polyculture  div_amf   1.502 0.088   0.000   3.663
## 5  Monoculture  div_par   1.520 0.042   0.343   2.373
## 6  Polyculture  div_par   1.286 0.039   0.358   2.428
## 7  Monoculture div_path   1.872 0.038   0.640   2.597
## 8  Polyculture div_path   1.804 0.030   0.730   2.505
## 9  Monoculture  div_sap   3.134 0.042   1.604   3.928
## 10 Polyculture  div_sap   3.071 0.037   1.005   3.729
## 11 Monoculture  obs_all 169.933 2.813 116.000 259.000
## 12 Polyculture  obs_all 174.659 3.273 104.000 272.000
## 13 Monoculture  obs_amf   5.367 0.415   0.000  20.000
## 14 Polyculture  obs_amf  10.697 0.880   0.000  53.000
## 15 Monoculture  obs_par  12.542 0.305   3.000  20.000
## 16 Polyculture  obs_par  13.348 0.330   5.000  25.000
## 17 Monoculture obs_path  19.875 0.542   9.000  36.000
## 18 Polyculture obs_path  21.424 0.394  12.000  32.000
## 19 Monoculture  obs_sap  95.625 1.859  60.000 161.000
## 20 Polyculture  obs_sap  94.106 1.624  52.000 133.000
```

```
##             FTBL variable    mean    SE     min     max
## 1  Monoculture_F  div_all   3.577 0.051   2.314   4.268
## 2  Monoculture_N  div_all   3.586 0.049   2.211   4.313
## 3  Polyculture_F  div_all   3.572 0.043   2.577   4.241
## 4  Polyculture_N  div_all   3.477 0.057   1.596   4.309
## 5  Monoculture_F  div_amf   1.088 0.098   0.000   2.263
## 6  Monoculture_N  div_amf   1.003 0.093   0.000   2.192
## 7  Polyculture_F  div_amf   1.478 0.131   0.000   3.322
## 8  Polyculture_N  div_amf   1.527 0.119   0.000   3.663
## 9  Monoculture_F  div_par   1.550 0.063   0.343   2.373
## 10 Monoculture_N  div_par   1.489 0.055   0.498   2.209
## 11 Polyculture_F  div_par   1.384 0.049   0.454   2.263
## 12 Polyculture_N  div_par   1.187 0.058   0.358   2.428
## 13 Monoculture_F div_path   1.887 0.057   0.640   2.597
## 14 Monoculture_N div_path   1.856 0.052   0.763   2.534
## 15 Polyculture_F div_path   1.810 0.046   0.730   2.505
## 16 Polyculture_N div_path   1.797 0.040   1.016   2.435
## 17 Monoculture_F  div_sap   3.111 0.060   1.733   3.722
## 18 Monoculture_N  div_sap   3.157 0.058   1.604   3.928
## 19 Polyculture_F  div_sap   3.120 0.046   1.889   3.649
## 20 Polyculture_N  div_sap   3.021 0.057   1.005   3.729
## 21 Monoculture_F  obs_all 172.783 3.916 120.000 259.000
## 22 Monoculture_N  obs_all 167.083 4.039 116.000 257.000
## 23 Polyculture_F  obs_all 178.167 4.725 113.000 272.000
## 24 Polyculture_N  obs_all 171.152 4.525 104.000 260.000
## 25 Monoculture_F  obs_amf   5.900 0.656   0.000  20.000
## 26 Monoculture_N  obs_amf   4.833 0.504   0.000  16.000
## 27 Polyculture_F  obs_amf  10.439 1.251   0.000  36.000
## 28 Polyculture_N  obs_amf  10.955 1.246   0.000  53.000
## 29 Monoculture_F  obs_par  12.867 0.417   5.000  19.000
## 30 Monoculture_N  obs_par  12.217 0.445   3.000  20.000
## 31 Polyculture_F  obs_par  13.773 0.460   5.000  25.000
## 32 Polyculture_N  obs_par  12.924 0.471   6.000  22.000
## 33 Monoculture_F obs_path  19.667 0.749  10.000  32.000
## 34 Monoculture_N obs_path  20.083 0.788   9.000  36.000
## 35 Polyculture_F obs_path  22.136 0.586  12.000  31.000
## 36 Polyculture_N obs_path  20.712 0.517  13.000  32.000
## 37 Monoculture_F  obs_sap  95.883 2.631  60.000 150.000
## 38 Monoculture_N  obs_sap  95.367 2.648  66.000 161.000
## 39 Polyculture_F  obs_sap  96.909 2.194  56.000 133.000
## 40 Polyculture_N  obs_sap  91.303 2.362  52.000 133.000
```

```
##    Block variable    mean    SE     min     max
## 1      F  div_all   3.575 0.033   2.314   4.268
## 2      N  div_all   3.529 0.038   1.596   4.313
## 3      F  div_amf   1.292 0.085   0.000   3.322
## 4      N  div_amf   1.278 0.080   0.000   3.663
## 5      F  div_par   1.463 0.040   0.343   2.373
## 6      N  div_par   1.331 0.042   0.358   2.428
## 7      F div_path   1.847 0.036   0.640   2.597
## 8      N div_path   1.825 0.032   0.763   2.534
## 9      F  div_sap   3.116 0.037   1.733   3.722
## 10     N  div_sap   3.086 0.041   1.005   3.928
## 11     F  obs_all 175.603 3.096 113.000 272.000
## 12     N  obs_all 169.214 3.046 104.000 260.000
## 13     F  obs_amf   8.278 0.751   0.000  36.000
## 14     N  obs_amf   8.040 0.745   0.000  53.000
## 15     F  obs_par  13.341 0.314   5.000  25.000
## 16     N  obs_par  12.587 0.325   3.000  22.000
## 17     F obs_path  20.960 0.481  10.000  32.000
## 18     N obs_path  20.413 0.462   9.000  36.000
## 19     F  obs_sap  96.421 1.694  56.000 150.000
## 20     N  obs_sap  93.238 1.769  52.000 161.000
```

**Model output** 

```
## $obs_all
## $obs_all[[1]]
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: Negative Binomial(72.5569)  ( log )
## Formula: round(obs_all, 0) ~ FarmType * Block + scale(pH) + scale(P) +  
##     scale(NP_ratio) + scale(TOC) + scale(N) + (1 | farmCode)
##    Data: alphaDF
## 
##      AIC      BIC   logLik deviance df.resid 
##   2387.4   2426.3  -1182.7   2365.4      241 
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -3.03093 -0.62900  0.03559  0.63479  3.09682 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  farmCode (Intercept) 0.01785  0.1336  
## Number of obs: 252, groups:  farmCode, 21
## 
## Fixed effects:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       5.138872   0.030501 168.480  < 2e-16 ***
## FarmType1        -0.015599   0.031471  -0.496  0.62013    
## Block1            0.024233   0.009424   2.571  0.01013 *  
## scale(pH)         0.046865   0.022418   2.091  0.03657 *  
## scale(P)         -0.057152   0.018528  -3.085  0.00204 ** 
## scale(NP_ratio)  -0.002160   0.020996  -0.103  0.91808    
## scale(TOC)        0.042225   0.025121   1.681  0.09279 .  
## scale(N)         -0.002351   0.023260  -0.101  0.91949    
## FarmType1:Block1 -0.003198   0.009166  -0.349  0.72719    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) FrmTy1 Block1 scl(H) scl(P) s(NP_) s(TOC) scl(N)
## FarmType1    0.049                                                 
## Block1       0.000  0.044                                          
## scale(pH)   -0.004  0.022  0.083                                   
## scale(P)     0.005  0.060 -0.099 -0.067                            
## scal(NP_rt) -0.007 -0.145 -0.008  0.230  0.305                     
## scale(TOC)   0.005  0.107  0.273 -0.082 -0.128 -0.052              
## scale(N)    -0.007 -0.143 -0.300  0.059 -0.130 -0.086 -0.759       
## FrmTyp1:Bl1  0.000 -0.007 -0.006 -0.025  0.155  0.023 -0.015  0.105
## 
## 
## $obs_amf
## $obs_amf[[1]]
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: Negative Binomial(5.2513)  ( log )
## Formula: round(obs_amf, 0) ~ FarmType * Block + scale(pH) + scale(P) +  
##     scale(NP_ratio) + scale(TOC) + scale(N) + (1 | farmCode)
##    Data: alphaDF
## 
##      AIC      BIC   logLik deviance df.resid 
##   1398.9   1437.7   -688.4   1376.9      241 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -1.8661 -0.7608 -0.1763  0.6275  3.7024 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  farmCode (Intercept) 0.8868   0.9417  
## Number of obs: 252, groups:  farmCode, 21
## 
## Fixed effects:
##                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       1.64476    0.21090   7.799 6.25e-15 ***
## FarmType1        -0.31726    0.21377  -1.484   0.1378    
## Block1            0.03442    0.04174   0.825   0.4096    
## scale(pH)         0.39244    0.09770   4.017 5.90e-05 ***
## scale(P)         -0.43113    0.08795  -4.902 9.49e-07 ***
## scale(NP_ratio)   0.04456    0.10473   0.426   0.6705    
## scale(TOC)        0.20184    0.11049   1.827   0.0677 .  
## scale(N)         -0.16969    0.11036  -1.538   0.1241    
## FarmType1:Block1  0.05337    0.04091   1.305   0.1920    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) FrmTy1 Block1 scl(H) scl(P) s(NP_) s(TOC) scl(N)
## FarmType1    0.057                                                 
## Block1      -0.001  0.023                                          
## scale(pH)   -0.016  0.028  0.033                                   
## scale(P)     0.022  0.022 -0.050 -0.107                            
## scal(NP_rt) -0.010 -0.112  0.004  0.145  0.277                     
## scale(TOC)  -0.003  0.073  0.286  0.047 -0.187 -0.024              
## scale(N)     0.004 -0.088 -0.322 -0.019 -0.098 -0.159 -0.699       
## FrmTyp1:Bl1 -0.006 -0.002  0.103  0.042  0.157  0.023 -0.007  0.118
## 
## 
## $obs_path
## $obs_path[[1]]
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: Negative Binomial(1502470)  ( log )
## Formula: round(obs_path, 0) ~ FarmType * Block + scale(pH) + scale(P) +  
##     scale(NP_ratio) + scale(TOC) + scale(N) + (1 | farmCode)
##    Data: alphaDF
## 
##      AIC      BIC   logLik deviance df.resid 
##   1454.7   1493.6   -716.4   1432.7      241 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.0088 -0.5369  0.0077  0.4129  2.1849 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  farmCode (Intercept) 0.02252  0.1501  
## Number of obs: 252, groups:  farmCode, 21
## 
## Fixed effects:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       3.0110201  0.0358499  83.990   <2e-16 ***
## FarmType1        -0.0512697  0.0374847  -1.368    0.171    
## Block1            0.0142554  0.0146585   0.972    0.331    
## scale(pH)         0.0111221  0.0304319   0.365    0.715    
## scale(P)         -0.0367495  0.0280220  -1.311    0.190    
## scale(NP_ratio)   0.0009822  0.0308410   0.032    0.975    
## scale(TOC)        0.0510629  0.0371905   1.373    0.170    
## scale(N)          0.0080882  0.0338275   0.239    0.811    
## FarmType1:Block1 -0.0195295  0.0142824  -1.367    0.172    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) FrmTy1 Block1 scl(H) scl(P) s(NP_) s(TOC) scl(N)
## FarmType1    0.061                                                 
## Block1      -0.004  0.053                                          
## scale(pH)   -0.009 -0.007  0.073                                   
## scale(P)     0.012  0.060 -0.120 -0.064                            
## scal(NP_rt) -0.012 -0.171 -0.017  0.322  0.340                     
## scale(TOC)   0.003  0.136  0.256 -0.039 -0.181 -0.061              
## scale(N)    -0.011 -0.179 -0.267  0.046 -0.083 -0.076 -0.778       
## FrmTyp1:Bl1  0.007 -0.013  0.035 -0.018  0.127  0.023 -0.022  0.097
## 
## 
## $obs_sap
## $obs_sap[[1]]
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: Negative Binomial(113.248)  ( log )
## Formula: round(obs_sap, 0) ~ FarmType * Block + scale(pH) + scale(P) +  
##     scale(NP_ratio) + scale(TOC) + scale(N) + (1 | farmCode)
##    Data: alphaDF
## 
##      AIC      BIC   logLik deviance df.resid 
##   2088.1   2127.0  -1033.1   2066.1      241 
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -3.11056 -0.57876 -0.03288  0.61243  3.12215 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  farmCode (Intercept) 0.019    0.1378  
## Number of obs: 252, groups:  farmCode, 21
## 
## Fixed effects:
##                    Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       4.5417364  0.0313793 144.737  < 2e-16 ***
## FarmType1         0.0099192  0.0323213   0.307  0.75892    
## Block1            0.0208517  0.0093780   2.223  0.02618 *  
## scale(pH)         0.0201789  0.0218434   0.924  0.35559    
## scale(P)         -0.0509043  0.0186432  -2.730  0.00632 ** 
## scale(NP_ratio)  -0.0341803  0.0212915  -1.605  0.10842    
## scale(TOC)        0.0303327  0.0250946   1.209  0.22677    
## scale(N)          0.0008815  0.0230258   0.038  0.96946    
## FarmType1:Block1 -0.0160516  0.0091347  -1.757  0.07888 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) FrmTy1 Block1 scl(H) scl(P) s(NP_) s(TOC) scl(N)
## FarmType1    0.048                                                 
## Block1      -0.001  0.044                                          
## scale(pH)   -0.003  0.016  0.082                                   
## scale(P)     0.007  0.057 -0.102 -0.079                            
## scal(NP_rt) -0.005 -0.142 -0.011  0.231  0.307                     
## scale(TOC)   0.005  0.106  0.274 -0.083 -0.114 -0.051              
## scale(N)    -0.007 -0.139 -0.299  0.061 -0.131 -0.087 -0.765       
## FrmTyp1:Bl1  0.003 -0.007 -0.013 -0.031  0.161  0.022 -0.011  0.102
## 
## 
## $obs_par
## $obs_par[[1]]
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: Negative Binomial(761905.3)  ( log )
## Formula: round(obs_par, 0) ~ FarmType * Block + scale(pH) + scale(P) +  
##     scale(NP_ratio) + scale(TOC) + scale(N) + (1 | farmCode)
##    Data: alphaDF
## 
##      AIC      BIC   logLik deviance df.resid 
##   1342.1   1381.0   -660.1   1320.1      241 
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.52339 -0.61249 -0.04597  0.58844  2.34192 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  farmCode (Intercept) 0.0114   0.1068  
## Number of obs: 252, groups:  farmCode, 21
## 
## Fixed effects:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)       2.551247   0.029304  87.063   <2e-16 ***
## FarmType1        -0.040815   0.031545  -1.294   0.1957    
## Block1            0.031431   0.018372   1.711   0.0871 .  
## scale(pH)         0.053770   0.031088   1.730   0.0837 .  
## scale(P)         -0.064868   0.031294  -2.073   0.0382 *  
## scale(NP_ratio)   0.003279   0.033616   0.098   0.9223    
## scale(TOC)       -0.036794   0.041702  -0.882   0.3776    
## scale(N)          0.022732   0.039577   0.574   0.5657    
## FarmType1:Block1 -0.011077   0.017949  -0.617   0.5371    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) FrmTy1 Block1 scl(H) scl(P) s(NP_) s(TOC) scl(N)
## FarmType1    0.062                                                 
## Block1      -0.016  0.060                                          
## scale(pH)   -0.021 -0.030  0.071                                   
## scale(P)     0.030  0.035 -0.128 -0.120                            
## scal(NP_rt) -0.006 -0.213 -0.017  0.390  0.381                     
## scale(TOC)   0.016  0.173  0.253  0.079 -0.270 -0.044              
## scale(N)    -0.018 -0.243 -0.261 -0.002  0.019 -0.051 -0.776       
## FrmTyp1:Bl1  0.005 -0.036  0.035 -0.012  0.132  0.046 -0.034  0.098
```


**Plots** 

\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-4-1} \end{center}



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-4-2} \end{center}



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-4-3} \end{center}

\pagebreak  

# Alpha x Environment 

**Tables** 

```
##       FarmType variable   mean    SE   min     max
## 1  Monoculture   FarmBi  1.000 0.000 1.000   1.000
## 2  Polyculture   FarmBi  0.000 0.000 0.000   0.000
## 3  Monoculture        N  0.047 0.002 0.019   0.159
## 4  Polyculture        N  0.039 0.002 0.019   0.093
## 5  Monoculture NP_ratio  0.417 0.054 0.020   3.273
## 6  Polyculture NP_ratio  0.140 0.009 0.041   0.666
## 7  Monoculture        P 29.126 1.951 1.283 108.662
## 8  Polyculture        P 36.985 1.741 6.527  92.802
## 9  Monoculture       pH  7.210 0.098 4.780   8.640
## 10 Polyculture       pH  7.434 0.067 4.560   8.290
## 11 Monoculture      TOC  0.511 0.015 0.280   1.185
## 12 Polyculture      TOC  0.512 0.015 0.217   1.118
```

```
##             FTBL variable   mean    SE   min     max
## 1  Monoculture_F   FarmBi  1.000 0.000 1.000   1.000
## 2  Monoculture_N   FarmBi  1.000 0.000 1.000   1.000
## 3  Polyculture_F   FarmBi  0.000 0.000 0.000   0.000
## 4  Polyculture_N   FarmBi  0.000 0.000 0.000   0.000
## 5  Monoculture_F        N  0.046 0.003 0.019   0.133
## 6  Monoculture_N        N  0.047 0.003 0.022   0.159
## 7  Polyculture_F        N  0.044 0.002 0.019   0.093
## 8  Polyculture_N        N  0.033 0.001 0.019   0.071
## 9  Monoculture_F NP_ratio  0.420 0.079 0.039   3.273
## 10 Monoculture_N NP_ratio  0.414 0.074 0.020   2.929
## 11 Polyculture_F NP_ratio  0.144 0.014 0.045   0.666
## 12 Polyculture_N NP_ratio  0.137 0.010 0.041   0.383
## 13 Monoculture_F        P 27.899 2.485 1.283  64.761
## 14 Monoculture_N        P 30.353 3.020 1.297 108.662
## 15 Polyculture_F        P 40.519 2.518 9.226  79.847
## 16 Polyculture_N        P 33.451 2.344 6.527  92.802
## 17 Monoculture_F       pH  7.174 0.143 4.780   8.460
## 18 Monoculture_N       pH  7.245 0.134 4.800   8.640
## 19 Polyculture_F       pH  7.392 0.105 4.560   8.260
## 20 Polyculture_N       pH  7.475 0.085 5.490   8.290
## 21 Monoculture_F      TOC  0.491 0.021 0.280   1.151
## 22 Monoculture_N      TOC  0.531 0.022 0.329   1.185
## 23 Polyculture_F      TOC  0.526 0.022 0.225   1.118
## 24 Polyculture_N      TOC  0.497 0.019 0.217   0.938
```

```
##    Block variable   mean    SE   min     max
## 1      F   FarmBi  0.476 0.045 0.000   1.000
## 2      N   FarmBi  0.476 0.045 0.000   1.000
## 3      F        N  0.045 0.002 0.019   0.133
## 4      N        N  0.040 0.002 0.019   0.159
## 5      F NP_ratio  0.276 0.040 0.039   3.273
## 6      N NP_ratio  0.269 0.038 0.020   2.929
## 7      F        P 34.510 1.853 1.283  79.847
## 8      N        P 31.976 1.888 1.297 108.662
## 9      F       pH  7.288 0.088 4.560   8.460
## 10     N       pH  7.366 0.078 4.800   8.640
## 11     F      TOC  0.509 0.015 0.225   1.151
## 12     N      TOC  0.513 0.015 0.217   1.185
```

**Model output** 

```
## $pH
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: 
## substitute(log(i + 1) ~ FarmType * Block + (1 | farmCode), list(i = as.name(x)))
##    Data: alphaDF
## 
## REML criterion at convergence: -723.4
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8633 -0.3966  0.0324  0.4570  3.5315 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  farmCode (Intercept) 0.013984 0.11825 
##  Residual             0.002066 0.04546 
## Number of obs: 252, groups:  farmCode, 21
## 
## Fixed effects:
##                    Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)       2.112e+00  2.599e-02  1.900e+01  81.243   <2e-16 ***
## FarmType1        -1.577e-02  2.599e-02  1.900e+01  -0.607   0.5513    
## Block1           -5.559e-03  2.867e-03  2.290e+02  -1.939   0.0537 .  
## FarmType1:Block1  5.983e-04  2.867e-03  2.290e+02   0.209   0.8349    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) FrmTy1 Block1
## FarmType1   0.048               
## Block1      0.000  0.000        
## FrmTyp1:Bl1 0.000  0.000  0.048 
## 
## $P
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: 
## substitute(log(i + 1) ~ FarmType * Block + (1 | farmCode), list(i = as.name(x)))
##    Data: alphaDF
## 
## REML criterion at convergence: 218.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.2777 -0.4870 -0.0088  0.4972  3.5178 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  farmCode (Intercept) 0.5715   0.756   
##  Residual             0.0930   0.305   
## Number of obs: 252, groups:  farmCode, 21
## 
## Fixed effects:
##                   Estimate Std. Error        df t value Pr(>|t|)    
## (Intercept)        3.26525    0.16628  19.00000  19.638 4.44e-14 ***
## FarmType1         -0.20566    0.16628  19.00000  -1.237  0.23120    
## Block1             0.04957    0.01923 229.00000   2.577  0.01058 *  
## FarmType1:Block1  -0.06002    0.01923 229.00000  -3.121  0.00203 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) FrmTy1 Block1
## FarmType1   0.048               
## Block1      0.000  0.000        
## FrmTyp1:Bl1 0.000  0.000  0.048 
## 
## $TOC
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: 
## substitute(log(i + 1) ~ FarmType * Block + (1 | farmCode), list(i = as.name(x)))
##    Data: alphaDF
## 
## REML criterion at convergence: -622
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.1568 -0.4937 -0.0256  0.4830  2.8885 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  farmCode (Intercept) 0.008292 0.09106 
##  Residual             0.003355 0.05792 
## Number of obs: 252, groups:  farmCode, 21
## 
## Fixed effects:
##                    Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)        0.407242   0.020226  18.999999  20.134 2.82e-14 ***
## FarmType1          0.000103   0.020226  18.999999   0.005  0.99599    
## Block1            -0.002389   0.003653 229.000000  -0.654  0.51375    
## FarmType1:Block1  -0.011097   0.003653 229.000000  -3.038  0.00266 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) FrmTy1 Block1
## FarmType1   0.048               
## Block1      0.000  0.000        
## FrmTyp1:Bl1 0.000  0.000  0.048 
## 
## $N
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: 
## substitute(log(i + 1) ~ FarmType * Block + (1 | farmCode), list(i = as.name(x)))
##    Data: alphaDF
## 
## REML criterion at convergence: -1415.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7105 -0.4985 -0.0760  0.3825  4.8384 
## 
## Random effects:
##  Groups   Name        Variance  Std.Dev.
##  farmCode (Intercept) 0.0002236 0.01495 
##  Residual             0.0001416 0.01190 
## Number of obs: 252, groups:  farmCode, 21
## 
## Fixed effects:
##                    Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)       4.165e-02  3.352e-03  1.900e+01  12.425 1.44e-10 ***
## FarmType1         3.961e-03  3.352e-03  1.900e+01   1.182 0.251854    
## Block1            2.193e-03  7.505e-04  2.290e+02   2.922 0.003828 ** 
## FarmType1:Block1 -2.617e-03  7.505e-04  2.290e+02  -3.487 0.000586 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) FrmTy1 Block1
## FarmType1   0.048               
## Block1      0.000  0.000        
## FrmTyp1:Bl1 0.000  0.000  0.048 
## 
## $NP_ratio
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: 
## substitute(log(i + 1) ~ FarmType * Block + (1 | farmCode), list(i = as.name(x)))
##    Data: alphaDF
## 
## REML criterion at convergence: -453.6
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6063 -0.3858 -0.0514  0.2507  5.2817 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  farmCode (Intercept) 0.044918 0.21194 
##  Residual             0.006094 0.07807 
## Number of obs: 252, groups:  farmCode, 21
## 
## Fixed effects:
##                    Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)       2.105e-01  4.656e-02  1.900e+01   4.522 0.000233 ***
## FarmType1         8.263e-02  4.656e-02  1.900e+01   1.775 0.091977 .  
## Block1            1.069e-03  4.923e-03  2.290e+02   0.217 0.828267    
## FarmType1:Block1 -8.836e-04  4.923e-03  2.290e+02  -0.179 0.857718    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) FrmTy1 Block1
## FarmType1   0.048               
## Block1      0.000  0.000        
## FrmTyp1:Bl1 0.000  0.000  0.048 
## 
## $FarmBi
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: 
## substitute(log(i + 1) ~ FarmType * Block + (1 | farmCode), list(i = as.name(x)))
##    Data: alphaDF
## 
## REML criterion at convergence: -16710.4
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -1.438 -1.438  0.000  0.000  0.000 
## 
## Random effects:
##  Groups   Name        Variance  Std.Dev. 
##  farmCode (Intercept) 0.000e+00 0.000e+00
##  Residual             2.922e-31 5.406e-16
## Number of obs: 252, groups:  farmCode, 21
## 
## Fixed effects:
##                    Estimate Std. Error         df    t value Pr(>|t|)
## (Intercept)       3.466e-01  3.409e-17  1.436e-17  1.017e+16        1
## FarmType1         3.466e-01  3.409e-17  1.436e-17  1.017e+16        1
## Block1           -2.776e-18  3.409e-17  1.436e-17 -8.100e-02        1
## FarmType1:Block1 -2.776e-18  3.409e-17  1.436e-17 -8.100e-02        1
## 
## Correlation of Fixed Effects:
##             (Intr) FrmTy1 Block1
## FarmType1   0.048               
## Block1      0.000  0.000        
## FrmTyp1:Bl1 0.000  0.000  0.048 
## convergence code: 0
## boundary (singular) fit: see ?isSingular
```


**Levene's Test** 

```
##          F value Pr(>F)
## pH         8.508  0.004
## NP_ratio  26.544  0.000
## P          0.444  0.506
## TOC        3.984  0.047
## N          0.579  0.448
```


**Plots** 

```
## $obs_all
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-1} \end{center}

```
## 
## $obs_amf
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-2} \end{center}

```
## 
## $obs_path
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-3} \end{center}

```
## 
## $obs_sap
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-4} \end{center}

```
## 
## $obs_par
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-5} \end{center}

```
## 
## $div_all
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-6} \end{center}

```
## 
## $div_amf
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-7} \end{center}

```
## 
## $div_path
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-8} \end{center}

```
## 
## $div_sap
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-9} \end{center}

```
## 
## $div_par
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-10} \end{center}

```
## $obs_all
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-11} \end{center}

```
## 
## $obs_amf
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-12} \end{center}

```
## 
## $obs_path
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-13} \end{center}

```
## 
## $obs_sap
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-14} \end{center}

```
## 
## $obs_par
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-15} \end{center}

```
## 
## $div_all
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-16} \end{center}

```
## 
## $div_amf
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-17} \end{center}

```
## 
## $div_path
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-18} \end{center}

```
## 
## $div_sap
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-19} \end{center}

```
## 
## $div_par
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-20} \end{center}

```
## $obs_all
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-21} \end{center}

```
## 
## $obs_amf
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-22} \end{center}

```
## 
## $obs_path
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-23} \end{center}

```
## 
## $obs_sap
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-24} \end{center}

```
## 
## $obs_par
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-25} \end{center}

```
## 
## $div_all
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-26} \end{center}

```
## 
## $div_amf
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-27} \end{center}

```
## 
## $div_path
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-28} \end{center}

```
## 
## $div_sap
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-29} \end{center}

```
## 
## $div_par
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-30} \end{center}

```
## $obs_all
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-31} \end{center}

```
## 
## $obs_amf
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-32} \end{center}

```
## 
## $obs_path
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-33} \end{center}

```
## 
## $obs_sap
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-34} \end{center}

```
## 
## $obs_par
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-35} \end{center}

```
## 
## $div_all
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-36} \end{center}

```
## 
## $div_amf
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-37} \end{center}

```
## 
## $div_path
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-38} \end{center}

```
## 
## $div_sap
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-39} \end{center}

```
## 
## $div_par
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-40} \end{center}

```
## $obs_all
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-41} \end{center}

```
## 
## $obs_amf
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-42} \end{center}

```
## 
## $obs_path
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-43} \end{center}

```
## 
## $obs_sap
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-44} \end{center}

```
## 
## $obs_par
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-45} \end{center}

```
## 
## $div_all
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-46} \end{center}

```
## 
## $div_amf
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-47} \end{center}

```
## 
## $div_path
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-48} \end{center}

```
## 
## $div_sap
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-49} \end{center}

```
## 
## $div_par
```



\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-8-50} \end{center}

\pagebreak  

# ALL
## ACROSS 

**Tables** 

```
##                   Predictors Coefficients
## 1                 Geographic        1.434
## 2                         pH        1.941
## 3                          P        1.158
## 4                          N        0.557
## 5 Percent Deviance Explained       28.102
## 6                        DIC      515.686
```

## WITHIN 

**Tables** 

```
##                   Predictors Coefficients
## 1                         pH        2.459
## 2                          P        3.990
## 3                        TOC        0.712
## 4                          N        0.185
## 5 Percent Deviance Explained       19.357
## 6                        DIC       90.271
```


**Plots** 

\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-11-1} \end{center}

\pagebreak  

# MONO
## ACROSS 

**Tables** 

```
##                   Predictors Coefficients
## 1                 Geographic        1.102
## 2                         pH        1.859
## 3                          P        1.657
## 4                        TOC        1.889
## 5                          N        1.045
## 6 Percent Deviance Explained       35.436
## 7                        DIC      141.934
```

## WITHIN 

**Tables** 

```
##                   Predictors Coefficients
## 1                         pH        3.506
## 2                          P        3.415
## 3                        TOC        0.852
## 4                          N        3.043
## 5 Percent Deviance Explained       27.130
## 6                        DIC       42.379
```


**Plots** 

\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-14-1} \end{center}

\pagebreak  


# POLY
## ACROSS 

**Tables** 

```
##                   Predictors Coefficients
## 1                 Geographic        2.322
## 2                         pH        1.363
## 3                          P        1.068
## 4                          N        0.259
## 5 Percent Deviance Explained       33.908
## 6                        DIC      130.348
```

## WITHIN 

**Tables** 

```
##                   Predictors Coefficients
## 1                         pH        1.975
## 2                          P        2.538
## 3                        TOC        0.629
## 4 Percent Deviance Explained       29.974
## 5                        DIC       37.558
```


**Plots** 

\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-17-1} \end{center}

\pagebreak  

# ALL PLOTS
**Plots** 

\begin{center}\includegraphics{withinAcross-05272020_files/figure-latex/unnamed-chunk-18-1} \end{center}

\pagebreak  



