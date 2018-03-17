

# Risk-adjusted portfolio calculation with Wikifolios


This script is used to facilitate the risk-adjusted calculation of portfolios containing wikifolio certificates. 


#### Background

Wikifolio certificates are exchange-traded certificates giving the possibility to participate at virtual portfolios called wikifolios (https://www.wikifolio.com/). 

The basis for a wikifolio certificate is an index which represents the performance of a social trading portfolio - the wikifolio - managed by the wikifolio trader on a 1:1 basis. By buying a wikifolio certificate, one gets the chance to participate in the development of the underlying wikifolio. In other words: If the value of the wikifolio increases by 10 percent, the investor can participate in this development by buying the corresponding wikifolio certificate. Each wikifolio can serve as a notional reference portfolio to which a corresponding wikifolio index refers.

#### Code Description


01_getData.R

Downloads historical data from wikifolio.com using a manually created csv-file with wikifolio symbols. Prior to downloading, the comprehensive wikifolio list can be filtered using several quality metrics, including money management, trading frequency, and liquidity of wikifolio assets.  

```r
wikiList <- read.csv("./tables/wikiList.csv", sep=",")
head(wikiList[,c(1:3)])
```

```
##                                Name         ISIN     Symbol
## 1                           #Future DE000LS9JHV7 WFFUTURE13
## 2 1Best of Bonus solide+Kurzlaeufer DE000LS9JED2 WFBONUSKUR
## 3              2Stein's Best Platin DE000LS9HX17 WFPLAT1000
## 4     AA+ Master-Trading ohne Hebel DE000LS9JY97 WF0AAABEST
## 5                            Abacus DE000LS9HZ72 WF00ABACUS
## 6                Aktien-Werte First DE000LS9HNU1 WF00ADS304
```

02_portfolios.R

Calculates optimal portfolio compositions based on efficient frontier as described in the modern portfolio theory and maximum drawdown limits. Calculations are performed using wikifolio historical data. Amongst others portfolio performance charts are plotted to evaluate risk/return behaviour of different strategies.


03_select.R

Calculates final portfolio weights based on selected risk tolerance. Output is a file containing the portfolio composition, e.g.


```r
head(portfolio_weights_final, 4)
```

```
##            ISIN High.Return High.Return1 MaxDD.5% MaxDD.5%1 Median Median1
## 10 DE000LS9LET4        0.10         0.14     0.21      0.48   0.16    0.31
## 5  DE000LS9JHV7        0.02         0.04     0.30      0.23   0.16    0.14
## 2  DE000LS9HNU1        0.11         0.00     0.30      0.00   0.20    0.00
## 3  DE000LS9HZ72        0.09         0.11     0.17      0.00   0.13    0.06
##    Current               Name Target
## 10       0        BullenArena  26000
## 5        0            #Future  19000
## 2    12000 Aktien-Werte First  17000
## 3     2000             Abacus  13000
```


#### Reference

[1] Function for computing the efficient frontier adapted from: Matuszak, Andrew. Chapter 5 – Building an Optimized Portfolio with R - Economist at Large. http://economistatlarge.com/portfolio-theory/r-optimized-portfolio. 




#### Further Reading

[1] Wikifolio Certificates | Wikifolio.com, https://www.wikifolio.com/en/int/help/faqs/financial-investment/wikifolio-certificates.

[2] Oehler, Andreas, et al. Benefits from Social Trading? Empirical Evidence for Certificates on Wikifolios. Social Science Research Network, July 2017. https://papers.ssrn.com/abstract=2807385.

[3] Thiel, Christian. “Wikifolio – Trading im 21. Jahrhundert.” Grossmutters Sparstrumpf, Aug. 2016, http://grossmutters-sparstrumpf.de/wikifolio-trading-im-21-jahrhundert/.

[4] Koch, Thomas. “Wikifolio-Papiere Reduzieren Das Ausfallrisiko.” DIE WELT, Apr. 2017. https://www.welt.de/print/die_welt/finanzen/article163727970/Wikifolio-Papiere-reduzieren-das-Ausfallrisiko.html.

[5] Turnover in Wikifolio Certificates at New High Says Börse Stuttgart. FTSE Global Markets, http://www.ftseglobalmarkets.com/news/turnover-in-wikifolio-certificates-at-new-high-says-borse-stuttgart.html. Jan 2018.

[6] Matuszak, Andrew. Chapter 5 – Building an Optimized Portfolio with R - Economist at Large. http://economistatlarge.com/portfolio-theory/r-optimized-portfolio.

[7] Zimmermann, David. “A Gentle Introduction to Finance Using R: Efficient Frontier and CAPM – Part 1.” R-Bloggers, May 2016, https://www.r-bloggers.com/a-gentle-introduction-to-finance-using-r-efficient-frontier-and-capm-part-1/.

[8] Dupuis, Francis. Portfolio Optimization With R – Programming For Finance. Jan. 2018, http://programmingforfinance.com/2017/10/portfolio-optimization-with-r/.



