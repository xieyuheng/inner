---
title: Financial market
---

# financial market

## links

- https://www.youtube.com/watch?v=6OoMQiClXCs
  An introduction to financial markets
  -- MoneyWeek Investment Tutorials

- https://www.investopedia.com/walkthrough/corporate-finance/1/financial-markets.aspx

## intro

- investors <-[return]- financial market <-[cost]- borrowers

- borrowers can be governments and companies

- financial market can be investment banks

## money market -- short term

## capital market -- long term

- equity
  - shares
    bring in new owners
    and give out dividends

- debts
  - bonds
    tradable debts

# stock valuation

## >< // 集合競價原則

- 委買隊列 : (* <price>, <number>) <ordered-list>
- 委賣隊列 : (* <price>, <number>) <ordered-list>
- 開盤價 : <price>
- 成交價 : (* <price>, <number>)

# terms

## <alpha>

- [define-1]
  an alpha is a mathematical, predictive model
  of the performance of financial instruments.

- [question]
  what are the presumptions of the concept of alpha?

- [question]
  by alpha, we are not predicting the exact <price> of a instrument,
  so, what are predicting?

- [question]
  what is the type of alpha?
  by [define-1], an alpha is a function of type :
  (-> <instrument-data> -- <prediction>)
  - in the most simple case <prediction> is simply a real number

  - there is a sense of time,
    <instrument-data> is accumulating
    it might include the <prediction>
    of the previous calculation [or be calculated again]
    - x -
      the type above does not express the sense of time.

## <statistic>

### pnl -- profit-and-loss

```python
daily_pnl(instrument_list) = sum for instrument in instrument_list: (position * daily_return(instrument))
daily_return(instrument) = (today_s_close(instrument) / yesterday_s_close(instrument)) - 1
# yesterday_s_close == today_s_open?
#   what can a stock exchange company do to the stocks
#   during yesterday_s_close and today_s_open?
```

### active return

- return relative to a benchmark.
  if a portfolio's return is 5%,
  and the benchmark's return is 3%,
  then the portfolio's active return is 2%.

- the segment of the returns in an investment portfolio
  that is due to active management decisions made by the portfolio manager.
  It does not include any return that is merely a function of the market's movement.

- the active return is calculated as
  the return of the portfolio minus some benchmark return.

### active risk -- tracking error

- a measure of the risk in an investment portfolio
  that is due to active management decisions made by the portfolio manager;
  it indicates how closely a portfolio follows the index
  to which it is benchmarked.

- The best measure is the standard deviation
  of the difference between the portfolio and index returns.

### IR -- information ratio -- appraisal ratio

- prediction ability of a model

- mean(daily_pnl) is the *expected value* of daily_pnl *over time*
  - i.e. the long-run average value
    of repetitions of the experiment it represents.

  - the expected value is also known as
    - the expectation,
    - mathematical expectation,
    - EV,
    - average,
    - mean value,
    - mean,
    - first moment.

```python
information_ratio = mean(daily_pnl) / standard_deviation(daily_pnl)
```

- Sharpe ratio is the annual IR: IR * sqrt(252)
  - the 252 represents the estimated number of trading days in a year

### turnover

- value traded / value held

### drawdown

- percentage of the largest loss

### volume -- measuring stock activity

- the number of shares or contracts
  traded in a security or an entire market
  during a given period of time.

- For example:
  - Trader 1 Buys 100 shares of stock
  - Trader 2 Buy 500 shares of stock
  - Trader 3 Sells 1000 shares of stock
  Total volume is then 1,600 shares for this sequence.
  volume increases regardless if it is a buy or sell order.

## >< to generate trading strategy from alpha

## >< <market-data> & <instrument-data>
