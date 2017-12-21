import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# create #

pd.Series([1,3,5,np.nan,6,8])

dates = pd.date_range('2013-01-01', periods=6)

df = pd.DataFrame(np.random.randn(6, 4),
                  index=dates,
                  columns=list('ABCD'))

# view #

df.index
df.columns
df.values
df.T

df.describe() # fancy

df.sort_index(axis=0)
df.sort_index(axis=1)

df.sort_index(axis='index')
df.sort_index(axis='columns')

df.sort_index(axis=0, ascending=True)
df.sort_index(axis=1, ascending=True)

df.sort_index(axis=0, ascending=False)
df.sort_index(axis=1, ascending=False)

df.sort_index(axis=0, ascending=False) \
  .sort_index(axis=1, ascending=False)

df.sort_values(by='B') # by a column
df.T.sort_values(by='2013-01-01', ascending=False).T

# selecte #

### simple get syntax

df['B'] # by a column first
df['B']['2013-01-01'] # then a index

df[0:3] # a range of index number, this gets three rows
df['2013-01-01':'2013-01-03'] # include '2013-01-03'

### selecte by label

# - so called 'cross section'
#   maybe each column is viewed as 'natural section'

df.loc['2013-01-01']
df.loc['2013-01-01', ['A', 'B']]

assert df['B']['2013-01-01'] == df.loc['2013-01-01']['B']

# byt we also have :
df['2013-01-01':'2013-01-03'] == df.loc['2013-01-01':'2013-01-03']
# there is no consistency at all
# we must only use a consistent subset of the language

### section by a range and labels
df.loc[:, ['A', 'B']]
df.loc['2013-01-01':'2013-01-03', ['A', 'B']]
df.loc['2013-01-01', ['A', 'B']]

# also

df['B']['2013-01-01']
df.loc['2013-01-01']['B']
df.loc['2013-01-01', 'B']
# df.at['2013-01-01', 'B'] # fail
df.at[dates[0], 'B']

### boolean indexing

# operation #

### stats operation

df.mean()
df.mean(axis=0)
df.mean(axis=1)

s = pd.Series([1,3,5,np.nan,6,8], index=dates).shift(2)

df.sub(s, axis='index')
(df.T - s).T
df.max() - df.min()
np.cumsum(df)
df.apply(np.cumsum)


dates = pd.date_range('2000-01-01', periods=10)

s = pd.Series(np.random.randn(10), index=dates)
s
s.cumsum()
s.cumsum().cumsum()

s.cumsum()

r = s.rolling(window=3)
r.mean()
r.max()
