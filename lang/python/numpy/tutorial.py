import numpy as np


# example #

a = np.arange(15).reshape(3, 5)
type(a)

a.ndim
a.shape
a.size
a.dtype
a.itemsize
a.data

b = np.array([6, 7, 8])
type(b)


# create #

a = np.array([2, 3, 4])
assert a.dtype == np.dtype('int64')

b = np.array([1.2, 3.5, 5.1])
assert b.dtype == np.dtype('float64')

np.array( [ (1.5, 2, 3), (4, 5, 6) ] )
np.array( [ [1, 2], [3, 4] ], dtype=complex )

np.zeros( (3, 4) )
np.ones( (2, 3, 4), dtype=np.int16 )
np.empty( (2, 3) )

np.arange( 10 )
np.arange( 10, 30 )
np.arange( 10, 30, 5 )
np.arange( 0, 2, 0.3 )

np.linspace( 0, 2, 9 ) # 9 numbers from 0 to 2
x = np.linspace( 0, (2 * np.pi), 100 )
f = np.sin(x)


# printing #

print(np.arange(6))
print(np.arange(12).reshape(4, 3))
print(np.arange(24).reshape(2, 3, 4))
print(np.arange(10000))
print(np.arange(10000).reshape(100, 100))

# to force numpy to print the entire array
# np.set_printoptions(threshold='nan')


# basic operations #

# arithmetic operators on arrays apply elementwise

a = np.array( [20, 30, 40, 50] )
b = np.arange( 4 )
c = a - b
b ** 2
b * b
10 * np.sin(a)
a < 35

# dot for matrix product

A = np.array( [[1, 1],
               [0, 1]] )

B = np.array( [[2, 0],
               [3, 4]] )
A.dot(B)
np.dot(A, B)

# upcasting result to the more general or precise one
a = np.ones(3, dtype=np.int32)
b = np.linspace(0, np.pi, 3)
b.dtype
c = a + b
c.dtype
d = np.exp(c * 1j)
d.dtype

# many unary operations
# are implemented as methods of the ndarray class
# - this is where postfix notation make sense

a = np.random.random((2, 3))

a.sum()
a.min()
a.max()

# these operations do not respect shape
# to respect shape, explict specify the axis parameter

b = np.arange(3 * 4 * 5).reshape(3, 4, 5)

b.sum(axis=0) # eliminate axis 0
b.sum(axis=1) # eliminate axis 1
b.sum(axis=2) # eliminate axis 2

assert b.shape             == (3, 4, 5)
assert b.sum(axis=0).shape == (   4, 5)
assert b.sum(axis=1).shape == (3,    5)
assert b.sum(axis=2).shape == (3, 4   )


# indexing, slicing and iterating #

def f(x, y):
    return (10 * x) + y

b = np.fromfunction(f, (5, 4), dtype=int)

b[ 2   , 3 ]
b[ 0:5 , 1 ]
b[ 0:  , 1 ]
b[  :  , 1 ]
b[ 1:3 , : ]

# iterating over the first axis
for row in b:
    print(row)

# iterating over all elements
for element in b.flat:
    print(element)


# shape manipulation #

# changing the shape of an array

a = np.floor(10 * np.random.random((3, 4)))
a.shape
a.ravel() # flatten
assert a.ravel().shape == (12, )

a.reshape(6, 2)
a.T
a.T.shape
a.shape # no side effect done

def tuple_reverse(tu):
    l = list(tu)
    l.reverse()
    return tuple(l)

a = np.floor(10 * np.random.random((3, 4, 5)))
assert a.shape == tuple_reverse(a.T.shape)

# stacking together different arrays

a = np.floor(10*np.random.random((2,2)))
b = np.floor(10*np.random.random((2,2)))

np.vstack((a, b))
np.hstack((a, b))

np.concatenate((a, b), axis=0)
np.concatenate((a, b), axis=1)

# splitting one array into several smaller ones

a = np.floor(10*np.random.random((6, 9)))
np.vsplit(a, 3)
np.hsplit(a, 3)
np.array_split(a, 3, axis=0)
np.array_split(a, 3, axis=1)

# copies and views #

a = np.arange(12)
b = a
b is a
b.shape = (3, 4)

# view or shallow copy

c = a.view()
c is a
c.base is a
c.flags.owndata
c.shape = (2, 6)
a.shape
c[0, 4] = 1234

# deep copy
a = np.array([[1, 2], [3, 4]])
d = a.copy()
d is a
d.base is a
d[0, 0] = 9999


# indexing with boolean arrays #

# arrays can be indexed by arrays
# function like this must be dependently typed

# - Array
#   : -> .shape : [:n Nat Vector]
#        .T : Type
#     -- Type

# - array-index1
#   : -> :shape :T Array
#        :index-shape :shape like Array
#     -- :index-shape :T Array

# - array-index2
#   : -> :shape :T Array
#        :index-shape Nat Array :shape.length swap Vector
#     -- :index-shape :T Array

# - where [like == to-type]

# a common use of indexing with arrays
#   is the search of the maximum value
#   of time-dependent series :

def shape_to_length(shape):
    product = 1
    for x in shape:
        product = product * x
    return product

def shape_arange(shape):
    return np.arange(shape_to_length(shape)).reshape(*shape)

shape = (10, 3)
data = np.sin(shape_arange(shape))
index_array = data.argmax(axis=0)

time = np.linspace(0, 90, 10)
time_max = time[index_array]
# can be viewed as array-index1

index_vect = [index_array, range(data.shape[1])]
data_max = data[index_vect]
# can be viewed as array-index2

np.all(data_max == data.max(axis=0))

# the indexing syntax is heavily overloaded


# arrays can be indexed by arrays of booleans #

# - array-filter
#   : -> :shape :T Array
#        :filter-array : :shape Bool Array
#     -- >< :T Array

# - where >< is a shape of 1D array
#   whose length is the number of ture in :filter-array

shape = (3,4)
a = shape_arange(shape)
b = a > 4

a[b]
a[b] = 0 # side-effect
a


# ix_() #

xx = np.array([[1, 2, 3]])
assert xx.shape == (1, 3)
yx = np.array([[100], [200], [300]])
assert yx.shape == (3, 1)

zx = xx + yx
for i, j in zip(range(zx.shape[0]), range(zx.shape[1])):
    assert zx[i, j] == xx[0, i] + yx[j, 0]

# with ix_()

x = np.array([1, 2, 3])
y = np.array([100, 200, 300])
xx, yx = np.ix_(x, y)

zx = xx + yx
for i, j in zip(range(zx.shape[0]), range(zx.shape[1])):
    assert zx[i, j] == x[i] + y[j]


# histograms #

import matplotlib.pyplot as plt

mu = 2
sigma = 0.5
v = np.random.normal(mu, sigma, 10000)

plt.hist(v, bins=50, normed=1)
# plt.show()


# rolling_window #

def rolling_window(a, window):
    shape = a.shape[:-1] + (a.shape[-1] - window + 1, window)
    strides = a.strides + (a.strides[-1], )
    return np.lib.stride_tricks.as_strided(a, shape=shape, strides=strides)

shape = (3, 4)
a = shape_arange(shape)
rolling_window(a, 1)
rolling_window(a, 2)
rolling_window(a, 3)
rolling_window(a, 4)


# rank #

r = rolling_window(x, n)
rank_r = r.argsort(axis=-1).argsort(axis=-1)

shape = (10, 3)
a = shape_arange(shape)
r = rolling_window(a, 4)

a.argsort()
a.argsort().argsort()

array = np.array([4,2,7,1])
temp = array.argsort()
ranks = np.arange(len(array))[temp.argsort()]

a.sort()



shape = (10, )
a = np.sin(shape_arange(shape))
r = rolling_window(a, 3)
s = r.argsort(axis=-1).argsort(axis=-1)
s[:,-1]
i = a.argmax(axis=0)


# nan #

a = np.array([np.NaN, np.NaN, np.NaN])
np.isnan(a)
for x in a:
    print()

b = a == np.NaN
