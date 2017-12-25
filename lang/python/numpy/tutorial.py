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

def rolling_window(a, window):
    # http://stackoverflow.com/questions/6811183/rolling-window-for-1d-arrays-in-numpy
    shape = a.shape[:-1] + (a.shape[-1] - window + 1, window)
    strides = a.strides + (a.strides[-1], )
    return np.lib.stride_tricks.as_strided(
        a, shape=shape, strides=strides)


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

><><><


# splitting one array into several smaller ones



# copies and views #



# indexing with boolean arrays #



# shape manipulation #



# copies and views #



# linear algebra #



# histograms #
