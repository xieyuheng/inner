class Part {
  amount: number
  date: Date
}

class Station {
  rate: number

  computePart(aPart) {
    return this.multiplyPartTimesRate(aPart)
  }

  multiplyPartTimesRate(aPart) {
    return Part({
      amount: aPart.amount * this.rate,
      date: aPart.date,
    })
  }
}


// 改成：

class Part {
  amount: number
  date: Date

  mul(aRate) {
    return Part({
      amount: this.amount * aRate,
      date: this.date,
    })
  }
}

class Station {
  rate: number

  computePart(aPart) {
    return aPart.mul(this.rate)
  }
}
