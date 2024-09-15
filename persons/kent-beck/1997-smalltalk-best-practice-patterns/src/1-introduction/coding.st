// 一个例子是这段代码：

Station>>computePart: aPart
  ^self multiplyPartTimesRate: aPart

Station>>multiplyPartTimesRate: aPart
  ^Part
    amount: aPart amount * self rate
    date: aPart date


// I said, “we seem to be using a lot of the Part’s data in
// multiplyPartTimesRate:. Why don’t we move this code into Part?”
// “But we didn’t design Parts to do arithmetic!” “Since the code
// seems to be telling us to do this, let’s try it.”

Part>>* aRate
  ^Part
    amount: amount * aRate
    date: date

Station>>computePart: aPart
  ^aPart * self rate
