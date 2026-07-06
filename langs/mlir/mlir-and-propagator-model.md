# Propagator Network 代码示例集

## 1. 基本算术与级联流水线

    (: a number-t)
    (: b number-t)
    (: c number-t)
    (: d number-t)

    (= sum-ab (adder a b))
    (= sum-abc (adder sum-ab c))
    (= product (multiplier sum-abc d))

    (= quotient remainder (divmod product a))
    (= squared (square remainder))
    (= doubled (doubler quotient))

## 2. 扇出（单输入驱动多个传播者）

    (: base number-t)
    (: coeff1 number-t)
    (: coeff2 number-t)

    (= result1 (multiplier base coeff1))
    (= result2 (multiplier base coeff2))
    (= base-squared (square base))
    (= base-negated (negator base))

## 3. 温度转换与物理计算

    (: celsius number-t)
    (: fahrenheit number-t)
    (: kelvin number-t)

    (= fahrenheit (celsius->fahrenheit celsius))
    (= kelvin (celsius->kelvin celsius))

    (= derived-celsius (fahrenheit->celsius fahrenheit))
    (= derived-fahrenheit (celsius->fahrenheit derived-celsius))

## 4. 几何计算网络

    (: width number-t)
    (: height number-t)

    (= area (multiplier width height))
    (= perimeter (adder (doubler width) (doubler height)))

    (: radius number-t)
    (: pi number-t)
    (= diameter (doubler radius))
    (= circumference (multiplier diameter pi))
    (= area-circle (multiplier pi (square radius)))

## 5. 统计与数据处理

    (: data1 number-t)
    (: data2 number-t)
    (: data3 number-t)
    (: data4 number-t)

    (= sum-12 (adder data1 data2))
    (= sum-34 (adder data3 data4))
    (= total (adder sum-12 sum-34))

    (: count number-t)
    (= count (constant 4))
    (= average (divider total count))

    (= diff1 (subtractor data1 average))
    (= diff2 (subtractor data2 average))
    (= diff3 (subtractor data3 average))
    (= diff4 (subtractor data4 average))

    (= sq1 (square diff1))
    (= sq2 (square diff2))
    (= sq3 (square diff3))
    (= sq4 (square diff4))

    (= sum-sq (adder sq1 sq2))
    (= sum-sq (adder sum-sq sq3))
    (= sum-sq (adder sum-sq sq4))

    (= variance (divider sum-sq count))

## 6. 混合线性变换（仿射变换）

    (: x number-t)
    (: weight number-t)
    (: bias number-t)

    (= weighted (multiplier weight x))
    (= output (adder weighted bias))

## 7. 多输出与副作用动作穿插

    (: dividend integer-t)
    (: divisor integer-t)
    (: log-prefix string-t)

    (= q r (divmod dividend divisor))

    (printer "Starting division process...")
    (logger log-prefix dividend)
    (assert-positive divisor)
    (assert-non-negative r)

    (printer "Quotient: " q)
    (printer "Remainder: " r)

## 8. 信号处理（单位换算与偏移）

    (: raw-signal number-t)
    (: offset number-t)
    (: gain number-t)

    (= centered (subtractor raw-signal offset))
    (= normalized (multiplier centered gain))

    (: min-val number-t)
    (: max-val number-t)
    (= clamped-output (clamp normalized min-val max-val))

## 9. 复合流水线（多级前馈网络）

    (: input1 number-t)
    (: input2 number-t)
    (: input3 number-t)

    (= processed-a (doubler input1))
    (= processed-b (square input2))
    (= processed-c (negator input3))

    (= feature-ab (adder processed-a processed-b))
    (= feature-total (adder feature-ab processed-c))

    (: scale number-t)
    (= final-output (multiplier feature-total scale))

    (printer "Layer 1 sum: " feature-ab)
    (printer "Final output: " final-output)

## 10. 常数值注入与依赖更新

    (: threshold number-t)
    (: current-value number-t)

    (= fixed-threshold (constant 100))
    (= delta (subtractor current-value fixed-threshold))
    (= penalty (relu delta))
    (= compensated (subtractor current-value penalty))
