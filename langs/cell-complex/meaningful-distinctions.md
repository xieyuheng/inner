---
title: Meaningful distinctions
date: 2017-01-08
---

- x -
  Bishop 說 'Meaningful distinctions deserve to be maintained.'
  '有意義的區別值得保留'
  如果在 at1 的語法設計中遵循這條規則
  我就不應該讓 type-arrow 與 body-arrow 使用同樣的 (-> ...)
  事實是
  二者在實現上的區別很大
  二者在理解上有些許共通之處

  ```
  type-arrow (-> ...)
  body-arrow (=> ...)
  ```

- k -
  這就像是 aristotle 在 categories 中所提到的 'equivocally'
  在自然語言中這種現象無法避免
  在程序語言中這種現象也存在
  但是我們還是需要考察是否應該使用 (-> ...)
  並且希望在這種考察中 能夠總結出一些準則
  以幫助我們在之後的設計中處理類似的決策

- x -
  我覺得確實應該區分兩種 (-> ...)
  但是我不知道應該如何選擇一個新的關鍵詞來代替其中的一個 '->'
  或找到兩個新的關鍵詞來代替這兩種對 '->' 的使用
  我們先擱置這個問題吧
