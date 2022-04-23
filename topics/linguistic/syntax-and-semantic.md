---
title: 語法與語義
author: 謝宇恆
newline: preserve
---

# 名

- 何爲語義
  何爲語法
- 如果只是單純地問出上面的問題的話
  我也沒法回答
- 這是因爲這兩個詞在各種語境下被濫用了
  在不同的語境下
  不同的作者使用它們的時候
  所指的常常是不同的概念
- 這兩個詞是非常典型的遇到這種困境的術語
  此時如果想使討論變得有意義
  討論者必須把這兩個詞的意義
  限制到更具體的語境中

# 定義

因此
我嘗試做如下的定義

- _信息_ 在於 _區分_
  只有當兩個人都有能力區分 紅 和 藍 之時
  這兩個人才能使用 紅 藍 兩種顏色的光來傳遞信息
  又比如
  我們可以把 任意一個物體 的兩種可區分的狀態
  稱作 _一比特_
  然後我們就可以以 _比特_ 爲單位來度量信息了
- 這裏的 _兩個人_ 其實可以具更廣泛的意義
  比如機器和人之間的信息傳遞也可以在這個模型中理解
- _語法_ 是 _信息的結構_
  只有當兩個人能夠傳遞給對方結構化的信息
  並且它們使用相同的 或相似的 方式來理解其結構時
  我們才會把他們互相傳遞信息的行爲稱作是 _交流_ 或 _通訊_
- 在上面對語法的定義中
  還有一個不易理解的詞
  即 _理解_ 這個詞
- 在傳統的數學中有些詞和概念被稱作是公理
  而在 _形式主義_ 的符號化的數學結構中
  也總會指定一個不經定義的符號的集合
  我們可以暫時把 _理解_ 這個詞理解爲 公理性的
  然後利用它來定義 _語義_
- 當連個人彼此交流時
  我稱他們所使用的相同的 或相似的 理解信息的結構的方式
  爲 _語法的語義_
- 在語言學中
  我們觀察 我們在日常生活中 所傳遞給他人的信息的 結構
- 而當編程時
  我設計信息的新的結構
  以用來與機器交流 或控制機器
- 所以當我說 _我爲蟬語設計了新的 λ-演算 的語法_ 時
  你明白了
  這就是在說
  我設計了新的結構化的信息
  當我在把這些結構化的信息傳遞給蟬語的棧機時
  蟬語的棧機將幫我完成 λ-演算
- 可以發現
  當討論 _用程序語言與機器交流_ 時
  我們其實可以不把 _理解_ 理解爲公理性的詞
  而進一步地定義這個詞
  以嘗試去以更具體的方式來理解它
  這種嘗試就留給我的讀者來做了
  我認爲
  當討論與機器的交流時
  我們之所以能夠進一步地具體化某些概念
  是因爲我們已經有很多數學理論來幫助我們理解機器的運作了
  而對於人腦的神經系統的運作方式
  我們還知之甚少

# the words

- what is semantic?
  and what is syntax?
- if you just ask these two questions as above.
  for real.
  I have to admit that nor do I know the answers.
- that is because
  these two words had been used
  in too many contexts in history.
  and in different contexts
  they are very likely to have different meanings.
- these two words
  are very typical terminologys
  that meet this kind of situation,
  of which when one wants to make
  one's discussion meaningful,
  one has to restrict the meanings of these two words
  into certain contexts.

# to define

therefore,
I try to make the following definitions of these two words,
to make our discussion meaningful.

- information is about to distinguish.
  only when two are both able to distinguish red and blue,
  then, two can send informations to each other by lights of these two colors.
  for example, we may call two distinguishable state of any kinds of physical object that we can send to each others -- "one bit",
  and we use "bit" as an unit to measure informations.
- you note that I use the word "two" instead of "two dolphins" or "two cicadas" or "two machines",
  it is because I mean to denote the general.
- syntaxes are structures of informations.
  only when two are able to send each other structured informations;
  and they use the same way (or similar ways) to understand the structures,
  then, we may call the actions of sending each other informations -- the communications between these two.
  for example, you might not able to communicate with me by Chinese text,
  for you do not use the same way (or a similar way)
  to understand the structures of Chinese text
  as the way I use to understand them.
- normal person will simply say that you do not speak Chinese.
  I can not say that, because :
  - I am not normal.
  - the structures of Chinese phonemes (Chinese speech sounds)
    and the structures of Chinese graphemes (Chinese chars)
    are highly (but not totally) separated, you can understand one of them but do not understand the other.
    this is the major reason that keeps China a big Central Empire,
    for when a Northern can not understand what a Southern say,
    he or she can still understand what a Southern write,
    this is because the structures of speech sounds change much rapidly then the structures of chars.
- there is still one verb which is hard to understand in the above definition of "syntax".
  that is just the verb "understand".
- you know, in math, or in a dictionary of some language,
  people always choose some words as undefined-primitive-words.
  with these undefined-primitive-words, the math can still forms a system (or many systems),
  and a dictionary can still be useful to people.
  let us temporarily choose "understand" as an undefined-primitive-word,
  and use it to define "semantic".
- when two communicate with each other,
  I call the way (or the similar ways) they use to understand
  the structures of the informations that they send to each other
  -- the semantics of the syntaxes (or of the structures of the informations).
- in linguistics, we study the structures of the informations we sent to each others in normal life.
  or, one may say that we study the structures of natural language.
- when programming,
  I design new structures of the informations
  that we can use to communicate (or control) the machines.
  or, one may say that I design computer programming language :)
- so, when I say that I had designed new syntax about lambda-calculus in cicada language,
  you know now, it is to say that I had designed a new structure of the informations
  that I can send to computers in cicada language,
  and when I send the informations to computers,
  they will do something that called "lambda-calculus" for me.
- you see, when talk about communicate with computers,
  we actually can not to choose the verb "understand" as an undefined-primitive-word,
  instead we can easily define this verb even further.
  I leave this definition to you -- my dear reader,
  and I can tell you,
  the reason that let us achieve this,
  is simply for we understand how machines "understand" things far better
  then we understand how human understand things.
  actually, we understand machines not much at all,
  but it is just that we understand human brains even less.
