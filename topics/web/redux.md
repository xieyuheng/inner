---
title: Redux
---

# TODO

- courses:

  - https://egghead.io/courses/building-react-applications-with-idiomatic-redux
  - https://app.egghead.io/playlists/fundamentals-of-redux-course-from-dan-abramov-bd5cc867

- videos:
  - Mark Erikson - The Fundamentals of Redux
    https://www.youtube.com/watch?v=ewelU8WHXQ4
  - https://www.learnwithjason.dev/let-s-learn-modern-redux

# Hype

关于 `@reduxjs/toolkit` 的文档。

# @reduxjs/toolkit

- No need to write type for action type string by hand.

- No need to write functions to create actions by hand.

- No need to write immutable updates by hand.

  Just write mutable updates, `immer` will create immutable updates from them.

  > Writing immutable update logic by hand is hard, and accidentally
  > mutating state in reducers is the single most common mistake
  > Redux users make.

# 关于测试

> By now you might be wondering, "Do I always have to put all my app's
> state into the Redux store?"
>
> The answer is NO. Global state that is needed across the app should go
> in the Redux store. State that's only needed in one place should be
> kept in component state.

这些 component state 就是需要测试的业务逻辑。
Redux 可以测试，但是这些用 `useState` 实现的 component state
还是没法测试。
