readonlylink 中，有些 headline 是类似分隔符的，比如 “part 1”，可以不占用一级 headline，用定制的 element 处理。
 
readonlylink 的作者订阅按钮，点击可以提示打开订阅列表。

recall 页面假设了用户只对得分最高的几个 mimor 感兴趣，所以即便在顶部加上了日历图等统计信息，也不需要使用 fixed layout，和 search 页面类似就行。

搜索 state 也应该被 cache，因为对于 recall 来说 targets 可能很多。
