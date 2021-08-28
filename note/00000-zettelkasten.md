---
title: Zettelkasten
date: 2021-08-22
---

# 区分

Zettelkasten 是一种记录笔记的方式，我认为它的要点在于，
记录笔记时，要区分 永久保留的笔记 (permanent) 与 相对临时的笔记。

在写永久保留笔记时，我们应该给出充分的语境，
让笔记相对独立，处在「适合分享给朋友的状态」，
并且使得回顾时更容易理解当时所想。

写作过程就是思考过程，当有意去写「永久保留的笔记」时，
所持的态度就好像是要为之后写文章与书做准备，
这样，简单地改变学习的习惯，就可以让之后的写作更轻松。

这种区分，就像是阴阳与生命的节奏，比如：
- 美国舞蹈家 Twyla Tharp -- 早上练习自己熟悉的舞蹈动作，下午思考新的的编舞。
- 德国社会学家 Niklas Luhmann -- 早上阅读，下午思考所读与自己所关心的研究课题之间的关系。

# 结构

用文件系统 + 纯文本文件来整理笔记。
用 markdown 作为标记语言，用相对路径来链接到别的文件。

每篇笔记要相对较短。
应该到简短到这样一种程度，使得我们回顾笔记的时候，
可以轻松地通读笔记，而不会感到很长。

每个文件有唯一的序号。
序号将作为文件名前缀，来管理笔记的顺序。

每篇笔记带有元数据，以便之后实现相关的软件，来检索与处理这些笔记。
元数据可以包括 标题 (title) 与 标签 (tags)。

# 工具

可以写一些工具（软件），比如：
- Mobile Web App -- 以便随时浏览与编辑
- 同步到 github 与 gitlab 等等
- 搜索：
  - 全文本搜索
  - 根据笔记之间的链接关系搜索
  - 根据笔记的 tags 搜索，或做 Formal concept analysis
- 社交：
  - 链接到朋友的笔记

# 例子

类似方法的使用者包括：
- Niklas Luhmann
- Gottfried Wilhelm Leibniz （根据 Wikipedia 的 [Zettelkasten][] 条目）
- Edsger Wybe Dijkstra 的 [EWD manuscripts](https://www.cs.utexas.edu/users/EWD)
- [Internet RFC -- Request for Comments](https://www.rfc-editor.org)
  - 可以视为一个社区共同的笔记
- 多产的 [Charles Sanders Peirce](https://en.wikipedia.org/wiki/Charles_Sanders_Peirce_bibliography)
  是否也再用类似的方法呢？

[Zettelkasten]: https://en.wikipedia.org/wiki/Zettelkasten
