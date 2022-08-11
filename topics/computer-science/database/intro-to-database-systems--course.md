---
title: Intro to Database Systems (Course)
school: Carnegie Mellon University
year: 2019
playlist: 'https://www.youtube.com/playlist?list=PLSE8ODhjZXjbohkNBWQs_otTrBTrjyohi'
---

# 01 - Course Introduction & Relational Model

select : set => set

```
repo.select(() => or({ ... }, { ... }, and({ ... }, { ... })))
```

project: set => set

```
repo.select(() => or({ ... }, { ... }, and({ ... }, { ... }))).project([ ... ])
repo.select(() => or({ ... }, { ... }, and({ ... }, { ... }))).map(() => ...)
```

```
select { } from () where {

}
```

union
intersection
difference

product -- need a prefix

# 02 - Advanced SQL
# 03 - Database Storage I
# 04 - Database Storage II
# 05 - Buffer Pools + Memory Management
# 06 - Hash Tables
# 07 - Tree Indexes I
# 08 - Tree Indexes II
# 09 - Multi-Threaded Index Concurrency Control
# 10 - Sorting & Aggregations
# 11 - Join Algorithms
# 12 - Query Execution I
# 13 - Query Execution II
# 14 - Query Planning & Optimization I
