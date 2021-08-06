stream, tree, mesh

# info

- DOMAIN MODELING WITH DATALOG by Norbert Wojtowicz
  - video: https://www.youtube.com/watch?v=oo-7mN9WXTw&ab_channel=%23pivorakLvivRubyMeetUp
  - links: https://gist.github.com/pithyless/e00362aa6061bfb4e4749079a33be073

# data -- stream, tree, mesh

- stream: order -- event, queue
- tree: hierarchy -- object composition, ui components
- mesh: relation & graph

## example: spotify

basic nodes:
- artist
- listener
- song

everything else are relations:
- album: artist, song
- playlist: user, song
- subscribe to playlist: user, playlist

# datalog -- entity, attribute, value

- xie: note that,
  - formal concept analysis only have: object (entity) and attribute
  - we can also add time to get: time, entity, attribute, value

## example: github

### create some users

``` js
{ "user_name": "xie" }
{ "user_name": "yu" }
{ "user_name": "heng" }
```

database:

``` js
[11, "user_name", "xie"]
[22, "user_name", "yu"]
[33, "user_name", "heng"]
```

### run some queries

``` js
query([11, _, _])
// -> success -- because there is an entity 11

query([_, "user_name", "xie"])
// -> success -- because there is an user called "xie"

query([33, "user_name", "xie"])
// -> fail -- because entity 33's user name is not "xie"
```

### queries that not only success or fail but also return matched result set

``` js
query([p`e`, _, _])
// -> { e: 11 }, { e: 22 }, { e: 33 }

query([p`e`, p`a`, _])
// -> { e: 11, a: "user_name" }, { e: 22, a: "user_name" }, { e: 33, a: "user_name" }

query([p`e`, "user_name", "xie"])
// -> { e: 11 }

query([p`e`, "user_name", p`v`])
// -> { e: 11, v: "xie" }, { e: 22, v: "yu" }, { e: 33, v: "heng" }

find([p`name`])
  .where([11, "user_name", p`name`])
// -> { name: "xie" }

find([p`name`])
  .where([_, "user_name", p`name`])
// -> { name: "xie" }, { name: "yu" }, { name: "heng" }

find([p`id`, p`name`])
  .where([p`id`, "user_name", p`name`])
// -> { id: 11, name: "xie" }, { id: 22, name: "yu" }, { id: 33, name: "heng" }
```

### add email address to users

``` js
{ "user_name": "xie",  "user_email": "x@c.com" }
{ "user_name": "yu",   "user_email": "y@c.com" }
{ "user_name": "heng", "user_email": "h@c.com" }
```

database:

``` js
[11, "user_name", "xie"]
[22, "user_name", "yu"]
[33, "user_name", "heng"]
[11, "user_email", "x@c.com"]
[22, "user_email", "y@c.com"]
[33, "user_email", "h@c.com"]
```

### run some queries

``` js
find([p`email`])
  .where(
    [p`id`, "user_name", "xie"],
    [p`id`, "user_email", p`email`])
// -> { email: "x@c.com" }
```

### introduce more concepts: repo, org, owner

``` js
{ "org_name": "cicada-lang" }
{ "repo_slug": "cicada-lang/cicada", "repo_owner": 44 }
{ "repo_slug": "cicada-lang/cicadascript", "repo_owner": 22 }

{ "repo_slug": "cicada-lang/cicada", "repo_owner": ["org_name", "cicada-lang"] }
{ "repo_slug": "cicada-lang/cicadascript", "repo_owner": ["user_name", "yu"] }

       ["org_name", "cicada-lang"] returns the unique id in
[p`id`, "org_name", "cicada-lang"]

       ["user_name", "yu"] returns the id in
[p`id`, "user_name", "yu"]

[11, "user_name", "xie"]
[22, "user_name", "yu"]
[33, "user_name", "heng"]
[11, "user_email", "x@c.com"]
[22, "user_email", "y@c.com"]
[33, "user_email", "h@c.com"]
[44, "org_name", "cicada-lang"]
[55, "repo_slug", "cicada-lang/cicada"]
[55, "repo_owner", 44]
[66, "repo_slug", "cicada-lang/cicadascript"]
[66, "repo_owner", 22]
```

``` js
find([p`repo`])
  .where(
    [p`p`, "user_name", "yu"],
    [p`r`, "repo_owner", p`p`],
    [p`r`, "repo_slug", p`repo`])
// -> { repo: "cicada-lang/cicadascript" }

find([p`name`, p`repo`])
  .where(
    or([p`p`, "org_name", p`name`],
       [p`p`, "user_name", p`name`]),
    [p`r`, "repo_owner", p`p`],
    [p`r`, "repo_slug", p`repo`])
// -> { name: "cicada-lang", repo: "cicada-lang/cicada" }, { name: "yu", repo: "cicada-lang/cicadascript" }

repo_owner(p`p`, p`name`)
  [p`p`, "org_name", p`name`]
  [p`p`, "user_name", p`name`]

find([p`name`, p`repo`])
  .where(
    repo_owner(p`p`, p`name`),
    [p`r`, "repo_owner", p`p`],
    [p`r`, "repo_slug", p`repo`])
// -> { name: "cicada-lang", repo: "cicada-lang/cicada" }, { name: "yu", repo: "cicada-lang/cicadascript" }
```

### introduce more concepts: fork

``` js
{ "repo_slug": "xie/cicadascript",
  "repo_owner": ["user_name", "xie"],
  "repo_fork": ["repo_slug", "cicada-lang/cicadascript"] }

[11, "user_name", "xie"]
[22, "user_name", "yu"]
[33, "user_name", "heng"]
[11, "user_email", "x@c.com"]
[22, "user_email", "y@c.com"]
[33, "user_email", "h@c.com"]
[44, "org_name", "cicada-lang"]
[55, "repo_slug", "cicada-lang/cicada"]
[55, "repo_owner", 44]
[66, "repo_slug", "cicada-lang/cicadascript"]
[66, "repo_owner", 22]
[77, "repo_slug", "xie/cicadascript"]
[77, "repo_owner", 11]
[77, "repo_fork", 66]
```

find all the repos that are forks:

``` js
find([p`repo`])
  .where(
    [p`r`, "repo_slug", p`repo`],
    [p`r`, "repo_fork", _])
// -> { repo: "xie/cicadascript" }
```

find all the repos that are not forks:

``` js
find([p`repo`])
  .where(
    [p`r`, "repo_slug", p`repo`],
    missing(p`r`, "repo_fork"))
// -> { repo: "cicada-lang/cicadascript" }, { repo: "cicada-lang/cicada" }
```

find fork relations:

``` js
find([p`orig`, p`fork`])
  .where(
    [p`orig_id`, "repo_slug", p`orig`],
    [p`fork_id`, "repo_slug", p`fork`],
    [p`fork_id`, "repo_fork", p`orig_id`])
// -> { fork: "xie/cicadascript", orig: "cicada-lang/cicadascript" }
```

## example: indexes

| structure | index          |
|-----------+----------------|
| key/value | A V E          |
| row       | E A V          |
| colume    | A E V          |
| document  | E A V          |
| graph     | V A E          |
| search    | inverted-index |

## we can do meta programming by implementing attribute as entity

``` js
[11, 1, "xie"]
[1, "db_ident", "user_name"]
[1, "db_value_type", "string"]
[1, "db_doc", "github username"]
[1, "db_unique", "unique_identity"]
[1, "db_fulltext", true]
```

## we can implement transaction by using: entity, attribute, value, tx, op

- tx -- transaction -- created by the same transaction
- op -- operation -- use true and false for model forgetting

updating := forget the old one, remember the new one, in the same transaction
