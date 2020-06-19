---
title: PostgreSQL Note
---

# PostgreSQL Note

## Command line

About database:
- create: `createdb <db_name>`
  like `CREATE DATABASE <db_name>;`
- delete: `dropdb <db_name>`
  like `DROP DATABASE <db_name>`
- list: `psql -l`
  like `SELECT * FROM pg_database;`

## Default databases

- `postgres` for the user `postgres`,
  some programs assume this database exists,
  (such as pgadmin).
- `template1`, whatever you put into it,
  it will be available in a new database
  if you use the following syntax: `create database <db_name>;`
  This can simplify your deployments a lot
  if you rely on pre-installed objects
  for example monitoring or development.
  We also can explicitly specify a db as template
  `CREATE DATABASE <db_name> TEMPLATE <db_name>;`
- `template0` is the only database that has "datallowcon" set to false,
  it is the default unmodifiable database.
  You never should make any changes there.
  In a new pos PostgreSQL instance,
  `template0` and `template1` are exactly the same.
  To recover a new `template1` from `template0`,
  ``` sql
  UPDATE pg_database SET datistemplate = false WHERE datname = 'template1';
  -- you can not drop a database flagged as a template.
  DROP database template1;
  CREATE database template1 TEMPLATE template0;
  UPDATE pg_database SET datistemplate = true WHERE datname = 'template1';
  ```

What about the overhead:
``` sql
SELECT * FROM pg_size_pretty(pg_database_size('template0'));
SELECT * FROM pg_size_pretty(pg_database_size('template1'));
SELECT * FROM pg_size_pretty(pg_database_size('postgres'));
```

# (2001) Postgresql Introduction and Concepts
