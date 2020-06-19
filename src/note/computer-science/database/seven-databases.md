---
title: Seven Databases
---

# Intro

## Type of datastore

- relational -- Postgres
- key-value -- Riak, Redis
- columnar -- HBase
- document-oriented -- MongoDB, CouchDB
- graph -- Neo4J

# PostgreSQL

## intro

- row -- tuple
  typed record

- header row -- header tuple
  definition of a record-type

- column -- attribute
  field of record and its type

- table -- relation
  a set of rows
  a set of typed records
  a subset of the set of all records of a certain record-type

## Day 1: Relations, CRUD, and Joins

``` sql
CREATE TABLE countries (
  country_code CHAR (2) PRIMARY KEY,
  country_name TEXT UNIQUE
);

-- PRIMARY KEY and UNIQUE are constraints
--   additional information about the fields of the record
--   other than types

INSERT INTO countries (country_code, country_name)
VALUES
  ('us', 'United States'),
  ('mx', 'Mexico'),
  ('au', 'Australia'),
  ('gb', 'United Kingdom'),
  ('de', 'Germany'),
  ('ll', 'Loompaland');

INSERT INTO countries
VALUES
  ('cn', 'China');

SELECT *
FROM countries;

DELETE FROM countries
WHERE country_code = 'll';

SELECT *
FROM countries;

CREATE TABLE cities (
  name TEXT NOT NULL,
  postal_code VARCHAR (9) CHECK (postal_code <> ''),
  country_code CHAR (2) REFERENCES countries,
  PRIMARY KEY (country_code, postal_code)
);

-- REFERENCES is foreign key constraint
-- PRIMARY KEY (compound key) uniquely identifies a row

INSERT INTO cities
VALUES ('YinChuan', '666666', 'cn');

INSERT INTO cities
VALUES ('Portland', '87200', 'us');

UPDATE cities
SET postal_code = '97205'
WHERE name = 'Portland';

-- join
--   join tables together when reading them

-- INNER JOIN

SELECT cities, country_name
FROM cities INNER JOIN countries
ON cities.country_code = countries.country_code;

-- SELECT specify pattern to print

SELECT cities.*, country_name
FROM cities INNER JOIN countries
ON cities.country_code = countries.country_code;

SELECT cities.*, country_name
FROM countries INNER JOIN cities -- the order does not matter
ON cities.country_code = countries.country_code;

CREATE TABLE venues (
  venue_id SERIAL PRIMARY KEY,
  name VARCHAR (255),
  street_address TEXT,
  type CHAR (7)
    CHECK ( TYPE IN ('public', 'private') )
    DEFAULT 'public',
  postal_code VARCHAR (9),
  country_code CHAR (2),
  FOREIGN KEY (country_code, postal_code)
    REFERENCES cities (country_code, postal_code) MATCH FULL
);

INSERT INTO venues (name, postal_code, country_code)
VALUES ('Crystal Ballroom', '97205', 'us');

-- compound INNER JOIN

SELECT venue.venue_id, venue.name, city.name
FROM venues venue INNER JOIN cities city
ON venue.postal_code = city.postal_code
AND venue.country_code = city.country_code;

SELECT venue, city
FROM venues venue INNER JOIN cities city
ON venue.postal_code = city.postal_code
AND venue.country_code = city.country_code;

INSERT INTO venues (name, postal_code, country_code)
VALUES ('Voodoo Donuts', '97205', 'us')
RETURNING venue_id; -- return columns after insertion

-- The Outer Limits

CREATE TABLE events (
  event_id SERIAL PRIMARY KEY,
  title TEXT,
  starts TIMESTAMP,
  ends TIMESTAMP,
  venue_id SERIAL REFERENCES venues
);

INSERT INTO events (title, starts, ends, venue_id)
VALUES
('LARP Club',
 '2012-02-15 17:30:00',
 '2012-02-15 19:30:00',
 2);

INSERT INTO events (title, starts, ends)
VALUES
('April Fools Day',
 '2012-04-01 00:00:00',
 '2012-04-01 23:59:00'),
('Christmas Day',
 '2012-12-25 00:00:00',
 '2012-12-25 23:59:00');

-- default JOIN is INNER JOIN

SELECT e.title, v.name
FROM events e JOIN venues v
ON e.venue_id = v.venue_id;

-- OUTER JOIN is all about NULL

SELECT e.title, v.name
FROM events e LEFT OUTER JOIN venues v
ON e.venue_id = v.venue_id;

SELECT e.title, v.name
FROM events e LEFT JOIN venues v -- same as LEFT OUTER JOIN
ON e.venue_id = v.venue_id;

SELECT e.title, v.name
FROM venues v RIGHT JOIN events e
ON e.venue_id = v.venue_id;

SELECT e.title, v.name
FROM venues v FULL JOIN events e
ON e.venue_id = v.venue_id;

-- Fast Lookups with Indexing

-- unique field can be used as hashing index
--   to find the record in constant time

CREATE INDEX events_title
ON events USING hash (title);

-- use btree for order relation

CREATE INDEX events_starts
ON events USING btree (starts);

SELECT *
FROM events
WHERE starts >= '2012-04-01';

-- Write a query that
--   finds the country name of the LARP Club event.

-- SELECT country.country_name
-- FROM events event
-- JOIN countries country
-- JOIN venues venue
-- WHERE event.title = 'LARP Club'
-- AND event.venue_id = venue.venue_id
-- AND venue.country_code = country.country_code;
```

# Riak

# HBase

# MongoDB

## [note]

- db -- namespace
  collection -- document set
  document -- json

- query as a filer of type

  ``` js
  { list_t(document_t) { document_t -> bool_t } -> list_t(document_t) }
  ```

## 5.2 Day 1: CRUD and Nesting

- create

  ``` js
  db.towns.insert ({
      name: "New York",
      population: 22200000,
      last_census: ISODate ("2009-07-31"),
      famous_for: [ "statue of liberty", "food" ],
      mayor : {
          name : "Michael Bloomberg",
          party : "I",
      }
  })
  ```

- object id consists of :
  time mid pid inc
  - thus distributed

- db.collection.interface
  - insert (document)
  - find (pattern, field_flag)
  - update (pattern, operation)
  - remove (pattern)

  - js function

    ``` js
    db.towns.find ({
        $where : function () {
            return this.population > 6000
                && this.population < 600000;
        },
        famous_for : /groundhog/,
    })
    ```

# CouchDB

# Neo4J

# Redis

- REmote DIctionary Service

## command line tools

- redis-server
  with dump.rdb file in current dir

- redis-cli -- client

## Day 1: CRUD and Datatypes

- for a url-shorten app

``` redis
SET 7wks http://www.sevenweeks.org/
GET 7wks

MSET gog http://www.google.com yah http://www.yahoo.com
MGET gog yah

SET count 2
INCR count
GET count

HMSET user:eric name "Eric Redmond" password s3cret
HMGET user:eric name password
HVALS user:eric
```
