---
title: A Relational Model of Data for Large Shared Data Banks
---

# A Relational Model of Data for Large Shared Data Banks

---
- Author: Edgar Frank Codd,
- Date: 1970
---

## Access path dependence

The problem of tree-structured data model,

> many ways to model the same data,
> if the model changed, API must change.

*Structure 1. Projects Subordinate to Parts*

``` js
part: {
  part id
  part name
  part description
  quantity-on-hand
  quantity-on-order
  project: {
    project id
    project name
    project description
    quantity committed
  }
}
```

*Structure 2. Parts Subordinate to Projects*

``` js
project: {
  project id
  project name
  project description
  quantity committed
  part: {
    part id
    part name
    part description
    quantity-on-hand
    quantity-on-order

  }
}
```

*Structure 3. Parts and Projects as Peers
Commitment Relationship Subordinate to Projects*

``` js
part: {
  part id
  part name
  part description
  quantity-on-hand
  quantity-on-order
}

project: {
  project id
  project name
  project description
  part: {
    part id
    quantity committed
  }
}
```

*Structure 4. Parts and Projects as Peers
Commitment Relationship Subordinate to Parts*

``` js
part: {
  part id
  part name
  part description
  quantity-on-hand
  quantity-on-order
  project: {
    project id
    quantity committed
  }
}

project: {
  project id
  project name
  project description
}
```

*Structure 5. Parts, Projects, and
Commitment Relationship as Peers*

``` js
part: {
  part id
  part name
  part description
  quantity-on-hand
  quantity-on-order
}

project: {
  project id
  project name
  project description
}

commit: {
  part id
  project id
  quantity committed
}
```

## Relational model

Most users should interact with a relational model of the data
consisting of a collection of time-varying relationships (rather than relations).

- **[Xie]**
  By "relationships" v.s. "relations",
  the author means "record" (with named fields) vs "tuple" (with unnamed fields)

  I do not use the term "relationships"

Terms:
- Active domain,
  the type depends on users activities
  which depends on time.
- Primary key.
- Foreign key.
- Non-simple domain.

## Non-simple domain & Normal form

A relation with simple domain can be expressed as a (2-dim) table.

Non-simple domain means, an entry of a table can be a little table.

Simple example,

``` js
Employee: {
  name: String
  birthDate: Date
  salaryHistory: Table {
    data: Date
    salary: Salary
  }
}

// normalization =>

Employee: {
  PK employeeId: Id
  name: String
  birthDate: Date
}

SalaryHistory: {
  PK recordId: Id
  FK employeeId: Id
  date: Data
  salary: Salary
}
```

Complex example,

``` js
Employee: {
  name: String
  birthDate: Date
  children: Table {
    name: String
    birthDate: Date
  }
  jobHistory: Table {
    jobTitle: String
    salaryHistory: Table {
      data: Date
      salary: Salary
    }
  }
}

// normalization =>

Employee: {
  PK employeeId: Id
  name: String
  birthDate: Date
}

Children: {
  PK childrenId: Id
  FK employeeId: Id
  name: String
  birthDate: Date
}

JobHistory: {
  PK jobHistoryId: Id
  FK employeeId: Id
  jobTitle: String
}

SalaryHistory: {
  PK salaryHistoryId: Id
  FK jobHistoryId: Id
  data: Date
  salary: Salary
}
```

## Operations on relations

Permutation of column.

We do not care much about this operation, if we always used named columns (fields).
But actually we would like to use unnamed fields from time to time.

Projection from column to field.

Join (generalization of function composition).
