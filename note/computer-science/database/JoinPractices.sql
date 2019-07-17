CREATE TABLE Department(
  DepartmentId INT PRIMARY KEY,
  DepartmentName VARCHAR(20));

CREATE TABLE Employee(
  EmployeeId INT PRIMARY KEY,
  LastName VARCHAR(20),
  Country VARCHAR(20),
  DepartmentId INT REFERENCES Department);

INSERT INTO Department (DepartmentId, DepartmentName)
VALUES
  (31, 'Sales'),
  (33, 'Engineering'),
  (34, 'Clerical'),
  (35, 'Marketing');

INSERT INTO Employee (EmployeeId, LastName, Country, DepartmentId)
VALUES
  (123, 'Rafferty', 'Australia', 31),
  (124, 'Jones', 'Australia', 33),
  (145, 'Heisenberg', 'Australia', 33),
  (201, 'Robinson', 'United States', 34),
  (305, 'Smith', 'Germany', 34),
  (306, 'Williams', 'Germany', NULL);

-- cross join
-- returns the cartesian product of rows from tables in the join.
-- normal uses are for checking the server's performance.

SELECT *
FROM Employee CROSS JOIN Department;

SELECT *
FROM Employee, Department;

-- inner join
-- requires each row in the two joined tables to satisfy join-predicate

SELECT Employee.LastName, Employee.DepartmentId, Department.DepartmentName
FROM Employee INNER JOIN Department
ON Employee.DepartmentId = Department.DepartmentId;

SELECT Employee.LastName, Employee.DepartmentId, Department.DepartmentName
FROM Employee, Department
WHERE Employee.DepartmentId = Department.DepartmentId;

-- equi join
-- special inner join
-- where join-predicate is limited to eq

SELECT Employee.LastName, Employee.DepartmentId, Department.DepartmentName
FROM Employee JOIN Department
ON Employee.DepartmentId = Department.DepartmentId;

SELECT Employee.LastName, Employee.DepartmentId, Department.DepartmentName
FROM Employee, Department
WHERE Employee.DepartmentId = Department.DepartmentId;

SELECT *
FROM Employee INNER JOIN Department
USING (DepartmentId);

-- natural join
-- The result of the natural join
--   is the set of all combinations of tuples in R and S
--   that are equal on their common attribute names.
-- in particular, the natural join allows the combination of relations
--   that are associated by a foreign key.
-- composition of relations.
-- relational counterpart of logical AND.
-- in category theory, the join is precisely the fiber product.

SELECT *
FROM Employee NATURAL JOIN Department;

-- outer join

-- the joined table retains each row
--   even if no other matching row exists.
-- left outer joins, right outer joins, and full outer joins
--   depending on which table's rows are retained: left, right, or both.

SELECT *
FROM Employee LEFT OUTER JOIN Department
ON Employee.DepartmentId = Department.DepartmentId;

SELECT *
FROM Employee RIGHT OUTER JOIN Department
ON Employee.DepartmentId = Department.DepartmentId;

SELECT *
FROM Employee FULL OUTER JOIN Department
ON Employee.DepartmentId = Department.DepartmentId;

-- self join
-- joining a table to itself.

SELECT A.EmployeeId, A.LastName, B.EmployeeId, B.LastName, A.Country
FROM Employee A INNER JOIN Employee B
ON A.Country = B.Country
WHERE A.EmployeeId < B.EmployeeId
ORDER BY A.EmployeeId, B.EmployeeId;
