CREATE TABLE department(
  DepartmentID INT PRIMARY KEY,
  DepartmentName VARCHAR(20));

CREATE TABLE employee(
  EmployeeID INT PRIMARY KEY,
  LastName VARCHAR(20),
  Country VARCHAR(20),
  DepartmentID INT REFERENCES department);

INSERT INTO department (DepartmentID, DepartmentName)
VALUES
  (31, 'Sales'),
  (33, 'Engineering'),
  (34, 'Clerical'),
  (35, 'Marketing');

INSERT INTO employee (EmployeeID, LastName, Country, DepartmentID)
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
FROM employee CROSS JOIN department;

SELECT *
FROM employee, department;

-- inner join
-- requires each row in the two joined tables to satisfy join-predicate

SELECT employee.LastName, employee.DepartmentID, department.DepartmentName
FROM employee INNER JOIN department
ON employee.DepartmentID = department.DepartmentID;

SELECT employee.LastName, employee.DepartmentID, department.DepartmentName
FROM employee, department
WHERE employee.DepartmentID = department.DepartmentID;

-- equi join
-- special inner join
-- where join-predicate is limited to eq

SELECT employee.LastName, employee.DepartmentID, department.DepartmentName
FROM employee JOIN department
ON employee.DepartmentID = department.DepartmentID;

SELECT employee.LastName, employee.DepartmentID, department.DepartmentName
FROM employee, department
WHERE employee.DepartmentID = department.DepartmentID;

SELECT *
FROM employee INNER JOIN department
USING (DepartmentID);

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
FROM employee NATURAL JOIN department;

-- outer join

-- the joined table retains each row
--   even if no other matching row exists.
-- left outer joins, right outer joins, and full outer joins
--   depending on which table's rows are retained: left, right, or both.

SELECT *
FROM employee LEFT OUTER JOIN department
ON employee.DepartmentID = department.DepartmentID;

SELECT *
FROM employee RIGHT OUTER JOIN department
ON employee.DepartmentID = department.DepartmentID;

SELECT *
FROM employee FULL OUTER JOIN department
ON employee.DepartmentID = department.DepartmentID;

-- self join
-- joining a table to itself.

SELECT F.EmployeeID, F.LastName, S.EmployeeID, S.LastName, F.Country
FROM employee F INNER JOIN employee S
ON F.Country = S.Country
WHERE F.EmployeeID < S.EmployeeID
ORDER BY F.EmployeeID, S.EmployeeID;
