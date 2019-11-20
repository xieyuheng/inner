CREATE TABLE department(
  department_id INT PRIMARY KEY,
  department_name VARCHAR(20));

CREATE TABLE employee(
  employee_id INT PRIMARY KEY,
  last_name VARCHAR(20),
  country VARCHAR(20),
  department_id INT REFERENCES department);

INSERT INTO department (department_id, department_name)
VALUES
  (31, 'Sales'),
  (33, 'Engineering'),
  (34, 'Clerical'),
  (35, 'Marketing');

INSERT INTO employee (employee_id, last_name, country, department_id)
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

SELECT employee.last_name, employee.department_id, department.department_name
FROM employee INNER JOIN department
ON employee.department_id = department.department_id;

SELECT employee.last_name, employee.department_id, department.department_name
FROM employee, department
WHERE employee.department_id = department.department_id;

-- equi join
-- special inner join
-- where join-predicate is limited to eq

SELECT employee.last_name, employee.department_id, department.department_name
FROM employee JOIN department
ON employee.department_id = department.department_id;

SELECT employee.last_name, employee.department_id, department.department_name
FROM employee, department
WHERE employee.department_id = department.department_id;

SELECT *
FROM employee INNER JOIN department
USING (department_id);

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
ON employee.department_id = department.department_id;

SELECT *
FROM employee RIGHT OUTER JOIN department
ON employee.department_id = department.department_id;

SELECT *
FROM employee FULL OUTER JOIN department
ON employee.department_id = department.department_id;

-- self join
-- joining a table to itself.

SELECT A.employee_id, A.last_name, B.employee_id, B.last_name, A.country
FROM employee A INNER JOIN employee B
ON A.country = B.country
WHERE A.employee_id < B.employee_id
ORDER BY A.employee_id, B.employee_id;
