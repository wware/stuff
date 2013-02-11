-- mysql -t -u root -p < foo.sql

DROP DATABASE IF EXISTS plugh;
CREATE DATABASE plugh;
USE plugh;

DROP PROCEDURE IF EXISTS `plugh.myfunction`;

CREATE TABLE `first_table` (
  `id` int(11) NOT NULL auto_increment,
  `name` varchar(80) NOT NULL,
  `value` float NOT NULL,
  PRIMARY KEY  (`id`),
  UNIQUE KEY `name` (`name`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

CREATE TABLE `second_table` (
  `id` int(11) NOT NULL auto_increment,
  `name` varchar(80) NOT NULL,
  PRIMARY KEY  (`id`),
  UNIQUE KEY `name` (`name`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

INSERT INTO `first_table` VALUES
       (1, 'Alpha', 1.3),
       (2, 'Bravo', 2.9),  -- no Bravo in second_table
       (3, 'Charlie', 4.6),
       (4, 'Delta', 7.032),
       (5, NULL, 7.032);

INSERT INTO `second_table` VALUES
       (1, 'Alpha'),
       (2, NULL),
       (3, 'Delta'),
       (4, 'Charlie'),
       (5, 'Echo');    -- no Echo in first_table

-- ----------------------------------------
--          STORED PROCEDURE             --
-- ----------------------------------------

DELIMITER //
CREATE PROCEDURE myfunction()
SQL SECURITY INVOKER
BEGIN
   SELECT Avg(value) AS averagevalue
   FROM first_table;
END //
DELIMITER ;

CALL myfunction();

-- ----------------------------------------
--                 JOINS                 --
-- ----------------------------------------

-- Inner join
SELECT * FROM
first_table INNER JOIN second_table
ON first_table.name = second_table.name;

-- Another syntax for inner join, technically not a join??
-- SELECT * FROM
-- first_table, second_table
-- WHERE first_table.name = second_table.name;

SELECT * FROM
first_table LEFT OUTER JOIN second_table
ON first_table.name = second_table.name;

SELECT * FROM
first_table RIGHT OUTER JOIN second_table
ON first_table.name = second_table.name;

-- MySQL does not have full outer joins like this
-- SELECT * FROM
-- first_table OUTER JOIN second_table
-- ON first_table.name = second_table.name;

-- but you can emulate them
SELECT * FROM first_table LEFT JOIN second_table
ON first_table.name = second_table.name
UNION
SELECT * FROM first_table RIGHT JOIN second_table
ON first_table.name = second_table.name;
