-- phpMyAdmin SQL Dump
-- version 3.2.0.1
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generation Time: Sep 15, 2009 at 10:59 du.
-- Server version: 5.1.37
-- PHP Version: 5.3.0

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;

--
-- Database: `myadm`
--

-- --------------------------------------------------------

--
-- Table structure for table `items`
--

DROP TABLE IF EXISTS `items`;
CREATE TABLE IF NOT EXISTS `items` (
  `subject` text,
  `c` tinyint(1) DEFAULT NULL,
  `b` tinyint(1) DEFAULT NULL,
  `d` tinyint(1) DEFAULT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `items`
--

INSERT INTO `items` (`subject`, `c`, `b`, `d`) VALUES
('q1', 0, 0, 0),
('q2', 0, 0, 1),
('q3', 0, 1, 0),
('q4', 0, 1, 1),
('q5', 1, 0, 0),
('q6', 1, 0, 1),
('q7', 1, 1, 0),
('q8', 1, 1, 1),
('qx9', 0, 1, 0),
('qx10', 0, 0, 0),
('qx11', 0, 0, 0),
('qx12', 0, 0, 1);

-- --------------------------------------------------------

--
-- Table structure for table `relationship_a`
--

DROP TABLE IF EXISTS `relationship_a`;
CREATE TABLE IF NOT EXISTS `relationship_a` (
  `subject` text,
  `object` text
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `relationship_a`
--

INSERT INTO `relationship_a` (`subject`, `object`) VALUES
('q1', 'q2'),
('q1', 'q4'),
('qx9','q3'),
('qx10', 'qx11');


-- --------------------------------------------------------

--
-- Table structure for table `relationship_e`
--

DROP TABLE IF EXISTS `relationship_e`;
CREATE TABLE IF NOT EXISTS `relationship_e` (
  `subject` text,
  `object` text
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

--
-- Dumping data for table `relationship_e`
--

INSERT INTO `relationship_e` (`subject`, `object`) VALUES
('q1', 'q4'),
('q4', 'q3'),
('q4', 'q2'),
('qx11','qx12');

/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
