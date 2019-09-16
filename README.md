# SQLYSER
An SQL analyser program to analyse SQL queries using simple Regular expressions.

## Requirement 
Python3

## Features
 - Get Source tables list from SQL
 - Get Target table name from SQL
 - Convert subqueries to temp tables and generate SQL

## Installation
 - Clone this repo and run following from terminal
```bash
python setup.py install
```

## Example and How to use
```python
sqlStr = '''
insert into a_target /* target Table */
(c1, c2, c3) 
select b.c1, b.c2, b.c3 
from ( select c1, c2 from b ) b /* Source Table b */
inner join c /* Source Table c */
on b.c4 = c.c4;
;'''
sql = SQL(sqlStr)
sql.getSourceTables()
```
```
set(['b','c'])
```