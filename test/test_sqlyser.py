from sqlyser import SQL

sqlstr = """
-- Sample Single Line Comment
/***** Sample multi line comment
-- comment 'some quoted '' strings in single line sql'
' Some quoted strings in multi line comments'
************************/
-- Query starts here 
insert into my_db1. tgt_tbl1
select column_a, column_b, 'Hello this is column ''C' as column_c, ' string with -- inside' as column_d,
'/* Multi Line comment like string insdie valid string */ /* Multi line comment''s string left open in valid literal' column_e,
' Multi line comment''s closig tag only inside valid string ***/' column_f,
substr(column_g, 1, 15) as column_g_sub, extract( day from current_date) as date_col
, substr(' Function with literal', 1, 5) as column_h
, case when ( some_col = x or some_other_col = y) and (yet_another_column=z)
then 'do this' else 'do new this' end as case_col
from db1.tbl1 tb1 -- Table 1
inner join db1. tbl2 tb2 /** Multi line SQL inside
sql
-- lets see how it goes
*******/
on tb1.jc1 = tb2 . jc2 -- join condition for tb1 and tb2
left outer join ( -- starting subquery
    select col_x, col_y, jc2, (col_z || col_a) as col_za, ltrim(rtrim(col_b)) as nested_func_col
    from db2.tbl2 tb2 inner join db2  .    tbl3_select tb3 /* Keyword in table name */
    on tb2.xcol = tb3.ycol
    where tb2.some_col = 5 /* Literal filter */
) tb3 on tb1 .jc2 = tb3 .jc2 -- I am funny guy and I can put comment anywhere ###############
order by column_a
; -- I can also nest the comments /** like this **/
"""

sql1 = SQL(sqlstr)
print(sql1, '\n')
print(sql1.sql_type)
print(sql1.sql_lang)

sql2 = SQL()
print(sql2)
sql2.sql = "Select * from db.tbl"
print(sql2)
