from sqlyser import SQL

sqlstr = """
-- Sample Single Line Comment
/***** Sample multi line comment
-- comment 'some quoted '' strings in single line sql'
' Some quoted strings in multi line comments'
************************/
-- Query starts here 
select column_a, column_b, 'Hello this is column ''C' as column_c, ' string with -- inside' as column_d,
'/* Multi Line comment like string insdie valid string */ /* Multi line comment''s string left open in valid literal' column_e,
' Multi line comment''s closig tag only inside valid string ***/' column_f,
substr(column_g, 1, 15) as column_g_sub, extract( day from current_date) as date_col
from db1.tbl1 tb1 -- Table 1
inner join db1.tbl2 tb2 /** Multi line SQL inside
sql
-- lets see how it goes
*******/
on tb1.jc1 = tb2.jc2 -- join condition for tb1 and tb2
left outer join ( -- starting subquery
    select col_x, col_y, jc2
    from db2.tbl2 tb2
    where tb2.some_col = 5 /* Literal filter */
) tb3 on tb1.jc2 = tb3.jc2 -- I am funny guy and I can put comment anywhere ###############
order by column_a
; -- I can also nest the comments /** like this **/
"""

sql = SQL(sqlstr)
print(sql)