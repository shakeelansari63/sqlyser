# -*- coding: utf-8 -*-
"""
Cleaning module for SQL

Developer: Shakeel Ansari
"""
import re
def cleanCode(inputCode):
    singleLineComment = re.compile(r"[\r\n]{1,}\s*\-\-.*?(?=[\r\n])", re.IGNORECASE)
    multiLineComment = re.compile(r"/\*[^\+]\s*(.*?)\*/",re.DOTALL)
    multiSpace = re.compile(r"( )(?= )")
    beginningSpaces = re.compile(r"^\s*", re.IGNORECASE)
    
    # Append Semicolon at end
    outputCode = inputCode + ";"
    
    # Remove Comments
    outputCode = re.sub(singleLineComment,'',outputCode)
    outputCode = re.sub(multiLineComment,'',outputCode)
    
    # Remove Tabs
    outputCode = outputCode.replace('\t',' ')
    
    # Remove Line Feeds and Carriage Returns
    outputCode = outputCode.replace('\n',' ').replace('\r',' ')
    
    # Add Spaces Before ")" and After "(" to consistently keep differences
    outputCode = outputCode.replace(")"," ) ").replace("("," ( ").replace(";"," ; ")
    
    # Remove multiple Spaces
    outputCode = re.sub(multiSpace, '', outputCode)
    
    # Remove leading spaces in a query
    outputCode = re.sub(beginningSpaces, '', outputCode)
    
    #Remove Spaces if between Table and Column / Database and Table
    outputCode = outputCode.replace(" . ",".").replace(" .",".").replace(". ",".")
    
    # Remove spaces between commas
    outputCode = outputCode.replace(" , ",",").replace(" ,",",").replace(", ",",").replace("; ;",";").replace(',',', ')
    
    # Add a space in Beginning and ending
    outputCode = " " + outputCode + " "

    # Return clean Code
    return outputCode

def reFormatSql(sql):
    """Re-format SQL for Output"""
    newLineRe = re.compile(r" SELECT | FROM | WHERE | JOIN | AND | ORDER BY | GROUP BY | HAVING ", re.IGNORECASE)
    findKeywords = re.findall(newLineRe, sql)
    for key in set(findKeywords):
        sql = sql.replace(key, '\n' + key)
    sql = re.sub(r',', '\n,', sql)
    return sql