# -*- coding: utf-8 -*-
"""
SQL Analyser Module to break SQLs into smaller pieces and analyse code.

Developer: Shakeel Ansari
"""
from __future__ import print_function
from .cleanse import cleanCode
import re
import os

class SQL():
    """
    Create and object of type SQL
    Syntax - sql = SQL(SQLCode <string>) 
    """
    _openParanthesisAlternative = "«"
    _closeParanthesisAlternative = "»"
    _FROMAlternative = "↔"
        
    def __init__(self, sql):
        self.setSQL(sql)
    
    def __str__(self):
        return self._sqlString 
    
    def setSQL(self, sql):
        """
        This function set the SQL string for SQL object. 
        It can be used to update the SQL string of SQL object anytime in code.
        
        e.g. - 
        >> sql = SQL("SELECT * FROM ABC;")
        >> sql.getSQL()
        SELECT * FROM ABC;
        >> sql.setSQL("SELECT * FROM A1;")
        >> sql.getSQL()
        SELECT * FROM A1;
        """
        self._sqlString = sql
        self._cleanSqlString = cleanCode(self._sqlString)
    
    def getSQL(self):
        """
        Returns the SQL string for SQL object
        
        e.g. -
        >> sql = SQL("SELECT * FROM ABC;")
        >> sql.getSQL()
        SELECT * FROM ABC;
        """
        return self._sqlString 
    
    def getSelectFromSQL(self):
        """
        Returns only select part from SQL string
        
        e.g. - 
        >> sql = SQL("INSERT INTO ZYX SELECT * FROM ABC;")
        >> sql.getSQL()
        SELECT * FROM ABC;
        """
        self._extractSelectFromSQL()
        return self._selectSQLOnly
    
    def getCleanSQL(self):
        """
        Returns clean SQL string by -
            1. Removing Comments between /* & */
            2. Replacing New Lines and Tabs with single Space
            3. Consistently Spacing parenthesis
            4. Triming spaces near '.' and ','
        
        e.g. -
        >> sqlStr = '''
        ...insert into a /* target Table */
        ...(c1, c2, c3) 
        ...select b.c1, b.c2, b.c3 
        ...from ( select c1, c2 from b ) b /* Source Table b */
        ...inner join c /* Source Table c */
        ...on b.c4 = c.c4;
        ...'''
        >> sql = SQL(sqlStr)
        >> sql.getCleanSQL()
        insert into a (c1, c2, c3) select b.c1, b.c2, b.c3 from ( select c1, c2 from b ) b inner join c on b.c4 = c.c4;
        """
        return self._cleanSqlString 
    
    def getDecomposedSQL(self):
        """
        Generate decomposed SQL from single SQL by materializing subqueries and derived queries
        
        e.g. -
        >> sqlStr = '''
        ...insert into a /* target Table */
        ...(c1, c2, c3) 
        ...select b.c1, b.c2, b.c3 
        ...from ( select c1, c2 from b ) b /* Source Table b */
        ...inner join c /* Source Table c */
        ...on b.c4 = c.c4;
        ...'''
        >> sql = SQL(sqlStr)
        >> sql.getDecomposedSQL()
        create table a_subtable_1_temp as (select c1, c2 from b) with data;
        
        insert into a (c1, c2, c3) select b.c1, b.c2, b.c3 from a_subtable_1_temp b inner join c on b.c4 = c.c4;
        """
        self._generateEncodedSQL()
        self._decomposeSQL()
        return self._generateDecodedSQL(self.decomposedSQL)
    
    def getTargetTable(self):
        """
        Extract and return Target Table from SQL
        
        e.g. - 
        >> sqlStr = '''
        ...insert into a_target /* target Table */
        ...(c1, c2, c3) 
        ...select b.c1, b.c2, b.c3 
        ...from ( select c1, c2 from b ) b /* Source Table b */
        ...inner join c /* Source Table c */
        ...on b.c4 = c.c4;
        ...'''
        >> sql = SQL(sqlStr)
        >> sql.getTargetTable()
        a_target
        """
        self._generateTargetTable()
        return self.targetTable
    
    def getSourceTables(self):
        """
        Extract and return set of source tables (Tables in FROM and JOINS) from SQL
        
        e.g. -
        >> sqlStr = '''
        ...insert into a_target /* target Table */
        ...(c1, c2, c3) 
        ...select b.c1, b.c2, b.c3 
        ...from ( select c1, c2 from b ) b /* Source Table b */
        ...inner join c /* Source Table c */
        ...on b.c4 = c.c4;
        ...'''
        >> sql = SQL(sqlStr)
        >> sql.getSourceTables()
        set(['b','c'])
        """
        self._generateEncodedSQL()
        self._decomposeSQL()
        self._generateSourceTablesSet()
        return self.sourceTable
    
    def getMatchingSourceTables(self, masterSet):
        """
        Match the source tables list with input Master set and return the list of tables which exist in Master
        
        e.g. 
        >> sqlStr = '''
        ...insert into a_target /* target Table */
        ...(c1, c2, c3) 
        ...select b.c1, b.c2, b.c3 
        ...from ( select c1, c2 from b ) b /* Source Table b */
        ...inner join c /* Source Table c */
        ...on b.c4 = c.c4;
        ...'''
        >> sql = SQL(sqlStr)
        >> sql.getMatchingSourceTables(['a','c','d'])
        set(['c'])
        """
        masterSet = [table.upper() for table in masterSet]
        sourceTableSet = set(self.getSourceTables())
        for table in self.sourceTable:
            if table.upper() not in masterSet:
                sourceTableSet.discard(table)
        return sourceTableSet
    
    def getNonMatchingSourceTables(self, masterSet):
        """
        Match the source tables list with input Master set and return the list of tables which does not exist in Master
        
        e.g. 
        >> sqlStr = '''
        ...insert into a_target /* target Table */
        ...(c1, c2, c3) 
        ...select b.c1, b.c2, b.c3 
        ...from ( select c1, c2 from b ) b /* Source Table b */
        ...inner join c /* Source Table c */
        ...on b.c4 = c.c4;
        ...'''
        >> sql = SQL(sqlStr)
        >> sql.getMatchingSourceTables(['a','c','d'])
        set(['b'])
        """
        masterSet = [table.upper() for table in masterSet]
        sourceTableSet = set(self.getSourceTables())
        for table in self.sourceTable:
            if table.upper() in masterSet:
                sourceTableSet.discard(table)
        return sourceTableSet
    
    def getStatementType(self):
        """
        Returns the type of SQL statement i.e. INSERT, UPDATE, DELETE, CREATE TABLE, CREATE VIEW etc.
        
        e.g. -
        >> sqlStr = '''
        ...insert into a_target /* target Table */
        ...(c1, c2, c3) 
        ...select b.c1, b.c2, b.c3 
        ...from ( select c1, c2 from b ) b /* Source Table b */
        ...inner join c /* Source Table c */
        ...on b.c4 = c.c4;
        ...'''
        >> sql = SQL(sqlStr)
        >> sql.getStatementType()
        INSERT
        """
        statementEvaluatorList = self._cleanSqlString.upper().split(' ')[1:3]
        statementType = statementEvaluatorList[0]
        if statementType in ('CREATE','REPLACE','DROP'):
            if statementEvaluatorList[1] in ('SET','MULTISET','VOLATILE','GLOBAL'):
                statementType = statementType + " " + "TABLE"
            else:
                statementType = statementType + " " + statementEvaluatorList[1]
        statementType = "INSERT" if statementType == "INS" else statementType
        statementType = "UPDATE" if statementType == "UPD" else statementType
        statementType = "SELECT" if statementType == "SEL" else statementType
        return statementType
    
    def getSqlKind(self):
        """
        Returnsd the Kind of SQL statement DDL, DML or DCL
        
        e.g. -
        >> sqlStr = '''
        ...insert into a_target /* target Table */
        ...(c1, c2, c3) 
        ...select b.c1, b.c2, b.c3 
        ...from ( select c1, c2 from b ) b /* Source Table b */
        ...inner join c /* Source Table c */
        ...on b.c4 = c.c4;
        ...'''
        >> sql = SQL(sqlStr)
        >> sql.getSqlKind()
        DML
        """
        if self.getStatementType().upper().startswith(('CREATE','DROP','TRUNCATE','REPLACE')):
            return "DDL"
        elif self.getStatementType().upper().startswith(('SELECT','INSERT','UPDATE','DELETE','MERGE','SEL','INS')):
            return "DML"
        elif self.getStatementType().upper().startswith(('GRANT','REVOKE')):
            return "DCL"
    
    def _extractSelectFromSQL(self):
        """
        Private method to extract Select clause from SQL
        
        e.g. -
        Input:
            insert into a_target /* target Table */
            (c1, c2, c3) 
            select b.c1, b.c2, b.c3 
            from ( select c1, c2 from b ) b /* Source Table b */
            inner join c /* Source Table c */
            on b.c4 = c.c4;
        
        Output:
            select b.c1, b.c2, b.c3 from ( select c1, c2 from b ) b inner join c on b.c4 = c.c4;
        """
        self._selectSQLOnly = ""
        createRe = re.compile(r"^ CREATE .*? (SELECT .*) \) \;", re.IGNORECASE|re.DOTALL)
        otherRe = re.compile(r"\s(SELECT .*)", re.IGNORECASE|re.DOTALL)
        tdCreateRe = re.compile(r"^ CREATE .*? (SEL .*) \) \;", re.IGNORECASE|re.DOTALL)
        tdOtherRe = re.compile(r"\s(SEL .*)", re.IGNORECASE|re.DOTALL)
        createSelectStatement = re.findall(createRe, self._cleanSqlString)
        otherSelectStatement = re.findall(otherRe, self._cleanSqlString)
        tdCreateSelectStatement = re.findall(tdCreateRe, self._cleanSqlString)
        tdOtherSelectStatement = re.findall(tdOtherRe, self._cleanSqlString)
        if len(createSelectStatement) > 0:
            self._selectSQLOnly = createSelectStatement[0]
        elif len(otherSelectStatement) > 0:
            self._selectSQLOnly = otherSelectStatement[0]
        elif len(tdCreateSelectStatement) > 0:
            self._selectSQLOnly = tdCreateSelectStatement[0]
        elif len(tdOtherSelectStatement) > 0:
            self._selectSQLOnly = tdOtherSelectStatement[0]
    
    def _generateTargetTable(self):
        """
        Private method to extract the target table from SQL by matching patterns in regular expression
        """
        tgtTables = []
        # Regular expression for Target Tables
        tgtTableReForInto = re.compile(r"(?<=^ INSERT INTO\s)[a-z0-9_$\.\&]*(?=\s)", re.IGNORECASE)
        tgtTableReForUpdate = re.compile(r"(?<=^ UPDATE\s)[a-z0-9_$\.\&]*(?=\s)", re.IGNORECASE)
        tgtTableReForDelete = re.compile(r"(?<=^ DELETE FROM\s)[a-z0-9_$\.\&]*(?=\s)", re.IGNORECASE)
        tgtTableReForReplaceView = re.compile(r"(?<=^ REPLACE VIEW\s)[a-z0-9_$\.\&]*(?=\s)", re.IGNORECASE)
        tgtTableReForCreateView = re.compile(r"(?<=^ CREATE VIEW\s)[a-z0-9_$\.\&]*(?=\s)", re.IGNORECASE)
        tgtTableReForCreateTable = re.compile(r"(?<=^ CREATE TABLE\s)[a-z0-9_$\.\&]*(?=\s|\,|\()" +
                                                r"|(?<=^ CREATE SET TABLE\s)[a-z0-9_$\.\&]*(?=\s|\,|\()" +
                                                r"|(?<=^ CREATE MULTISET TABLE\s)[a-z0-9_$\.\&]*(?=\s|\,|\()" +
                                                r"|(?<=^ CREATE VOLATILE TABLE\s)[a-z0-9_$\.\&]*(?=\s|\,|\()" +
                                                r"|(?<=^ CREATE GLOBAL TEMPORARY TABLE\s)[a-z0-9_$\.\&]*(?=\s|\,|\()" +
                                                r"|(?<=^ CREATE SET VOLATILE TABLE\s)[a-z0-9_$\.\&]*(?=\s|\,|\()" +
                                                r"|(?<=^ CREATE SET GLOBAL TEMPORARY TABLE\s)[a-z0-9_$\.\&]*(?=\s|\,|\()" +
                                                r"|(?<=^ CREATE MULTISET VOLATILE TABLE\s)[a-z0-9_$\.\&]*(?=\s|\,|\()" +
                                                r"|(?<=^ CREATE MULTISET GLOBAL TEMPORARY TABLE\s)[a-z0-9_$\.\&]*(?=\s|\,|\()", re.IGNORECASE)
        tgtTableReForDropTable = re.compile(r"(?<=^ DROP TABLE\s)[a-z0-9_$\.\&]*(?=\s)", re.IGNORECASE)
        
        # Add the list of taget tables into a list for each pattern
        tgtTables.extend(re.findall(tgtTableReForInto, self._cleanSqlString))
        tgtTables.extend(re.findall(tgtTableReForUpdate, self._cleanSqlString))
        tgtTables.extend(re.findall(tgtTableReForDelete, self._cleanSqlString))
        tgtTables.extend(re.findall(tgtTableReForReplaceView, self._cleanSqlString))
        tgtTables.extend(re.findall(tgtTableReForCreateView, self._cleanSqlString))
        tgtTables.extend(re.findall(tgtTableReForCreateTable, self._cleanSqlString))
        tgtTables.extend(re.findall(tgtTableReForDropTable, self._cleanSqlString))
        
        # If target table list has some elements, set the target table to first element
        # else set as UNKNOWN
        if len(tgtTables) > 0:
            for tgtTable in tgtTables:
                if tgtTable != "":
                    self.targetTable = tgtTable
                    break
            else:
                self.targetTable = "UNKNOWN"
        else:
            self.targetTable = "UNKNOWN"
    
    def _generateEncodedSQL(self):
        """
        This method will encode 
            paranthesis to "«" & "»"
            FROM keyword which are found in EXTRACT and TRIM functions and SUBSTRING to "↔"
        for avoiding them picked while generating tables list
        e.g. -
        Input:
            insert into a ( c1,c2,c3 ) select extract ( day from b.c1 ) as c1, b.c2, b.c3 from ( select c1,c2 from b ) b inner join c on b.c4 = c.c4;
        
        Output:
            insert into a ( c1,c2,c3 ) select extract « day ↔ b.c1 » as c1, b.c2, b.c3 from ( select c1,c2 from b ) b inner join c on b.c4 = c.c4;
        """
        
        # Encode FROM keywords whcih are used in Functions like EXTRACT, TRIM and SUBSTRING
        self._encodedSQL = self._cleanSqlString.upper().replace(" LEADING FROM "," LEADING " + self._FROMAlternative)
        leadingCharList = re.findall(r'LEADING \'.*?\' FROM', self._cleanSqlString.upper())
        for x in leadingCharList:
            self._encodedSQL = self._encodedSQL.replace(x, x.replace('FROM',self._FROMAlternative))
        self._encodedSQL = self._encodedSQL.replace(" TRAILING FROM "," TRAILING " + self._FROMAlternative)
        self._encodedSQL = self._encodedSQL.replace(" BOTH FROM "," BOTH " + self._FROMAlternative)
        self._encodedSQL = self._encodedSQL.replace("EXTRACT ( DAY FROM "," EXTRACT ( DAY " + self._FROMAlternative)
        self._encodedSQL = self._encodedSQL.replace("EXTRACT ( MONTH FROM "," EXTRACT ( MONTH " + self._FROMAlternative)
        self._encodedSQL = self._encodedSQL.replace("EXTRACT ( YEAR FROM "," EXTRACT ( YEAR " + self._FROMAlternative)
        
        # Regular expression for SUBSTRING
        substringRe = re.compile(r"SUBSTRING \( .*? FROM [0-9]*?.*? \)", re.IGNORECASE|re.DOTALL)
        substringPatterns = re.findall(substringRe, self._encodedSQL)
        for patterString in substringPatterns:
            encodedColumnString = patterString.replace('FROM ',self._FROMAlternative)
            self._encodedSQL = self._encodedSQL.replace(patterString, encodedColumnString)
        
        # Regular Expression to Find column lists in SELECT 
        selectColumnListRe = re.compile(r" SELECT .*? FROM ", re.IGNORECASE|re.DOTALL)
        selColumnListRe = re.compile(r" SEL .*? FROM ", re.IGNORECASE|re.DOTALL)
        inClauseStringRe = re.compile(r" IN \( [a-z0-9_$\'\,\-\s]*? \) ", re.IGNORECASE|re.DOTALL)
        existsClauseStringRe = re.compile(r"EXISTS \( .*? \) ", re.IGNORECASE|re.DOTALL)
        qualifyStringRe = re.compile(r"QUALIFY [a-z0-9\_]*? \([a-z0-9_$\s]*?\) OVER \([a-z0-9\_\$\,\.\&\n\s]*?\)", re.IGNORECASE|re.DOTALL)
        trimStringRe = re.compile(r"\sTRIM\s\([a-z0-9\$\,\.\s\'\-\_]*?\)",re.IGNORECASE)
        zeroStringRe = re.compile(r"\sZEROIFNULL\s\([a-z0-9\$\,\.\s\'\-\_]*?\)",re.IGNORECASE)
        
        columnStringList = re.findall(selectColumnListRe, self._encodedSQL)
        for columnString in columnStringList:
            encodedColumnString = columnString.replace("(", self._openParanthesisAlternative).replace(")", self._closeParanthesisAlternative)
            self._encodedSQL = self._encodedSQL.replace(columnString, encodedColumnString)
        
        columnStringList = re.findall(selColumnListRe, self._encodedSQL)
        for columnString in columnStringList:
            encodedColumnString = columnString.replace("(", self._openParanthesisAlternative).replace(")", self._closeParanthesisAlternative)
            self._encodedSQL = self._encodedSQL.replace(columnString, encodedColumnString)
        
        inClauseStringList = re.findall(inClauseStringRe, self._encodedSQL)
        for inClauseString in inClauseStringList:
            encodedColumnString = inClauseString.replace("(", self._openParanthesisAlternative).replace(")", self._closeParanthesisAlternative)
            self._encodedSQL = self._encodedSQL.replace(inClauseString, encodedColumnString)
        
        existsClauseStringList = re.findall(existsClauseStringRe, self._encodedSQL)
        for existsClauseString in existsClauseStringList:
            encodedColumnString = existsClauseString.replace("(", self._openParanthesisAlternative).replace(")", self._closeParanthesisAlternative)
            self._encodedSQL = self._encodedSQL.replace(existsClauseString, encodedColumnString)
        
        qualifyStringList = re.findall(qualifyStringRe, self._encodedSQL)
        for qualifyString in qualifyStringList:
            encodedColumnString = qualifyString.replace("(", self._openParanthesisAlternative).replace(")", self._closeParanthesisAlternative)
            self._encodedSQL = self._encodedSQL.replace(qualifyString, encodedColumnString)
        
        # Encode common Methods left in WHERE Clause
        trimStringList = re.findall(trimStringRe, self._encodedSQL)
        for trimString in trimStringList:
            encodedColumnString = trimString.replace("(", self._openParanthesisAlternative).replace(")", self._closeParanthesisAlternative)
            self._encodedSQL = self._encodedSQL.replace(trimString, encodedColumnString)
        
        zeroStringList = re.findall(zeroStringRe, self._encodedSQL)
        for zeroString in zeroStringList:
            encodedColumnString = zeroString.replace("(", self._openParanthesisAlternative).replace(")", self._closeParanthesisAlternative)
            self._encodedSQL = self._encodedSQL.replace(zeroString, encodedColumnString)
    
    def _generateDecodedSQL(self, sqlString):
        """
        This method returns SQL by decoding the Paranthesis and FROM keywords
        
        e.g. - 
        Input:
            insert into a ( c1,c2,c3 ) select extract « day ↔ b.c1 » as c1, b.c2, b.c3 from ( select c1,c2 from b ) b inner join c on b.c4 = c.c4;
        
        Output:
            insert into a ( c1,c2,c3 ) select extract ( day from b.c1 ) as c1, b.c2, b.c3 from ( select c1,c2 from b ) b inner join c on b.c4 = c.c4;
        """
        sqlString = sqlString.upper().replace(self._FROMAlternative, "FROM ").replace(self._openParanthesisAlternative, "(").replace(self._closeParanthesisAlternative, ")")
        return sqlString
    
    def _decomposeSQL(self):
        """
        Private method to decompose SQL String by materializing the sub queries & derived tables.
        """
        sqlString = self._encodedSQL
        self.decomposedSQL = ""
        subTablePattern = self.getTargetTable()
        #subTablePattern = random.choice('ABCDEFGHIJKLMNOPQRSTUVWXYZ') + random.choice('ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_') + random.choice('ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_') + random.choice('ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_') + random.choice('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
        self.subQueryDict = {}
        subQuery = self._getInnerSubQuery(sqlString)
        i = 1
        while subQuery != None:
             # Generate Table 
             tableName = subTablePattern + "_TEMP__SUBTABLE_" + str(i)
             #subQueryDecoded = self._getDecodedSQL(subQuery[0])
             self.subQueryDict[tableName] = subQuery[0]
             self.decomposedSQL = self.decomposedSQL + "\nCREATE VOLATILE PRD_WRK." + tableName + " AS ( \n" + subQuery[0] + "; \n) WITH DATA;"
             sqlString = sqlString.replace("(" + subQuery[1] + ")", tableName)
             subQuery = self._getInnerSubQuery(sqlString)
             i += 1
        # Add final SQL to      
        self.decomposedSQL = self.decomposedSQL + "\n\n" + sqlString + ";"
        
    def _verifyValidQueryOrSubquery(self, sqlString):
        if ((" SELECT " in sqlString.upper() or " SEL " in sqlString.upper()) and " FROM " in sqlString.upper()) or (" JOIN " in sqlString.upper() and " ON " in sqlString.upper()):
            return True
        return False
    
    def _getInnerSubQuery(self, sqlString):
        """
        Method to find inner SubQuery from the Query Submitted
        """
        # Regular Expression to fetch Inner Subquery
        subQueryRe = re.compile(r"(?<=[(])[a-z0-9\>\<\+\-\.\,\-\s\«\»\↔\→\←\$\_\*\=\'\|\/\%]*(?=[)])", re.IGNORECASE)
        outputSqls = re.findall(subQueryRe, sqlString)
        for query in outputSqls:
            if self._verifyValidQueryOrSubquery(query):
                if " SELECT " in query.upper() or " SEL " in query.upper():
                    return [query, query]
                else:
                    return ["SELECT * FROM " + query, query]
        return None
    
    def _stripMultiNewLines(self, sqlScript):
        """
        Method to strip multiple consecutive new lines from SQL
        """
        multiLineRe = re.compile(r"([\n\r]{2})(?=\n|\r)")
        cleanScript = re.sub(multiLineRe, '', sqlScript)
        return cleanScript
    
    def _generateSourceTablesSet(self):
        """
        This Function extract Source Tables from provided query and loads into a Set
        """
        # Create an empty Set for source atbles
        self.sourceTable = set()
        
        # Regular expression for getting only From Clause
        #sourceTableQueryLexerRe = re.compile(r"(FROM .*? )(WHERE |UNION |MINUS |INTERSECT |EXCEPT |GROUP BY |ORDER BY |SEL |SELECT |QUALIFY |;)",re.IGNORECASE|re.DOTALL)
        sourceTableQueryLexerRe = re.compile(r"FROM .*? ;",re.IGNORECASE|re.DOTALL)
        lexedSourceTableQuery = re.findall(sourceTableQueryLexerRe, self.decomposedSQL)
        
        # Join all From Calsues into single string
        #self.tableString = " ".join([sql[0] for sql in lexedSourceTableQuery])
        self.tableString = " ".join(lexedSourceTableQuery)
        self.tableString = " " + self.tableString
        
        # Replace Multi Spaces with a single Space
        multiSpaceRe = re.compile(r"( )(?= )")
        self.tableString = re.sub(multiSpaceRe, '', self.tableString)
        
        # Strip column Aliases
        columnAliasRe = re.compile(r"(?<= \( )([a-z0-9\$\,\_]*?)(?= \)) ", re.IGNORECASE)
        self.tableString = re.sub(columnAliasRe, '__', self.tableString)
        
        tableNameRe = re.compile(r"( FROM | JOIN |\,)([a-z0-9\$\.\_]*?)(?=\s)", re.IGNORECASE)
        tableList = re.findall(tableNameRe, self.tableString)
        if tableList:
            for tableGroup in tableList:
                if tableGroup[1] != "" and "_TEMP__SUBTABLE_" not in tableGroup[1]:
                    self.sourceTable.add(tableGroup[1])

    def getFormatedCleanSql(self, sql=None):
        """Re-format SQL for Output"""
        if not sql:
            self._generateEncodedSQL()
        newLineRe = re.compile(r" SELECT | FROM | WHERE | JOIN | AND | ORDER BY | GROUP BY | HAVING ", re.IGNORECASE)
        findKeywords = re.findall(newLineRe, self._encodedSQL)
        sql = self._encodedSQL
        for key in set(findKeywords):
            sql = sql.replace(key, '\n' + key)
        sql = re.sub(r',', '\n,', sql)
        return self._generateDecodedSQL(sql)

    def checkSqlIsOne2One(self):
        """Check if SQL is straigt one to one"""
        if self.getStatementType() == "CREATE TABLE":
            one2oneSqlRe1 = re.compile(r"\s*SELECT \* FROM [a-z0-9\_\&\.]*? ;\s*", re.IGNORECASE)
            #one2oneSqlRe1 = re.compile(r"[a-zA-Z0-9\_\&\. \(\=\)\']*? SELECT \* FROM [a-z0-9\_\&\.]*?;", re.IGNORECASE)
            one2oneSqlRe2 = re.compile(r"\s*SELECT \* FROM [a-z0-9\_\&\.]*? \) WITH DATA.*? ;\s*", re.IGNORECASE)
            return re.match(one2oneSqlRe1, self.getSelectFromSQL()) != None or re.match(one2oneSqlRe2, self.getSelectFromSQL()) != None
        else:
            return False
    
class SQLScript():
    """
    Create an object of Type SQLScript where 1 script can contain multiple SQLs
    
    Syntax - sqlscript = SQLScript(sqlCodeOrFile, codeTypeFlag)
            sqlCodeOrFile can be a full string containg whole SQL Script or a file name of SQLScript
            codeTypeFlag tells what sqlCodeOrFile is -
                codeTypeFlag = 's' for String
                codeTypeFlag = 'f' for File name
    """
    sqlDelimiter = ';'
    def __init__(self, scriptOrFile, flag):
        if flag == 's' and scriptOrFile != "":
            self.sqlString = scriptOrFile
        elif flag == 'f' and os.path.isfile(scriptOrFile):
            fp = open(scriptOrFile,'r')
            self.sqlString = fp.read()
            fp.close()
        else:
            raise ValueError("Insufficient Information for Type SQLScript")
        self._cleanSQLScript = self._bteqCleanCode()
        self._cleanSQLScript = cleanCode(self._cleanSQLScript)
    
    def _splitSQLCode(self):
        """
        Private method to split the SQLScript into multiple SQLs by delimiting on ';'
        """
        self.splitSQLList = self._cleanSQLScript.split(self.sqlDelimiter)
        self.splitSQLList = [ SQL(brokenSQL + ";") for brokenSQL in self.splitSQLList if brokenSQL.replace(" ","") != "" ]
    
    def _bteqCleanCode(self):
        """
        Clean BTEQ script specific code i.e. anuy command starting with "."
        """
        btqCmdRe1 = re.compile(r"[\n\r]\s*\..*?(?=[\n\r])", re.IGNORECASE|re.DOTALL)
        btqCmdRe2 = re.compile(r"^\s*\..*?(?=[\n\r])", re.IGNORECASE|re.DOTALL)
        return re.sub(btqCmdRe2, '', re.sub(btqCmdRe1, '', self.sqlString))
    
    def getSplitSql(self):
        """
        Splits the SQL script into individual SQLs and returns a list of SQL objects
        """
        self._splitSQLCode()
        return self.splitSQLList
    
    def getSourceTargetMapping(self):
        """
        Returns a List of dictionaries containing Source And Target Rows
        """
        mapSrcTgt = []
        for sql in self.getSplitSql():
            if sql.getTargetTable() != "UNKNOWN" or len(sql.getSourceTables()) > 0:
                mapping = {}
                mapping['src'] = sql.getSourceTables()
                mapping['tgt'] = sql.getTargetTable()
                mapSrcTgt.append(mapping)
        return mapSrcTgt
    
if __name__ == "__main__":
    # The module is to be imported by other Process. 
    print("Nothing to do here!!! ☻")