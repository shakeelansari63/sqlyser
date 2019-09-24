# -*- coding: utf-8 -*-
"""
SQL Analyser Module to break SQLs into smaller pieces and analyse code.

Developer: Shakeel Ansari
"""
from .cleanse import cleanCode
import re
import os
from .sql import SQL
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