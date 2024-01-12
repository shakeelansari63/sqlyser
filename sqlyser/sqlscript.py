# -*- coding: utf-8 -*-
"""
SQL Analyser Module to break SQLs into smaller pieces and analyse code.

Developer: Shakeel Ansari
"""
import re
import os
from .sql import SQL
from .cleanse import cleanCode
from typing import List, Optional
from dataclasses import dataclass


@dataclass
class SrcTgtMapping:
    src: List[str]
    tgt: Optional[str]


class SQLScript:
    """
    Create an object of Type SQLScript where 1 script can contain multiple SQLs

    Syntax - sqlscript = SQLScript(sqlCodeOrFile, codeTypeFlag)
            sqlCodeOrFile can be a full string containg whole SQL Script or a file name of SQLScript
            codeTypeFlag tells what sqlCodeOrFile is -
                codeTypeFlag = 's' for String
                codeTypeFlag = 'f' for File name
    """

    _sql_delimiter = ";"

    def __init__(self, scriptOrFile, flag):
        if flag == "s" and scriptOrFile != "":
            self.sqlString = scriptOrFile
        elif flag == "f" and os.path.isfile(scriptOrFile):
            fp = open(scriptOrFile, "r")
            self.sqlString = fp.read()
            fp.close()
        else:
            raise ValueError("Insufficient Information for Type SQLScript")
        self._clean_sql_script = self._clean_teradata_bteq()
        self._clean_sql_script = cleanCode(self._clean_sql_script)

    def _split_sql_code(self) -> None:
        """
        Private method to split the SQLScript into multiple SQLs by delimiting on ';'
        """
        _split_sql: List[str] = self._clean_sql_script.split(self._sql_delimiter)
        self.split_sql_list = [
            SQL(_broken_sql + ";")
            for _broken_sql in _split_sql
            if _broken_sql.replace(" ", "") != ""
        ]

    def _clean_teradata_bteq(self):
        """
        Clean BTEQ script specific code i.e. anuy command starting with "."
        """
        _bteq_cmd_re1 = re.compile(
            r"[\n\r]\s*\..*?(?=[\n\r])", re.IGNORECASE | re.DOTALL
        )
        _bteq_cmd_re2 = re.compile(r"^\s*\..*?(?=[\n\r])", re.IGNORECASE | re.DOTALL)
        return re.sub(_bteq_cmd_re2, "", re.sub(_bteq_cmd_re1, "", self.sqlString))

    def get_split_sql(self) -> List[SQL]:
        """
        Splits the SQL script into individual SQLs and returns a list of SQL objects
        """
        self._split_sql_code()
        return self.split_sql_list

    def get_lineage_mapping(self) -> List[SrcTgtMapping]:
        """
        Returns a List of dictionaries containing Source And Target Rows
        """
        map_src_tgt = []
        for sql in self.get_split_sql():
            if sql.target_table not in ("UNKNOWN", None) or len(sql.source_tables) > 0:
                mapping = SrcTgtMapping(src=sql.source_tables, tgt=sql.target_table)
                map_src_tgt.append(mapping)

        return map_src_tgt
