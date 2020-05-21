# --*-- code : utf8 --*--
import re
from collections import OrderedDict


class SQL:
    """SQl Class for SQL objects"""

    def __init__(self, raw_sql=''):
        self.sql = raw_sql

    @property
    def sql(self):
        return self._raw_sql

    @sql.setter
    def sql(self, raw_sql):
        self._raw_sql = raw_sql

        # Empty Dictionary. This will be populated later
        try:
            del(self._encoder_dict)
        except:
            pass
        self._encoder_dict = OrderedDict()

        # Call Clean SQL method to generate clean SQL String
        self.__clean_encode_sql()

    def __str__(self):

        # Return Decoded re-formated SQL
        return f"Clean SQL: {self.clean_sql}\n\nClean Encoded SQL: {self._clean_encoded_sql}\n\nEncoder Dictionary: {self.__get_encoder_dict()}"

    def __get_encoder_dict(self):
        return "\n".join([f"{key} : {value}" for key, value in self._encoder_dict.items()])

    def __decode_sql(self, input_sql):
        # Initialize Decoded SQL with clean SQL
        decoded_sql = input_sql

        for key in reversed(self._encoder_dict.keys()):
            temp_sql = decoded_sql.replace(key, self._encoder_dict[key])
            decoded_sql = temp_sql
        return decoded_sql

    def __is_valid_function(self, function_string):
        return not (" SELECT " in function_string or " JOIN " in function_string)

    def __encode_column_list(self):
        """Encode case and functions from main Clean SQL Query"""
        functions_list = re.compile(r"(?<=\s)[a-z0-9\_\.]+ \( .*? \)", re.I)\
            .findall(self._clean_encoded_sql)
        if len(functions_list) > 0:
            for index, func in enumerate(functions_list, start=1):
                if self.__is_valid_function(func):
                    # Generate Key for function encoding
                    key = '&&-FUNCTION' + str(index) + '-&&'
                    self._encoder_dict[key] = func

                    # Encode the Function
                    func_encoded_sql = self._clean_encoded_sql.replace(
                        func, key)
                    self._clean_encoded_sql = func_encoded_sql

        # Find all column lists between SELECT and FROM Keywords
        select_from_list = re.compile(r"SELECT (.*?) FROM", re.I | re.DOTALL)\
            .findall(self._clean_encoded_sql)

        if len(select_from_list) > 0:
            for index, col_list_str in enumerate(select_from_list, start=1):
                key = '&&-COLUMNLIST' + str(index) + '-&&'

                # Add to dictionary
                self._encoder_dict[key] = col_list_str

                # Encode
                encoded_col_sql = self._clean_encoded_sql.replace(
                    col_list_str, key)
                self._clean_encoded_sql = encoded_col_sql

    def __clean_encode_sql(self):
        """Clean SQL string by removing multiple spaces and new lines and comments"""

        # Regular expression for Single quoted strings with single quote as escape character
        single_quoted_string = re.compile(r"('(?:''|[^'])*')")

        # Regular expression for multi line comments using /* */ method
        multi_line_comment = re.compile(r"/\*[^\+]\s*(.*?)\*/", re.DOTALL)

        # Regular expression for single line comment using -- method
        single_line_comment = re.compile(r"(\-\-.*?)[\r\n]")

        # Regular expression for multiple consecutive spaces
        multiple_space = re.compile(r"\s\s+")

        # Initialize clean sql with raw sql
        self._clean_encoded_sql = self._raw_sql

        # Find and encode single quoted strings to preserve the content of literals
        single_quoted_substr = single_quoted_string.findall(
            self._clean_encoded_sql)

        # Generate Translation Dictionary for SQL string in single quote
        if len(single_quoted_substr) > 0:

            for index, sub_str in enumerate(single_quoted_substr, start=1):
                # Generate keys
                key = '&&-LITERAL' + str(index) + '-&&'
                self._encoder_dict[key] = sub_str

                # Enocode substrings in SQL
                literal_enc_sql = self._clean_encoded_sql.replace(sub_str, key)
                self._clean_encoded_sql = literal_enc_sql

        # Remove multi line comments from SQl String
        no_mlc_sql = multi_line_comment.sub('', self._clean_encoded_sql)
        self._clean_encoded_sql = no_mlc_sql

        # Remove Single Line Comments from SQL
        no_slc_sql = single_line_comment.sub('', self._clean_encoded_sql)
        self._clean_encoded_sql = no_slc_sql

        # Replace all New Line characters and tabs with space
        no_crlf_sql = self._clean_encoded_sql.replace(
            '\n', ' ').replace('\r', ' ').replace('\t', '')
        self._clean_encoded_sql = no_crlf_sql

        # Remove Leading and trailing Spaces
        no_ltspace_sql = self._clean_encoded_sql.strip()
        self._clean_encoded_sql = no_ltspace_sql

        # Add spaces near paranthesis for standardisation
        paranthesis_sapce_sql = self._clean_encoded_sql.replace(
            '(', ' ( ').replace(')', ' ) ')
        self._clean_encoded_sql = paranthesis_sapce_sql

        # Add Spaces near commas ',' for standardisation
        comma_space_sql = self._clean_encoded_sql.replace(',', ' , ')
        self._clean_encoded_sql = comma_space_sql

        # Remove Multiple consecutive spaces
        no_mulspace_sql = multiple_space.sub(' ', self._clean_encoded_sql)
        self._clean_encoded_sql = no_mulspace_sql

        # Capitalise the whole SQL
        capital_sql = self._clean_encoded_sql.upper()
        self._clean_encoded_sql = capital_sql

        # Remove space around period (.)
        np_periodspace_sql = self._clean_encoded_sql.replace(
            ' . ', '.').replace(' .', '.').replace('. ', '.')
        self._clean_encoded_sql = np_periodspace_sql

        # Encode the Functions
        self.__encode_column_list()

    def __reformat_encoded_sql(self):
        # Initialize Reformatted SQL
        reformated_clean_encoded_sql = self._clean_encoded_sql

        new_line_keywords = re.compile(r"^SELECT |^SEL | SELECT | SEL | FROM | JOIN | BY | \( | \)")\
            .findall(self._clean_encoded_sql)

        for sql_keyword in set(new_line_keywords):
            reformatted_sql = reformated_clean_encoded_sql.replace(
                sql_keyword, sql_keyword + '\n')
            reformated_clean_encoded_sql = reformatted_sql

        return reformated_clean_encoded_sql

    @property
    def clean_sql(self):
        clean_sql = self.__decode_sql(self.__reformat_encoded_sql())
        return clean_sql

    @property
    def target_table(self):
        target_table = None
        # ToDo Generate target Table Name
        return target_table

    @property
    def source_tables(self):
        source_tables = set([])
        # ToDo Generate source Tables List
        return source_tables

    @property
    def sql_type(self):
        sql_type = self._clean_encoded_sql.split(' ')[:5] \
            if len(self._clean_encoded_sql.split(' ')) > 5 \
            else self._clean_encoded_sql.split(' ')

        # Translate Acronyms in Teradata
        if sql_type[0] in ('INS', 'INSERT'):
            return 'INSERT'
        elif sql_type[0] in ('SEL', 'SELECT', 'WITH'):
            return 'SELECT'
        elif sql_type[0] in ('DEL', 'DELETE'):
            return 'DELETE'
        elif sql_type[0] in ('UPD', 'UPDATE'):
            return 'UPDATE'
        elif sql_type[0] in ('CREATE', 'REPLACE') and 'TABLE' in sql_type:
            return 'CREATE TABLE'
        elif sql_type[0] in ('CREATE', 'REPLACE') and 'VIEW' in sql_type:
            return 'CREATE VIEW'
        elif sql_type[0] in ('CREATE', 'REPLACE') and 'MACRO' in sql_type:
            return 'CREATE MACRO'
        elif sql_type[0] in ('CREATE', 'REPLACE') and 'PROCEDURE' in sql_type:
            return 'CREATE PROCEDURE'
        elif sql_type[0] in ('CREATE', 'REPLACE') and 'FUNCTION' in sql_type:
            return 'CREATE FUNCTION'
        elif sql_type[0] == 'DROP' and 'TABLE' in sql_type:
            return 'DROP TABLE'
        elif sql_type[0] == 'DROP' and 'VIEW' in sql_type:
            return 'DROP VIEW'
        elif sql_type[0] == 'DROP' and 'MACRO' in sql_type:
            return 'DROP MACRO'
        elif sql_type[0] == 'DROP' and 'PROCEDURE' in sql_type:
            return 'DROP PROCEDURE'
        elif sql_type[0] == 'DROP' and 'FUNCTION' in sql_type:
            return 'DROP FUNCTION'
        elif sql_type[0] == 'ALTER' and 'TABLE' in sql_type:
            return 'ALTER TABLE'
        elif sql_type[0] in ('EXEC', 'EXECUTE'):
            return 'EXECUTE MACRO'
        elif sql_type[0] == 'CALL':
            return 'CALL PROCEDURE'
        elif sql_type[0] in ('GRANT', 'REVOKE', 'MERGE'):
            return sql_type[0]
        else:
            return None

    @property
    def sql_lang(self):
        if self.sql_type in ('SELECT', 'INSERT', 'UPDATE', 'DELETE', 'MERGE'):
            return 'DML'
        elif self.sql_type in ('CREATE TABLE', 'CREATE VIEW', 'CREATE MACRO', 'CREATE PROCEDURE',
                               'CREATE FUNCTION', 'DROP TABLE', 'DROP VIEW', 'DROP MACRO',
                               'DROP PROCEDURE', 'DROP FUNCTION', 'ALTER TABLE'):
            return 'DDL'
        elif self.sql_type in ('GRANT', 'REVOKE'):
            return 'DCL'
        return None

    @property
    def select_sql(self):
        select_sql = None
        # ToDo Extract Select from SQL
        # And Format
        return select_sql
