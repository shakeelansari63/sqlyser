# --*-- code : utf8 --*--
import re
from collections import OrderedDict


class SQL:
    """SQl Class for SQL objects"""
    # Constant
    TBL_PATTERN = r'([a-z0-9\$\#\"\&\.\-\_]*) '
    LIT_TEMPLATE = '&&-LITERAL{idx}-&&'
    LIT_TEMPLATE = '&&-LITERAL{idx}-&&'
    FNC_TEMPLATE = '&&-FUNCTION{idx}-&&'
    COL_TEMPLATE = '&&-COLUMNLIST{idx}-&&'
    GRP_TEMPLATE = '&&-GROUPLIST{idx}-&&'
    ORD_TEMPLATE = '&&-ORDERLIST{idx}-&&'
    HAV_TEMPLATE = '&&-HAVINGLIST{idx}-&&'
    FORMAT_NEWLINE_KEYWORD = r"^SELECT |^SEL | SELECT | SEL | FROM | INNER\s+JOIN " + \
        r"| LEFT[OUTER\s]+JOIN | RIGHT[OUTER\s]+JOIN | FULL[OUTER\s]+JOIN " + \
        r"| CROSS\s+JOIN | JOIN | GROUP BY | ORDER BY | ON | WHERE " + \
        r"| AND | HAVING | \( | \) | , "

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
        return "Clean SQL: {cleansql}\n\n".format(cleansql=self.clean_sql) + \
            "Clean Encoded SQL: {enc_sql}\n\n".format(enc_sql=self._clean_encoded_sql) + \
            "Encoder Dict: {enc_dict}".format(
                enc_dict=self.__get_encoder_dict())

    def __get_encoder_dict(self):
        return "\n".join(["{key} : {value}".format(key=key, value=value) for key, value in self._encoder_dict.items()])

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
                    key = self.FNC_TEMPLATE.format(idx=str(index))
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
                key = self.COL_TEMPLATE.format(idx=str(index))

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
                key = self.LIT_TEMPLATE.format(idx=str(index))
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

    def reformat(self, sql):
        # Initialize Reformatted SQL
        reformated_sql = sql

        new_line_keywords = re.compile(
            self.FORMAT_NEWLINE_KEYWORD).findall(sql)

        for sql_keyword in set(new_line_keywords):
            reformatted_sql = reformated_sql.replace(
                sql_keyword, '\n' + sql_keyword)
            reformated_sql = reformatted_sql

        return reformated_sql

    @property
    def clean_sql(self):
        clean_sql = self.__decode_sql(
            self.reformat(self._clean_encoded_sql))
        return clean_sql

    @property
    def target_table(self):
        target_table = []

        sql_type = self.sql_type

        # Begining of valid SQL
        if sql_type in ('CREATE TABLE', 'REPLACE TABLE'):
            table_name_re = [re.compile(r'^CREATE .*?TABLE ' + self.TBL_PATTERN, re.I),
                             re.compile(r'^REPLACE .*?TABLE ' + self.TBL_PATTERN, re.I)]

        elif sql_type == 'INSERT':
            table_name_re = [re.compile(r'INSERT INTO ' + self.TBL_PATTERN, re.I),
                             re.compile(r'INS INTO ' + self.TBL_PATTERN, re.I)]

        elif sql_type == 'UPDATE':
            table_name_re = [re.compile(r'^UPDATE ' + self.TBL_PATTERN, re.I),
                             re.compile(r'^UPD ' + self.TBL_PATTERN, re.I)]

        elif sql_type == 'DELETE':
            table_name_re = [re.compile(r'^DELETE ' + self.TBL_PATTERN, re.I),
                             re.compile(r'^DEL ' + self.TBL_PATTERN, re.I)]

        elif sql_type == 'MERGE':
            table_name_re = [re.compile(
                r'^MERGE INTO ' + self.TBL_PATTERN, re.I)]

        elif sql_type == 'EXECUTE MACRO':
            table_name_re = [re.compile(r'^EXECUTE ' + self.TBL_PATTERN, re.I),
                             re.compile(r'^EXEC ' + self.TBL_PATTERN, re.I)]

        elif sql_type == 'CALL PROCEDURE':
            table_name_re = [re.compile(r'^CALL ' + self.TBL_PATTERN, re.I)]

        elif sql_type != 'SELECT':
            table_name_re = [re.compile(
                r'^' + sql_type + ' ' + self.TBL_PATTERN, re.I)]

        else:
            return None

        for tbl_regexp in table_name_re:
            target_table.extend(re.findall(
                tbl_regexp, self._clean_encoded_sql))

        if target_table:
            return target_table[0].strip()
        else:
            return None

    @property
    def source_tables(self):
        source_tables = []

        # Regular expressions for Source Tables
        table_name_re = [
            re.compile(r' FROM ' + self.TBL_PATTERN, re.I),
            re.compile(r' JOIN ' + self.TBL_PATTERN, re.I),
            re.compile(r' , ' + self.TBL_PATTERN, re.I),
            re.compile(r' USING ' + self.TBL_PATTERN, re.I)
        ]

        # Find tables matching the regular expression
        for tbl_regexp in table_name_re:
            source_tables.extend(re.findall(
                tbl_regexp, self._clean_encoded_sql))

        # Return List of unique tables
        return list(set(source_tables))

    @property
    def sql_type(self):
        sql_type = self._clean_encoded_sql.split(' ')[:5] \
            if len(self._clean_encoded_sql.split(' ')) > 5 \
            else self._clean_encoded_sql.split(' ')

        # Translate Acronyms in Teradata
        if sql_type[0] == 'INS':
            return 'INSERT'

        elif sql_type[0] in ('SEL', 'WITH'):
            return 'SELECT'

        elif sql_type[0] == 'DEL':
            return 'DELETE'

        elif sql_type[0] == 'UPD':
            return 'UPDATE'

        elif sql_type[0] in ('EXEC', 'EXECUTE'):
            return 'EXECUTE MACRO'

        elif sql_type[0] == 'CALL':
            return 'CALL PROCEDURE'

        elif sql_type[0] in ('CREATE', 'REPLACE') and 'TABLE' in sql_type:
            return '{sql_tp} TABLE'.format(sql_tp=sql_type[0])

        elif sql_type[0] in ('CREATE', 'REPLACE', 'DROP', 'ALTER'):
            return '{sql_tp1} {sql_tp2}'.format(sql_tp1=sql_type[0], sql_tp2=sql_type[1])

        else:
            return sql_type[0]

    @property
    def sql_lang(self):
        sql_type = self.sql_type.split(' ')[0]

        if sql_type in ('SELECT', 'INSERT', 'UPDATE', 'DELETE', 'MERGE'):
            return 'DML'
        elif sql_type in ('CREATE', 'DROP', 'REPLACE', 'ALTER'):
            return 'DDL'
        elif sql_type in ('GRANT', 'REVOKE'):
            return 'DCL'
        return None

    @property
    def select_sql(self):
        select_sql = None
        # ToDo Extract Select from SQL
        # And Format
        return select_sql
