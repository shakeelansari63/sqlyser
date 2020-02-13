# --*-- code : utf8 --*--
import re

class SQL:
    """SQl Class for SQL objects"""

    def __init__(self, raw_sql):
        self.raw_sql = raw_sql

        # Call Clean SQL method to generate clean SQL String
        self.__clean_encode_sql()
    
    def __str__(self):
        #return self.__decode_sql()
        return self.clean_encoded_sql

    def __decode_sql(self):
        # Initialize Decoded SQL with clean SQL
        decoded_sql = self.clean_encoded_sql

        for key in self.encoder_dict:
            temp_sql = decoded_sql.replace(key, self.encoder_dict[key])
            decoded_sql = temp_sql
        return decoded_sql

    def __clean_encode_sql(self):
        """Clean SQL string by removing multiple spaces and new lines and comments"""

        # Regular expression for Single quoted strings with single quote as escape character
        single_quoted_string = re.compile(r"('(?:''|[^'])*')")

        # Regular expression for multi line comments using /* */ method
        multi_line_comment = re.compile(r"/\*[^\+]\s*(.*?)\*/",re.DOTALL)

        # Regular expression for single line comment using -- method
        single_line_comment = re.compile(r"(\-\-.*?)[\r\n]")

        # Regular expression for multiple consecutive spaces
        multiple_space = re.compile(r"\s\s+")

        # Initialize clean sql with raw sql
        self.clean_encoded_sql = self.raw_sql

        # Find and encode single quoted strings to preserve the content of literals
        single_quoted_substr = single_quoted_string.findall(self.clean_encoded_sql)

        # Generate Translation Dictionary for SQL string in single quote
        if len(single_quoted_substr) > 0:
            # Empty Dictionary
            self.encoder_dict = dict()

            for index, sub_str in enumerate(single_quoted_substr):
                # Generate keys
                key = '&&-' + str(index) + '-&&'
                self.encoder_dict[key] = sub_str
                
                # Enocode substrings in SQL
                literal_enc_sql = self.clean_encoded_sql.replace(sub_str, key)
                self.clean_encoded_sql = literal_enc_sql

        # Remove multi line comments from SQl String
        no_mlc_sql = multi_line_comment.sub('', self.clean_encoded_sql)
        self.clean_encoded_sql = no_mlc_sql

        # Remove Single Line Comments from SQL
        no_slc_sql = single_line_comment.sub('', self.clean_encoded_sql)
        self.clean_encoded_sql = no_slc_sql

        # Replace all New Line characters and tabs with space
        no_crlf_sql = self.clean_encoded_sql.replace('\n',' ').replace('\r',' ').replace('\t','')
        self.clean_encoded_sql = no_crlf_sql

        # Remove Leading and trailing Spaces
        no_ltspace_sql = self.clean_encoded_sql.strip()
        self.clean_encoded_sql = no_ltspace_sql

        # Remove Multiple consecutive spaces
        no_mulspace_sql = multiple_space.sub(' ', self.clean_encoded_sql)
        self.clean_encoded_sql = no_mulspace_sql

