#include<iostream>
#include<vector>
#include<string>
#include<cctype>
#include<map>
#include<fstream>
#include<map>
using namespace std;

enum TokenType { 
    T_INT, T_STR, T_ID, T_DBL, T_DOUBLE_VAL,
    T_NUM, T_STRING, T_IF, T_ELSE, T_RETURN, T_FUNC,
    T_ASSIGN, T_PLUS, T_MINUS, T_MUL, T_DIV, T_WHILE, T_FOR,
    T_LPAREN, T_RPAREN, T_LBRACE, T_RBRACE,
    T_SEMICOLON, T_COMMA, T_PRINT,
    T_GT, T_LT, T_GTE, T_LTE, T_EQ, T_NEQ, T_AND, T_OR, 
    T_EOF
};

string tokenTypeToString(TokenType type) {
    switch (type) {
        case T_INT: return "int";
        case T_STR: return "str";
        case T_ID: return "id";
        case T_DOUBLE_VAL: return "Double Value";
        case T_NUM: return "num";
        case T_STRING: return "string";
        case T_IF: return "if";
        case T_ELSE: return "else";
        case T_RETURN: return "return";
        case T_WHILE: return "while";
        case T_FUNC: return "function";
        case T_DBL: return "double";
        case T_ASSIGN: return "=";
        case T_PLUS: return "+";
        case T_MINUS: return "-";
        case T_MUL: return "*";
        case T_DIV: return "/";
        case T_FOR: return "for ";
        case T_LPAREN: return "(";
        case T_RPAREN: return ")";
        case T_LBRACE: return "{";
        case T_RBRACE: return "}";
        case T_COMMA: return ",";
        case T_PRINT: return "print";
        case T_SEMICOLON: return ";";
        case T_GT: return ">";
        case T_LT: return "<";
        case T_GTE: return ">=";
        case T_LTE: return "<=";
        case T_EQ: return "==";
        case T_NEQ: return "!=";
        case T_AND: return "&&";
        case T_OR: return "||";
        case T_EOF: return "eof";
        default: return "UNKNOWN_TOKEN";
    }
}

bool isTypeCompatible(TokenType varType, TokenType valueType) {
    return varType == valueType;
}

struct Token {
    TokenType type;
    string value;
    int line;

    Token(TokenType type, const string& value, int line)
        : type(type), value(value), line(line) {}
};

class Lexer {
    private:
        string src;
        size_t pos;
        int line;
    public:
        Lexer(const string& src) {
            this->src = src;
            this->pos = 0;
            this->line = 1;
        }
        string consumeNumber() {
            /*
                Consume decimal and integers from the code.
            */
            size_t start = pos;
            bool hasDecimal = false;

            while (pos < src.size() && (isdigit(src[pos]) || src[pos] == '.')) {
                if (src[pos] == '.') {
                    if (hasDecimal) break;    
                    hasDecimal = true;
                }
                pos++;
            }

            return src.substr(start, pos - start);
        }

        string consumeWord() {
            /*
                Consume AlphaNumeric words from the code.
            */
            size_t start = pos;
            while (pos < src.size() && isalnum(src[pos])) pos++;
            return src.substr(start, pos - start);
        }

        string consumeString() {
            /*
                Consume string literals. Literals are the words that start & end with "".
            */
            pos++;  
            size_t start = pos;
            while (pos < src.size() && src[pos] != '"') pos++;
            string strLiteral = src.substr(start, pos - start);
            pos++;  
            return strLiteral;
        }

        vector<Token> tokenize() {
            /*
                The tokenize function parses the code into tokens, and returns
                a vector of tokens.
            */
            vector<Token> tokens;

            while (pos < src.size()) {
                char current = src[pos];

                if (isspace(current)) {
                    if (current == '\n') line++;
                    pos++;
                    continue;
                }

                // Check if the current line is a comment.
                if (current == '#') {
                    while (pos < src.size() && src[pos] != '\n') {
                        pos++;
                    }
                    continue;
                }
                // check if the current word is a number?
                if (isdigit(current) || (current == '.' && isdigit(src[pos+1]))) {
                    string number = consumeNumber();
                    
                    if (number.find('.') != std::string::npos) {
                          
                        tokens.emplace_back(T_DOUBLE_VAL, number, line);
                    } else {
                          
                        tokens.emplace_back(T_NUM, number, line);
                    }
                    continue;
                }

                // check if the current word is either a reserved word or value.
                if (isalpha(current)) {
                    string word = consumeWord();

                    if (word == "int") tokens.emplace_back(T_INT, word, line);
                    else if (word == "str") tokens.emplace_back(T_STR, word, line);
                    else if (word == "if") tokens.emplace_back(T_IF, word, line);
                    else if (word == "else") tokens.emplace_back(T_ELSE, word, line);
                    else if (word == "sending") tokens.emplace_back(T_RETURN, word, line);
                    else if (word == "til") tokens.emplace_back(T_WHILE, word, line);
                    else if (word == "dbl") tokens.emplace_back(T_DBL, word, line);
                    else if (word == "vibe") tokens.emplace_back(T_FUNC, word, line);
                    else if (word == "dbl") tokens.emplace_back(T_DBL, word, line);
                    else if (word == "fore") tokens.emplace_back(T_FOR, word, line);// for loop has construct jabtk
                    else if (word == "yap") tokens.emplace_back(T_PRINT, word, line); // print statement
                    else tokens.emplace_back(T_ID, word, line);

                    continue;
                }

                // string literals
                if (current == '"') {
                    tokens.emplace_back(T_STRING, consumeString(), line);
                    continue;
                }

                // special characters
                switch (current) {
                    case '=': 
                        if (src[pos + 1] == '=') { tokens.emplace_back(T_EQ, "==", line); pos++; }
                        else { tokens.emplace_back(T_ASSIGN, string(1, current), line); }
                        break;
                    case '+': tokens.emplace_back(T_PLUS, string(1, current), line); break;
                    case '-': tokens.emplace_back(T_MINUS, string(1, current), line); break;
                    case '*': tokens.emplace_back(T_MUL, string(1, current), line); break;
                    case '/': tokens.emplace_back(T_DIV, string(1, current), line); break;
                    case '(': tokens.emplace_back(T_LPAREN, string(1, current), line); break;
                    case ')': tokens.emplace_back(T_RPAREN, string(1, current), line); break;
                    case ',': tokens.emplace_back(T_COMMA, string(1, current), line); break;
                    case '{': tokens.emplace_back(T_LBRACE, string(1, current), line); break;
                    case '}': tokens.emplace_back(T_RBRACE, string(1, current), line); break;
                    case ';': tokens.emplace_back(T_SEMICOLON, string(1, current), line); break;
                    case '>': 
                        if (src[pos + 1] == '=') { tokens.emplace_back(T_GTE, ">=", line); pos++; }
                        else { tokens.emplace_back(T_GT, string(1, current), line); }
                        break;
                    case '<': 
                        if (src[pos + 1] == '=') { tokens.emplace_back(T_LTE, "<=", line); pos++; }
                        else { tokens.emplace_back(T_LT, string(1, current), line); }
                        break;
                    case '!': 
                        if (src[pos + 1] == '=') { tokens.emplace_back(T_NEQ, "!=", line); pos++; }
                        else {
                            cout << "Unexpected character " << current << " at line " << line << endl;
                            exit(1);
                        }
                        break;
                    case '&':
                        if (src[pos + 1] == '&') { tokens.emplace_back(T_AND, "&&", line); pos++; }
                        else {
                            cout << "Unexpected character " << current << " at line " << line << endl;
                            exit(1);
                        }
                        break;
                    case '|':
                        if (src[pos + 1] == '|') { tokens.emplace_back(T_OR, "||", line); pos++; }
                        else {
                            cout << "Unexpected character " << current << " at line " << line << endl;
                            exit(1);
                        }
                        break;
                    default: 
                        cout << "Unexpected character " << current << " at line " << line << endl; 
                        exit(1);
                }
                pos++;
            }
            tokens.emplace_back(T_EOF, "", line);
            return tokens;
        }
};

class Parser {
    private:
        vector<Token> tokens;
        size_t pos;
        map<string, TokenType> symbolTable;

        void parseStatement() {
            /*
                The statement is the main executor of the parser. We need to check if the
                token is a declaration, assignment, print, if-else block, while loop, for loop,
                expression, or a block of code.
            */
            if (tokens[pos].type == T_INT || tokens[pos].type == T_STR || tokens[pos].type == T_DBL) {
                parseDeclaration(); 
            } else if (tokens[pos].type == T_ID) {
                parseAssignment(); 
            } else if (tokens[pos].type == T_IF) {
                parseIfStatement();
            } else if (tokens[pos].type == T_WHILE) {
                parseWhileLoop();
            } else if(tokens[pos].type == T_FOR) {
                parseForLoop();
            } else if (tokens[pos].type == T_RETURN) {
                parseReturnStatement();
            } else if (tokens[pos].type == T_FUNC) {
                parseFunction();
            } else if (tokens[pos].type == T_LBRACE) {
                parseBlock();
            } else if (tokens[pos].type == T_PRINT) {
                parsePrintStatement();
            }else {
                cout << "Syntax error: Unexpected token: " << tokens[pos].value
                    << " on line: "<< tokens[pos].line << endl;
                    exit(1);
            }
        }

        void expect(TokenType type) {
            /*
                The expect function is the main parser for the code. An approach for parsing is 
                building a syntax tree, but that is unnecessary, when we can do parsing in O(n)
                operation by simply checking the logic of how tokens appear.
            */
            if (tokens[pos].type != type) {
                cout << "Syntax Error: expected -> " << tokenTypeToString(type) << " but found: "
                     << tokenTypeToString(tokens[pos].type) << " on Line: " << tokens[pos].line << "."<< endl;
                exit(1);
            }
            pos++;
        }


        void parsePrintStatement() {
            // yap "something" ;
            expect(T_PRINT);
            expect(T_STRING);
            expect(T_SEMICOLON);
        }

        void parseBlock() {
            /*
                {
                    Statements here. Recursively, they can be blocks too.
                }
            */
            expect(T_LBRACE);
            while (tokens[pos].type != T_RBRACE && tokens[pos].type != T_EOF) {
                parseStatement();
            }
            expect(T_RBRACE);
        }

        
        void parseDeclaration() {
            /*
                This code checks for both declaration and assignment too.
                Both cases where 
                    a) we declare the variable only
                    b) We also assign it some base value
                
                are supported.
            */
            TokenType type = tokens[pos].type;
            pos++;  // consume type token

            string variableName = tokens[pos].value;
            expect(T_ID);

            // check if the variable exists in the symbol table or not. 
            // REDECLARATION OF THE SAME VARIABLE IS PROHIBITED.
            if (symbolTable.find(variableName) != symbolTable.end()) {
                cout << "Error: Variable '" << variableName << "' redeclared on Line: " 
                    << tokens[pos-1].line << ". Previously declared as "
                    << tokenTypeToString(symbolTable[variableName]) << "." << endl;
                exit(1);
            }
            symbolTable[variableName] = type;

            if (tokens[pos].type == T_ASSIGN) {
                pos++;  // consume assign token
                TokenType valueType;
                if (type == T_INT) {
                    expect(T_NUM);
                    valueType = T_INT;
                } 
                else if (type == T_DBL) {
                    expect(T_DOUBLE_VAL);
                    valueType = T_DBL;
                }
                else if (type == T_STR) {
                    expect(T_STRING);
                    valueType = T_STR;
                }
                
                // Type checking
                if (type != valueType) {
                    cout << "Type Error: Cannot assign " << tokenTypeToString(valueType) 
                        << " to " << tokenTypeToString(type) << " on Line: " << tokens[pos-1].line << endl;
                    exit(1);
                }
            }
            expect(T_SEMICOLON);
        }

        void parseAssignment() {
            /*
                Tackles stand-alone assignment to a variable.
            */
            string variableName = tokens[pos].value;
            expect(T_ID);
            
            // Check if variable exists in symbol table
            if (symbolTable.find(variableName) == symbolTable.end()) {
                cout << "Error: Undeclared variable '" << variableName << "' on Line: " << tokens[pos-1].line << endl;
                exit(1);
            }

            expect(T_ASSIGN);
            
            TokenType actualType = symbolTable[variableName];
            TokenType expectedType;

            if (tokens[pos].type == T_ID) {
                string rhsVarName = tokens[pos].value;
                if (symbolTable.find(rhsVarName) == symbolTable.end()) {
                    cout << "Error: Undeclared variable '" << rhsVarName << "' on Line: " << tokens[pos].line << endl;
                    exit(1);
                }
                expectedType = symbolTable[rhsVarName];
                parseExpression();
            } else if (tokens[pos].type == T_NUM) {
                expectedType = T_INT;
                expect(T_NUM);
            } else if (tokens[pos].type == T_DOUBLE_VAL) {
                expectedType = T_DBL;
                expect(T_DOUBLE_VAL);
            } else if (tokens[pos].type == T_STRING) {
                expectedType = T_STR;
                expect(T_STRING);
            } else {
                cout << "Syntax Error: Unexpected token in assignment on Line: " << tokens[pos].line << endl;
                exit(1);
            }

            // Type checking
            if (expectedType != actualType) {
                cout << "Type Error: Cannot assign " << tokenTypeToString(actualType) 
                    << " to " << tokenTypeToString(expectedType) << " on Line: " << tokens[pos-1].line << endl;
                exit(1);
            }

            expect(T_SEMICOLON);
        }

        void parseIfStatement() {
            /*
                if(condition) Block.
                else Block.
            */
            expect(T_IF);
            expect(T_LPAREN);
            parseExpression();
            expect(T_RPAREN);
            parseBlock();
            if (tokens[pos].type == T_ELSE)
            {
                expect(T_ELSE);
                parseBlock();
            }

        }

        void parseWhileLoop() {
            /*
                while(condition) Block
            */
            expect(T_WHILE);
            expect(T_LPAREN);
            parseExpression();
            expect(T_RPAREN);
            parseStatement();
        }

        void parseReturnStatement() {
            // return someExpression
            expect(T_RETURN);
            parseExpression();
            expect(T_SEMICOLON);
        }

        void parseForLoop(){
            /*
                FOR (DECLARATION WITH ASSIGNMENT; EXPRESSION; ASSIGNMENT) BLOCK.
                example: 

                jabtk(int x=0; x < 10; x = x+1;) {
                    a = x < 5;
                }
            */
            expect(T_FOR);
            expect(T_LPAREN);
            parseDeclaration();
            parseExpression();
            expect(T_SEMICOLON);
            parseAssignment();
            expect(T_RPAREN);
            parseBlock();
        }

        void parseFunction() {
            /*
                vibe add(int a, str b) {
                    a = a + 1;
                }
            */
            expect(T_FUNC);
            expect(T_ID);   
            expect(T_LPAREN);
            while (tokens[pos].type != T_RPAREN) {
                if (tokens[pos].type == T_INT || tokens[pos].type == T_STR) {
                    pos++;
                    expect(T_ID);   
                    if (tokens[pos].type == T_COMMA) pos++;
                }
            }
            expect(T_RPAREN);
            parseBlock();
        }

        void parseExpression() {
            /*
                This can be arithmetic, or boolean expression. Expressions can
                be recursively expressions.
            */
            if (tokens[pos].type == T_ID) {
                expect(T_ID);
            } else if (tokens[pos].type == T_NUM) {
                expect(T_NUM);
            } else if (tokens[pos].type == T_STRING) {
                expect(T_STRING);
            } else if (tokens[pos].type == T_DOUBLE_VAL) {
                expect(T_DOUBLE_VAL);
            } else {
                cout << "Syntax Error: Unexpected token in expression on line: " << tokens[pos].line << endl;
                exit(1);
            }

            // Check for relational or logical operators
            while (tokens[pos].type == T_GT || tokens[pos].type == T_LT || tokens[pos].type == T_GTE ||
                tokens[pos].type == T_LTE || tokens[pos].type == T_EQ || tokens[pos].type == T_NEQ ||
                tokens[pos].type == T_AND || tokens[pos].type == T_OR || tokens[pos].type == T_PLUS || 
                tokens[pos].type == T_MINUS || tokens[pos].type == T_MUL || tokens[pos].type == T_DIV) {
                
                TokenType op = tokens[pos].type; // Store the operator
                pos++;  // Consume the operator
                
                // Parse the next operand after the operator
                if (tokens[pos].type == T_ID || tokens[pos].type == T_NUM || 
                    tokens[pos].type == T_DOUBLE_VAL || tokens[pos].type == T_STRING) {
                    parseExpression(); // Recursively parse the next part of the expression
                } else {
                    cout << "Syntax Error: Expected operand after operator on line: " << tokens[pos].line << endl;
                    exit(1);
                }
            }
        }



    public:
        Parser(const vector<Token>& tokens) {
            this->tokens = tokens;
            this->pos = 0;
        }

        void parse() {
            while (tokens[pos].type != T_EOF) {
                parseStatement();
            }
        }
};

string readFileFromSource(const string& fileName) {
    ifstream file(fileName);
    if(!file) {
        cerr << "Error: Could not open file" << endl;
        exit(1);
    }

    string content((istreambuf_iterator<char>(file)), istreambuf_iterator<char>());
    file.close();
    return content;
}

int main(int argc, char* argv[]) {
    if(argc != 2) {
        cerr << "Usage Error: " << argv[0] << " incorrectly recieved fileName parameter;" << endl;
        return (1);
    }

    string fileName = argv[1];
    if (fileName.substr(fileName.find_last_of(".") + 1) != "skibidi") {
        cerr << "Error: File " << argv[1] <<" must have a .skibidi extension" << endl;
        return 1;
    }

    string input = readFileFromSource(fileName);

    Lexer lexer(input);
    Parser p(lexer.tokenize());

    p.parse();
    cout << "Code parsed successfully! You Hoo!!!" << endl;
}