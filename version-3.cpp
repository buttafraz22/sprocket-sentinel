#include<iostream>
#include<vector>
#include<string>
#include<cctype>
#include<algorithm>
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

enum TACOp {
    TAC_ADD, TAC_SUB, TAC_MUL, TAC_DIV,
    TAC_ASSIGN, TAC_LABEL, TAC_GOTO,
    TAC_IF_EQ, TAC_IF_NEQ, TAC_IF_LT, TAC_IF_GT, TAC_IF_LTE, TAC_IF_GTE,
    TAC_PARAM, TAC_CALL, TAC_RETURN,
    TAC_PRINT
};

struct TACInstruction {
    TACOp op;
    string result;
    string arg1;
    string arg2;
    
    TACInstruction(TACOp op, string result, string arg1 = "", string arg2 = "")
        : op(op), result(result), arg1(arg1), arg2(arg2) {}
        
    string toString() const {
        switch(op) {
            case TAC_ADD:
                return result + " :=  " + arg1 + " + " + arg2;
            case TAC_SUB:
                return result + " := " + arg1 + " - " + arg2;
            case TAC_MUL:
                return result + " := " + arg1 + " * " + arg2;
            case TAC_DIV:
                return result + " := " + arg1 + " / " + arg2;
            case TAC_ASSIGN:
                return result + " := " + arg1;
            case TAC_LABEL:
                return result + ":";
            case TAC_GOTO:
                return "goto " + result;
            case TAC_IF_EQ:
                return "if " + arg1 + " == " + arg2 + " goto " + result;
            case TAC_IF_NEQ:
                return "if " + arg1 + " != " + arg2 + " goto " + result;
            case TAC_IF_LT:
                return "if " + arg1 + " < " + arg2 + " goto " + result;
            case TAC_IF_GT:
                return "if " + arg1 + " > " + arg2 + " goto " + result;
            case TAC_IF_LTE:
                return "if " + arg1 + " <= " + arg2 + " goto " + result;
            case TAC_IF_GTE:
                return "if " + arg1 + " >= " + arg2 + " goto " + result;
            case TAC_PARAM:
                return "param " + result;
            case TAC_CALL:
                return result + " = call " + arg1;
            case TAC_RETURN:
                return "return " + result;
            case TAC_PRINT:
                return "print " + result;
            default:
                return "unknown instruction";
        }
    }
};

class TACGenerator {
private:
    vector<TACInstruction> instructions;
    int tempCount = 0;
    int labelCount = 0;
    
    string newTemp() {
        return "t" + to_string(tempCount++);
    }
    
    string newLabel() {
        return "L" + to_string(labelCount++);
    }

public:
    vector<TACInstruction> getInstructions() {
        return instructions;
    }
    string generateTemp() {
        return newTemp();
    }
    
    string generateLabel() {
        return newLabel();
    }
    
    void addInstruction(const TACInstruction& inst) {
        instructions.push_back(inst);
    }
    
    void printInstructions() {
        cout << "\nGenerated Three Address Code:\n";
        for(size_t i = 0; i < instructions.size(); i++) {
            cout << i << ": " << instructions[i].toString() << endl;
        }
    }
    
    // Helper methods for common operations
    string generateBinaryOp(TACOp op, const string& arg1, const string& arg2) {
        string temp = newTemp();
        instructions.push_back(TACInstruction(op, temp, arg1, arg2));
        return temp;
    }
    
    void generateAssignment(const string& target, const string& value) {
        instructions.push_back(TACInstruction(TAC_ASSIGN, target, value));
    }
    
    void generateIfGoto(TACOp condition, const string& arg1, const string& arg2, const string& label) {
        instructions.push_back(TACInstruction(condition, label, arg1, arg2));
    }
    
    void generateGoto(const string& label) {
        instructions.push_back(TACInstruction(TAC_GOTO, label));
    }
    
    void generateLabel(const string& label) {
        instructions.push_back(TACInstruction(TAC_LABEL, label));
    }
    
    void generatePrint(const string& value) {
        instructions.push_back(TACInstruction(TAC_PRINT, value));
    }

    string generateComparison(TACOp op, const string& arg1, const string& arg2) {
        string temp = newTemp();
        switch(op) {
            case TAC_IF_EQ:
                instructions.push_back(TACInstruction(TAC_ASSIGN, temp, arg1 + " == " + arg2));
                break;
            case TAC_IF_NEQ:
                instructions.push_back(TACInstruction(TAC_ASSIGN, temp, arg1 + " != " + arg2));
                break;
            case TAC_IF_LT:
                instructions.push_back(TACInstruction(TAC_ASSIGN, temp, arg1 + " < " + arg2));
                break;
            case TAC_IF_GT:
                instructions.push_back(TACInstruction(TAC_ASSIGN, temp, arg1 + " > " + arg2));
                break;
            case TAC_IF_LTE:
                instructions.push_back(TACInstruction(TAC_ASSIGN, temp, arg1 + " <= " + arg2));
                break;
            case TAC_IF_GTE:
                instructions.push_back(TACInstruction(TAC_ASSIGN, temp, arg1 + " >= " + arg2));
                break;
        }
        return temp;
    }
};

class Parser {
    private:
        vector<Token> tokens;
        size_t pos;
        map<string, TokenType> symbolTable;
        TACGenerator tacGen;


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
            /*
                yap "something"
            */
            expect(T_PRINT);
            
            // Handle different types of print values
            string value;
            TokenType type;
            
            if (tokens[pos].type == T_STRING) {
                value = tokens[pos].value;
                type = T_STRING;
                expect(T_STRING);
            } else if (tokens[pos].type == T_ID) {
                value = tokens[pos].value;
                type = symbolTable[value];  // Get type from symbol table
                expect(T_ID);
            } else if (tokens[pos].type == T_NUM) {
                value = tokens[pos].value;
                type = T_NUM;
                expect(T_NUM);
            } else if (tokens[pos].type == T_DOUBLE_VAL) {
                value = tokens[pos].value;
                type = T_DOUBLE_VAL;
                expect(T_DOUBLE_VAL);
            } else {
                cout << "Error: Invalid print argument on line " << tokens[pos].line << endl;
                exit(1);
            }
            
            tacGen.generatePrint(value);
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
                string expressionValue;
                
                if (type == T_INT) {
                    expect(T_NUM);
                    valueType = T_INT;
                    expressionValue = tokens[pos-1].value;
                } 
                else if (type == T_DBL) {
                    expect(T_DOUBLE_VAL);
                    valueType = T_DBL;
                    expressionValue = tokens[pos-1].value;
                }
                else if (type == T_STR) {
                    expect(T_STRING);
                    valueType = T_STR;
                    expressionValue = "\"" + tokens[pos-1].value + "\"";
                }

                // Type checking
                if (!isTypeCompatible(type, valueType)) {
                    cout << "Type Error: Cannot assign " << tokenTypeToString(valueType) 
                        << " to " << tokenTypeToString(type) << " on Line: " << tokens[pos-1].line << endl;
                    exit(1);
                }

                // Generate TAC instruction for simple declaration and assignment
                tacGen.generateAssignment(variableName, expressionValue);
            }
            expect(T_SEMICOLON);
        }

        void parseAssignment() {
            /*
                Tackles stand-alone assignment to a variable.
            */
            string variableName = tokens[pos].value;
            expect(T_ID);
            
            if (symbolTable.find(variableName) == symbolTable.end()) {
                cout << "Error: Undeclared variable '" << variableName << "' on Line: " 
                    << tokens[pos-1].line << endl;
                exit(1);
            }
            
            expect(T_ASSIGN);
            
            string exprResult = parseExpression();
            tacGen.generateAssignment(variableName, exprResult);
            
            expect(T_SEMICOLON);

        }

        void parseIfStatement() {
            /*
                if(condition) block
            */
            expect(T_IF);
            expect(T_LPAREN);

            string condition = parseExpression();  // This will now return the temp variable holding comparison result
            string elseLabel = tacGen.generateLabel();
            string endLabel = tacGen.generateLabel();

            expect(T_RPAREN);

            // Generate the conditional jump using the comparison result
            tacGen.generateIfGoto(TAC_IF_EQ, condition, "false", elseLabel);

            parseBlock();
            tacGen.generateGoto(endLabel);

            tacGen.generateLabel(elseLabel);
            if (tokens[pos].type == T_ELSE) {
                expect(T_ELSE);
                parseBlock();
            }

            tacGen.generateLabel(endLabel);
        }

        void parseWhileLoop() {
            /*
                while(condition) Block
            */
            expect(T_WHILE);
        
            string startLabel = tacGen.generateLabel();
            string endLabel = tacGen.generateLabel();
            
            tacGen.generateLabel(startLabel);
            
            expect(T_LPAREN);
            string condition = parseExpression();
            expect(T_RPAREN);
            
            tacGen.generateIfGoto(TAC_IF_EQ, condition, "false", endLabel);
            
            parseStatement();
            tacGen.generateGoto(startLabel);
            tacGen.generateLabel(endLabel);

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
            
            string startLabel = tacGen.generateLabel();
            string endLabel = tacGen.generateLabel();
            string incrementLabel = tacGen.generateLabel();
            
            tacGen.generateLabel(startLabel);
            
            string condition = parseExpression();
            expect(T_SEMICOLON);
            
            tacGen.generateIfGoto(TAC_IF_EQ, condition, "false", endLabel);
            
            // Store the increment statement tokens
            vector<Token> incrementTokens;
            size_t tempPos = pos;
            while (tokens[pos].type != T_RPAREN) {
                incrementTokens.push_back(tokens[pos]);
                pos++;
            }
            expect(T_RPAREN);
            
            parseBlock();
            
            tacGen.generateLabel(incrementLabel);
            
            // Process the increment statement
            vector<Token> savedTokens = tokens;
            size_t savedPos = pos;
            
            // Replace tokens temporarily with increment tokens
            tokens = incrementTokens;
            pos = 0;
            
            // Parse and generate TAC for increment
            parseAssignment();
            
            // Restore original tokens and position
            tokens = savedTokens;
            pos = savedPos;
            
            // Jump back to condition check
            tacGen.generateGoto(startLabel);
            
            // Generate end label
            tacGen.generateLabel(endLabel);
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

        string parseExpression() {
            /*
                This can be arithmetic, or boolean expression. Expressions can
                be recursively expressions.
            */
            string result;

            if (tokens[pos].type == T_ID) {
                result = tokens[pos].value;
                expect(T_ID);
            } else if (tokens[pos].type == T_NUM) {
                result = tokens[pos].value;
                expect(T_NUM);
            } else if (tokens[pos].type == T_DOUBLE_VAL) {
                result = tokens[pos].value;
                expect(T_DOUBLE_VAL);
            } else if (tokens[pos].type == T_STRING) {
                result = "\"" + tokens[pos].value + "\"";
                expect(T_STRING);
            }

            while (pos < tokens.size() && (tokens[pos].type == T_GT || tokens[pos].type == T_LT || 
                tokens[pos].type == T_GTE || tokens[pos].type == T_LTE || tokens[pos].type == T_EQ || 
                tokens[pos].type == T_NEQ || tokens[pos].type == T_AND || tokens[pos].type == T_OR || 
                tokens[pos].type == T_PLUS || tokens[pos].type == T_MINUS || tokens[pos].type == T_MUL || 
                tokens[pos].type == T_DIV)) {
                
                TokenType op = tokens[pos].type;
                pos++; // consume operator

                string nextOperand;
                if (tokens[pos].type == T_ID || tokens[pos].type == T_NUM || 
                    tokens[pos].type == T_DOUBLE_VAL || tokens[pos].type == T_STRING) {
                    nextOperand = tokens[pos].value;
                    pos++; // consume the operand
                }

                // Handle comparison operators
                if (op == T_GT || op == T_LT || op == T_GTE || op == T_LTE || 
                    op == T_EQ || op == T_NEQ) {
                    TACOp tacOp;
                    switch(op) {
                        case T_LTE: tacOp = TAC_IF_LTE; break;
                        case T_GTE: tacOp = TAC_IF_GTE; break;
                        case T_NEQ: tacOp = TAC_IF_NEQ; break;
                        case T_GT: tacOp = TAC_IF_GT; break;
                        case T_LT: tacOp = TAC_IF_LT; break;
                        case T_EQ: tacOp = TAC_IF_EQ; break;
                        default: tacOp = TAC_IF_EQ;
                    }
                    result = tacGen.generateComparison(tacOp, result, nextOperand);
                }
                // Handle arithmetic operators
                else {
                    TACOp tacOp;
                    switch(op) {
                        case T_PLUS: tacOp = TAC_ADD; break;
                        case T_MINUS: tacOp = TAC_SUB; break;
                        case T_MUL: tacOp = TAC_MUL; break;
                        case T_DIV: tacOp = TAC_DIV; break;
                        default: tacOp = TAC_ADD;
                    }
                    result = tacGen.generateBinaryOp(tacOp, result, nextOperand);
                }
            }

            return result;
        }

    public:
        Parser(const vector<Token>& tokens) {
            this->tokens = tokens;
            this->pos = 0;
        }

        vector<TACInstruction> parse() {
            while (tokens[pos].type != T_EOF) {
                parseStatement();
            }
            return tacGen.getInstructions();
        }
};

// Assembly Generator class for x86_64
class AssemblyGenerator {
private:
    vector<string> assembly;
    map<string, string> varToReg;
    map<string, int> varToStack;
    int stackOffset = 0;
    int labelCount = 0;
    vector<string> availableRegs = {"rax", "rbx", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11"};

    string getNextReg() {
        if (!availableRegs.empty()) {
            string reg = availableRegs.back();
            availableRegs.pop_back();
            return reg;
        }
        // If no registers available, allocate stack space
        stackOffset += 8;
        return "qword [rbp-" + to_string(stackOffset) + "]";  // Added qword specifier
    }

    void freeReg(const string& reg) {
        if (reg.find("[") == string::npos) {
            availableRegs.push_back(reg);
        }
    }

    string allocateVar(const string& var) {
        if (varToReg.find(var) == varToReg.end()) {
            string location = getNextReg();
            varToReg[var] = location;
        }
        return varToReg[var];
    }

    // Helper to ensure proper operand format
    string ensureOperandFormat(const string& operand) {
        if (operand.find("[") != string::npos && operand.find("qword") == string::npos) {
            return "qword " + operand;
        }
        return operand;
    }

    void saveRegisters() {
        for (const auto& reg : {"rax", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11"}) {
            assembly.push_back("    push " + string(reg));
        }
    }

    void restoreRegisters() {
        vector<string> regs = {"r11", "r10", "r9", "r8", "rdi", "rsi", "rdx", "rcx", "rax"};
        for (const auto& reg : regs) {
            assembly.push_back("    pop " + reg);
        }
    }

public:
    void generateProlog() {
        assembly.push_back("global main");
        assembly.push_back("extern printf");
        assembly.push_back("section .text");
        assembly.push_back("main:");
        assembly.push_back("    push rbp");
        assembly.push_back("    mov rbp, rsp");
        assembly.push_back("    sub rsp, 208  ; Reserve stack space");
    }

    void generateEpilog() {
        assembly.push_back("    xor eax, eax");  // Changed to use 32-bit register for clarity
        assembly.push_back("    leave");
        assembly.push_back("    ret");
    }

    void generateFromTAC(const TACInstruction& tac) {
        switch (tac.op) {
            case TAC_ADD: {
                string dest = allocateVar(tac.result);
                string src1 = allocateVar(tac.arg1);
                string src2 = allocateVar(tac.arg2);
                
                // Ensure proper memory operand format
                dest = ensureOperandFormat(dest);
                src1 = ensureOperandFormat(src1);
                src2 = ensureOperandFormat(src2);

                if (dest.find("[") != string::npos) {
                    // If destination is memory, need intermediate register
                    assembly.push_back("    mov rax, " + src1);
                    assembly.push_back("    add rax, " + src2);
                    assembly.push_back("    mov " + dest + ", rax");
                } else {
                    assembly.push_back("    mov " + dest + ", " + src1);
                    assembly.push_back("    add " + dest + ", " + src2);
                }
                break;
            }
            case TAC_SUB: {
                string dest = ensureOperandFormat(allocateVar(tac.result));
                string src1 = ensureOperandFormat(allocateVar(tac.arg1));
                string src2 = ensureOperandFormat(allocateVar(tac.arg2));

                if (dest.find("[") != string::npos) {
                    assembly.push_back("    mov rax, " + src1);
                    assembly.push_back("    sub rax, " + src2);
                    assembly.push_back("    mov " + dest + ", rax");
                } else {
                    assembly.push_back("    mov " + dest + ", " + src1);
                    assembly.push_back("    sub " + dest + ", " + src2);
                }
                break;
            }
            case TAC_MUL: {
                string dest = ensureOperandFormat(allocateVar(tac.result));
                string src1 = ensureOperandFormat(allocateVar(tac.arg1));
                string src2 = ensureOperandFormat(allocateVar(tac.arg2));

                assembly.push_back("    mov rax, " + src1);
                assembly.push_back("    imul rax, " + src2);
                assembly.push_back("    mov " + dest + ", rax");
                break;
            }
            case TAC_DIV: {
                string dest = ensureOperandFormat(allocateVar(tac.result));
                string src1 = ensureOperandFormat(allocateVar(tac.arg1));
                string src2 = ensureOperandFormat(allocateVar(tac.arg2));

                assembly.push_back("    mov rax, " + src1);
                assembly.push_back("    xor rdx, rdx");
                assembly.push_back("    mov rcx, " + src2);  // Move divisor to register
                assembly.push_back("    div rcx");  // div always uses register
                assembly.push_back("    mov " + dest + ", rax");
                break;
            }
            case TAC_ASSIGN: {
                string dest = ensureOperandFormat(allocateVar(tac.result));
                if (all_of(tac.arg1.begin(), tac.arg1.end(), ::isdigit)) {
                    if (dest.find("[") != string::npos) {
                        assembly.push_back("    mov rax, " + tac.arg1);
                        assembly.push_back("    mov " + dest + ", rax");
                    } else {
                        assembly.push_back("    mov " + dest + ", " + tac.arg1);
                    }
                } else {
                    string src = ensureOperandFormat(allocateVar(tac.arg1));
                    if (dest.find("[") != string::npos && src.find("[") != string::npos) {
                        assembly.push_back("    mov rax, " + src);
                        assembly.push_back("    mov " + dest + ", rax");
                    } else {
                        assembly.push_back("    mov " + dest + ", " + src);
                    }
                }
                break;
            }
            case TAC_PRINT: {
                static bool format_added = false;
                if (!format_added) {
                    assembly.push_back("section .data");
                    assembly.push_back("    fmt_int: db '%d', 10, 0");
                    assembly.push_back("section .text");
                    format_added = true;
                }
                string src = ensureOperandFormat(allocateVar(tac.result));
                if (src.find("[") != string::npos) {
                    assembly.push_back("    mov rsi, " + src);  // Load from memory to register
                } else {
                    assembly.push_back("    mov rsi, " + src);
                }
                assembly.push_back("    mov rdi, fmt_int");
                assembly.push_back("    xor eax, eax");
                assembly.push_back("    call printf");
                break;
            }
            case TAC_LABEL:
                assembly.push_back(tac.result + ":");
                break;
            case TAC_GOTO:
                assembly.push_back("    jmp " + tac.result);
                break;
            case TAC_IF_EQ: {
                string src1 = ensureOperandFormat(allocateVar(tac.arg1));
                string src2 = ensureOperandFormat(allocateVar(tac.arg2));
                assembly.push_back("    mov rax, " + src1);
                assembly.push_back("    cmp rax, " + src2);
                assembly.push_back("    je " + tac.result);
                break;
            }
            case TAC_IF_NEQ: {
                string src1 = ensureOperandFormat(allocateVar(tac.arg1));
                string src2 = ensureOperandFormat(allocateVar(tac.arg2));
                assembly.push_back("    mov rax, " + src1);
                assembly.push_back("    cmp rax, " + src2);
                assembly.push_back("    jne " + tac.result);
                break;
            }
            case TAC_IF_LT: {
                string src1 = ensureOperandFormat(allocateVar(tac.arg1));
                string src2 = ensureOperandFormat(allocateVar(tac.arg2));
                assembly.push_back("    mov rax, " + src1);
                assembly.push_back("    cmp rax, " + src2);
                assembly.push_back("    jl " + tac.result);
                break;
            }
            case TAC_IF_GT: {
                string src1 = ensureOperandFormat(allocateVar(tac.arg1));
                string src2 = ensureOperandFormat(allocateVar(tac.arg2));
                assembly.push_back("    mov rax, " + src1);
                assembly.push_back("    cmp rax, " + src2);
                assembly.push_back("    jg " + tac.result);
                break;
            }
            case TAC_IF_LTE: {
                string src1 = ensureOperandFormat(allocateVar(tac.arg1));
                string src2 = ensureOperandFormat(allocateVar(tac.arg2));
                assembly.push_back("    mov rax, " + src1);
                assembly.push_back("    cmp rax, " + src2);
                assembly.push_back("    jle " + tac.result);
                break;
            }
            case TAC_IF_GTE: {
                string src1 = ensureOperandFormat(allocateVar(tac.arg1));
                string src2 = ensureOperandFormat(allocateVar(tac.arg2));
                assembly.push_back("    mov rax, " + src1);
                assembly.push_back("    cmp rax, " + src2);
                assembly.push_back("    jge " + tac.result);
                break;
            }
            case TAC_CALL: {
                saveRegisters();  // Save registers before call
                
                // If there's a return value destination
                if (!tac.result.empty()) {
                    string dest = ensureOperandFormat(allocateVar(tac.result));
                    assembly.push_back("    call " + tac.arg1);
                    // Move return value (in rax) to destination
                    if (dest.find("[") != string::npos) {
                        assembly.push_back("    mov " + dest + ", rax");
                    } else {
                        assembly.push_back("    mov " + dest + ", rax");
                    }
                } else {
                    assembly.push_back("    call " + tac.arg1);
                }
                
                restoreRegisters();  // Restore registers after call
                break;
            }
            case TAC_PARAM: {
                // Parameter passing according to System V AMD64 ABI
                // First 6 integer/pointer arguments go in: rdi, rsi, rdx, rcx, r8, r9
                static const vector<string> paramRegs = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
                static int paramCount = 0;
                
                string src = ensureOperandFormat(allocateVar(tac.result));
                
                if (paramCount < paramRegs.size()) {
                    // If parameter is in memory, load it to register first
                    if (src.find("[") != string::npos) {
                        assembly.push_back("    mov rax, " + src);
                        assembly.push_back("    mov " + paramRegs[paramCount] + ", rax");
                    } else {
                        assembly.push_back("    mov " + paramRegs[paramCount] + ", " + src);
                    }
                } else {
                    // For additional parameters, push them on stack in reverse order
                    if (src.find("[") != string::npos) {
                        assembly.push_back("    mov rax, " + src);
                        assembly.push_back("    push rax");
                    } else {
                        assembly.push_back("    push " + src);
                    }
                }
                paramCount++;
                break;
            }
            case TAC_RETURN: {
                if (!tac.result.empty()) {
                    string src = ensureOperandFormat(allocateVar(tac.result));
                    // If returning from memory location
                    if (src.find("[") != string::npos) {
                        assembly.push_back("    mov rax, " + src);
                    } else {
                        assembly.push_back("    mov rax, " + src);
                    }
                } else {
                    assembly.push_back("    xor rax, rax");  // Return 0 by default
                }
                assembly.push_back("    leave");
                assembly.push_back("    ret");
                break;
            }
        }
    }


    void generateAssembly(const vector<TACInstruction>& tacInstructions) {
        generateProlog();
        for (const auto& tac : tacInstructions) {
            generateFromTAC(tac);
        }
        generateEpilog();
    }

    void outputAssembly(const string& filename) {
        ofstream outFile(filename + ".asm");
        for (const auto& line : assembly) {
            outFile << line << endl;
        }
        outFile.close();

        string compileCmd = "nasm -f elf64 " + filename + ".asm -o " + filename + ".o";
        string linkCmd = "gcc -no-pie " + filename + ".o -o " + filename;

        system(compileCmd.c_str());
        system(linkCmd.c_str());
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

    vector<TACInstruction> tacTokens = p.parse();
    AssemblyGenerator asmGen;
    asmGen.generateAssembly(tacTokens);
    asmGen.outputAssembly(fileName.substr(0, fileName.find_last_of(".")));

    cout << "Code parsed successfully! You Hoo!!!" << endl;
}