/***
 **
 ** Recogniser.java            
 **
 ***/

/***
 * Yunus Can Bilge
 * yunuscan.bilge@gmail.com
 * Programming Languages and Compilers Assignment 2 Code 
 * 
 ***/


/* At this stage, this parser accepts a subset of VC defined	by
 * the following grammar. 
 *
 * You need to modify the supplied parsing methods (if necessary) and 
 * add the missing ones to obtain a parser for the VC language.
 *
 * (23--March--2013)

 program       -> func-decl

 // declaration

 func-decl     -> void identifier "(" ")" compound-stmt

 identifier    -> ID

 // statements 
 compound-stmt -> "{" stmt* "}" 
 stmt          -> continue-stmt
 |  expr-stmt
 continue-stmt -> continue ";"
 expr-stmt     -> expr? ";"

 // expressions 
 expr                -> assignment-expr
 assignment-expr     -> additive-expr
 additive-expr       -> multiplicative-expr
 |  additive-expr "+" multiplicative-expr
 multiplicative-expr -> unary-expr
 |  multiplicative-expr "*" unary-expr
 unary-expr          -> "-" unary-expr
 |  primary-expr

 primary-expr        -> identifier
 |  INTLITERAL
 | "(" expr ")"
 */

package VC.Recogniser;

import VC.Scanner.Scanner;
import VC.Scanner.SourcePosition;
import VC.Scanner.Token;
import VC.ErrorReporter;

public class Recogniser {

	private Scanner scanner;
	private ErrorReporter errorReporter;
	private Token currentToken;

	private boolean varDeclOn;

	public Recogniser(Scanner lexer, ErrorReporter reporter) {
		scanner = lexer;
		errorReporter = reporter;
		varDeclOn = false;
		currentToken = scanner.getToken();
	}

	// match checks to see f the current token matches tokenExpected.
	// If so, fetches the next token.
	// If not, reports a syntactic error.
	/*
	 * match method matches the tokens at production right-hand side
	 */
	void match(int tokenExpected) throws SyntaxError {
		if (currentToken.kind == tokenExpected) {
			currentToken = scanner.getToken();
		} else {
			syntacticError("\"%\" expected here", Token.spell(tokenExpected));
		}
	}

	// accepts the current token and fetches the next
	void accept() {
		currentToken = scanner.getToken();
	}

	void syntacticError(String messageTemplate, String tokenQuoted)
			throws SyntaxError {
		SourcePosition pos = currentToken.position;
		errorReporter.reportError(messageTemplate, tokenQuoted, pos);
		throw (new SyntaxError());
	}

	// ========================== PROGRAMS ========================

	public void parseProgram() {

		try {
			while (currentToken.kind == Token.VOID
					|| currentToken.kind == Token.BOOLEAN
					|| currentToken.kind == Token.INT
					|| currentToken.kind == Token.FLOAT) {
				parseType();
				parseIdent();
				if (currentToken.kind == Token.LPAREN) {
					//sonradan ekledim
					varDeclOn = false;
					parseFuncDecl();
				} else {
					varDeclOn = true;
					parseVarDecl();
				}
			}
			if (currentToken.kind != Token.EOF) {
				syntacticError("\"%\" wrong result type for a function",
						currentToken.spelling);
			}
		} catch (SyntaxError s) {
		}
	}
	//take a look at later
	// ========================== DECLARATIONS ========================
	void parseFuncDecl() throws SyntaxError {

		parseParaList();
		parseCompoundStmt();
	}

	void parseVarDecl() throws SyntaxError {
		parseType();
		parseInitDeclList();
		match(Token.SEMICOLON);
	}

	void parseInitDeclList() throws SyntaxError {
		parseInitDecl();
		while (currentToken.kind == Token.COMMA) {
			varDeclOn = false;
			match(Token.COMMA);
			parseInitDecl();
		}

	}

	void parseInitDecl() throws SyntaxError {
		parseDecl();

		if (currentToken.kind == Token.EQ) {
			match(Token.EQ);
			parseInitialiser();
		}
	}

	void parseDecl() throws SyntaxError {
		if (varDeclOn != true) {
			parseIdent();
			varDeclOn = false;
		}
		if (currentToken.kind == Token.LBRACKET) {
			match(Token.LBRACKET);
			if (currentToken.kind == Token.INTLITERAL) {
				match(Token.INTLITERAL);
			}
			match(Token.RBRACKET);
		}
	}

	void parseInitialiser() throws SyntaxError {
		switch (currentToken.kind) {
		case Token.LCURLY:
			match(Token.LCURLY);
			parseExpr();
			while (currentToken.kind == Token.COMMA) {
				match(Token.COMMA);
				parseExpr();
			}
			match(Token.RCURLY);
			break;
		default:
			parseExpr();
			break;
		}
	}

	// ======================= PRIMITIVE TYPES ==============================
	// bundada biseyler olabilir
	void parseType() throws SyntaxError {
		switch (currentToken.kind) {
		case Token.VOID:
			match(Token.VOID);
			break;
		case Token.BOOLEAN:
			match(Token.BOOLEAN);
			break;
		case Token.INT:
			match(Token.INT);
			break;
		case Token.FLOAT:
			match(Token.FLOAT);
			break;
		}
	}

	// ======================= STATEMENTS ==============================

	void parseCompoundStmt() throws SyntaxError {

		match(Token.LCURLY);
		while (currentToken.kind == Token.BOOLEAN
				|| currentToken.kind == Token.VOID
				|| currentToken.kind == Token.INT
				|| currentToken.kind == Token.FLOAT) {
			parseVarDecl();
		}
		while (currentToken.kind == Token.IF || currentToken.kind == Token.FOR
				|| currentToken.kind == Token.WHILE
				|| currentToken.kind == Token.BREAK
				|| currentToken.kind == Token.CONTINUE
				|| currentToken.kind == Token.RETURN
				|| currentToken.kind == Token.LCURLY
				|| currentToken.kind == Token.PLUS
				|| currentToken.kind == Token.MINUS
				|| currentToken.kind == Token.NOT
				|| currentToken.kind == Token.ID
				|| currentToken.kind == Token.LPAREN
				|| currentToken.kind == Token.INTLITERAL
				|| currentToken.kind == Token.FLOATLITERAL
				|| currentToken.kind == Token.BOOLEANLITERAL
				|| currentToken.kind == Token.STRINGLITERAL) {
			parseStmtList();
		}
		match(Token.RCURLY);
	}

	// Here, a new nontermial has been introduced to define { stmt } *
	void parseStmtList() throws SyntaxError {

		while (currentToken.kind != Token.RCURLY)
			parseStmt();
	}

	void parseStmt() throws SyntaxError {

		switch (currentToken.kind) {

		case Token.CONTINUE:
			parseContinueStmt();
			break;
		case Token.IF:
			parseIfStmt();
			break;
		case Token.FOR:
			parseForStmt();
			break;
		case Token.WHILE:
			parseWhileStmt();
			break;
		case Token.BREAK:
			parseBreakStmt();
			break;
		case Token.RETURN:
			parseReturnStmt();
			break;
		case Token.LCURLY:
			parseCompoundStmt();
			break;

		default:
			parseExprStmt();

			break;

		}
	}

	void parseIfStmt() throws SyntaxError {
		match(Token.IF);
		match(Token.LPAREN);
		parseExpr();
		match(Token.RPAREN);
		parseStmt();
		if (currentToken.kind == Token.ELSE) {
			match(Token.ELSE);
			parseStmt();
		}
	}

	void parseForStmt() throws SyntaxError {
		match(Token.FOR);
		match(Token.LPAREN);
		if(currentToken.kind == Token.PLUS || 
				currentToken.kind == Token.MINUS ||
				currentToken.kind == Token.NOT ||
				currentToken.kind == Token.ID ||
				currentToken.kind == Token.LPAREN ||
				currentToken.kind == Token.INTLITERAL ||
				currentToken.kind == Token.FLOATLITERAL ||
				currentToken.kind == Token.BOOLEANLITERAL ||
				currentToken.kind == Token.STRINGLITERAL) {
				parseExpr();
		}
		match(Token.SEMICOLON);
		if(currentToken.kind == Token.PLUS || 
				currentToken.kind == Token.MINUS ||
				currentToken.kind == Token.NOT ||
				currentToken.kind == Token.ID ||
				currentToken.kind == Token.LPAREN ||
				currentToken.kind == Token.INTLITERAL ||
				currentToken.kind == Token.FLOATLITERAL ||
				currentToken.kind == Token.BOOLEANLITERAL ||
				currentToken.kind == Token.STRINGLITERAL) {
				parseExpr();
		}
		match(Token.SEMICOLON);
		if(currentToken.kind == Token.PLUS || 
				currentToken.kind == Token.MINUS ||
				currentToken.kind == Token.NOT ||
				currentToken.kind == Token.ID ||
				currentToken.kind == Token.LPAREN ||
				currentToken.kind == Token.INTLITERAL ||
				currentToken.kind == Token.FLOATLITERAL ||
				currentToken.kind == Token.BOOLEANLITERAL ||
				currentToken.kind == Token.STRINGLITERAL) {
				parseExpr();
		}
		match(Token.RPAREN);
		parseStmt();
		
	}

	void parseWhileStmt() throws SyntaxError {
		match(Token.WHILE);
		match(Token.LPAREN);
		parseExpr();
		match(Token.RPAREN);
		parseStmt();
	}

	void parseBreakStmt() throws SyntaxError {
		match(Token.BREAK);
		match(Token.SEMICOLON);
	}

	void parseContinueStmt() throws SyntaxError {

		match(Token.CONTINUE);
		match(Token.SEMICOLON);

	}

	void parseReturnStmt() throws SyntaxError {
		match(Token.RETURN);
		if (currentToken.kind == Token.ID
				|| currentToken.kind == Token.INTLITERAL
				|| currentToken.kind == Token.MINUS
				|| currentToken.kind == Token.LPAREN
				|| currentToken.kind == Token.PLUS
				|| currentToken.kind == Token.NOT
				|| currentToken.kind == Token.FLOATLITERAL
				|| currentToken.kind == Token.BOOLEANLITERAL
				|| currentToken.kind == Token.STRINGLITERAL) {
			parseExpr();
			match(Token.SEMICOLON);
		} else {
			match(Token.SEMICOLON);
		}

	}

	void parseExprStmt() throws SyntaxError {

		if (currentToken.kind == Token.ID
				|| currentToken.kind == Token.INTLITERAL
				|| currentToken.kind == Token.MINUS
				|| currentToken.kind == Token.LPAREN
				|| currentToken.kind == Token.PLUS
				|| currentToken.kind == Token.NOT
				|| currentToken.kind == Token.FLOATLITERAL
				|| currentToken.kind == Token.BOOLEANLITERAL
				|| currentToken.kind == Token.STRINGLITERAL) {
			parseExpr();
			match(Token.SEMICOLON);
		} else {
			match(Token.SEMICOLON);
		}
	}

	// ======================= IDENTIFIERS ======================

	// Call parseIdent rather than match(Token.ID).
	// In Assignment 3, an Identifier node will be constructed in here.

	void parseIdent() throws SyntaxError {

		if (currentToken.kind == Token.ID) {
			currentToken = scanner.getToken();
		} else
			syntacticError("identifier expected here", "");
	}

	// take a look at later
	// ======================= OPERATORS ======================

	// Call acceptOperator rather than accept().
	// In Assignment 3, an Operator Node will be constructed in here.

	void acceptOperator() throws SyntaxError {

		currentToken = scanner.getToken();
	}

	// ======================= EXPRESSIONS ======================

	void parseExpr() throws SyntaxError {
		parseAssignExpr();
	}

	// take a look at

	void parseAssignExpr() throws SyntaxError {

		parseCondOrExpr();
		while (currentToken.kind == Token.EQ) {
			match(Token.EQ);
			parseCondOrExpr();
		}

	}

	void parseCondOrExpr() throws SyntaxError {

		parseCondAndExpr();
		while (currentToken.kind == Token.OROR) {
			acceptOperator();
			parseCondAndExpr();
		}
	}

	void parseCondAndExpr() throws SyntaxError {
		parseEqualityExpr();
		while (currentToken.kind == Token.ANDAND) {
			acceptOperator();
			parseEqualityExpr();
		}
	}

	void parseEqualityExpr() throws SyntaxError {
		parseRelExpr();
		if (currentToken.kind == Token.EQEQ) {
			while (currentToken.kind == Token.EQEQ) {
				accept();
				parseRelExpr();
			}
		} else if (currentToken.kind == Token.NOTEQ) {
			while (currentToken.kind == Token.NOTEQ) {
				accept();
				parseRelExpr();
			}
		}
	}

	void parseRelExpr() throws SyntaxError {

		parseAdditiveExpr();
		if (currentToken.kind == Token.LT) {
			while (currentToken.kind == Token.LT) {
				acceptOperator();
				parseAdditiveExpr();
			}
		} else if (currentToken.kind == Token.LTEQ) {
			while (currentToken.kind == Token.LTEQ) {
				acceptOperator();
				parseAdditiveExpr();
			}
		} else if (currentToken.kind == Token.GT) {
			while (currentToken.kind == Token.GT) {
				acceptOperator();
				parseAdditiveExpr();
			}
		} else if (currentToken.kind == Token.GTEQ) {
			while (currentToken.kind == Token.GTEQ) {
				acceptOperator();
				parseAdditiveExpr();
			}
		}
	}

	void parseAdditiveExpr() throws SyntaxError {
		parseMultiplicativeExpr();
		if (currentToken.kind == Token.PLUS) {
			while (currentToken.kind == Token.PLUS) {
				acceptOperator();
				parseMultiplicativeExpr();
			}
		} else if (currentToken.kind == Token.MINUS) {
			while (currentToken.kind == Token.MINUS) {
				acceptOperator();
				parseMultiplicativeExpr();
			}
		}

	}
	void parseMultiplicativeExpr() throws SyntaxError {

		parseUnaryExpr();
		if (currentToken.kind == Token.MULT) {
			while (currentToken.kind == Token.MULT) {
				acceptOperator();
				parseUnaryExpr();
			}
		} else if (currentToken.kind == Token.DIV) {
			while (currentToken.kind == Token.DIV) {
				acceptOperator();
				parseUnaryExpr();
			}
		}
	}


	void parseUnaryExpr() throws SyntaxError {

		switch (currentToken.kind) {
		case Token.MINUS: {
			acceptOperator();
			parseUnaryExpr();
		}
			break;
		case Token.PLUS: {
			acceptOperator();
			parseUnaryExpr();
		}
			break;
		case Token.NOT: {
			acceptOperator();
			parseUnaryExpr();
		}
			break;
		default:
			parsePrimaryExpr();
			break;

		}
	}
	//take a look at
	void parsePrimaryExpr() throws SyntaxError {
		switch (currentToken.kind) {

		case Token.ID: {
			parseIdent();

			if (currentToken.kind == Token.LBRACKET) {
				match(Token.LBRACKET);
				parseExpr();
				match(Token.RBRACKET);
			} else if (currentToken.kind == Token.LPAREN) {
				parseArgList();
			}
		}
			break;

		case Token.LPAREN: {
			match(Token.LPAREN);
			parseExpr();
			match(Token.RPAREN);
		}
			break;

		case Token.INTLITERAL:
			parseIntLiteral();
			break;
		case Token.FLOATLITERAL:
			parseFloatLiteral();
			break;
		case Token.STRINGLITERAL:
			parseStringLiteral();
			break;
		case Token.BOOLEANLITERAL:
			parseBooleanLiteral();
			break;

		default:
			syntacticError("illegal parimary expression", currentToken.spelling);

		}
	}

	// ========================== LITERALS ========================

	// Call these methods rather than accept(). In Assignment 3,
	// literal AST nodes will be constructed inside these methods.

	void parseIntLiteral() throws SyntaxError {

		if (currentToken.kind == Token.INTLITERAL) {
			currentToken = scanner.getToken();
		} else
			syntacticError("integer literal expected here", "");
	}

	void parseStringLiteral() throws SyntaxError {
		if (currentToken.kind == Token.STRINGLITERAL) {
			currentToken = scanner.getToken();
		} else {
			syntacticError("String literal expected here", "");
		}
	}

	void parseFloatLiteral() throws SyntaxError {

		if (currentToken.kind == Token.FLOATLITERAL) {
			currentToken = scanner.getToken();
		} else
			syntacticError("float literal expected here", "");
	}

	void parseBooleanLiteral() throws SyntaxError {

		if (currentToken.kind == Token.BOOLEANLITERAL) {
			currentToken = scanner.getToken();
		} else
			syntacticError("boolean literal expected here", "");
	}

	// ================ parameters ==============================
	void parseParaList() throws SyntaxError {
		match(Token.LPAREN);
		if (currentToken.kind != Token.RPAREN) {
			parseProperParaList();
		}
		match(Token.RPAREN);
	}

	void parseProperParaList() throws SyntaxError {
		// parseDecl();
		parsParaDecl();
		while (currentToken.kind == Token.COMMA) {
			match(Token.COMMA);
			// parseDecl();
			parsParaDecl();
		}
	}
	//parseDecl declarator
	void parsParaDecl() throws SyntaxError {

		if (currentToken.kind == Token.VOID) {
			match(Token.VOID);
			parseDecl();
		} else if (currentToken.kind == Token.BOOLEAN) {
			match(Token.BOOLEAN);
			parseDecl();
		} else if (currentToken.kind == Token.INT) {
			match(Token.INT);
			parseDecl();
		} else if (currentToken.kind == Token.FLOAT) {
			match(Token.FLOAT);
			parseDecl();
		}
	}
	void parseArgList() throws SyntaxError {
		match(Token.LPAREN);
		if (currentToken.kind != Token.RPAREN) {
			parseProperArgList();
			match(Token.RPAREN);
		} else {
			match(Token.RPAREN);
		}
	}
	void parseProperArgList() throws SyntaxError {
		parseArg();
		while (currentToken.kind == Token.COMMA) {
			match(Token.COMMA);
			parseArg();
		}
	}
	void parseArg() throws SyntaxError {
		parseExpr();
	}
}
