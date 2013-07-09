/*
 * Parser.java            
 *
 * This parser for a subset of the VC language is intended to 
 *  demonstrate how to create the AST nodes, including (among others): 
 *  [1] a list (of statements)
 *  [2] a function
 *  [3] a statement (which is an expression statement), 
 *  [4] a unary expression
 *  [5] a binary expression
 *  [6] terminals (identifiers, integer literals and operators)
 *
 * In addition, it also demonstrates how to use the two methods start 
 * and finish to determine the position information for the start and 
 * end of a construct (known as a phrase) corresponding an AST node.
 *
 * NOTE THAT THE POSITION INFORMATION WILL NOT BE MARKED. HOWEVER, IT CAN BE
 * USEFUL TO DEBUG YOUR IMPLEMENTATION.
 *
 * (10	April  2013)


program       -> func-decl
func-decl     -> type identifier "(" ")" compound-stmt
type          -> void
identifier    -> ID
// statements
compound-stmt -> "{" stmt* "}" 
stmt          -> expr-stmt
expr-stmt     -> expr? ";"
// expressions 
expr                -> additive-expr
additive-expr       -> multiplicative-expr
                    |  additive-expr "+" multiplicative-expr
                    |  additive-expr "-" multiplicative-expr
multiplicative-expr -> unary-expr
	            |  multiplicative-expr "*" unary-expr
	            |  multiplicative-expr "/" unary-expr
unary-expr          -> "-" unary-expr
		    |  primary-expr

primary-expr        -> identifier
 		    |  INTLITERAL
		    | "(" expr ")"
 */
/*
 * Yunus Can Bilge
 * yunuscan.bilge@gmail.com
 * Programming Languages and Compilers Assignment 3 
 */

package VC.Parser;

import java.util.Iterator;
import java.util.Stack;

import VC.Scanner.Scanner;
import VC.Scanner.SourcePosition;
import VC.Scanner.Token;
import VC.ErrorReporter;
import VC.ASTs.*;

public class Parser {

	private Scanner scanner;
	private ErrorReporter errorReporter;
	private Token currentToken;
	private SourcePosition previousTokenPosition;
	private SourcePosition dummyPos = new SourcePosition();
	private boolean varDeclOn;
	private Stack<Arg> argStack;
	private Type ttt;
	private Ident iii;
	private boolean paraDeclOn;
	private Type paraDeclOnType;
	private Type compoundttt;
	private Ident compoundiii;
	private Stack<ParaDecl> paraDeclStack;
	private boolean compoundStmt;
	private Stack<Type> s1;
	private Stack<Ident> s2;
	private Stack<List> s3;
	private Stack<Stmt> s4;
	private boolean variablePlaceOn;
	private boolean gloabalVarDeclOn;
	private Type globalVarDeclOnType;

	public Parser(Scanner lexer, ErrorReporter reporter) {
		scanner = lexer;
		errorReporter = reporter;
		paraDeclOnType = null;
		paraDeclOn = false;
		previousTokenPosition = new SourcePosition();
		argStack = new Stack<Arg>();
		currentToken = scanner.getToken();
		varDeclOn = false;
		ttt = null;
		iii = null;
		compoundStmt = false;

		compoundttt = null;
		compoundiii = null;
		paraDeclStack = new Stack<ParaDecl>();
		s1 = new Stack<Type>();
		s2 = new Stack<Ident>();
		s3 = new Stack<List>();
		s4 = new Stack<Stmt>();
		variablePlaceOn = false;
		gloabalVarDeclOn = true;
		globalVarDeclOnType = null;
	}

	// match checks to see f the current token matches tokenExpected.
	// If so, fetches the next token.
	// If not, reports a syntactic error.

	void match(int tokenExpected) throws SyntaxError {
		if (currentToken.kind == tokenExpected) {
			previousTokenPosition = currentToken.position;
			currentToken = scanner.getToken();
		} else {
			syntacticError("\"%\" expected here", Token.spell(tokenExpected));
		}
	}

	void accept() {
		previousTokenPosition = currentToken.position;
		currentToken = scanner.getToken();
	}

	void syntacticError(String messageTemplate, String tokenQuoted)
			throws SyntaxError {
		SourcePosition pos = currentToken.position;
		errorReporter.reportError(messageTemplate, tokenQuoted, pos);
		throw (new SyntaxError());
	}

	// start records the position of the start of a phrase.
	// This is defined to be the position of the first
	// character of the first token of the phrase.

	void start(SourcePosition position) {
		position.lineStart = currentToken.position.lineStart;
		position.charStart = currentToken.position.charStart;
	}

	// finish records the position of the end of a phrase.
	// This is defined to be the position of the last
	// character of the last token of the phrase.

	void finish(SourcePosition position) {
		position.lineFinish = previousTokenPosition.lineFinish;
		position.charFinish = previousTokenPosition.charFinish;
	}

	void copyStart(SourcePosition from, SourcePosition to) {
		to.lineStart = from.lineStart;
		to.charStart = from.charStart;
	}

	// ========================== PROGRAMS ========================
	
	public Program parseProgram() {

		Program programAST = null;
		List dlAST = new EmptyDeclList(dummyPos);
		SourcePosition programPos = new SourcePosition();
		start(programPos);
		List varDecList = new EmptyDeclList(dummyPos);
		// List l = null;
		Decl fAST = null;
		try {

			while (currentToken.kind == Token.VOID
					|| currentToken.kind == Token.BOOLEAN
					|| currentToken.kind == Token.INT
					|| currentToken.kind == Token.FLOAT) {
				Type t = parseType();
				gloabalVarDeclOn = true;
				globalVarDeclOnType = t;
				s1.push(t);
				Ident i = parseIdent();
				s2.push(i);
				ttt = t;
				iii = i;
				if (currentToken.kind == Token.LPAREN) {
					// sonradan ekledim
					varDeclOn = false;
					fAST = null;

					SourcePosition funcPos = new SourcePosition();
					start(funcPos);
					List fplAST = parseParaList();
					s3.push(fplAST);
					Stmt cAST = parseCompoundStmt();
					s4.push(cAST);
					finish(funcPos);

					// s2.add(dlAST);

				} else {
					// varDeclOn = true;
					// varDecList = parseVarDecl();
					if (currentToken.kind == Token.EQ) {
						match(Token.EQ);
						varDecList = parseInitialiser();
					} else {
						varDeclOn = true;
						varDecList = parseInitDeclList();
						varDeclOn = false;
					}
					match(Token.SEMICOLON);
					variablePlaceOn = true;
					// Decl decl = new GlobalVarDecl(t, i,
					// new EmptyExpr(dummyPos), dummyPos); // ParaDecl
					// // parDecl = new
					// // ParaDecl(t,
					// // i, dummyPos);
					//
					// varDecList = new DeclList(decl, varDecList, dummyPos);

					// System.out.println(varDecList.toString());
				}

			}

			if (!s1.empty() && !s2.empty() && !s3.empty() && !s4.empty()) {
				Iterator<Type> typeIt = s1.iterator();
				Iterator<Ident> typeId = s2.iterator();
				Iterator<List> typeIL = s3.iterator();
				Iterator<Stmt> typeSt = s4.iterator();
				while (typeIt.hasNext() && typeId.hasNext() && typeIL.hasNext()
						&& typeSt.hasNext()) {
					Type tS = s1.pop();
					Ident iS = s2.pop();
					List lS = s3.pop();
					Stmt sS = s4.pop();
					fAST = new FuncDecl(tS, iS, lS, sS, dummyPos);
					dlAST = new DeclList(fAST, dlAST, dummyPos);
				}
			}
			if (variablePlaceOn) {
				Iterator<Type> typeIt = s1.iterator();
				Iterator<Ident> typeId = s2.iterator();

				while (typeIt.hasNext() && typeId.hasNext()) {
					Decl decl = new GlobalVarDecl(s1.pop(), s2.pop(),
							new EmptyExpr(dummyPos), dummyPos);
					varDecList = new DeclList(decl, varDecList, dummyPos);
				}
				variablePlaceOn = false;
			}
			
			finish(programPos);
			if (varDecList instanceof EmptyDeclList) {
				programAST = new Program(dlAST, programPos);
			} else if (dlAST instanceof EmptyDeclList) {
				programAST = new Program(varDecList, programPos);
			}
			if (programAST == null) {
				programAST = new Program(new EmptyDeclList(dummyPos),
						programPos);
			}
			if (currentToken.kind != Token.EOF) {
				syntacticError("\"%\" unknown type", currentToken.spelling);
			}

		} catch (SyntaxError s) {
			return null;
		}

		return programAST;
	}

	// TODO FUNDECLARATORN VAR DECLARATON CAGIRMAK YERINE ORADA YAPACAN O KADAR
	// ========================== DECLARATIONS ========================

	List parseFuncDeclList() throws SyntaxError {
		List dlAST = null;
		Decl dAST = null;

		SourcePosition funcPos = new SourcePosition();
		start(funcPos);

		dAST = parseFuncDecl();

		if (currentToken.kind == Token.VOID) {
			dlAST = parseFuncDeclList();
			finish(funcPos);
			dlAST = new DeclList(dAST, dlAST, funcPos);
		} else if (dAST != null) {
			finish(funcPos);
			dlAST = new DeclList(dAST, new EmptyDeclList(dummyPos), funcPos);
		}
		if (dlAST == null)
			dlAST = new EmptyDeclList(dummyPos);

		return dlAST;
	}

	
	List parseVarDecl() throws SyntaxError {
		List lAST = null;
		Type tAST = null;
		Ident iAST = null;
		SourcePosition funcPos = new SourcePosition();
		start(funcPos);
		if (compoundStmt == true) {
			tAST = parseType();
			iAST = parseIdent();
			compoundttt = tAST;
			compoundiii = iAST;
		} else {
			tAST = ttt;
			iAST = iii;
		}
		List l = null;
		if (currentToken.kind == Token.EQ) {
			varDeclOn = true;
			l = parseInitDecl();
			varDeclOn = false;
		} else {
			varDeclOn = true;
			l = parseInitDeclList();
			varDeclOn = false;
		}
		match(Token.SEMICOLON);
		Decl decl = new LocalVarDecl(tAST, iAST, new EmptyExpr(dummyPos),
				dummyPos); // ParaDecl parDecl = new ParaDecl(t, i, dummyPos);
		lAST = new DeclList(decl, l, dummyPos);
		return lAST;
	}

	Decl parseFuncDecl() throws SyntaxError {

		Decl fAST = null;

		SourcePosition funcPos = new SourcePosition();
		start(funcPos);

		Type tAST = parseType();
		Ident iAST = parseIdent();
		List fplAST = parseParaList();
		Stmt cAST = parseCompoundStmt();
		finish(funcPos);
		fAST = new FuncDecl(tAST, iAST, fplAST, cAST, funcPos);
		return fAST;
	}

	// var decleration halledilecek
	// Decl parseVarDecl() throws SyntaxError {
	// Type tAST = parseType();
	// List fplAST = parseInitDeclList();
	// match(Token.SEMICOLON);
	//
	// }

	/*
	 * GlobalVarDecl(Type tAST, Ident idAST, Expr eAST) = LocalVarDecl(Type
	 * tAST, Ident idAST, Expr eAST) = ParaDecl(Type tAST, Ident idAST)
	 * DeclList(Decl dAST, List dlList)
	 */

	
	List parseInitDeclList() throws SyntaxError {
		List l1 = null;
		List l2 = null;
		if (varDeclOn != true) {
			l1 = parseInitDecl();
			while (currentToken.kind == Token.COMMA) {
				varDeclOn = false;

				match(Token.COMMA);
				l1 = parseInitDecl();
			}
		} else {
			varDeclOn = false;
			l1 = new EmptyDeclList(dummyPos);
		}
		while (currentToken.kind == Token.COMMA) {
			varDeclOn = false;
			match(Token.COMMA);
			gloabalVarDeclOn = true;
			l1 = parseInitDecl();
		}
		return l1;
	}

	// OK gibi
	
	List parseInitDecl() throws SyntaxError {
		Decl decl = null;
		List l1 = new EmptyDeclList(dummyPos);
		List l2 = null;
		decl = parseDecl();

		if (currentToken.kind == Token.EQ) {
			match(Token.EQ);
			l1 = parseInitialiser();
		}
		l2 = new DeclList(decl, l1, dummyPos);
		return l2;
	}

	
	Decl parseDecl() throws SyntaxError {

		Decl decl = null;
		Ident i = null;

		Type iAST = null;
		if (varDeclOn != true) {
			i = parseIdent();
			varDeclOn = false;
		}
		if (currentToken.kind == Token.LBRACKET) {
			match(Token.LBRACKET);
			if (currentToken.kind == Token.INTLITERAL) {
				match(Token.INTLITERAL);
				iAST = new IntType(dummyPos);
			}
			match(Token.RBRACKET);
		}
		if (i == null) {
			i = compoundiii;
		}
		if (iAST == null) {
			iAST = compoundttt;

		}
		if (paraDeclOn == true) {
			iAST = paraDeclOnType;
			paraDeclOn = false;
			decl = new ParaDecl(iAST, i, dummyPos);
		} else if (gloabalVarDeclOn) {
			iAST = globalVarDeclOnType;
			decl = new GlobalVarDecl(iAST, i, new EmptyExpr(dummyPos), dummyPos);
			gloabalVarDeclOn = false;
		} else {
			decl = new LocalVarDecl(iAST, i, new EmptyExpr(dummyPos), dummyPos);
		}
		// decl = new ParaDecl(iAST, i, dummyPos);

		return decl;
	}

	// OK gibi
	
	List parseInitialiser() throws SyntaxError {
		Expr exprAST = null;
		List l1 = new EmptyDeclList(dummyPos);
		List l = null;
		switch (currentToken.kind) {
		case Token.LCURLY:
			match(Token.LCURLY);
			exprAST = parseExpr();
			l = new ExprList(exprAST, l1, dummyPos);
			while (currentToken.kind == Token.COMMA) {
				match(Token.COMMA);
				exprAST = parseExpr();
				l = new ExprList(exprAST, l, dummyPos);
			}
			match(Token.RCURLY);
			break;
		default:
			exprAST = parseExpr();
			l = new ExprList(exprAST, l1, dummyPos);
			break;
		}
		return l;
	}

	// ======================== TYPES ==========================
	// OK
	Type parseType() throws SyntaxError {
		Type typeAST = null;

		SourcePosition typePos = new SourcePosition();
		start(typePos);

		switch (currentToken.kind) {
		case Token.VOID:
			match(Token.VOID);
			finish(typePos);
			typeAST = new VoidType(typePos);
			break;
		case Token.BOOLEAN:
			match(Token.BOOLEAN);
			finish(typePos);
			typeAST = new BooleanType(typePos);
			break;
		case Token.INT:
			match(Token.INT);
			finish(typePos);
			typeAST = new IntType(typePos);
			break;
		case Token.FLOAT:
			match(Token.FLOAT);
			finish(typePos);
			typeAST = new FloatType(typePos);
			break;
		}

		return typeAST;
	}

	// ======================= STATEMENTS ==============================
	// TODO parseCompoundStmt bitecek declaration lardan sonra
	// OK gibi
	Stmt parseCompoundStmt() throws SyntaxError {
		Stmt cAST = null;
		List slAST = null;
		List vlAST = null;
		SourcePosition stmtPos = new SourcePosition();
		start(stmtPos);

		match(Token.LCURLY);

		while (currentToken.kind == Token.BOOLEAN
				|| currentToken.kind == Token.VOID
				|| currentToken.kind == Token.INT
				|| currentToken.kind == Token.FLOAT) {
			compoundStmt = true;
			vlAST = parseVarDecl();
			compoundStmt = false;
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
			slAST = parseStmtList();
		}

		// Insert code here to build a DeclList node for variable declarations

		match(Token.RCURLY);
		finish(stmtPos);

		/*
		 * In the subset of the VC grammar, no variable declarations are
		 * allowed. Therefore, a block is empty iff it has no statements.
		 */
		if (slAST == null && vlAST == null) {
			// cAST = new CompoundStmt(new EmptyDeclList(dummyPos),
			// new EmptyStmtList(dummyPos), stmtPos);
			cAST = new EmptyCompStmt(dummyPos);
		} else if (vlAST == null) {
			cAST = new CompoundStmt(new EmptyDeclList(dummyPos), slAST, stmtPos);
		} else if (slAST == null) {
			cAST = new CompoundStmt(vlAST, new EmptyStmtList(dummyPos), stmtPos);
		} else {
			cAST = new CompoundStmt(vlAST, slAST, stmtPos);
		}
		return cAST;
	}

	// BUNU KULLANMA
	List parseStmtList() throws SyntaxError {
		List slAST = null;

		SourcePosition stmtPos = new SourcePosition();
		start(stmtPos);

		if (currentToken.kind != Token.RCURLY) {
			Stmt sAST = parseStmt();
			{
				if (currentToken.kind != Token.RCURLY) {
					slAST = parseStmtList();
					finish(stmtPos);
					slAST = new StmtList(sAST, slAST, stmtPos);
				} else {
					finish(stmtPos);
					slAST = new StmtList(sAST, new EmptyStmtList(dummyPos),
							stmtPos);
				}
			}
		} else
			slAST = new EmptyStmtList(dummyPos);

		return slAST;
	}

	// OK
	Stmt parseStmt() throws SyntaxError {
		Stmt sAST = null;

		switch (currentToken.kind) {
		case Token.CONTINUE:
			sAST = parseContinueStmt();
			break;
		case Token.IF:
			sAST = parseIfStmt();
			break;
		case Token.FOR:
			sAST = parseForStmt();
			break;
		case Token.WHILE:
			sAST = parseWhileStmt();
			break;
		case Token.BREAK:
			sAST = parseBreakStmt();
			break;
		case Token.RETURN:
			sAST = parseReturnStmt();
			break;
		case Token.LCURLY:
			sAST = parseCompoundStmt();
			break;

		default:
			sAST = parseExprStmt();

			break;

		}

		return sAST;
	}

	// OK
	Stmt parseIfStmt() throws SyntaxError {
		Expr expr1 = null;
		Stmt stmt1 = null;
		Stmt stmt2 = null;

		Stmt sAST = null;

		match(Token.IF);
		match(Token.LPAREN);
		expr1 = parseExpr();
		match(Token.RPAREN);
		stmt1 = parseStmt();
		if (currentToken.kind == Token.ELSE) {
			match(Token.ELSE);
			stmt2 = parseStmt();
			sAST = new IfStmt(expr1, stmt1, stmt2, dummyPos);
		} else {
			sAST = new IfStmt(expr1, stmt1, dummyPos);

		}
		return sAST;
	}

	// OK
	Stmt parseForStmt() throws SyntaxError {
		Stmt sAST = null;
		Expr expr1 = null;
		Expr expr2 = null;
		Expr expr3 = null;
		Stmt s2AST = null;
		match(Token.FOR);
		match(Token.LPAREN);
		if (currentToken.kind == Token.PLUS || currentToken.kind == Token.MINUS
				|| currentToken.kind == Token.NOT
				|| currentToken.kind == Token.ID
				|| currentToken.kind == Token.LPAREN
				|| currentToken.kind == Token.INTLITERAL
				|| currentToken.kind == Token.FLOATLITERAL
				|| currentToken.kind == Token.BOOLEANLITERAL
				|| currentToken.kind == Token.STRINGLITERAL) {
			expr1 = parseExpr();
		} else {
			expr1 = new EmptyExpr(dummyPos);
		}
		match(Token.SEMICOLON);
		if (currentToken.kind == Token.PLUS || currentToken.kind == Token.MINUS
				|| currentToken.kind == Token.NOT
				|| currentToken.kind == Token.ID
				|| currentToken.kind == Token.LPAREN
				|| currentToken.kind == Token.INTLITERAL
				|| currentToken.kind == Token.FLOATLITERAL
				|| currentToken.kind == Token.BOOLEANLITERAL
				|| currentToken.kind == Token.STRINGLITERAL) {
			expr2 = parseExpr();
		} else {
			expr2 = new EmptyExpr(dummyPos);
		}
		match(Token.SEMICOLON);
		if (currentToken.kind == Token.PLUS || currentToken.kind == Token.MINUS
				|| currentToken.kind == Token.NOT
				|| currentToken.kind == Token.ID
				|| currentToken.kind == Token.LPAREN
				|| currentToken.kind == Token.INTLITERAL
				|| currentToken.kind == Token.FLOATLITERAL
				|| currentToken.kind == Token.BOOLEANLITERAL
				|| currentToken.kind == Token.STRINGLITERAL) {
			expr3 = parseExpr();
		} else {
			expr3 = new EmptyExpr(dummyPos);
		}
		match(Token.RPAREN);
		s2AST = parseStmt();
		sAST = new ForStmt(expr1, expr2, expr3, s2AST, dummyPos);
		return sAST;
	}

	// OK
	Stmt parseWhileStmt() throws SyntaxError {
		Stmt sAST = null;
		Expr exprAST = null;
		Stmt s2AST = null;
		match(Token.WHILE);
		match(Token.LPAREN);
		exprAST = parseExpr();
		match(Token.RPAREN);
		s2AST = parseStmt();

		sAST = new WhileStmt(exprAST, s2AST, dummyPos);

		return sAST;
	}

	// OK
	Stmt parseBreakStmt() throws SyntaxError {
		Stmt sAST = null;
		match(Token.BREAK);
		match(Token.SEMICOLON);
		sAST = new BreakStmt(dummyPos);
		return sAST;
	}

	// OK
	Stmt parseContinueStmt() throws SyntaxError {
		Stmt sAST = null;
		match(Token.CONTINUE);
		match(Token.SEMICOLON);
		sAST = new ContinueStmt(dummyPos);
		return sAST;
	}

	// OK
	// simdi return dogru sanirim
	Stmt parseReturnStmt() throws SyntaxError {
		Stmt sAST = null;

		SourcePosition stmtPos = new SourcePosition();
		start(stmtPos);

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
			Expr eAST = parseExpr();
			match(Token.SEMICOLON);
			finish(stmtPos);
			sAST = new ReturnStmt(eAST, dummyPos);
		} else {
			match(Token.SEMICOLON);
			finish(stmtPos);
			sAST = new ReturnStmt(new EmptyExpr(dummyPos), dummyPos);
		}
		return sAST;
	}

	// OK
	Stmt parseExprStmt() throws SyntaxError {
		Stmt sAST = null;

		SourcePosition stmtPos = new SourcePosition();
		start(stmtPos);

		if (currentToken.kind == Token.ID
				|| currentToken.kind == Token.INTLITERAL
				|| currentToken.kind == Token.LPAREN
				|| currentToken.kind == Token.MINUS
				|| currentToken.kind == Token.PLUS
				|| currentToken.kind == Token.NOT
				|| currentToken.kind == Token.FLOATLITERAL
				|| currentToken.kind == Token.BOOLEANLITERAL
				|| currentToken.kind == Token.STRINGLITERAL) {
			Expr eAST = parseExpr();
			match(Token.SEMICOLON);
			finish(stmtPos);
			sAST = new ExprStmt(eAST, stmtPos);
		} else {
			match(Token.SEMICOLON);
			finish(stmtPos);
			sAST = new ExprStmt(new EmptyExpr(dummyPos), stmtPos);
		}
		return sAST;
	}

	// ======================= PARAMETERS =======================
	
	// OK gibi
	List parseParaList() throws SyntaxError {
		List formalsAST = null;

		SourcePosition formalsPos = new SourcePosition();
		start(formalsPos);

		match(Token.LPAREN);

		if (currentToken.kind != Token.RPAREN) {
			formalsAST = parseProperParaList();
		} else {
			formalsAST = new EmptyParaList(formalsPos);
		}
		match(Token.RPAREN);
		finish(formalsPos);
		return formalsAST;
	}

	// OK gibi//TODO
	// DeclList(Decl dAST, List dlList)
	List parseProperParaList() throws SyntaxError {
		List l1 = new EmptyParaList(dummyPos);
		// List declList = null;
		ParaDecl decl = null;
		decl = parsParaDecl();
		paraDeclStack.push(decl);

		while (currentToken.kind == Token.COMMA) {
			match(Token.COMMA);
			decl = parsParaDecl();
			paraDeclStack.push(decl);
		}
		Iterator<ParaDecl> declIt = paraDeclStack.iterator();
		List l2 = new ParaList(paraDeclStack.pop(), l1, dummyPos);
		while (declIt.hasNext()) {
			l2 = new ParaList(paraDeclStack.pop(), l2, dummyPos);
		}
		return l2;
	}

	// TODO
	ParaDecl parsParaDecl() throws SyntaxError {
		ParaDecl decl = null;
		Type t = null;
		paraDeclOn = true;
		if (currentToken.kind == Token.VOID) {
			// match(Token.VOID);
			t = parseType();
			paraDeclOnType = t;
			decl = (ParaDecl) parseDecl();
		} else if (currentToken.kind == Token.BOOLEAN) {
			// match(Token.BOOLEAN);
			t = parseType();
			paraDeclOnType = t;
			decl = (ParaDecl) parseDecl();
		} else if (currentToken.kind == Token.INT) {
			// match(Token.INT);
			t = parseType();
			paraDeclOnType = t;
			decl = (ParaDecl) parseDecl();
		} else if (currentToken.kind == Token.FLOAT) {
			// match(Token.FLOAT);
			t = parseType();
			paraDeclOnType = t;
			decl = (ParaDecl) parseDecl();
		}

		// System.out.println("cc = " + compoundttt);
		return decl;
	}

	
	List parseArgList() throws SyntaxError {
		List l = new EmptyArgList(dummyPos);
		match(Token.LPAREN);
		if (currentToken.kind != Token.RPAREN) {
			l = parseProperArgList();
			match(Token.RPAREN);
		} else {
			l = new EmptyArgList(dummyPos);
			match(Token.RPAREN);
		}
		return l;
	}


	List parseProperArgList() throws SyntaxError {
		Arg argAST = null;
		List argList = null;
		List emptyList = new EmptyArgList(dummyPos);
		argAST = parseArg();
		argStack.push(argAST);
		// argList = new ArgList(argAST, emptyList, dummyPos);
		// argList = new ArgList(argAST, emptyList, dummyPos);

		while (currentToken.kind == Token.COMMA) {
			match(Token.COMMA);
			argAST = parseArg();
			argStack.push(argAST);
			// argList = new ArgList(argAST, argList, dummyPos);
		}
		Iterator<Arg> argIt = argStack.iterator();
		argList = new ArgList(argStack.pop(), emptyList, dummyPos);
		while (argIt.hasNext()) {
			argList = new ArgList(argStack.pop(), argList, dummyPos);
		}

		return argList;

	}

	// OK
	Arg parseArg() throws SyntaxError {
		Expr exprAST = null;
		Arg arg = null;
		exprAST = parseExpr();
		arg = new Arg(exprAST, dummyPos);
		return arg;
	}

	// ======================= EXPRESSIONS ======================
	// OK
	Expr parseExpr() throws SyntaxError {
		Expr exprAST = null;
		// exprAST = parseAdditiveExpr();
		exprAST = parseAssignExpr();
		return exprAST;
	}


	Expr parseAssignExpr() throws SyntaxError {
		Expr exprAST = null;
		SourcePosition addStartPos = new SourcePosition();
		start(addStartPos);

		exprAST = parseCondOrExpr();
		while (currentToken.kind == Token.EQ) {
			// almayabilirsin bu operator u
			Operator opAST = acceptOperator();
			Expr e2AST = parseCondOrExpr();

			SourcePosition addPos = new SourcePosition();
			copyStart(addStartPos, addPos);
			finish(addPos);
			// TODO bu assignexpr mi binary expr mi bilemidim hata varsa burada
			// olabilir
			exprAST = new AssignExpr(exprAST, e2AST, dummyPos);
		}
		return exprAST;
	}

	// OK
	Expr parseCondOrExpr() throws SyntaxError {
		Expr exprAST = null;
		SourcePosition addStartPos = new SourcePosition();
		start(addStartPos);

		exprAST = parseCondAndExpr();
		while (currentToken.kind == Token.OROR) {
			Operator opAST = acceptOperator();
			Expr e2AST = parseCondAndExpr();

			SourcePosition addPos = new SourcePosition();
			copyStart(addStartPos, addPos);
			finish(addPos);
			exprAST = new BinaryExpr(exprAST, opAST, e2AST, addPos);
		}
		return exprAST;
	}

	// OK
	Expr parseCondAndExpr() throws SyntaxError {
		Expr exprAST = null;
		SourcePosition addStartPos = new SourcePosition();
		start(addStartPos);

		exprAST = parseEqualityExpr();
		while (currentToken.kind == Token.ANDAND) {
			Operator opAST = acceptOperator();
			Expr e2AST = parseEqualityExpr();

			SourcePosition addPos = new SourcePosition();
			copyStart(addStartPos, addPos);
			finish(addPos);
			exprAST = new BinaryExpr(exprAST, opAST, e2AST, addPos);
		}
		return exprAST;
	}

	// OK
	Expr parseEqualityExpr() throws SyntaxError {
		Expr exprAST = null;
		SourcePosition addStartPos = new SourcePosition();
		start(addStartPos);

		exprAST = parseRelExpr();
		while (currentToken.kind == Token.EQEQ
				|| currentToken.kind == Token.NOTEQ) {
			Operator opAST = acceptOperator();
			Expr e2AST = parseRelExpr();

			SourcePosition addPos = new SourcePosition();
			copyStart(addStartPos, addPos);
			finish(addPos);
			exprAST = new BinaryExpr(exprAST, opAST, e2AST, addPos);
		}
		return exprAST;
	}

	// OK
	Expr parseRelExpr() throws SyntaxError {
		Expr exprAST = null;
		SourcePosition addStartPos = new SourcePosition();
		start(addStartPos);

		exprAST = parseAdditiveExpr();
		while (currentToken.kind == Token.LT || currentToken.kind == Token.LTEQ
				|| currentToken.kind == Token.GT
				|| currentToken.kind == Token.GTEQ) {
			Operator opAST = acceptOperator();
			Expr e2AST = parseAdditiveExpr();

			SourcePosition addPos = new SourcePosition();
			copyStart(addStartPos, addPos);
			finish(addPos);
			exprAST = new BinaryExpr(exprAST, opAST, e2AST, addPos);
		}
		return exprAST;

	}

	// OK
	Expr parseAdditiveExpr() throws SyntaxError {
		Expr exprAST = null;

		SourcePosition addStartPos = new SourcePosition();
		start(addStartPos);

		exprAST = parseMultiplicativeExpr();
		while (currentToken.kind == Token.PLUS
				|| currentToken.kind == Token.MINUS) {
			Operator opAST = acceptOperator();
			Expr e2AST = parseMultiplicativeExpr();

			SourcePosition addPos = new SourcePosition();
			copyStart(addStartPos, addPos);
			finish(addPos);
			exprAST = new BinaryExpr(exprAST, opAST, e2AST, addPos);
		}
		return exprAST;
	}

	Expr parseMultiplicativeExpr() throws SyntaxError {

		Expr exprAST = null;

		SourcePosition multStartPos = new SourcePosition();
		start(multStartPos);

		exprAST = parseUnaryExpr();
		while (currentToken.kind == Token.MULT
				|| currentToken.kind == Token.DIV) {
			Operator opAST = acceptOperator();
			Expr e2AST = parseUnaryExpr();
			SourcePosition multPos = new SourcePosition();
			copyStart(multStartPos, multPos);
			finish(multPos);
			exprAST = new BinaryExpr(exprAST, opAST, e2AST, multPos);
		}
		return exprAST;
	}

	// OK
	Expr parseUnaryExpr() throws SyntaxError {

		Expr exprAST = null;

		SourcePosition unaryPos = new SourcePosition();
		start(unaryPos);

		switch (currentToken.kind) {
		case Token.MINUS: {
			Operator opAST = acceptOperator();
			Expr e2AST = parseUnaryExpr();
			finish(unaryPos);
			exprAST = new UnaryExpr(opAST, e2AST, unaryPos);
		}
			break;
		case Token.PLUS: {
			Operator opAST = acceptOperator();
			Expr e2AST = parseUnaryExpr();
			finish(unaryPos);
			exprAST = new UnaryExpr(opAST, e2AST, unaryPos);
		}
			break;
		case Token.NOT: {
			Operator opAST = acceptOperator();
			Expr e2AST = parseUnaryExpr();
			finish(unaryPos);
			exprAST = new UnaryExpr(opAST, e2AST, unaryPos);
		}
			break;
		default:
			exprAST = parsePrimaryExpr();
			break;

		}
		return exprAST;
	}

	// OK
	Expr parsePrimaryExpr() throws SyntaxError {

		Expr exprAST = null;
		Expr expr = null;
		List l = null;

		SourcePosition primPos = new SourcePosition();
		start(primPos);

		switch (currentToken.kind) {

		case Token.ID:
			Ident iAST = parseIdent();
			finish(primPos);
			if (currentToken.kind == Token.LPAREN) {
				l = parseArgList();
				exprAST = new CallExpr(iAST, l, dummyPos);
			} else if (currentToken.kind == Token.LBRACKET) {
				match(Token.LBRACKET);
				expr = parseExpr();
				match(Token.RBRACKET);
				exprAST = new ArrayExpr(new SimpleVar(iAST, dummyPos), expr,
						dummyPos);
			} else {
				Var simVAST = new SimpleVar(iAST, primPos);
				exprAST = new VarExpr(simVAST, primPos);
			}
			// Var simVAST = new SimpleVar(iAST, primPos);
			// exprAST = new VarExpr(simVAST, primPos);
			break;
		case Token.LPAREN: {
			accept();
			exprAST = parseExpr();
			match(Token.RPAREN);
		}
			break;

		case Token.INTLITERAL:
			IntLiteral ilAST = parseIntLiteral();
			finish(primPos);
			exprAST = new IntExpr(ilAST, primPos);
			break;
		case Token.FLOATLITERAL:
			FloatLiteral flAST = parseFloatLiteral();
			finish(primPos);
			exprAST = new FloatExpr(flAST, primPos);
			break;
		case Token.STRINGLITERAL:
			StringLiteral slAST = parseStringLiteral();
			finish(primPos);
			exprAST = new StringExpr(slAST, primPos);
			break;
		case Token.BOOLEANLITERAL:
			BooleanLiteral blAST = parseBooleanLiteral();
			finish(primPos);
			exprAST = new BooleanExpr(blAST, primPos);
			break;
		default:
			syntacticError("illegal parimary expression", currentToken.spelling);

		}
		return exprAST;
	}

	// ========================== ID, OPERATOR and LITERALS
	// ========================
	// OK
	Ident parseIdent() throws SyntaxError {

		Ident I = null;

		if (currentToken.kind == Token.ID) {
			previousTokenPosition = currentToken.position;
			String spelling = currentToken.spelling;
			I = new Ident(spelling, previousTokenPosition);
			currentToken = scanner.getToken();
		} else
			syntacticError("identifier expected here", "");
		return I;
	}

	// acceptOperator parses an operator, and constructs a leaf AST for it
	// OK
	Operator acceptOperator() throws SyntaxError {
		Operator O = null;

		previousTokenPosition = currentToken.position;
		String spelling = currentToken.spelling;
		O = new Operator(spelling, previousTokenPosition);
		currentToken = scanner.getToken();
		return O;
	}

	// OK
	IntLiteral parseIntLiteral() throws SyntaxError {
		IntLiteral IL = null;

		if (currentToken.kind == Token.INTLITERAL) {
			String spelling = currentToken.spelling;
			accept();
			IL = new IntLiteral(spelling, previousTokenPosition);
		} else
			syntacticError("integer literal expected here", "");
		return IL;
	}

	// OK
	FloatLiteral parseFloatLiteral() throws SyntaxError {
		FloatLiteral FL = null;

		if (currentToken.kind == Token.FLOATLITERAL) {
			String spelling = currentToken.spelling;
			// System.out.println(spelling);
			accept();
			FL = new FloatLiteral(spelling, previousTokenPosition);
		} else
			syntacticError("float literal expected here", "");
		return FL;
	}

	// OK
	BooleanLiteral parseBooleanLiteral() throws SyntaxError {
		BooleanLiteral BL = null;

		if (currentToken.kind == Token.BOOLEANLITERAL) {
			String spelling = currentToken.spelling;
			accept();
			BL = new BooleanLiteral(spelling, previousTokenPosition);
		} else
			syntacticError("boolean literal expected here", "");
		return BL;
	}

	// OK
	StringLiteral parseStringLiteral() throws SyntaxError {
		StringLiteral SL = null;

		if (currentToken.kind == Token.STRINGLITERAL) {
			String spelling = currentToken.spelling;
			accept();
			SL = new StringLiteral(spelling, previousTokenPosition);
		} else {
			syntacticError("String literal expected here", "");
		}
		return SL;
	}

}
