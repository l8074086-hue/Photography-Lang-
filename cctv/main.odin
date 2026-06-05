package cctv

import "core:fmt"
import "core:os"
import "core:strings"
import "core:unicode"
import "core:unicode/utf8"
AST_Kind :: enum {
	String,
	Number,
	Ident,
	Binary,
	Unary,
	Call,
	ExpressionStmt,
	VarStmt,
	Block,
	IfStmt,
	WhileStmt,
}

AST_Node :: struct {
	kind:        AST_Kind,
	text:        string,
	op:          Token_Kind,
	left:        ^AST_Node,
	right:       ^AST_Node,
	else_branch: ^AST_Node,
	next:        ^AST_Node,
}

Token_Kind :: enum {
	Invalid,
	EOF,
	Ident,
	Number,
	String,
	Plus,
	Minus,
	Star,
	Slash,
	LParen,
	RParen,
	LBrace,
	RBrace,
	Semicolon,
	Dot,
	Equal,
	EqualEqual,
	BangEqual,
	Less,
	Greater,
	LessEqual,
	GreaterEqual,
	Bang,
	Comma,
}

Token :: struct {
	kind: Token_Kind,
	text: string,
}

Parser :: struct {
	lex:  ^Lexer,
	cur:  Token,
	peek: Token,
}

Lexer :: struct {
	src:    string,
	pos:    int,
	ch:     rune,
	ch_pos: int,
	start:  int,
}

init :: proc(l: ^Lexer, src: string) {
	l.src = src
	l.pos = 0
	l.ch = ' '
	advance(l)
}

advance :: proc(l: ^Lexer) {
	if l.pos < len(l.src) {
		r, w := utf8.decode_rune_in_string(l.src[l.pos:])
		l.ch_pos = l.pos
		l.pos += w
		l.ch = r
	} else {
		if l.ch != -1 {
			l.ch_pos = l.pos
		}
		l.ch = -1
	}
}

peek :: proc(l: ^Lexer) -> rune {
	if l.pos < len(l.src) {
		r, _ := utf8.decode_rune_in_string(l.src[l.pos:])
		return r
	}
	return -1
}

skip_whitespace :: proc(l: ^Lexer) {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' {
		advance(l)
	}
}

next_token :: proc(l: ^Lexer) -> Token {
	skip_whitespace(l)
	l.start = l.ch_pos

	if l.ch == -1 {
		return Token{.EOF, ""}
	}

	switch l.ch {
	case '+':
		advance(l)
		return Token{.Plus, "+"}
	case '-':
		advance(l)
		return Token{.Minus, "-"}
	case '*':
		advance(l)
		return Token{.Star, "*"}
	case '/':
		advance(l)
		return Token{.Slash, "/"}
	case '(':
		advance(l)
		return Token{.LParen, "("}
	case ')':
		advance(l)
		return Token{.RParen, ")"}
	case '{':
		advance(l)
		return Token{.LBrace, "{"}
	case '}':
		advance(l)
		return Token{.RBrace, "}"}
	case ';':
		advance(l)
		return Token{.Semicolon, ";"}
	case '=':
		if peek(l) == '=' {
			advance(l); advance(l)
			return Token{.EqualEqual, "=="}
		}
		advance(l)
		return Token{.Equal, "="}
	case '!':
		if peek(l) == '=' {
			advance(l); advance(l)
			return Token{.BangEqual, "!="}
		}
		advance(l)
		return Token{.Bang, "!"}
	case '<':
		if peek(l) == '=' {
			advance(l); advance(l)
			return Token{.LessEqual, "<="}
		}
		advance(l)
		return Token{.Less, "<"}
	case '>':
		if peek(l) == '=' {
			advance(l); advance(l)
			return Token{.GreaterEqual, ">="}
		}
		advance(l)
		return Token{.Greater, ">"}
	case ',':
		advance(l)
		return Token{.Comma, ","}
	case '.':
		advance(l)
		return Token{.Dot, "."}
	case '"':
		for {
			advance(l)
			if l.ch == '"' || l.ch == -1 {
				break
			}
		}
		text := l.src[l.start:l.pos]
		advance(l)
		return Token{.String, text}
	}

	if unicode.is_letter(l.ch) || l.ch == '_' {
		for unicode.is_letter(l.ch) || unicode.is_digit(l.ch) || l.ch == '_' {
			advance(l)
		}
		text := l.src[l.start:l.ch_pos]
		return Token{.Ident, text}
	}

	if unicode.is_digit(l.ch) {
		for unicode.is_digit(l.ch) {
			advance(l)
		}
		text := l.src[l.start:l.ch_pos]
		return Token{.Number, text}
	}

	return Token{.Invalid, ""}
}

new_ast_node :: proc(kind: AST_Kind) -> ^AST_Node {
	node := new(AST_Node)
	node.kind = kind
	return node
}

literal_node :: proc(p: ^Parser, kind: AST_Kind) -> ^AST_Node {
	node := new_ast_node(kind)
	node.text = p.cur.text
	advance_token(p)
	return node
}

parse_init :: proc(p: ^Parser, l: ^Lexer) {
	p.lex = l
	advance_token(p)
	advance_token(p)
}

advance_token :: proc(p: ^Parser) {
	p.cur = p.peek
	p.peek = next_token(p.lex)
}

expect :: proc(p: ^Parser, kind: Token_Kind) -> bool {
	if p.cur.kind != kind {return false}
	advance_token(p)
	return true
}

expect_ident :: proc(p: ^Parser) -> string {
	if p.cur.kind != .Ident {return ""}
	text := p.cur.text
	advance_token(p)
	return text
}

parse_call :: proc(p: ^Parser, callee: ^AST_Node) -> ^AST_Node {
	advance_token(p)
	head, tail: ^AST_Node
	if p.cur.kind != .RParen {
		head = parse_assignment(p)
		tail = head
		for p.cur.kind == .Comma {
			advance_token(p)
			arg := parse_assignment(p)
			tail.next = arg
			tail = arg
		}
	}
	expect(p, .RParen)
	node := new_ast_node(.Call)
	node.left = callee
	node.right = head
	return node
}

parse_expression_stmt :: proc(p: ^Parser) -> ^AST_Node {
	expr := parse_assignment(p)
	expect(p, .Semicolon)
	node := new_ast_node(.ExpressionStmt)
	node.left = expr
	return node
}

parse_var_stmt :: proc(p: ^Parser) -> ^AST_Node {
	advance_token(p)
	name := expect_ident(p)
	node := new_ast_node(.VarStmt)
	node.text = name
	if expect(p, .Equal) {
		node.left = parse_assignment(p)
	}
	expect(p, .Semicolon)
	return node
}

parse_block :: proc(p: ^Parser) -> ^AST_Node {
	advance_token(p)
	head, tail: ^AST_Node
	for p.cur.kind != .RBrace && p.cur.kind != .EOF {
		stmt := parse_statement(p)
		if head == nil {
			head = stmt
			tail = stmt
		} else {
			tail.next = stmt
			tail = stmt
		}
	}
	expect(p, .RBrace)
	node := new_ast_node(.Block)
	node.right = head
	return node
}

parse_if_stmt :: proc(p: ^Parser) -> ^AST_Node {
	advance_token(p)
	expect(p, .LParen)
	cond := parse_assignment(p)
	expect(p, .RParen)
	then := parse_statement(p)
	else_branch: ^AST_Node
	if p.cur.kind == .Ident && p.cur.text == "else" {
		advance_token(p)
		else_branch = parse_statement(p)
	}
	node := new_ast_node(.IfStmt)
	node.left = cond
	node.right = then
	node.else_branch = else_branch
	return node
}

parse_while_stmt :: proc(p: ^Parser) -> ^AST_Node {
	advance_token(p)
	expect(p, .LParen)
	cond := parse_assignment(p)
	expect(p, .RParen)
	body := parse_statement(p)
	node := new_ast_node(.WhileStmt)
	node.left = cond
	node.right = body
	return node
}

parse_statement :: proc(p: ^Parser) -> ^AST_Node {
	if p.cur.kind == .Ident {
		switch p.cur.text {
		case "var":
			return parse_var_stmt(p)
		case "if":
			return parse_if_stmt(p)
		case "while":
			return parse_while_stmt(p)
		case "else":
		// handled inside parse_if_stmt, shouldn't reach here
		}
	}
	if p.cur.kind == .LBrace {
		return parse_block(p)
	}
	return parse_expression_stmt(p)
}

parse_factor :: proc(p: ^Parser) -> ^AST_Node {
	#partial switch p.cur.kind {
	case .Number:
		return literal_node(p, .Number)
	case .String:
		return literal_node(p, .String)
	case .Ident:
		node := literal_node(p, .Ident)
		if p.cur.kind == .LParen {
			return parse_call(p, node)
		}
		return node
	case .LParen:
		advance_token(p)
		expr := parse_assignment(p)
		expect(p, .RParen)
		return expr
	case .Minus:
		advance_token(p)
		operand := parse_factor(p)
		node := new_ast_node(.Unary)
		node.op = .Minus
		node.left = operand
		return node
	}
	return nil
}

parse_term :: proc(p: ^Parser) -> ^AST_Node {
	left := parse_factor(p)
	for p.cur.kind == .Star || p.cur.kind == .Slash {
		op := p.cur.kind
		advance_token(p)
		right := parse_factor(p)
		node := new_ast_node(.Binary)
		node.op = op
		node.left = left
		node.right = right
		left = node
	}
	return left
}

parse_comparison :: proc(p: ^Parser) -> ^AST_Node {
	left := parse_term(p)
	for p.cur.kind == .EqualEqual ||
	    p.cur.kind == .BangEqual ||
	    p.cur.kind == .Less ||
	    p.cur.kind == .LessEqual ||
	    p.cur.kind == .Greater ||
	    p.cur.kind == .GreaterEqual {
		op := p.cur.kind
		advance_token(p)
		right := parse_term(p)
		node := new_ast_node(.Binary)
		node.op = op
		node.left = left
		node.right = right
		left = node
	}
	return left
}

parse_expr :: proc(p: ^Parser) -> ^AST_Node {
	left := parse_comparison(p)
	for p.cur.kind == .Plus || p.cur.kind == .Minus {
		op := p.cur.kind
		advance_token(p)
		right := parse_comparison(p)
		node := new_ast_node(.Binary)
		node.op = op
		node.left = left
		node.right = right
		left = node
	}
	return left
}

parse_assignment :: proc(p: ^Parser) -> ^AST_Node {
	left := parse_expr(p)
	if p.cur.kind == .Equal {
		advance_token(p)
		right := parse_assignment(p)
		node := new_ast_node(.Binary)
		node.op = .Equal
		node.left = left
		node.right = right
		return node
	}
	return left
}
print_ast :: proc(node: ^AST_Node, indent: int, label: string = "") {
	for i in 0 ..< indent {
		fmt.printf("  ")
	}
	if label != "" {
		fmt.printf("%s: ", label)
	}
	switch node.kind {
	case .Number, .String, .Ident:
		fmt.printf("%v %q\n", node.kind, node.text)
	case .Unary:
		fmt.printf("Unary %v\n", node.op)
		print_ast(node.left, indent + 1)
	case .Binary:
		fmt.printf("Binary %v\n", node.op)
		print_ast(node.left, indent + 1)
		print_ast(node.right, indent + 1)
	case .Call:
		fmt.printf("Call\n")
		print_ast(node.left, indent + 1, "callee")
		i := 0
		for arg := node.right; arg != nil; arg = arg.next {
			print_ast(arg, indent + 1, fmt.tprintf("arg %d", i))
			i += 1
		}
	case .ExpressionStmt:
		fmt.printf("ExpressionStmt\n")
		print_ast(node.left, indent + 1)
	case .VarStmt:
		fmt.printf("VarStmt %q\n", node.text)
		if node.left != nil {
			print_ast(node.left, indent + 1, "init")
		}
	case .Block:
		fmt.printf("Block\n")
		for stmt := node.right; stmt != nil; stmt = stmt.next {
			print_ast(stmt, indent + 1)
		}
	case .IfStmt:
		fmt.printf("IfStmt\n")
		print_ast(node.left, indent + 1, "cond")
		print_ast(node.right, indent + 1, "then")
		if node.else_branch != nil {
			print_ast(node.else_branch, indent + 1, "else")
		}
	case .WhileStmt:
		fmt.printf("WhileStmt\n")
		print_ast(node.left, indent + 1, "cond")
		print_ast(node.right, indent + 1, "body")
	}
}

main :: proc() {
	data, err := os.read_entire_file("to_parse.shot", context.allocator)
	if err != nil {
		return
	}
	defer delete(data, context.allocator)
	it := string(data)
	lex: Lexer
	init(&lex, it)

	parse: Parser
	parse_init(&parse, &lex)

	head, tail: ^AST_Node
	for parse.cur.kind != .EOF {
		stmt := parse_statement(&parse)
		if head == nil {
			head = stmt
			tail = stmt
		} else {
			tail.next = stmt
			tail = stmt
		}
	}

	for stmt := head; stmt != nil; stmt = stmt.next {
		print_ast(stmt, 0)
		fmt.printf("\n")
	}
}
