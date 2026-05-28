package cctv

import "core:fmt"
import "core:unicode"
import "core:unicode/utf8"

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
}

Token :: struct {
	kind: Token_Kind,
	text: string,
}

Lexer :: struct {
	src:      string,
	pos:      int,
	ch:       rune,
	ch_pos: int,
	start:    int,
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

main :: proc() {
	lex: Lexer
	init(&lex, `hello 123 3.14 "foo" + - * / ( ) { } ; obj.field`)

	for {
		tok := next_token(&lex)
		fmt.printf("%v %q\n", tok.kind, tok.text)
		if tok.kind == .EOF {
			break
		}
	}
}
