//! SuperPascal Parser
//!
//! This crate implements a recursive descent parser for SuperPascal.
//! It builds an AST from tokens produced by the lexer.

use ast::Node;
use errors::{Diagnostic, ParserError, ParserResult};
use lexer::Lexer;
use tokens::{Span, Token, TokenKind};

/// Parser for SuperPascal programs
pub struct Parser {
    lexer: Lexer,
    current: Option<Token>,
    peek: Option<Token>,
    filename: Option<String>,
}

impl Parser {
    /// Create a new parser from source code
    pub fn new(source: &str) -> ParserResult<Self> {
        Self::new_with_file(source, None)
    }

    /// Create a new parser from source code with filename
    pub fn new_with_file(source: &str, filename: Option<String>) -> ParserResult<Self> {
        let lexer = Lexer::new(source);
        let mut parser = Self {
            lexer,
            current: None,
            peek: None,
            filename,
        };
        // Prime the parser with first two tokens
        parser.advance()?;
        parser.advance()?;
        Ok(parser)
    }

    /// Convert a ParserError to an enhanced Diagnostic
    pub fn error_to_diagnostic(&self, error: &ParserError) -> Diagnostic {
        let mut diag = error.to_diagnostic(self.filename.clone());
        
        // Add enhanced context based on error type
        match error {
            ParserError::UnexpectedToken { expected, found, .. } => {
                // Add suggestion for common mistakes
                if expected == "identifier" && found.contains("keyword") {
                    diag = diag.with_suggestion(
                        format!("Keywords cannot be used as identifiers. Use a different name.")
                    );
                } else if expected == ";" && found.contains("end") {
                    diag = diag.with_suggestion(
                        format!("Missing semicolon before 'end'. Add ';' after the previous statement.")
                    );
                }
            }
            ParserError::UnexpectedEof { expected, .. } => {
                diag = diag.with_suggestion(
                    format!("The file ended unexpectedly. Add '{}' before the end of file.", expected)
                );
            }
            ParserError::InvalidSyntax { message, .. } => {
                // Try to provide helpful context
                if message.contains("statement") {
                    diag = diag.with_suggestion(
                        "Check that all statements are properly terminated with semicolons.".to_string()
                    );
                }
            }
        }
        
        diag
    }

    /// Advance to the next token
    fn advance(&mut self) -> ParserResult<()> {
        self.current = self.peek.take();
        match self.lexer.next_token() {
            Ok(token) => {
                self.peek = Some(token);
                Ok(())
            }
            Err(e) => Err(ParserError::InvalidSyntax {
                message: format!("Lexer error: {}", e),
                span: self.current.as_ref().map(|t| t.span).unwrap_or_else(|| Span::at(0, 1, 1)),
            }),
        }
    }

    /// Get the current token
    fn current(&self) -> Option<&Token> {
        self.current.as_ref()
    }

    /// Get the peek token
    fn peek_token(&self) -> Option<&Token> {
        self.peek.as_ref()
    }

    /// Check if current token matches a kind
    fn check(&self, kind: &TokenKind) -> bool {
        self.current()
            .map(|t| &t.kind == kind)
            .unwrap_or(false)
    }

    /// Check if peek token matches a kind
    fn check_peek(&self, kind: &TokenKind) -> bool {
        self.peek_token()
            .map(|t| &t.kind == kind)
            .unwrap_or(false)
    }

    /// Consume current token if it matches, otherwise error
    fn consume(&mut self, kind: TokenKind, expected: &str) -> ParserResult<Token> {
        // Special handling for Identifier - match any identifier
        let matches = if matches!(kind, TokenKind::Identifier(_)) {
            matches!(self.current().map(|t| &t.kind), Some(TokenKind::Identifier(_)))
        } else {
            self.check(&kind)
        };

        if matches {
            let token = self.current().unwrap().clone();
            self.advance()?;
            Ok(token)
        } else {
            let found = self
                .current()
                .map(|t| format!("{:?}", t.kind))
                .unwrap_or_else(|| "EOF".to_string());
            let span = self
                .current()
                .map(|t| t.span)
                .unwrap_or_else(|| Span::at(0, 1, 1));
            Err(ParserError::UnexpectedToken {
                expected: expected.to_string(),
                found,
                span,
            })
        }
    }

    /// Parse a complete program
    pub fn parse(&mut self) -> ParserResult<Node> {
        self.parse_program()
    }

    /// Parse program: PROGRAM identifier ; block .
    fn parse_program(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        // PROGRAM keyword
        self.consume(TokenKind::KwProgram, "PROGRAM")?;

        // Program name
        let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier after PROGRAM".to_string(),
                span: name_token.span,
            }),
        };

        // Semicolon
        self.consume(TokenKind::Semicolon, ";")?;

        // Block
        let block = self.parse_block()?;

        // Period
        self.consume(TokenKind::Dot, ".")?;

        // Check for EOF (allow whitespace/comments after period)
        // Skip any remaining tokens that are just whitespace/comments
        while let Some(token) = self.current() {
            // If we see EOF, we're done
            if matches!(token.kind, TokenKind::Eof) {
                break;
            }
            // Otherwise, there's unexpected content
            return Err(ParserError::InvalidSyntax {
                message: "Unexpected tokens after program end".to_string(),
                span: token.span,
            });
        }

        let span = start_span.merge(block.span());
        Ok(Node::Program(ast::Program {
            name,
            block: Box::new(block),
            span,
        }))
    }

    /// Parse block: [declarations] BEGIN statements END
    fn parse_block(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let mut const_decls = vec![];
        let mut type_decls = vec![];
        let mut var_decls = vec![];
        let mut proc_decls = vec![];
        let mut func_decls = vec![];

        // Parse declarations (const, type, var, procedures, functions)
        loop {
            if self.check(&TokenKind::KwConst) {
                const_decls.extend(self.parse_const_decls()?);
            } else if self.check(&TokenKind::KwType) {
                type_decls.extend(self.parse_type_decls()?);
            } else if self.check(&TokenKind::KwVar) {
                var_decls.extend(self.parse_var_decls()?);
            } else if self.check(&TokenKind::KwProcedure) {
                proc_decls.push(self.parse_procedure_decl()?);
            } else if self.check(&TokenKind::KwFunction) {
                func_decls.push(self.parse_function_decl()?);
            } else {
                break;
            }
        }

        // BEGIN
        self.consume(TokenKind::KwBegin, "BEGIN")?;

        // Statements
        let mut statements = vec![];
        while !self.check(&TokenKind::KwEnd) {
            statements.push(self.parse_statement()?);
            // Optional semicolon between statements
            if self.check(&TokenKind::Semicolon) {
                self.advance()?;
            }
        }

        // END
        let end_token = self.consume(TokenKind::KwEnd, "END")?;
        let span = start_span.merge(end_token.span);

        Ok(Node::Block(ast::Block {
            const_decls,
            type_decls,
            var_decls,
            proc_decls,
            func_decls,
            statements,
            span,
        }))
    }

    /// Parse constant declarations: CONST const_decl { ; const_decl }
    fn parse_const_decls(&mut self) -> ParserResult<Vec<Node>> {
        self.consume(TokenKind::KwConst, "CONST")?;
        let mut decls = vec![];
        loop {
            decls.push(self.parse_const_decl()?);
            if !self.check(&TokenKind::Semicolon) {
                break;
            }
            self.advance()?;
            if !self.check(&TokenKind::Identifier(String::new())) {
                break;
            }
        }
        Ok(decls)
    }

    /// Parse single constant declaration: identifier = expression
    fn parse_const_decl(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                span: name_token.span,
            }),
        };

        self.consume(TokenKind::Equal, "=")?;
        let value = self.parse_expression()?;

        let span = start_span.merge(value.span());
        Ok(Node::ConstDecl(ast::ConstDecl {
            name,
            value: Box::new(value),
            span,
        }))
    }

    /// Parse type declarations: TYPE type_decl { ; type_decl }
    fn parse_type_decls(&mut self) -> ParserResult<Vec<Node>> {
        self.consume(TokenKind::KwType, "TYPE")?;
        let mut decls = vec![];
        loop {
            decls.push(self.parse_type_decl()?);
            if !self.check(&TokenKind::Semicolon) {
                break;
            }
            self.advance()?;
            if !self.check(&TokenKind::Identifier(String::new())) {
                break;
            }
        }
        Ok(decls)
    }

    /// Parse single type declaration: identifier = type
    fn parse_type_decl(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                span: name_token.span,
            }),
        };

        self.consume(TokenKind::Equal, "=")?;
        let type_expr = self.parse_type()?;

        let span = start_span.merge(type_expr.span());
        Ok(Node::TypeDecl(ast::TypeDecl {
            name,
            type_expr: Box::new(type_expr),
            span,
        }))
    }

    /// Parse variable declarations: VAR var_decl { ; var_decl }
    fn parse_var_decls(&mut self) -> ParserResult<Vec<Node>> {
        self.consume(TokenKind::KwVar, "VAR")?;
        let mut decls = vec![];
        loop {
            decls.push(self.parse_var_decl()?);
            if !self.check(&TokenKind::Semicolon) {
                break;
            }
            self.advance()?;
            if !self.check(&TokenKind::Identifier(String::new())) {
                break;
            }
        }
        Ok(decls)
    }

    /// Parse single variable declaration: identifier_list : type
    fn parse_var_decl(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let mut names = vec![];
        loop {
            let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
            let name = match &name_token.kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => return Err(ParserError::InvalidSyntax {
                    message: "Expected identifier".to_string(),
                    span: name_token.span,
                }),
            };
            names.push(name);

            if !self.check(&TokenKind::Comma) {
                break;
            }
            self.advance()?;
        }

        self.consume(TokenKind::Colon, ":")?;
        let type_expr = self.parse_type()?;

        let span = start_span.merge(type_expr.span());
        Ok(Node::VarDecl(ast::VarDecl {
            names,
            type_expr: Box::new(type_expr),
            span,
        }))
    }

    /// Parse procedure declaration: PROCEDURE identifier [ ( params ) ] ; block ;
    fn parse_procedure_decl(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwProcedure, "PROCEDURE")?;

        let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                span: name_token.span,
            }),
        };

        let params = if self.check(&TokenKind::LeftParen) {
            self.parse_params()?
        } else {
            vec![]
        };

        self.consume(TokenKind::Semicolon, ";")?;
        let block = self.parse_block()?;
        self.consume(TokenKind::Semicolon, ";")?;

        let span = start_span.merge(block.span());
        Ok(Node::ProcDecl(ast::ProcDecl {
            name,
            params,
            block: Box::new(block),
            span,
        }))
    }

    /// Parse function declaration: FUNCTION identifier [ ( params ) ] : type ; block ;
    fn parse_function_decl(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwFunction, "FUNCTION")?;

        let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                span: name_token.span,
            }),
        };

        let params = if self.check(&TokenKind::LeftParen) {
            self.parse_params()?
        } else {
            vec![]
        };

        self.consume(TokenKind::Colon, ":")?;
        let return_type = self.parse_type()?;
        self.consume(TokenKind::Semicolon, ";")?;
        let block = self.parse_block()?;
        self.consume(TokenKind::Semicolon, ";")?;

        let span = start_span.merge(block.span());
        Ok(Node::FuncDecl(ast::FuncDecl {
            name,
            params,
            return_type: Box::new(return_type),
            block: Box::new(block),
            span,
        }))
    }

    /// Parse parameter list: ( param { ; param } )
    fn parse_params(&mut self) -> ParserResult<Vec<ast::Param>> {
        self.consume(TokenKind::LeftParen, "(")?;
        let mut params = vec![];

        if !self.check(&TokenKind::RightParen) {
            loop {
                params.push(self.parse_param()?);
                if !self.check(&TokenKind::Semicolon) {
                    break;
                }
                self.advance()?;
            }
        }

        self.consume(TokenKind::RightParen, ")")?;
        Ok(params)
    }

    /// Parse parameter: [ VAR | CONST ] identifier_list : type
    fn parse_param(&mut self) -> ParserResult<ast::Param> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let param_type = if self.check(&TokenKind::KwVar) {
            self.advance()?;
            ast::ParamType::Var
        } else if self.check(&TokenKind::KwConst) {
            self.advance()?;
            ast::ParamType::Const
        } else {
            ast::ParamType::Value
        };

        let mut names = vec![];
        loop {
            let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
            let name = match &name_token.kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => return Err(ParserError::InvalidSyntax {
                    message: "Expected identifier".to_string(),
                    span: name_token.span,
                }),
            };
            names.push(name);

            if !self.check(&TokenKind::Comma) {
                break;
            }
            self.advance()?;
        }

        self.consume(TokenKind::Colon, ":")?;
        let type_expr = self.parse_type()?;

        let span = start_span.merge(type_expr.span());
        Ok(ast::Param {
            names,
            param_type,
            type_expr: Box::new(type_expr),
            span,
        })
    }

    /// Parse type: identifier | ARRAY [ index_type ] OF element_type | RECORD field_list END
    fn parse_type(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        if self.check(&TokenKind::KwArray) {
            self.advance()?;
            self.consume(TokenKind::LeftBracket, "[")?;
            let index_type = self.parse_type()?;
            self.consume(TokenKind::RightBracket, "]")?;
            self.consume(TokenKind::KwOf, "OF")?;
            let element_type = self.parse_type()?;
            let span = start_span.merge(element_type.span());
            Ok(Node::ArrayType(ast::ArrayType {
                index_type: Box::new(index_type),
                element_type: Box::new(element_type),
                span,
            }))
        } else if self.check(&TokenKind::KwRecord) {
            self.advance()?;
            let mut fields = vec![];
            while !self.check(&TokenKind::KwEnd) {
                fields.push(self.parse_field_decl()?);
                self.consume(TokenKind::Semicolon, ";")?;
            }
            let end_token = self.consume(TokenKind::KwEnd, "END")?;
            let span = start_span.merge(end_token.span);
            Ok(Node::RecordType(ast::RecordType {
                fields,
                span,
            }))
        } else {
            let name_token = self.consume(TokenKind::Identifier(String::new()), "type identifier")?;
            let name = match &name_token.kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => return Err(ParserError::InvalidSyntax {
                    message: "Expected type identifier".to_string(),
                    span: name_token.span,
                }),
            };
            Ok(Node::NamedType(ast::NamedType {
                name,
                span: name_token.span,
            }))
        }
    }

    /// Parse field declaration: identifier_list : type
    fn parse_field_decl(&mut self) -> ParserResult<ast::FieldDecl> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let mut names = vec![];
        loop {
            let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
            let name = match &name_token.kind {
                TokenKind::Identifier(name) => name.clone(),
                _ => return Err(ParserError::InvalidSyntax {
                    message: "Expected identifier".to_string(),
                    span: name_token.span,
                }),
            };
            names.push(name);

            if !self.check(&TokenKind::Comma) {
                break;
            }
            self.advance()?;
        }

        self.consume(TokenKind::Colon, ":")?;
        let type_expr = self.parse_type()?;

        let span = start_span.merge(type_expr.span());
        Ok(ast::FieldDecl {
            names,
            type_expr: Box::new(type_expr),
            span,
        })
    }

    /// Parse statement
    fn parse_statement(&mut self) -> ParserResult<Node> {
        if self.check(&TokenKind::KwIf) {
            self.parse_if_statement()
        } else if self.check(&TokenKind::KwWhile) {
            self.parse_while_statement()
        } else if self.check(&TokenKind::KwFor) {
            self.parse_for_statement()
        } else if self.check(&TokenKind::KwRepeat) {
            self.parse_repeat_statement()
        } else if self.check(&TokenKind::KwCase) {
            self.parse_case_statement()
        } else if matches!(self.current().map(|t| &t.kind), Some(TokenKind::Identifier(_))) {
            // Could be assignment or procedure call
            if self.check_peek(&TokenKind::Assign) {
                self.parse_assignment_statement()
            } else {
                self.parse_call_statement()
            }
        } else {
            let span = self
                .current()
                .map(|t| t.span)
                .unwrap_or_else(|| Span::at(0, 1, 1));
            Err(ParserError::InvalidSyntax {
                message: "Expected statement".to_string(),
                span,
            })
        }
    }

    /// Parse if statement: IF expression THEN statement [ ELSE statement ]
    fn parse_if_statement(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwIf, "IF")?;
        let condition = self.parse_expression()?;
        self.consume(TokenKind::KwThen, "THEN")?;
        let then_block = self.parse_statement()?;

        let else_block = if self.check(&TokenKind::KwElse) {
            self.advance()?;
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        let span = if let Some(ref else_block) = else_block {
            start_span.merge(else_block.span())
        } else {
            start_span.merge(then_block.span())
        };

        Ok(Node::IfStmt(ast::IfStmt {
            condition: Box::new(condition),
            then_block: Box::new(then_block),
            else_block,
            span,
        }))
    }

    /// Parse while statement: WHILE expression DO statement
    fn parse_while_statement(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwWhile, "WHILE")?;
        let condition = self.parse_expression()?;
        self.consume(TokenKind::KwDo, "DO")?;
        let body = self.parse_statement()?;

        let span = start_span.merge(body.span());
        Ok(Node::WhileStmt(ast::WhileStmt {
            condition: Box::new(condition),
            body: Box::new(body),
            span,
        }))
    }

    /// Parse for statement: FOR identifier := expression TO|DOWNTO expression DO statement
    fn parse_for_statement(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwFor, "FOR")?;
        let var_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let var_name = match &var_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                span: var_token.span,
            }),
        };

        self.consume(TokenKind::Assign, ":=")?;
        let start_expr = self.parse_expression()?;

        let direction = if self.check(&TokenKind::KwTo) {
            self.advance()?;
            ast::ForDirection::To
        } else if self.check(&TokenKind::KwDownto) {
            self.advance()?;
            ast::ForDirection::Downto
        } else {
            return Err(ParserError::UnexpectedToken {
                expected: "TO or DOWNTO".to_string(),
                found: format!("{:?}", self.current().map(|t| &t.kind)),
                span: self.current().map(|t| t.span).unwrap_or_else(|| Span::at(0, 1, 1)),
            });
        };

        let end_expr = self.parse_expression()?;
        self.consume(TokenKind::KwDo, "DO")?;
        let body = self.parse_statement()?;

        let span = start_span.merge(body.span());
        Ok(Node::ForStmt(ast::ForStmt {
            var_name,
            start_expr: Box::new(start_expr),
            direction,
            end_expr: Box::new(end_expr),
            body: Box::new(body),
            span,
        }))
    }

    /// Parse repeat statement: REPEAT statements UNTIL expression
    fn parse_repeat_statement(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwRepeat, "REPEAT")?;
        let mut statements = vec![];
        while !self.check(&TokenKind::KwUntil) {
            statements.push(self.parse_statement()?);
            if self.check(&TokenKind::Semicolon) {
                self.advance()?;
            }
        }
        self.consume(TokenKind::KwUntil, "UNTIL")?;
        let condition = self.parse_expression()?;

        let span = start_span.merge(condition.span());
        Ok(Node::RepeatStmt(ast::RepeatStmt {
            statements,
            condition: Box::new(condition),
            span,
        }))
    }

    /// Parse case statement: CASE expression OF case_branch { ; case_branch } [ ELSE statement ] END
    fn parse_case_statement(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        self.consume(TokenKind::KwCase, "CASE")?;
        let expr = self.parse_expression()?;
        self.consume(TokenKind::KwOf, "OF")?;

        let mut cases = vec![];
        while !self.check(&TokenKind::KwElse) && !self.check(&TokenKind::KwEnd) {
            cases.push(self.parse_case_branch()?);
            if self.check(&TokenKind::Semicolon) {
                self.advance()?;
            }
        }

        let else_branch = if self.check(&TokenKind::KwElse) {
            self.advance()?;
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        let end_token = self.consume(TokenKind::KwEnd, "END")?;
        let span = start_span.merge(end_token.span);

        Ok(Node::CaseStmt(ast::CaseStmt {
            expr: Box::new(expr),
            cases,
            else_branch,
            span,
        }))
    }

    /// Parse case branch: case_value_list : statement
    fn parse_case_branch(&mut self) -> ParserResult<ast::CaseBranch> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let mut values = vec![];
        loop {
            values.push(self.parse_expression()?);
            if !self.check(&TokenKind::Comma) {
                break;
            }
            self.advance()?;
        }

        self.consume(TokenKind::Colon, ":")?;
        let statement = self.parse_statement()?;

        let span = start_span.merge(statement.span());
        Ok(ast::CaseBranch {
            values,
            statement: Box::new(statement),
            span,
        })
    }

    /// Parse assignment statement: lvalue := expression
    fn parse_assignment_statement(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let target = self.parse_lvalue()?;
        self.consume(TokenKind::Assign, ":=")?;
        let value = self.parse_expression()?;

        let span = start_span.merge(value.span());
        Ok(Node::AssignStmt(ast::AssignStmt {
            target: Box::new(target),
            value: Box::new(value),
            span,
        }))
    }

    /// Parse call statement: identifier [ ( args ) ]
    fn parse_call_statement(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                span: name_token.span,
            }),
        };

        let args = if self.check(&TokenKind::LeftParen) {
            self.parse_args()?
        } else {
            vec![]
        };

        let span = if let Some(last_arg) = args.last() {
            start_span.merge(last_arg.span())
        } else {
            name_token.span
        };

        Ok(Node::CallStmt(ast::CallStmt {
            name,
            args,
            span,
        }))
    }

    /// Parse lvalue: identifier [ [ expression ] ] [ . identifier ]
    fn parse_lvalue(&mut self) -> ParserResult<Node> {
        let name_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
        let name = match &name_token.kind {
            TokenKind::Identifier(name) => name.clone(),
            _ => return Err(ParserError::InvalidSyntax {
                message: "Expected identifier".to_string(),
                span: name_token.span,
            }),
        };

        let mut expr: Node = Node::IdentExpr(ast::IdentExpr {
            name,
            span: name_token.span,
        });

        // Parse array indexing and field access
        loop {
            if self.check(&TokenKind::LeftBracket) {
                self.advance()?;
                let index = self.parse_expression()?;
                self.consume(TokenKind::RightBracket, "]")?;
                let span = expr.span().merge(index.span());
                expr = Node::IndexExpr(ast::IndexExpr {
                    array: Box::new(expr),
                    index: Box::new(index),
                    span,
                });
            } else if self.check(&TokenKind::Dot) {
                self.advance()?;
                let field_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
                let field = match &field_token.kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => return Err(ParserError::InvalidSyntax {
                        message: "Expected identifier".to_string(),
                        span: field_token.span,
                    }),
                };
                let span = expr.span().merge(field_token.span);
                expr = Node::FieldExpr(ast::FieldExpr {
                    record: Box::new(expr),
                    field,
                    span,
                });
            } else {
                break;
            }
        }

        Ok(expr)
    }

    /// Parse expression (using Pratt parser for precedence)
    fn parse_expression(&mut self) -> ParserResult<Node> {
        self.parse_expression_precedence(0)
    }

    /// Parse expression with precedence (Pratt parser)
    fn parse_expression_precedence(&mut self, min_precedence: u8) -> ParserResult<Node> {
        // Parse left operand (prefix)
        let mut left = self.parse_prefix()?;

        // Parse binary operators (infix)
        while let Some(op) = self.parse_binary_operator() {
            let precedence = self.get_precedence(&op);
            if precedence < min_precedence {
                break;
            }
            self.advance()?;
            let right = self.parse_expression_precedence(precedence + 1)?;
            let span = left.span().merge(right.span());
            left = Node::BinaryExpr(ast::BinaryExpr {
                op,
                left: Box::new(left),
                right: Box::new(right),
                span,
            });
        }

        Ok(left)
    }

    /// Parse prefix expression (unary operators, literals, identifiers, etc.)
    fn parse_prefix(&mut self) -> ParserResult<Node> {
        let start_span = self
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| Span::at(0, 1, 1));

        let token_kind = self.current().map(|t| t.kind.clone());
        match token_kind.as_ref() {
            Some(TokenKind::IntegerLiteral { value, .. }) => {
                let token = self.current().unwrap().clone();
                let value = *value;
                self.advance()?;
                Ok(Node::LiteralExpr(ast::LiteralExpr {
                    value: ast::LiteralValue::Integer(value),
                    span: token.span,
                }))
            }
            Some(TokenKind::CharLiteral(value)) => {
                let token = self.current().unwrap().clone();
                let value = *value;
                self.advance()?;
                Ok(Node::LiteralExpr(ast::LiteralExpr {
                    value: ast::LiteralValue::Char(value),
                    span: token.span,
                }))
            }
            Some(TokenKind::StringLiteral(value)) => {
                let token = self.current().unwrap().clone();
                let value_clone = value.clone();
                self.advance()?;
                Ok(Node::LiteralExpr(ast::LiteralExpr {
                    value: ast::LiteralValue::String(value_clone),
                    span: token.span,
                }))
            }
            Some(TokenKind::BooleanLiteral(value)) => {
                let token = self.current().unwrap().clone();
                let value = *value;
                self.advance()?;
                Ok(Node::LiteralExpr(ast::LiteralExpr {
                    value: ast::LiteralValue::Boolean(value),
                    span: token.span,
                }))
            }
            Some(TokenKind::Plus) => {
                self.advance()?;
                let expr = self.parse_prefix()?;
                let span = start_span.merge(expr.span());
                Ok(Node::UnaryExpr(ast::UnaryExpr {
                    op: ast::UnaryOp::Plus,
                    expr: Box::new(expr),
                    span,
                }))
            }
            Some(TokenKind::Minus) => {
                self.advance()?;
                let expr = self.parse_prefix()?;
                let span = start_span.merge(expr.span());
                Ok(Node::UnaryExpr(ast::UnaryExpr {
                    op: ast::UnaryOp::Minus,
                    expr: Box::new(expr),
                    span,
                }))
            }
            Some(TokenKind::KwNot) => {
                self.advance()?;
                let expr = self.parse_prefix()?;
                let span = start_span.merge(expr.span());
                Ok(Node::UnaryExpr(ast::UnaryExpr {
                    op: ast::UnaryOp::Not,
                    expr: Box::new(expr),
                    span,
                }))
            }
            Some(TokenKind::LeftParen) => {
                self.advance()?;
                let expr = self.parse_expression()?;
                self.consume(TokenKind::RightParen, ")")?;
                Ok(expr)
            }
            Some(TokenKind::Identifier(_)) => {
                // Could be identifier, function call, or array/record access
                let name_token = self.current().unwrap().clone();
                let name = match &name_token.kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => unreachable!(),
                };
                self.advance()?;

                if self.check(&TokenKind::LeftParen) {
                    // Function call
                    let args = self.parse_args()?;
                    let span = if let Some(last_arg) = args.last() {
                        name_token.span.merge(last_arg.span())
                    } else {
                        name_token.span
                    };
                    Ok(Node::CallExpr(ast::CallExpr {
                        name,
                        args,
                        span,
                    }))
                } else {
                    // Start with identifier, then parse postfix (indexing, field access)
                    let mut expr: Node = Node::IdentExpr(ast::IdentExpr {
                        name,
                        span: name_token.span,
                    });
                    expr = self.parse_postfix(expr)?;
                    Ok(expr)
                }
            }
            _ => {
                let span = self
                    .current()
                    .map(|t| t.span)
                    .unwrap_or_else(|| Span::at(0, 1, 1));
                Err(ParserError::InvalidSyntax {
                    message: "Expected expression".to_string(),
                    span,
                })
            }
        }
    }

    /// Parse postfix (array indexing, field access)
    fn parse_postfix(&mut self, mut expr: Node) -> ParserResult<Node> {
        loop {
            if self.check(&TokenKind::LeftBracket) {
                self.advance()?;
                let index = self.parse_expression()?;
                self.consume(TokenKind::RightBracket, "]")?;
                let span = expr.span().merge(index.span());
                expr = Node::IndexExpr(ast::IndexExpr {
                    array: Box::new(expr),
                    index: Box::new(index),
                    span,
                });
            } else if self.check(&TokenKind::Dot) {
                self.advance()?;
                let field_token = self.consume(TokenKind::Identifier(String::new()), "identifier")?;
                let field = match &field_token.kind {
                    TokenKind::Identifier(name) => name.clone(),
                    _ => return Err(ParserError::InvalidSyntax {
                        message: "Expected identifier".to_string(),
                        span: field_token.span,
                    }),
                };
                let span = expr.span().merge(field_token.span);
                expr = Node::FieldExpr(ast::FieldExpr {
                    record: Box::new(expr),
                    field,
                    span,
                });
            } else {
                break;
            }
        }
        Ok(expr)
    }

    /// Parse binary operator (if present)
    fn parse_binary_operator(&self) -> Option<ast::BinaryOp> {
        match self.current().map(|t| &t.kind) {
            Some(TokenKind::Plus) => Some(ast::BinaryOp::Add),
            Some(TokenKind::Minus) => Some(ast::BinaryOp::Subtract),
            Some(TokenKind::Star) => Some(ast::BinaryOp::Multiply),
            Some(TokenKind::Slash) => Some(ast::BinaryOp::Divide),
            Some(TokenKind::KwDiv) => Some(ast::BinaryOp::Div),
            Some(TokenKind::KwMod) => Some(ast::BinaryOp::Mod),
            Some(TokenKind::Equal) => Some(ast::BinaryOp::Equal),
            Some(TokenKind::NotEqual) => Some(ast::BinaryOp::NotEqual),
            Some(TokenKind::Less) => Some(ast::BinaryOp::Less),
            Some(TokenKind::LessEqual) => Some(ast::BinaryOp::LessEqual),
            Some(TokenKind::Greater) => Some(ast::BinaryOp::Greater),
            Some(TokenKind::GreaterEqual) => Some(ast::BinaryOp::GreaterEqual),
            Some(TokenKind::KwAnd) => Some(ast::BinaryOp::And),
            Some(TokenKind::KwOr) => Some(ast::BinaryOp::Or),
            _ => None,
        }
    }

    /// Get operator precedence
    fn get_precedence(&self, op: &ast::BinaryOp) -> u8 {
        match op {
            // Logical operators (lowest precedence)
            ast::BinaryOp::Or => 1,
            ast::BinaryOp::And => 2,
            // Relational operators
            ast::BinaryOp::Equal | ast::BinaryOp::NotEqual | ast::BinaryOp::Less
            | ast::BinaryOp::LessEqual | ast::BinaryOp::Greater | ast::BinaryOp::GreaterEqual => 3,
            // Additive operators
            ast::BinaryOp::Add | ast::BinaryOp::Subtract => 4,
            // Multiplicative operators (highest precedence)
            ast::BinaryOp::Multiply | ast::BinaryOp::Divide | ast::BinaryOp::Div | ast::BinaryOp::Mod => 5,
        }
    }

    /// Parse argument list: ( expression { , expression } )
    fn parse_args(&mut self) -> ParserResult<Vec<Node>> {
        self.consume(TokenKind::LeftParen, "(")?;
        let mut args = vec![];

        if !self.check(&TokenKind::RightParen) {
            loop {
                args.push(self.parse_expression()?);
                if !self.check(&TokenKind::Comma) {
                    break;
                }
                self.advance()?;
            }
        }

        self.consume(TokenKind::RightParen, ")")?;
        Ok(args)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_program() {
        let source = r#"
            program Hello;
            begin
                writeln('Hello, World!');
            end.
        "#;
        let mut parser = Parser::new(source).unwrap();
        let result = parser.parse();
        if let Err(e) = &result {
            eprintln!("Parse error: {}", e);
        }
        assert!(result.is_ok(), "Parse failed: {:?}", result);
    }

    #[test]
    fn test_enhanced_error_diagnostics() {
        // Test parsing an invalid program to generate an error
        let source = r#"
            program Test;
            begin
                x :=  // Missing expression
            end.
        "#;
        let mut parser = Parser::new_with_file(source, Some("test.pas".to_string())).unwrap();
        let result = parser.parse();
        
        // Should fail with an error
        assert!(result.is_err());
        
        if let Err(error) = result {
            // Convert to enhanced diagnostic
            let diag = parser.error_to_diagnostic(&error);
            
            // Check FPC format
            let fpc_format = diag.format_fpc();
            assert!(fpc_format.contains("test.pas"));
            assert!(fpc_format.contains("Error:"));
            
            // Check enhanced format
            let enhanced = diag.format_enhanced();
            assert!(enhanced.contains("test.pas"));
            assert!(enhanced.contains("Error:"));
            // Should have suggestion if available
            if diag.suggestion.is_some() {
                assert!(enhanced.contains("Suggestion:"));
            }
        }
    }

    #[test]
    fn test_parser_with_filename() {
        let source = "program Test; begin end.";
        let parser = Parser::new_with_file(source, Some("myfile.pas".to_string()));
        assert!(parser.is_ok());
        let parser = parser.unwrap();
        assert_eq!(parser.filename, Some("myfile.pas".to_string()));
    }
}
