use std::mem;

use crate::lexer::{self, token::Token};

use self::error::{Error, Result};

pub mod ast;
pub mod error;

pub struct Parser<'a> {
    lexer: lexer::Lexer<'a>,

    current: Token,
    peek: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: lexer::Lexer<'a>) -> Result<Self> {
        let mut parser = Parser {
            lexer,

            current: Token::Eof,
            peek: Token::Eof,
        };

        // Fill current and peek tokens
        parser.next_token()?;
        parser.next_token()?;

        Ok(parser)
    }

    fn next_token(&mut self) -> Result<()> {
        // Swap peek into current value
        mem::swap(&mut self.current, &mut self.peek);

        // Set peek to next token in lexer
        self.peek = self.lexer.next_token().map_err(Error::LexerError)?;

        Ok(())
    }

    pub fn parse(&mut self) -> Result<ast::Program> {
        let mut prog = ast::Program::new();

        while self.current != Token::Eof {
            let stmt = self.parse_statement()?;
            prog.push(stmt);

            self.next_token()?;
        }

        Ok(prog)
    }

    fn parse_statement(&mut self) -> Result<ast::Statement> {
        match self.current {
            Token::Let => self.parse_declare_statement(false),
            Token::Const => self.parse_declare_statement(true),
            Token::Return => self.parse_return_statement(),
            Token::Function => self.parse_function_declare_statement(),

            _ => self.parse_expression_statement(),
        }
    }

    fn expect(&mut self, token: Token) -> Result<()> {
        self.next_token()?;

        if self.current == token {
            Ok(())
        } else {
            Err(Error::UnexpectedToken {
                want: format!("{}", token),
                got: format!("{}", &self.current),
            })
        }
    }

    fn parse_function_declare_statement(&mut self) -> Result<ast::Statement> {
        println!("{}", self.current);

        self.next_token()?;
        let function_name = self.parse_identifier_name()?;

        // Get parameter list
        self.expect(Token::LeftParen)?;
        let parameters = self.parse_function_parameters()?;
        self.expect(Token::LeftBrace)?;

        let body = if let ast::Statement::Block(block) = self.parse_block_statement()? {
            block
        } else {
            return Err(Error::UnexpectedToken {
                want: "function body block statement".to_string(),
                got: format!("{}", &self.current),
            });
        };

        Ok(ast::Statement::Let(ast::DeclareStatement { 
            name: function_name,
            value: ast::Expression::Function(ast::FunctionLiteral {
                parameters,
                body,
            })
        }))

        // Ok(ast::Expression::Function(ast::FunctionLiteral {
        //     parameters,
        //     body,
        // }))
    }

    fn parse_declare_statement(&mut self, constant: bool) -> Result<ast::Statement> {
        self.next_token()?;

        let name = self.parse_identifier_name()?;

        self.expect(Token::Assign)?;
        self.next_token()?;

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek == Token::Semicolon {
            self.next_token()?;
        }

        Ok(if constant {
            ast::Statement::Const(ast::DeclareStatement { name, value })
        } else {
            ast::Statement::Let(ast::DeclareStatement { name, value })
        })
    }

    fn parse_return_statement(&mut self) -> Result<ast::Statement> {
        self.next_token()?;

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek == Token::Semicolon {
            self.next_token()?;
        }

        Ok(ast::Statement::Return(ast::ReturnStatement { value }))
    }

    /// Parses an expression
    fn parse_expression(&mut self, prec: Precedence) -> Result<ast::Expression> {
        let mut left = self.prefix_parse()?;

        // Continue until lower precedence operator
        let prec_val = prec as u32;
        while self.peek != Token::Semicolon && prec_val < (precedence(&self.peek) as u32) {
            match self.infix_parse(&left) {
                Some(infix) => left = infix?,
                None => {
                    return Ok(left);
                }
            };
        }

        Ok(left)
    }

    fn parse_expression_statement(&mut self) -> Result<ast::Statement> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek == Token::Semicolon {
            self.next_token()?;
        }

        Ok(ast::Statement::Expression(expr))
    }

    fn parse_identifier_name(&self) -> Result<String> {
        // Try to find an identifier for the expression
        if let Token::Identifier(name) = &self.current {
            Ok(name.to_string())
        } else {
            Err(Error::UnexpectedToken {
                want: "identifier".to_string(),
                got: format!("{}", &self.current),
            })
        }
    }

    fn infix_parse(&mut self, left: &ast::Expression) -> Option<Result<ast::Expression>> {
        match self.peek {
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Slash
            | Token::Percent
            | Token::Equal
            | Token::NotEqual
            | Token::LessThan
            | Token::GreaterThan => Some(self.parse_infix_expression(left)),
            Token::LeftParen => Some(self.parse_call_expression(left)),
            Token::LeftBracket => Some(self.parse_index_expression(left)),
            Token::Assign => Some(self.parse_assign_expression(left)),

            // No infix parsing function
            _ => None,
        }
    }

    fn parse_call_expression(&mut self, left: &ast::Expression) -> Result<ast::Expression> {
        self.next_token()?;

        // Are there zero arguments?
        if self.peek == Token::RightParen {
            self.next_token()?;
            return Ok(ast::Expression::Call(ast::CallExpression {
                function: Box::new(left.clone()),
                arguments: vec![],
            }));
        }

        let arguments = self.parse_expression_list(Token::RightParen)?;

        Ok(ast::Expression::Call(ast::CallExpression {
            function: Box::new(left.clone()),
            arguments,
        }))
    }

    fn parse_infix_expression(&mut self, left: &ast::Expression) -> Result<ast::Expression> {
        // Advance past left expression
        self.next_token()?;

        // Capture operator and determine its precedence
        let operator = self.current.clone();

        let prec = precedence(&self.current);
        self.next_token()?;

        // Parse right expression
        let right = Box::new(self.parse_expression(prec)?);

        Ok(ast::Expression::Infix(ast::InfixExpression {
            left: Box::new(left.clone()),
            operator,
            right,
        }))
    }

    fn parse_index_expression(&mut self, left: &ast::Expression) -> Result<ast::Expression> {
        self.next_token()?;
        self.next_token()?;


        let left = Box::new(left.clone());
        let index = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.expect(Token::RightBracket)?;

        // Assigning to the hash index
        if self.peek == Token::Assign {
            // Skip after the equal
            self.next_token()?;
            self.next_token()?;

            Ok(ast::Expression::Assign(ast::AssignExpression {
                left: Box::new(ast::Expression::Index(ast::IndexExpression {
                    left,
                    index
                })),
                value: Box::new(self.parse_expression(Precedence::Lowest)?)
            }))
        } else {
            Ok(ast::Expression::Index(ast::IndexExpression {
                left,
                index
            }))
        }
    }

    fn parse_expression_list(&mut self, end: Token) -> Result<Vec<ast::Expression>> {
        // Empty list
        if self.peek == end {
            self.next_token()?;
            return Ok(vec![]);
        }

        let mut expressions = vec![];

        // Get rest of expressions
        self.next_token()?;
        expressions.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek == Token::Comma {
            self.next_token()?;
            self.next_token()?;

            expressions.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect(end)?;

        Ok(expressions)
    }

    fn parse_assign_expression(&mut self, left: &ast::Expression) -> Result<ast::Expression> {
        self.next_token()?;
        self.next_token()?;

        let value = self.parse_expression(Precedence::Lowest)?;

        Ok(ast::Expression::Assign(ast::AssignExpression {
            left: Box::new(left.clone()),
            value: Box::new(value),
        }))
    }

    fn prefix_parse(&mut self) -> Result<ast::Expression> {
        match &self.current {
            Token::Identifier(_) => {
                let name = self.parse_identifier_name()?;
                Ok(if self.peek == Token::Assign {
                    self.next_token()?;
                    self.next_token()?;

                    ast::Expression::Assign(ast::AssignExpression {
                        left: Box::new(ast::Expression::Identifier(name)),
                        value: Box::new(self.parse_expression(Precedence::Lowest)?)
                    })
                } else {
                    ast::Expression::Identifier(name)
                })
            },
            Token::Integer(i) => Ok(ast::Expression::Integer(*i)),
            Token::Float(f) => Ok(ast::Expression::Float(*f)),
            Token::String(s) => Ok(ast::Expression::String(s.to_string())),
            Token::Boolean(b) => Ok(ast::Expression::Boolean(*b == true)),
            Token::Exclaim | Token::Minus | Token::Ampersand | Token::Asterisk => {
                self.parse_prefix_expression()
            }
            Token::Equal => {
                self.next_token()?;
                Ok(self.parse_expression(Precedence::Sum)?)
            }
            Token::LeftParen => self.parse_grouped_expression(),
            Token::LeftBracket => {
                let elements = self.parse_expression_list(Token::RightBracket)?;
                Ok(ast::Expression::Array(ast::ArrayLiteral { elements }))
            }
            Token::LeftBrace => self.parse_hash_literal(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal(),
            _ => Err(Error::UnexpectedToken {
                want: "matching prefix parse function".to_string(),
                got: format!("{}", self.current),
            }),
        }
    }

    fn parse_if_expression(&mut self) -> Result<ast::Expression> {
        // Parse the opening of the if statement and conditional
        self.next_token()?;

        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.expect(Token::LeftBrace)?;

        // Parse the body of the if block.
        let consequence = if let ast::Statement::Block(block) = self.parse_block_statement()? {
            block
        } else {
            return Err(Error::UnexpectedToken {
                want: "if block statement".to_string(),
                got: format!("{}", &self.current),
            });
        };

        // Check for associated else block
        if self.peek != Token::Else {
            return Ok(ast::Expression::If(ast::IfExpression {
                condition,
                consequence,
                alternative: None,
            }));
        }

        // Parse the body of the else block
        self.next_token()?;

        let alternative = Some(if self.peek == Token::If {
            self.next_token()?;
            vec![ast::Statement::Expression(self.parse_if_expression()?)]
        } else {
            self.expect(Token::LeftBrace)?;
            if let ast::Statement::Block(block) = self.parse_block_statement()? {
                block
            } else {
                return Err(Error::UnexpectedToken {
                    want: "else block statement".to_string(),
                    got: format!("{}", &self.current),
                });
            }
        });

        Ok(ast::Expression::If(ast::IfExpression {
            condition,
            consequence,
            alternative,
        }))
    }

    fn parse_function_literal(&mut self) -> Result<ast::Expression> {
        self.expect(Token::LeftParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect(Token::LeftBrace)?;

        let body = if let ast::Statement::Block(block) = self.parse_block_statement()? {
            block
        } else {
            return Err(Error::UnexpectedToken {
                want: "function body block statement".to_string(),
                got: format!("{}", &self.current),
            });
        };

        Ok(ast::Expression::Function(ast::FunctionLiteral {
            parameters,
            body,
        }))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<String>> {
        let mut p = vec![];

        // End of parameter list?
        if self.peek == Token::RightParen {
            self.next_token()?;
            return Ok(p);
        }

        // Parse the first parameter.
        self.next_token()?;
        p.push(self.parse_identifier_name()?);

        // Parse each remaining parameter.
        while self.peek == Token::Comma {
            self.next_token()?;
            self.next_token()?;

            p.push(self.parse_identifier_name()?);
        }

        self.expect(Token::RightParen)?;

        Ok(p)
    }

    fn parse_block_statement(&mut self) -> Result<ast::Statement> {
        self.next_token()?;

        let mut statements = vec![];

        // Keep consuming statements until end of block or EOF.
        while self.current != Token::RightBrace && self.current != Token::Eof {
            statements.push(self.parse_statement()?);
            self.next_token()?;
        }

        Ok(ast::Statement::Block(statements))
    }

    fn parse_hash_literal(&mut self) -> Result<ast::Expression> {
        let mut pairs = vec![];
        while self.peek != Token::RightBrace {
            self.next_token()?;

            let key = self.parse_expression(Precedence::Lowest)?;
            self.expect(Token::Colon)?;
            self.next_token()?;

            let value = self.parse_expression(Precedence::Lowest)?;

            // Duplicate keys can be parsed, up to the runtime to handle
            pairs.push((key, value));

            // Parse the rest of the items
            if self.peek != Token::RightBrace {
                self.expect(Token::Comma)?;
            }
        }

        self.expect(Token::RightBrace)?;

        Ok(ast::Expression::Hash(ast::HashLiteral { pairs }))
    }

    fn parse_prefix_expression(&mut self) -> Result<ast::Expression> {
        let operator = self.current.clone();

        self.next_token()?;

        let right = Box::new(self.parse_expression(Precedence::Prefix)?);

        Ok(ast::Expression::Prefix(ast::PrefixExpression {
            operator,
            right,
        }))
    }

    /// Traverse the tokens until captured the whole expression in parenthesis
    fn parse_grouped_expression(&mut self) -> Result<ast::Expression> {
        // One after left paren
        self.next_token()?;
        let expr = self.parse_expression(Precedence::Lowest)?;
        self.expect(Token::RightParen)?;

        Ok(expr)
    }
}

pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

// Gets order of operations for each token
fn precedence(token: &Token) -> Precedence {
    match token {
        Token::Equal => Precedence::Equals,
        Token::NotEqual => Precedence::Equals,
        Token::LessThan => Precedence::LessGreater,
        Token::GreaterThan => Precedence::LessGreater,
        Token::Plus => Precedence::Sum,
        Token::Minus => Precedence::Sum,
        Token::Slash => Precedence::Product,
        Token::Asterisk => Precedence::Product,
        Token::Percent => Precedence::Product,
        Token::LeftParen => Precedence::Call,
        Token::LeftBracket => Precedence::Index,

        _ => Precedence::Lowest,
    }
}