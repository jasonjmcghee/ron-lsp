package today.jason.ronlsp;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.TokenType;

%%

%class RonLexer
%implements FlexLexer
%unicode
%function advance
%type IElementType
%eof{  return;
%eof}

WHITE_SPACE = [ \t\r\n]+
LINE_COMMENT = "//" [^\r\n]*
BLOCK_COMMENT = "/*" ( [^*] | ("*" [^/]) )* "*/"

DIGIT = [0-9]
NUMBER = "-"? {DIGIT}+ ("." {DIGIT}+)? ([eE] [+-]? {DIGIT}+)?

STRING = \" ( [^\"\\\n] | \\ [^\n] )* \"

IDENTIFIER = [a-z_][a-zA-Z0-9_]*
UPPER_IDENTIFIER = [A-Z][a-zA-Z0-9_]*

%%

<YYINITIAL> {
    {WHITE_SPACE}           { return TokenType.WHITE_SPACE; }
    {LINE_COMMENT}          { return RonTokenTypes.COMMENT; }
    {BLOCK_COMMENT}         { return RonTokenTypes.COMMENT; }
    
    {STRING}                { return RonTokenTypes.STRING; }
    {NUMBER}                { return RonTokenTypes.NUMBER; }
    
    "true"                  { return RonTokenTypes.KEYWORD; }
    "false"                 { return RonTokenTypes.KEYWORD; }
    "Some"                  { return RonTokenTypes.KEYWORD; }
    "None"                  { return RonTokenTypes.KEYWORD; }
    "Ok"                    { return RonTokenTypes.KEYWORD; }
    "Err"                   { return RonTokenTypes.KEYWORD; }
    
    {UPPER_IDENTIFIER} / {WHITE_SPACE}* "("  { return RonTokenTypes.TYPE_NAME; }
    {UPPER_IDENTIFIER}                       { return RonTokenTypes.ENUM_VARIANT; }
    {IDENTIFIER}                             { return RonTokenTypes.IDENTIFIER; }
    
    "("                     { return RonTokenTypes.PUNCTUATION; }
    ")"                     { return RonTokenTypes.PUNCTUATION; }
    "["                     { return RonTokenTypes.PUNCTUATION; }
    "]"                     { return RonTokenTypes.PUNCTUATION; }
    "{"                     { return RonTokenTypes.PUNCTUATION; }
    "}"                     { return RonTokenTypes.PUNCTUATION; }
    ":"                     { return RonTokenTypes.PUNCTUATION; }
    ","                     { return RonTokenTypes.PUNCTUATION; }
    
    [^]                     { return TokenType.BAD_CHARACTER; }
}
