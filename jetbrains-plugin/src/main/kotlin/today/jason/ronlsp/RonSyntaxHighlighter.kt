package today.jason.ronlsp

import com.intellij.lexer.Lexer
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors
import com.intellij.openapi.editor.HighlighterColors
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase
import com.intellij.psi.TokenType
import com.intellij.psi.tree.IElementType

class RonSyntaxHighlighter : SyntaxHighlighterBase() {
    override fun getHighlightingLexer(): Lexer = RonLexer()

    override fun getTokenHighlights(tokenType: IElementType): Array<TextAttributesKey> {
        return when (tokenType) {
            RonTokenTypes.COMMENT -> COMMENT_KEYS
            RonTokenTypes.STRING -> STRING_KEYS
            RonTokenTypes.NUMBER -> NUMBER_KEYS
            RonTokenTypes.IDENTIFIER -> IDENTIFIER_KEYS
            RonTokenTypes.TYPE_NAME -> TYPE_NAME_KEYS
            RonTokenTypes.ENUM_VARIANT -> ENUM_VARIANT_KEYS
            RonTokenTypes.KEYWORD -> KEYWORD_KEYS
            RonTokenTypes.PUNCTUATION -> PUNCTUATION_KEYS
            TokenType.BAD_CHARACTER -> BAD_CHAR_KEYS
            else -> EMPTY_KEYS
        }
    }

    companion object {
        val COMMENT = TextAttributesKey.createTextAttributesKey("RON_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT)
        val STRING = TextAttributesKey.createTextAttributesKey("RON_STRING", DefaultLanguageHighlighterColors.STRING)
        val NUMBER = TextAttributesKey.createTextAttributesKey("RON_NUMBER", DefaultLanguageHighlighterColors.NUMBER)
        val IDENTIFIER = TextAttributesKey.createTextAttributesKey("RON_IDENTIFIER", DefaultLanguageHighlighterColors.IDENTIFIER)
        val TYPE_NAME = TextAttributesKey.createTextAttributesKey("RON_TYPE_NAME", DefaultLanguageHighlighterColors.FUNCTION_DECLARATION)
        val ENUM_VARIANT = TextAttributesKey.createTextAttributesKey("RON_ENUM_VARIANT", DefaultLanguageHighlighterColors.STATIC_FIELD)
        val KEYWORD = TextAttributesKey.createTextAttributesKey("RON_KEYWORD", DefaultLanguageHighlighterColors.KEYWORD)
        val PUNCTUATION = TextAttributesKey.createTextAttributesKey("RON_PUNCTUATION", DefaultLanguageHighlighterColors.COMMA)
        val BAD_CHAR = TextAttributesKey.createTextAttributesKey("RON_BAD_CHARACTER", HighlighterColors.BAD_CHARACTER)

        private val COMMENT_KEYS = arrayOf(COMMENT)
        private val STRING_KEYS = arrayOf(STRING)
        private val NUMBER_KEYS = arrayOf(NUMBER)
        private val IDENTIFIER_KEYS = arrayOf(IDENTIFIER)
        private val TYPE_NAME_KEYS = arrayOf(TYPE_NAME)
        private val ENUM_VARIANT_KEYS = arrayOf(ENUM_VARIANT)
        private val KEYWORD_KEYS = arrayOf(KEYWORD)
        private val PUNCTUATION_KEYS = arrayOf(PUNCTUATION)
        private val BAD_CHAR_KEYS = arrayOf(BAD_CHAR)
        private val EMPTY_KEYS = emptyArray<TextAttributesKey>()
    }
}
