package today.jason.ronlsp

import com.intellij.lexer.FlexAdapter
import com.intellij.lexer.Lexer
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase
import com.intellij.psi.TokenType
import com.intellij.psi.tree.IElementType

class RonSyntaxHighlighter : SyntaxHighlighterBase() {
    override fun getHighlightingLexer(): Lexer = FlexAdapter(RonLexer(null))

    override fun getTokenHighlights(tokenType: IElementType): Array<TextAttributesKey> =
        pack(map(tokenType)?.textAttributesKey)

    companion object {
        fun map(tokenType: IElementType): RonColors? = when (tokenType) {
            RonTokenTypes.COMMENT -> RonColors.COMMENT
            RonTokenTypes.STRING -> RonColors.STRING
            RonTokenTypes.NUMBER -> RonColors.NUMBER
            RonTokenTypes.IDENTIFIER -> RonColors.IDENTIFIER
            RonTokenTypes.TYPE_NAME -> RonColors.TYPE_NAME
            RonTokenTypes.ENUM_VARIANT -> RonColors.ENUM_VARIANT
            RonTokenTypes.KEYWORD -> RonColors.KEYWORD
            RonTokenTypes.PUNCTUATION -> RonColors.PUNCTUATION
            TokenType.BAD_CHARACTER -> RonColors.BAD_CHARACTER

            else -> null
        }
    }
}
