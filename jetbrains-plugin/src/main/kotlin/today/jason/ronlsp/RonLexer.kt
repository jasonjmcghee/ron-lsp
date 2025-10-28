package today.jason.ronlsp

import com.intellij.lexer.LexerBase
import com.intellij.psi.TokenType
import com.intellij.psi.tree.IElementType

class RonLexer : LexerBase() {
    private var buffer: CharSequence = ""
    private var startOffset = 0
    private var endOffset = 0
    private var currentPosition = 0
    private var currentToken: IElementType? = null

    override fun start(buffer: CharSequence, startOffset: Int, endOffset: Int, initialState: Int) {
        this.buffer = buffer
        this.startOffset = startOffset
        this.endOffset = endOffset
        this.currentPosition = startOffset
        advance()
    }

    override fun getState(): Int = 0

    override fun getTokenType(): IElementType? = currentToken

    override fun getTokenStart(): Int = startOffset

    override fun getTokenEnd(): Int = currentPosition

    override fun advance() {
        if (currentPosition >= endOffset) {
            currentToken = null
            return
        }

        startOffset = currentPosition
        val char = buffer[currentPosition]

        currentToken = when {
            char.isWhitespace() -> {
                while (currentPosition < endOffset && buffer[currentPosition].isWhitespace()) {
                    currentPosition++
                }
                TokenType.WHITE_SPACE
            }
            char == '/' && currentPosition + 1 < endOffset -> {
                when (buffer[currentPosition + 1]) {
                    '/' -> readLineComment()
                    '*' -> readBlockComment()
                    else -> readPunctuation()
                }
            }
            char == '"' -> readString()
            char.isDigit() || (char == '-' && currentPosition + 1 < endOffset && buffer[currentPosition + 1].isDigit()) -> readNumber()
            char in "()[]{}:," -> readPunctuation()
            char.isLetter() || char == '_' -> readIdentifier()
            else -> {
                currentPosition++
                TokenType.BAD_CHARACTER
            }
        }
    }

    private fun readLineComment(): IElementType {
        while (currentPosition < endOffset && buffer[currentPosition] != '\n') {
            currentPosition++
        }
        return RonTokenTypes.COMMENT
    }

    private fun readBlockComment(): IElementType {
        currentPosition += 2 // skip /*
        while (currentPosition + 1 < endOffset) {
            if (buffer[currentPosition] == '*' && buffer[currentPosition + 1] == '/') {
                currentPosition += 2
                break
            }
            currentPosition++
        }
        return RonTokenTypes.COMMENT
    }

    private fun readString(): IElementType {
        currentPosition++ // skip opening "
        while (currentPosition < endOffset) {
            val ch = buffer[currentPosition]
            if (ch == '"' && (currentPosition == 0 || buffer[currentPosition - 1] != '\\')) {
                currentPosition++
                break
            }
            currentPosition++
        }
        return RonTokenTypes.STRING
    }

    private fun readNumber(): IElementType {
        if (buffer[currentPosition] == '-') currentPosition++
        while (currentPosition < endOffset && (buffer[currentPosition].isDigit() || buffer[currentPosition] in "._eE+-")) {
            currentPosition++
        }
        return RonTokenTypes.NUMBER
    }

    private fun readIdentifier(): IElementType {
        while (currentPosition < endOffset && (buffer[currentPosition].isLetterOrDigit() || buffer[currentPosition] == '_')) {
            currentPosition++
        }
        val text = buffer.subSequence(startOffset, currentPosition).toString()

        // Check what comes after the identifier (skip whitespace)
        var lookAhead = currentPosition
        while (lookAhead < endOffset && buffer[lookAhead].isWhitespace()) {
            lookAhead++
        }
        val isFollowedByParen = lookAhead < endOffset && buffer[lookAhead] == '('

        return when {
            text in setOf("true", "false", "Some", "None", "Ok", "Err") -> RonTokenTypes.KEYWORD
            text.firstOrNull()?.isUpperCase() == true -> {
                // If followed by (, it's a type constructor
                // If not, it's an enum variant
                if (isFollowedByParen) RonTokenTypes.TYPE_NAME else RonTokenTypes.ENUM_VARIANT
            }
            else -> RonTokenTypes.IDENTIFIER
        }
    }

    private fun readPunctuation(): IElementType {
        currentPosition++
        return RonTokenTypes.PUNCTUATION
    }

    override fun getBufferSequence(): CharSequence = buffer
    override fun getBufferEnd(): Int = endOffset
}
