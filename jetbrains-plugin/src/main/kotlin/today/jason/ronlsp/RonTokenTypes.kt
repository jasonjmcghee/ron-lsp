package today.jason.ronlsp

import com.intellij.psi.tree.IElementType

object RonTokenTypes {
    val COMMENT = IElementType("RON_COMMENT", RonLanguage.INSTANCE)
    val STRING = IElementType("RON_STRING", RonLanguage.INSTANCE)
    val NUMBER = IElementType("RON_NUMBER", RonLanguage.INSTANCE)
    val IDENTIFIER = IElementType("RON_IDENTIFIER", RonLanguage.INSTANCE)
    val TYPE_NAME = IElementType("RON_TYPE_NAME", RonLanguage.INSTANCE)  // ADD THIS
    val ENUM_VARIANT = IElementType("RON_ENUM_VARIANT", RonLanguage.INSTANCE)
    val PUNCTUATION = IElementType("RON_PUNCTUATION", RonLanguage.INSTANCE)
    val KEYWORD = IElementType("RON_KEYWORD", RonLanguage.INSTANCE)
}
