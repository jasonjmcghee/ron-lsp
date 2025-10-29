package today.jason.ronlsp

import com.intellij.psi.tree.IElementType

object RonTokenTypes {
    @JvmField val COMMENT = IElementType("RON_COMMENT", RonLanguage.INSTANCE)
    @JvmField val STRING = IElementType("RON_STRING", RonLanguage.INSTANCE)
    @JvmField val NUMBER = IElementType("RON_NUMBER", RonLanguage.INSTANCE)
    @JvmField val IDENTIFIER = IElementType("RON_IDENTIFIER", RonLanguage.INSTANCE)
    @JvmField val TYPE_NAME = IElementType("RON_TYPE_NAME", RonLanguage.INSTANCE)
    @JvmField val ENUM_VARIANT = IElementType("RON_ENUM_VARIANT", RonLanguage.INSTANCE)
    @JvmField val PUNCTUATION = IElementType("RON_PUNCTUATION", RonLanguage.INSTANCE)
    @JvmField val KEYWORD = IElementType("RON_KEYWORD", RonLanguage.INSTANCE)
}
