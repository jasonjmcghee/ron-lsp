package today.jason.ronlsp

import com.intellij.openapi.fileTypes.LanguageFileType

class RonFileType : LanguageFileType(RonLanguage.INSTANCE) {
    companion object {
        @JvmStatic
        val INSTANCE = RonFileType()
    }

    override fun getName(): String = "RON"

    override fun getDescription(): String = "Rusty Object Notation"

    override fun getDefaultExtension(): String = "ron"

    override fun getIcon() = RonIcons.RON
}
