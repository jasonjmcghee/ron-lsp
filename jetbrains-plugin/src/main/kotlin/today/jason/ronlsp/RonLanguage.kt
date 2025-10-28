package today.jason.ronlsp

import com.intellij.lang.Language

class RonLanguage : Language("RON") {
    companion object {
        @JvmStatic
        val INSTANCE = RonLanguage()
    }

    override fun getDisplayName(): String = "RON"
}
