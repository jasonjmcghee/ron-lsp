package today.jason.ronlsp

import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.options.colors.AttributesDescriptor
import com.intellij.openapi.options.colors.ColorDescriptor
import com.intellij.openapi.options.colors.ColorSettingsPage

class RonColorsSettingsPage : ColorSettingsPage {
    override fun getDisplayName() = RonBundle.message("settings.ron.color.scheme.title")
    override fun getIcon() = RonIcons.RON
    override fun getAttributeDescriptors() = ATTRS
    override fun getColorDescriptors(): Array<ColorDescriptor> = ColorDescriptor.EMPTY_ARRAY
    override fun getHighlighter() = RonSyntaxHighlighter()
    override fun getAdditionalHighlightingTagToDescriptorMap() = ANNOTATOR_TAGS
    override fun getDemoText() = """
        /* @[crate::MyStruct] */
        
        // RON example
        MyStruct(
            id: 42,
            name: "example",
            option: Some("value"),
        )
    """.trimIndent()

    companion object {
        private val ATTRS: Array<AttributesDescriptor> =
            RonColors.values().map { it.attributesDescriptor }.toTypedArray()

        // This tags should be kept in sync with RsHighlightingAnnotator highlighting logic
        private val ANNOTATOR_TAGS: Map<String, TextAttributesKey> =
            RonColors.values().associateBy({ it.name }, { it.textAttributesKey })
    }
}