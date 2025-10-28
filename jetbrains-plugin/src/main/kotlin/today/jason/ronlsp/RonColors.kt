package today.jason.ronlsp

import com.intellij.lang.annotation.HighlightSeverity
import com.intellij.openapi.editor.HighlighterColors
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.options.OptionsBundle
import com.intellij.openapi.options.colors.AttributesDescriptor
import com.intellij.openapi.util.NlsContexts.AttributeDescriptor
import java.util.function.Supplier
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors

enum class RonColors(humanName: Supplier<@AttributeDescriptor String>, default: TextAttributesKey? = null) {
    COMMENT(OptionsBundle.messagePointer("options.language.defaults.line.comment"), DefaultLanguageHighlighterColors.LINE_COMMENT),
    STRING(RonBundle.messagePointer("settings.ron.color.string"), DefaultLanguageHighlighterColors.STRING),
    NUMBER(RonBundle.messagePointer("settings.ron.color.number"), DefaultLanguageHighlighterColors.NUMBER),
    IDENTIFIER(RonBundle.messagePointer("settings.ron.color.identifier"), DefaultLanguageHighlighterColors.INSTANCE_FIELD),
    TYPE_NAME(RonBundle.messagePointer("settings.ron.color.struct"), DefaultLanguageHighlighterColors.CLASS_NAME),
    ENUM_VARIANT(RonBundle.messagePointer("settings.ron.color.enum.variant"), DefaultLanguageHighlighterColors.STATIC_FIELD),
    KEYWORD(RonBundle.messagePointer("settings.ron.color.keyword"), DefaultLanguageHighlighterColors.KEYWORD),
    PUNCTUATION(OptionsBundle.messagePointer("options.language.defaults.comma"), DefaultLanguageHighlighterColors.COMMA),
    BAD_CHARACTER(RonBundle.messagePointer("settings.ron.bad.char"), HighlighterColors.BAD_CHARACTER),
    ;

    val textAttributesKey = TextAttributesKey.createTextAttributesKey("today.jason.ronlsp.ron.$name", default)
    val attributesDescriptor = AttributesDescriptor(humanName, textAttributesKey)
    val testSeverity: HighlightSeverity = HighlightSeverity(name, HighlightSeverity.INFORMATION.myVal)
}