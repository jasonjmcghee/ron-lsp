package today.jason.ronlsp.settings

import com.intellij.openapi.fileChooser.FileChooserDescriptor
import com.intellij.openapi.options.Configurable
import com.intellij.openapi.project.Project
import com.intellij.openapi.ui.TextBrowseFolderListener
import com.intellij.openapi.ui.TextFieldWithBrowseButton
import com.intellij.ui.components.JBCheckBox
import com.intellij.ui.components.JBLabel
import com.intellij.util.ui.FormBuilder
import javax.swing.JComponent
import javax.swing.JPanel

class RonLspSettingsConfigurable(private val project: Project) : Configurable {
    private var serverPathField: TextFieldWithBrowseButton? = null
    private var enableLoggingCheckbox: JBCheckBox? = null

    override fun getDisplayName(): String = "RON LSP"

    override fun createComponent(): JComponent {
        val settings = RonLspSettings.getInstance(project)

        serverPathField = TextFieldWithBrowseButton().apply {
            text = settings.state.serverPath
            val descriptor = FileChooserDescriptor(
                true,  // chooseFiles
                false, // chooseFolders
                false, // chooseJars
                false, // chooseJarsAsFiles
                false, // chooseJarContents
                false  // chooseMultiple
            ).apply {
                title = "Select RON LSP Server"
                description = "Select the path to the ron-lsp executable"
            }
            addActionListener(TextBrowseFolderListener(descriptor, project))
        }

        enableLoggingCheckbox = JBCheckBox("Enable LSP logging", settings.state.enableLogging)

        return FormBuilder.createFormBuilder()
            .addLabeledComponent(JBLabel("Server path:"), serverPathField!!, 1, false)
            .addComponent(enableLoggingCheckbox!!)
            .addComponentFillVertically(JPanel(), 0)
            .panel
    }

    override fun isModified(): Boolean {
        val settings = RonLspSettings.getInstance(project)
        return serverPathField?.text != settings.state.serverPath ||
                enableLoggingCheckbox?.isSelected != settings.state.enableLogging
    }

    override fun apply() {
        val settings = RonLspSettings.getInstance(project)
        settings.state.serverPath = serverPathField?.text ?: "ron-lsp"
        settings.state.enableLogging = enableLoggingCheckbox?.isSelected ?: false
    }

    override fun reset() {
        val settings = RonLspSettings.getInstance(project)
        serverPathField?.text = settings.state.serverPath
        enableLoggingCheckbox?.isSelected = settings.state.enableLogging
    }
}
