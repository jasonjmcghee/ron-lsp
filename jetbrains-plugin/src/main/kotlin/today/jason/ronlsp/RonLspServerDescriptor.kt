package today.jason.ronlsp

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.platform.lsp.api.ProjectWideLspServerDescriptor
import today.jason.ronlsp.settings.RonLspSettings

class RonLspServerDescriptor(project: Project) : ProjectWideLspServerDescriptor(project, "RON") {
    private val settings = RonLspSettings.getInstance(project)

    override fun isSupportedFile(file: VirtualFile): Boolean {
        return file.extension == "ron"
    }

    override fun createCommandLine(): GeneralCommandLine {
        val serverPath = settings.state.serverPath.ifBlank { "ron-lsp" }
        return GeneralCommandLine(serverPath)
    }
}
