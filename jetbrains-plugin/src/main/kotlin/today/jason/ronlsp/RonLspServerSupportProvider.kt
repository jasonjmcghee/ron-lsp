package today.jason.ronlsp

import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.platform.lsp.api.LspServer
import com.intellij.platform.lsp.api.LspServerSupportProvider
import com.intellij.platform.lsp.api.lsWidget.LspServerWidgetItem
import today.jason.ronlsp.settings.RonLspSettingsConfigurable

class RonLspServerSupportProvider : LspServerSupportProvider {
    override fun fileOpened(
        project: Project,
        file: VirtualFile,
        serverStarter: LspServerSupportProvider.LspServerStarter
    ) {
        if (file.extension == "ron") {
            serverStarter.ensureServerStarted(RonLspServerDescriptor(project))
        }
    }

    override fun createLspServerWidgetItem(
        lspServer: LspServer,
        currentFile: VirtualFile?
    ): LspServerWidgetItem {
        return LspServerWidgetItem(
            lspServer,
            currentFile,
            RonIcons.FILE,
            RonLspSettingsConfigurable::class.java
        )
    }
}
