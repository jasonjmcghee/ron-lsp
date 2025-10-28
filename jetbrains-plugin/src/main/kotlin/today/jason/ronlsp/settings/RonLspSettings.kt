package today.jason.ronlsp.settings

import com.intellij.openapi.components.*
import com.intellij.openapi.project.Project

@Service(Service.Level.PROJECT)
@State(
    name = "RonLspSettings",
    storages = [Storage("ron-lsp.xml")]
)
class RonLspSettings : PersistentStateComponent<RonLspSettings.State> {
    private var state = State()

    override fun getState(): State = state

    override fun loadState(state: State) {
        this.state = state
    }

    data class State(
        var serverPath: String = "ron-lsp",
        var enableLogging: Boolean = false
    )

    companion object {
        fun getInstance(project: Project): RonLspSettings {
            return project.service()
        }
    }
}
