plugins {
    id("java")
    id("org.jetbrains.kotlin.jvm") version "2.1.0"
    id("org.jetbrains.intellij.platform") version "2.1.0"
}

group = "today.jason"
version = "0.1.0"

repositories {
    mavenCentral()
    intellijPlatform {
        defaultRepositories()
    }
}

dependencies {
    intellijPlatform {
        intellijIdeaUltimate("2025.2.4")
        instrumentationTools()
    }
}

intellijPlatform {
    pluginConfiguration {
        name = "RON LSP"
        version = project.version.toString()

        ideaVersion {
            sinceBuild = "252"
            untilBuild = "252.*"
        }

        changeNotes = """
            <h3>0.1.0</h3>
            <ul>
                <li>Initial release</li>
                <li>LSP integration for RON files</li>
                <li>Support for diagnostics, completion, hover, go-to-definition, and code actions</li>
                <li>Configurable server path</li>
            </ul>
        """.trimIndent()
    }

    publishing {
        token = providers.environmentVariable("PUBLISH_TOKEN")
    }
}

tasks {
    buildSearchableOptions {
        enabled = false
    }

    withType<JavaCompile> {
        sourceCompatibility = "21"
        targetCompatibility = "21"
    }

    withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
        kotlinOptions.jvmTarget = "21"
    }
}
