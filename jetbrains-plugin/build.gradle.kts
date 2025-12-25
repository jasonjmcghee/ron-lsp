import org.jetbrains.kotlin.gradle.dsl.JvmTarget
import java.net.URI

plugins {
    id("java")
    id("org.jetbrains.kotlin.jvm") version "2.1.0"
    id("org.jetbrains.intellij.platform") version "2.1.0"
}

group = "today.jason"
version = "0.1.2"

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(21))
    }
}

repositories {
    mavenCentral()
    intellijPlatform {
        defaultRepositories()
    }
}

configurations {
    create("jflex")
}

dependencies {
    intellijPlatform {
        intellijIdeaUltimate("2025.3")
        instrumentationTools()
    }
    implementation("org.jetbrains:annotations:26.0.2")

    // Use JetBrains patched JFlex for IntelliJ compatibility
    "jflex"("org.jetbrains.intellij.deps.jflex:jflex:1.9.2")
}

intellijPlatform {
    pluginConfiguration {
        name = "RON LSP"
        version = project.version.toString()

        ideaVersion {
            sinceBuild = "253"
        }

        changeNotes = """
            <h3>0.1.2</h3>
            <ul>
                <li>Enhance idea version support</li>
            </ul>
            <h3>0.1.1</h3>
            <ul>
                <li>Add icon and color configuration</li>
                <li>Use JFlex for lexing</li>
            </ul>
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

sourceSets {
    main {
        java {
            srcDirs("src/main/gen")
        }
    }
}

tasks {
    buildSearchableOptions {
        enabled = false
    }

    val generateLexer = register<JavaExec>("generateLexer") {
        val src = "src/main/grammar/Ron.flex"
        val dst = "src/main/gen"
        val skeletonFile = "src/main/grammar/idea-flex.skeleton"

        inputs.file(src)
        outputs.dir(dst)

        classpath = configurations["jflex"]
        mainClass.set("jflex.Main")
        args = listOf(
            "-d", dst,
            "--nobak",
            "--skel", skeletonFile,  // Use IntelliJ skeleton
            src
        )

        // Download skeleton file if it doesn't exist
        doFirst {
            val skeletonPath = file(skeletonFile)
            if (!skeletonPath.exists()) {
                skeletonPath.parentFile.mkdirs()
                val url = URI("https://raw.githubusercontent.com/JetBrains/intellij-community/master/tools/lexer/idea-flex.skeleton").toURL()
                url.openStream().use { input: java.io.InputStream ->
                    skeletonPath.outputStream().use { output: java.io.OutputStream ->
                        input.copyTo(output)
                    }
                }
                println("Downloaded idea-flex.skeleton")
            }
        }
    }

    withType<JavaCompile> {
        sourceCompatibility = "21"
        targetCompatibility = "21"
        dependsOn(generateLexer)
    }

    withType<org.jetbrains.kotlin.gradle.tasks.KotlinCompile> {
        compilerOptions.jvmTarget.set(JvmTarget.JVM_21)
    }

    compileKotlin {
        dependsOn(generateLexer)
    }
}
