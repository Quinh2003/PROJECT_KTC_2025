allprojects {
    repositories {
        google()
        mavenCentral()
    }
}

// Sử dụng cấu hình build mặc định của Android
// Xóa bỏ cấu hình chuyển hướng thư mục build để tránh xung đột với các plugin

subprojects {
    project.evaluationDependsOn(":app")
}

tasks.register<Delete>("clean") {
    delete(rootProject.buildDir)
}
}
