/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.incremental.storage

import org.jetbrains.kotlin.TestWithWorkingDir

internal class IncrementalFileToPathConverterTest : TestWithWorkingDir() {
    fun testPathTransform() {
        val testRoot = workingDir.resolve("testDir")
        val pathConverter = IncrementalFileToPathConverter(testRoot)
        val relativeFilePath = "testFile.txt"
        val testFile = testRoot.resolve(relativeFilePath)
        val transformedPath = pathConverter.toPath(testFile)
        assertEquals("${'$'}PROJECT_DIR${'$'}/$relativeFilePath", transformedPath)
        assertEquals(testFile.absolutePath, pathConverter.toFile(transformedPath).absolutePath)
    }

}