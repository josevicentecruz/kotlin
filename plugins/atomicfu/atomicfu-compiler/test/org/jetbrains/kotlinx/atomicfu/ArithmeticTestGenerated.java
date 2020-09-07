/*
 * Copyright 2010-2020 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlinx.atomicfu;

import com.intellij.testFramework.TestDataPath;
import org.jetbrains.kotlin.test.JUnit3RunnerWithInners;
import org.jetbrains.kotlin.test.KotlinTestUtils;
import org.jetbrains.kotlin.test.TestMetadata;
import org.junit.runner.RunWith;

import java.io.File;
import java.util.regex.Pattern;

/** This class is generated by {@link org.jetbrains.kotlin.generators.tests.TestsPackage}. DO NOT MODIFY MANUALLY */
@SuppressWarnings("all")
@TestMetadata("plugins/atomicfu/atomicfu-compiler/testData/basic")
@TestDataPath("$PROJECT_ROOT")
@RunWith(JUnit3RunnerWithInners.class)
public class ArithmeticTestGenerated extends AbstractBasicAtomicfuTest {
    private void runTest(String testDataFilePath) throws Exception {
        KotlinTestUtils.runTest(this::doTest, this, testDataFilePath);
    }

    public void testAllFilesPresentInBasic() throws Exception {
        KotlinTestUtils.assertAllTestsPresentByMetadataWithExcluded(this.getClass(), new File("plugins/atomicfu/atomicfu-compiler/testData/basic"), Pattern.compile("^(.+)\\.kt$"), null, true);
    }

    @TestMetadata("ArithmeticTest.kt")
    public void testArithmeticTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/ArithmeticTest.kt");
    }

    @TestMetadata("AtomicArrayTest.kt")
    public void testAtomicArrayTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/AtomicArrayTest.kt");
    }

    @TestMetadata("ArrayInlineFunctionTest.kt")
    public void testArrayInlineFunctionTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/ArrayInlineFunctionTest.kt");
    }

    @TestMetadata("ExtensionsTest.kt")
    public void testExtensionsTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/ExtensionsTest.kt");
    }

    @TestMetadata("IndexArrayElementGetterTest.kt")
    public void testIndexArrayElementGetterTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/IndexArrayElementGetterTest.kt");
    }

    @TestMetadata("LockFreeStackTest.kt")
    public void testLockFreeStackTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/LockFreeStackTest.kt");
    }

    @TestMetadata("LockFreeQueueTest.kt")
    public void testLockFreeQueueTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/LockFreeQueueTest.kt");
    }

    @TestMetadata("LockFreeLongCounterTest.kt")
    public void testLockFreeLongCounterTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/LockFreeLongCounterTest.kt");
    }

    @TestMetadata("LockFreeIntBitsTest.kt")
    public void testLockFreeIntBitsTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/LockFreeIntBitsTest.kt");
    }

    @TestMetadata("LoopTest.kt")
    public void testLoopTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/LoopTest.kt");
    }

    @TestMetadata("ScopeTest.kt")
    public void testScopeTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/ScopeTest.kt");
    }

    @TestMetadata("LockTest.kt")
    public void testLockTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/LockTest.kt");
    }

    @TestMetadata("MultiInitTest.kt")
    public void testMultiInitTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/MultiInitTest.kt");
    }

    @TestMetadata("SimpleLockTest.kt")
    public void testSimpleLockTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/SimpleLockTest.kt");
    }

    @TestMetadata("TopLevelTest.kt")
    public void testTopLevelTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/TopLevelTest.kt");
    }

    @TestMetadata("UncheckedCastTest.kt")
    public void testUncheckedCastTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/UncheckedCastTest.kt");
    }

    @TestMetadata("PropertyDeclarationTest.kt")
    public void testPropertyDeclarationTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/PropertyDeclarationTest.kt");
    }

    @TestMetadata("ParameterizedInlineFunExtensionTest.kt")
    public void testParameterizedInlineFunExtensionTest() throws Exception {
        runTest("plugins/atomicfu/atomicfu-compiler/testData/basic/ParameterizedInlineFunExtensionTest.kt");
    }
}
