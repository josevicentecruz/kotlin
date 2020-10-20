/*
 * Copyright 2000-2018 JetBrains s.r.o. and Kotlin Programming Language contributors.
 * Use of this source code is governed by the Apache 2.0 license that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.codegen

import org.jetbrains.kotlin.backend.common.CodegenUtil.isInlineClassWithUnderlyingTypeAny
import org.jetbrains.kotlin.codegen.inline.InlineCodegen
import org.jetbrains.kotlin.config.LanguageFeature
import org.jetbrains.kotlin.descriptors.FunctionDescriptor
import org.jetbrains.kotlin.descriptors.ValueParameterDescriptor
import org.jetbrains.kotlin.descriptors.impl.AnonymousFunctionDescriptor
import org.jetbrains.kotlin.resolve.calls.callUtil.getResolvedCall
import org.jetbrains.kotlin.resolve.calls.model.DefaultValueArgument
import org.jetbrains.kotlin.resolve.calls.model.ExpressionValueArgument
import org.jetbrains.kotlin.resolve.calls.model.VarargValueArgument
import org.jetbrains.kotlin.resolve.jvm.AsmTypes.OBJECT_TYPE
import org.jetbrains.kotlin.types.upperIfFlexible
import org.jetbrains.org.objectweb.asm.Type

class CallBasedArgumentGenerator(
    private val codegen: ExpressionCodegen,
    private val callGenerator: CallGenerator,
    private val valueParameters: List<ValueParameterDescriptor>,
    private val valueParameterTypes: List<Type>
) : ArgumentGenerator() {
    private val isVarargInvoke: Boolean =
        JvmCodegenUtil.isDeclarationOfBigArityFunctionInvoke(valueParameters.firstOrNull()?.containingDeclaration)

    private val isPolymorphicSignature: Boolean =
        codegen.state.languageVersionSettings.supportsFeature(LanguageFeature.PolymorphicSignature) &&
        (valueParameters.firstOrNull()?.containingDeclaration as? FunctionDescriptor)?.let { function ->
            JvmCodegenUtil.isPolymorphicSignature(function)
        } == true

    init {
        if (!isVarargInvoke && !isPolymorphicSignature) {
            assert(valueParameters.size == valueParameterTypes.size) {
                "Value parameters and their types mismatch in sizes: ${valueParameters.size} != ${valueParameterTypes.size}"
            }
        }
    }

    override fun generateExpression(i: Int, argument: ExpressionValueArgument) {
        callGenerator.genValueAndPut(
            valueParameters[i],
            argument.valueArgument!!.getArgumentExpression()!!,
            if (isVarargInvoke) JvmKotlinType(OBJECT_TYPE) else getJvmKotlinType(i),
            i
        )
        val descriptor = argument.valueArgument?.getArgumentExpression().getResolvedCall(codegen.bindingContext)
            ?.resultingDescriptor ?: return
        if (descriptor.returnType?.isInlineClassWithUnderlyingTypeAny() == true &&
            descriptor.containingDeclaration is AnonymousFunctionDescriptor &&
            // TODO: HACK
            callGenerator !is InlineCodegen<*>
        ) {
            StackValue.unboxInlineClass(OBJECT_TYPE, descriptor.returnType!!, codegen.v)
        }
    }

    override fun generateDefault(i: Int, argument: DefaultValueArgument) {
        callGenerator.putValueIfNeeded(
            getJvmKotlinType(i),
            StackValue.createDefaultValue(valueParameterTypes[i]),
            ValueKind.DEFAULT_PARAMETER,
            i
        )
    }

    override fun generateVararg(i: Int, argument: VarargValueArgument) {
        if (isPolymorphicSignature) {
            for ((index, arg) in argument.arguments.withIndex()) {
                val expression = arg.getArgumentExpression()!!
                val type = JvmKotlinType(valueParameterTypes[index], codegen.kotlinType(expression))
                callGenerator.genValueAndPut(null, expression, type, index)
            }
            return
        }

        // Upper bound for type of vararg parameter should always have a form of 'Array<out T>',
        // while its lower bound may be Nothing-typed after approximation
        val lazyVararg = codegen.genVarargs(argument, valueParameters[i].type.upperIfFlexible())
        callGenerator.putValueIfNeeded(getJvmKotlinType(i), lazyVararg, ValueKind.GENERAL_VARARG, i)
    }

    override fun generateDefaultJava(i: Int, argument: DefaultValueArgument) {
        val argumentValue = valueParameters[i].findJavaDefaultArgumentValue(valueParameterTypes[i], codegen.typeMapper)

        callGenerator.putValueIfNeeded(getJvmKotlinType(i), argumentValue)
    }

    override fun reorderArgumentsIfNeeded(args: List<ArgumentAndDeclIndex>) {
        callGenerator.reorderArgumentsIfNeeded(args, valueParameterTypes)
    }

    private fun getJvmKotlinType(i: Int): JvmKotlinType =
        JvmKotlinType(valueParameterTypes[i], valueParameters[i].unsubstitutedType)

    private val ValueParameterDescriptor.unsubstitutedType
        get() = containingDeclaration.original.valueParameters[index].type
}
