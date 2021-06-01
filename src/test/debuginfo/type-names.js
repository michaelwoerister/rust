"use strict";

function getFunctionDetails(name)
{
    var localVariable = host.currentThread.Stack.Frames[0].LocalVariables[name];
    var functionPointerType = localVariable.targetType.genericArguments[0];
    var functionType = functionPointerType.baseType;
    host.diagnostics.debugLog("Return Type: ", functionType.functionReturnType, "\n");
    host.diagnostics.debugLog("Parameter Types: ", functionType.functionParameterTypes, "\n");
}
