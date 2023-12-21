/*--------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See LICENSE in the project root for license information.
 *-------------------------------------------------------------------------------------------*/

using codecabc.Unity.Analyzers.Resources;

namespace codecabc.Unity.Analyzers;

public static class DiagnosticCategory
{
	public static readonly string Performance = Strings.CategoryPerformance;
	public static readonly string Correctness = Strings.CategoryCorrectness;
	public static readonly string TypeSafety = Strings.CategoryTypeSafety;
}
