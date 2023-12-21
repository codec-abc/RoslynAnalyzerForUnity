using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using codecabc.Unity.Analyzers.Resources;
using System.Text;
using AttributeValidation;

namespace codecabc.Unity.Analyzers;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public class NoNumericAttribute : DiagnosticAnalyzer
{
	internal static readonly DiagnosticDescriptor Rule = new(
		id: "ACR0003",
		title: Strings.NoNumericAttributeErrorDescription,
		messageFormat: Strings.NoNumericAttributeErrorDescription,
		category: DiagnosticCategory.Correctness,
		defaultSeverity: DiagnosticSeverity.Info,
		isEnabledByDefault: true,
		description: Strings.NoNumericAttributeErrorDescription);

	public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

	public override void Initialize(AnalysisContext context)
	{
		context.EnableConcurrentExecution();
		context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
		context.RegisterSyntaxNodeAction(AnalyzeMemberDeclaration, SyntaxKind.PropertyDeclaration);
		context.RegisterSyntaxNodeAction(AnalyzeMemberDeclaration, SyntaxKind.FieldDeclaration);
	}

	private static void AnalyzeMemberDeclaration(SyntaxNodeAnalysisContext context)
	{
		var model = context.SemanticModel;
		var type = model.GetSymbolInfo(context.Node);
		ISymbol? symbol;
		TypeInfo? typeInfo;
		ITypeSymbol? typeSymbol;
		TypeSyntax typeSyntax;
		switch (context.Node)
		{
			//case PropertyDeclarationSyntax pdec:
			//	symbol = model.GetDeclaredSymbol(pdec);
			//	typeInfo = model.GetTypeInfo(pdec);
			//	typeSyntax = pdec.Type;
			//	typeSymbol = model.GetTypeInfo(pdec.Type).Type;
			//	break;
			case FieldDeclarationSyntax fdec:
				if (fdec.Declaration.Variables.Count == 0)
				{
					return;
				}

				typeSymbol = model.GetTypeInfo(fdec.Declaration.Type).Type;
				typeSyntax = fdec.Declaration.Type;
				// attributes are applied to all fields declaration symbols
				// just get the first one
				symbol = model.GetDeclaredSymbol(fdec.Declaration.Variables[0]);
				typeInfo = model.GetTypeInfo(fdec);
				break;
			default:
				// we only support field/property analysis
				return;
		}

		if (symbol == null)
		{
			return;
		}

		if (
			!Utils.IsSymbolSerializedTypeInUnity(symbol, context) ||
			!IsReportable(symbol, type, typeInfo, typeSymbol, typeSyntax, context)
		)
		{
			return;
		}

		context.ReportDiagnostic(Diagnostic.Create(Rule, context.Node.GetLocation(), symbol.Name));
	}

	private static bool IsReportable(
		ISymbol symbol,
		SymbolInfo sbInfo,
		TypeInfo? typeInfo,
		ITypeSymbol? typeSymbol,
		TypeSyntax typeSyntax,
		SyntaxNodeAnalysisContext ctx)
	{
		if (typeSymbol == null)
		{
			return false;
		}

		if (!Utils.IsSymbolSerializedInFile(typeSymbol, symbol, ctx))
		{
			return false;
		}

		var realType = typeSymbol;
		var isEnumerable = false;

		if (Utils.TryGetArrayType(typeSymbol, ctx, out var arrayType))
		{
			realType = arrayType;
			isEnumerable = true;
		}
		else if (Utils.TryGetGenericListType(typeSymbol, ctx, out var listType))
		{
			realType = listType;
			isEnumerable = true;
		}

		if (!Utils.IsNumericType(realType, ctx))
		{
			return false;
		}

		if (!isEnumerable)
		{
			return !Utils.HasAnyAttributes(
				symbol,
				typeof(NumericAnyValueAttribute),
				typeof(MinValueAttribute),
				typeof(MaxValueAttribute));
		} 
		else
		{
			return !Utils.HasAnyAttributes(
				symbol,
				typeof(EnumerableNumericAnyValueAttribute),
				typeof(EnumerableMinValueAttribute),
				typeof(EnumerableMaxValueAttribute));
		}
	}
}
