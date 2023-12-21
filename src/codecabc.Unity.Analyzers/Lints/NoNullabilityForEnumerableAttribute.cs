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

namespace codecabc.Unity.Analyzers;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public class NoNullabilityForEnumerableAttribute : DiagnosticAnalyzer
{
	internal static readonly DiagnosticDescriptor Rule = new(
		id: "ACR0007",
		title: Strings.NoNullabilityForEnumerableAttribute,
		messageFormat: Strings.NoNullabilityForEnumerableAttribute,
		category: DiagnosticCategory.Correctness,
		defaultSeverity: DiagnosticSeverity.Info,
		isEnabledByDefault: true,
		description: Strings.NoNullabilityForEnumerableAttribute);

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
		if (typeSymbol == null || typeSymbol.IsValueType)
		{
			return false;
		}

		var realType = typeSymbol;

		if (Utils.TryGetArrayType(typeSymbol, ctx, out var arrayType))
		{
			realType = arrayType;
		}
		else if (Utils.TryGetGenericListType(typeSymbol, ctx, out var listType))
		{
			realType = listType;
		}
		else
		{
			return false;
		}

		// Strings can never be null for serialized types in Unity
		if (realType.Matches(typeof(string)) || realType.SpecialType == SpecialType.System_String)
		{
			return false;
		}

		// Only ref to Unity's object can be null so ignoring other types.
		if (!Utils.IsExtensionOfUnityObject(realType, ctx))
		{
			return false;
		}

		if (!Utils.IsSymbolSerializedInFile(realType, symbol, ctx))
		{
			return false;
		}

		var hasNotNullAttribute = !Utils.HasAnyAttributes(
			symbol,
			typeof(AttributeValidation.EnumerableNotNullAttribute),
			typeof(AttributeValidation.EnumerableCanBeNullAttribute));

		return hasNotNullAttribute;
	}
}

[ExportCodeFixProvider(LanguageNames.CSharp)]
public class NoNullabilityForEnumerableCodeFix : CodeFixProvider
{
	public sealed override ImmutableArray<string> FixableDiagnosticIds =>
		ImmutableArray.Create(NoNullabilityForEnumerableAttribute.Rule.Id);

	public sealed override FixAllProvider GetFixAllProvider() => WellKnownFixAllProviders.BatchFixer;

	public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
	{
		var declaration = await context.GetFixableNodeAsync<MemberDeclarationSyntax>();

		if (declaration is not (PropertyDeclarationSyntax or FieldDeclarationSyntax))
		{
			return;
		}

		context.RegisterCodeFix(
			CodeAction.Create(
				Strings.NoNullabilityForEnumerableCodeFixTitle,
				ct => AddEnumerableNotNullAttributeAsync(context.Document, declaration, ct),
				declaration.ToFullString()),
			context.Diagnostics);
	}

	private static async Task<Document> AddEnumerableNotNullAttributeAsync(
		Document document,
		MemberDeclarationSyntax declaration,
		CancellationToken cancellationToken)
	{
		var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
		var model = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

		var attributes = new SyntaxList<AttributeListSyntax>();

		foreach (var attributeList in declaration.AttributeLists)
		{
			attributes = attributes.Add(attributeList);
		}

		var newAttribute = SyntaxFactory.AttributeList(
			SyntaxFactory.SingletonSeparatedList<AttributeSyntax>(
			SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("AttributeValidation.EnumerableNotNull"))));

		attributes = attributes.Add(newAttribute);

		var newDeclaration = declaration
			.WithAttributeLists(attributes)
			.WithLeadingTrivia(declaration.GetLeadingTrivia());

		var newRoot = root?.ReplaceNode(declaration, newDeclaration);
		if (newRoot == null)
		{
			return document;
		}

		return document.WithSyntaxRoot(newRoot);
	}
}
