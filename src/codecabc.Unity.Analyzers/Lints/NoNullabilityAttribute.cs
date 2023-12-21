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
public class NoNullabilityAttribute : DiagnosticAnalyzer
{
	internal static readonly DiagnosticDescriptor Rule = new(
		id: "ACR0001",
		title: Strings.NoNotNullAttributeErrorDescription,
		messageFormat: Strings.NoNotNullAttributeErrorDescription,
		category: DiagnosticCategory.Correctness,
		defaultSeverity: DiagnosticSeverity.Info,
		isEnabledByDefault: true,
		description: Strings.NoNotNullAttributeErrorDescription);

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

		// Strings can never be null for serialized types in Unity
		if (typeSymbol.Matches(typeof(string)) || typeSymbol.SpecialType == SpecialType.System_String)
		{
			return false;
		}

		// Only ref to Unity's object can be null so ignoring other types.
		if (!Utils.IsExtensionOfUnityObject(typeSymbol, ctx))
		{
			return false;
		}

		if (!Utils.IsSymbolSerializedInFile(typeSymbol, symbol, ctx))
		{
			return false;
		}

		var hasNotNullAttribute = !Utils.HasAnyAttributes(
			symbol,
			typeof(AttributeValidation.NotNullAttribute),
			typeof(AttributeValidation.CanBeNullAttribute));

		return hasNotNullAttribute;
	}
}

[ExportCodeFixProvider(LanguageNames.CSharp)]
public class NoNullabilityAttributeCodeFix : CodeFixProvider
{
	public sealed override ImmutableArray<string> FixableDiagnosticIds =>
		ImmutableArray.Create(NoNullabilityAttribute.Rule.Id);

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
				Strings.NoNotNullAttributeCodeFixTitle,
				ct => AddNotNullAttributeAsync(context.Document, declaration, ct),
				declaration.ToFullString()),
			context.Diagnostics);
	}

	private static async Task<Document> AddNotNullAttributeAsync(
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
			SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("AttributeValidation.NotNull"))));

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
