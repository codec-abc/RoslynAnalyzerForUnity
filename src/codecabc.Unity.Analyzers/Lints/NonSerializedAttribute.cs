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
public class NonSerializedAttributeForUnityObjects : DiagnosticAnalyzer
{
	internal static readonly DiagnosticDescriptor Rule = new(
		id: "ACR0002",
		title: Strings.NonSerializedAttributeForUnityObjectsErrorDescription,
		messageFormat: Strings.NonSerializedAttributeForUnityObjectsErrorDescription,
		category: DiagnosticCategory.Correctness,
		defaultSeverity: DiagnosticSeverity.Info,
		isEnabledByDefault: true,
		description: Strings.NonSerializedAttributeForUnityObjectsErrorDescription);

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
			case PropertyDeclarationSyntax pdec:
				var propSymbol = model.GetDeclaredSymbol(pdec);
				symbol = propSymbol;
				typeInfo = model.GetTypeInfo(pdec);
				typeSyntax = pdec.Type;
				typeSymbol = model.GetTypeInfo(pdec.Type).Type;

				if (!Utils.IsAutoProperty(propSymbol))
				{
					return;
				}

				break;
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
		if (!Utils.CanTypeBeSerializedInUnity(typeSymbol, ctx))
		{
			return false;
		}

		if (symbol is IPropertySymbol propertySymbol_)
		{
			var props = Utils.GetAutoPropertyFields(propertySymbol_);

			var hasNonSerialized =
				props.Any(symbol => Utils.HasAnyAttributes(symbol, typeof(System.NonSerializedAttribute)));

			if (hasNonSerialized)
			{
				return false;
			}
		}
		else if (
			symbol.GetAttributes().Any(
			a =>
			{
				return
					a.AttributeClass != null && (
					a.AttributeClass.Matches(typeof(System.NonSerializedAttribute)) ||
					a.AttributeClass.Matches(typeof(UnityEngine.SerializeField)));
				
			}
		))
		{
			return false;
		}
		

		// In case we want to create a IgnoreNonSerialized attribute to discard this warning.

		//if (symbol.GetAttributes().Any(
		//	a =>
		//	{
		//		var result =
		//			a.AttributeClass != null &&
		//			(
		//				a.AttributeClass.Matches(typeof(AttributeValidation.IgnoreNonSerialized))
		//			);

		//		return result;
		//	}))
		//{
		//	return false;
		//}

		return symbol switch
		{
			IFieldSymbol fieldSymbol =>
				fieldSymbol.DeclaredAccessibility != Accessibility.Public &&
				fieldSymbol.DeclaredAccessibility != Accessibility.Internal &&
				!fieldSymbol.IsStatic &&
				!fieldSymbol.IsConst &&
				!fieldSymbol.IsReadOnly,

			IPropertySymbol propertySymbol =>
				!propertySymbol.IsStatic &&
				!propertySymbol.IsReadOnly,

			_ => false,
		};


	}
}

[ExportCodeFixProvider(LanguageNames.CSharp)]
public class NonSerializedAttributeForUnityCodeFix : CodeFixProvider
{
	public sealed override ImmutableArray<string> FixableDiagnosticIds =>
		ImmutableArray.Create(NonSerializedAttributeForUnityObjects.Rule.Id);

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
				Strings.NonSerializedAttributeForUnityObjectsCodeFixTitle,
				ct => AddNonSerializedAttributeAsync(context.Document, declaration, ct),
				declaration.ToFullString()),
			context.Diagnostics);
	}

	private static async Task<Document> AddNonSerializedAttributeAsync(
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
			SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("System.NonSerialized"))));

		if (declaration is PropertyDeclarationSyntax)
		{
			newAttribute = SyntaxFactory.AttributeList(
			SyntaxFactory.SingletonSeparatedList<AttributeSyntax>(
			SyntaxFactory.Attribute(SyntaxFactory.IdentifierName("field: System.NonSerialized"))));
		}

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
