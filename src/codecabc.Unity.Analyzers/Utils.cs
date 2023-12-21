using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;

namespace codecabc.Unity.Analyzers
{
	internal static class Utils
	{
		// see https://docs.unity3d.com/Manual/script-Serialization.html
		// help from https://www.meziantou.net/working-with-types-in-a-roslyn-analyzer.htm
		// TODO : complete definition
		public static bool CanTypeBeSerializedInUnity(ITypeSymbol? typeSymbol, SyntaxNodeAnalysisContext ctx)
		{
			return
				IsUnitySingleSerializableElement(typeSymbol, ctx) ||
				IsArrayOfSerializedType(typeSymbol, ctx) ||
				IsListOfSerializedType(typeSymbol, ctx);
		}

		public static bool IsExtensionOfUnityObject(ITypeSymbol? typeSymbol, SyntaxNodeAnalysisContext ctx)
		{
			return typeSymbol.Extends(typeof(UnityEngine.Object));
		}

		public static bool IsUnitySingleSerializableElement(ITypeSymbol? typeSymbol, SyntaxNodeAnalysisContext ctx)
		{
			return
				IsExtensionOfUnityObject(typeSymbol, ctx) ||
				IsEnum(typeSymbol, ctx) ||
				IsPrimitiveType(typeSymbol, ctx) ||
				IsUnityBuiltInTypes(typeSymbol, ctx) ||
				HasSerializableAttribute(typeSymbol, ctx);
		}

		public static bool IsSymbolSerializedInFile(
			ITypeSymbol? typeSymbol, 
			ISymbol symbol, 
			SyntaxNodeAnalysisContext ctx)
		{
			if (!Utils.CanTypeBeSerializedInUnity(typeSymbol, ctx))
			{
				return false;
			}

			var hasNonSerialized = HasAnyAttributes(symbol, typeof(System.NonSerializedAttribute));

			if (hasNonSerialized)
			{
				return false;
			}

			var hasSerializedField = HasAnyAttributes(symbol, typeof(UnityEngine.SerializeField));

			if (hasSerializedField)
			{
				return true;
			}

			var isSerialized = symbol switch
			{
				IFieldSymbol fieldSymbol =>
					fieldSymbol.DeclaredAccessibility == Accessibility.Public &&
					!fieldSymbol.IsStatic &&
					!fieldSymbol.IsReadOnly,
				// redundant on public fields and invalid on static/readonly fields
				//IPropertySymbol => true, // Should never be on a property
				_ => false,
			};


			return isSerialized;
		}

		private static bool IsUnityBuiltInTypes(ITypeSymbol? typeSymbol, SyntaxNodeAnalysisContext ctx)
		{
			if (typeSymbol != null && typeSymbol.Matches(typeof(UnityEngine.Vector3)))
			{
				return true;
			}
			if (typeSymbol != null && typeSymbol.Matches(typeof(UnityEngine.Vector2)))
			{
				return true;
			}
			if (typeSymbol != null && typeSymbol.Matches(typeof(UnityEngine.Vector4)))
			{
				return true;
			}
			if (typeSymbol != null && typeSymbol.Matches(typeof(UnityEngine.Quaternion)))
			{
				return true;
			}
			if (typeSymbol != null && typeSymbol.Matches(typeof(UnityEngine.Ray)))
			{
				return true;
			}
			if (typeSymbol != null && typeSymbol.Matches(typeof(UnityEngine.Color)))
			{
				return true;
			}
			if (typeSymbol != null && typeSymbol.Matches(typeof(UnityEngine.Color32)))
			{
				return true;
			}
			if (typeSymbol != null && typeSymbol.Matches(typeof(UnityEngine.Vector2Int)))
			{
				return true;
			}
			if (typeSymbol != null && typeSymbol.Matches(typeof(UnityEngine.Vector3Int)))
			{
				return true;
			}
			if (typeSymbol != null && typeSymbol.Matches(typeof(UnityEngine.Rect)))
			{
				return true;
			}
			if (typeSymbol != null && typeSymbol.Matches(typeof(UnityEngine.Matrix4x4)))
			{
				return true;
			}

			return false;
		}

		public static bool HasSerializableAttribute(ITypeSymbol? typeSymbol, SyntaxNodeAnalysisContext ctx)
		{
			if (typeSymbol == null)
			{
				return true;
			}

			return
				typeSymbol.GetAttributes().Any(a =>
					a.AttributeClass != null &&
					a.AttributeClass.Matches(typeof(System.SerializableAttribute)));
		}

		public static bool IsEnum(ITypeSymbol? typeSymbol, SyntaxNodeAnalysisContext ctx)
		{
			var returned = typeSymbol != null && 
				(typeSymbol.TypeKind == TypeKind.Enum ||
				 typeSymbol.SpecialType == SpecialType.System_Enum );
			return returned;
		}


		public static bool IsPrimitiveType(ITypeSymbol? typeSymbol, SyntaxNodeAnalysisContext ctx)
		{
			var returned = 
				typeSymbol != null &&
				(
					typeSymbol.SpecialType == SpecialType.System_Boolean ||
					typeSymbol.SpecialType == SpecialType.System_Char ||
					typeSymbol.SpecialType == SpecialType.System_SByte ||
					typeSymbol.SpecialType == SpecialType.System_Byte ||
					typeSymbol.SpecialType == SpecialType.System_Int16 ||
					typeSymbol.SpecialType == SpecialType.System_UInt16 ||
					typeSymbol.SpecialType == SpecialType.System_Int32 ||
					typeSymbol.SpecialType == SpecialType.System_UInt32 ||
					typeSymbol.SpecialType == SpecialType.System_Int64 ||
					typeSymbol.SpecialType == SpecialType.System_UInt64 ||
					typeSymbol.SpecialType == SpecialType.System_Decimal ||
					typeSymbol.SpecialType == SpecialType.System_Single ||
					typeSymbol.SpecialType == SpecialType.System_Double ||
					typeSymbol.SpecialType == SpecialType.System_String);

			return returned;
		}

		public static bool IsNumericType(ITypeSymbol? typeSymbol, SyntaxNodeAnalysisContext ctx)
		{
			var returned =
				typeSymbol != null &&
				(
					typeSymbol.SpecialType == SpecialType.System_Char ||
					typeSymbol.SpecialType == SpecialType.System_SByte ||
					typeSymbol.SpecialType == SpecialType.System_Byte ||
					typeSymbol.SpecialType == SpecialType.System_Int16 ||
					typeSymbol.SpecialType == SpecialType.System_UInt16 ||
					typeSymbol.SpecialType == SpecialType.System_Int32 ||
					typeSymbol.SpecialType == SpecialType.System_UInt32 ||
					typeSymbol.SpecialType == SpecialType.System_Int64 ||
					typeSymbol.SpecialType == SpecialType.System_UInt64 ||
					typeSymbol.SpecialType == SpecialType.System_Decimal ||
					typeSymbol.SpecialType == SpecialType.System_Single ||
					typeSymbol.SpecialType == SpecialType.System_Double);

			return returned;
		}

		public static bool IsArrayOfSerializedType(ITypeSymbol? typeSymbol, SyntaxNodeAnalysisContext ctx)
		{
			if (typeSymbol != null)
			{
				if (typeSymbol is IArrayTypeSymbol arrayTypeSymbol)
				{
					var arrayType = arrayTypeSymbol.ElementType;
					return IsUnitySingleSerializableElement(arrayType, ctx);
				}
			}

			return false;
		}

		public static bool TryGetArrayType(
			ITypeSymbol typeSymbol, 
			SyntaxNodeAnalysisContext ctx, 
			out ITypeSymbol arrayType)
		{
			arrayType = null;

			if (typeSymbol != null)
			{
				if (typeSymbol is IArrayTypeSymbol arrayTypeSymbol)
				{
					arrayType = arrayTypeSymbol.ElementType;
					return true;
				}
			}

			return false;
		}

		public static bool IsListOfSerializedType(ITypeSymbol? typeSymbol, SyntaxNodeAnalysisContext ctx)
		{
			if (typeSymbol != null)
			{
				if (typeSymbol is INamedTypeSymbol namedTypeSymbol)
				{
					var def = namedTypeSymbol.OriginalDefinition;
					Compilation? compilation = ctx.Compilation;
					INamedTypeSymbol? list = compilation?.GetTypeByMetadataName("System.Collections.Generic.List`1");

					if (SymbolEqualityComparer.Default.Equals(def, list))
					{
						var typeArg = namedTypeSymbol.TypeArguments[0];
						return IsUnitySingleSerializableElement(typeArg.OriginalDefinition, ctx);
					}
				}
			}

			return false;
		}

		public static bool TryGetGenericListType(
			ITypeSymbol typeSymbol,
			SyntaxNodeAnalysisContext ctx,
			out ITypeSymbol listType)
		{
			listType = null;

			if (typeSymbol != null)
			{
				if (typeSymbol is INamedTypeSymbol namedTypeSymbol)
				{
					var def = namedTypeSymbol.OriginalDefinition;
					Compilation? compilation = ctx.Compilation;
					INamedTypeSymbol? list = compilation?.GetTypeByMetadataName("System.Collections.Generic.List`1");

					if (SymbolEqualityComparer.Default.Equals(def, list))
					{
						var typeArg = namedTypeSymbol.TypeArguments[0];
						listType = typeArg;
						return true;
					}
				}
			}

			return false;
		}

		public static bool IsAutoProperty(IPropertySymbol? propertySymbol)
		{
			if (propertySymbol == null)
			{
				return false;
			}
			// Get fields declared in the same type as the property
			var fields = propertySymbol.ContainingType.GetMembers().OfType<IFieldSymbol>();

			// Check if one field is associated to
			return fields.Any(field => SymbolEqualityComparer.Default.Equals(field.AssociatedSymbol, propertySymbol));
		}

		public static List<IFieldSymbol> GetAutoPropertyFields(IPropertySymbol? propertySymbol)
		{
			if (propertySymbol == null)
			{
				return new List<IFieldSymbol>();
			}
			// Get fields declared in the same type as the property
			var fields = propertySymbol.ContainingType.GetMembers().OfType<IFieldSymbol>();

			// Check if one field is associated to
			return fields.Where(field => SymbolEqualityComparer.Default.Equals(field.AssociatedSymbol, propertySymbol)).ToList();
		}

		public static bool IsSymbolSerializedTypeInUnity(ISymbol symbol, SyntaxNodeAnalysisContext ctx)
		{
			var containingType = symbol.ContainingType;
			if (containingType == null)
			{
				return false;
			}

			if (IsExtensionOfUnityObject(containingType, ctx))
			{
				return true;
			}

			var attributes = containingType.GetAttributes();

			return attributes.Any(a =>
					a.AttributeClass != null &&
					a.AttributeClass.Matches(typeof(System.SerializableAttribute)));
		}

		public static bool HasAnyAttributes(ISymbol symbol, params Type[] types)
		{
			return symbol.GetAttributes().Any(
				a => 
					a.AttributeClass != null && 
					(types.Any(t =>
						a.AttributeClass.Matches(t))
						));
		}

		public static bool HasAllAttributes(ISymbol symbol, params Type[] types)
		{
			return symbol.GetAttributes().Any(
				a => a.AttributeClass != null && (
				types.All(t =>
					a.AttributeClass.Matches(t))
				));
		}


	}
}
