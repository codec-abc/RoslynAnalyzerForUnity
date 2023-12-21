namespace AttributeValidation
{
	class BaseValidatableAttribute : UnityEngine.PropertyAttribute { }

	class SingleElementValidatableAttribute : BaseValidatableAttribute { }

	class EnumerableValidatableAttribute : BaseValidatableAttribute { }

	class NotNullAttribute : SingleElementValidatableAttribute { }
	class CanBeNullAttribute : SingleElementValidatableAttribute { }

	class NumericAnyValueAttribute : SingleElementValidatableAttribute { }
	class MinValueAttribute : SingleElementValidatableAttribute { }
	class MaxValueAttribute : SingleElementValidatableAttribute { }

	class StringMatchRegexAttribute : SingleElementValidatableAttribute { }
	class StringNotEmptyAttribute : SingleElementValidatableAttribute { }
	class StringNotWhiteSpaceAttribute : SingleElementValidatableAttribute { }
	class StringAnyValueAttribute : SingleElementValidatableAttribute { }

	class PathValidationAttribute : SingleElementValidatableAttribute { }
	class AddressableValidationAttribute : SingleElementValidatableAttribute { }
	class ResourceValidationAttribute : SingleElementValidatableAttribute { }

	class RequireComponentsAttribute : SingleElementValidatableAttribute { }
	class NoComponentRequired : SingleElementValidatableAttribute { }

	class HierarchyValidationAttribute : SingleElementValidatableAttribute { }
	class NoHierarchyValidationAttribute : SingleElementValidatableAttribute { }

	class EnumerableNotNullAttribute : EnumerableValidatableAttribute { }
	class EnumerableCanBeNullAttribute : EnumerableValidatableAttribute { }

	class EnumerableNumericAnyValueAttribute : EnumerableValidatableAttribute { }
	class EnumerableMinValueAttribute : EnumerableValidatableAttribute { }
	class EnumerableMaxValueAttribute : EnumerableValidatableAttribute { }

	class EnumerableStringMatchRegexAttribute : EnumerableValidatableAttribute { }
	class EnumerableStringNotEmptyAttribute : EnumerableValidatableAttribute { }
	class EnumerableStringNotWhiteSpaceAttribute : EnumerableValidatableAttribute { }

	class EnumerableStringAnyValueAttribute : EnumerableValidatableAttribute { }
	class EnumerablePathValidationAttribute : EnumerableValidatableAttribute { }
	class EnumerableAddressableValidationAttribute : EnumerableValidatableAttribute { }

	class EnumerableResourceValidationAttribute : EnumerableValidatableAttribute { }
	class EnumerableRequireComponentsAttribute : EnumerableValidatableAttribute { }
	class EnumerableNoComponentRequired : EnumerableValidatableAttribute { }

	class EnumerableHierarchyValidationAttribute : EnumerableValidatableAttribute { }
	class EnumerableNoHierarchyValidationAttribute : EnumerableValidatableAttribute { }
}
