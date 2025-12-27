package com.gdn.x.productcategorybase.util;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.ProductDTO;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;

import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.ExtractionType;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateDTO;
import com.gdn.x.productcategorybase.dto.response.AttributeHistoryResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.outbound.model.MatrixAttributeExtractionResponse;
import com.gdn.x.productcategorybase.outbound.model.SingleBaseResponse;

public class ProductUtil {

  public static boolean isProductDetailChanged(Product newProduct, Product oldProduct) {
    String newDescription = StringUtils.EMPTY;
    String oldDescription = StringUtils.EMPTY;
    if (Objects.nonNull(newProduct.getDescription())) {
      newDescription = new String(newProduct.getDescription());
    }
    if (Objects.nonNull(oldProduct.getDescription())) {
      oldDescription = new String(oldProduct.getDescription());
    }
    return !StringUtils.equals(newProduct.getProductCategories().get(0).getCategory().getCategoryCode(),
        oldProduct.getProductCategories().get(0).getCategory().getCategoryCode()) || !StringUtils
        .equals(newProduct.getName(), oldProduct.getName()) || !StringUtils
        .equals(newProduct.getBrand(), oldProduct.getBrand()) || !StringUtils
        .equals(newProduct.getUniqueSellingPoint(), oldProduct.getUniqueSellingPoint()) || !StringUtils
        .equals(newDescription, oldDescription);
  }

  public static boolean isDimensionUpdated(Product product, SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO) {
    if (validateDimensionValue(product.getLength(), simpleMasterProductUpdateDTO.getLength()) && validateDimensionValue(
        product.getWidth(), simpleMasterProductUpdateDTO.getWidth()) && validateDimensionValue(product.getHeight(),
        simpleMasterProductUpdateDTO.getHeight()) && validateDimensionValue(product.getWeight(),
        simpleMasterProductUpdateDTO.getWeight()) && validateDimensionValue(product.getShippingWeight(),
        simpleMasterProductUpdateDTO.getShippingWeight())) {
      return isDimensionValueUpdated(product.getLength(), simpleMasterProductUpdateDTO.getLength()) || isDimensionValueUpdated(
          product.getWidth(), simpleMasterProductUpdateDTO.getWidth()) || isDimensionValueUpdated(product.getHeight(),
          simpleMasterProductUpdateDTO.getHeight()) || isDimensionValueUpdated(product.getWeight(),
          simpleMasterProductUpdateDTO.getWeight()) || isDimensionValueUpdated(product.getShippingWeight(),
          simpleMasterProductUpdateDTO.getShippingWeight());
    }
    return false;
  }

  public static void getSimpleMasterProductUpdateDTOFromProduct(
      SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO, Product product) {
    simpleMasterProductUpdateDTO.setLength(product.getLength());
    simpleMasterProductUpdateDTO.setWidth(product.getWidth());
    simpleMasterProductUpdateDTO.setHeight(product.getHeight());
    simpleMasterProductUpdateDTO.setWeight(product.getWeight());
    simpleMasterProductUpdateDTO.setShippingWeight(product.getShippingWeight());
  }

  public static void getSimpleMasterProductUpdateDTOFromProduct(
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO, ProductDTO productDTO) {
    simpleMasterProductUpdateDTO.setLength(productDTO.getLength());
    simpleMasterProductUpdateDTO.setWidth(productDTO.getWidth());
    simpleMasterProductUpdateDTO.setHeight(productDTO.getHeight());
    simpleMasterProductUpdateDTO.setWeight(productDTO.getWeight());
    simpleMasterProductUpdateDTO.setShippingWeight(productDTO.getShippingWeight());
  }

  private static boolean validateDimensionValue(Double productValue, Double simpleMasterProductUpdateValue) {
    return Objects.nonNull(productValue) && Objects.nonNull(simpleMasterProductUpdateValue);
  }

  private static boolean isDimensionValueUpdated(Double value1, Double value2) {
    return Double.compare(value1, value2) != 0;
  }

  public static boolean isDimensionOrDgLevelUpdated(Product product,
      SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO) {
    return !compareDoubleValues(product.getLength(), simpleMasterProductUpdateDTO.getLength()) || !compareDoubleValues(
        product.getWidth(), simpleMasterProductUpdateDTO.getWidth()) || !compareDoubleValues(product.getHeight(),
        simpleMasterProductUpdateDTO.getHeight()) || !compareDoubleValues(product.getWeight(),
        simpleMasterProductUpdateDTO.getWeight()) || !compareDoubleValues(product.getShippingWeight(),
        simpleMasterProductUpdateDTO.getShippingWeight()) || !compareIntegerValues(
        product.getProductItems().get(0).getDangerousGoodsLevel(),
        simpleMasterProductUpdateDTO.getDangerousGoodsLevel());
  }

  private static boolean compareDoubleValues(Double value1, Double value2) {
    return Objects.isNull(value1) ?
        Objects.isNull(value2) :
        (Objects.isNull(value2) ? false : value1.doubleValue() == value2.doubleValue());
  }

  private static boolean compareIntegerValues(Integer value1, Integer value2) {
    return Objects.isNull(value1) ?
        Objects.isNull(value2) :
        (Objects.isNull(value2) ? false : value1.intValue() == value2.intValue());
  }

  public static List<ProductAttribute> getProductAttributesEligibleForAutoFill(Product product) {
    List<ProductAttribute> productAttributeListEligibleForAutoFill = new ArrayList<>();
    Category category = product.getProductCategories().get(0).getCategory();
    if (ExtractionType.INLINE_TEXT.equals(category.getExtractionType()) && CollectionUtils.isNotEmpty(
        product.getProductAttributes())) {
      for (ProductAttribute productAttribute : product.getProductAttributes()) {
        if (isAttributeEligibleForAutoFill(productAttribute.getAttribute()) && isProductValueEmpty(
            productAttribute.getProductAttributeValues())) {
          productAttributeListEligibleForAutoFill.add(productAttribute);
        }
      }
    }
    return productAttributeListEligibleForAutoFill;
  }

  public static List<ProductAttribute> getCategoryNewAttributesEligibleForAutoFill(Product product) {
    List<ProductAttribute> productAttributeListEligibleForAutoFill = new ArrayList<>();
    Category category = product.getProductCategories().get(0).getCategory();
    Set<String> attributeMappedToProduct =
        Optional.ofNullable(product.getProductAttributes()).orElse(new ArrayList<>()).stream()
            .map(ProductAttribute::getAttribute).map(Attribute::getAttributeCode).collect(Collectors.toSet());
    if (ExtractionType.INLINE_TEXT.equals(category.getExtractionType()) && CollectionUtils.isNotEmpty(
        category.getCategoryAttributes())) {
      for (CategoryAttribute categoryAttribute : category.getCategoryAttributes()) {
        if (!attributeMappedToProduct.contains(categoryAttribute.getAttribute().getAttributeCode())
            && isAttributeEligibleForAutoFill(categoryAttribute.getAttribute())) {
          productAttributeListEligibleForAutoFill.add(
              ConverterUtil.generateNewProductAttributeForAutoFill(product, categoryAttribute.getAttribute()));
        }
      }
    }
    return productAttributeListEligibleForAutoFill;
  }

  public static List<AttributeHistoryResponse> extractAndSetValue(Product product,
      List<ProductAttribute> productAttributeListEligibleForAutoFill,
      List<ProductAttribute> newProductAttributeListEligibleForAutoFill, Map<String, String> attributeNameAndValueMap) {
    List<AttributeHistoryResponse> attributeHistoryResponseList = new ArrayList<>();

    for (ProductAttribute productAttribute : productAttributeListEligibleForAutoFill) {
      boolean isValueUpdated = updateExtractedAttributeValue(productAttribute, attributeNameAndValueMap);
      if (isValueUpdated) {
        attributeHistoryResponseList.add(new AttributeHistoryResponse(productAttribute.getAttribute().getName(),
            productAttribute.getProductAttributeValues().get(0).getDescriptiveAttributeValue()));
      }
    }

    for (ProductAttribute productAttribute : newProductAttributeListEligibleForAutoFill) {
      boolean isValueUpdated = updateExtractedAttributeValue(productAttribute, attributeNameAndValueMap);
      if (isValueUpdated) {
        product.getProductAttributes().add(productAttribute);
        attributeHistoryResponseList.add(new AttributeHistoryResponse(productAttribute.getAttribute().getName(),
            productAttribute.getProductAttributeValues().get(0).getDescriptiveAttributeValue()));
      }
    }
    return attributeHistoryResponseList;
  }

  public static boolean updateExtractedAttributeValue(ProductAttribute productAttribute,
      Map<String, String> attributeNameAndValueMap) {
    if (attributeNameAndValueMap.containsKey(getDsAttributeNameOrAttributeName(productAttribute))) {
      productAttribute.getProductAttributeValues().get(0)
          .setDescriptiveAttributeValue(attributeNameAndValueMap.get(getDsAttributeNameOrAttributeName(productAttribute)));
      productAttribute.getProductAttributeValues().get(0)
          .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
      productAttribute.setExtractedValue(true);
      return true;
    }
    return false;
  }

  private static String getDsAttributeNameOrAttributeName(ProductAttribute productAttribute) {
    return StringUtils.isNotBlank(productAttribute.getAttribute().getDsAttributeName()) ?
        productAttribute.getAttribute().getDsAttributeName() :
        productAttribute.getAttribute().getName();
  }

  public static boolean isExtractedValueValid(Map.Entry<String, Object> entry) {
    return StringUtils.isNotBlank(entry.getKey()) && Objects.nonNull(entry.getValue())
        && entry.getValue() instanceof String && org.apache.commons.lang3.StringUtils.isNotBlank(
        String.valueOf(entry.getValue()));
  }

  public static boolean isAttributeExtractionResponseValid(
      SingleBaseResponse<MatrixAttributeExtractionResponse> matrixAttributeExtractionResponse) {
    return Objects.nonNull(matrixAttributeExtractionResponse) && Objects.nonNull(
        matrixAttributeExtractionResponse.getValue()) && MapUtils.isNotEmpty(
        matrixAttributeExtractionResponse.getValue().getExtractedAttributes());
  }

  public static boolean isAttributeEligibleForAutoFill(Attribute attribute) {
    return AttributeType.DESCRIPTIVE_ATTRIBUTE.equals(attribute.getAttributeType()) && !attribute.isVariantCreation()
        && !attribute.isSkuValue();
  }

  public static boolean isProductValueEmpty(List<ProductAttributeValue> productAttributeValues) {
    return CollectionUtils.isNotEmpty(productAttributeValues) && (
        org.apache.commons.lang3.StringUtils.isBlank(productAttributeValues.get(0).getDescriptiveAttributeValue())
            || Constants.HYPHEN.equals(productAttributeValues.get(0).getDescriptiveAttributeValue()));
  }

  public static void resetExtractedAttributeValueFlag(Product product, boolean resetExtractedAttributeValue) {
    if (resetExtractedAttributeValue && CollectionUtils.isNotEmpty(product.getProductAttributes())) {
      product.getProductAttributes().forEach(productAttribute -> productAttribute.setExtractedValue(false));
    }
  }

  public static void validateProductItemAttributes(Product product, boolean validateProductItemAttributeEnabled) {
    if (validateProductItemAttributeEnabled) {
      List<ProductItem> productItems = Optional.ofNullable(product.getProductItems()).orElse(new ArrayList<>()).stream()
          .filter(Predicate.not(ProductItem::isMarkForDelete)).collect(Collectors.toList());
      if (CollectionUtils.isNotEmpty(productItems)) {
        ProductItem firstProductItem = productItems.stream().findFirst().get();
        Set<String> firstItemVariantCreatingAttributes = Optional.ofNullable(
            Optional.ofNullable(firstProductItem.getProductItemAttributeValues()).orElse(new ArrayList<>()).stream()
                .filter(Predicate.not(ProductItemAttributeValue::isMarkForDelete))
                .map(ProductItemAttributeValue::getAttribute).filter(ProductUtil::isVariantCreatingAttribute)
                .map(Attribute::getAttributeCode).collect(Collectors.toSet())).orElse(new HashSet<>());
        for (ProductItem productItem : productItems) {
          Set<String> itemVariantCreatingAttributes = Optional.ofNullable(
              Optional.ofNullable(productItem.getProductItemAttributeValues()).orElse(new ArrayList<>()).stream()
                  .filter(Predicate.not(ProductItemAttributeValue::isMarkForDelete))
                  .map(ProductItemAttributeValue::getAttribute).filter(ProductUtil::isVariantCreatingAttribute)
                  .map(Attribute::getAttributeCode).collect(Collectors.toSet())).orElse(new HashSet<>());
          if (!firstItemVariantCreatingAttributes.equals(itemVariantCreatingAttributes)) {
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
                ErrorMessage.PRODUCT_VARIANT_IS_NOT_ALLOWED.getMessage());
          }
        }
      }
    }
  }

  private static boolean isVariantCreatingAttribute(Attribute attribute) {
    return AttributeType.DEFINING_ATTRIBUTE.equals(attribute.getAttributeType()) || attribute.isVariantCreation();
  }

  public static void validateProductBasicInfoBatchSize(List<String> productCodes, int productBasicInfoFetchBatchSize) {
    if (productCodes.size() > productBasicInfoFetchBatchSize) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.PRODUCT_BATCH_SIZE_EXCEEDED.getMessage());
    }
  }

}
