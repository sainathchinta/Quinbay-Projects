package com.gdn.mta.product.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.util.CollectionUtils;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductFieldHistory;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

public final class ProductContentUtil {

  private static final String NAME_FIELD = "Nama";
  private static final String BRAND_FIELD = "Brand";
  private static final String DESCRIPTION_FIELD = "Deskripsi";
  private static final String UNIQUE_SELLING_POINT_FIELD = "Unique Selling Point";
  private static final String PRODUCT_STORY_FIELD = "Product Story";
  private static final String LENGTH_FIELD = "Panjang";
  private static final String WIDTH_FIELD = "Lebar";
  private static final String HEIGHT_FIELD = "Tinggi";
  private static final String WEIGHT_FIELD = "Berat";
  private static final String DANGEROUS_GOOD_LEVEL_FIELD = "Dangerous Good Level";
  private static final String UPC_CODE_FIELD = "Kode Manufaktur";
  private static final String ATTRIBUTE_FIELD = "Atribut";
  private static final String HTML_ELEMENT_OPEN_P_TAG = "<p>";
  private static final String HTML_ELEMENT_CLOSE_P_TAG = "</p>";

  public static List<ProductFieldHistory> getProductContentDifference(ProductRequest newProduct,
      ProductDetailResponse oldProduct) throws Exception {
    List<ProductFieldHistory> productFieldHistories = new ArrayList<ProductFieldHistory>();
    if (!StringUtils.equals(oldProduct.getName(), newProduct.getName())) {
      productFieldHistories.add(new ProductFieldHistory(ProductContentUtil.NAME_FIELD, oldProduct
          .getName(), newProduct.getName()));
    }
    if (!StringUtils.equals(oldProduct.getBrand(), newProduct.getBrand())) {
      productFieldHistories.add(new ProductFieldHistory(ProductContentUtil.BRAND_FIELD, oldProduct
          .getBrand(), newProduct.getBrand()));
    }
    if (!StringUtils.equals(
        new String(oldProduct.getDescription() != null ? oldProduct.getDescription() : new byte[1])
            .trim(), new String(newProduct.getDescription() != null ? newProduct.getDescription()
            : new byte[1]).trim())) {
      productFieldHistories.add(new ProductFieldHistory(ProductContentUtil.DESCRIPTION_FIELD,
          removeHtmlElementPTag(new String(oldProduct.getDescription()).trim()),
          removeHtmlElementPTag(new String(newProduct.getDescription()).trim())));
    }
    if (!StringUtils.equals(oldProduct.getUniqueSellingPoint(), newProduct.getUniqueSellingPoint())) {
      productFieldHistories
          .add(new ProductFieldHistory(ProductContentUtil.UNIQUE_SELLING_POINT_FIELD,
              removeHtmlElementPTag(oldProduct.getUniqueSellingPoint()),
              removeHtmlElementPTag(newProduct.getUniqueSellingPoint())));
    }
    if (!StringUtils.equals(oldProduct.getProductStory() != null ? oldProduct.getProductStory()
        .trim() : new String(), newProduct.getProductStory() != null ? newProduct.getProductStory()
        .trim() : new String())) {
      productFieldHistories.add(new ProductFieldHistory(ProductContentUtil.PRODUCT_STORY_FIELD,
          removeHtmlElementPTag(oldProduct.getProductStory() != null ? oldProduct.getProductStory().trim() : new String()),
          removeHtmlElementPTag(newProduct.getProductStory() != null ? newProduct.getProductStory().trim() : new String())));
    }
    if (!StringUtils.equals(String.valueOf(oldProduct.getLength()),
        String.valueOf(newProduct.getLength()))) {
      productFieldHistories.add(new ProductFieldHistory(ProductContentUtil.LENGTH_FIELD, String
          .valueOf(oldProduct.getLength()), String.valueOf(newProduct.getLength())));
    }
    if (!StringUtils.equals(String.valueOf(oldProduct.getWidth()),
        String.valueOf(newProduct.getWidth()))) {
      productFieldHistories.add(new ProductFieldHistory(ProductContentUtil.WIDTH_FIELD, String
          .valueOf(oldProduct.getWidth()), String.valueOf(newProduct.getWidth())));
    }
    if (!StringUtils.equals(String.valueOf(oldProduct.getHeight()),
        String.valueOf(newProduct.getHeight()))) {
      productFieldHistories.add(new ProductFieldHistory(ProductContentUtil.HEIGHT_FIELD, String
          .valueOf(oldProduct.getHeight()), String.valueOf(newProduct.getHeight())));
    }
    if (!StringUtils.equals(String.valueOf(oldProduct.getWeight()),
        String.valueOf(newProduct.getWeight()))) {
      productFieldHistories.add(new ProductFieldHistory(ProductContentUtil.WEIGHT_FIELD, String
          .valueOf(oldProduct.getWeight()), String.valueOf(newProduct.getWeight())));
    }
    Map<String, ProductItemRequest> newProductItems = new HashMap<String, ProductItemRequest>();
    for (ProductItemRequest newProductItem : newProduct.getProductItems()) {
      newProductItems.put(newProductItem.getSkuCode(), newProductItem);
    }
    for (ProductItemResponse oldProductItem : oldProduct.getProductItemResponses()) {
      ProductItemRequest newProductItem = newProductItems.get(oldProductItem.getSkuCode());
      if (newProductItem != null) {
        if (!StringUtils.equals(String.valueOf(oldProductItem.getDangerousGoodsLevel()),
            String.valueOf(newProductItem.getDangerousGoodsLevel()))) {
          productFieldHistories.add(new ProductFieldHistory(oldProductItem.getGeneratedItemName()
              + " " + ProductContentUtil.DANGEROUS_GOOD_LEVEL_FIELD, String.valueOf(oldProductItem
              .getDangerousGoodsLevel()), String.valueOf(newProductItem.getDangerousGoodsLevel())));
        }
        if (!StringUtils.equals(oldProductItem.getUpcCode(), newProductItem.getUpcCode())) {
          productFieldHistories.add(new ProductFieldHistory(oldProductItem.getGeneratedItemName()
              + " " + ProductContentUtil.UPC_CODE_FIELD, oldProductItem.getUpcCode(),
              newProductItem.getUpcCode()));
        }
      }
    }
    Map<String, ProductAttributeRequest> newProductAttributes =
        new HashMap<String, ProductAttributeRequest>();
    for (ProductAttributeRequest newProductAttribute : newProduct.getProductAttributes()) {
      if (!AttributeType.DEFINING_ATTRIBUTE.equals(newProductAttribute.getAttribute()
          .getAttributeType())) {
        newProductAttributes.put(newProductAttribute.getId(), newProductAttribute);
      }
    }
    for (ProductAttributeResponse oldProductAttribute : oldProduct.getProductAttributeResponses()) {
      ProductAttributeRequest newProductAttribute =
          newProductAttributes.get(oldProductAttribute.getId());
      if (newProductAttribute != null) {
        if (!oldProductAttribute.getProductAttributeValues().isEmpty() && !newProductAttribute
            .getProductAttributeValues().isEmpty()) {
          if (oldProductAttribute.getAttribute() != null && AttributeType.DESCRIPTIVE_ATTRIBUTE
              .name().equals(oldProductAttribute.getAttribute().getAttributeType())) {
            if (!StringUtils.equals(oldProductAttribute.getProductAttributeValues().get(0)
                    .getDescriptiveAttributeValue(),
                newProductAttribute.getProductAttributeValues().get(0)
                    .getDescriptiveAttributeValue())) {
              productFieldHistories.add(new ProductFieldHistory(
                  ProductContentUtil.ATTRIBUTE_FIELD + " " + oldProductAttribute.getAttribute()
                      .getName(), oldProductAttribute.getProductAttributeValues().get(0)
                  .getDescriptiveAttributeValue(),
                  newProductAttribute.getProductAttributeValues().get(0)
                      .getDescriptiveAttributeValue()));
            }
          } else if (oldProductAttribute.getAttribute() != null
              && AttributeType.PREDEFINED_ATTRIBUTE.name()
              .equals(oldProductAttribute.getAttribute().getAttributeType())) {
            PredefinedAllowedAttributeValueResponse oldValueResponse =
                oldProductAttribute.getProductAttributeValues().get(0)
                    .getPredefinedAllowedAttributeValue();
            PredefinedAllowedAttributeValueRequest newValueResponse =
                newProductAttribute.getProductAttributeValues().get(0)
                    .getPredefinedAllowedAttributeValue();
            if (oldValueResponse != null && newValueResponse != null && !StringUtils
                .equals(oldValueResponse.getValue(), newValueResponse.getValue())) {
              productFieldHistories.add(new ProductFieldHistory(
                  ProductContentUtil.ATTRIBUTE_FIELD + " " + oldProductAttribute.getAttribute()
                      .getName(), oldValueResponse.getValue(), newValueResponse.getValue()));
            }
          }
        }
      }
    }
    return productFieldHistories;
  }

  private static String removeHtmlElementPTag(String source) throws Exception {
    String destination =
        StringUtils.removeStart(source, ProductContentUtil.HTML_ELEMENT_OPEN_P_TAG);
    destination = StringUtils.removeEnd(destination, ProductContentUtil.HTML_ELEMENT_CLOSE_P_TAG);
    return destination;
  }

  public static boolean isWarnaAndDefiningAttributeOrVariantCreationTrue(
      ProductLevel3AttributeRequest productLevel3AttributeRequest) {
    return Constants.WARNA.equals(productLevel3AttributeRequest.getAttributeName()) && (
        AttributeType.DEFINING_ATTRIBUTE.name().equals(productLevel3AttributeRequest.getAttributeType())
            || productLevel3AttributeRequest.isVariantCreation());
  }

  public static boolean isFamilyColourAttributeNotPresentInItem(
      ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest,
      String familyColourAttributeCode) {
    return !Optional.ofNullable(productVariantPriceStockAndImagesRequest.getAttributesMap()).orElse(new TreeMap<>())
        .containsKey(familyColourAttributeCode);
  }

  public static boolean isFamilyColourPresentInCategoryAttribute(CategoryAttributeResponse categoryAttributeResponse,
      String familyColourAttributeCode) {
    return familyColourAttributeCode.equals(categoryAttributeResponse.getAttribute().getAttributeCode());
  }

  private static ProductItemAttributeValueRequest getNoFamilyColourProductItemAttributeValueRequest(
      CategoryAttributeResponse categoryAttributeResponse) {
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setName(categoryAttributeResponse.getAttribute().getName());
    attributeRequest.setAttributeCode(categoryAttributeResponse.getAttribute().getAttributeCode());
    attributeRequest.setAttributeType(
        AttributeType.valueOf(categoryAttributeResponse.getAttribute().getAttributeType()));
    attributeRequest.setSearchAble(categoryAttributeResponse.getAttribute().isSearchAble());
    attributeRequest.setMandatory(categoryAttributeResponse.getAttribute().isMandatory());
    attributeRequest.setDescription(categoryAttributeResponse.getAttribute().getDescription());
    attributeRequest.setSkuValue(categoryAttributeResponse.getAttribute().isSkuValue());
    attributeRequest.setBasicView(categoryAttributeResponse.getAttribute().isBasicView());
    attributeRequest.setScreeningMandatory(categoryAttributeResponse.getAttribute().isScreeningMandatory());
    attributeRequest.setVariantCreation(categoryAttributeResponse.getAttribute().isVariantCreation());
    attributeRequest.setVariantCreatingUI(categoryAttributeResponse.getAttribute().isVariantCreatingUI());
    attributeRequest.setId(categoryAttributeResponse.getAttribute().getId());
    ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
    productItemAttributeValueRequest.setAttribute(attributeRequest);
    productItemAttributeValueRequest.setValue(StringUtils.EMPTY);
    return productItemAttributeValueRequest;
  }

  private static boolean isWarnaAndDefiningAttributeOrVariantCreationTrue(
      ProductAttributeRequest productAttributeRequest) {
    return Constants.WARNA.equals(productAttributeRequest.getAttribute().getName()) && (
        AttributeType.DEFINING_ATTRIBUTE.equals(productAttributeRequest.getAttribute().getAttributeType())
            || productAttributeRequest.getAttribute().isVariantCreation());
  }

  private static boolean isFamilyColourAttributeNotPresentInItemRequest(
      ProductItemCreationRequest productItemCreationRequest, String familyColourAttributeCode) {
    return Optional.ofNullable(productItemCreationRequest.getProductItemAttributeValueRequests())
        .orElse(new ArrayList<>()).stream().noneMatch(
            productItemAttributeValueRequest -> familyColourAttributeCode.equals(
                productItemAttributeValueRequest.getAttribute().getAttributeCode()));
  }

  public static void autoFillFamilyColourItemAttribute(ProductCreationRequest request,
      CategoryDetailResponse categoryDetail, String familyColourAttributeCode, boolean autoFillFamilyColourAttribute) {
    if (autoFillFamilyColourAttribute) {
      boolean isWarnaPresentAndDefiningAttributeType =
          Optional.ofNullable(request.getProductAttributes()).orElse(new ArrayList<>()).stream()
              .anyMatch(ProductContentUtil::isWarnaAndDefiningAttributeOrVariantCreationTrue);
      if (isWarnaPresentAndDefiningAttributeType) {

        // 1st step to check if family colour present in category attribute
        CategoryAttributeResponse familyColourPresentInCategoryAttribute =
            Optional.ofNullable(categoryDetail).map(CategoryDetailResponse::getCategoryAttributes)
                .orElse(new ArrayList<>()).stream().filter(
                    categoryAttributeResponse -> ProductContentUtil.isFamilyColourPresentInCategoryAttribute(
                        categoryAttributeResponse, familyColourAttributeCode)).findFirst().orElse(null);

        // 2nd step list all items which don't have family colour attribute
        List<ProductItemCreationRequest> productItemCreationRequests = request.getProductItemRequests().stream().filter(
            productItemCreationRequest -> ProductContentUtil.isFamilyColourAttributeNotPresentInItemRequest(
                productItemCreationRequest, familyColourAttributeCode)).collect(Collectors.toList());

        // 3rd step if isFamilyColourPresentInCategoryAttribute != null and productItemCreationRequests is not empty populate family colour in item attribute request
        if (Objects.nonNull(familyColourPresentInCategoryAttribute) && !CollectionUtils.isEmpty(
            productItemCreationRequests)) {
          ProductItemAttributeValueRequest familyColourProductItemAttributeValueRequest =
              ProductContentUtil.getNoFamilyColourProductItemAttributeValueRequest(
                  familyColourPresentInCategoryAttribute);
          for (ProductItemCreationRequest productItemCreationRequest : productItemCreationRequests) {
            List<ProductItemAttributeValueRequest> productItemAttributeValueRequests =
                Optional.ofNullable(productItemCreationRequest.getProductItemAttributeValueRequests())
                    .orElse(new ArrayList<>());
            productItemAttributeValueRequests.add(familyColourProductItemAttributeValueRequest);
            productItemCreationRequest.setProductItemAttributeValueRequests(productItemAttributeValueRequests);
          }
        }
      }
    }
  }

  public static void validateAttributeMapInProductItemRequest(boolean validateAttributeMapForNewlyAddedItems,
      List<ProductVariantPriceStockAndImagesRequest> newlyAddedProductItemRequests) {
    if (validateAttributeMapForNewlyAddedItems) {
      Set<String> firstNewlyAddedItemAttributeMapAttributeCodes = newlyAddedProductItemRequests.stream().findFirst()
          .map(ProductVariantPriceStockAndImagesRequest::getAttributesMap).map(Map::keySet).orElse(new HashSet<>());
      for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : newlyAddedProductItemRequests) {
        Set<String> attributeCodes =
            Optional.ofNullable(productVariantPriceStockAndImagesRequest.getAttributesMap()).map(Map::keySet)
                .orElse(new HashSet<>());
        if (!firstNewlyAddedItemAttributeMapAttributeCodes.equals(attributeCodes)) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              ApiErrorCode.ATTRIBUTE_MAP_CONTAIN_DIFFERENT_ATTRIBUTE_FOR_NEWLY_ADDED_ITEM.getDesc());
        }
      }
    }
  }
}
