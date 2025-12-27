package com.gdn.x.productcategorybase.util;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;

import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.ProductMigrationStatus;
import com.gdn.x.productcategorybase.domain.event.model.CommonImageBackfillingEventModel;
import com.gdn.x.productcategorybase.domain.event.model.DimensionsAndUomEventModel;
import com.gdn.x.productcategorybase.domain.event.model.DistributionItemInfoEventModel;
import com.gdn.x.productcategorybase.domain.event.model.InternalProductHistoryEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSuitabilityAttributeModel;
import com.gdn.x.productcategorybase.domain.event.model.SizeChartUpdateEventModel;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.AttributeValueDTO;
import com.gdn.x.productcategorybase.RestrictedKeywordActivity;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordActivityHistory;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordHistoryEventModel;
import com.gdn.x.productcategorybase.dto.MigrationPayload;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductMasterDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductMigrationRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AiGeneratedFieldsResponse;
import com.gdn.x.productcategorybase.dto.response.AuditTrailDto;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailMapResponse;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailResponse;
import com.gdn.x.productcategorybase.dto.response.DimensionsAndUomResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoPerSkuResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionItemInfoResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataItemResponse;
import com.gdn.x.productcategorybase.dto.response.UomType;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.Origin;
import com.gdn.x.productcategorybase.entity.ProductItemUomInfo;
import com.gdn.x.productcategorybase.entity.ProductMigration;
import com.gdn.x.productcategorybase.entity.RestrictedKeyword;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.tuple.Pair;

import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.inventory.dto.WarehouseMasterSKUEvent;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ProductPublishEventType;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemDomainEventModel;
import com.gdn.x.productcategorybase.dto.ProductAndItemLevelUpdatesDTO;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateDTO;
import com.gdn.x.productcategorybase.dto.UpdatedFieldsDTO;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductItemImageRequest;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.entity.SizeChart;
import com.gdn.x.productcategorybase.enums.UpdatedFields;
import lombok.extern.slf4j.Slf4j;
import com.gdn.common.util.BeanUtils;
import com.gdn.x.productcategorybase.BrandAuthorisationActivity;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationWip;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateWipRequest;
import com.gdn.x.productcategorybase.BrandAuthorizationWipStatus;

@Slf4j
public class CommonUtil {

  private static final String OLD_ATTRIBUTE_VALUE = "Old Values → ";
  private static final String NEW_ATTRIBUTE_VALUE = "\nNew Values → ";
  private static final String PRODUCT_NAME = "productName";
  private static final String CATEGORY_NAME = "categoryName";

  public static void productDomainEventModelFlagUpdateBasedOnProductOrItemLevelChange(ProductDomainEventModel
          productDomainEventModel, boolean productLevelDataUpdated, Set<String> updatedItemSkuCodes) {
    if (!productLevelDataUpdated) {
      if (CollectionUtils.isEmpty(productDomainEventModel.getEventTypes())) {
        productDomainEventModel.setEventTypes(new HashSet<>());
      }
      productDomainEventModel.getEventTypes().add(ProductPublishEventType.PUBLISH_SPECIFIC_ITEM_DATA_CHANGE_EVENT.name());
      if (CollectionUtils.isNotEmpty(updatedItemSkuCodes)) {
        for (ProductItemDomainEventModel productItemDomainEventModel : Optional.ofNullable(productDomainEventModel
            .getProductItems()).orElse(new ArrayList<>())) {
          if (updatedItemSkuCodes.contains(productItemDomainEventModel.getSkuCode())) {
            productItemDomainEventModel.setPublishL4(true);
          }
        }
      }
    }
  }

  public static Map<String, Boolean> getCategoryEligibleForSizeChartAdditionMap(
      String sizeChartAttributeCode, List<Category> categories) {
    Map<String, Boolean> categoryAndEligibilityMap = new HashMap<>();
    for (Category category : categories) {
      List<String> attributeCodes;
      attributeCodes =
          category.getCategoryAttributes().stream().filter(Objects::nonNull).map(CategoryAttribute::getAttribute)
              .filter(Objects::nonNull).map(Attribute::getAttributeCode).collect(toList());
      categoryAndEligibilityMap.put(category.getCategoryCode(),
          attributeCodes.contains(sizeChartAttributeCode));
    }
    return categoryAndEligibilityMap;
  }

  public static boolean commonImageUpdateWithProductImage(List<ProductImage> productImages, boolean commonImageUpdated) {
    return commonImageUpdated || CollectionUtils.isNotEmpty(Optional.ofNullable(productImages).orElse(new ArrayList<>())
        .stream().filter(ProductImage::isCommonImage).collect(toList()));
  }

  public static boolean itemLevelImagesUpdated(List<ProductItemImage> productItemImages, boolean itemImagesUpdated) {
    return itemImagesUpdated || CollectionUtils.isNotEmpty(Optional.ofNullable(productItemImages).orElse(new ArrayList<>())
        .stream().filter(Predicate.not(ProductItemImage::isCommonImage)).collect(Collectors.toList()));
  }

  public static boolean contentUpdateAtProductLevel(Product newProduct, Product savedProduct) {
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = new SimpleMasterProductUpdateDTO();
    ProductUtil.getSimpleMasterProductUpdateDTOFromProduct(simpleMasterProductUpdateDTO, newProduct);
    return ProductUtil.isProductDetailChanged(newProduct, savedProduct) ||
        ProductUtil.isDimensionUpdated(savedProduct, simpleMasterProductUpdateDTO) ||
        nonVariantCreatingAttributeUpdated(newProduct, savedProduct);
  }

  public static boolean nonVariantCreatingAttributeUpdated(Product newProduct, Product savedProduct) {
    Map<String, ProductAttribute> oldProductAttributeMap = savedProduct.getProductAttributes()
        .stream().filter(productAttribute -> nonVariantCreatingProductAtrribute(productAttribute))
            .collect(Collectors.toMap(productAttribute -> productAttribute.getAttribute().getAttributeCode(),
                productAttribute -> productAttribute, (productAttribute1, productAttribute2) -> productAttribute1));
    Map<String, ProductAttribute> newProductAttributeMap = newProduct.getProductAttributes()
        .stream().filter(productAttribute -> nonVariantCreatingProductAtrribute(productAttribute))
        .collect(Collectors.toMap(productAttribute -> productAttribute.getAttribute().getAttributeCode(),
            productAttribute -> productAttribute, (productAttribute1, productAttribute2) -> productAttribute1));
    return nonVariantCreatingAttributeUpdated(oldProductAttributeMap, newProductAttributeMap) ||
        nonVariantCreatingAttributeUpdated(newProductAttributeMap, oldProductAttributeMap);
  }

  public static boolean nonVariantCreatingProductAtrribute(ProductAttribute productAttribute) {
    return !productAttribute.isMarkForDelete() && (Objects.nonNull(productAttribute.getAttribute()) &&
        !productAttribute.getAttribute().isVariantCreation());
  }

  public static boolean nonVariantCreatingAttributeUpdated(Map<String, ProductAttribute> productAttributeMap,
      Map<String, ProductAttribute> productAttributeMapNew) {
    boolean attributeUpdated = false;
    for (String attributeCode : productAttributeMap.keySet()) {
      if (!productAttributeMapNew.containsKey(attributeCode)) {
        attributeUpdated = true;
      } else {
        attributeUpdated = attributeUpdated || attributeValueUpdatedForNonVariantCreatingProductAtrribute(
            productAttributeValueForNonVariantCreatingProductAttribute(productAttributeMap.get(attributeCode)),
            productAttributeValueForNonVariantCreatingProductAttribute(productAttributeMapNew.get(attributeCode)));
      }
    }
    return attributeUpdated;
  }

  public static boolean attributeValueUpdatedForNonVariantCreatingProductAtrribute(ProductAttributeValue
      oldProductAttributeValue, ProductAttributeValue newProductAttributeValue) {
    return  !StringUtils.equals(oldProductAttributeValue.getDescriptiveAttributeValue(),
        newProductAttributeValue.getDescriptiveAttributeValue()) ||
        isPredefinedAllowedAttributeValueUpdated(oldProductAttributeValue.getPredefinedAllowedAttributeValue(),
            newProductAttributeValue.getPredefinedAllowedAttributeValue());
  }

  public static boolean isPredefinedAllowedAttributeValueUpdated(PredefinedAllowedAttributeValue oldAttributeValue,
      PredefinedAllowedAttributeValue newAttributeValue) {
    if (Objects.isNull(oldAttributeValue) && Objects.isNull(newAttributeValue)) {
      return false;
    } else if (Objects.isNull(oldAttributeValue) || Objects.isNull(newAttributeValue)) {
      return true;
    }
    return !StringUtils.equals(oldAttributeValue.getValue(), newAttributeValue.getValue());
  }

  public static ProductAttributeValue productAttributeValueForNonVariantCreatingProductAttribute(ProductAttribute
      productAttribute) {
    return Optional.ofNullable(productAttribute.getProductAttributeValues()).orElse(new ArrayList<>()).stream()
        .filter(Predicate.not(ProductAttributeValue::isMarkForDelete)).findFirst().orElse(new ProductAttributeValue());
  }

  public static Pair<Boolean, Set<String>> skuCodesOfUpdatedItems(List<ProductItem> productItems, List<ProductItem> newProductItems, boolean checkImageDiff) {
    boolean commonImageUpdated = false;
    Map<String, ProductItem> newItemSkuCodes = Optional.ofNullable(newProductItems).orElse(new ArrayList<>()).stream()
        .filter(Predicate.not(ProductItem::isMarkForDelete)).collect(Collectors.toMap(ProductItem::getSkuCode, Function.identity()));
    Map<String, ProductItem> oldItemSkuCodes = Optional.ofNullable(productItems).orElse(new ArrayList<>()).stream()
        .filter(Predicate.not(ProductItem::isMarkForDelete)).collect(Collectors.toMap(ProductItem::getSkuCode, Function.identity()));
    Set<String> skuCodes = skuCodesOfUPCCodeUpdatedItems(newItemSkuCodes, oldItemSkuCodes);
    skuCodes.addAll(skuCodesOfAddedVariants(newItemSkuCodes.keySet(), oldItemSkuCodes.keySet()));
    skuCodes.addAll(skuCodesOfDeletedVariants(newItemSkuCodes.keySet(), oldItemSkuCodes.keySet()));
    if (checkImageDiff) {
      Pair<Boolean, Set<String>> imageUpdated = getSkusForWhichImageUpdated(productItems, newProductItems);
      skuCodes.addAll(imageUpdated.getRight());
      commonImageUpdated = imageUpdated.getLeft();
    }
    return Pair.of(commonImageUpdated, skuCodes);
  }

  public static Pair<Boolean, Set<String>> getSkusForWhichImageUpdated(List<ProductItem> productItems,
      List<ProductItem> newProductItems) {
    Set<String> skuCodes = new HashSet<>();
    boolean commonImageUpdated = false;

    Map<String, ProductItemImage> oldProductItemImageMap = new HashMap<>();
    for (ProductItem oldProductItem : Optional.ofNullable(productItems).orElse(new ArrayList<>())) {
      oldProductItemImageMap.putAll(
          oldProductItem.getProductItemImages().stream().filter(Predicate.not(ProductItemImage::isMarkForDelete))
              .collect(Collectors.toMap(
                  oldProductItemImage -> getKeyFromItemCodeAndLocationPath(oldProductItem.getSkuCode(),
                      oldProductItemImage.getLocationPath()), Function.identity(), (v1, v2) -> v2)));
    }

    for (ProductItem newProductItem : newProductItems) {
      for (ProductItemImage newProductItemImage : newProductItem.getProductItemImages()) {
        ProductItemImage oldProductItemImage = oldProductItemImageMap.get(
            getKeyFromItemCodeAndLocationPath(newProductItem.getSkuCode(), newProductItemImage.getLocationPath()));
        if (Objects.nonNull(oldProductItemImage)) {
          if (newProductItemImage.isMarkForDelete() != oldProductItemImage.isMarkForDelete()
              || newProductItemImage.isMainImages() != oldProductItemImage.isMainImages()) {
            commonImageUpdated =
                commonImageUpdated || isCommonImageUpdated(newProductItem, newProductItemImage, skuCodes);
          }
        } else {
          commonImageUpdated =
              commonImageUpdated || isCommonImageUpdated(newProductItem, newProductItemImage, skuCodes);
        }
      }
    }

    return Pair.of(commonImageUpdated, skuCodes);
  }

  private static String getKeyFromItemCodeAndLocationPath(String itemCode, String locationPath) {
    return new StringBuilder(itemCode).append(Constants.HYPHEN).append(locationPath).toString();
  }

  private static boolean isCommonImageUpdated(ProductItem newProductItem, ProductItemImage newProductItemImage,
      Set<String> skuCodes) {
    boolean commonImageUpdated = false;
    if (newProductItemImage.isCommonImage()) {
      commonImageUpdated = true;
    } else {
      skuCodes.add(newProductItem.getSkuCode());
    }
    return commonImageUpdated;
  }

  private static Set<String> skuCodesOfUPCCodeUpdatedItems(Map<String, ProductItem> newItemSkuCodes,
      Map<String, ProductItem> oldItemSkuCodes) {
    return oldItemSkuCodes.keySet().stream().filter(skuCode -> newItemSkuCodes.containsKey(skuCode) &&
        !StringUtils.equals(newItemSkuCodes.get(skuCode).getUpcCode(),oldItemSkuCodes.get(skuCode).getUpcCode()))
        .collect(toSet());
  }

  private static Set<String> skuCodesOfAddedVariants(Set<String> newItemSkuCodes, Set<String> oldItemSkuCodes) {
    return oldItemSkuCodes.stream().filter(skuCode -> !newItemSkuCodes.contains(skuCode)).collect(toSet());
  }

  private static Set<String> skuCodesOfDeletedVariants(Set<String> newItemSkuCodes, Set<String> oldItemSkuCodes) {
     return newItemSkuCodes.stream().filter(skuCode -> !oldItemSkuCodes.contains(skuCode)).collect(toSet());
  }

  public static ProductAndItemLevelUpdatesDTO productAndItemLevelUpdates(Product newProduct, Product savedProduct) {
    Pair<Boolean, Set<String>> itemContentUpdated = skuCodesOfUpdatedItems(savedProduct.getProductItems(), newProduct.getProductItems(), true);
    ProductAndItemLevelUpdatesDTO productAndItemLevelUpdatesDTO = new ProductAndItemLevelUpdatesDTO();
    productAndItemLevelUpdatesDTO.setProductLevelDataUpdated(contentUpdateAtProductLevel(newProduct, savedProduct));
    productAndItemLevelUpdatesDTO.setUpdatedItemSkuCodes(itemContentUpdated.getRight());
    if (itemContentUpdated.getLeft()) {
      productAndItemLevelUpdatesDTO.setProductLevelDataUpdated(true);
    }
    return productAndItemLevelUpdatesDTO;
  }

  public static void validateAndSetVideoToRequest(Product savedProduct, Product product,
      ProductRequest request) {
    if (!Boolean.TRUE.equals(request.getVideoUpdated())) {
      product.setVideo(savedProduct.getVideo());
    }
  }

  public static ProductPublishUpdateDTO updateProductPublishUpdateDTO(
      ProductAndItemImageRequest productAndItemImageRequest) {
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());
    if (productAndItemImageRequest.getProductImages().stream().filter(image -> image.isCommonImage()).findFirst()
        .isPresent()) {
      productPublishUpdateDTO.setProductLevelDataUpdated(true);
    } else {
      List<ProductItemImageRequest> productItemImageRequests = productAndItemImageRequest.getProductItemImages();
      for (ProductItemImageRequest productItemImageRequest : productItemImageRequests) {
        if (productItemImageRequest.getItemImages().stream().filter(image -> !image.isCommonImage()).findFirst()
            .isPresent()) {
          productPublishUpdateDTO.getUpdatedItemSkuCodes().add(productItemImageRequest.getSkuCode());
        }
      }
    }
    return productPublishUpdateDTO;
  }


  public static void validateProductDimension(WarehouseMasterSKUEvent warehouseMasterSKUEvent,
      boolean validateDimensionSwitch) {
    if (validateDimensionSwitch) {
      warehouseMasterSKUEvent.setLength(validateFieldForPositiveValues(warehouseMasterSKUEvent.getLength()));
      warehouseMasterSKUEvent.setWidth(validateFieldForPositiveValues(warehouseMasterSKUEvent.getWidth()));
      warehouseMasterSKUEvent.setWeight(validateFieldForPositiveValues(warehouseMasterSKUEvent.getWeight()));
      warehouseMasterSKUEvent.setHeight(validateFieldForPositiveValues(warehouseMasterSKUEvent.getHeight()));
    }
  }

  private static double validateFieldForPositiveValues(double value) {
    double roundedValue = Math.round(value * 1000.0) / 1000.0;
    if (roundedValue <= 0) {
      return 0.0;
    } else {
      return roundedValue;
    }
  }

  public static void updateDimensions(Product product, WarehouseMasterSKUEvent request) {
    product.setLength(request.getLength());
    product.setWidth(request.getWidth());
    product.setHeight(request.getHeight());
    product.setWeight(request.getWeight());
  }

  // Update the main image to be unique in case of multiple main images by setting all others to non-main.
  public static void updateMainImageToUniqueInCaseOfMultipleMainImages(ProductItem productItem) {
    ProductItemImage activeMainItemImage = productItem.getProductItemImages().stream()
        .filter(ProductItemImage::isActive).filter(ProductItemImage::isMainImages).findFirst().orElse(null);

    if (Objects.nonNull(activeMainItemImage)) {
      productItem.getProductItemImages().stream()
          .filter(ProductItemImage::isActive).filter(ProductItemImage::isMainImages)
          .filter(item -> item != activeMainItemImage).forEach(item -> item.setMainImages(false));
    }
  }

  public  static  Set<String> getUpdatedFields(Product savedProduct, Product product) {
    UpdatedFieldsDTO updatedFieldsDTO = new UpdatedFieldsDTO();
    updatedFieldsDTO.setProductNameUpdated(!org.apache.commons.lang3.StringUtils.equals(savedProduct.getName(), product.getName()));
    Optional<ProductCategory> savedProductCategory = savedProduct.getProductCategories().stream()
      .filter(Predicate.not(ProductCategory::isMarkForDelete)).findFirst();
    Optional<ProductCategory> productCategory = product.getProductCategories().stream()
      .filter(Predicate.not(ProductCategory::isMarkForDelete)).findFirst();
    if (savedProductCategory.isPresent() && productCategory.isPresent()) {
      updatedFieldsDTO.setCategoryUpdated(!org.apache.commons.lang3.StringUtils.equals(
        productCategory.get().getCategory().getCategoryCode(),
        savedProductCategory.get().getCategory().getCategoryCode()));
    }
    updatedFieldsDTO.setBrandUpdated(!org.apache.commons.lang3.StringUtils.equals(savedProduct.getBrand(), product.getBrand()));
    updatedFieldsDTO.setDescriptionUpdated(
        !org.apache.commons.lang3.StringUtils.equals(Arrays.toString(savedProduct.getDescription()), Arrays.toString(product.getDescription())));
    return CommonUtil.getUpdatedFieldsSet(new HashSet<>(), updatedFieldsDTO);
  }

  private static Set<String> getUpdatedFieldsSet(Set<String> updatedFields, UpdatedFieldsDTO updatedFieldsDTO) {
    if (updatedFieldsDTO.isDescriptionUpdated()) {
      updatedFields.add(UpdatedFields.DESCRIPTION_UPDATE.name());
    }
    if (updatedFieldsDTO.isProductNameUpdated()) {
      updatedFields.add(UpdatedFields.NAME_UPDATE.name());
    }
    if (updatedFieldsDTO.isBrandUpdated()) {
      updatedFields.add(UpdatedFields.BRAND_UPDATE.name());
    }
    if (updatedFieldsDTO.isCategoryUpdated()) {
      updatedFields.add(UpdatedFields.CATEGORY_UPDATE.name());
    }
    return updatedFields;
  }

  public static String generateSizeChartCode(String sequence) {
    return new StringBuilder(Constants.PREFIX_SIZE_CHART_CODE).append(Constants.HYPHEN).append(
        StringUtils.leftPad(sequence, Constants.SIZE_CHART_CODE_SIZE, Constants.PADDING_STRING))
      .toString();
  }


  public static BrandAuthUpdateRequest formUpdateRequestFromCreation(BrandAuthCreateRequest brandAuthCreateRequest){
    BrandAuthUpdateRequest updateRequest = new BrandAuthUpdateRequest();
    BeanUtils.copyProperties(brandAuthCreateRequest, updateRequest);
    return updateRequest;
  }

  public static boolean isAttributeEligibleForConcatenation(boolean valueTypeAdditionForDefiningAttributes,
      boolean concatenateValueWithValueType, AttributeValueDTO attributeValueDTO) {
    return valueTypeAdditionForDefiningAttributes && concatenateValueWithValueType && StringUtils.isNotBlank(
        attributeValueDTO.getValue()) && StringUtils.isNotBlank(attributeValueDTO.getValueType());
  }

  public static Set<String> getUpdatedFieldForDescription(Product savedProduct, Product product) {
    UpdatedFieldsDTO updatedFieldsDTO = new UpdatedFieldsDTO();
    updatedFieldsDTO.setDescriptionUpdated(
        !org.apache.commons.lang3.StringUtils.equals(Arrays.toString(savedProduct.getDescription()), Arrays.toString(
            product.getDescription())));
    return CommonUtil.getUpdatedFieldsSet(new HashSet<>(), updatedFieldsDTO);
  }

  public static RestrictedKeywordHistoryEventModel populateHistoryModel(
    RestrictedKeyword restrictedKeyword, Boolean newValidateByDs) {
    RestrictedKeywordHistoryEventModel restrictedKeywordHistoryEventModel =
      new RestrictedKeywordHistoryEventModel();

    RestrictedKeywordActivityHistory keywordActivityHistory =
      new RestrictedKeywordActivityHistory();
    keywordActivityHistory.setActivity(
      RestrictedKeywordActivity.DS_VALIDATION_UPDATED.getDescription());
    keywordActivityHistory.setOldValue(Objects.nonNull(restrictedKeyword.getValidateByDs()) ?
      restrictedKeyword.getValidateByDs().toString() :
      Boolean.FALSE.toString());
    keywordActivityHistory.setNewValue(
      Objects.nonNull(newValidateByDs) ? newValidateByDs.toString() : Boolean.FALSE.toString());

    restrictedKeywordHistoryEventModel.setRestrictedKeywordActivityHistoryList(
      Collections.singletonList(keywordActivityHistory));
    restrictedKeywordHistoryEventModel.setKeywordId(restrictedKeyword.getId());
    restrictedKeywordHistoryEventModel.setStoreId(restrictedKeyword.getStoreId());
    restrictedKeywordHistoryEventModel.setUserName(GdnMandatoryParameterUtil.getUsername());
    return restrictedKeywordHistoryEventModel;
  }

  public static BasicSizeChartDetailMapResponse generateBasicSizeChartDetailResponse(List<SizeChart> sizeCharts) {
    BasicSizeChartDetailMapResponse basicSizeChartDetailMapResponse =
        new BasicSizeChartDetailMapResponse();
    Map<String, BasicSizeChartDetailResponse> basicSizeChartDetailResponseMap = sizeCharts.stream()
        .collect(
            Collectors.toMap(SizeChart::getSizeChartCode, CommonUtil::setBasicSizeChartDetails));
    basicSizeChartDetailMapResponse.setBasicSizeChartDetailResponseMap(
        basicSizeChartDetailResponseMap);
    return basicSizeChartDetailMapResponse;
  }

  private static BasicSizeChartDetailResponse setBasicSizeChartDetails(SizeChart sizeChart) {
    BasicSizeChartDetailResponse basicSizeChartDetailResponse = new BasicSizeChartDetailResponse();
    basicSizeChartDetailResponse.setSizeChartName(sizeChart.getName());
    basicSizeChartDetailResponse.setBusinessPartnerCode(sizeChart.getBusinessPartnerCode());
    return basicSizeChartDetailResponse;
  }

  public static SizeChartUpdateEventModel getSizeChartUpdateEventModel(SizeChart sizeChart) {
    SizeChartUpdateEventModel sizeChartUpdateEventModel = new SizeChartUpdateEventModel();
    sizeChartUpdateEventModel.setSizeChartCode(sizeChart.getSizeChartCode());
    sizeChartUpdateEventModel.setSizeChartName(sizeChart.getName());
    sizeChartUpdateEventModel.setStoreId(sizeChart.getStoreId());
    return sizeChartUpdateEventModel;
  }

  public static String getAttributeValueBasedOnSizeChart(boolean concatenateValueWithValueType,
      boolean valueTypeAdditionForDefiningAttributes, String value, String valueType, String attributeType,
      String sizeChartValueTypeDelimiter) {
    if (concatenateValueWithValueType && valueTypeAdditionForDefiningAttributes
        && AttributeType.DEFINING_ATTRIBUTE.name().equals(attributeType) && StringUtils.isNotBlank(valueType)) {
      value = valueType.concat(sizeChartValueTypeDelimiter).concat(value);
    }
    return value;
  }

  public static BrandAuthorisationHistory generateCreateWipHistory(
      BrandAuthorisationWip createResponse, String username, String storeId,
    DateFormat historyDateFormat) {
    String activity = BrandAuthorisationActivity.REQUESTED.getDescription().concat(
      ConverterUtil.generateDateForHistory(createResponse.getAuthStartDate(),
        createResponse.getAuthExpireDate(), historyDateFormat));
    BrandAuthorisationHistory brandAuthorisationHistory = new BrandAuthorisationHistory();
    brandAuthorisationHistory.setActivity(activity);
    brandAuthorisationHistory.setNewStatus(StringUtils.EMPTY);
    brandAuthorisationHistory.setOldStatus(StringUtils.EMPTY);
    brandAuthorisationHistory.setBrandCode(createResponse.getBrandCode());
    brandAuthorisationHistory.setSellerCode(createResponse.getSellerCode());
    brandAuthorisationHistory.setStoreId(storeId);
    brandAuthorisationHistory.setCreatedBy(username);
    brandAuthorisationHistory.setUpdatedBy(username);
    return brandAuthorisationHistory;
  }

  public static BrandAuthorisationWip generateCreateBrandAuthWipRequest(
      BrandAuthCreateWipRequest request, String storeId) {
    BrandAuthorisationWip brandAuthorisation = new BrandAuthorisationWip();
    BeanUtils.copyProperties(request, brandAuthorisation, "authorisationStatus", "documentLinks");
    brandAuthorisation.setAuthorisationStatus(
        BrandAuthorizationWipStatus.valueOf(Constants.IN_REVIEW));
    brandAuthorisation.setStoreId(storeId);
    if (CollectionUtils.isNotEmpty(request.getDocumentLinks())) {
      brandAuthorisation.setDocumentLink(request.getDocumentLinks().stream().map(String::valueOf)
          .collect(Collectors.joining(Constants.COMMA)));
    }
    return brandAuthorisation;
  }

  public static boolean validateNewFlowCreation(Date authStartDate,
    int brandAuthWipThresholdInDays) {
    return authStartDate.after(
      DateUtils.addDays(new Date(System.currentTimeMillis()), brandAuthWipThresholdInDays));
  }

  public static void updateProductMigrationEntity(String requestId,
    ProductMigrationRequest productMigrationRequest, ProductMigration savedProductMigration) {
    savedProductMigration.setStatus(productMigrationRequest.getUpdatedStatus());
    savedProductMigration.setErrorMessage(productMigrationRequest.getErrorMessage());
    savedProductMigration.setUpdatedBy(requestId);
    savedProductMigration.setUpdatedDate(new Date());
  }

  public static ProductMigrationRequest getProductMigrationRequest(
      CommonImageBackfillingEventModel productAttributeDataBackFillingEventModel, String errorMessage) {
    return ProductMigrationRequest.builder()
        .productCode(productAttributeDataBackFillingEventModel.getProductCode())
        .migrationType(productAttributeDataBackFillingEventModel.getMigrationType())
        .updatedStatus(ProductMigrationStatus.FAILED.name()).migrationPayload(
            String.valueOf(productAttributeDataBackFillingEventModel.getMigrationPayloadList()))
        .errorMessage(errorMessage).build();
  }

  public static CommonImageBackfillingEventModel toCommonImageBackfillingEventModel(String storeId,
    ProductMigration productMigration) {
    CommonImageBackfillingEventModel model = new CommonImageBackfillingEventModel();
    ObjectMapper objectMapper = new ObjectMapper();
    model.setStoreId(storeId);
    model.setMigrationType(productMigration.getMigrationType());
    model.setProductCode(productMigration.getProductCode());
    model.setTimestamp(System.currentTimeMillis());
    Optional.ofNullable(productMigration.getMigrationPayload()).ifPresent(payload -> {
      try {
        List<MigrationPayload> migrationPayloadList =
          objectMapper.readValue(payload, new TypeReference<List<MigrationPayload>>() {
          });
        model.setMigrationPayloadList(migrationPayloadList);
      } catch (JsonProcessingException e) {
        log.error("Error while parsing migration payload for productCode : {} ",
          productMigration.getProductCode());
        model.setMigrationPayloadList(Collections.emptyList());
      }
    });
    return model;
  }

  public static Integer[] convertToIntArray(String configDays) {
    return Objects.isNull(configDays) || StringUtils.isEmpty(configDays) ?
        new Integer[0] :
        Arrays.stream(configDays.split(Constants.COMMA)).map(String::trim)
            .filter(configDay -> !configDay.isEmpty()).mapToInt(Integer::parseInt).boxed()
            .toArray(Integer[]::new);
  }

  public static List<ProductAttribute> setMissingProductAttributesAndValues(
    List<Attribute> attributes, Product product,
    Map<String, List<PredefinedAllowedAttributeValue>> preDefinedAllowedAttributeIdAndValueMap) {
    List<ProductAttribute> productAttributesToBeBackFilled = new ArrayList<>();
    for (Attribute attribute : attributes) {
      ProductAttribute productAttribute = getProductAttribute(product, attribute, Constants.ATTRIBUTE_AUTO_HEAL);
      if (setMissingProductAttributeValues(attribute, productAttribute,
        preDefinedAllowedAttributeIdAndValueMap)) {
        productAttributesToBeBackFilled.add(productAttribute);
      }
    }
    return productAttributesToBeBackFilled;
  }

  public static ProductAttribute getProductAttribute(Product product, Attribute attribute, String createdBy) {
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setProduct(product);
    productAttribute.setAttribute(attribute);
    productAttribute.setProductId(product.getId());
    productAttribute.setCreatedBy(createdBy);
    productAttribute.setMarkForDelete(false);
    productAttribute.setProductAttributeName(attribute.getName());
    productAttribute.setAttributeId(attribute.getId());
    productAttribute.setStoreId(attribute.getStoreId());
    return productAttribute;
  }

  public static boolean setMissingProductAttributeValues(Attribute attribute,
    ProductAttribute productAttribute, Map<String, List<PredefinedAllowedAttributeValue>> preDefinedAllowedAttributeIdAndValueMap) {

    return switch (attribute.getAttributeType()) {
      case DESCRIPTIVE_ATTRIBUTE -> createDescriptiveAttributeValue(productAttribute, Constants.HYPHEN);
      case PREDEFINED_ATTRIBUTE -> createPredefinedAttributeValue(attribute, productAttribute,
        preDefinedAllowedAttributeIdAndValueMap);
      default -> false;
    };
  }

  public static boolean createDescriptiveAttributeValue(ProductAttribute productAttribute,
      String descriptiveAttributeValue) {
    ProductAttributeValue productAttributeValue = createBaseProductAttributeValue(productAttribute);
    productAttributeValue.setDescriptiveAttributeValue(descriptiveAttributeValue);
    productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttribute.setProductAttributeValues(Collections.singletonList(productAttributeValue));
    return true;
  }

  private static boolean createPredefinedAttributeValue(Attribute attribute,
    ProductAttribute productAttribute,
    Map<String, List<PredefinedAllowedAttributeValue>> preDefinedAllowedAttributeIdAndValueMap) {

    if (attribute.isMandatory()) {
      return false;
    }

    List<PredefinedAllowedAttributeValue> allowedValues =
      preDefinedAllowedAttributeIdAndValueMap.getOrDefault(attribute.getId(),
        Collections.emptyList());

    if (allowedValues.isEmpty()) {
      return false;
    }

    PredefinedAllowedAttributeValue value = allowedValues.getFirst();
    if (Objects.isNull(value)) {
      return false;
    }

    ProductAttributeValue productAttributeValue = createBaseProductAttributeValue(productAttribute);
    productAttributeValue.setDescriptiveAttributeValueType(
      DescriptiveAttributeValueType.PREDEFINED);
    productAttributeValue.setPredefinedAllowedAttributeValue(value);
    productAttributeValue.setPredefinedAllowedAttributeValueId(value.getId());
    productAttribute.setProductAttributeValues(Collections.singletonList(productAttributeValue));
    return true;
  }

  public static ProductAttributeValue createBaseProductAttributeValue(
    ProductAttribute productAttribute) {
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setStoreId(productAttribute.getStoreId());
    productAttributeValue.setMarkForDelete(false);
    productAttributeValue.setProductAttribute(productAttribute);
    return productAttributeValue;
  }

  public static void processDescriptiveMultiValueAttributes(
      List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList, ProductAttribute productAttribute) {
    List<String> attributeValues =
        productSuitabilityAttributeModelList.stream().map(ProductSuitabilityAttributeModel::getValue)
            .collect(Collectors.toCollection(ArrayList::new));
    for (String attributeValue : attributeValues) {
      ProductAttributeValue productAttributeValue = CommonUtil.createBaseProductAttributeValue(productAttribute);
      productAttributeValue.setDescriptiveAttributeValue(attributeValue);
      productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.MULTIPLE);
      productAttribute.getProductAttributeValues().add(productAttributeValue);
    }
  }

  public static void removeProductAttributeValuesAndProductAttribute(ProductAttribute productAttribute) {
    for (ProductAttributeValue productAttributeValue : productAttribute.getProductAttributeValues()) {
      productAttributeValue.setMarkForDelete(true);
      productAttributeValue.setUpdatedBy(Constants.DS_EXTRACTED_ATTRIBUTE);
    }
    productAttribute.setMarkForDelete(true);
    productAttribute.setUpdatedBy(Constants.DS_EXTRACTED_ATTRIBUTE);
  }

  public static void removeExistingPredefinedValues(
      List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList, ProductAttribute productAttribute,
      Map<String, String> existingPredefinedIdAndValueMap, Set<String> predefinedAttributeValuesWithNullMapping,
      boolean dsExtractionListenerRemoveEmptyPredefinedValue) {

    Set<String> newPredefinedAttributeValues =
        productSuitabilityAttributeModelList.stream().map(ProductSuitabilityAttributeModel::getValue)
            .collect(Collectors.toSet());

    for (Map.Entry<String, String> entry : existingPredefinedIdAndValueMap.entrySet()) {
      String predefinedId = entry.getKey();
      String predefinedValue = entry.getValue();

      productAttribute.getProductAttributeValues().stream()
          .filter(value -> StringUtils.equals(predefinedId, value.getPredefinedAllowedAttributeValueId())).findFirst()
          .ifPresent(value -> {
            boolean markForDelete = !newPredefinedAttributeValues.contains(predefinedValue);
            value.setMarkForDelete(markForDelete);
          });
    }
    if (dsExtractionListenerRemoveEmptyPredefinedValue) {
      productAttribute.getProductAttributeValues().stream().filter(
              productAttributeValue -> predefinedAttributeValuesWithNullMapping.contains(productAttributeValue.getId()))
          .forEach(productAttributeValue -> productAttributeValue.setMarkForDelete(true));
    }
  }

  public static Map<String, String> getExistingPredefinedIdAndValueMap(ProductAttribute productAttribute,
      Set<String> predefinedAttributeValuesWithNullMapping) {
    Map<String, String> existingPredefinedIdAndValueMap = new HashMap<>();
    for (ProductAttributeValue productAttributeValue : productAttribute.getProductAttributeValues()) {
      PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
          productAttributeValue.getPredefinedAllowedAttributeValue();
      if (Objects.isNull(predefinedAllowedAttributeValue)) {
        predefinedAttributeValuesWithNullMapping.add(productAttributeValue.getId());
        continue;
      }
      existingPredefinedIdAndValueMap.put(predefinedAllowedAttributeValue.getId(),
          predefinedAllowedAttributeValue.getValue());
    }
    return existingPredefinedIdAndValueMap;
  }

  public static void removeProductAttributeIfAllValuesDeleted(ProductAttribute productAttribute) {
    productAttribute.setMarkForDelete(
        productAttribute.getProductAttributeValues().stream().allMatch(ProductAttributeValue::isMarkForDelete));
  }

  public static void processExistingDescriptiveMultiValueAttributes(
      List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList, ProductAttribute productAttribute) {
    if (Objects.isNull(productSuitabilityAttributeModelList.get(0).getValue())) {
      // if the attribute value from event is sent null , then unmap all the existing ds attribute mapped to product
      CommonUtil.removeProductAttributeValuesAndProductAttribute(productAttribute);
    } else {
      // Filter out the existing values which are sent in event
      List<String> attributeValuesFromRequest =
          productSuitabilityAttributeModelList.stream().map(ProductSuitabilityAttributeModel::getValue)
              .collect(Collectors.toCollection(ArrayList::new));
      List<String> existingAttributeValues = new ArrayList<>();
      for (ProductAttributeValue productAttributeValue : productAttribute.getProductAttributeValues()) {
        String existingValue = productAttributeValue.getDescriptiveAttributeValue();
        existingAttributeValues.add(existingValue);
        productAttributeValue.setMarkForDelete(!attributeValuesFromRequest.contains(existingValue));
      }
      List<String> newAttributeValues = new ArrayList<>(attributeValuesFromRequest);
      newAttributeValues.removeAll(existingAttributeValues);
      // For the new attribute values , map the values to product
      if (CollectionUtils.isNotEmpty(newAttributeValues)) {
        for (String attributeValue : newAttributeValues) {
          ProductAttributeValue productAttributeValue = CommonUtil.createBaseProductAttributeValue(productAttribute);
          productAttributeValue.setDescriptiveAttributeValue(attributeValue);
          productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.MULTIPLE);
          productAttribute.getProductAttributeValues().add(productAttributeValue);
        }
      }
      CommonUtil.removeProductAttributeIfAllValuesDeleted(productAttribute);
    }
  }

  public static void setBasicProductAttributeValueDetails(ProductAttributeValue productAttributeValue,
      PredefinedAllowedAttributeValue predefinedAllowedAttributeValue) {
    productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
    productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    productAttributeValue.setPredefinedAllowedAttributeValueId(predefinedAllowedAttributeValue.getId());
  }

  public static void processExistingDescriptiveAttributeValue(
      List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList, ProductAttribute productAttribute) {
    String attributeValueFromRequest =
        productSuitabilityAttributeModelList.stream().findFirst().map(ProductSuitabilityAttributeModel::getValue)
            .orElse(null);
    Map<String, ProductAttributeValue> descriptiveValueAndProductAttributeValueMap =
        productAttribute.getProductAttributeValues().stream().filter(
                productAttributeValue -> StringUtils.isNotBlank(productAttributeValue.getDescriptiveAttributeValue()))
            .collect(Collectors.toMap(ProductAttributeValue::getDescriptiveAttributeValue, Function.identity(),
                (existing, duplicate) -> existing));
    Optional<ProductAttributeValue> optionalProductAttributeValue =
        productAttribute.getProductAttributeValues().stream()
            .filter(Predicate.not(ProductAttributeValue::isMarkForDelete)).findFirst();
    // Case 1: Existing attribute value to be removed
    if (Objects.isNull(attributeValueFromRequest)) {
      optionalProductAttributeValue.ifPresent(value -> value.setMarkForDelete(true));
      productAttribute.setMarkForDelete(true);
      productAttribute.setUpdatedBy(Constants.DS_EXTRACTED_ATTRIBUTE);
      return;
    }
    // Case 2: Value exists and value needs to be updated → mark mfd true for old, create new entry
    if (optionalProductAttributeValue.isEmpty()) {
      createDescriptiveAttributeValue(productAttribute, attributeValueFromRequest);
      return;
    }
    if (optionalProductAttributeValue.filter(
        existing -> !attributeValueFromRequest.equals(existing.getDescriptiveAttributeValue())).isPresent()) {
      optionalProductAttributeValue.get().setMarkForDelete(true);
      if (descriptiveValueAndProductAttributeValueMap.containsKey(attributeValueFromRequest)) {
        descriptiveValueAndProductAttributeValueMap.get(attributeValueFromRequest).setMarkForDelete(false);
      } else {
        addDescriptiveAttributeValues(productAttribute, attributeValueFromRequest);
      }
    }
  }

  public static Map<String, ProductAttributeValue> getPredefinedValueToProductAttributeValueMap(
      ProductAttribute productAttribute) {
    return productAttribute.getProductAttributeValues().stream()
        .filter(productAttributeValue -> Objects.nonNull(productAttributeValue.getPredefinedAllowedAttributeValue()))
        .filter(productAttributeValue -> StringUtils.isNotBlank(
            productAttributeValue.getPredefinedAllowedAttributeValue().getValue())).collect(
            Collectors.toMap(pav -> pav.getPredefinedAllowedAttributeValue().getValue(), Function.identity(),
                (existing, duplicate) -> existing));
  }

  public static InternalProductHistoryEventModel getInternalProductHistoryEventModel(Product product, String username,
      String activity) {
    InternalProductHistoryEventModel internalProductHistoryEventModel = new InternalProductHistoryEventModel();
    internalProductHistoryEventModel.setStoreId(product.getStoreId());
    internalProductHistoryEventModel.setProductCode(product.getProductCode());
    internalProductHistoryEventModel.setUsername(username);
    internalProductHistoryEventModel.setActivity(activity);
    return internalProductHistoryEventModel;
  }

  public static List<String> getExistingAttributeValues(Attribute attribute, List<String> oldValues,
      ProductAttribute productAttribute) {
    com.gdn.x.productcategorybase.AttributeType type = attribute.getAttributeType();
    Stream<ProductAttributeValue> attributeValueStream = productAttribute.getProductAttributeValues().stream()
        .filter(Predicate.not(ProductAttributeValue::isMarkForDelete));
    return switch (type) {
      case DESCRIPTIVE_ATTRIBUTE, DESCRIPTIVE_MULTIVALUE ->
          attributeValueStream.map(ProductAttributeValue::getDescriptiveAttributeValue).collect(Collectors.toList());
      case PREDEFINED_ATTRIBUTE, PREDEFINED_MULTIVALUE ->
          attributeValueStream.map(ProductAttributeValue::getPredefinedAllowedAttributeValue).filter(Objects::nonNull)
              .map(PredefinedAllowedAttributeValue::getValue).collect(Collectors.toList());
      default -> oldValues;
    };
  }


  public static String getInternalHistoryNotes(List<String> existingValues, List<String> newValues,
      String dsAttributeName) {
    String oldValStr = existingValues.stream().map(String::valueOf).collect(
        Collectors.joining(Constants.COMMA + Constants.SPACE, Constants.OPEN_SQUARE_BRACKET,
            Constants.CLOSE_SQUARE_BRACKET));
    String newValStr = newValues.stream().map(String::valueOf).collect(
        Collectors.joining(Constants.COMMA + Constants.SPACE, Constants.OPEN_SQUARE_BRACKET,
            Constants.CLOSE_SQUARE_BRACKET));
    return dsAttributeName + Constants.NEXT_LINE + OLD_ATTRIBUTE_VALUE + oldValStr + NEW_ATTRIBUTE_VALUE + newValStr;
  }

  public static List<String> getNewAttributeValues(
      List<ProductSuitabilityAttributeModel> productSuitabilityAttributeModelList,
      com.gdn.x.productcategorybase.AttributeType attributeType) {
    List<String> newValues =
        productSuitabilityAttributeModelList.stream().map(ProductSuitabilityAttributeModel::getValue)
            .filter(Objects::nonNull).collect(Collectors.toList());
    if (CollectionUtils.isEmpty(newValues)) {
      newValues = List.of(Constants.HYPHEN);
    } else if (com.gdn.x.productcategorybase.AttributeType.DESCRIPTIVE_ATTRIBUTE.equals(attributeType)
        || com.gdn.x.productcategorybase.AttributeType.PREDEFINED_ATTRIBUTE.equals(attributeType)) {
      // For descriptive attribute and predefined attribute we can only add one value
      newValues = List.of(newValues.get(0));
    }
    return newValues;
  }

  public static boolean addDescriptiveAttributeValues(ProductAttribute productAttribute,
      String descriptiveAttributeValue) {
    ProductAttributeValue productAttributeValue = createBaseProductAttributeValue(productAttribute);
    productAttributeValue.setDescriptiveAttributeValue(descriptiveAttributeValue);
    productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttribute.getProductAttributeValues().add(productAttributeValue);
    return true;
  }

  public static ProductMigrationRequest getProductMigrationRequest(
      CommonImageBackfillingEventModel productAttributeDataBackFillingEventModel,
      String errorMessage, ProductMigrationStatus productMigrationStatus) {
    return ProductMigrationRequest.builder()
        .productCode(productAttributeDataBackFillingEventModel.getProductCode())
        .migrationType(productAttributeDataBackFillingEventModel.getMigrationType())
        .updatedStatus(productMigrationStatus.name()).migrationPayload(
            String.valueOf(productAttributeDataBackFillingEventModel.getMigrationPayloadList()))
        .errorMessage(errorMessage).build();
  }

  public static AuditTrailDto getAuditTrailDto(String newValue, String oldValue,
      ProductMasterDataUpdateRequest productMasterDataUpdateRequest, String activity) {
    AuditTrailDto auditTrailDto = new AuditTrailDto();
    auditTrailDto.setNewValue(newValue);
    auditTrailDto.setOldValue(oldValue);
    auditTrailDto.setProductSku(productMasterDataUpdateRequest.getProductSku());
    auditTrailDto.setGdnSku(Constants.DEFAULT);
    auditTrailDto.setActionKey(activity);
    auditTrailDto.setPickupPointCode(Constants.HYPHEN);
    auditTrailDto.setName(productMasterDataUpdateRequest.getName());
    auditTrailDto.setBusinessPartnerCode(productMasterDataUpdateRequest.getBusinessPartnerCode());
    return auditTrailDto;
  }

  public static DistributionInfoResponse getDistributionInfoResponse(Product product) {
    ObjectMapper objectMapper = new ObjectMapper();
    DistributionInfoResponse distributionInfoResponse = null;
    if (Objects.nonNull(product) && StringUtils.isNotBlank(product.getDistributionInfo())) {
      String distributionInfoJson = product.getDistributionInfo();
      Map<String, String> distributionInfoMap = null;
      try {
        distributionInfoMap = objectMapper.readValue(distributionInfoJson,
            new TypeReference<Map<String, String>>() {});
      } catch (Exception e) {
        distributionInfoMap = new HashMap<>();
      }
      if (MapUtils.isNotEmpty(distributionInfoMap)) {
        distributionInfoResponse = new DistributionInfoResponse(
            distributionInfoMap.getOrDefault(PRODUCT_NAME, StringUtils.EMPTY),
            distributionInfoMap.getOrDefault(CATEGORY_NAME, StringUtils.EMPTY));
      }
    }
    return distributionInfoResponse;
  }

  public static AiGeneratedFieldsResponse getAIGeneratedFieldsResponse(String aiGeneratedFields) {
    ObjectMapper objectMapper = new ObjectMapper();
    AiGeneratedFieldsResponse aiGeneratedFieldsResponse = new AiGeneratedFieldsResponse();
    try {
      aiGeneratedFieldsResponse =
          objectMapper.readValue(aiGeneratedFields, AiGeneratedFieldsResponse.class);
    } catch (Exception e) {
      log.error("Error while generating AiGeneratedFieldsResponse ", e);
    }
    return aiGeneratedFieldsResponse;
  }

  public static DistributionInfoPerSkuResponse mapToDistributionInfoPerSkuResponse(
      ProductItemUomInfo entity, Product product, Map<String, ProductItem> skuCodeAndProductItemMap) {
    DistributionInfoResponse distributionInfoResponse = CommonUtil.getDistributionInfoResponse(product);
    DistributionItemInfoResponse itemInfoResponse =
        new DistributionItemInfoResponse(Optional.ofNullable(entity.getOrigin()).orElse(Origin.LOCAL).name(),
        skuCodeAndProductItemMap.getOrDefault(entity.getSkuCode(), new ProductItem()).getOmniChannelSku(),
        entity.isExpiry());
    List<DimensionsAndUomResponse> dimensionsList = buildDimensionsList(entity);
    Optional.ofNullable(dimensionsList).orElse(new ArrayList<>()).forEach(
        dimensionsAndUomResponse -> dimensionsAndUomResponse.setUomName(
            UomType.getNameByCode(dimensionsAndUomResponse.getUomCode())));
    Optional.ofNullable(dimensionsList).orElse(new ArrayList<>()).forEach(
        dimensionsAndUomResponse -> dimensionsAndUomResponse.setUpcEan(dimensionsAndUomResponse.getUpcEanList()));
    return new DistributionInfoPerSkuResponse(distributionInfoResponse, entity.getSkuCode(),
        skuCodeAndProductItemMap.getOrDefault(entity.getSkuCode(), new ProductItem()).getGeneratedItemName(),
        itemInfoResponse, dimensionsList);
  }

  public static List<DimensionsAndUomResponse> buildDimensionsList(ProductItemUomInfo entity) {
    ObjectMapper objectMapper = new ObjectMapper();
    List<DimensionsAndUomResponse> dimensionsList = null;
    if (StringUtils.isNotBlank(entity.getUom())) {
      try {
        dimensionsList = objectMapper.readValue(entity.getUom(),
            new TypeReference<List<DimensionsAndUomResponse>>() {});
      } catch (Exception e) {
        dimensionsList = new ArrayList<>();
        log.error("Error parsing UOM JSON for SKU: {} ", entity.getSkuCode(), e);
      }
    }
    return dimensionsList;
  }

  public static void setProductItemUomInfoEventModel(ProductItem productItem,
      ProductItemDomainEventModel productItemDomainEventModel) {
    ProductItemUomInfo productItemUomInfo = productItem.getProductItemUomInfo();
    if (Objects.nonNull(productItemUomInfo)) {
      DistributionItemInfoEventModel distributionItemInfoEventModel = new DistributionItemInfoEventModel(
          Optional.ofNullable(productItemUomInfo.getOrigin()).orElse(Origin.LOCAL).name(),
          productItem.getOmniChannelSku(), productItemUomInfo.isExpiry());
      productItemDomainEventModel.setDistributionItemInfo(distributionItemInfoEventModel);
      List<DimensionsAndUomResponse> dimensionsList = CommonUtil.buildDimensionsList(productItemUomInfo);
      List<DimensionsAndUomEventModel> dimensionsAndUOMList = new ArrayList<>();
      for (DimensionsAndUomResponse dimensionsAndUomResponse : dimensionsList) {
        DimensionsAndUomEventModel dimensionsAndUomEventModel = getDimensionsAndUomEventModel(dimensionsAndUomResponse);
        dimensionsAndUOMList.add(dimensionsAndUomEventModel);
      }
      productItemDomainEventModel.setDimensionsAndUOM(dimensionsAndUOMList);
    }
  }

  private static DimensionsAndUomEventModel getDimensionsAndUomEventModel(
      DimensionsAndUomResponse dimensionsAndUomResponse) {
    DimensionsAndUomEventModel dimensionsAndUomEventModel = new DimensionsAndUomEventModel();
    dimensionsAndUomEventModel.setUomCode(dimensionsAndUomResponse.getUomCode());
    dimensionsAndUomEventModel.setUomName(dimensionsAndUomResponse.getUomName());
    dimensionsAndUomEventModel.setUomType(dimensionsAndUomResponse.getUomType());
    dimensionsAndUomEventModel.setConversion(dimensionsAndUomResponse.getConversion());
    dimensionsAndUomEventModel.setLength(dimensionsAndUomResponse.getLength());
    dimensionsAndUomEventModel.setHeight(dimensionsAndUomResponse.getHeight());
    dimensionsAndUomEventModel.setWeight(dimensionsAndUomResponse.getWeight());
    dimensionsAndUomEventModel.setWidth(dimensionsAndUomResponse.getWidth());
    dimensionsAndUomEventModel.setUpcEan(dimensionsAndUomResponse.getUpcEanList());
    return dimensionsAndUomEventModel;
  }

  public static List<ProductMasterDataItemResponse> setProductMasterData(List<Product> productCodeDetails) {
    List<ProductMasterDataItemResponse> productMasterDataResponseList = new ArrayList();
    for (Product product : productCodeDetails) {
      for (ProductItem productItem : product.getProductItems()) {
        ProductMasterDataItemResponse productMasterDataItemResponse =
            ProductMasterDataItemResponse.builder().productCode(product.getProductCode()).length(product.getLength()).width(product.getWidth()).height(product.getHeight())
                .weight(Optional.ofNullable(product.getWeight()).orElse(0.0)).uom(product.getUom()).brand(product.getBrand()).shippingWeight(Optional.ofNullable(product.getShippingWeight()).orElse(0.0))
                .description(Objects.nonNull(product.getDescription()) ? new String(product.getDescription()) : null)
                .productMarkForDelete(product.isMarkForDelete()).productItemMarkForDelete(productItem.isMarkForDelete())
                .skuCode(productItem.getSkuCode()).dangerousGoodsLevel(productItem.getDangerousGoodsLevel())
                .upcCode(productItem.getUpcCode()).vatApplicable(productItem.getVatApplicable())
                .activated(product.isActivated()).generatedItemName(productItem.getGeneratedItemName())
                .merchantCode(product.getCreatedMerchant()).categoryName(
                    Optional.ofNullable(product.getProductCategories()).filter(categories -> !categories.isEmpty())
                        .map(List::getFirst).map(category -> category.getCategory().getName()).orElse(null)).category(
                    Optional.ofNullable(product.getProductCategories()).filter(categories -> !categories.isEmpty())
                        .map(List::getFirst).map(category -> category.getCategory().getCategoryCode()).orElse(null))
                .catalogType(CatalogType.MASTER_CATALOG.name()).build();
        productMasterDataResponseList.add(productMasterDataItemResponse);
      }
    }
    return productMasterDataResponseList;
  }

  public static String extractProductCodeFromItemCode(String itemCode) {
    if (StringUtils.isNotBlank(itemCode)) {
      int idx = -1;
      for (int count = 0; count < 2; count++) {
        idx = itemCode.indexOf(Constants.HYPHEN, idx + 1);
        if (idx == -1) {
          return StringUtils.EMPTY;
        }
      }
      return itemCode.substring(0, idx);
    }
    return StringUtils.EMPTY;
  }
}