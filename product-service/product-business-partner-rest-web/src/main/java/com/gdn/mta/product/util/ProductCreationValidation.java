package com.gdn.mta.product.util;

import com.gda.mta.product.dto.B2bDetailsDTO;
import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerAttributeRequest;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductItemBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.mta.product.repository.AttributeRepository;
import com.gdn.mta.product.service.FileStorageService;
import com.gdn.mta.product.service.config.PreOrderConfig;
import com.gdn.mta.product.service.exception.ApiDataNotFoundException;
import com.gdn.mta.product.util.validator.DescriptiveFieldCharacterValidator;
import com.gdn.mta.product.web.model.ProductBusinessPartnerControllerErrorMessage;
import com.gdn.mta.product.web.model.ProductControllerErrorMessage;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.helper.RequestHelper;
import com.gdn.partners.pbp.helper.ResponseHelper;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3HelperBean;
import com.gdn.x.businesspartner.commons.enums.MerchantType;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * Created by Vishal on 29/06/18.
 */
@Slf4j
@Component
public class ProductCreationValidation {
  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private AttributeRepository attributeRepository;

  @Autowired
  private PreOrderConfig preOrderConfig;

  @Value("${validate.product.descriptive.fields.characters}")
  private boolean validateProductDescriptiveFieldCharacters;

  @Value("${validate.product.descriptive.exclusion.list}")
  private String validateProductDescriptiveFieldExclusionList;

  @Value("${product.bundling.enabled}")
  private boolean productBundlingEnabled;

  @Value("${product.bundling.eligible.merchants}")
  private String productBundlingEligibleMerchantTypes;

  @Value("${product.bundling.max.number.of.skus}")
  private int productBundlingMaxNumberOfSkus;

  @Value("${warehouse.bom.activated}")
  private boolean warehouseBomActivated;

  @Value("${negative.stock.validation.switch}")
  private boolean negativeStockValidation;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${size.chart.value.type.delimiter}")
  private String sizeChartValueTypeDelimiter;

  @Value("${value.type.addition.for.defining.attributes}")
  private boolean valueTypeAdditionForDefiningAttributes;

  @Value("${b2b.base.price.backfill.enabled}")
  private boolean b2bBasePriceBackFillEnabled;

  public void validateProduct(ProductCreationRequest request, boolean familyColourValidationSwitch,
      String imageSourceDirectory, String fullImageSourceDirectory, long maxStockLimit, boolean validateBrandCode,
      boolean validateAtL4Level, boolean validateDuplicateUpcCodeCreate, boolean isSellerOmg) {
    GdnPreconditions.checkArgument(
        !(CollectionUtils.isEmpty(request.getProductItemRequests())) && !(request.getProductItemRequests().stream()
            .anyMatch(productItemRequest -> CollectionUtils.isEmpty(productItemRequest.getImages()))),
        ProductControllerErrorMessage.PRODUCT_ITEM_DETAILS_MUST_NOT_BE_BLANK);
    if (StringUtils.isNotBlank(request.getBusinessPartnerCode())) {
      if(validateAtL4Level) {
        GdnPreconditions.checkArgument(request.getProductItemRequests().stream().allMatch(productItemRequest -> StringUtils.isNotEmpty(productItemRequest.getPickupPointId())),
            ProductControllerErrorMessage.PICKUP_POINT_ID_MUST_NOT_BE_BLANK);
      } else {
        for (ProductItemCreationRequest productItemCreationRequest : request.getProductItemRequests()) {
          GdnPreconditions.checkArgument(productItemCreationRequest.getPickupPoints().stream()
                  .allMatch(productItemRequest -> StringUtils.isNotEmpty(productItemRequest.getPickupPointId())),
              ProductControllerErrorMessage.PICKUP_POINT_ID_MUST_NOT_BE_BLANK);
        }
      }
    }
    fileStorageService.editImageNameIfGcsEnabled(request);
    String unavailableImageName = fileStorageService.checkImageAvailability(request,
        Optional.ofNullable(request.getProductCreationType()).map(ProductCreationType::getProductCreationType)
            .orElse(StringUtils.EMPTY));
    if (StringUtils.isNotEmpty(unavailableImageName)) {
      log.error("Image not found with imageName :{}, for product code :{}", unavailableImageName,
          request.getProductCode());
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          ProductControllerErrorMessage.IMAGE_NOT_FOUND + unavailableImageName);
    }
    filterNonMandatoryDefiningProductAttributeAndItemsRequest(request);
    if (StringUtils.isNotEmpty(request.getBusinessPartnerCode()) && CollectionUtils
        .isNotEmpty(request.getProductBusinessPartnerAttributes())) {
      for (ProductBusinessPartnerAttributeRequest partnerAttributeRequest : request
          .getProductBusinessPartnerAttributes()) {
        if (partnerAttributeRequest.isMandatory()) {
          GdnPreconditions.checkArgument(!StringUtils.isBlank(partnerAttributeRequest.getValue()),
              ProductBusinessPartnerControllerErrorMessage.MISSING_MANDATORY_VALUE_IN_BP_ATTRIBUTES);
        }
      }
    }
    for (ProductAttributeRequest productAttribute : request.getProductAttributes()) {
      checkMandatoryAttributeValueNotEmpty(productAttribute, request.getBusinessPartnerCode());
      if (AttributeType.DESCRIPTIVE_ATTRIBUTE.equals(productAttribute.getAttribute().getAttributeType()) && StringUtils
          .isBlank(productAttribute.getProductAttributeValues().get(0).getDescriptiveAttributeValue())) {
        productAttribute.getProductAttributeValues().get(0).setDescriptiveAttributeValue("-");
      }
      if (AttributeType.PREDEFINED_ATTRIBUTE.equals(productAttribute.getAttribute().getAttributeType())
          && !productAttribute.getAttribute().isMandatory()) {
        if (StringUtils.isBlank(
            productAttribute.getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue().getValue())) {
          setPredefinedAllowedAttributeValueRequest(productAttribute);
        }
        if (validateBrandCode && StringUtils.isEmpty(request.getOldProductCode())) {
          if (Constants.BRAND.equals(productAttribute.getProductAttributeName())
              && !productAttribute.getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue()
              .getPredefinedAllowedAttributeCode().equals(request.getBrandCode())) {
            log.error("brand code invalid for productCode : {} ", request.getProductCode());
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
                ProductControllerErrorMessage.BRAND_CODE_INVALID);
          }
        }
      }
    }
    if (CollectionUtils.isNotEmpty(request.getProductBusinessPartnerAttributes())) {
      for (ProductBusinessPartnerAttributeRequest partnerAttributeRequest : request
          .getProductBusinessPartnerAttributes()) {
        if (StringUtils.isBlank(partnerAttributeRequest.getValue()) && !partnerAttributeRequest.isMandatory()) {
          partnerAttributeRequest.setValue(Constants.DELIMITER_DASH);
        }
      }
    }

    if (!checkWarnaAndFamilyColorBothShouldBePresent(request)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.WARNA_AND_FAMILY_COLOR_ERROR.getDesc());
    }

    if (familyColourValidationSwitch) {
      boolean isWarnaPresentAndDefiningAttributeType = request.getProductAttributes().stream().anyMatch(
          ProductCreationValidation::isWarnaAndDefiningAttributeOrVariantCreationTrue);
      if (isWarnaPresentAndDefiningAttributeType) {
        request.getProductItemRequests().stream().filter(productItemCreationRequest -> CollectionUtils
            .isNotEmpty(productItemCreationRequest.getProductItemAttributeValueRequests()))
            .forEach(ProductCreationValidation::checkFamilyColour);
      }
    }

    if (validateAtL4Level) {
      if (request.getProductItemRequests().stream()
          .filter(productItemCreationRequest -> Objects.nonNull(productItemCreationRequest.getStock()))
          .anyMatch(productItemCreationRequest -> productItemCreationRequest.getStock() > maxStockLimit)) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            String.format(ProductControllerErrorMessage.MAX_STOCK_LIMIT_ERROR, maxStockLimit));
      }
    } else {
      for (ProductItemCreationRequest productItemCreationRequest : request.getProductItemRequests()) {
        if (productItemCreationRequest.getPickupPoints().stream()
            .filter(pickupPointCreateRequest -> Objects.nonNull(pickupPointCreateRequest.getStock()))
            .anyMatch(pickupPointCreateRequest -> {
              if (preOrderConfig.isPoQuotaFeatureSwitch() && isSellerOmg && Objects.nonNull(request.getPreOrder())
                  && Boolean.TRUE.equals(request.getPreOrder().getIsPreOrder())
                  && !CommonUtils.validateForPreOrderStockChecks(pickupPointCreateRequest.getStock(),
                  request.getPreOrder().getPreOrderDate())) {
                pickupPointCreateRequest.setStock(0);
              }
              return pickupPointCreateRequest.getStock() > maxStockLimit
                  || (preOrderConfig.isPoQuotaFeatureSwitch() && pickupPointCreateRequest.getPreOrderQuotaNullSafe() > maxStockLimit);
            })) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              String.format(ProductControllerErrorMessage.MAX_STOCK_LIMIT_ERROR, maxStockLimit));
        }
      }
    }

    for (ProductItemCreationRequest productItemCreationRequest : request.getProductItemRequests()) {
      if (negativeStockValidation && (productItemCreationRequest.getPickupPoints().stream()
          .filter(pickupPointCreateRequest -> Objects.nonNull(pickupPointCreateRequest.getStock()))
          .anyMatch(pickupPointCreateRequest ->
              pickupPointCreateRequest.getStock() < 0 || (preOrderConfig.isPoQuotaFeatureSwitch() && pickupPointCreateRequest.getPreOrderQuotaNullSafe() < 0)))) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ApiErrorCode.NEGATIVE_STOCK_NOT_ALLOWED.getDesc());
      }
    }

    if (request.isFreeSample() && (Objects.nonNull(request.getPreOrder()) && Boolean.TRUE
      .equals(request.getPreOrder().getIsPreOrder()))) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        ApiErrorCode.FREE_SAMPLE_CANNOT_BE_SET.getDesc());
    }

    for (ProductItemCreationRequest productItemCreationRequest : request.getProductItemRequests()) {
      if (validateAtL4Level) {
        if (request.isFreeSample() && (Objects.nonNull(productItemCreationRequest.getWholesalePriceActivated())
            && Boolean.TRUE.equals(productItemCreationRequest.getWholesalePriceActivated()))) {
          log.info("Setting wholesale price activated as false for free sample product : {}", request.getProductCode());
          productItemCreationRequest.setWholesalePriceActivated(false);
        }
      } else {
        for (PickupPointCreateRequest pickupPointCreateRequest : productItemCreationRequest.getPickupPoints()) {
          if (request.isFreeSample() && (Objects.nonNull(pickupPointCreateRequest.getWholesalePriceActivated())
              && Boolean.TRUE.equals(pickupPointCreateRequest.getWholesalePriceActivated()))) {
            log.info("Setting wholesale price activated as false for free sample product : {}",
                request.getProductCode());
            pickupPointCreateRequest.setWholesalePriceActivated(false);
          }
        }
      }
      if (validateBrandCode && StringUtils.isEmpty(request.getOldProductCode())) {
        productItemCreationRequest.getProductItemAttributeValueRequests().forEach(
            productItemAttributeValueRequest -> validateItemBrandCode(productItemAttributeValueRequest, request));
      }
    }
    for (ProductItemCreationRequest productItemCreationRequest : request.getProductItemRequests()) {
      for (PickupPointCreateRequest pickupPointCreateRequest : productItemCreationRequest.getPickupPoints()) {
        if (cncForWarehouseFeatureSwitch) {
          checkValidProductPickupPointStatus(pickupPointCreateRequest);
        } else {
          if (pickupPointCreateRequest.isCncActive()) {
            pickupPointCreateRequest.setCncDisplay(true);
            pickupPointCreateRequest.setCncBuyable(true);
          }
        }
      }
    }

    List<String> upcCodes = new ArrayList<>();
    for (ProductItemCreationRequest productItemCreationRequest : request.getProductItemRequests()) {
      if (validateDuplicateUpcCodeCreate) {
        if (StringUtils.isNotEmpty(productItemCreationRequest.getUpcCode())) {
          if (upcCodes.contains(productItemCreationRequest.getUpcCode())) {
            log.error("Duplicate EAN/ UPC for productCode : {} value : {} ", request.getProductCode(),
                productItemCreationRequest.getUpcCode());
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.DUPLICATE_UPC_CODE.getDesc());
          } else {
            upcCodes.add(productItemCreationRequest.getUpcCode());
          }
        }
      }
    }

    DescriptiveFieldCharacterValidator.validateProductCreationRequest(request,
        Arrays.asList(validateProductDescriptiveFieldExclusionList.split(Constants.COMMA)),
        validateProductDescriptiveFieldCharacters);
  }

  public void validateProductSizeChartAttribute(ProductCreationRequest productRequest, String channelId)
      throws Exception {
    if (valueTypeAdditionForDefiningAttributes && Constants.MTA_API_CHANNEL_ID.equalsIgnoreCase(channelId)) {
      Set<String> attributeIds = RequestHelper.getSizeChartAttributeCodeWithValuesWithoutDelimiter(productRequest,
          sizeChartValueTypeDelimiter);
      Map<String, String> allowedAttributeValueIdAndValueTypeMap =
          getAllowedAttributeValueIdAndValueTypeMap(attributeIds);
      Map<String, Map<String, String>> attributeCodeAndValueValueType =
          RequestHelper.updateProductAllowedAttributeValue(productRequest, allowedAttributeValueIdAndValueTypeMap);
      RequestHelper.updateProductItemAttributeValue(productRequest, attributeCodeAndValueValueType);
    }
  }

  private Map<String, String> getAllowedAttributeValueIdAndValueTypeMap(Set<String> attributeIds) throws Exception {
    Map<String, String> allowedAttributeValueIdAndValueTypeMap = new HashMap<>();
    for (String attributeId : attributeIds) {
      AttributeResponse attributeResponse = attributeRepository.findDetailById(attributeId);
      allowedAttributeValueIdAndValueTypeMap.putAll(
          ResponseHelper.addAllowedAttributeValueWithValueTypeIsNotEmpty(attributeResponse));
    }
    return allowedAttributeValueIdAndValueTypeMap;
  }

  public void bundleProductValidation(ProductCreationRequest productCreationRequest, ProfileResponse profileResponse) {
    if (productBundlingEnabled && Objects.nonNull(profileResponse) && Arrays.asList(
            productBundlingEligibleMerchantTypes.split(Constants.COMMA))
        .contains(profileResponse.getCompany().getMerchantType()) && productCreationRequest.isBundleProduct()) {

      // Remove products from bundle recipe which don't have itemSkus
      RequestHelper.removeProductsFromBundleRecipeWhichDontHaveItemSku(productCreationRequest);

      // Any one of variants should have bundle recipe during creation
      validateAnyVariantWithoutBundleRecipe(productCreationRequest);

      // All variants should have maximum 10 skus in bundle recipe
      validateAnyVariantWithMoreThanMaxSkusInBundleRecipe(productCreationRequest);

      //All recipe sku quantity should be greater than 0
      validateAnyVariantWithQuantityLessThanEqualToZero(productCreationRequest);

      // All variants should have item sku in bundle recipe
      validateAnyVariantWithoutProperItemSkuInBundleRecipe(productCreationRequest);

      //Call to x-product if child product are active or not and are trading product if seller type is td otherwise it should belong to same merchant
      validateBundleProductChildSkuStatusAndChildSkusByMerchantType(productCreationRequest, profileResponse);

    } else {
      // If the switch is off or merchant type is not configured or its not bundle product we will clear bundling details
      productCreationRequest.setBundleProduct(false);
      Optional.ofNullable(productCreationRequest.getProductItemRequests()).orElse(new ArrayList<>())
          .forEach(productItemCreationRequest -> productItemCreationRequest.setBundleRecipe(null));
    }
  }

  private void validateAnyVariantWithoutBundleRecipe(ProductCreationRequest productCreationRequest) {
    boolean anyVariantWithBundleRecipe =
        Optional.ofNullable(productCreationRequest.getProductItemRequests()).orElse(new ArrayList<>()).stream()
            .anyMatch(
                productItemCreationRequest -> CollectionUtils.isNotEmpty(productItemCreationRequest.getBundleRecipe()));
    if (!anyVariantWithBundleRecipe) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.BUNDLING_INFO_NOT_PRESENT_FOR_VARIANT.getDesc());
    }
  }

  private void validateAnyVariantWithMoreThanMaxSkusInBundleRecipe(ProductCreationRequest productCreationRequest) {
    boolean anyVariantWithMoreThanMaxSkusInBundleRecipe =
        Optional.ofNullable(productCreationRequest.getProductItemRequests()).orElse(new ArrayList<>()).stream()
            .anyMatch(productItemCreationRequest -> productItemCreationRequest.getBundleRecipe().size()
                > productBundlingMaxNumberOfSkus);
    if (anyVariantWithMoreThanMaxSkusInBundleRecipe) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.BUNDLE_RECIPE_MAX_SKUS.getDesc());
    }
  }

  private void validateAnyVariantWithoutProperItemSkuInBundleRecipe(ProductCreationRequest productCreationRequest) {
    boolean anyVariantWithoutProperItemSkuInBundleRecipe =
        Optional.ofNullable(productCreationRequest.getProductItemRequests()).orElse(new ArrayList<>()).stream()
            .flatMap(productItemCreationRequest -> productItemCreationRequest.getBundleRecipe().stream())
            .anyMatch(bundleRecipeRequest -> !RequestHelper.isItemSku(bundleRecipeRequest.getItemSku()));
    if (anyVariantWithoutProperItemSkuInBundleRecipe) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.BUNDLE_RECIPE_CHILD_PRODUCTS_SHOULD_BE_AT_VARIANT_LEVEL.getDesc());
    }
  }

  private void validateAnyVariantWithQuantityLessThanEqualToZero(ProductCreationRequest productCreationRequest) {
    boolean anyVariantWithoutProperItemSkuInBundleRecipe =
        Optional.ofNullable(productCreationRequest.getProductItemRequests()).orElse(new ArrayList<>()).stream()
            .flatMap(productItemCreationRequest -> productItemCreationRequest.getBundleRecipe().stream())
            .anyMatch(bundleRecipeRequest -> bundleRecipeRequest.getQuantity() <= 0);
    if (anyVariantWithoutProperItemSkuInBundleRecipe) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.BUNDLE_RECIPE_LESS_THAN_EQUAL_TO_ZERO_QUANTITY.getDesc());
    }
  }

  private void validateBundleProductChildSkuStatusAndChildSkusByMerchantType(ProductCreationRequest productCreationRequest, ProfileResponse profileResponse) {
    List<String> childProductSkus = RequestHelper.getProductSkuForBundleProductChildSkus(productCreationRequest);
    List<ProductBasicResponse> productBasicResponseList = xProductOutbound.getProductBasicDetails(childProductSkus);
    Map<String, ProductBasicResponse> productBasicResponseMap =
        Optional.ofNullable(productBasicResponseList).orElse(new ArrayList<>()).stream()
            .collect(Collectors.toMap(ProductBasicResponse::getProductSku, Function.identity(), (v1, v2) -> v2));

    //validate product activation status
    validateBundleChildSkuStatus(childProductSkus, productBasicResponseMap);

    // validate child sku with merchant type
    validateBundleChildSkuByMerchantType(productBasicResponseList, profileResponse);

    //validated which child sku variant is deleted or not
    validateIfChildSkusAreDeleted(productCreationRequest);
  }

  private void validateIfChildSkusAreDeleted(ProductCreationRequest productCreationRequest) {
    List<String> childItemSkus = RequestHelper.getItemSkuForBundleProductChildSkus(productCreationRequest);
    List<ItemBasicDetailV2Response> itemBasicDetailV2Responses =
        xProductOutbound.getItemBasicDetailV2Response(childItemSkus, false);
    boolean isAnyItemDeleted = itemBasicDetailV2Responses.stream().anyMatch(ItemBasicDetailV2Response::isMarkForDelete);
    if (isAnyItemDeleted || childItemSkus.size() != itemBasicDetailV2Responses.size()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.BUNDLE_RECIPE_CHILD_IS_DELETED.getDesc());
    }
  }

  private void validateBundleChildSkuStatus(List<String> childProductSkus,
      Map<String, ProductBasicResponse> productBasicResponseMap) {
    for (String childProductSku : childProductSkus) {
      if (productBasicResponseMap.containsKey(childProductSku)) {
        ProductBasicResponse productBasicResponse = productBasicResponseMap.get(childProductSku);
        if (!productBasicResponse.isProductExists()) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              String.format(ApiErrorCode.BUNDLE_RECIPE_CHILD_PRODUCT_NOT_FOUND.getDesc(), childProductSku));
        } else if (productBasicResponse.isMarkForDelete() || productBasicResponse.isSuspended()
            || productBasicResponse.isArchived()) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              String.format(ApiErrorCode.BUNDLE_RECIPE_CHILD_IS_NOT_ACTIVATED.getDesc(), childProductSku));
        }
      } else {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            String.format(ApiErrorCode.BUNDLE_RECIPE_CHILD_PRODUCT_NOT_FOUND.getDesc(), childProductSku));
      }
    }
  }

  private void validateBundleChildSkuByMerchantType(List<ProductBasicResponse> productBasicResponseList,
      ProfileResponse profileResponse) {
    if (MerchantType.TD.name().equals(profileResponse.getCompany().getMerchantType())) {
      // verify all child sku is trading product
      boolean anyChildSkuWhichIsNotTradingProduct =
          Optional.ofNullable(productBasicResponseList).orElse(new ArrayList<>()).stream()
              .anyMatch(Predicate.not(ProductBasicResponse::isTradingProduct));
      if (anyChildSkuWhichIsNotTradingProduct) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ApiErrorCode.BUNDLE_RECIPE_CHILD_IS_NOT_TRADING_PRODUCT.getDesc());
      }
    } else {
      boolean anyChildSkuWhichDontBelongToCurrentSeller =
          Optional.ofNullable(productBasicResponseList).orElse(new ArrayList<>()).stream().map(
                  productBasicResponse -> productBasicResponse.getProductSku()
                      .substring(0, productBasicResponse.getProductSku().lastIndexOf(Constants.HYPHEN)))
              .anyMatch(merchantCode -> !profileResponse.getBusinessPartnerCode().equals(merchantCode));
      // verify all child sku belong to current seller
      if (anyChildSkuWhichDontBelongToCurrentSeller) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ApiErrorCode.BUNDLE_RECIPE_CHILD_DOES_NOT_BELONG_TO_CURRENT_SELLER.getDesc());
      }
    }
  }

  public static void validateItemBrandCode(ProductItemAttributeValueRequest productItemAttributeValueRequest,
      ProductCreationRequest productCreationRequest) {
    if (Constants.BRAND.equals(productItemAttributeValueRequest.getAttribute().getName())
        && !productItemAttributeValueRequest.getAttribute().getPredefinedAllowedAttributeValues().get(0)
        .getPredefinedAllowedAttributeCode().equals(productCreationRequest.getBrandCode())) {
      log.error("invalid brand code for productCode  : {} ", productCreationRequest.getProductCode());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ProductControllerErrorMessage.BRAND_CODE_INVALID);
    }
  }

  public static void validateWholeSalePriceSettingOnFlow1(ProductCreationRequest productCreationRequest,
      int maxWholesalePriceRequests, Integer minimumPrice) {
    for (ProductItemCreationRequest productItemCreationRequest : productCreationRequest.getProductItemRequests()) {
      CommonUtils.validateWholesalePrice(productItemCreationRequest.getProductItemWholesalePriceRequests(),
          productItemCreationRequest.getSalePrice(), minimumPrice, maxWholesalePriceRequests, null);
    }
  }

  public static void validateWholeSalePriceSettingOnFlow1AtL5(ProductCreationRequest productCreationRequest,
      int maxWholesalePriceRequests, Integer minimumPrice) {
    for (ProductItemCreationRequest productItemCreationRequest : productCreationRequest.getProductItemRequests()) {
      for (PickupPointCreateRequest pickupPointCreateRequest : productItemCreationRequest.getPickupPoints()) {
        CommonUtils.validateWholesalePrice(pickupPointCreateRequest.getProductItemWholesalePriceRequests(),
            pickupPointCreateRequest.getSalePrice(), minimumPrice, maxWholesalePriceRequests, null);
      }
    }
  }

  private static boolean checkWarnaAndFamilyColorBothShouldBePresent(ProductCreationRequest productRequest) {
    boolean isFamilyColorPresent = productRequest.getProductItemRequests().stream().flatMap(
        productItemRequest -> Optional.ofNullable(productItemRequest.getProductItemAttributeValueRequests())
            .orElse(new ArrayList<>()).stream()).anyMatch(
        productItemAttributeValueRequest -> Constants.FAMILY_COLOUR.equals(
            productItemAttributeValueRequest.getAttribute().getName()));
    boolean isWarnaPresent =
        Optional.ofNullable(productRequest.getProductAttributes()).orElse(new ArrayList<>()).stream()
            .anyMatch(ProductCreationValidation::isWarnaAndDefiningAttributeOrVariantCreationTrue);
    if (isFamilyColorPresent) {
      return isWarnaPresent;
    }
    return true;
  }

  public static void validateWholeSalePriceSettingOnFlow2(
      List<ProductItemBusinessPartnerRequest> productItemBusinessPartnerRequests, Integer minimumPrice, int maxWholesalePriceRequests) {
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : productItemBusinessPartnerRequests) {
      CommonUtils.validateWholesalePrice(productItemBusinessPartnerRequest.getProductItemWholesalePriceRequests(),
          productItemBusinessPartnerRequest.getSalePrice(), minimumPrice, maxWholesalePriceRequests, null);
    }
  }


  private static boolean isWarnaAndDefiningAttributeOrVariantCreationTrue(
      ProductAttributeRequest productAttributeRequest) {
    return Constants.WARNA.equals(productAttributeRequest.getAttribute().getName()) && (
        AttributeType.DEFINING_ATTRIBUTE.equals(productAttributeRequest.getAttribute().getAttributeType())
            || productAttributeRequest.getAttribute().isVariantCreation());
  }

  private static void checkMandatoryAttributeValueNotEmpty(ProductAttributeRequest productAttribute,
      String businessPartnerCode) {
    if (AttributeType.PREDEFINED_ATTRIBUTE.equals(productAttribute.getAttribute().getAttributeType())
        && productAttribute.getAttribute().isMandatory()) {
      if (!(productAttribute.getAttribute().isSkuValue() && StringUtils.isBlank(businessPartnerCode))) {
        if (StringUtils.isBlank(
            productAttribute.getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue().getValue())
            || Constants.DELIMITER_DASH.equals(
            productAttribute.getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue().getValue())) {
          throw new ApiDataNotFoundException(Constants.PREDEFINED_MANDATORY_MUST_NOT_NULL_OR_INVALID,
              ApiErrorCode.MANDATORY_FIELD_NOT_FOUND);
        }
      }
    }
    if (AttributeType.DESCRIPTIVE_ATTRIBUTE.equals(productAttribute.getAttribute().getAttributeType())
        && productAttribute.getAttribute().isMandatory()) {
      if (!(productAttribute.getAttribute().isSkuValue() && StringUtils.isBlank(businessPartnerCode))) {
        for (ProductAttributeValueRequest productAttributeValueRequest : productAttribute.getProductAttributeValues()) {
          if (StringUtils.isBlank(productAttributeValueRequest.getDescriptiveAttributeValue())) {
            throw new ApiDataNotFoundException(Constants.DESCRIPTIVE_MANDATORY_MUST_NOT_NULL,
                ApiErrorCode.MANDATORY_FIELD_NOT_FOUND);
          }
        }
      }
    }
    if (AttributeType.DEFINING_ATTRIBUTE.equals(productAttribute.getAttribute().getAttributeType()) && productAttribute
        .getAttribute().isMandatory()) {
      if (CollectionUtils.isNotEmpty(productAttribute.getProductAttributeValues())) {
        for (ProductAttributeValueRequest productAttributeValueRequest : productAttribute.getProductAttributeValues()) {
          if (StringUtils.isBlank(
            Optional.ofNullable(productAttributeValueRequest.getAllowedAttributeValue())
              .map(AllowedAttributeValueRequest::getValue).orElse(null))) {
            throw new ApiDataNotFoundException(Constants.DEFINING_ATTRIBUTE_VALUE_MUST_NOT_NULL,
              ApiErrorCode.MANDATORY_FIELD_NOT_FOUND);
          }
        }
      } else {
        throw new ApiDataNotFoundException(Constants.DEFINING_ATTRIBUTE_MUST_NOT_NULL,
            ApiErrorCode.MANDATORY_FIELD_NOT_FOUND);
      }
    }
    if (!productAttribute.getAttribute().isSkuValue() && AttributeType.DESCRIPTIVE_ATTRIBUTE
        .equals(productAttribute.getAttribute().getAttributeType())
        && productAttribute.getProductAttributeValues().get(0).getDescriptiveAttributeValue().length() > 255) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          Constants.MAXIMUM_CHARACTERS_ACCEPTED_IS_255_CHARACTERS_INCLUDING_SPACE);
    }
  }

  private static void setPredefinedAllowedAttributeValueRequest(ProductAttributeRequest productAttribute) {
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setPredefinedAllowedAttributeCode(
        productAttribute.getAttribute().getAttributeCode() + Constants.DELIMITER_DASH + Constants.DEFAULT);
    predefinedAllowedAttributeValueRequest.setSequence(0);
    predefinedAllowedAttributeValueRequest.setValue(Constants.DELIMITER_DASH);
    productAttribute.getProductAttributeValues().get(0)
        .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
  }

  private static void checkFamilyColour(ProductItemCreationRequest productItemCreationRequest) {
    productItemCreationRequest.getProductItemAttributeValueRequests().stream().filter(
        productItemAttributeValueRequest -> Constants.FAMILY_COLOUR
            .equals(productItemAttributeValueRequest.getAttribute().getName())).collect(Collectors.toList())
        .forEach(ProductCreationValidation::checkFamilyColourValue);
  }

  private static void checkFamilyColourValue(ProductItemAttributeValueRequest productItemAttributeValueRequest) {
    if (StringUtils.isEmpty(productItemAttributeValueRequest.getValue())) {
      throw new ApiDataNotFoundException(Constants.FAMILY_COLOR_FIELD_MUST_NOT_NULL, ApiErrorCode.FAMILY_COLOUR_NOT_FOUND);
    }
  }

  private static void filterNonMandatoryDefiningProductAttributeAndItemsRequest(
      ProductCreationRequest productCreationRequest) {
    Map<String, ProductAttributeRequest> productAttributeRequestMap =
        productCreationRequest.getProductAttributes().stream().filter(
            ProductCreationValidation::isNonMandatoryVariantCreatingAttributeEmpty)
            .collect(Collectors
                .toMap(productAttributeRequest -> productAttributeRequest.getAttribute().getAttributeCode(),
                    Function.identity()));
    log.info("Found the non mandatory defining attribute with empty values : {},", productAttributeRequestMap);
    productCreationRequest.getProductAttributes().removeIf(productAttributeRequest -> productAttributeRequestMap
        .containsKey(productAttributeRequest.getAttribute().getAttributeCode()));
    productCreationRequest.getProductItemRequests().stream().forEach(
        productItemCreationRequest -> removeAttributeMappingWithItems(productItemCreationRequest,
            productAttributeRequestMap));
  }

  private static void removeAttributeMappingWithItems(ProductItemCreationRequest productItemCreationRequest,
      Map<String, ProductAttributeRequest> productAttributeRequestMap) {
    if (Objects.nonNull(productItemCreationRequest.getAttributesMap()) && !productItemCreationRequest.getAttributesMap()
        .isEmpty()) {
      for (Map.Entry<String, ProductAttributeRequest> productAttributeRequestEntry : productAttributeRequestMap
          .entrySet()) {
        if (productItemCreationRequest.getAttributesMap().containsKey(productAttributeRequestEntry.getKey())) {
          log.info("Deleting the attribute mapping in items for attributeCode : {}",
              productAttributeRequestEntry.getKey());
          productItemCreationRequest.getAttributesMap().remove(productAttributeRequestEntry.getKey());
        }
      }
    }
  }

  private static boolean isNonMandatoryVariantCreatingAttributeEmpty(ProductAttributeRequest productAttribute) {
    if (AttributeType.DEFINING_ATTRIBUTE.equals(productAttribute.getAttribute().getAttributeType()) && productAttribute
        .getAttribute().isVariantCreation() && !productAttribute.getAttribute().isMandatory()) {
      if (CollectionUtils.isNotEmpty(productAttribute.getProductAttributeValues())) {
        for (ProductAttributeValueRequest productAttributeValueRequest : productAttribute.getProductAttributeValues()) {
          if (Objects.isNull(productAttributeValueRequest.getAllowedAttributeValue()) || StringUtils
              .isBlank(productAttributeValueRequest.getAllowedAttributeValue().getValue())) {
            return true;
          }
        }
      } else {
        return true;
      }
    }
    if (AttributeType.DESCRIPTIVE_ATTRIBUTE.equals(productAttribute.getAttribute().getAttributeType())
        && productAttribute.getAttribute().isVariantCreation() && !productAttribute.getAttribute().isMandatory()) {
      if (CollectionUtils.isNotEmpty(productAttribute.getProductAttributeValues())) {
        for (ProductAttributeValueRequest productAttributeValueRequest : productAttribute.getProductAttributeValues()) {
          if (StringUtils.isBlank(productAttributeValueRequest.getDescriptiveAttributeValue())) {
            return true;
          }
        }
      } else {
        return true;
      }
    }
    return false;
  }

  private void checkValidProductPickupPointStatus(PickupPointCreateRequest pickupPointCreateRequest) {
    ProductLevel3Status productDeliveryStatus =
        ProductLevel3HelperBean.getProductLevel3Status(pickupPointCreateRequest.isBuyable(),
            pickupPointCreateRequest.isDisplay());
    ProductLevel3Status productCncStatus =
        ProductLevel3HelperBean.getProductLevel3Status(pickupPointCreateRequest.isCncBuyable(),
            pickupPointCreateRequest.isCncDisplay());
    if (ProductLevel3Status.OFFLINE.equals(productDeliveryStatus) || ProductLevel3Status.OFFLINE.equals(
        productCncStatus)) {
      return;
    }
    if (!productDeliveryStatus.equals(productCncStatus)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.INVALID_PRODUCT_ITEM_STATUS.getDesc());
    }
  }

  public void validateBasePriceForB2BSeller(ProductCreationRequest request,
    ProfileResponse profileResponse) {
    List<String> salesChannel = CommonUtils.salesChannelFromProfileResponse(profileResponse);
    if (salesChannel.contains(Constants.B2B_SELLER_CHANNEL) && b2bBasePriceBackFillEnabled) {
      List<PickupPointCreateRequest> pickupPointCreateRequests =
        request.getProductItemRequests().stream().map(ProductItemCreationRequest::getPickupPoints)
          .flatMap(List::stream).toList();
      for (PickupPointCreateRequest pickupPointCreateRequest : pickupPointCreateRequests) {
        if (Objects.isNull(pickupPointCreateRequest.getB2bFields()) || Objects.isNull(
          pickupPointCreateRequest.getB2bFields().getPrice())) {
          Optional.ofNullable(pickupPointCreateRequest.getB2bFields()).ifPresentOrElse(
            b2bDetailsDTO -> b2bDetailsDTO.setPrice(pickupPointCreateRequest.getPrice()),
            () -> pickupPointCreateRequest.setB2bFields(
              new B2bDetailsDTO(pickupPointCreateRequest.getPrice(), false, false, false)));
        }
      }
    }
  }
}