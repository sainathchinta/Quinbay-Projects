package com.gdn.mta.product.util.validator;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gda.mta.product.dto.MasterDataUpdateRequest;
import com.gda.mta.product.dto.MasterProductEditDTO;
import com.gda.mta.product.dto.PickupPointDeleteRequest;
import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gda.mta.product.dto.response.L2StockAvailabilityDTO;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.product.enums.L3InfoUpdateChangeType;
import com.gdn.mta.product.enums.ProductType;

import com.gdn.mta.product.service.exception.ApiIncorrectInputDataException;
import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WarehouseInventoryResponseDTO;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.jsoup.Jsoup;
import org.jsoup.safety.Whitelist;
import com.gda.mta.product.dto.BuyableScheduleRequest;
import com.gda.mta.product.dto.DiscoverableScheduleRequest;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.PredictionCategoryMappingUpdateRequest;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.PreOrderType;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.util.BarcodeGenerator;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ValidationUtil {

  private static final String MAXIMUM = "Maximum ";
  private static final String MAXIMUM_WEEK_ERROR = " weeks are allowed";
  private static final String DAY_MUST_BE_LESS_THAN_LIMIT = " days are allowed";
  private static final String PREORDER_DATE_EXCEEDED_LIMIT = "PreOrder date is exceeded ";
  private static final String INVALID_EAN_UPC_FORMAT = "EAN UPC code is not in valid format";
  private static final String DAYS = " days";
  private static final int MAX_DAYS = 90;
  public static final String SHIPPING_WEIGHT = "Shipping Weight";
  public static final String WEIGHT = "Weight";
  public static final String HEIGHT = "Height";
  public static final String LENGTH = "Length";
  public static final String WIDTH = "Width";
  public static final String PRE_ORDER_DAY_TYPE = "DAYS";
  public static final String PREORDER_WEEK_TYPE = "WEEK";
  public static final String PRE_ORDER_DATE_TYPE = "DATE";
  public static final double GMS_TO_KG_FACTOR = 1000.00;
  public static final double DEFAULT_DIMENSION_VALUES = 0.0;
  private static final int PRODUCT_NAME_LIMIT = 150;

  public static void validatePreOrder(PreOrderRequest preOrderRequest, int preOrderMaximumDays, int preOrderMaximumWeek,
      boolean convertPreOrderDateToJKT) throws Exception {
    if (PreOrderType.DAYS.name().equals(preOrderRequest.getPreOrderType())) {
      if (preOrderRequest.getPreOrderValue() <= 0) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.DAY_MUST_NOT_BE_ZERO);
      } else if (preOrderRequest.getPreOrderValue() > preOrderMaximumDays) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            MAXIMUM + preOrderMaximumDays + DAY_MUST_BE_LESS_THAN_LIMIT);
      }
      setPreOrderDate(preOrderRequest, preOrderRequest.getPreOrderValue());
    } else if (PreOrderType.WEEK.name().equals(preOrderRequest.getPreOrderType())) {
      if (preOrderRequest.getPreOrderValue() <= 0) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.WEEK_MUST_BE_MORE_THAN_ZERO);
      } else if (preOrderRequest.getPreOrderValue() > preOrderMaximumWeek) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            MAXIMUM + preOrderMaximumWeek + MAXIMUM_WEEK_ERROR);
      }
      setPreOrderDate(preOrderRequest, preOrderRequest.getPreOrderValue() * Constants.DAYS_IN_ONE_WEEK);
    } else if (PreOrderType.DATE.name().equals(preOrderRequest.getPreOrderType())) {
      SimpleDateFormat PREORDER_DATE_FORMAT = new SimpleDateFormat("dd/MM/yyyy");
      Date currentDate = PREORDER_DATE_FORMAT.parse(PREORDER_DATE_FORMAT.format(new Date()));
      Date preOrderDate = PREORDER_DATE_FORMAT
          .parse(PREORDER_DATE_FORMAT.format(preOrderRequest.getPreOrderDate()));
      Calendar calendar = Calendar.getInstance();
      calendar.setTime(currentDate);
      calendar.add(Calendar.DATE, MAX_DAYS);
      Date daysAddedDate = calendar.getTime();
      if (currentDate.equals(preOrderDate) || currentDate.after(preOrderDate)) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ErrorMessages.PREORDER_DATE_MUST_BE_GREATER_THAN_CURRENT_DATE);
      } else if (preOrderDate.after(daysAddedDate)) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessages.PREORDER_DATE_CANNOT_BE_GREATER_THAN_90_DAYS);
      }
      if (!convertPreOrderDateToJKT) {
        // Due to date formatting to dd/MM/YY Timezone is being lost and in xprod we are doing
        // -7 hrs which is causing discrepancy
        preOrderRequest.setPreOrderDate(preOrderDate);
      }
      long totalDays = Math.abs(preOrderDate.getTime() - currentDate.getTime()) / Constants.TOTAL_HOURS;
      if (totalDays > preOrderMaximumDays) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            PREORDER_DATE_EXCEEDED_LIMIT + preOrderMaximumDays + DAYS);
      }
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.WRONG_PREORDER_TYPE);
    }
  }

  private static void setPreOrderDate(PreOrderRequest preOrderRequest, int days) {
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, days);
    preOrderRequest.setPreOrderDate(cal.getTime());
  }

  public static void validateCreateProductItemsRequest(ProductCreationRequest productCreationRequest,
      boolean validateItemAndProductAttributeValue, List<String> variantRestrictedValues,
      boolean variantRestrictedValuesFlag) {
    if (!ValidationUtil.isWarnaPresent(productCreationRequest)) {
      productCreationRequest.getProductItemRequests().stream()
        .map(ProductItemCreationRequest::getProductItemAttributeValueRequests)
        .filter(CollectionUtils::isNotEmpty).forEach(
          productItemAttributeValueRequests -> productItemAttributeValueRequests.removeIf(
            productItemAttributeValueRequest -> Constants.FAMILY_COLOUR.equals(
              productItemAttributeValueRequest.getAttribute().getName())));
    }
    List<ProductItemCreationRequest> productItemRequests = productCreationRequest.getProductItemRequests();
    List<ProductItemCreationRequest> updatedProductItemRequests = new ArrayList<>();
    for (ProductItemCreationRequest productItemCreationRequest : productItemRequests) {
      List<ProductItemAttributeValueRequest> attributeValueRequests = productItemCreationRequest.getProductItemAttributeValueRequests();
      List<ProductItemAttributeValueRequest> uniqueAttributeValueRequests = uniqueProductItemAttributeValueRequests(attributeValueRequests);
      productItemCreationRequest.setProductItemAttributeValueRequests(uniqueAttributeValueRequests);
      updatedProductItemRequests.add(productItemCreationRequest);
    }
    productCreationRequest.setProductItemRequests(updatedProductItemRequests);
    if (validateItemAndProductAttributeValue) {
      validateProductAndItemVariantCreationValues(productCreationRequest);
    }
    if (variantRestrictedValuesFlag) {
      checkAttributeValueContainsRestrictedValues(productCreationRequest, variantRestrictedValues);
    }
    for (ProductItemCreationRequest productItemCreationRequest : productCreationRequest.getProductItemRequests()) {
      if (StringUtils.isNotEmpty(productItemCreationRequest.getMerchantSku()) && productItemCreationRequest.getMerchantSku().length() > 255) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.MAXIMUM_SELLER_SKU_EXCEEDED.getDesc());
      }
    }
  }

  private static void checkAttributeValueContainsRestrictedValues(ProductCreationRequest productCreationRequest,
      List<String> variantRestrictedValues) {
    if (CollectionUtils.isNotEmpty(variantRestrictedValues)) {
      for (ProductItemCreationRequest productItemCreationRequest : productCreationRequest.getProductItemRequests()) {
        if (MapUtils.isNotEmpty(productItemCreationRequest.getAttributesMap())) {
          for (Map.Entry<String, String> itemAttributeValue : productItemCreationRequest.getAttributesMap()
              .entrySet()) {
            if (variantRestrictedValues.contains(itemAttributeValue.getValue().toLowerCase())) {
              log.error("Error while validating the item attribute value : {} for product code : {} ",
                  itemAttributeValue, productCreationRequest.getProductCode());
              throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
                  ApiErrorCode.ITEM_ATTRIBUTE_VALUE_INVALID.getDesc());
            }
          }
        }
      }
    }
  }

  public static void validateProductAndItemVariantCreationValues(ProductCreationRequest productCreationRequest) {
    Map<String, List<String>> attributeCodeAndValues = new HashMap<>();
    for (ProductAttributeRequest productAttributeRequest : productCreationRequest.getProductAttributes()) {
      List<String> listOfValues =
          attributeCodeAndValues.getOrDefault(productAttributeRequest.getAttribute().getAttributeCode(),
              new ArrayList<>());
      if (AttributeType.DESCRIPTIVE_ATTRIBUTE.equals(productAttributeRequest.getAttribute().getAttributeType())) {
        productAttributeRequest.getProductAttributeValues().stream()
            .map(ProductAttributeValueRequest::getDescriptiveAttributeValue).forEach(listOfValues::add);
      } else if (AttributeType.DEFINING_ATTRIBUTE.equals(productAttributeRequest.getAttribute().getAttributeType())) {
        productAttributeRequest.getProductAttributeValues().stream()
            .map(productAttributeValueRequest -> productAttributeValueRequest.getAllowedAttributeValue().getValue())
            .forEach(listOfValues::add);
      }
      attributeCodeAndValues.put(productAttributeRequest.getAttribute().getAttributeCode(), listOfValues);
    }
    validateItemValuesWithProductValues(productCreationRequest, attributeCodeAndValues);
  }

  private static void validateItemValuesWithProductValues(ProductCreationRequest productCreationRequest,
      Map<String, List<String>> attributeCodeAndValues) {
    for (ProductItemCreationRequest productItemCreationRequest : productCreationRequest.getProductItemRequests()) {
      if (MapUtils.isNotEmpty(productItemCreationRequest.getAttributesMap())) {
        for (Map.Entry<String, String> itemAttributeValue : productItemCreationRequest.getAttributesMap().entrySet()) {
          if (!attributeCodeAndValues.containsKey(itemAttributeValue.getKey())) {
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
                ApiErrorCode.ITEM_ATTRIBUTE_VALUE_INVALID.getDesc());
          }
          List<String> valuesAtProductLevel = attributeCodeAndValues.get(itemAttributeValue.getKey());
          if (!valuesAtProductLevel.contains(itemAttributeValue.getValue())) {
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
                ApiErrorCode.ITEM_ATTRIBUTE_VALUE_INVALID.getDesc());
          }
        }
      }
    }
  }

  private static List<ProductItemAttributeValueRequest> uniqueProductItemAttributeValueRequests(
    List<ProductItemAttributeValueRequest> productItemAttributeValueRequests) {
    Map<String, ProductItemAttributeValueRequest> uniqueAttributesMap = new LinkedHashMap<>();
    List<ProductItemAttributeValueRequest> uniqueAttributesList = new ArrayList<>();
    if(CollectionUtils.isNotEmpty(productItemAttributeValueRequests)) {
      for (ProductItemAttributeValueRequest productItemAttributeValueRequest : productItemAttributeValueRequests) {
        String attributeCode = productItemAttributeValueRequest.getAttribute().getAttributeCode();
        uniqueAttributesMap.putIfAbsent(attributeCode, productItemAttributeValueRequest);
      }

      uniqueAttributesList.addAll(uniqueAttributesMap.values());
      productItemAttributeValueRequests.clear();
      productItemAttributeValueRequests.addAll(uniqueAttributesList);
    }
    return uniqueAttributesList;
  }


  public static void validateFreeSample(ProductCreationRequest productCreationRequest) {
    if (productCreationRequest.isFreeSample() && (productCreationRequest.isOff2OnChannelActive() || (
        Objects.nonNull(productCreationRequest.getPreOrder()) && Boolean.TRUE
            .equals(productCreationRequest.getPreOrder().getIsPreOrder())))) {
      log.error("Error creation product {} as free sample product has pre order : {} ",
          productCreationRequest.getProductCode());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.FREE_SAMPLE_CANNOT_BE_SET.getDesc());
    }

    if (productCreationRequest.isFreeSample()) {
      for (ProductItemCreationRequest productItemCreationRequest : productCreationRequest.getProductItemRequests()) {
        if (productItemCreationRequest.getPickupPoints().stream().anyMatch(PickupPointCreateRequest::isCncActive)) {
          log.error("Free sample and cnc can not be true at the same time {} ", productCreationRequest.getProductCode());
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.FREE_SAMPLE_CANNOT_BE_TRUE);
        }
        if (productItemCreationRequest.getPickupPoints().stream().anyMatch(PickupPointCreateRequest::isBuyable)
            || productItemCreationRequest.getPickupPoints().stream().anyMatch(PickupPointCreateRequest::isDisplay)) {
          log.error("Error updating free sample product : {} as flags are true for product",
              productCreationRequest.getProductCode());
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              ErrorMessages.FREE_SAMPLE_MUST_NOT_BE_BUYABLE_OR_DISCOVERABLE);
        }
      }
    }
  }

  public static ApiErrorCode validateCncWithBopis(ProductCreationRequest productCreationRequest,
    boolean bopisCNCRestrictionEnabled) {
    if(bopisCNCRestrictionEnabled) {
      Integer productType = Objects.nonNull(productCreationRequest.getProductType()) ?
              productCreationRequest.getProductType() :
              (productCreationRequest.getProductItemRequests().stream().map(ProductItemCreationRequest::getProductType)
                      .filter(Objects::nonNull).findFirst().orElse(null));
      if (ProductType.BOPIS.getProductType().equals(productType)) {
        for (ProductItemCreationRequest productItemCreationRequest : productCreationRequest.getProductItemRequests()) {
          if (productItemCreationRequest.getPickupPoints().stream().anyMatch(PickupPointCreateRequest::isCncActive)) {
            log.error("CNC can not be true with bopis product {} ",
                    productCreationRequest.getProductCode());
            return ApiErrorCode.BOPIS_CNC_CHANGE_ERROR;

          }
        }
      }
    }
    return null;
  }

  private static boolean isWarnaPresent(ProductCreationRequest productCreationRequest) {
    return productCreationRequest.getProductAttributes().stream()
        .anyMatch(productAttributeRequest -> Constants.WARNA.equals(productAttributeRequest.getProductAttributeName()));
  }

  public static boolean validateImageExtension(ProductRequest productRequest, List<String> validImageExtension,
      boolean checkOnlyNewImagesForExtension) {
    Stream<String> productImages = Stream.empty();
    Stream<String> productItemImages = Stream.empty();
    Stream<String> productItemCreationImages = Stream.empty();
    if (CollectionUtils.isNotEmpty(productRequest.getImages())) {
      if (checkOnlyNewImagesForExtension) {
        productImages = productRequest.getImages().stream()
            .filter(image -> !image.isMarkForDelete() && Constants.NEW.equals(image.getReviewType()))
            .map(Image::getLocationPath);
      } else {
        productImages =
            productRequest.getImages().stream().filter(image -> !image.isMarkForDelete()).map(Image::getLocationPath);
      }
    }
    if (CollectionUtils.isNotEmpty(productRequest.getProductItems())) {
      if (checkOnlyNewImagesForExtension) {
        productItemImages =
            productRequest.getProductItems().stream().filter(item -> CollectionUtils.isNotEmpty(item.getImages()))
                .flatMap(item -> item.getImages().stream()
                    .filter(image -> !image.isMarkForDelete() && Constants.NEW.equals(image.getReviewType())))
                .map(Image::getLocationPath);
      } else {
        productItemImages =
            productRequest.getProductItems().stream().filter(item -> CollectionUtils.isNotEmpty(item.getImages()))
                .flatMap(item -> item.getImages().stream().filter(image -> !image.isMarkForDelete()))
                .map(Image::getLocationPath);
      }
    }
    if (ProductCreationRequest.class == productRequest.getClass()) {
      ProductCreationRequest productCreationRequest = (ProductCreationRequest) productRequest;
      if (CollectionUtils.isNotEmpty(productCreationRequest.getProductItemRequests())) {
        if (checkOnlyNewImagesForExtension) {
          productItemCreationImages = productCreationRequest.getProductItemRequests().stream()
              .filter(item -> CollectionUtils.isNotEmpty(item.getImages())).flatMap(item -> item.getImages().stream()
                  .filter(image -> !image.isMarkForDelete() && Constants.NEW.equals(image.getReviewType())))
              .map(Image::getLocationPath);
        } else {
          productItemCreationImages = productCreationRequest.getProductItemRequests().stream()
              .filter(item -> CollectionUtils.isNotEmpty(item.getImages()))
              .flatMap(item -> item.getImages().stream().filter(image -> !image.isMarkForDelete()))
              .map(Image::getLocationPath);
        }
      }
    }
    List<String> images =
        Stream.concat(Stream.concat(productImages, productItemImages), productItemCreationImages).distinct()
            .collect(Collectors.toList());
    boolean hasValidImageExtension = false;
    if (CollectionUtils.isNotEmpty(images)) {
      hasValidImageExtension = images.stream()
          .filter(locationPath -> StringUtils.isNotEmpty(locationPath) && -1 != locationPath.lastIndexOf(Constants.DOT))
          .map(locationPath -> locationPath.substring(locationPath.lastIndexOf(Constants.DOT), locationPath.length()))
          .map(String::toLowerCase).anyMatch(extension -> !validImageExtension.contains(extension));
    }
    return !hasValidImageExtension;
  }

  public static void validateImagePredictionAndCategoryMappingRequest(
      ProductImagePredictionAndCategoryMappingRequest request) {
    checkArgument(StringUtils.isNotBlank(request.getPredictionType()), ErrorMessages.PREDICTION_TYPE_MUST_NOT_BE_BLANK);
    checkArgument(Objects.nonNull(request.getCategoryMappings()), ErrorMessages.CATEGORY_MAPPINGS_MUST_NOT_BE_NULL);
    for (PredictionCategoryMappingUpdateRequest predictionCategoryMappingRequest : request.getCategoryMappings()) {
      checkArgument(StringUtils.isNotBlank(predictionCategoryMappingRequest.getCategoryCode()),
          ErrorMessages.CATEGORY_CODE_MUST_NOT_BE_BLANK);
    }
  }

  public static boolean checkIfProductIsMPP(ProductCreationRequest productCreationRequest) {
    return productCreationRequest.getProductItemRequests().stream()
        .filter(productItemCreationRequest -> CollectionUtils.isNotEmpty(productItemCreationRequest.getPickupPoints()))
        .anyMatch(productItemCreationRequest -> productItemCreationRequest.getPickupPoints().size() > 1);
  }

  public static void validateDescriptiveFieldsForProductCreation(ProductCreationRequest request,
    boolean sanitizeProductName, boolean sanitiseAttributeValues) {
    request.setDescription(
        validateDescription(new String(request.getDescription(), StandardCharsets.UTF_8)).getBytes());
    request.setLongDescription(
        validateDescription(new String(request.getLongDescription(), StandardCharsets.UTF_8)).getBytes());
    request.setUniqueSellingPoint(validateData(request.getUniqueSellingPoint()));
    request.setSpecificationDetail(validateData(request.getSpecificationDetail()));
    if (sanitizeProductName) {
      request.setName(validateDataForProductName(request.getName()));
    } else {
      request.setName(validateData(request.getName()));
    }
    request.setUom(validateData(request.getUom()));
    request.setProductStory(validateData(request.getProductStory()));
    request.setUrl(validateData(request.getUrl()));
    if (CollectionUtils.isNotEmpty(request.getProductAttributes())) {
      for (ProductAttributeRequest productAttributeRequest : request.getProductAttributes()) {
        if (AttributeType.DESCRIPTIVE_ATTRIBUTE.equals(productAttributeRequest.getAttribute().getAttributeType())
            && CollectionUtils.isNotEmpty(productAttributeRequest.getProductAttributeValues())) {
          productAttributeRequest.getProductAttributeValues().get(0).setDescriptiveAttributeValue(
              validateData(productAttributeRequest.getProductAttributeValues().get(0).getDescriptiveAttributeValue()));
        }
      }
    }
    for (ProductItemCreationRequest productItemCreationRequest : request.getProductItemRequests()) {
      if (CollectionUtils.isNotEmpty(productItemCreationRequest.getProductItemAttributeValueRequests())) {
        for (ProductItemAttributeValueRequest productItemAttributeValueRequest : productItemCreationRequest.getProductItemAttributeValueRequests()) {
          if (AttributeType.DESCRIPTIVE_ATTRIBUTE.equals(
              productItemAttributeValueRequest.getAttribute().getAttributeType())) {
            productItemAttributeValueRequest.setValue(validateData(productItemAttributeValueRequest.getValue()));
          }
        }
      }
      if (sanitiseAttributeValues) {
        for (ProductAttributeRequest productAttributeRequest : request.getProductAttributes()) {
          if (AttributeType.DESCRIPTIVE_ATTRIBUTE.equals(productAttributeRequest.getAttribute().getAttributeType())) {
            productAttributeRequest.getProductAttributeValues().stream().filter(Objects::nonNull).forEach(
                attributesValues -> attributesValues.setDescriptiveAttributeValue(
                    StringUtils.normalizeSpace(attributesValues.getDescriptiveAttributeValue())));
          }
        }
        productItemCreationRequest.setAttributesMap(sanitizeAttributeValues(productItemCreationRequest));
      }
    }
  }

  private static TreeMap<String, String> sanitizeAttributeValues(
      ProductItemCreationRequest productItemCreationRequest) {
    TreeMap<String, String> attributeMap = new TreeMap<>();
    Optional.ofNullable(productItemCreationRequest.getAttributesMap()).ifPresent(attributesMap -> attributesMap.forEach(
        (key, value) -> attributeMap.put(key, StringUtils.normalizeSpace(value))));
    return attributeMap;
  }

  public static void validateDescriptiveFieldsForProductUpdate(ProductLevel3 request) {
    request.setDescription(validateDescription(request.getDescription()));
    request.setUniqueSellingPoint(validateData(request.getUniqueSellingPoint()));
    request.setSpecificationDetail(validateData(request.getSpecificationDetail()));
    request.setProductName(validateData(request.getProductName()));
    request.setProductStory(validateData(request.getProductStory()));
    request.setUrl(validateData(request.getUrl()));
    if (CollectionUtils.isNotEmpty(request.getAttributes())) {
      for (ProductLevel3Attribute productLevel3Attribute : request.getAttributes()) {
        if (AttributeType.DESCRIPTIVE_ATTRIBUTE.name().equals(productLevel3Attribute.getAttributeType())
            && CollectionUtils.isNotEmpty(productLevel3Attribute.getValues())) {
          productLevel3Attribute.getValues().set(0, validateData(productLevel3Attribute.getValues().get(0)));
        }
      }
    }
  }

  public static void validateProductNameAndDescriptionAndUrl(
    ProductMasterDataEditRequest productMasterDataEditRequest) {
    productMasterDataEditRequest.setDescription(
      validateDescription(productMasterDataEditRequest.getDescription()));
    productMasterDataEditRequest.setProductName(
      validateDataForProductName(productMasterDataEditRequest.getProductName()));
    productMasterDataEditRequest.setUrl(validateData(productMasterDataEditRequest.getUrl()));
  }

  private static String validateDescription(String request) {
    if (StringUtils.isBlank(request)) {
      return request;
    }
    if (request.contains(Constants.LT) || request.contains(Constants.GT) || request.contains(Constants.AMP)) {
      request = Jsoup.clean(StringEscapeUtils.unescapeHtml3(request), Constants.HTTPS,
          customWhitelist().addAttributes(Constants.P, Constants.STYLE).addAttributes(Constants.DIV, Constants.STYLE)
              .addAttributes(Constants.SPAN, Constants.STYLE));
      request = StringEscapeUtils.unescapeHtml3(request);
    } else {
      request = StringEscapeUtils.unescapeHtml3(Jsoup.clean(request, Constants.HTTPS,
          customWhitelist().addAttributes(Constants.P, Constants.STYLE).addAttributes(Constants.DIV, Constants.STYLE)
              .addAttributes(Constants.SPAN, Constants.STYLE)));
    }
    return request;
  }

  private static String validateData(String request) {
    if (StringUtils.isBlank(request)) {
      return request;
    }
    if (request.contains(Constants.LT) || request.contains(Constants.GT) || request.contains(Constants.AMP)) {
      request = Jsoup.clean(StringEscapeUtils.unescapeHtml3(request), Constants.HTTPS, customWhitelist());
      request = StringEscapeUtils.unescapeHtml3(request);
    } else {
      request = StringEscapeUtils.unescapeHtml3(Jsoup.clean(request, Constants.HTTPS, customWhitelist()));
    }
    return request;
  }

  public static String validateDataForProductName(String request) {
    if (StringUtils.isBlank(request)) {
      return request;
    }
    if (request.contains(Constants.LT) || request.contains(Constants.GT) || request.contains(
      Constants.AMP)) {
      request =
        Jsoup.clean(StringEscapeUtils.unescapeHtml3(request), Constants.HTTPS, Whitelist.none());
      request = StringEscapeUtils.unescapeHtml3(request);
    } else {
      request =
        StringEscapeUtils.unescapeHtml3(Jsoup.clean(request, Constants.HTTPS, Whitelist.none()));
    }
    return request;
  }

  private static Whitelist customWhitelist() {
    return Whitelist.relaxed().addTags(Constants.IFRAME)
        .addAttributes(Constants.IFRAME, Constants.ALIGN, Constants.ALT, Constants.HEIGHT, Constants.SRC,
            Constants.TITLE, Constants.WIDTH, Constants.ALLOW_FULL_SCREEN);
  }

  public static void validateDuplicateProductAttributes(ProductCreationRequest request) {
    List<ProductAttributeRequest> uniqueAttributes = new ArrayList<>(
      Optional.ofNullable(request.getProductAttributes()).orElse(new ArrayList<>()).stream()
        .filter(Objects::nonNull).collect(
          Collectors.toMap(attribute -> attribute.getAttribute().getAttributeCode(),
            attribute -> attribute, (attribute1, attribute2) -> attribute1)).values());

    request.setProductAttributes(uniqueAttributes);
  }

  public static ApiErrorCode validateShippingAndDimension(ProductCreationRequest request,
    int maxProductDimensionLimit, boolean validateProductType, boolean instore2FlowSwitch) {
    ApiErrorCode apiErrorCode = null;
    if (validateProductType && Optional.of(request).map(ProductCreationRequest::getProductItemRequests)
        .orElseGet(ArrayList::new).stream().map(ProductItemCreationRequest::getProductType).anyMatch(Objects::isNull)) {
      log.error("product Type is null of productCode : {} ", request.getProductCode());
      return ApiErrorCode.PRODUCT_TYPE_NULL;
    }
    if (Optional.of(request).map(ProductCreationRequest::getProductType)
      .filter(ValidationUtil::isProductTypeValidAndRequiresShippingWeight).isPresent()) {
      apiErrorCode = performShippingAndDimensionsValidations(request, maxProductDimensionLimit, instore2FlowSwitch);
    } else {
      Optional<ApiErrorCode> itemApiErrorCode =
        Optional.of(request).map(ProductCreationRequest::getProductItemRequests)
          .orElseGet(ArrayList::new).stream().map(ProductItemCreationRequest::getProductType)
          .filter(ValidationUtil::isProductTypeValidAndRequiresShippingWeight).findFirst()
          .map(productType -> performShippingAndDimensionsValidations(request,
            maxProductDimensionLimit, instore2FlowSwitch));
      if (itemApiErrorCode.isPresent()) {
        log.error(
          "Validation failed for product creation for request : {} due to Invalid Dimensions ",
          request.getProductItemRequests());
        apiErrorCode = itemApiErrorCode.get();
      }
    }
    return apiErrorCode;
  }

  public static boolean isProductTypeValidAndRequiresShippingWeight(Integer productType) {
    if(Objects.isNull(productType)){
      return false;
    }
    return (productType.compareTo(1) == 0 || productType.compareTo(2) == 0);
  }

  private static ApiErrorCode performShippingAndDimensionsValidations(
    ProductCreationRequest productCreationRequest, int maxProductDimensionLimit,
    boolean instore2FlowSwitch) {
    productCreationRequest.setShippingWeight(validateAndSetDimensionFieldValues(SHIPPING_WEIGHT,
      productCreationRequest.getShippingWeight()));
    productCreationRequest.setWeight(
      validateAndSetDimensionFieldValues(WEIGHT, productCreationRequest.getWeight()));
    productCreationRequest.setHeight(
      validateAndSetDimensionFieldValues(HEIGHT, productCreationRequest.getHeight()));
    productCreationRequest.setLength(
      validateAndSetDimensionFieldValues(LENGTH, productCreationRequest.getLength()));
    productCreationRequest.setWidth(
      validateAndSetDimensionFieldValues(WIDTH, productCreationRequest.getWidth()));
    double weightInKgs = productCreationRequest.getWeight() / GMS_TO_KG_FACTOR;
    return validateMinAndMaxDimension(productCreationRequest.getProductCode(), maxProductDimensionLimit,
      weightInKgs,
        productCreationRequest.getLength(), productCreationRequest.getWidth(), productCreationRequest.getHeight(),
        productCreationRequest.getWeight(), productCreationRequest.getShippingWeight(),
      productCreationRequest.isOff2OnChannelActive(), productCreationRequest.isB2cActivated(), instore2FlowSwitch);
  }

  public static ApiErrorCode validateMinAndMaxDimension(
    String productCode, int maxProductDimensionLimit, double weightInKgs,
    Double length, Double width, Double height, Double weight, Double shippingWeight,
    boolean off2OnChannelActive, Boolean b2cActivated, boolean instore2FlowSwitch) {
    if (hasZeroDimension(length, width, height, weight, shippingWeight)
      && !CommonUtils.isPureInstoreProduct(off2OnChannelActive, b2cActivated, instore2FlowSwitch)) {
      log.error(
        "Validation failed for product creation for product : {} due to Invalid Dimensions ",
        productCode);
      return ApiErrorCode.DIMENSION_LESS_THAN_ZERO;
    } else if (hasExceededDimensionLimit(maxProductDimensionLimit, length, width, height,
      weightInKgs)) {
      log.error(
        "Validation failed for product creation for product : {} due to Dimensions exceeded ",
        productCode);
      ApiErrorCode apiErrorCode = ApiErrorCode.DIMENSION_EXCEEDED_THRESHOLD;
      apiErrorCode.setConfiguredValue(maxProductDimensionLimit);
      return apiErrorCode;
    }
    return null;
  }

  private static boolean hasZeroDimension(double... dimensions) {
    for (double dimension : dimensions) {
      if (dimension == 0) {
        return true;
      }
    }
    return false;
  }

  private static boolean hasExceededDimensionLimit(double limit, double... dimensions) {
    for (double dimension : dimensions) {
      if (dimension > limit) {
        return true;
      }
    }
    return false;
  }


  private static double validateAndSetDimensionFieldValues(String fieldName, Double value) {
    double roundedValue = Math.round(value * 1000.0) / 1000.0;
    if (roundedValue <= 0) {
      value = 0.0;
    } else {
      value = roundedValue;
    }
    if (Double.compare(value, 0.0) <= 0) {
      return 0;
    }
    return value;
  }


  public static List<String> validateB2cAndB2bActivatedFlags(ProductCreationRequest request,
      ProfileResponse profileResponse, boolean instoreNewFlowEnabled,
      boolean setDefaultB2CActivated) {
    List<String> salesChannel =
        validateEligibilityForB2cAndB2b(request, profileResponse, instoreNewFlowEnabled,
            setDefaultB2CActivated);
    for (ProductItemCreationRequest productItemCreationRequest : request.getProductItemRequests()) {
      for (PickupPointCreateRequest pickupPointCreateRequest : productItemCreationRequest.getPickupPoints()) {
        if (pickupPointCreateRequest.isBuyable() || pickupPointCreateRequest.isDisplay() || pickupPointCreateRequest.isCncActive()) {
          request.setB2cActivated(true);
        }
        if (Objects.nonNull(pickupPointCreateRequest.getB2bFields()) && (pickupPointCreateRequest.getB2bFields().isBuyable()
            || pickupPointCreateRequest.getB2bFields().isDisplay())) {
          request.setB2bActivated(true);
        }
      }
    }
    if (!request.isB2cActivated()) {
      request.setOnline(false);
    }
    if (!instoreNewFlowEnabled && request.isOff2OnChannelActive()) {
      request.setB2cActivated(true);
    }
    return salesChannel;
  }

  private static List<String> validateEligibilityForB2cAndB2b(ProductCreationRequest request, ProfileResponse profileResponse,
      boolean instoreNewFlowEnabled, boolean setDefaultB2CActivated) {
    List<String> salesChannel = CommonUtils.salesChannelFromProfileResponse(profileResponse);
    if (!salesChannel.contains(Constants.B2C_SELLER_CHANNEL)) {
      request.setB2cActivated(false);
      for (ProductItemCreationRequest productItemCreationRequest : request.getProductItemRequests()) {
        for (PickupPointCreateRequest pickupPointCreateRequest : productItemCreationRequest.getPickupPoints()) {
          pickupPointCreateRequest.setBuyable(false);
          pickupPointCreateRequest.setDisplay(false);
          pickupPointCreateRequest.setCncActive(false);
          }
        }
      }
    if (!salesChannel.contains(Constants.B2B_SELLER_CHANNEL)) {
      request.setB2bActivated(false);
      for (ProductItemCreationRequest productItemCreationRequest : request.getProductItemRequests()) {
        for (PickupPointCreateRequest pickupPointCreateRequest : productItemCreationRequest.getPickupPoints()) {
          pickupPointCreateRequest.setB2bFields(null);
        }
      }
    }
    validateEligibilityForB2cAndB2bInCaseOfOnlyB2bOrB2cSellers(request, salesChannel,
        instoreNewFlowEnabled, setDefaultB2CActivated,
        Optional.ofNullable(profileResponse).map(ProfileResponse::getCompany)
            .map(CompanyDTO::isOfflineToOnlineFlag).orElse(false));
    return salesChannel;
  }

  private static void validateEligibilityForB2cAndB2bInCaseOfOnlyB2bOrB2cSellers(
      ProductCreationRequest request, List<String> salesChannel, boolean instoreNewFlowEnabled,
      boolean setDefaultB2CActivated, boolean isInstoreSeller) {
    if (salesChannel.size() == Constants.SIZE_ONE) {
      if (salesChannel.contains(Constants.B2C_SELLER_CHANNEL)) {
        if (!instoreNewFlowEnabled) {
          request.setB2cActivated(true);
        }
        if(setDefaultB2CActivated && !isInstoreSeller) {
          request.setB2cActivated(true);
        }
      } else if (salesChannel.contains(Constants.B2B_SELLER_CHANNEL)) {
        request.setB2bActivated(true);
      }
    }
  }

  public static ApiErrorCode getValidationResponseForPreOrder(PreOrderRequest preOrderRequest, int preOrderMaximumDays,
      int preOrderMaximumWeek, boolean isSellerOmg, boolean preOrderQuotaSwitch) throws Exception {
    String preOrderType = preOrderRequest.getPreOrderType();
    int preOrderValue = 0;
    SimpleDateFormat PREORDER_DATE_FORMAT = new SimpleDateFormat("dd/MM/yyyy");
    Date currentDate = PREORDER_DATE_FORMAT.parse(PREORDER_DATE_FORMAT.format(new Date()));
    if (preOrderQuotaSwitch && isSellerOmg && !CommonUtils.validateOmgSellerAndPreOrderType(preOrderType)) {
      return ApiErrorCode.INVALID_PREORDER_TYPE_OMG;
    }
    switch (preOrderType) {
      case PRE_ORDER_DAY_TYPE:
        return validatePreOrderDaysType(preOrderValue, preOrderMaximumDays, preOrderRequest);
      case PREORDER_WEEK_TYPE:
        return validatePreOrderWeekType(preOrderValue, preOrderMaximumWeek, preOrderRequest);
      case PRE_ORDER_DATE_TYPE:
        return validatePreOrderDateType(preOrderRequest.getPreOrderDate(), preOrderMaximumDays,
          preOrderRequest, currentDate);
      default:
        return ApiErrorCode.INVALID_PREORDER_TYPE;
    }
  }

  private static ApiErrorCode validatePreOrderDaysType(int preOrderValue, int preOrderMaximumDays,
    PreOrderRequest preOrderRequest) {
    preOrderValue = preOrderRequest.getPreOrderValue();
    if (preOrderValue <= 0) {
      return ApiErrorCode.PREORDER_DAYS_LESS_THAN_ZERO;
    } else if (preOrderValue > preOrderMaximumDays) {
      ApiErrorCode apiErrorCode = ApiErrorCode.PREORDER_DAYS_EXCEEDED_LIMIT;
      apiErrorCode.setConfiguredValue(preOrderMaximumDays);
      return apiErrorCode;
    }
    setPreOrderDate(preOrderRequest, preOrderValue);
    return null;
  }

  private static ApiErrorCode validatePreOrderWeekType(int preOrderValue, int preOrderMaximumWeek,
    PreOrderRequest preOrderRequest) {
    preOrderValue = preOrderRequest.getPreOrderValue();
    int daysInOneWeek = Constants.DAYS_IN_ONE_WEEK;
    if (preOrderValue <= 0) {
      return ApiErrorCode.PREORDER_WEEK_LESS_THAN_ZERO;
    } else if (preOrderValue > preOrderMaximumWeek) {
      ApiErrorCode apiErrorCode = ApiErrorCode.PREORDER_WEEK_EXCEEDED_LIMIT;
      apiErrorCode.setConfiguredValue(preOrderMaximumWeek);
      return apiErrorCode;
    }
    setPreOrderDate(preOrderRequest, preOrderValue * daysInOneWeek);
    return null;
  }

  private static ApiErrorCode validatePreOrderDateType(Date preOrderDate, int preOrderMaximumDays,
    PreOrderRequest preOrderRequest, Date currentDate) {
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(currentDate);
    calendar.add(Calendar.DATE, preOrderMaximumDays);
    Date daysAddedDate = calendar.getTime();
    if (currentDate.equals(preOrderDate) || currentDate.after(preOrderDate)) {
      return ApiErrorCode.PREORDER_DATE_BEFORE_CURRENT_DATE;
    } else if (preOrderDate.after(daysAddedDate)) {
      ApiErrorCode apiErrorCode = ApiErrorCode.PREORDER_DAYS_EXCEEDED_LIMIT;
      apiErrorCode.setConfiguredValue(preOrderMaximumDays);
    }
    preOrderRequest.setPreOrderDate(preOrderDate);
    long totalDays =
      Math.abs(preOrderDate.getTime() - currentDate.getTime()) / Constants.TOTAL_HOURS;
    if (totalDays > preOrderMaximumDays) {
      ApiErrorCode apiErrorCode = ApiErrorCode.PREORDER_DATE_LIMIT_EXCEEDED;
      apiErrorCode.setConfiguredValue(preOrderMaximumDays);
      return apiErrorCode;
    }
    return null;
  }

  public static ApiErrorCode validateEligibilityForSchedulesAtL5(ProductL3UpdateRequest request,
    Map<String, Set<String>> requestsForSchedulesValidations, ProductL3Response savedProductData,
    boolean schedulesAddEditEnabled) {
    ApiErrorCode apiErrorCode = null;
    if (schedulesAddEditEnabled) {
      // Validate modified pickup points
      apiErrorCode = validateModifiedPickupPoints(request, requestsForSchedulesValidations, savedProductData);
      // If no error found, validate added pickup points
      if (Objects.isNull(apiErrorCode)) {
        apiErrorCode = validateAddedPickupPoints(request, requestsForSchedulesValidations);
      }
    }
    return apiErrorCode;
    
  }

  private static ApiErrorCode validateModifiedPickupPoints(ProductL3UpdateRequest request,
    Map<String, Set<String>> requestsForSchedulesValidations, ProductL3Response savedProductData) {
    for (ProductVariantPriceStockAndImagesRequest productVariant :
      Optional.ofNullable(request.getProductItems()).orElseGet(ArrayList::new)) {
      for (ItemPickupPointRequest modifiedL5 : productVariant.getModifiedItemPickupPoints()) {
        modifiedL5.setItemSku(productVariant.getItemSku());
        ApiErrorCode errorCode = fetchApiErrorCodeSchedulesAtL5(modifiedL5, requestsForSchedulesValidations, null);
        if (Objects.isNull(errorCode) && scheduleNotRemoved(modifiedL5, request)) {
          errorCode = validateScheduleEligibilityForFreeSampleAndOfflineProducts(request, savedProductData);
        }
        if (Objects.nonNull(errorCode)) {
          return errorCode;
        }
      }
    }
    return null;
  }

  private static ApiErrorCode validateAddedPickupPoints(ProductL3UpdateRequest request,
    Map<String, Set<String>> requestsForSchedulesValidations) {
    ApiErrorCode apiErrorCode = null;
    for (ItemPickupPointRequest addPickupPoint : request.getAddPickupPoints()) {
      apiErrorCode = fetchApiErrorCodeSchedulesAtL5(addPickupPoint, requestsForSchedulesValidations, apiErrorCode);
      if (Objects.nonNull(apiErrorCode)) {
        return apiErrorCode;
      }
    }
    return null;
  }


  private static boolean scheduleNotRemoved(ItemPickupPointRequest modifiedL5,
    ProductL3UpdateRequest request) {
    // Check if the request is a free sample or online
    if (request.isFreeSample() || Boolean.FALSE.equals(request.getOnline())) {
      // If either schedule is non-null, validation is needed
      if (Objects.nonNull(modifiedL5.getBuyableSchedule()) || Objects.nonNull(
        modifiedL5.getDiscoverableSchedule())) {
        return true;
      }
      // If schedule removal is true, validation is not needed
      if (Boolean.TRUE.equals(modifiedL5.isScheduleRemoval())) {
        return false;
      }
    }
    // no validation needed if request is not free sample or offline
    return false;
  }


  private static ApiErrorCode validateScheduleEligibilityForFreeSampleAndOfflineProducts(
    ProductL3UpdateRequest request, ProductL3Response savedProductData) {
    // Check if the product is a free sample in saved data or request
    if (Optional.ofNullable(savedProductData).map(ProductL3Response::isFreeSample).orElse(false)
      || request.isFreeSample()) {
      return ApiErrorCode.SCHEDULE_NOT_ALLOWED_FOR_FREE_SAMPLE;
    }
    // Check if the product is offline in the saved data or in the request
    if (Optional.ofNullable(savedProductData).filter(Predicate.not(ProductL3Response::isOnline))
      .isPresent()) {
      return ApiErrorCode.SCHEDULE_INVALID_FOR_OFFLINE_PRODUCT;
    } else if (Boolean.FALSE.equals(request.getOnline())) {
      return ApiErrorCode.SCHEDULE_INVALID_FOR_OFFLINE_PRODUCT;

    }
    return null;
  }

  private static ApiErrorCode fetchApiErrorCodeSchedulesAtL5(
    ItemPickupPointRequest itemPickupPointRequest, Map<String, Set<String>> requestsForSchedulesValidations,
    ApiErrorCode apiErrorCode) {
    if (Objects.nonNull(itemPickupPointRequest.getBuyableSchedule()) || Objects.nonNull(
      itemPickupPointRequest.getDiscoverableSchedule())) {
      // Validate the start and end dates of the schedules
      apiErrorCode = validateSchedulesStartAndEndDate(itemPickupPointRequest, requestsForSchedulesValidations);
    }
    return apiErrorCode;
  }

  private static ApiErrorCode validateSchedulesStartAndEndDate(
    ItemPickupPointRequest itemPickupPointRequest, Map<String, Set<String>> requestsForSchedulesValidations) {
    String itemSku =
      Optional.ofNullable(itemPickupPointRequest.getItemSku()).orElse(StringUtils.EMPTY);
    String offlineItemId =
      itemSku.concat(Constants.HYPHEN).concat(itemPickupPointRequest.getPickupPointId());
    boolean skipBuyableValidations = false, skipDiscoverableValidations = false;
    Date currentDate = new Date();
    Date startDateForBuyable = Optional.ofNullable(itemPickupPointRequest.getBuyableSchedule())
      .map(BuyableScheduleRequest::getStartDateTime).orElse(null);
    Date startDateForDiscoverable =
      Optional.ofNullable(itemPickupPointRequest.getDiscoverableSchedule())
        .map(DiscoverableScheduleRequest::getStartDateTime).orElse(null);
    Date endDateForBuyable = Optional.ofNullable(itemPickupPointRequest.getBuyableSchedule())
      .map(BuyableScheduleRequest::getEndDateTime).orElse(null);
    Date endDateForDiscoverable =
      Optional.ofNullable(itemPickupPointRequest.getDiscoverableSchedule())
        .map(DiscoverableScheduleRequest::getEndDateTime).orElse(null);
    if(requestsForSchedulesValidations.getOrDefault(Constants.BUYABLE_SCHEDULE_UPDATE,
      Collections.emptySet()).contains(offlineItemId)){
      // if schedules are not new and the end date is after current date ie schedule is active
      //start date can be before current date setting null locally to bypass validations
      skipBuyableValidations = true;
    }
    if(requestsForSchedulesValidations.getOrDefault(Constants.DISCOVERABLE_SCHEDULE_UPDATE,
      Collections.emptySet()).contains(offlineItemId)){
      skipDiscoverableValidations = true;
    }
    // Check if the schedule dates are incomplete
    if (incompleteSchedule(itemPickupPointRequest.getBuyableSchedule(), startDateForBuyable,
      endDateForBuyable, skipBuyableValidations) || incompleteSchedule(itemPickupPointRequest.getDiscoverableSchedule(),
      startDateForDiscoverable, endDateForDiscoverable, skipDiscoverableValidations)) {
      return ApiErrorCode.SCHEDULE_DATE_INCOMPLETE;
    }
    // Check if any start date is before the current date
    // Validate start dates only if not skipped else null before filter
    boolean startDateBeforeNow =
      Stream.of(skipDiscoverableValidations ? null : startDateForDiscoverable,
          skipBuyableValidations ? null : startDateForBuyable).filter(Objects::nonNull)
        .anyMatch(date -> date.before(currentDate));

    if (startDateBeforeNow) {
      return ApiErrorCode.INVALID_SCHEDULE_DATE_TIME;
    }
    // Validate that end dates are not before start dates
    return validateEndDatesNotBeforeStartDates(startDateForBuyable, endDateForBuyable,
      startDateForDiscoverable, endDateForDiscoverable, skipBuyableValidations, skipDiscoverableValidations);
  }

  private static boolean incompleteSchedule(Object schedule, Date startDate, Date endDate,
    boolean skipValidations) {
    if (Objects.isNull(schedule)) {
      return false;
    }
    // if schedule is not Null, start and date should be populated
    return (Objects.isNull(startDate) || Objects.isNull(endDate));
  }

  private static ApiErrorCode validateEndDatesNotBeforeStartDates(Date startDateForBuyable,
    Date endDateForBuyable, Date startDateForDiscoverable, Date endDateForDiscoverable,
    boolean skipBuyableValidations, boolean skipDiscoverableValidations) {
    Map<Date, Date> scheduleMap = new HashMap<>();
    if(!skipBuyableValidations) {
      //Add to map if validations are not skipped for existing valid schedules
      scheduleMap.put(startDateForBuyable, endDateForBuyable);
    }
    if(!skipDiscoverableValidations) {
      scheduleMap.put(startDateForDiscoverable, endDateForDiscoverable);
    }
    // Check if any end date is before its corresponding start date
    for (Map.Entry<Date, Date> entry : scheduleMap.entrySet()) {
      Date startDate = entry.getKey();
      Date endDate = entry.getValue();

      if (Objects.nonNull(startDate) && Objects.nonNull(endDate) && endDate.before(startDate)) {
        return ApiErrorCode.INVALID_SCHEDULE_DATE_TIME;
      }
    }
    return null;
  }

  public static void validateFormatAndDuplicateUpcCodes(ProductL3UpdateRequest request, List<Integer> eanUpcValidLength) {
    Set<String> upcCodesSet = new HashSet<>();
    if (CollectionUtils.isEmpty(request.getProductItems())) {
      return;
    }
    request.getProductItems().stream().map(ProductVariantPriceStockAndImagesRequest::getUpcCode)
        .filter(StringUtils::isNotBlank).forEach(upcCode -> {
          if (!upcCodesSet.add(upcCode)) {
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.UPC_CODE_UPDATE_FAILED.getDesc());
          }
          if (!BarcodeGenerator.isValidUPCCode(upcCode, eanUpcValidLength)) {
            throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, INVALID_EAN_UPC_FORMAT);
          }
        });
  }

  public static void validateDistinctProductAttributeValues(ProductCreationRequest request,
      String sizeChartValueTypeDelimiter) {
    for (ProductAttributeRequest productAttributeRequest : Optional.ofNullable(request.getProductAttributes())
        .orElse(new ArrayList<>())) {
      if (isDefiningAttributeAndProductAttributeValuesPresent(productAttributeRequest)) {
        validateDistinctProductDefiningattributeValues(sizeChartValueTypeDelimiter, productAttributeRequest);
      }
    }
  }

  private static boolean isDefiningAttributeAndProductAttributeValuesPresent(
      ProductAttributeRequest productAttributeRequest) {
    return Objects.nonNull(productAttributeRequest.getAttribute()) && AttributeType.DEFINING_ATTRIBUTE.equals(
        productAttributeRequest.getAttribute().getAttributeType()) && CollectionUtils.isNotEmpty(
        productAttributeRequest.getProductAttributeValues());
  }

  private static void validateDistinctProductDefiningattributeValues(String sizeChartValueTypeDelimiter,
      ProductAttributeRequest productAttributeRequest) {
    List<String> attributeValues = addProductAllowedAttributeValues(productAttributeRequest);
    long originalSize = attributeValues.size();
    long sizeAfterSanitization =
        attributeValues.stream().map(value -> sanitizeAllowedAttributeValue(value, sizeChartValueTypeDelimiter))
            .distinct().count();
    checkArgument(originalSize == sizeAfterSanitization, ErrorMessages.DUPLICATE_DEFINING_ATTRIBUTE_VALUE_PRESENT);
  }

  private static List<String> addProductAllowedAttributeValues(ProductAttributeRequest productAttributeRequest) {
    List<String> attributeValues = new ArrayList<>();
    for (ProductAttributeValueRequest productAttributeValueRequest : productAttributeRequest.getProductAttributeValues()) {
      if (Objects.nonNull(productAttributeValueRequest.getAllowedAttributeValue())) {
        attributeValues.add(productAttributeValueRequest.getAllowedAttributeValue().getValue());
      }
    }
    return attributeValues;
  }

  private static String sanitizeAllowedAttributeValue(String value, String sizeChartValueTypeDelimiter) {
    String[] tokens = value.split(sizeChartValueTypeDelimiter);
    return tokens[tokens.length - 1];
  }

  public static boolean performValidationOnDescriptionAndShipping(ProfileResponse profileResponse,
      boolean instoreNewFlowEnabled, Boolean offOn2ChannelActive, Boolean b2cActivated,
      List<ProductItemCreationRequest> productItemRequests) {
    boolean performValidationOnDescriptionAndShipping = true;
    if (checkIfProductIsPureInStoreProduct(profileResponse, instoreNewFlowEnabled, offOn2ChannelActive, b2cActivated)) {
      if (CollectionUtils.isNotEmpty(productItemRequests)) {
        boolean onlineOrMissingL5 = false;
        for (ProductItemCreationRequest productItemCreationRequest : productItemRequests) {
          if (CollectionUtils.isEmpty(productItemCreationRequest.getPickupPoints())) {
            onlineOrMissingL5 = true;
            break;
          } else {
            onlineOrMissingL5 = isOnlineOrMissingL5(productItemCreationRequest, onlineOrMissingL5);
          }
        }
        performValidationOnDescriptionAndShipping =
            isPerformValidationOnDescriptionAndShipping(onlineOrMissingL5, performValidationOnDescriptionAndShipping);
      }
    }
    return performValidationOnDescriptionAndShipping;
  }

  private static boolean isPerformValidationOnDescriptionAndShipping(boolean onlineOrMissingL5,
      boolean performValidationOnDescriptionAndShipping) {
    return onlineOrMissingL5 && performValidationOnDescriptionAndShipping;
  }

  private static boolean isOnlineOrMissingL5(ProductItemCreationRequest productItemCreationRequest,
      boolean onlineOrMissingL5) {
    if (productItemCreationRequest.getPickupPoints().stream().anyMatch(
        pickupPointCreateRequest -> pickupPointCreateRequest.isBuyable()
            || pickupPointCreateRequest.isDisplay())) {
      onlineOrMissingL5 = true;
    }
    return onlineOrMissingL5;
  }

  private static boolean checkIfProductIsPureInStoreProduct(ProfileResponse profileResponse, boolean instoreNewFlowEnabled,
      Boolean offOn2ChannelActive, Boolean b2cActivated) {
    return instoreNewFlowEnabled && Optional.ofNullable(profileResponse).map(ProfileResponse::getCompany).map(
        CompanyDTO::isOfflineToOnlineFlag).orElse(false) && Boolean.TRUE.equals(offOn2ChannelActive)
        && Boolean.FALSE.equals(b2cActivated);
  }

  public static void updateEmptyDescriptionForPureInStoreProduct(ProductRequest request,
      boolean performValidationOnDescriptionAndShipping) {
    if (Objects.isNull(request.getDescription()) || Objects.isNull(request.getLongDescription())) {
      if (!performValidationOnDescriptionAndShipping) {
        log.info("Setting description to empty for pure in-store product : {} ", request.getProductCode());
        request.setDescription(StringUtils.EMPTY.getBytes());
        request.setLongDescription(StringUtils.EMPTY.getBytes());
      }
    }
  }

  public static void updateNullDimensionsForPureInStoreProduct(boolean isAnyValueNull, ProductRequest request,
      boolean performValidationOnDescriptionAndShipping) {
    if (!performValidationOnDescriptionAndShipping) {
      if (isAnyValueNull) {
        request.setLength(DEFAULT_DIMENSION_VALUES);
        request.setWidth(DEFAULT_DIMENSION_VALUES);
        request.setHeight(DEFAULT_DIMENSION_VALUES);
        request.setWeight(DEFAULT_DIMENSION_VALUES);
        request.setShippingWeight(DEFAULT_DIMENSION_VALUES);
      }
    }
  }

  public static boolean isAnyValueNull(Double... values) {
    return Arrays.stream(values).anyMatch(Objects::isNull);
  }

  public static void validateWareHouseStockAvailabilityForPPDeletion(
    ProfileResponse profileResponse, List<String> itemsInEligibleForDeletion,
    List<InventoryDetailInfoResponseV2DTO> inventoryResponses,
    List<List<PickupPointDeleteRequest>> partitionPickupPointInventoryRequests) {
    int countPPNotEligibleForDeletion = 0;
    for (InventoryDetailInfoResponseV2DTO inventoryDetailInfoResponseDTO : inventoryResponses) {
      String itemSku = inventoryDetailInfoResponseDTO.getWebItemSku();
      int warehouseAvailable = getWarehouseAvailableStockValue(inventoryDetailInfoResponseDTO);
      if (warehouseAvailable > 0) {
        // distribution + non distribution
        itemsInEligibleForDeletion.add(itemSku);
        log.info("Found Warehouse stock linked to item : {} intended for Deletion ", itemSku);
        countPPNotEligibleForDeletion++;
      }
    }
    if (countPPNotEligibleForDeletion > 0) {
      long pickupPointDeleteRequestCount =
        partitionPickupPointInventoryRequests.stream().flatMap(List::stream)
          .map(PickupPointDeleteRequest::getPickupPointId).distinct().count();
      log.info("PP Deletion failed for Seller : {} on finding Stock in warehouse for : {} ",
        profileResponse.getBusinessPartnerCode(), itemsInEligibleForDeletion);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        String.format(ErrorMessages.PP_DELETE_REQUEST_INVALID_FOR_WAREHOUSE_STOCK,
          countPPNotEligibleForDeletion, pickupPointDeleteRequestCount));
    }
  }

  private static int getWarehouseAvailableStockValue(
    InventoryDetailInfoResponseV2DTO inventoryDetailInfoResponseDTO) {
    int warehouseAvailable = 0;
    // get stock available in distribution WH
    warehouseAvailable += Optional.ofNullable(inventoryDetailInfoResponseDTO)
      .map(InventoryDetailInfoResponseV2DTO::getWarehouseInventoryResponseList)
      .orElse(Collections.emptyList()).stream()
      .mapToInt(WarehouseInventoryResponseDTO::getAvailableStock).sum();
    //  stock available in non distribution WH added with stock in distribution
    warehouseAvailable += Optional.ofNullable(inventoryDetailInfoResponseDTO)
      .map(InventoryDetailInfoResponseV2DTO::getNonDistributionWarehouseInventoryResponseList)
      .orElse(Collections.emptyList()).stream()
      .mapToInt(WarehouseInventoryResponseDTO::getAvailableStock).sum();
    return warehouseAvailable;
  }

  public static void validateWareHouseStockAvailabilityForL2Deletion(
    ProfileResponse profileResponse,
    List<L2StockAvailabilityDTO> inventoryStockAvailabilityByItemCodes,
    List<String> deletedItemCodes) {
    //Check if stock is present at Distribution or non distribution warehouse
    List<String> itemCodesWithStockInInventory = inventoryStockAvailabilityByItemCodes.stream()
      .filter(l2StockAvailabilityDTO -> l2StockAvailabilityDTO.isDistributionWarehouseAvailable()
        || l2StockAvailabilityDTO.isNonDistributionWarehouseAvailable())
      .map(L2StockAvailabilityDTO::getWarehouseItemSku).collect(Collectors.toList());
    if (CollectionUtils.isNotEmpty(itemCodesWithStockInInventory)) {
      log.info("Variant Deletion failed for Seller : {} on finding Stock in L2's for : {} ",
        profileResponse.getBusinessPartnerCode(), itemCodesWithStockInInventory);
      ApiErrorCode apiErrorCode = ApiErrorCode.VARIANT_DELETE_REQUEST_INVALID_FOR_WAREHOUSE_STOCK;
      throw new ApiIncorrectInputDataException(String.format(ErrorMessages.VARIANT_DELETE_REQUEST_INVALID_FOR_WAREHOUSE_STOCK,
        itemCodesWithStockInInventory.size(), deletedItemCodes.size()), apiErrorCode);
    }
  }

  public static <T extends MasterDataUpdateRequest> void validateProductNameAndDescription(
    int productNameCharacterLimit, T request) {

    GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getProductName()),
      ErrorMessages.PRODUCT_NAME_MUST_NOT_BE_BLANK);

    GdnPreconditions.checkArgument(request.getProductName().length() <= productNameCharacterLimit,
      ErrorMessages.PRODUCT_NAME_LTE_LIMIT);

    if (request.isPureInstoreProduct()) {
      if (StringUtils.isBlank(request.getDescription())) {
        request.setDescription(StringUtils.EMPTY);
      }
    } else {
      GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getDescription()),
        ErrorMessages.DESCRIPTION_MUST_NOT_BE_BLANK);
    }
  }

  public static ApiErrorCode validateProductTypeAndDimensionsUpdate(
    ProductMasterDataEditRequest request, MasterProductEditDTO masterDto) {

    boolean isBopisProduct = ProductType.BOPIS.getProductType().equals(request.getProductType());
    boolean isBigProduct =
      ProductType.BIG_PRODUCT.getProductType().equals(request.getProductType());
    // 1. Validate eligibility for PRODUCT_TYPE_UPDATE
    if (request.getMasterDataEditChangeTypes()
      .contains(L3InfoUpdateChangeType.PRODUCT_TYPE_UPDATE)) {
      ApiErrorCode bpBopisEligibilityError = validateSellerShippingEligibility(request, isBigProduct, isBopisProduct);
      if (bpBopisEligibilityError != null)
        return bpBopisEligibilityError;
    }
    // 2. Validate shipping dimensions and weights if applicable
    if (isProductTypeValidAndRequiresShippingWeight(request.getProductType())) {
      ApiErrorCode dimensionError = validationDimensionsThreshold(request, masterDto);
      if (dimensionError != null)
        return dimensionError;
    }
    // 3. Validate BOPIS eligibility for category
    if (isBopisProduct) {
      Optional<CategoryDetailResponse> categoryDetail =
        Optional.ofNullable(masterDto.getCategoryDetailResponse());
      if (categoryDetail.isPresent() && !categoryDetail.get().isBopisEligible()) {
        log.error("Product Category {} is not eligible for BOPIS Product Type: {}",
          categoryDetail.get().getCategoryCode(), request.getProductType());
        return ApiErrorCode.BP_BOPIS_ELIGIBILITY_ERROR;
      }
    }

    // 4. Validate BOPIS product type with CNC active at L5
    if(isBopisProduct && Boolean.TRUE.equals(masterDto.getCncActiveAtL5())){
        log.error("Product {} is BOPIS product type and has CNC active at L5, which is not allowed",
            request.getProductCode());
        return ApiErrorCode.BOPIS_CNC_CHANGE_ERROR;
    }
    return null;
  }

  private static ApiErrorCode validateSellerShippingEligibility(ProductMasterDataEditRequest request,
    boolean isBigProduct, boolean isBopisProduct) {
    boolean isBigProductNotEligible = isBigProduct && !request.sellerBigProductFlag;
    boolean isBopisNotEligible = isBopisProduct && !request.sellerBopisFlag;
    if (isBigProductNotEligible || isBopisNotEligible) {
      log.error("Seller {} is not eligible for the Product Type: {}",
        request.getBusinessPartnerCode(), request.getProductType());
      return ApiErrorCode.BP_BOPIS_ELIGIBILITY_ERROR;
    }
    return null;
  }

  private static ApiErrorCode validationDimensionsThreshold(ProductMasterDataEditRequest request,
    MasterProductEditDTO masterDto) {
    double weightInKgs = request.getWeight() / GMS_TO_KG_FACTOR;

    ApiErrorCode dimensionError =
      ValidationUtil.validateMinAndMaxDimension(request.getProductCode(),
        masterDto.getMaxProductDimensionLimit(), weightInKgs, request.getLength(),
        request.getWidth(), request.getHeight(), request.getWeight(), request.getShippingWeight(),
        request.isOff2OnChannelActive(), request.getB2cActivated(), true);
    if (Objects.nonNull(dimensionError)) {
      return dimensionError;
    }
    return null;
  }

}
