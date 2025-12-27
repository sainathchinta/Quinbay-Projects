package com.gdn.mta.bulk.util;

import static com.gdn.mta.bulk.BulkProcessValidationErrorMessages.SHIPPING_TYPE_INELIGIBLE;
import static com.gdn.mta.bulk.helper.ProductLevel3ProcessorValidationMessageHelper.errorMessage;
import static com.gdn.mta.bulk.helper.ProductLevel3ProcessorValidationMessageHelper.errorMessageBasedOnMerchant;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
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
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.B2bDetailsDTO;
import com.gda.mta.product.dto.BundleRecipeRequest;
import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerAttributeRequest;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.generator.GenerateShippingWeightRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.BulkUploadOption;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.ProductSalesChannelType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.ValidateExcelRowsRequest;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessImage;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.helper.ExcelTemplateUtil;
import com.gdn.mta.bulk.models.BulkErrorCategory;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.models.BulkUploadFileHeaders;
import com.gdn.mta.bulk.service.BulkCategoryProcessorServiceBean;
import com.gdn.mta.bulk.service.FileStorageService;
import com.gdn.mta.bulk.service.ProductLevel3GenericProcessorServiceBean;
import com.gdn.mta.bulk.service.util.ConverterUtil;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.partners.bulk.util.BulkCnCreationHeaderNames;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.StoreCopyConstants;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.campaign.dto.ItemDetailsDto;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.dto.ProductSpecialAttributeDTO;
import com.gdn.x.product.rest.web.model.response.ItemInfoResponseV2;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponseV2;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CatalogRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailAndShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProtectedBrandResponse;
import com.google.common.base.Splitter;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
public class BulkCreationCommonUtil {

  @Autowired
  private FileStorageService fileStorageService;

  @Value("${allowed.image.extensions}")
  private List<String> allowedImageExtensions;

  private BulkCreationCommonUtil() {
  }

  private static final int CN_BRAND_INDEX = 6;
  private static final double MINIMUM_VALUE = 0.0;
  private static String JPEG = "jpeg";
  private static String JPG = "jpg";
  private static String PNG= "png";
  private static String WEBP= "webp";
  public static final Pattern ITEM_SKU_PATTERN = Pattern.compile("^[A-Z0-9]{1,}-[0-9]{5,}-[0-9]{5,}-[0-9]{5,}$");
  private static final String BFB_BASE_PRICE = "bfbBasePrice";
  private static final String BFB_MANAGED = "bfbManaged";
  private static final String BFB_STATUS = "bfbStatus";

  private static final ObjectMapper objectMapper = new ObjectMapper();

  private static final Set<String> ALLOWED_IMAGE_PREFIXES =
      ImmutableSet.of(Constant.HTTPS_PREFIX, Constant.HTTP_PREFIX);
  public static final Map<Boolean, String> emptyFileErrorMessage =
      ImmutableMap.of(true, ProductLevel3GenericProcessorServiceBean.EXCEL_FILE_MUST_NOT_BE_BLANK_EN, false,
          ProductLevel3GenericProcessorServiceBean.EXCEL_FILE_MUST_NOT_BE_BLANK_ID);
  public static final Map<Boolean, String> excelVersionOutdatedErrorMessage =
      ImmutableMap.of(true, ProductLevel3GenericProcessorServiceBean.EXCEL_VERSION_OUTDATED_EN, false,
          ProductLevel3GenericProcessorServiceBean.EXCEL_VERSION_OUTDATED_ID);
  private static final Map<Boolean, String> DELIVERY_STATUS_HEADER = Map.of(
      true, GenericBulkHeaders.DELIVERY_STATUS,
      false, BulkCnCreationHeaderNames.PRODUCT_DELIVERY_STATUS_ID_HEADER_NAME
  );
  private static final Map<Boolean, String> DELIVERY_STATUS_EN_HEADER = Map.of(
      true, GenericBulkHeaders.DELIVERY_STATUS_EN,
      false, BulkCnCreationHeaderNames.PRODUCT_DELIVERY_STATUS_EN_HEADER_NAME
  );
  public static final Map<Boolean, String> CNC_HEADER = Map.of(
      true, GenericBulkHeaders.CNC,
      false, BulkCnCreationHeaderNames.PRODUCT_CNC_STATUS_HEADER_NAME
  );
  public static final Map<Boolean, String> CNC_EN_HEADER = Map.of(
      true, GenericBulkHeaders.CNC_EN,
      false, BulkCnCreationHeaderNames.PRODUCT_CNC_STATUS_HEADER_NAME
  );
  private static final Map<Boolean, String> INSTORE_HEADER = Map.of(
      true, GenericBulkHeaders.INSTORE,
      false, BulkCnCreationHeaderNames.INSTORE
  );

  public static String toLowerCaseName(String value) {
    return value.toLowerCase().replaceAll(Constant.NOT_ALPHANUMERIC_REGEX, Constant.DASH);
  }

  public static Map<String, PickupPointDTO> getPickupPoints(ProfileResponse businessPartner) {
    return businessPartner.getPickupPoints().stream()
        .collect(Collectors.toMap(PickupPointDTO::getCode, pickupPoint -> pickupPoint, (a, b) -> b));
  }

  public static Map<String, PickupPointResponse> getPickupPointsV2(
    List<PickupPointResponse> pickupPointResponseList) {
    return pickupPointResponseList.stream().collect(
      Collectors.toMap(PickupPointResponse::getCode, pickupPoint -> pickupPoint, (a, b) -> b));
  }

  public static boolean isDefiningOrVariantCreation(AttributeResponse attributeResponse) {
    return AttributeType.DEFINING_ATTRIBUTE.name().equals(attributeResponse.getAttributeType()) || attributeResponse
        .isVariantCreation();
  }

  public static boolean validateBundlingInfo(Map<String, Object> row, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant,
      int productBundlingMaxNumberOfSkus, StringBuilder validationErrorMessage, boolean result) {
    String childSkus = String.valueOf(Optional.ofNullable(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(row, BulkCnCreationHeaderNames.CHILD_SKU_ID,
            BulkCnCreationHeaderNames.CHILD_SKU_EN)).orElse(StringUtils.EMPTY));
    String quantities = String.valueOf(Optional.ofNullable(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(row, BulkCnCreationHeaderNames.QUANTITY_ID,
            BulkCnCreationHeaderNames.QUANTITY_EN)).orElse(StringUtils.EMPTY));
    Set<String> childSkuList = new HashSet<>();
    List<String> quantityList = new ArrayList<>();
    if (StringUtils.isNotBlank(childSkus)) {
      childSkuList.addAll(
          Arrays.stream(childSkus.split(Constants.COMMA)).map(StringUtils::trimToEmpty).collect(Collectors.toSet()));
    }
    if (StringUtils.isNotBlank(quantities)) {
      quantityList = Arrays.asList(quantities.split(Constants.COMMA));
    }
    if (isBundlingInfoInvalid(row, bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant,
        productBundlingMaxNumberOfSkus, childSkuList, quantityList, validationErrorMessage)) {
      return false;
    }
    return result;
  }

  private static boolean isBundlingInfoInvalid(Map<String, Object> row, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant,
      int productBundlingMaxNumberOfSkus, Set<String> childSkuList, List<String> quantityList,
      StringBuilder validationErrorMessage) {
    if (CollectionUtils.isNotEmpty(childSkuList) || CollectionUtils.isNotEmpty(quantityList)) {
      String resultErrorMessage = null;
      String columnRowInformation = String.valueOf(row.getOrDefault(GenericBulkHeaders.ROW_NUMBER, StringUtils.EMPTY));
      if (childSkuList.size() != quantityList.size()) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.CHILD_SKU_QUANTITY_MISMATCH_EN,
            BulkProcessValidationErrorMessages.CHILD_SKU_QUANTITY_MISMATCH_ID);
        bulkUploadErrorCounter.incrementProductName();
        log.error("Validation error bulk process : {}, Row {}. Product Name - {} . Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation)),
            row.getOrDefault(GenericBulkParameters.PRODUCT_NAME_COLUMN, StringUtils.EMPTY), BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.CHILD_SKU_QUANTITY_MISMATCH_EN);
        addValidationErrorMessage(bulkUploadErrorCounter.getProductType(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        return true;
      }
      if (childSkuList.size() > productBundlingMaxNumberOfSkus) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.CHILD_SKU_MAX_LIMIT_EN,
            BulkProcessValidationErrorMessages.CHILD_SKU_MAX_LIMIT_ID);
        bulkUploadErrorCounter.incrementProductName();
        log.error("Validation error bulk process : {}, Row {}. Product Name - {} . Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation)),
            row.get(GenericBulkParameters.PRODUCT_NAME_COLUMN), BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.CHILD_SKU_MAX_LIMIT_EN);
        addValidationErrorMessage(bulkUploadErrorCounter.getProductType(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        return true;
      }
      for (String skuQuantity : quantityList) {
        int quantity = 0;
        try {
          quantity = Integer.parseInt(StringUtils.trimToEmpty(skuQuantity));
        } catch (Exception e) {
          resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
              BulkProcessValidationErrorMessages.QUANTITY_INVALID_EN,
              BulkProcessValidationErrorMessages.QUANTITY_INVALID);
          log.warn(Constant.WARNING_PREFIX, e);
          bulkUploadErrorCounter.incrementHarga();
          addValidationErrorMessage(bulkUploadErrorCounter.getHarga(), validationErrorMessage,
              errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
          log.error("Validation error bulk process: {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
              bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation)),
              row.get(GenericBulkParameters.PRODUCT_NAME_COLUMN), BulkErrorCategory.INPUT_ERROR.getDescription(),
              BulkProcessValidationErrorMessages.QUANTITY_INVALID);
          return true;
        }
        if (quantity < 1) {
          resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
              BulkProcessValidationErrorMessages.QUANTITY_MUST_BE_GREATER_THAN_MINIMUM_VALUE_EN,
              BulkProcessValidationErrorMessages.QUANTITY_MUST_BE_GREATER_THAN_MINIMUM_VALUE);
          bulkUploadErrorCounter.incrementProductName();
          log.error("Validation error bulk process : {}, Row {}. Product Name - {} . Error msg - {} : {}.",
              bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation)),
              row.get(GenericBulkParameters.PRODUCT_NAME_COLUMN), BulkErrorCategory.INPUT_ERROR.getDescription(),
              BulkProcessValidationErrorMessages.QUANTITY_MUST_BE_GREATER_THAN_MINIMUM_VALUE_EN);
          addValidationErrorMessage(bulkUploadErrorCounter.getProductType(), validationErrorMessage,
              errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
          return true;
        }
      }
      for (String childSku : childSkuList) {
        if (!ITEM_SKU_PATTERN.matcher(StringUtils.trimToEmpty(childSku)).matches()) {
          resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
              BulkProcessValidationErrorMessages.CHILD_SKU_INVALID_EN,
              BulkProcessValidationErrorMessages.CHILD_SKU_INVALID_ID);
          bulkUploadErrorCounter.incrementProductName();
          log.error("Validation error bulk process : {}, Row {}. Product Name - {} . Error msg - {} : {}.",
              bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation)),
              row.get(GenericBulkParameters.PRODUCT_NAME_COLUMN), BulkErrorCategory.INPUT_ERROR.getDescription(),
              BulkProcessValidationErrorMessages.CHILD_SKU_INVALID_EN);
          addValidationErrorMessage(bulkUploadErrorCounter.getProductType(), validationErrorMessage,
              errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
          return true;
        }
      }
    }
    return false;
  }

  private void uploadImageFiles(Map<String, String> mappingImageFiles, BulkProcess bulkProcess,
      String mtaImageSource) {
    for (Map.Entry<String, String> entry : mappingImageFiles.entrySet()) {
      try {
        fileStorageService.uploadImageFilesToSourceLocation(entry, bulkProcess, mtaImageSource);
      } catch (Exception e) {
        log.error("Error at uploadImageFile, data not found for: {},{},{}", bulkProcess.getBulkProcessCode(), entry, e);
      }
    }
  }

  public static void initializeProductCategory(String storeId, ProductCreationRequest productCollectionRequest,
      CategoryDetailResponse categoryDetailResponse) {
    CatalogRequest catalogRequest = new CatalogRequest();
    BeanUtils.copyProperties(categoryDetailResponse.getCatalog(), catalogRequest);
    CategoryRequest categoryRequest = new CategoryRequest();
    categoryRequest.setStoreId(storeId);
    categoryRequest.setId(categoryDetailResponse.getId());
    categoryRequest.setName(categoryDetailResponse.getName());
    categoryRequest.setCategoryCode(categoryDetailResponse.getCategoryCode());
    categoryRequest.setLogisticAdjustment(categoryDetailResponse.getLogisticAdjustment());
    categoryRequest.setCatalog(catalogRequest);
    ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
    productCategoryRequest.setStoreId(storeId);
    productCategoryRequest.setCategory(categoryRequest);
    productCollectionRequest.getProductCategories().add(productCategoryRequest);
  }

  public static void setDataFinalStatus(BulkProcessData data, Map<Integer, String> rowNumberErrorMap, int systemError) {
    if (systemError > Constant.ZERO) {
      data.setSystemErrorCount(1);
      data.setStatus(BulkProcessData.STATUS_FAIL);
    } else {
      if (rowNumberErrorMap.containsKey(data.getRowNumber())) {
        data.setErrorMessage(rowNumberErrorMap.get(data.getRowNumber()));
        data.setStatus(BulkProcessData.STATUS_FAIL);
        data.setInputErrorCount(1);
      } else {
        data.setStatus(BulkProcessData.STATUS_SUCCESS);
      }
    }
    data.setEndDate(new Date());
  }

  public static Map<String, List<String>> getCategoryVariantCreationAttributes(
      Map<String, CategoryDetailAndShippingResponse> categoryIdAndDetailMap,
      Map<String, String> variantCreationNameAndColumn,
      Map<String, Map<String, String>> categoryVariantAttributeIdAndNameMap, boolean isInternationalMerchant) {
    Map<String, List<String>> categoryDefiningAttributeSortedIndex = new HashMap<>();

    for (Map.Entry<String, CategoryDetailAndShippingResponse> categoryIdAndDetailResponse : categoryIdAndDetailMap
        .entrySet()) {
      Map<String, String> definingAttributesIdAndName = new TreeMap<>();
      for (CategoryAttributeResponse categoryAttributeResponse : categoryIdAndDetailResponse.getValue()
          .getCategoryAttributes()) {
        if (!categoryAttributeResponse.isMarkForDelete() && isDefiningOrVariantCreation(
            categoryAttributeResponse.getAttribute())) {
          definingAttributesIdAndName.putIfAbsent(categoryAttributeResponse.getAttribute().getId(),
              isInternationalMerchant ?
                  StringUtils.isNotBlank(categoryAttributeResponse.getAttribute().getNameEnglish()) ?
                      categoryAttributeResponse.getAttribute().getNameEnglish() :
                      categoryAttributeResponse.getAttribute().getName() :
                  categoryAttributeResponse.getAttribute().getName());
        }
      }
      categoryVariantAttributeIdAndNameMap
          .putIfAbsent(categoryIdAndDetailResponse.getKey(), definingAttributesIdAndName);
    }


    for (Map.Entry<String, Map<String, String>> categoryVariantAttributes : categoryVariantAttributeIdAndNameMap.entrySet()) {
      List<String> definingAttribute = new ArrayList<>();
      for (Map.Entry<String, String> categoryAttribute : categoryVariantAttributes.getValue().entrySet()) {
        if (categoryAttribute.getValue().toLowerCase().contains(GenericBulkParameters.WARNA)
            || categoryAttribute.getValue().toLowerCase().contains(GenericBulkParameters.WARNA_EN)) {
          definingAttribute.add(
              getAttributeNameBasedOnHeader(GenericBulkHeaders.WARNA_EN, GenericBulkHeaders.WARNA,
                  isInternationalMerchant));
          variantCreationNameAndColumn.putIfAbsent(categoryAttribute.getValue(),
              getAttributeNameBasedOnHeader(GenericBulkHeaders.WARNA_EN, GenericBulkHeaders.WARNA,
                  isInternationalMerchant));
        } else if (categoryAttribute.getValue().toLowerCase().contains(GenericBulkParameters.UKURAN)
            || categoryAttribute.getValue().toLowerCase().contains(GenericBulkParameters.UKURAN_EN)) {
          definingAttribute.add(
              getAttributeNameBasedOnHeader(GenericBulkHeaders.UKURAN_EN, GenericBulkHeaders.UKURAN,
                  isInternationalMerchant));
          variantCreationNameAndColumn.putIfAbsent(categoryAttribute.getValue(),
              getAttributeNameBasedOnHeader(GenericBulkHeaders.UKURAN_EN, GenericBulkHeaders.UKURAN,
                  isInternationalMerchant));
        } else if (categoryAttribute.getValue().toLowerCase().contains(GenericBulkParameters.VARIASI)
            || categoryAttribute.getValue().toLowerCase().contains(GenericBulkParameters.VARIASI_EN)) {
          definingAttribute.add(
              getAttributeNameBasedOnHeader(GenericBulkHeaders.VARANSI_EN, GenericBulkHeaders.VARANSI,
                  isInternationalMerchant));
          variantCreationNameAndColumn.putIfAbsent(categoryAttribute.getValue(),
              getAttributeNameBasedOnHeader(GenericBulkHeaders.VARANSI_EN, GenericBulkHeaders.VARANSI,
                  isInternationalMerchant));
        }
        categoryDefiningAttributeSortedIndex.putIfAbsent(categoryVariantAttributes.getKey(), definingAttribute);
      }
    }
    return categoryDefiningAttributeSortedIndex;
  }

  public static boolean checkIfEmptyOrNotApplicable(String value) {
    return (StringUtils.isNotBlank(String.valueOf(value)) && !(GenericBulkParameters.NOT_APPLICABLE
        .equalsIgnoreCase(value)));
  }

  public static Map<String, String> getUserInputCategoryTreeAndChildCategory(List<Map<String, Object>> userInputRows,
      Set<String> C1CategoryNames) {
    Map<String, String> categoryTreeAndCnCategoryCode = new HashMap<>();
    for (Map<String, Object> userInputRow : userInputRows) {
      String categoryHierarchyDelimited = StringUtils.EMPTY;
      if (checkIfEmptyOrNotApplicable(String.valueOf(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(userInputRow, GenericBulkHeaders.CATEGORY,
              GenericBulkHeaders.CATEGORY_EN)))) {
        categoryHierarchyDelimited = String.valueOf(
            BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(userInputRow, GenericBulkHeaders.CATEGORY,
                GenericBulkHeaders.CATEGORY_EN));
        C1CategoryNames.add(String.valueOf(
            BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(userInputRow, GenericBulkHeaders.CATEGORY,
                GenericBulkHeaders.CATEGORY_EN)));

        if (checkIfEmptyOrNotApplicable(String.valueOf(
            BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(userInputRow, GenericBulkHeaders.CATEGORY,
                GenericBulkHeaders.C2)))) {
          categoryHierarchyDelimited = categoryHierarchyDelimited.concat(
              GenericBulkParameters.CATEGORY_TREE + String.valueOf(
                  BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(userInputRow, GenericBulkHeaders.C2,
                      GenericBulkHeaders.C2)));
          if (checkIfEmptyOrNotApplicable(String.valueOf(
              BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(userInputRow, GenericBulkHeaders.CN,
                  GenericBulkHeaders.CN)))) {
            categoryHierarchyDelimited = categoryHierarchyDelimited.concat(
                GenericBulkParameters.CATEGORY_TREE + String.valueOf(
                    BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(userInputRow, GenericBulkHeaders.CN,
                        GenericBulkHeaders.CN)));
          }
        }
      }

      if (StringUtils.isNotBlank(categoryHierarchyDelimited)) {
        // Here +2 i.e GenericBulkParameters.CATEGORY_TREE.length()) is to get last category name
        // If '->' does not exist which means product is getting created with C1 (might happen in QA)
        if (categoryHierarchyDelimited.contains(GenericBulkParameters.CATEGORY_TREE)) {
          categoryTreeAndCnCategoryCode.putIfAbsent(categoryHierarchyDelimited, categoryHierarchyDelimited.substring(
              categoryHierarchyDelimited.lastIndexOf(GenericBulkParameters.CATEGORY_TREE)
                  + GenericBulkParameters.CATEGORY_TREE.length()));
        } else {
          categoryTreeAndCnCategoryCode.putIfAbsent(categoryHierarchyDelimited, categoryHierarchyDelimited);
        }
      }

      userInputRow.put(GenericBulkHeaders.CATEGORY_TREE_HIERARCHY, categoryHierarchyDelimited);
    }
    log.info("categoryTreeAndCnCategoryCode : {} ", categoryTreeAndCnCategoryCode);
    return categoryTreeAndCnCategoryCode;
  }

  public static void getMatchingCategoryHierarchyDelimitedAndCategoryIdMap(
      Map<String, String> userInputCategoryHierarchyDelimitedAndCategoryNameMap, Set<String> userInputC1CategoryNames,
      List<CategoryTreeResponse> response, boolean isInternationalMerchant) {
    Map<String, String> categoryHierarchyDelimitedCnIdMap = new HashMap<>();
    for (CategoryTreeResponse parentCategoryResponseFromPCB : response) {
      String categoryName = StringUtils.EMPTY;
      if (userInputC1CategoryNames.contains(parentCategoryResponseFromPCB.getCategoryEnglishName())
          && isInternationalMerchant) {
        categoryName = parentCategoryResponseFromPCB.getCategoryEnglishName();
      } else if (userInputC1CategoryNames.contains(parentCategoryResponseFromPCB.getCategoryName())
          && !isInternationalMerchant) {
        categoryName = parentCategoryResponseFromPCB.getCategoryName();
      }

      if (StringUtils.isNotBlank(categoryName)) {
        if (CollectionUtils.isEmpty(parentCategoryResponseFromPCB.getChildren())) {
          categoryHierarchyDelimitedCnIdMap.putIfAbsent(categoryName, parentCategoryResponseFromPCB.getId());
        } else {
          setCategoryHierarchyDelimitedAndIds(parentCategoryResponseFromPCB.getChildren(), categoryName,
              categoryHierarchyDelimitedCnIdMap, isInternationalMerchant);
        }
      }
    }

    userInputCategoryHierarchyDelimitedAndCategoryNameMap.entrySet().forEach(
        userInputCategoryHierarchyDelimitedAndCategoryName -> userInputCategoryHierarchyDelimitedAndCategoryName
            .setValue(
                categoryHierarchyDelimitedCnIdMap.get(userInputCategoryHierarchyDelimitedAndCategoryName.getKey())));
  }

  private static void setCategoryHierarchyDelimitedAndIds(List<CategoryTreeResponse> categoryTreeResponseList,
      String intermediateCategoryDelimited, Map<String, String> c1ToCnHierarchyMap, boolean isInternationalMerchant) {
    for (CategoryTreeResponse categoryTreeResponse : categoryTreeResponseList) {
      String categoryName;
      if (isInternationalMerchant)
        categoryName = categoryTreeResponse.getCategoryEnglishName();
      else
        categoryName = categoryTreeResponse.getCategoryName();
      intermediateCategoryDelimited = new StringBuilder().append(intermediateCategoryDelimited)
        .append(GenericBulkParameters.CATEGORY_TREE).append(categoryName).toString();
      if (CollectionUtils.isEmpty(categoryTreeResponse.getChildren())) {
        c1ToCnHierarchyMap.put(intermediateCategoryDelimited, categoryTreeResponse.getId());
      } else {
        setCategoryHierarchyDelimitedAndIds(categoryTreeResponse.getChildren(), intermediateCategoryDelimited,
            c1ToCnHierarchyMap, isInternationalMerchant);
      }
      intermediateCategoryDelimited =
          StringUtils.substringBeforeLast(intermediateCategoryDelimited, GenericBulkParameters.CATEGORY_TREE);
    }
  }

  public static void addCnCategoryIdToEachRow(Map<String, String> userInputCategoryTreeAndChildCategory,
      List<Map<String, Object>> userInputRows) {
    // Adding Cn category id to the user row
    for (Map<String, Object> userInputRow : userInputRows) {
      userInputRow.put(GenericBulkHeaders.CN_CATEGORY_ID, Objects.isNull(
          userInputCategoryTreeAndChildCategory.get(userInputRow.get(GenericBulkHeaders.CATEGORY_TREE_HIERARCHY))) ?
          StringUtils.EMPTY :
          userInputCategoryTreeAndChildCategory.get(userInputRow.get(GenericBulkHeaders.CATEGORY_TREE_HIERARCHY)));
      // Add a unique identifier for rows with similar parent value
      // If parent value = parent and cn id is category-id
      // The identifier will be parent+category-id
      if (StringUtils.isNotBlank(String.valueOf(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(userInputRow, GenericBulkHeaders.PARENT,
              GenericBulkHeaders.PARENT_EN)))) {
        userInputRow.put(GenericBulkHeaders.CN_CATEGORY_CODE_PARENT_CODE_IDENTIFIER,
            BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(userInputRow, GenericBulkHeaders.PARENT,
                GenericBulkHeaders.PARENT_EN) + GenericBulkParameters.PLUS_SIGN + userInputRow.get(
                GenericBulkHeaders.CN_CATEGORY_ID));
      } else {
        userInputRow.put(GenericBulkHeaders.CN_CATEGORY_CODE_PARENT_CODE_IDENTIFIER, StringUtils.EMPTY);
      }
    }
  }

  public static boolean validateExcelPickupPointsForGeneriCreation(Map<String, Object> raw,
      Map<String, PickupPointResponse> pickupPoints, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String ppNameDelimiter,
      Set<String> accessiblePickupPointCodes) {
    boolean result = true;
    StringBuilder validationErrorMessage = new StringBuilder();
    String columnRowInformation = String.valueOf(raw.get(GenericBulkHeaders.ROW_NUMBER));
    String pickupPoint = getPickupPointCodeFromPickupPointCodeAndName(ppNameDelimiter,
        getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.PICKUP_POINT,
            GenericBulkHeaders.PICKUP_POINT_EN).toString());
    if (!pickupPoints.containsKey(pickupPoint)){
      String resultError = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_INVALID_EN,
          BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_INVALID);
      bulkUploadErrorCounter.incrementPickupPoint();
      addValidationErrorMessage(bulkUploadErrorCounter.getPickupPoint(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultError, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, : Row : {}. Product Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation,
          raw.get(GenericBulkParameters.PRODUCT_NAME_COLUMN), BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_INVALID);
    } else if (CollectionUtils.isNotEmpty(accessiblePickupPointCodes)
        && !accessiblePickupPointCodes.contains(pickupPoint)) {
      String resultError = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.INACCESSIBLE_PICKUP_POINTS_EN,
          BulkProcessValidationErrorMessages.INACCESSIBLE_PICKUP_POINTS);
      bulkUploadErrorCounter.incrementPickupPoint();
      addValidationErrorMessage(bulkUploadErrorCounter.getPickupPoint(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultError, StringUtils.EMPTY));
      result = false;
      log.error(
          "Validation error bulk process : {}, : Row : {}. Product Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation,
          raw.get(GenericBulkParameters.PRODUCT_NAME_COLUMN),
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.INACCESSIBLE_PICKUP_POINTS);
    }
    if (!result) {
      bulkProcessNotes.setNotes(validationErrorMessage.toString());
    }
    return result;
  }

  public static boolean validateExcelAttributes(Map<String, Object> raw, BulkProcess bulkProcess,
      BulkProcessNotes bulkProcessNotes, BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant,
      Map<String, CategoryDetailAndShippingResponse> categoryIdAndDetailMap,
      Map<String, List<String>> attributesIdAndPossibleValues, boolean brandFetchIgnoreCase) {
    boolean result = true;
    StringBuilder validationErrorMessage = new StringBuilder();
    String columnRowInformation = String.valueOf(raw.get(GenericBulkHeaders.ROW_NUMBER));
    Map<String, String> otherAttributesNamesAndValues = getOtherAttributesNames(raw);
    CategoryDetailAndShippingResponse categoryAndAttributeResponse =
        categoryIdAndDetailMap.get(String.valueOf(raw.get(GenericBulkHeaders.CN_CATEGORY_ID)));
    for (CategoryAttributeResponse attributeResponse : categoryAndAttributeResponse.getCategoryAttributes()) {
      if (!attributeResponse.getAttribute().isMarkForDelete()) {
        AttributeResponse attribute = attributeResponse.getAttribute();
        String value = otherAttributesNamesAndValues.get(attribute.getName());
        if (isNonVariantCreationAndNotBrandAndNotFamilyColourAttribute(attribute)) {
          if (attribute.isMandatory() && StringUtils.isBlank(value)) {
            String resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
              BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK_EN,
              BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK);
            value = otherAttributesNamesAndValues.get(attribute.getNameEnglish());
            bulkUploadErrorCounter.incrementFeature();
            addValidationErrorMessage(bulkUploadErrorCounter.getFeature(), validationErrorMessage,
              errorMessage(getAttributeName(isInternationalMerchant, attribute) + StringUtils.SPACE,
                resultErrorMessage, StringUtils.EMPTY));
            result = false;
            log.error(
              "Validation error bulk process : {}, Row : {} . Error msg - {}. Attribute Name - {}.",
              bulkProcess.getBulkProcessCode(), columnRowInformation,
              BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK,
              attribute.getName());
          }
          if (StringUtils.isNotBlank(value) && AttributeType.PREDEFINED_ATTRIBUTE.name()
              .equals(attribute.getAttributeType())) {
            result = validatePreDefinedAttribute(value, attribute, bulkUploadErrorCounter, isInternationalMerchant,
                columnRowInformation, validationErrorMessage, attributesIdAndPossibleValues,
                bulkProcess.getBulkProcessCode(), result);
          }
        } else if (isBrandAndOrFamilyColourAttribute(attribute)) {
          if (GenericBulkParameters.BRAND.equals(attribute.getName())) {
            result = validateBrand(raw, attribute, bulkUploadErrorCounter, isInternationalMerchant,
              columnRowInformation, validationErrorMessage, attributesIdAndPossibleValues,
              bulkProcess.getBulkProcessCode(), result, brandFetchIgnoreCase);
          } else {
            result = validateFamilyColor(raw, attribute, bulkUploadErrorCounter, isInternationalMerchant,
                columnRowInformation, validationErrorMessage, attributesIdAndPossibleValues,
                bulkProcess.getBulkProcessCode(), result);
          }
        }
      }
    }
    if (!result) {
      bulkProcessNotes.setNotes(validationErrorMessage.toString());
    }
    return result;
  }

  private static boolean validateBrand(Map<String, Object> raw, AttributeResponse attribute,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      StringBuilder validationErrorMessage, Map<String, List<String>> attributesIdAndPossibleValues, String blpCode,
      boolean result, boolean brandFetchIgnoreCase) {
    String value =
        String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.BRAND, GenericBulkHeaders.BRAND_EN));
    String cleanBrandValue;
    if (value.contains(Constant.IN_REVIEW)) {
      cleanBrandValue = value.substring(0, value.indexOf(Constant.IN_REVIEW_BRAND_SUFFIX));
    } else {
      cleanBrandValue = value;
    }
    List<String> stringList = attributesIdAndPossibleValues.getOrDefault(attribute.getId(), new ArrayList<>());
    if (brandFetchIgnoreCase) {
      overrideBrandIgnoreCase(raw, stringList, cleanBrandValue, true);
      cleanBrandValue = String.valueOf(
        getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.BRAND, GenericBulkHeaders.BRAND_EN));
    }
    if (StringUtils.isBlank(cleanBrandValue)) {
      String resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.BRAND_CANNOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.BRAND_CANNOT_BE_BLANK);
      bulkUploadErrorCounter.incrementFeature();
      addValidationErrorMessage(bulkUploadErrorCounter.getFeature(), validationErrorMessage,
          errorMessage(getAttributeName(isInternationalMerchant, attribute) + StringUtils.SPACE, resultErrorMessage,
              StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row : {} . Error msg - {}. Attribute Name - {}.", blpCode,
          columnRowInformation, BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK,
          attribute.getName());
    } else if (!stringList.contains(cleanBrandValue)) {
      String resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID_EN,
          BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID);
      bulkUploadErrorCounter.incrementFeature();
      addValidationErrorMessage(bulkUploadErrorCounter.getFeature(), validationErrorMessage,
          errorMessage("Brand ", resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Bulk process : {}, Row : {} . Error msg - {}. Attribute Name - {}.", blpCode, columnRowInformation,
          BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID, attribute.getName() + ".");
    }
    return result;
  }

  public static void overrideBrandIgnoreCase(Map<String, Object> raw, List<String> stringList,
    String cleanBrandValue, boolean genericCreation) {

    Optional<String> matchedBrand = stringList.stream().filter(Objects::nonNull)
      .filter(brand -> brand.equalsIgnoreCase(cleanBrandValue)).findFirst();

    if (genericCreation) {
      matchedBrand.ifPresent(brand -> {
        raw.put(GenericBulkHeaders.BRAND_EN, brand);
        raw.put(GenericBulkHeaders.BRAND, brand);
      });
    } else {
      matchedBrand.ifPresent(brand -> {
        raw.put(BulkCnCreationHeaderNames.BRAND_HEADER_NAME, brand);
      });
    }
  }

  private static boolean validateFamilyColor(Map<String, Object> raw, AttributeResponse attribute,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      StringBuilder validationErrorMessage, Map<String, List<String>> attributesIdAndPossibleValues, String blpCode,
      boolean result) {
    String value = String.valueOf(
        getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.FAMILY_COLOUR, GenericBulkHeaders.FAMILY_COLOUR_EN));
    if (StringUtils.isNotBlank(value)) {
      List<String> stringList = attributesIdAndPossibleValues.get(attribute.getId());
      if (!stringList.contains(value)) {
        String resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID_EN,
            BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID);
        bulkUploadErrorCounter.incrementFeature();
        addValidationErrorMessage(bulkUploadErrorCounter.getFeature(), validationErrorMessage,
            errorMessage(getAttributeName(isInternationalMerchant, attribute) + StringUtils.SPACE, resultErrorMessage,
                StringUtils.EMPTY));
        result = false;
        log.error("Bulk process : {}, Row : {} . Error msg - {}. Attribute Name - {}.", blpCode, columnRowInformation,
            BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID, attribute.getName() + ".");
      }
    }
    return result;
  }

  private static boolean validatePreDefinedAttribute(String value, AttributeResponse attribute,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      StringBuilder validationErrorMessage, Map<String, List<String>> attributesIdAndPossibleValues, String blpCode,
      boolean result) {
    List<String> stringList = attributesIdAndPossibleValues.get(attribute.getId());
    if (!stringList.contains(value)) {
      String resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID_EN,
          BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID);
      bulkUploadErrorCounter.incrementFeature();
      addValidationErrorMessage(bulkUploadErrorCounter.getFeature(), validationErrorMessage,
          errorMessage(getAttributeName(isInternationalMerchant, attribute) + StringUtils.SPACE, resultErrorMessage,
              StringUtils.EMPTY));
      result = false;
      log.error("Bulk process : {}, Row : {} . Error msg - {}. Attribute Name - {}.", blpCode, columnRowInformation,
          BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID, attribute.getName() + ".");
    }
    return result;
  }

  public static boolean isBrandAndOrFamilyColourAttribute(AttributeResponse attributeResponse) {
    return (GenericBulkParameters.FAMILY_COLOUR.equals(attributeResponse.getName()) || GenericBulkParameters.BRAND
        .equals(attributeResponse.getName()));
  }

  public static boolean validateCategory(CategoryDetailAndShippingResponse categoryDetailAndShippingResponse,
      BulkUploadErrorCounter bulkUploadErrorCounter) {
    boolean result = true;
    if (categoryDetailAndShippingResponse.getChildCount() != 0
      || categoryDetailAndShippingResponse.isMarkForDelete()
      || !categoryDetailAndShippingResponse.isActivated()) {
      bulkUploadErrorCounter.incrementVariation();
      if (bulkUploadErrorCounter.getVariation() <= Constant.ERROR_COUNT)
        result = false;
    }
    return result;
  }

  public static boolean isWarnaAndDescriptive(AttributeResponse attribute, String attributeName) {
    return (Constant.WARNA.equalsIgnoreCase(attributeName) || Constant.COLOR.equalsIgnoreCase(attributeName))
        && AttributeType.DESCRIPTIVE_ATTRIBUTE.name().equals(attribute.getAttributeType());
  }

  public static String getAttributeName(boolean isInternationalMerchant, AttributeResponse attribute) {
    if (isInternationalMerchant && StringUtils.isNotBlank(attribute.getNameEnglish())) {
      return attribute.getNameEnglish();
    }
    return attribute.getName();
  }

  public static Map<String, List<String>> getAttributesValues(Map<String, AttributeResponse> attributes) {
    Map<String, List<String>> attributesValues = new HashMap<>();
    for (Map.Entry<String, AttributeResponse> entry : attributes.entrySet()) {
      AttributeResponse attribute = entry.getValue();
      if (attribute.getAttributeType().equals(String.valueOf(AttributeType.PREDEFINED_ATTRIBUTE))) {
        List<String> values = new ArrayList<>();
        for (PredefinedAllowedAttributeValueResponse predefinedAllowedValue : attribute
            .getPredefinedAllowedAttributeValues()) {
          values.add(predefinedAllowedValue.getValue());
        }
        attributesValues.put(entry.getKey(), values);
      } else if (AttributeType.DEFINING_ATTRIBUTE.toString().equals(attribute.getAttributeType())) {
        List<String> values = new ArrayList<>();
        for (AllowedAttributeValueResponse allowedValue : attribute.getAllowedAttributeValues()) {
          values.add(allowedValue.getValue());
        }
        attributesValues.put(entry.getKey(), values);
      }
    }
    return attributesValues;
  }

  public String validateExcelImagesForSingleRow(List<Map<String, Object>> rows, BulkProcess bulkProcess,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant,
      Map<String, String> imageUrlAndLocationMap, List<BulkProcessImage> imageList, int imageMaxSize) throws Exception {
    //Group rows contain all rows with same parent column , sort group rows based on row number to validate the
    //first row to process common images and ignore for rest of the rows
    rows.sort(Comparator.comparingInt(row -> (int) row.getOrDefault(GenericBulkHeaders.ROW_NUMBER, 0)));
    Map<String, Object> firstRow = rows.stream().findFirst().orElse(Collections.emptyMap());
    String columnRowInformation = String.valueOf(firstRow.get(GenericBulkHeaders.ROW_NUMBER));
    Map<String, String> imageAndImageUrlReverseMap = new HashMap<>();
    List<String> images = new ArrayList<>();
    StringBuilder validationErrorMessage = new StringBuilder();
    Map<String, BulkProcessImage> imageURLMap = new HashMap<>();

    if (CollectionUtils.isNotEmpty(imageList)) {
      imageURLMap = imageList.stream().collect(Collectors.toMap(BulkProcessImage::getImageURL, Function.identity()));
    }

    for (int imageNumber = 0; imageNumber < GenericBulkParameters.NUMBER_OF_IMAGES; imageNumber++) {
      String image = String.valueOf(
          getValueBasedOnEnOrIdHeader(firstRow, GenericBulkHeaders.IMAGE_ID_HEADER_LIST.get(imageNumber),
              GenericBulkHeaders.IMAGE_EN_HEADER_LIST.get(imageNumber)));
      validateImageAndPrepareImageUrlAndLocationMap(bulkProcess, imageUrlAndLocationMap, columnRowInformation,
          imageAndImageUrlReverseMap, images, validationErrorMessage, imageURLMap, image);
    }

    if (images.isEmpty() && StringUtils.isBlank(validationErrorMessage.toString())) {
      String resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.IMAGES_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.IMAGES_MUST_NOT_BE_BLANK);
      BulkCreationCommonUtil.addValidationErrorMessage(bulkUploadErrorCounter.getImage(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      log.error("Bulk process: {}, Row {} - {} : {} ", bulkProcess.getBulkProcessCode(),
          (Integer.parseInt(columnRowInformation) + 1), BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.IMAGES_MUST_NOT_BE_BLANK);
    }
    //Validate variant images for all the variants
    for (Map<String, Object> row : rows) {
      String image = String.valueOf(Optional.ofNullable(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(row, GenericBulkHeaders.VARIANT_IMAGE_EN,
              GenericBulkHeaders.VARIANT_IMAGE_ID)).orElse(StringUtils.EMPTY));
      validateImageAndPrepareImageUrlAndLocationMap(bulkProcess, imageUrlAndLocationMap, columnRowInformation,
          imageAndImageUrlReverseMap, images, validationErrorMessage, imageURLMap, image);
    }
    //Download n validate image from GCS only when no error msg in download image table
    if (StringUtils.isBlank(validationErrorMessage.toString())) {
      fileStorageService.downloadAndValidateProductCreationImages(bulkProcess,
        bulkUploadErrorCounter, imageAndImageUrlReverseMap, validationErrorMessage, images,
        columnRowInformation, imageMaxSize, isInternationalMerchant);
    }

    if (StringUtils.isBlank(String.valueOf(firstRow.get(GenericBulkParameters.IMAGE_START_COLUMN)))) {
      String resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.FIRST_IMAGE_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.FIRST_IMAGE_MUST_NOT_BE_BLANK);
      BulkCreationCommonUtil.addValidationErrorMessage(bulkUploadErrorCounter.getImage(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      log.error("Bulk process: {}, Row {} - {} : {} ", bulkProcess.getBulkProcessCode(),
          (Integer.parseInt(columnRowInformation) + 1), BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.FIRST_IMAGE_MUST_NOT_BE_BLANK);
    }
    return validationErrorMessage.toString();
  }

  public void validateImageAndPrepareImageUrlAndLocationMap(BulkProcess bulkProcess,
      Map<String, String> imageUrlAndLocationMap, String columnRowInformation,
      Map<String, String> imageAndImageUrlReverseMap, List<String> images, StringBuilder validationErrorMessage,
      Map<String, BulkProcessImage> imageURLMap, String image) {
    if (StringUtils.isNotBlank(image) && !images.contains(image)) {
      if (ALLOWED_IMAGE_PREFIXES.stream().noneMatch(image::startsWith)) {
        images.add(image);
      } else {
        BulkProcessImage bulkProcessImage = imageURLMap.get(image);
        if (Objects.nonNull(bulkProcessImage)) {
          if (StringUtils.isNotBlank(bulkProcessImage.getErrorMessage())) {
            log.error("Bulk process: {}, Row {} - {} : {} ", bulkProcess.getBulkProcessCode(),
                (Integer.parseInt(columnRowInformation) + 1), BulkErrorCategory.INPUT_ERROR.getDescription(),
                bulkProcessImage.getErrorMessage());
            validationErrorMessage.append(bulkProcessImage.getErrorMessage()).append(Constant.PERIOD);
            return;
          }
          // Compare with global map to make sure not to download same image twice
          String[] splitImageFilename = bulkProcessImage.getLocation().split("/");
          String imageFileName = splitImageFilename[splitImageFilename.length - 1];
          if (!imageUrlAndLocationMap.containsKey(bulkProcessImage.getImageURL())) {
            imageUrlAndLocationMap.putIfAbsent(bulkProcessImage.getImageURL(), imageFileName);
            imageAndImageUrlReverseMap.putIfAbsent(imageFileName, bulkProcessImage.getImageURL());
          }
          images.add(imageFileName);
        }
      }
    }
  }

  public static boolean validateExcelRow(Map<String, Object> raw, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, Integer minimumPrice,
      long maxStockLimit, MerchantStatusType merchantStatusType, int productBundlingMaxNumberOfSkus,
      boolean productBundlingEnabled, String merchantType, String productBundlingEligibleMerchantTypes,
      String commonImageErrorMessage, boolean bopisCncRestrictionEnabled, boolean pureInstoreProduct,
      boolean instoreSeller, ValidateExcelRowsRequest validateExcelRowsRequest) {
    String columnRowInformation = String.valueOf(raw.get(GenericBulkHeaders.ROW_NUMBER));
    boolean result = true;
    String productName = String.valueOf(
        getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.PRODUCT_NAME, GenericBulkHeaders.PRODUCT_NAME_EN));
    StringBuilder validationErrorMessage = new StringBuilder();
    result = validateProductName(raw, bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant,
        columnRowInformation, productName, validationErrorMessage, result);
    result = validateProductDescription(raw, bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant,
        columnRowInformation, productName, validationErrorMessage, result, pureInstoreProduct);
    result =
        validateProductUSP(raw, bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant, columnRowInformation,
            validationErrorMessage, result);
    result = validateCommonImagesForEachVariant(bulkProcessNotes, bulkUploadErrorCounter, commonImageErrorMessage,
        columnRowInformation, result, productName, validationErrorMessage);
    overrideEmptyProductType(validateExcelRowsRequest, raw, validateExcelRowsRequest.getPrimaryIdentifier(),
        GenericBulkHeaders.PRODUCT_TYPE, GenericBulkHeaders.PRODUCT_TYPE_EN);
    result = validateProductType(raw, bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant,
        columnRowInformation, productName, validationErrorMessage, result);
    result = validateProductSalePrice(raw, bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant,
        columnRowInformation, productName, validationErrorMessage, minimumPrice, result);
    result = validateProductStock(raw, bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant,
        columnRowInformation, productName, validationErrorMessage, result, maxStockLimit);
    result = validateProductPickUp(raw, bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant,
        columnRowInformation, productName, validationErrorMessage, result);
    if(isShippingTypeNotBopis(raw, GenericBulkHeaders.PRODUCT_TYPE, GenericBulkHeaders.PRODUCT_TYPE_EN)) {
      result = validateProductLength(raw, bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant,
          columnRowInformation, productName, validationErrorMessage, result, pureInstoreProduct);
      result = validateProductHeight(raw, bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant,
          columnRowInformation, productName, validationErrorMessage, result, pureInstoreProduct);
      result = validateProductWidth(raw, bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant,
          columnRowInformation, productName, validationErrorMessage, result, pureInstoreProduct);
      result = validateProductWeight(raw, bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant,
          columnRowInformation, productName, validationErrorMessage, result, pureInstoreProduct);
    }
    result = validateDeliveryStatus(
        getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.DELIVERY_STATUS, GenericBulkHeaders.DELIVERY_STATUS_EN),
        bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant, columnRowInformation, productName,
        validationErrorMessage, result, instoreSeller);
    result = validateCncStatus(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.CNC, GenericBulkHeaders.CNC_EN),
        bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant, columnRowInformation, productName,
        validationErrorMessage, result, merchantStatusType.getType(), bopisCncRestrictionEnabled,
        bopisCncRestrictionEnabled && isBopisProduct(raw));
    result =
        validateInstoreStatus(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.INSTORE, GenericBulkHeaders.INSTORE),
            bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant, columnRowInformation, productName,
            validationErrorMessage, result, instoreSeller);
    if (merchantStatusType.getType() >= MerchantStatusType.BFB.getType()) {
      result = validateBfbManagedStatus(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.BFB_MANAGED_HEADER_ID,
              GenericBulkHeaders.BFB_MANAGED_HEADER_EN), bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant,
          columnRowInformation, productName, validationErrorMessage, result);
      result = validateBfbBuyableStatus(
          getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.BFB_STATUS_HEADER, GenericBulkHeaders.BFB_STATUS_HEADER),
          bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant, columnRowInformation, productName,
          validationErrorMessage, result);
      String bfbBasePriceHeader =
          BulkCreationCommonUtil.getAttributeNameBasedOnHeader(GenericBulkHeaders.BFB_BASE_PRICE_HEADER_EN,
              GenericBulkHeaders.BFB_BASE_PRICE_HEADER_ID, isInternationalMerchant);
      result =
          validateBfbBasePrice(raw, productName, columnRowInformation, bulkProcessNotes, bulkUploadErrorCounter, result,
              validationErrorMessage, isInternationalMerchant, merchantStatusType, String.valueOf(
                  getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.BFB_BASE_PRICE_HEADER_EN,
                      GenericBulkHeaders.BFB_BASE_PRICE_HEADER_ID)), bfbBasePriceHeader).getRight();
    }
    if (ExcelTemplateUtil.isEligibleForBundleCreation(merchantType, productBundlingEligibleMerchantTypes,
        productBundlingEnabled)) {
      result = BulkCreationCommonUtil.validateBundlingInfo(raw, bulkProcessNotes, bulkUploadErrorCounter,
          isInternationalMerchant, productBundlingMaxNumberOfSkus, validationErrorMessage, result);
    }
    //validate
    if (!result) {
      bulkProcessNotes.setNotes(validationErrorMessage.toString());
    }
    return result;
  }

  private static boolean isBopisProduct(Map<String, Object> raw) {
    return BulkUploadOption.SHIPPING_TYPE_BOPIS.getdescription().equals(String.valueOf(
        getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.PRODUCT_TYPE, GenericBulkHeaders.PRODUCT_TYPE_EN)));
  }

  private static boolean validateProductName(Map<String, Object> raw, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      String productName, StringBuilder validationErrorMessage, boolean result) {
    String resultErrorMessage;
    if (StringUtils.isBlank(String.valueOf(
        getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.PRODUCT_NAME, GenericBulkHeaders.PRODUCT_NAME_EN)))) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.PRODUCT_NAME_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.PRODUCT_NAME_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementProductName();
      addValidationErrorMessage(bulkUploadErrorCounter.getProductName(), validationErrorMessage,
          isInternationalMerchant ?
              errorMessage(StringUtils.EMPTY, BulkProcessValidationErrorMessages.PRODUCT_NAME_MUST_NOT_BE_BLANK_EN,
                  StringUtils.EMPTY) :
              errorMessage(StringUtils.EMPTY, BulkProcessValidationErrorMessages.PRODUCT_NAME_MUST_NOT_BE_BLANK,
                  StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row {}. Product Name - empty/blank. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation, BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.PRODUCT_NAME_MUST_NOT_BE_BLANK);
    } else if (productName.length() > Constant.PRODUCT_NAME_LENGTH) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.PRODUCT_NAME_MUST_NOT_BE_MORE_THAN_150_CHARACTERS_EN,
          BulkProcessValidationErrorMessages.PRODUCT_NAME_MUST_NOT_BE_MORE_THAN_150_CHARACTERS);
      bulkUploadErrorCounter.incrementProductName();
      addValidationErrorMessage(bulkUploadErrorCounter.getProductName(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error(
          "Validation error bulk process : {}, Row {}. Product Name - exceeded maximum characters length. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation, BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.PRODUCT_NAME_MUST_NOT_BE_MORE_THAN_150_CHARACTERS);
    }
    return result;
  }

  private static boolean validateProductDescription(Map<String, Object> raw, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      String productName, StringBuilder validationErrorMessage, boolean result, boolean pureInstoreProduct) {
    String resultErrorMessage;
    String description = String.valueOf(
        getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.DESCRIPTION, GenericBulkHeaders.DESCRIPTION_EN));
    if (StringUtils.isBlank(String.valueOf(
        getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.DESCRIPTION, GenericBulkHeaders.DESCRIPTION_EN)))
        && !pureInstoreProduct) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.DESCRIPTION_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.DESCRIPTION_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementDescription();
      addValidationErrorMessage(bulkUploadErrorCounter.getDescription(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.DESCRIPTION_MUST_NOT_BE_BLANK);
    } else {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.DESCRIPTION_MUST_NOT_BE_MORE_THAN_5000_CHARACTERS_EN,
          BulkProcessValidationErrorMessages.DESCRIPTION_MUST_NOT_BE_MORE_THAN_5000_CHARACTERS);
      if (description.length() > Constant.DESCRIPTION_LENGTH) {
        bulkUploadErrorCounter.incrementDescription();
        addValidationErrorMessage(bulkUploadErrorCounter.getDescription(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error(
            "Validation error bulk process : {}, Row {}. Description - exceeded maximum characters length. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), columnRowInformation, BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.DESCRIPTION_MUST_NOT_BE_MORE_THAN_5000_CHARACTERS);
      }
    }
    return result;
  }

  private static boolean validateProductUSP(Map<String, Object> raw, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      StringBuilder validationErrorMessage, boolean result) {
    String resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
        BulkProcessValidationErrorMessages.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS_EN,
        BulkProcessValidationErrorMessages.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS);
    if (String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.USP, GenericBulkHeaders.USP_EN)).length()
        > Constant.USP_LENGTH) {
      bulkUploadErrorCounter.incrementUniqueSellingPoint();
      addValidationErrorMessage(bulkUploadErrorCounter.getUniqueSellingPoint(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error(
          "Validation error bulk process : {}, Row {}. Unique selling point - exceeded maximum characters length. Error"
              + " msg - {} : {}.", bulkProcessNotes.getBulkProcessCode(), columnRowInformation,
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS);
    }
    return result;
  }

  private static boolean validateProductType(Map<String, Object> raw, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      String productName, StringBuilder validationErrorMessage, boolean result) {
    String resultErrorMessage;
    if (StringUtils.isBlank(String.valueOf(
        getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.PRODUCT_TYPE, GenericBulkHeaders.PRODUCT_TYPE_EN)))) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.PRODUCT_TYPE_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.PRODUCT_TYPE_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementProductType();
      addValidationErrorMessage(bulkUploadErrorCounter.getProductType(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row {}. Product Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.PRODUCT_TYPE_MUST_NOT_BE_BLANK);
    } else {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.PRODUCT_TYPE_INVALID_EN,
          BulkProcessValidationErrorMessages.PRODUCT_TYPE_INVALID);
      String productType = String.valueOf(
          getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.PRODUCT_TYPE, GenericBulkHeaders.PRODUCT_TYPE_EN));
      List<String> shippingTypeEligibleValues;
      if (isInternationalMerchant) {
        shippingTypeEligibleValues = BulkParameters.BULK_OPTION_UPLOAD_SUPPORT_EN;
      } else {
        shippingTypeEligibleValues = BulkParameters.BULK_OPTION_UPLOAD_SUPPORT;
      }
      if (!shippingTypeEligibleValues.contains(productType)) {
        bulkUploadErrorCounter.incrementProductType();
        addValidationErrorMessage(bulkUploadErrorCounter.getProductType(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process: {}, Row {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.PRODUCT_TYPE_INVALID);
      }
    }
    return result;
  }

  private static boolean validateProductSalePrice(Map<String, Object> raw, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      String productName, StringBuilder validationErrorMessage, Integer minimumPrice, boolean result) {
    String resultErrorMessage;
    double price = 0.0;
    if (StringUtils.isBlank(String.valueOf(
        getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.SALE_PRICE, GenericBulkHeaders.SALE_PRICE_EN)))) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.PRICE_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.PRICE_MUST_NOT_BE_BLANK_EN);
      bulkUploadErrorCounter.incrementHarga();
      addValidationErrorMessage(bulkUploadErrorCounter.getHarga(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process: {}, Row {}. Product Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.PRICE_MUST_NOT_BE_BLANK);
    } else {
      try {
        price = Double.parseDouble(String.valueOf(
            getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.SALE_PRICE, GenericBulkHeaders.SALE_PRICE_EN)));
      } catch (Exception e) {
        resultErrorMessage =
            errorMessageBasedOnMerchant(isInternationalMerchant, BulkProcessValidationErrorMessages.PRICE_INVALID_EN,
                BulkProcessValidationErrorMessages.PRICE_INVALID);
        log.warn(BulkProcessValidationErrorMessages.WARNING_PREFIX, e);
        bulkUploadErrorCounter.incrementHarga();
        addValidationErrorMessage(bulkUploadErrorCounter.getHarga(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process: {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.PRICE_INVALID);
      }
      if ((price < minimumPrice)) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.MINIMUM_SALE_PRICE_VALUE_INVALID_EN,
            BulkProcessValidationErrorMessages.MINIMUM_SALE_PRICE_VALUE_INVALID);
        bulkUploadErrorCounter.incrementHargaPenjualan();
        addValidationErrorMessage(bulkUploadErrorCounter.getHargaPenjualan(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, Double.toString((double) minimumPrice)));
        result = false;
        log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.MINIMUM_PRICE_VALUE_INVALID);
      }
      if (price % 1 != 0) {
        raw.put(getAttributeNameBasedOnHeader(GenericBulkHeaders.SALE_PRICE_EN, GenericBulkHeaders.SALE_PRICE,
            isInternationalMerchant), (int) Math.round(price));
      }
    }
    return result;
  }

  private static boolean validateProductStock(Map<String, Object> raw, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      String productName, StringBuilder validationErrorMessage, boolean result, long maxStockLimit) {
    String resultErrorMessage;
    Double stock = 0.0;
    String stocksValue = String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.STOCK, GenericBulkHeaders.STOCK_EN));
      try {
        if (StringUtils.isBlank(stocksValue)) {
          stocksValue = Constant.DEFAULT_STOCK_VALUE;
        }
        stock = Double.parseDouble(stocksValue);
      } catch (Exception e) {
        resultErrorMessage =
            errorMessageBasedOnMerchant(isInternationalMerchant, BulkProcessValidationErrorMessages.STOCK_INVALID_EN,
                BulkProcessValidationErrorMessages.STOCK_INVALID);
        log.warn(BulkProcessValidationErrorMessages.WARNING_PREFIX, e);
        bulkUploadErrorCounter.incrementStock();
        addValidationErrorMessage(bulkUploadErrorCounter.getStock(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, : Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.STOCK_INVALID);
      }
      if (stock < 0) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.MINIMUM_STOCK_VALUE_INVALID_EN,
            BulkProcessValidationErrorMessages.MINIMUM_STOCK_VALUE_INVALID);
        bulkUploadErrorCounter.incrementStock();
        addValidationErrorMessage(bulkUploadErrorCounter.getStock(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));

        result = false;
        log.error("Validation error bulk process: {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.MINIMUM_STOCK_VALUE_INVALID);
      }
      if (stock > maxStockLimit) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            String.format(BulkProcessValidationErrorMessages.MAXIMUM_STOCK_VALUE_INVALID_EN, maxStockLimit),
            String.format(BulkProcessValidationErrorMessages.MAXIMUM_STOCK_VALUE_INVALID, maxStockLimit));
        bulkUploadErrorCounter.incrementStock();
        addValidationErrorMessage(bulkUploadErrorCounter.getStock(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process: {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.MAXIMUM_STOCK_VALUE_INVALID);
      }
    return result;
  }

  private static boolean validateProductPickUp(Map<String,Object> raw, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      String productName, StringBuilder validationErrorMessage, boolean result) {
    if (StringUtils.isBlank(String.valueOf(
        getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.PICKUP_POINT, GenericBulkHeaders.PICKUP_POINT_EN)))) {
      String resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementPickupPoint();
      addValidationErrorMessage(bulkUploadErrorCounter.getPickupPoint(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY,resultErrorMessage,StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row : {}. Product Name " + "- {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    }
    return result;
  }

  private static boolean validateDeliveryStatus(Object rawDeliveryStatus, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      String productName, StringBuilder validationErrorMessage, boolean result, boolean instoreSeller) {
    String resultErrorMessage;
    try {
      if (StringUtils.isBlank(String.valueOf(rawDeliveryStatus))) {
        rawDeliveryStatus =
            instoreSeller ? Constant.INSTORE_SELLER_DEFAULT_DELIVERY_STATUS : Constant.DEFAULT_DELIVERY_STATUS;
      }
      Integer.parseInt(String.valueOf(rawDeliveryStatus));
    } catch (Exception e) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.DELIVERY_STATUS_VALUE_INVALID_EN,
          BulkProcessValidationErrorMessages.DELIVERY_STATUS_VALUE_INVALID);
      bulkUploadErrorCounter.incrementDeliveryStatus();
      addValidationErrorMessage(bulkUploadErrorCounter.getDeliveryStatus(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      log.error("Validation error bulk process : {}, Row : {}. Product Name " + "- {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.DELIVERY_STATUS_VALUE_INVALID_EN);
      return false;
    }
    if (!validateDeliveryStatusOrCncActiveValue(Integer.parseInt(String.valueOf(rawDeliveryStatus)))) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.DELIVERY_STATUS_VALUE_INVALID_EN,
          BulkProcessValidationErrorMessages.DELIVERY_STATUS_VALUE_INVALID);
      bulkUploadErrorCounter.incrementDeliveryStatus();
      addValidationErrorMessage(bulkUploadErrorCounter.getDeliveryStatus(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row : {}. Product Name " + "- {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.DELIVERY_STATUS_VALUE_INVALID_EN);
    }
    return result;
  }

  private static boolean validateInstoreStatus(Object rawInstoreStatus, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      String productName, StringBuilder validationErrorMessage, boolean result, boolean instoreSeller) {
    String resultErrorMessage;
    if (instoreSeller) {
      try {
        if (StringUtils.isBlank(String.valueOf(rawInstoreStatus))) {
          rawInstoreStatus = Constant.DEFAULT_INSTORE_STATUS;
        }
        Integer.parseInt(String.valueOf(rawInstoreStatus));
      } catch (Exception e) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.INSTORE_STATUS_VALUE_INVALID_EN,
            BulkProcessValidationErrorMessages.INSTORE_STATUS_VALUE_INVALID_ID);
        bulkUploadErrorCounter.incrementDeliveryStatus();
        addValidationErrorMessage(bulkUploadErrorCounter.getDeliveryStatus(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        log.error("Validation error bulk process : {}, Row : {}. Product Name " + "- {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.INSTORE_STATUS_VALUE_INVALID_EN);
        return false;
      }
      if (!validateDeliveryStatusOrCncActiveValue(Integer.parseInt(String.valueOf(rawInstoreStatus)))) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.INSTORE_STATUS_VALUE_INVALID_EN,
            BulkProcessValidationErrorMessages.INSTORE_STATUS_VALUE_INVALID_ID);
        bulkUploadErrorCounter.incrementDeliveryStatus();
        addValidationErrorMessage(bulkUploadErrorCounter.getDeliveryStatus(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, Row : {}. Product Name " + "- {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.INSTORE_STATUS_VALUE_INVALID_EN);
      }
      return result;
    }
    return result;
  }

  public static boolean validateCncStatus(Object rawCncStatus, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      String productName, StringBuilder validationErrorMessage, boolean result, int merchantStatus,
      boolean bopisCncRestrictionEnabled, boolean isBopisProduct) {
    if (merchantStatus == MerchantStatusType.DELIVERY_AND_CNC.getType()
        || merchantStatus == MerchantStatusType.BFB_AND_CNC.getType()) {
      String resultErrorMessage;
        try {
          if (StringUtils.isBlank(String.valueOf(rawCncStatus))) {
            rawCncStatus = Constant.DEFAULT_CNC_STATUS;
          }
          Integer.parseInt(String.valueOf(rawCncStatus));
        } catch (Exception e) {
          resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
              BulkProcessValidationErrorMessages.CNC_STATUS_VALUE_INVALID_EN,
              BulkProcessValidationErrorMessages.CNC_STATUS_VALUE_INVALID);
          bulkUploadErrorCounter.incrementCncActive();
          addValidationErrorMessage(bulkUploadErrorCounter.getCncActive(), validationErrorMessage,
              errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
          log.error("Validation error bulk process : {}, Row : {}. Product Name " + "- {}. Error msg - {} : {}.",
              bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
              BulkErrorCategory.INPUT_ERROR.getDescription(),
              BulkProcessValidationErrorMessages.CNC_STATUS_VALUE_INVALID_EN);
          return false;
        }
        if (!validateDeliveryStatusOrCncActiveValue(Integer.parseInt(String.valueOf(rawCncStatus)))) {
          result = addCncStatusInvalidErrorMessage(bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant,
              columnRowInformation, productName, validationErrorMessage, BulkProcessValidationErrorMessages.CNC_STATUS_VALUE_INVALID_EN,
              BulkProcessValidationErrorMessages.CNC_STATUS_VALUE_INVALID);
        }
      if (bopisCncRestrictionEnabled && isBopisProduct && (1 == Integer.parseInt(
          String.valueOf(rawCncStatus)))) {
        result = addCncStatusInvalidErrorMessage(bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant,
            columnRowInformation, productName, validationErrorMessage,
            BulkProcessValidationErrorMessages.PRODUCT_CANT_BE_SET_CNC,
            BulkProcessValidationErrorMessages.PRODUCT_CANT_BE_SET_CNC);
        }
      }
    return result;
  }

  private static boolean isExternalUserOnly(String primaryIdentifier) {
    if (StringUtils.isNotBlank(primaryIdentifier)) {
      try {
        Map<String, String> map = objectMapper.readValue(primaryIdentifier, new TypeReference<>() {
        });
        return Boolean.parseBoolean(map.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, String.valueOf(false)));
      } catch (Exception e) {
        log.error("Error when trying to check if user is only external ", e);
      }
    }
    return false;
  }

  private static boolean addCncStatusInvalidErrorMessage(BulkProcessNotes bulkProcessNotes, BulkUploadErrorCounter bulkUploadErrorCounter,
      boolean isInternationalMerchant, String columnRowInformation, String productName,
      StringBuilder validationErrorMessage, String cncStatusValueInvalidEn, String cncStatusValueInvalid) {
    String resultErrorMessage;
    boolean result;
    resultErrorMessage =
        errorMessageBasedOnMerchant(isInternationalMerchant, cncStatusValueInvalidEn, cncStatusValueInvalid);
    bulkUploadErrorCounter.incrementCncActive();
    addValidationErrorMessage(bulkUploadErrorCounter.getCncActive(), validationErrorMessage,
        errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
    result = false;
    log.error("Validation error bulk process : {}, Row : {}. Product Name " + "- {}. Error msg - {} : {}.",
        bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
        BulkErrorCategory.INPUT_ERROR.getDescription(), cncStatusValueInvalidEn);
    return result;
  }

  public static boolean validateBfbBuyableStatus(Object rawDeliveryStatus, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      String productName, StringBuilder validationErrorMessage, boolean result) {
    String resultErrorMessage;
    try {
      if (StringUtils.isBlank(String.valueOf(rawDeliveryStatus))) {
        rawDeliveryStatus = Constant.DEFAULT_BFB_BUYABLE_FLAG;
      }
      Integer.parseInt(String.valueOf(rawDeliveryStatus));
    } catch (Exception e) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.BFB_BUYABLE_VALUE_INVALID_EN,
          BulkProcessValidationErrorMessages.BFB_BUYABLE_VALUE_INVALID_EN);
      bulkUploadErrorCounter.incrementDeliveryStatus();
      addValidationErrorMessage(bulkUploadErrorCounter.getDeliveryStatus(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      log.error("Validation error bulk process : {}, Row : {}. Product Name " + "- {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.BFB_BUYABLE_VALUE_INVALID_EN);
      return false;
    }

    if (!validateDeliveryStatusOrCncActiveValue(Integer.parseInt(String.valueOf(rawDeliveryStatus)))) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.BFB_BUYABLE_VALUE_INVALID_EN,
          BulkProcessValidationErrorMessages.BFB_BUYABLE_VALUE_INVALID_EN);
      bulkUploadErrorCounter.incrementCncActive();
      addValidationErrorMessage(bulkUploadErrorCounter.getCncActive(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row : {}. Product Name " + "- {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.BFB_BUYABLE_VALUE_INVALID_EN);
    }
    return result;
  }

  public static boolean validateBfbManagedStatus(Object rawManageStatus, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      String productName, StringBuilder validationErrorMessage, boolean result) {
    String resultErrorMessage;
    try {
      if (StringUtils.isBlank(String.valueOf(rawManageStatus))) {
        rawManageStatus = Constant.DEFAULT_BFB_MANAGED_FLAG;
      }
      Integer.parseInt(String.valueOf(rawManageStatus));
    } catch (Exception e) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.BFB_MANAGED_VALUE_INVALID_EN,
          BulkProcessValidationErrorMessages.BFB_MANAGED_VALUE_INVALID_EN);
      bulkUploadErrorCounter.incrementDeliveryStatus();
      addValidationErrorMessage(bulkUploadErrorCounter.getDeliveryStatus(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      log.error("Validation error bulk process : {}, Row : {}. Product Name " + "- {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.BFB_MANAGED_VALUE_INVALID_EN);
      return false;
    }

    if (!validateDeliveryStatusOrCncActiveValue(Integer.parseInt(String.valueOf(rawManageStatus)))) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.BFB_MANAGED_VALUE_INVALID_EN,
          BulkProcessValidationErrorMessages.BFB_MANAGED_VALUE_INVALID_EN);
      bulkUploadErrorCounter.incrementCncActive();
      addValidationErrorMessage(bulkUploadErrorCounter.getCncActive(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row : {}. Product Name " + "- {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.BFB_MANAGED_VALUE_INVALID_EN);
    }
    return result;
  }

  private static boolean validateDeliveryStatusOrCncActiveValue(int value) {
    return value == 0 || value == 1;
  }

  private static boolean validateProductLength(Map<String, Object> raw, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      String productName, StringBuilder validationErrorMessage, boolean result, boolean pureInstoreProduct) {
    Double length = 0.0;
    String resultErrorMessage;
    if (StringUtils.isBlank(
        String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.LENGTH, GenericBulkHeaders.LENGTH_EN)))
        && !pureInstoreProduct) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.LENGTH_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.LENGTH_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementLength();
      addValidationErrorMessage(bulkUploadErrorCounter.getLength(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row : {}. Product Name " + "- {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.LENGTH_MUST_NOT_BE_BLANK);
    } else {
      try {
        length = parseDimensionForValidation(
            String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.LENGTH, GenericBulkHeaders.LENGTH_EN)));
      } catch (Exception e) {
        resultErrorMessage =
            errorMessageBasedOnMerchant(isInternationalMerchant, BulkProcessValidationErrorMessages.LENGTH_INVALID_EN,
                BulkProcessValidationErrorMessages.LENGTH_INVALID);
        log.warn(BulkProcessValidationErrorMessages.WARNING_PREFIX, e);
        bulkUploadErrorCounter.incrementLength();
        addValidationErrorMessage(bulkUploadErrorCounter.getLength(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.LENGTH_INVALID);
      }
      if (Double.compare(length, MINIMUM_VALUE) < 0) {
        bulkUploadErrorCounter.incrementLength();
        result = populateErrorMessageForDimensionInvalid(bulkProcessNotes, bulkUploadErrorCounter, columnRowInformation,
            productName, validationErrorMessage, isInternationalMerchant ?
                BulkProcessValidationErrorMessages.MINIMUM_LENGTH_VALUE_INVALID_EN :
                BulkProcessValidationErrorMessages.MINIMUM_LENGTH_VALUE_INVALID);
      } else if (!pureInstoreProduct && Double.compare(length, MINIMUM_VALUE) <= 0) {
        bulkUploadErrorCounter.incrementLength();
        result = populateErrorMessageForDimensionInvalid(bulkProcessNotes, bulkUploadErrorCounter, columnRowInformation,
            productName, validationErrorMessage, isInternationalMerchant ?
                BulkProcessValidationErrorMessages.MINIMUM_LENGTH_VALUE_INVALID_EN :
                BulkProcessValidationErrorMessages.MINIMUM_LENGTH_VALUE_INVALID);
      }
    }
    return result;
  }

  private static boolean validateProductWidth(Map<String, Object> raw, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      String productName, StringBuilder validationErrorMessage, boolean result, boolean pureInstoreProduct) {
    String resultErrorMessage;
    Double width = 0.0;
    if (StringUtils.isBlank(
        String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.WIDTH, GenericBulkHeaders.WIDTH)))
        && !pureInstoreProduct) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.WIDTH_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.WIDTH_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementWidth();
      addValidationErrorMessage(bulkUploadErrorCounter.getWidth(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row : {}. Product " + "Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.WIDTH_MUST_NOT_BE_BLANK);
    } else {
      try {
        width = parseDimensionForValidation(
            String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.WIDTH, GenericBulkHeaders.WIDTH_EN)));
      } catch (Exception e) {
        resultErrorMessage =
            errorMessageBasedOnMerchant(isInternationalMerchant, BulkProcessValidationErrorMessages.WIDTH_INVALID_EN,
                BulkProcessValidationErrorMessages.WIDTH_INVALID);
        log.warn(BulkProcessValidationErrorMessages.WARNING_PREFIX, e);
        bulkUploadErrorCounter.incrementWidth();
        addValidationErrorMessage(bulkUploadErrorCounter.getWidth(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, : Row : {}. Product " + "Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.WIDTH_INVALID);
      }
      if (Double.compare(width, MINIMUM_VALUE) < 0) {
        bulkUploadErrorCounter.incrementWidth();
        result =
            populateErrorMessageForDimensionInvalid(bulkProcessNotes, bulkUploadErrorCounter,
                columnRowInformation, productName, validationErrorMessage, isInternationalMerchant ?
                    BulkProcessValidationErrorMessages.MINIMUM_WIDTH_VALUE_INVALID_EN :
                    BulkProcessValidationErrorMessages.MINIMUM_WIDTH_VALUE_INVALID);
      } else if (!pureInstoreProduct && Double.compare(width, MINIMUM_VALUE) <= 0) {
        bulkUploadErrorCounter.incrementWidth();
        result =
            populateErrorMessageForDimensionInvalid(bulkProcessNotes, bulkUploadErrorCounter,
                columnRowInformation, productName, validationErrorMessage, isInternationalMerchant ?
                    BulkProcessValidationErrorMessages.MINIMUM_WIDTH_VALUE_INVALID_EN :
                    BulkProcessValidationErrorMessages.MINIMUM_WIDTH_VALUE_INVALID);
      }
    }
    return result;
  }

  private static boolean validateProductHeight(Map<String, Object> raw, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      String productName, StringBuilder validationErrorMessage, boolean result, boolean pureInstoreProduct) {
    String resultErrorMessage;
    Double height = 0.0;
    if (StringUtils.isBlank(
        String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.HEIGTH, GenericBulkHeaders.HEIGTH_EN)))
        && !pureInstoreProduct) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.HEIGHT_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.HEIGHT_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementHeight();
      addValidationErrorMessage(bulkUploadErrorCounter.getHeight(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.HEIGHT_MUST_NOT_BE_BLANK);
    } else {
      try {
        height = parseDimensionForValidation(
            String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.HEIGTH, GenericBulkHeaders.HEIGTH_EN)));
      } catch (Exception e) {
        resultErrorMessage =
            errorMessageBasedOnMerchant(isInternationalMerchant, BulkProcessValidationErrorMessages.HEIGHT_INVALID_EN,
                BulkProcessValidationErrorMessages.HEIGHT_INVALID);
        log.warn(BulkProcessValidationErrorMessages.WARNING_PREFIX, e);
        bulkUploadErrorCounter.incrementHeight();
        addValidationErrorMessage(bulkUploadErrorCounter.getHeight(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.HEIGHT_INVALID);
      }
      if (Double.compare(height, MINIMUM_VALUE) < 0) {
        bulkUploadErrorCounter.incrementHeight();
        result = populateErrorMessageForDimensionInvalid(bulkProcessNotes, bulkUploadErrorCounter, columnRowInformation,
            productName, validationErrorMessage, isInternationalMerchant ?
                BulkProcessValidationErrorMessages.MINIMUM_HEIGHT_VALUE_INVALID_EN :
                BulkProcessValidationErrorMessages.MINIMUM_HEIGHT_VALUE_INVALID);
      } else if (!pureInstoreProduct && Double.compare(height, MINIMUM_VALUE) <= 0) {
        bulkUploadErrorCounter.incrementHeight();
        result = populateErrorMessageForDimensionInvalid(bulkProcessNotes, bulkUploadErrorCounter, columnRowInformation,
            productName, validationErrorMessage, isInternationalMerchant ?
                BulkProcessValidationErrorMessages.MINIMUM_HEIGHT_VALUE_INVALID_EN :
                BulkProcessValidationErrorMessages.MINIMUM_HEIGHT_VALUE_INVALID);
      }
    }
    return result;
  }

  private static boolean populateErrorMessageForDimensionInvalid(BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, String columnRowInformation,
      String productName, StringBuilder validationErrorMessage, String errorMessage) {
    boolean result;
    addValidationErrorMessage(bulkUploadErrorCounter.getHeight(), validationErrorMessage,
        errorMessage(StringUtils.EMPTY, errorMessage, StringUtils.EMPTY));
    result = false;
    log.error("Validation error bulk process : {}, Row : {}. Product " + "Name - {}. Error msg - {} : {}.",
        bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
        BulkErrorCategory.INPUT_ERROR.getDescription(), errorMessage);
    return result;
  }

  private static boolean validateProductWeight(Map<String, Object> raw, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      String productName, StringBuilder validationErrorMessage, boolean result, boolean pureInstoreProduct) {
    String resultErrorMessage;
    Double weight = 0.0;
    if (StringUtils.isBlank(
        String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.WEIGHT, GenericBulkHeaders.WEIGHT_EN)))
        && !pureInstoreProduct) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.WEIGHT_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.WEIGHT_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementWeight();
      addValidationErrorMessage(bulkUploadErrorCounter.getWeight(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.WEIGHT_MUST_NOT_BE_BLANK);
    } else {
      try {
        weight = parseDimensionForValidation(
            String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.WEIGHT, GenericBulkHeaders.WEIGHT_EN)));
      } catch (Exception e) {
        resultErrorMessage =
            errorMessageBasedOnMerchant(isInternationalMerchant, BulkProcessValidationErrorMessages.WEIGHT_INVALID_EN,
                BulkProcessValidationErrorMessages.WEIGHT_INVALID);
        log.warn(BulkProcessValidationErrorMessages.WARNING_PREFIX, e);
        bulkUploadErrorCounter.incrementWeight();
        addValidationErrorMessage(bulkUploadErrorCounter.getWeight(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.WEIGHT_INVALID);
      }
      if (weight < Constant.WEIGHT && !isBopisProduct(raw)) {
        resultErrorMessage =
            errorMessageBasedOnMerchant(isInternationalMerchant, BulkProcessValidationErrorMessages.WRONG_WEIGHT_EN,
                BulkProcessValidationErrorMessages.WRONG_WEIGHT);
        bulkUploadErrorCounter.incrementWeight();
        addValidationErrorMessage(bulkUploadErrorCounter.getWeight(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), columnRowInformation, productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.WRONG_WEIGHT);
      }
    }
    return result;
  }

  private static boolean isShippingTypeNotBopis(Map<String, Object> raw, String idHeader, String engHeader) {
    return !BulkUploadOption.SHIPPING_TYPE_BOPIS.getdescription()
        .equals(String.valueOf(getValueBasedOnEnOrIdHeader(raw, idHeader, engHeader)));
  }

  public static boolean validateEANValue(Map<String, Object> raw, BulkUploadErrorCounter bulkUploadErrorCounter,
      boolean isInternationalMerchant, boolean valid, StringBuilder validationErrorMessage, String columnRowInformation,
      String blpCode, Set<String> eanUpcSet, List<Integer> eanUpcValidLength) {
    String resultErrorMessage;
    boolean validType = true;
    if (StringUtils.isNotBlank(
        String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.UPC, GenericBulkHeaders.UPC_EN)))) {
      String upcValue = String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.UPC, GenericBulkHeaders.UPC_EN));
      try {
        Long.parseLong(String.valueOf(upcValue));
      } catch (NumberFormatException nfe) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.EAN_UPC_VALUES_INVALID_EN,
            BulkProcessValidationErrorMessages.EAN_UPC_VALUES_INVALID);
        bulkUploadErrorCounter.incrementVariation();
        addValidationErrorMessage(bulkUploadErrorCounter.getVariation(), validationErrorMessage,
           errorMessage(StringUtils.EMPTY,resultErrorMessage,StringUtils.EMPTY));
        valid = false;
        validType = false;
        log.error("{}, Row : {} . Error msg - {}. Attribute Name - {}.", blpCode, columnRowInformation,
            BulkProcessValidationErrorMessages.EAN_UPC_VALUES_INVALID, Constant.EAN_UPC);
      }

      int length = String.valueOf(upcValue).length();
      if (!eanUpcValidLength.contains(length) && validType) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.EAN_UPC_VALUES_INVALID_EN,
            BulkProcessValidationErrorMessages.EAN_UPC_VALUES_INVALID);
        bulkUploadErrorCounter.incrementVariation();
        addValidationErrorMessage(bulkUploadErrorCounter.getVariation(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        valid = false;
        log.error("{} Row : {} . Error msg - {}. Attribute Name - {}.", blpCode, columnRowInformation,
            BulkProcessValidationErrorMessages.EAN_UPC_VALUES_INVALID, Constant.EAN_UPC);
      }
      if (valid && eanUpcSet.contains(String.valueOf(upcValue))) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.DUPLICATE_EAN_UPC_VALUES_INVALID_EN,
          BulkProcessValidationErrorMessages.DUPLICATE_EAN_UPC_VALUES_INVALID);
        bulkUploadErrorCounter.incrementVariation();
        addValidationErrorMessage(bulkUploadErrorCounter.getVariation(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        valid = false;
        log.error("{} Row : {} . Error msg - {}. Attribute Name - {}.", blpCode,
          columnRowInformation,
          BulkProcessValidationErrorMessages.DUPLICATE_EAN_UPC_VALUES_INVALID_EN, Constant.EAN_UPC);
      } else {
        eanUpcSet.add(upcValue);
      }
    }
    return valid;
  }

  public static boolean validateDefiningOrVariantCreation(Map<String, Object> row, String attributeValue, boolean isInternationalMerchant,
      String columnRowInformation, List<String> attributeValues, AttributeResponse attribute, String blpCode,
      BulkUploadErrorCounter bulkUploadErrorCounter, StringBuilder validationErrorMessage, String attributeName,
      boolean valid, String sizeChartDelimiter) {
    String resultErrorMessage;
    if (StringUtils.isBlank(String.valueOf(attributeValue)) && attribute.isMandatory()) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementVariation();
      addValidationErrorMessage(bulkUploadErrorCounter.getVariation(), validationErrorMessage,
          errorMessage(getAttributeName(isInternationalMerchant, attribute) + StringUtils.SPACE, resultErrorMessage,
              StringUtils.EMPTY));
      valid = false;
      log.error("{} Row : {} . Error msg - {}. Attribute Name - {}.", blpCode, columnRowInformation,
          BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID, attributeName);
    } else if ((StringUtils.isNotBlank(attributeValue) && validateDefiningAttributeValues(row, attribute, attributeValues, sizeChartDelimiter) && !attributeValues.contains(attributeValue))) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID_EN,
          BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID);
      bulkUploadErrorCounter.incrementVariation();
      addValidationErrorMessage(bulkUploadErrorCounter.getVariation(), validationErrorMessage,
          errorMessage(getAttributeName(isInternationalMerchant, attribute) + StringUtils.SPACE, resultErrorMessage,
              StringUtils.EMPTY));
      valid = false;
      log.error("{} Row : {} . Error msg - {}. Attribute Name - {}.", blpCode, columnRowInformation,
          BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID, attributeName);
    }
    return valid;
  }

  public static List<Map<String, Object>> adjustValidAndInvalidRowsBasedOnParentId(BulkProcess bulkProcess,
      List<Map<String, Object>> validRows, List<Map<String, Object>> inValidRows, List<Integer> validRowNumbers,
      boolean isInternationalMerchant) {
    List<Map<String, Object>> newValidRows = new ArrayList<>();
    Map<Object, Object> inValidParentIdParentIdMap = inValidRows.stream().map(invalidRow -> String.valueOf(
            getValueBasedOnEnOrIdHeader(invalidRow, GenericBulkHeaders.PARENT, GenericBulkHeaders.PARENT_EN)))
        .filter(StringUtils::isNotBlank).distinct().collect(Collectors.toMap(Function.identity(), Function.identity()));
    for (Map<String, Object> row : validRows) {
      if (Objects.isNull(getValueBasedOnEnOrIdHeader(row, GenericBulkHeaders.PARENT, GenericBulkHeaders.PARENT_EN))
          || Objects.isNull(inValidParentIdParentIdMap.get(
          getValueBasedOnEnOrIdHeader(row, GenericBulkHeaders.PARENT, GenericBulkHeaders.PARENT_EN)))) {
        newValidRows.add(row);
        validRowNumbers.add((Integer) row.get(GenericBulkHeaders.ROW_NUMBER));
      } else {
        String resultErrorMessage=errorMessageBasedOnMerchant(isInternationalMerchant,BulkProcessValidationErrorMessages.INVALID_ROW_BY_PARENT_ID_MESSAGE_EN,BulkProcessValidationErrorMessages.INVALID_ROW_BY_PARENT_ID_MESSAGE);
        BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
        bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
        bulkProcessNotes.setBulkProcess(bulkProcess);
        bulkProcessNotes.setNotes(Constant.ROW + row.get(GenericBulkHeaders.ROW_NUMBER) + Constant.PERIOD);
        bulkProcessNotes
            .setNotes(errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY) + Constant.PERIOD);
        row.put(GenericBulkHeaders.BULK_PROCESS_NOTES, bulkProcessNotes.getNotes());
        inValidRows.add(row);
        bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
      }
    }
    return newValidRows;
  }

  public static void generateProductItemNamesForGenericCreation(List<Map<String, Object>> validRows,
      Map<String, List<String>> categoryVariantCreationAttributes) {
    // Generate product item names
    String productName = String.valueOf(getValueBasedOnEnOrIdHeader(validRows.get(0), GenericBulkHeaders.PRODUCT_NAME,
        GenericBulkHeaders.PRODUCT_NAME_EN));
    for (Map<String, Object> row : validRows) {
      StringBuilder productItemName = new StringBuilder(productName);
      if (MapUtils.isNotEmpty(categoryVariantCreationAttributes) && categoryVariantCreationAttributes
          .containsKey(row.get(GenericBulkHeaders.CN_CATEGORY_ID))) {
        for (String columnAttribute : categoryVariantCreationAttributes.get(
            row.get(GenericBulkHeaders.CN_CATEGORY_ID))) {
          String attributeValue = String.valueOf(row.get(columnAttribute));
          if (StringUtils.isBlank(attributeValue)) {
            productItemName.append(StringUtils.EMPTY);
          } else {
            productItemName.append(StringUtils.SPACE + attributeValue);
          }
        }
      }
      row.put(GenericBulkHeaders.GENERATED_ITEM_NAME, productItemName.toString());
    }
  }

  public static ProductCreationRequest convertUserRowsToProductCollectionRequests(List<Map<String, Object>> groupRow,
      Map<String, AttributeResponse> attributeIdAndResponse, BulkProcess bulkProcess, ProfileResponse businessPartner,
      Map<String, CategoryDetailAndShippingResponse> categoryIdAndDetailMap, Map<String, String> attributeNameAndIdMap,
      Map<String, String> variantCreationNameAndColumn, boolean isInternationalMerchant,
      GenerateShippingWeightRequest generateShippingWeightRequest, MerchantStatusType merchantStatusType,
      String ppNameDelimiter, boolean cncForWarehouseFeatureSwitch, boolean instoreSeller) {
    Map<String, Object> parentRow = groupRow.stream().findFirst().orElse(new HashMap<>());
    Map<String, String> otherAttributeNameAndValueMap = getOtherAttributesNames(parentRow);
    // Initialize product attributes with attribute type DESCRIPTIVE_ATTRIBUTE and PREDEFINED_ATTRIBUTE
    CategoryDetailAndShippingResponse categoryDetailResponse =
        categoryIdAndDetailMap.get(parentRow.get(GenericBulkHeaders.CN_CATEGORY_ID));
    // Initialize product request
    ProductCreationRequest productCollectionRequest =
        prepareProductRequestForGenericCreation(bulkProcess, parentRow, businessPartner, categoryDetailResponse,
            instoreSeller, groupRow);
    // Fetch the category detail associated with the current group of products
    initializeNonVariantCreationAttributes(bulkProcess, isInternationalMerchant, otherAttributeNameAndValueMap,
        productCollectionRequest, categoryDetailResponse, attributeIdAndResponse);
    // Set value product brand
    setBrandAttributeValue(attributeIdAndResponse, bulkProcess, attributeNameAndIdMap, isInternationalMerchant,
        parentRow, productCollectionRequest);
    // Initialize product attributes with attribute type DEFINING_ATTRIBUTE and variant creation
    initializeVariantCreationAttributes(bulkProcess, variantCreationNameAndColumn, isInternationalMerchant,
        groupRow, productCollectionRequest, categoryDetailResponse, attributeIdAndResponse);
    //Initialize items
    for (Map<String, Object> currentRow : groupRow) {
      initializeItemDetailsForGenericCreation(variantCreationNameAndColumn, productCollectionRequest,
          categoryDetailResponse, currentRow, parentRow, merchantStatusType, ppNameDelimiter,
          cncForWarehouseFeatureSwitch, instoreSeller);
    }
    productCollectionRequest.setBundleProduct(CollectionUtils.isNotEmpty(
        productCollectionRequest.getProductItemRequests().stream().findFirst().orElse(new ProductItemCreationRequest())
            .getBundleRecipe()));
    // Initialize product categories
    initializeProductCategory(bulkProcess.getStoreId(), productCollectionRequest, categoryDetailResponse);
    // Initialize product shipping weight
    initializeShippingWeight(productCollectionRequest, categoryDetailResponse, generateShippingWeightRequest);
    productCollectionRequest.setPrioritySeller(getPriority(bulkProcess));
    return productCollectionRequest;
  }

  private static void setBrandAttributeValue(Map<String, AttributeResponse> attributes, BulkProcess bulkProcess,
      Map<String, String> attributeNameAndIdMap, boolean isInternationalMerchant, Map<String, Object> parentRaw,
      ProductCreationRequest productCollectionRequest) {
    String brandValue =
        String.valueOf(getValueBasedOnEnOrIdHeader(parentRaw, GenericBulkHeaders.BRAND, GenericBulkHeaders.BRAND_EN));
    String cleanBrandValue;
    if (brandValue.contains(Constant.IN_REVIEW)) {
      cleanBrandValue = brandValue.substring(0, brandValue.indexOf(Constant.IN_REVIEW_BRAND_SUFFIX));
    } else {
      cleanBrandValue = brandValue;
    }
    AttributeResponse brandResponse = attributes.get(attributeNameAndIdMap.get(GenericBulkParameters.BRAND));
    Optional<PredefinedAllowedAttributeValueResponse> optionalBrandValueResponse =
        brandResponse.getPredefinedAllowedAttributeValues().stream()
            .filter(predefinedValue -> predefinedValue.getValue().equals(cleanBrandValue)).findFirst();
    ProductAttributeValueRequest brandAttributeValueRequest = new ProductAttributeValueRequest();
    brandAttributeValueRequest.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
    brandAttributeValueRequest.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
    brandAttributeValueRequest.getPredefinedAllowedAttributeValue()
        .setId(optionalBrandValueResponse.map(PredefinedAllowedAttributeValueResponse::getId).orElse(StringUtils.EMPTY));
    brandAttributeValueRequest.getPredefinedAllowedAttributeValue().setValue(
        optionalBrandValueResponse.map(PredefinedAllowedAttributeValueResponse::getValue).orElse(StringUtils.EMPTY));
    ProductAttributeRequest brandAttributeRequest =
        setValuesForProductAttributeRequest(bulkProcess.getStoreId(), brandResponse, isInternationalMerchant);
    brandAttributeRequest.setProductAttributeValues(new ArrayList<>());
    brandAttributeRequest.getProductAttributeValues().add(brandAttributeValueRequest);
    productCollectionRequest.getProductAttributes().add(brandAttributeRequest);
    productCollectionRequest
        .setBrand(optionalBrandValueResponse.map(PredefinedAllowedAttributeValueResponse::getValue).orElse(null));
    productCollectionRequest.setBrandCode(
        optionalBrandValueResponse.map(PredefinedAllowedAttributeValueResponse::getPredefinedAllowedAttributeCode)
            .orElse(null));
    brandAttributeValueRequest.getPredefinedAllowedAttributeValue().setPredefinedAllowedAttributeCode(
        optionalBrandValueResponse.map(PredefinedAllowedAttributeValueResponse::getPredefinedAllowedAttributeCode)
            .orElse(null));
    if (brandValue.contains(Constant.IN_REVIEW)) {
      productCollectionRequest.setBrandApprovalStatus(BrandApprovalStatus.DRAFT.name());
    } else {
      productCollectionRequest.setBrandApprovalStatus(
          optionalBrandValueResponse.map(PredefinedAllowedAttributeValueResponse::getBrandApprovalStatus).orElse(null));
    }
  }

  private static void initializeVariantCreationAttributes(BulkProcess bulkProcess,
      Map<String, String> variantCreationNameAndColumnIndex, boolean isInternationalMerchant,
      List<Map<String,Object>> groupRow, ProductCreationRequest productCollectionRequest,
      CategoryDetailAndShippingResponse categoryDetailResponse, Map<String, AttributeResponse> attributeIdAndResponse) {
    for (CategoryAttributeResponse categoryAttributeResponse : categoryDetailResponse.getCategoryAttributes()) {
      if (!categoryAttributeResponse.isMarkForDelete() && isDefiningOrVariantCreation(
          categoryAttributeResponse.getAttribute())) {
        AttributeResponse attribute = attributeIdAndResponse.get(categoryAttributeResponse.getAttribute().getId());
        if (Objects.nonNull(attribute)) {
          Map<String, String> attributeValueInsensitiveAttributeValueMap = new HashMap<>();
          // Search defining and variant creation attribute values per column
          for (Map<String, Object> raw : groupRow) {
            Map<String, Object> removedMandatory = removeMandatoryCharactersFromHeaders(raw);
            String attributeValue = StringUtils.EMPTY;
            if (variantCreationNameAndColumnIndex.containsKey(attribute.getName())) {
              attributeValue = String.valueOf(removedMandatory.get(attribute.getName()));
            } else if (variantCreationNameAndColumnIndex.containsKey(attribute.getNameEnglish())) {
              attributeValue = String.valueOf(removedMandatory.get(attribute.getNameEnglish()));
            }
            String lowerCaseAttributeValue = StringUtils.lowerCase(attributeValue);
            if (!StringUtils.isBlank(attributeValue) && !attributeValueInsensitiveAttributeValueMap
                .containsKey(lowerCaseAttributeValue)) {
              attributeValueInsensitiveAttributeValueMap.put(lowerCaseAttributeValue, attributeValue);
            }
          }
          if (!CollectionUtils.isEmpty(attributeValueInsensitiveAttributeValueMap.values())) {
            ProductAttributeRequest productAttributeRequest =
                setValuesForProductAttributeRequest(bulkProcess.getStoreId(), attribute, isInternationalMerchant);
            productAttributeRequest.setProductAttributeValues(new ArrayList<>());
            for (String attributeValue : attributeValueInsensitiveAttributeValueMap.values()) {
              ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
              productAttributeValueRequest.setStoreId(bulkProcess.getStoreId());
              initializeAttributes(attribute, productAttributeValueRequest, attributeValue);
              productAttributeRequest.getProductAttributeValues().add(productAttributeValueRequest);
            }
            productCollectionRequest.getProductAttributes().add(productAttributeRequest);
          }
        }
      }
    }
  }

  private static void initializeItemDetailsForGenericCreation(Map<String, String> variantCreationNameAndColumn,
      ProductCreationRequest productCollectionRequest, CategoryDetailResponse categoryDetailResponse,
      Map<String, Object> currentRow, Map<String, Object> parentRow, MerchantStatusType merchantStatusType,
      String ppNameDelimiter, boolean cncForWarehouseFeatureSwitch, boolean instoreSeller) {
    ProductItemCreationRequest productItemRequest = new ProductItemCreationRequest();
    List<ProductItemAttributeValueRequest> itemAttributeValueRequests = new ArrayList<>();
    TreeMap<String, String> attributeMap = new TreeMap<>();
    List<PickupPointCreateRequest> pickupPoints=new ArrayList<>();
    pickupPoints.add(
        setItemPickupPointDetails(currentRow, parentRow, merchantStatusType, ppNameDelimiter,
            cncForWarehouseFeatureSwitch, instoreSeller));
    // Add family colour only if warna attribute is filled
    boolean hasWarnaValue = false;

    for (CategoryAttributeResponse categoryAttributeResponse : categoryDetailResponse.getCategoryAttributes()) {
      AttributeResponse attribute = categoryAttributeResponse.getAttribute();
      // Initialize variant creation attributes
      if (!attribute.isMarkForDelete() && isDefiningOrVariantCreation(attribute)) {
        String attributeName = variantCreationNameAndColumn.getOrDefault(attribute.getName(),
                variantCreationNameAndColumn.getOrDefault(attribute.getNameEnglish(), StringUtils.EMPTY));
        String attributeValue = String.valueOf(currentRow.get(attributeName));
        // Create attribute map of defining attributes
        // Attribute value can be empty for non mandatory defining attribute
        if (StringUtils.isNotBlank(attributeValue)) {
          if (Constant.WARNA.equals(attribute.getName())) {
            hasWarnaValue = true;
          }
          if(Objects.nonNull(currentRow.get(attributeName))) {
            attributeMap.putIfAbsent(attribute.getAttributeCode(), attributeValue);
          }
        }
      }
    }

    for (CategoryAttributeResponse categoryAttributeResponse : categoryDetailResponse.getCategoryAttributes()) {
      AttributeResponse attribute = categoryAttributeResponse.getAttribute();
      if (hasWarnaValue && !attribute.isMarkForDelete() && GenericBulkParameters.FAMILY_COLOUR
          .equals(attribute.getName())) {
        // Add family colour if present
        AttributeRequest attributeRequest = getAttributeRequest(categoryAttributeResponse, attribute);
        String attributeValue = String.valueOf(getValueBasedOnEnOrIdHeader(currentRow, GenericBulkHeaders.FAMILY_COLOUR,
            GenericBulkHeaders.FAMILY_COLOUR_EN));
        itemAttributeValueRequests.add(new ProductItemAttributeValueRequest(attributeRequest, attributeValue,
            productCollectionRequest.getStoreId()));
      }
    }
    // Set all item related information
    setItemDetails(productItemRequest, currentRow, parentRow, ppNameDelimiter);
    if (CollectionUtils.isNotEmpty(pickupPoints)){
      productItemRequest.setPickupPoints(pickupPoints);
    }
    productItemRequest.setAttributesMap(attributeMap);
    productItemRequest.setProductItemAttributeValueRequests(itemAttributeValueRequests);
    productCollectionRequest.getProductItemRequests().add(productItemRequest);
  }

  private static AttributeRequest getAttributeRequest(CategoryAttributeResponse categoryAttributeResponse,
      AttributeResponse attribute) {
    AttributeRequest attributeRequest = new AttributeRequest();
    BeanUtils.copyProperties(categoryAttributeResponse.getAttribute(), attributeRequest, "attributeType",
        "allowedAttributeValues", "predefinedAllowedAttributeValues");
    attributeRequest.setAttributeType(AttributeType.valueOf(attribute.getAttributeType()));
    return attributeRequest;
  }

  private static ProductCreationRequest prepareProductRequestForGenericCreation(BulkProcess bulkProcess,
      Map<String, Object> parentRaw, ProfileResponse businessPartner,
      CategoryDetailAndShippingResponse categoryDetailResponse, boolean instoreSeller,
      List<Map<String, Object>> groupRow) {
    boolean pureInstoreProduct = BulkCreationCommonUtil.isPureInstoreProduct(groupRow, instoreSeller, true);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setStoreId(bulkProcess.getStoreId());
    productCreationRequest.setCreatedBy(bulkProcess.getCreatedBy());
    productCreationRequest.setCreatedDate(Calendar.getInstance().getTime());
    productCreationRequest.setUpdatedBy(bulkProcess.getUpdatedBy());
    productCreationRequest.setUpdatedDate(new Date());
    productCreationRequest.setName(String.valueOf(
        getValueBasedOnEnOrIdHeader(parentRaw, GenericBulkHeaders.PRODUCT_NAME, GenericBulkHeaders.PRODUCT_NAME_EN)));
    productCreationRequest.setUrl(String.valueOf(
        getValueBasedOnEnOrIdHeader(parentRaw, GenericBulkHeaders.URL_VIDEO, GenericBulkHeaders.URL_VIDEO_EN)));
    setProductDimensionForGeneric(parentRaw, productCreationRequest, pureInstoreProduct);
    String formattedDescription = getParagraphString(String.valueOf(getValueBasedOnEnOrIdHeader(parentRaw, GenericBulkHeaders.DESCRIPTION, GenericBulkHeaders.DESCRIPTION_EN)));
    productCreationRequest.setDescription(formattedDescription.getBytes());
    productCreationRequest.setLongDescription(formattedDescription.getBytes());
    productCreationRequest.setUniqueSellingPoint(getParagraphString(
        String.valueOf(getValueBasedOnEnOrIdHeader(parentRaw, GenericBulkHeaders.USP, GenericBulkHeaders.USP_EN))));
    productCreationRequest.setSpecificationDetail(GenericBulkParameters.SEPARATOR);
    productCreationRequest.setUom(GenericBulkParameters.DEFAULT_UOM);
    productCreationRequest.setProductAttributes(new ArrayList<>());
    productCreationRequest.setProductCategories(new ArrayList<>());
    productCreationRequest.setBusinessPartnerCode(businessPartner.getBusinessPartnerCode());
    productCreationRequest.setBusinessPartnerName(businessPartner.getCompany().getBusinessPartnerName());
    if (businessPartner.getCompany().isInternationalFlag() && StringUtils
        .isNotEmpty(categoryDetailResponse.getNameEnglish())) {
      productCreationRequest.setCategoryName(categoryDetailResponse.getNameEnglish());
    } else {
      productCreationRequest.setCategoryName(categoryDetailResponse.getName());
    }
    if (instoreSeller) {
      Set<ProductSalesChannelType> productSalesChannelTypes = getProductSalesChannel(instoreSeller, groupRow, true);
      productCreationRequest.setB2cActivated(productSalesChannelTypes.contains(ProductSalesChannelType.B2C_PRODUCT));
      productCreationRequest.setOff2OnChannelActive(
          productSalesChannelTypes.contains(ProductSalesChannelType.INSTORE_PRODUCT));
    }
    return productCreationRequest;
  }

  private static void setProductDimensionForGeneric(Map<String, Object> parentRaw,
      ProductCreationRequest productCreationRequest, boolean pureInstoreProduct) {
    double length = 0.0;
    double width = 0.0;
    double height = 0.0;
    double weight = 0.0;
    if (isShippingTypeNotBopis(parentRaw, GenericBulkHeaders.PRODUCT_TYPE, GenericBulkHeaders.PRODUCT_TYPE_EN)) {
      length = parseDimensionForRequestFormation(String.valueOf(
          getValueBasedOnEnOrIdHeader(parentRaw, GenericBulkHeaders.LENGTH, GenericBulkHeaders.LENGTH_EN)));
      width = parseDimensionForRequestFormation(String.valueOf(
          getValueBasedOnEnOrIdHeader(parentRaw, GenericBulkHeaders.WIDTH, GenericBulkHeaders.WIDTH_EN)));
      height = parseDimensionForRequestFormation(String.valueOf(
          getValueBasedOnEnOrIdHeader(parentRaw, GenericBulkHeaders.HEIGTH, GenericBulkHeaders.HEIGTH_EN)));
      weight = getKilogramFromGram(parseDimensionForRequestFormation(String.valueOf(
          getValueBasedOnEnOrIdHeader(parentRaw, GenericBulkHeaders.WEIGHT, GenericBulkHeaders.WEIGHT_EN))));
      if (pureInstoreProduct && Stream.of(length, width, height, weight)
          .anyMatch(dimension -> Double.compare(dimension, MINIMUM_VALUE) == 0)) {
        length = 0.0;
        width = 0.0;
        height = 0.0;
        weight = 0.0;
      }
    }
    productCreationRequest.setLength(length);
    productCreationRequest.setWidth(width);
    productCreationRequest.setHeight(height);
    productCreationRequest.setWeight(weight);
  }

  private static void initializeNonVariantCreationAttributes(BulkProcess bulkProcess, boolean isInternationalMerchant,
      Map<String, String> otherAttributeNameAndValueMap, ProductCreationRequest productCollectionRequest,
      CategoryDetailAndShippingResponse categoryDetailResponse, Map<String, AttributeResponse> attributeIdAndResponse) {
    log.info("otherAttributeNameAndValueMap : {} ", otherAttributeNameAndValueMap);
    for (CategoryAttributeResponse categoryAttributeResponse : categoryDetailResponse.getCategoryAttributes()) {
      if (!categoryAttributeResponse.isMarkForDelete() && isNonVariantCreationAndNotBrandAndNotFamilyColourAttribute(
          categoryAttributeResponse.getAttribute())) {
        AttributeResponse attribute = attributeIdAndResponse.get(categoryAttributeResponse.getAttribute().getId());
        if (Objects.nonNull(attribute)) {
          String value = otherAttributeNameAndValueMap.getOrDefault(attribute.getName(),
              otherAttributeNameAndValueMap.getOrDefault(attribute.getNameEnglish(), StringUtils.EMPTY));
          ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
          productAttributeValueRequest.setStoreId(bulkProcess.getStoreId());
          initializeAttributes(attribute, productAttributeValueRequest, value);
          ProductAttributeRequest productAttributeRequest =
              setValuesForProductAttributeRequest(bulkProcess.getStoreId(), attribute, isInternationalMerchant);
          productAttributeRequest.setProductAttributeValues(new ArrayList<>());
          productAttributeRequest.getProductAttributeValues().add(productAttributeValueRequest);
          productCollectionRequest.getProductAttributes().add(productAttributeRequest);
        }
      }
    }
  }

  private static void setItemDetails(ProductItemCreationRequest itemCreationRequest, Map<String, Object> raw,
      Map<String, Object> parentRaw, String ppNameDelimiter) {
    // Get item stock/price to use it in items if item stock/price is null
    Double parentSalePrice = null;
    Integer parentStock = null;

    // Fecth product type and pickup point from parent since, it is same across all items
    Integer productType =
        (Integer) BulkUploadOption.valueFrom(String.valueOf(getValueBasedOnEnOrIdHeader(parentRaw, GenericBulkHeaders.PRODUCT_TYPE,
            GenericBulkHeaders.PRODUCT_TYPE_EN))).getValue();
    String pickupPointCode =
      String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.PICKUP_POINT,
          GenericBulkHeaders.PICKUP_POINT_EN));
    pickupPointCode = getPickupPointCodeFromPickupPointCodeAndName(ppNameDelimiter, pickupPointCode);

    Double price = parentSalePrice;
    if (!StringUtils.isBlank(String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.SALE_PRICE,
        GenericBulkHeaders.SALE_PRICE_EN)))) {
      try {
        price = Double.parseDouble(String.valueOf(
            getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.SALE_PRICE, GenericBulkHeaders.SALE_PRICE_EN)));
      } catch (Exception e) {
        log.warn(BulkProcessValidationErrorMessages.WARNING_PREFIX, e);
      }
    }

    Integer stock = parentStock;
    if (!StringUtils.isBlank(String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.STOCK,
        GenericBulkHeaders.STOCK_EN)))) {
      try {
        stock = Integer.parseInt(String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.STOCK,
            GenericBulkHeaders.STOCK_EN)));
      } catch (Exception e) {
        log.warn(BulkProcessValidationErrorMessages.WARNING_PREFIX, e);
      }
    }

    itemCreationRequest.setBundleRecipe(getBundleRecipe(raw));
    itemCreationRequest.setItemGeneratedName(
        String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.GENERATED_ITEM_NAME,
            GenericBulkHeaders.GENERATED_ITEM_NAME)));
    itemCreationRequest.setProductType(productType);
    itemCreationRequest.setPrice(price);
    itemCreationRequest.setSalePrice(price);
    itemCreationRequest.setStock(stock);
    // BY default set to 0
    itemCreationRequest.setMinimumStock(0);
    itemCreationRequest.setPickupPointId(pickupPointCode);
    // By default set it to True
    itemCreationRequest.setDisplay(true);
    itemCreationRequest.setBuyable(true);
    itemCreationRequest.setMerchantSku(String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.SELLER_SKU,
        GenericBulkHeaders.SELLER_SKU_EN)));
    itemCreationRequest.setUpcCode(String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.UPC,
        GenericBulkHeaders.UPC_EN)));
    itemCreationRequest.setImages(new ArrayList<>());
  }

  public static String getPickupPointCodeFromPickupPointCodeAndName(String ppNameDelimiter, String pickupPointCode) {
    return Splitter.on(ppNameDelimiter).trimResults().split(pickupPointCode).iterator().next();
  }

  public static String getWareHouseCodeFromWareHouseCodeAndName(String wareHouseDelimeter, String wareHouse) {
    return Splitter.on(wareHouseDelimeter).trimResults().split(wareHouse).iterator().next();
  }

  public static PickupPointCreateRequest setItemPickupPointDetails(Map<String, Object> raw,
      Map<String, Object> parentRaw, MerchantStatusType merchantStatusType, String ppNameDelimiter,
      boolean cncForWarehouseFeatureSwitch, boolean instoreSeller) {
    int stock = 0;
        PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    Double parentSalePrice = Double.parseDouble(
        String.valueOf(getValueBasedOnEnOrIdHeader(parentRaw, GenericBulkHeaders.SALE_PRICE, GenericBulkHeaders.SALE_PRICE_EN)));
    String pickupPointCode = String.valueOf(
        getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.PICKUP_POINT, GenericBulkHeaders.PICKUP_POINT_EN));
    pickupPointCode = getPickupPointCodeFromPickupPointCodeAndName(ppNameDelimiter, pickupPointCode);
    String deliveryStatusValue = String.valueOf(
        getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.DELIVERY_STATUS, GenericBulkHeaders.DELIVERY_STATUS_EN));
    int deliveryStatus = getDeliveryStatusValue(deliveryStatusValue, instoreSeller);

    if (merchantStatusType.getType() == MerchantStatusType.DELIVERY_AND_CNC.getType()
        || merchantStatusType.getType() == MerchantStatusType.BFB_AND_CNC.getType()) {
      int cncStatus = StringUtils.isBlank(
          String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.CNC, GenericBulkHeaders.CNC_EN))) ?
          Integer.parseInt(Constant.DEFAULT_CNC_STATUS) :
          Integer.parseInt(
              String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.CNC, GenericBulkHeaders.CNC_EN)));
      boolean cncActivated = cncStatus == 1;
      if (cncForWarehouseFeatureSwitch) {
        pickupPointCreateRequest.setCncBuyable(cncActivated);
        pickupPointCreateRequest.setCncDisplay(cncActivated);
      } else {
        pickupPointCreateRequest.setCncActive(cncActivated);
      }
    }
    Map<String, String> rawBfbValues =
        BulkCreationCommonUtil.getRawBfbValues(raw, GenericBulkHeaders.BFB_BASE_PRICE_HEADER_ID,
            GenericBulkHeaders.BFB_BASE_PRICE_HEADER_EN, GenericBulkHeaders.BFB_MANAGED_HEADER_ID,
            GenericBulkHeaders.BFB_MANAGED_HEADER_EN, GenericBulkHeaders.BFB_STATUS_HEADER);
    setB2bFieldInProductCreationRequest(merchantStatusType, pickupPointCreateRequest, rawBfbValues);

    try {
      stock = StringUtils.isBlank(String.valueOf(
          getValueBasedOnEnOrIdHeader(parentRaw, GenericBulkHeaders.STOCK, GenericBulkHeaders.STOCK_EN))) ?
          Integer.parseInt(Constant.DEFAULT_STOCK_VALUE) :
          Integer.parseInt(String.valueOf(
              getValueBasedOnEnOrIdHeader(parentRaw, GenericBulkHeaders.STOCK, GenericBulkHeaders.STOCK_EN)));

      if (!StringUtils.isBlank(
          String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.STOCK, GenericBulkHeaders.STOCK_EN)))) {
        stock = Integer.parseInt(String.valueOf(getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.STOCK, GenericBulkHeaders.STOCK_EN)));
      }
    } catch (Exception e) {
      log.warn(BulkProcessValidationErrorMessages.WARNING_PREFIX, e);
    }

    Double price = parentSalePrice;
    if (!StringUtils.isBlank(String.valueOf(
        getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.SALE_PRICE, GenericBulkHeaders.SALE_PRICE_EN)))) {
      try {
        price = Double.parseDouble(String.valueOf(
            getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.SALE_PRICE, GenericBulkHeaders.SALE_PRICE_EN)));
      } catch (Exception e) {
        log.warn(BulkProcessValidationErrorMessages.WARNING_PREFIX, e);
      }
    }

    pickupPointCreateRequest.setPrice(price);
    pickupPointCreateRequest.setSalePrice(price);
    pickupPointCreateRequest.setStock(stock);
    pickupPointCreateRequest.setBuyable(false);
    pickupPointCreateRequest.setDisplay(false);
    if (deliveryStatus == 1) {
      pickupPointCreateRequest.setBuyable(true);
      pickupPointCreateRequest.setDisplay(true);
    }
    // BY default set to 0
    pickupPointCreateRequest.setMinimumStock(0);
    pickupPointCreateRequest.setPickupPointId(pickupPointCode);

    return pickupPointCreateRequest;
  }

  private static ProductAttributeRequest setValuesForProductAttributeRequest(String storeId,
      AttributeResponse attribute, boolean isInternationalMerchant) {
    String attributeName = getAttributeName(isInternationalMerchant, attribute);
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    productAttributeRequest.setStoreId(storeId);
    productAttributeRequest.setAttribute(new AttributeRequest());
    productAttributeRequest.setProductAttributeName(attributeName);
    productAttributeRequest.getAttribute().setStoreId(storeId);
    productAttributeRequest.getAttribute().setId(attribute.getId());
    productAttributeRequest.getAttribute().setAttributeCode(attribute.getAttributeCode());
    productAttributeRequest.getAttribute().setName(attributeName);
    productAttributeRequest.getAttribute().setAttributeType(AttributeType.valueOf(attribute.getAttributeType()));
    productAttributeRequest.getAttribute().setSkuValue(attribute.isSkuValue());
    productAttributeRequest.getAttribute().setVariantCreation(attribute.isVariantCreation());
    productAttributeRequest.getAttribute().setMandatory(attribute.isMandatory());
    return productAttributeRequest;
  }

  private static void initializeAttributes(AttributeResponse attribute,
      ProductAttributeValueRequest productAttributeValueRequest, String attributeValue) {
    if (attribute.getAttributeType().equals(String.valueOf(AttributeType.DESCRIPTIVE_ATTRIBUTE))) {
      productAttributeValueRequest.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
      if (!StringUtils.isBlank(attributeValue)) {
        productAttributeValueRequest.setDescriptiveAttributeValue(attributeValue);
      } else {
        productAttributeValueRequest.setDescriptiveAttributeValue(Constant.HYPHEN);
      }
    } else if (attribute.getAttributeType().equals(String.valueOf(AttributeType.PREDEFINED_ATTRIBUTE))) {
      productAttributeValueRequest.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
      productAttributeValueRequest.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
      // Search predefined attribute value
      if (StringUtils.isBlank(attributeValue)) {
        productAttributeValueRequest.getPredefinedAllowedAttributeValue().setValue(Constant.HYPHEN);
        productAttributeValueRequest.getPredefinedAllowedAttributeValue()
            .setPredefinedAllowedAttributeCode(attribute.getAttributeCode() + Constant.HYPHEN + Constant.DEFAULT);
      } else {
        for (PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValue : attribute.getPredefinedAllowedAttributeValues()) {
          if (predefinedAllowedAttributeValue.getValue().equals(attributeValue)) {
            productAttributeValueRequest.getPredefinedAllowedAttributeValue()
                .setId(predefinedAllowedAttributeValue.getId());
            productAttributeValueRequest.getPredefinedAllowedAttributeValue()
                .setValue(predefinedAllowedAttributeValue.getValue());
            break;
          }
        }
      }
    } else {
      productAttributeValueRequest.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
      productAttributeValueRequest.setAllowedAttributeValue(new AllowedAttributeValueRequest());
      // Search defining attribute value
      for (AllowedAttributeValueResponse allowedAttributeValue : attribute.getAllowedAttributeValues()) {
        if (allowedAttributeValue.getValue().equals(attributeValue)) {
          productAttributeValueRequest.getAllowedAttributeValue().setId(allowedAttributeValue.getId());
          productAttributeValueRequest.getAllowedAttributeValue().setValue(allowedAttributeValue.getValue());
          break;
        }
      }
    }
  }

  public static boolean isNonVariantCreationAndNotBrandAndNotFamilyColourAttribute(
      AttributeResponse attributeResponse) {
    return !(AttributeType.DEFINING_ATTRIBUTE.name().equals(attributeResponse.getAttributeType()) || attributeResponse
        .isVariantCreation() || (GenericBulkParameters.FAMILY_COLOUR.equals(attributeResponse.getName())
        || GenericBulkParameters.BRAND.equals(attributeResponse.getName())));
  }

  public static Double getKilogramFromGram(Double gram) {
    return gram / 1000;
  }

  public static String getParagraphString(String input) {
    if (StringUtils.isBlank(input)) {
      return input;
    }
    String[] inputArray = input.split(System.getProperty("line.separator"));
    String formattedInput = StringUtils.EMPTY;
    for (String userInput : inputArray) {
      formattedInput = new StringBuilder(formattedInput).append(Constant.PARAGRAPH_START).append(userInput)
          .append(Constant.PARAGRAPH_END).toString();
    }
    return formattedInput;
  }

  public void setProductCreationRequest(ProductCreationRequest productCollectionRequest, BulkProcess bulkProcess,
      String productCode, List<Map<String, Object>> validUserRows, Map<String, String> imageUrlAndLocationMap,
      String imageSource, Map<String, CategoryDetailAndShippingResponse> categoryIdAndDetailMap,
      String autoUploadUrlSubstring, boolean isInternationalMerchant) {
    productCollectionRequest.setProductCode(productCode);
    BulkProcessType bulkProcessType =
      BulkProcessType.getBulkProcessType(bulkProcess.getBulkProcessType());
    productCollectionRequest.setProductCreationType(
      CommonUtils.getProductCreationTypeFromBulkProcessType(bulkProcessType));
    Map<String, String> mappingImageFiles =
        generateImageFilename(validUserRows, productCollectionRequest, imageUrlAndLocationMap, autoUploadUrlSubstring, allowedImageExtensions);
    initializeBusinessPartnerAttributes(validUserRows, productCollectionRequest, bulkProcess, categoryIdAndDetailMap, isInternationalMerchant);
    uploadImageFiles(mappingImageFiles, bulkProcess, imageSource);
  }

  private static void initializeBusinessPartnerAttributes(List<Map<String, Object>> userRows,
      ProductCreationRequest productCreationRequest, BulkProcess bulkProcess,
      Map<String, CategoryDetailAndShippingResponse> categoryIdAndDetailMap, boolean isInternationalMerchant) {
    Map<String, Object> parentRow = userRows.stream().findFirst().orElse(new HashMap<>());
    CategoryDetailAndShippingResponse categoryDetailResponse =
        categoryIdAndDetailMap.get(parentRow.get(GenericBulkHeaders.CN_CATEGORY_ID));
    List<ProductBusinessPartnerAttributeRequest> productBusinessPartnerAttributeRequests = new ArrayList<>();
    Map<String, String> otherAttributesNamesMap = getOtherAttributesNames(parentRow);
    for (CategoryAttributeResponse categoryAttribute : categoryDetailResponse.getCategoryAttributes()) {
      // Add SKU value = true attributes
      if (!categoryAttribute.isMarkForDelete() && categoryAttribute.getAttribute().isSkuValue()) {
        ProductBusinessPartnerAttributeRequest productBusinessPartnerAttributeRequest =
            new ProductBusinessPartnerAttributeRequest();
        productBusinessPartnerAttributeRequest.setStoreId(bulkProcess.getStoreId());
        productBusinessPartnerAttributeRequest.setCreatedDate(Calendar.getInstance().getTime());
        productBusinessPartnerAttributeRequest.setCreatedBy(bulkProcess.getCreatedBy());
        productBusinessPartnerAttributeRequest.setUpdatedDate(new Date());
        productBusinessPartnerAttributeRequest.setUpdatedBy(bulkProcess.getUpdatedBy());
        productBusinessPartnerAttributeRequest.setAttributeId(categoryAttribute.getAttribute().getId());
        String specialAttributeValue;
        specialAttributeValue =
            otherAttributesNamesMap.getOrDefault(categoryAttribute.getAttribute().getName(), StringUtils.EMPTY);
        if (StringUtils.isBlank(specialAttributeValue)) {
          specialAttributeValue = otherAttributesNamesMap
              .getOrDefault(categoryAttribute.getAttribute().getNameEnglish(), StringUtils.EMPTY);
        }
        if (StringUtils.isBlank(specialAttributeValue)) {
          productBusinessPartnerAttributeRequest.setValue(Constant.HYPHEN);
        } else {
          productBusinessPartnerAttributeRequest.setValue(specialAttributeValue);
        }
        productBusinessPartnerAttributeRequests.add(productBusinessPartnerAttributeRequest);
      }
    }
    productCreationRequest.setProductBusinessPartnerAttributes(productBusinessPartnerAttributeRequests);
  }

  private static void initializeShippingWeight(ProductCreationRequest productCollectionRequest,
      CategoryDetailAndShippingResponse categoryDetailResponse,
      GenerateShippingWeightRequest generateShippingWeightRequest) {
    generateShippingWeightRequest.setLength(productCollectionRequest.getLength());
    generateShippingWeightRequest.setWidth(productCollectionRequest.getWidth());
    generateShippingWeightRequest.setHeight(productCollectionRequest.getHeight());
    generateShippingWeightRequest.setWeight(productCollectionRequest.getWeight());
    generateShippingWeightRequest.setCategoryCode(categoryDetailResponse.getCategoryCode());
  }

  private static Map<String, String> getOtherAttributesNames(Map<String, Object> parentRaw) {
    Map<String, String> otherAttributeNameAndValueMap = new HashMap<>();
    for (int attributeNumber = 1;
        attributeNumber <= GenericBulkParameters.NUMBER_OF_OTHER_ATTRIBUTES; attributeNumber++) {
      otherAttributeNameAndValueMap.putIfAbsent(String.valueOf(
              getValueBasedOnEnOrIdHeader(parentRaw,GenericBulkHeaders.ATTRIBUTE_NAME_EN + attributeNumber,
                  GenericBulkHeaders.ATTRIBUTE_NAME + Constant.SPACE + attributeNumber)),
          String.valueOf(getValueBasedOnEnOrIdHeader(parentRaw,
              GenericBulkHeaders.ATTRIBUTE_VALUE + Constant.SPACE + attributeNumber,
              GenericBulkHeaders.ATTRIBUTE_VALUE_EN + attributeNumber)));
    }
    return otherAttributeNameAndValueMap;
  }

  private static Map<String, String> generateImageFilename(List<Map<String, Object>> userRows,
      ProductCreationRequest productCreationRequest, Map<String, String> imageUrlAndLocationMap,
      String autoUploadUrlSubstring, List<String> allowedImageExtensions) {
    Integer indexColumnImages = GenericBulkParameters.IMAGE_START_COLUMN;
    Map<String, String> imageAndImageUrlReverseMap = new HashMap<>();

    Map<String, String> mappingImageFilenames = new LinkedHashMap<>();
    //Group rows contain all rows with same parent column , sort group rows based on row number to fetch the
    //first row to process common images and ignore for rest of the rows
    userRows.sort(Comparator.comparingInt(row -> (int) row.getOrDefault(GenericBulkHeaders.ROW_NUMBER, 0)));
    //Process common images for first row only
    Map<String, Object> firstRaw = userRows.stream().findFirst().orElse(Collections.emptyMap());
      for (int i = 0; i < GenericBulkParameters.NUMBER_OF_IMAGES; i++) {
        String imageFilename = String.valueOf(
            getValueBasedOnEnOrIdHeader(firstRaw, GenericBulkHeaders.IMAGE_ID_HEADER_LIST.get(i),
                GenericBulkHeaders.IMAGE_EN_HEADER_LIST.get(i)));
        prepareMappingImageFilenamesMap(productCreationRequest, imageUrlAndLocationMap, autoUploadUrlSubstring,
            imageAndImageUrlReverseMap, mappingImageFilenames, imageFilename);
      }
      //Populate variant images in mappingImageFilenames map
    for (Map<String, Object> currentRow : userRows) {
      String imageFilename = String.valueOf(Optional.ofNullable(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(currentRow, GenericBulkHeaders.VARIANT_IMAGE_EN,
              GenericBulkHeaders.VARIANT_IMAGE_ID)).orElse(StringUtils.EMPTY));
      prepareMappingImageFilenamesMap(productCreationRequest, imageUrlAndLocationMap, autoUploadUrlSubstring,
          imageAndImageUrlReverseMap, mappingImageFilenames, imageFilename);
    }

  StringBuilder validImageFilename = new StringBuilder(productCreationRequest.getProductCode()).append(File.separator)
        .append(toLowerCaseName(productCreationRequest.getBrand()));
    validImageFilename.append(Constant.UNDERSCORE);
    validImageFilename.append(toLowerCaseName(productCreationRequest.getName()));
    validImageFilename.append(Constant.IMAGE_KEYWORD_FULL);
    int imageCounter = 1;
    for (Map.Entry<String, String> entry : mappingImageFilenames.entrySet()) {
      String currentImageCounter;
      if (imageCounter < 9) {
        currentImageCounter = Constant.ZERO_STRING + imageCounter;
      } else {
        currentImageCounter = String.valueOf(imageCounter);
      }
      String[] splitImageFilename = entry.getKey().split("\\.");
      String imageFileType = splitImageFilename[splitImageFilename.length - 1];
      if (!allowedImageExtensions.contains(imageFileType.toLowerCase())) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            BulkProcessValidationErrorMessages.IMAGE_FILE_TYPE_INVALID);
      }
      entry.setValue(validImageFilename.toString() + currentImageCounter + Constant.DOT + imageFileType.toLowerCase());
      imageCounter++;
    }
    // Generate product image filename
    imageCounter = 0;
    for (Map.Entry<String, String> entry : mappingImageFilenames.entrySet()) {
      Image image = new Image();
      image.setLocationPath(entry.getValue());
      image.setMainImages(imageCounter == 0);
      image.setSequence(imageCounter);
      image.setHashCode(ImageUtil.generateHashcodeByLocationPath(entry.getValue()));
      image.setOriginalImage(Boolean.TRUE);
      image.setStoreId(productCreationRequest.getStoreId());
      if (imageAndImageUrlReverseMap.containsKey(entry.getKey())) {
        image.setUrlPath(imageAndImageUrlReverseMap.get(entry.getKey()));
      }
      productCreationRequest.getImages().add(image);
      imageCounter++;
    }
    boolean containsUrlImages = false;
    Set<Image> commonImageSet = new HashSet<>();
    Set<String> uniqueCommonImagesSet = new HashSet<>();
    for (int imageNumber = 1; imageNumber <= GenericBulkParameters.NUMBER_OF_IMAGES; imageNumber++) {
      String imageFilename = String.valueOf(
          getValueBasedOnEnOrIdHeader(firstRaw, GenericBulkHeaders.IMAGE_ID_HEADER_LIST.get(imageNumber - 1),
              GenericBulkHeaders.IMAGE_EN_HEADER_LIST.get(imageNumber - 1)));
      containsUrlImages = BulkCategoryProcessorServiceBean.validateAndProcessCommonImages(productCreationRequest,
          imageUrlAndLocationMap, mappingImageFilenames, containsUrlImages, commonImageSet, uniqueCommonImagesSet,
          imageNumber, imageFilename);
    }
    //Item level image request formation along with common images
    containsUrlImages =
        BulkCategoryProcessorServiceBean.prepareProductItemCreationRequest(userRows, productCreationRequest,
            imageUrlAndLocationMap, mappingImageFilenames, containsUrlImages, commonImageSet, uniqueCommonImagesSet);
    productCreationRequest.setCommonImages(new ArrayList<>(commonImageSet));
    productCreationRequest.setContainsUrlImage(containsUrlImages);
    return mappingImageFilenames;
  }

  public static void prepareMappingImageFilenamesMap(ProductCreationRequest productCreationRequest,
      Map<String, String> imageUrlAndLocationMap, String autoUploadUrlSubstring,
      Map<String, String> imageAndImageUrlReverseMap, Map<String, String> mappingImageFilenames, String imageFilename) {
    if (StringUtils.isNotBlank(imageFilename) && !imageFilename.startsWith(Constant.HTTPS_PREFIX)) {
      mappingImageFilenames.put(imageFilename, null);
    } else if (isValidHttpsImage(imageFilename, imageUrlAndLocationMap)) {
      if (imageFilename.contains(autoUploadUrlSubstring)) {
        productCreationRequest.setProductCreationType(ProductCreationType.AUTO_UPLOAD);
      }
      mappingImageFilenames.put(imageUrlAndLocationMap.get(imageFilename), null);
      imageAndImageUrlReverseMap.putIfAbsent(imageUrlAndLocationMap.get(imageFilename), imageFilename);
    }
  }

  public static boolean isValidHttpsImage(String imageFilename, Map<String, String> imageUrlAndLocationMap) {
    return (StringUtils.isNotBlank(imageFilename) && imageFilename.startsWith(Constant.HTTPS_PREFIX))
        && imageUrlAndLocationMap.containsKey(imageFilename);
  }

  public static List<List<Object>> setStateAndFetchInvalidRows(BulkProcess bulkProcess,
      List<BulkProcessData> failedRowData) throws IOException {
    List<List<Object>> userInputRows = new ArrayList<>();
    if (CollectionUtils.isEmpty(failedRowData)) {
      bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
      bulkProcess.setSuccessCount(bulkProcess.getTotalCount());
    } else {
      int failedL3Count =
          new HashSet<>(failedRowData.stream().map(BulkProcessData::getParentProduct).collect(Collectors.toSet()))
              .size();
      if (failedL3Count == bulkProcess.getTotalCount()) {
        bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
      } else {
        bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
      }
      int systemError = 0, inputError = 0;
      for (BulkProcessData bulkProcessData : failedRowData) {
        ObjectMapper mapper = new ObjectMapper();
        LinkedHashMap<String, Object> rowDataJson =
            mapper.readValue(bulkProcessData.getBulkRequestData(), new TypeReference<LinkedHashMap<String, Object>>() {
            });
        List<Object> inputRow = new ArrayList<>(rowDataJson.values());
        inputRow.add(bulkProcessData.getErrorMessage());
        userInputRows.add(inputRow);
        if (Optional.ofNullable(bulkProcessData.getInputErrorCount()).orElse(0) != Constant.ZERO) {
          inputError++;
        }
        if (Optional.ofNullable(bulkProcessData.getSystemErrorCount()).orElse(0) != Constant.ZERO) {
          systemError++;
        }
      }
      bulkProcess.setSystemErrorCount(systemError);
      bulkProcess.setInputErrorCount(inputError);
      bulkProcess.setSuccessCount(bulkProcess.getTotalCount() - failedL3Count);
    }
    bulkProcess.setEndDate(Calendar.getInstance().getTime());
    return userInputRows;
  }

  public static void setDescriptionForGeneric(BulkProcess bulkProcess, int internalActivationPeriod, int failedSize) {
    String[] excelFileName = bulkProcess.getDescription().split("\\. ");
    String slaNotificationMsg = BulkCreationCommonUtil
        .getSLANotification(bulkProcess.getInternationalMerchant(), internalActivationPeriod, null);
    String successMessageProductUpload;
    if (bulkProcess.getSuccessCount() != 0) {
      successMessageProductUpload =
          new StringBuilder(Constant.DESCRIPTION_SUCCESS).append(bulkProcess.getSuccessCount()).append(Constant.SPACE)
              .append(Constant.OUT_OF).append(Constant.SPACE).append(bulkProcess.getTotalCount()).append(Constant.SPACE)
              .append(Constant.DESCRIPTION_PRODUCTS).append(slaNotificationMsg).toString();
    } else {
      successMessageProductUpload = StringUtils.EMPTY;
    }
    if (failedSize == Constant.ZERO) {
      bulkProcess.setDescription(
          new StringBuilder(excelFileName[0]).append(Constant.SPACE).append(Constant.DESCRIPTION_BULK_UPLOAD_SUFFIX)
              .append(Constant.SPACE).append(successMessageProductUpload).toString());
    } else {
      bulkProcess.setDescription(
          new StringBuilder(excelFileName[0]).append(Constant.SPACE).append(Constant.DESCRIPTION_BULK_UPLOAD_SUFFIX)
              .append(successMessageProductUpload).append(Constant.SPACE).toString());
    }
  }

  public static boolean isDescriptiveOrPredefinedAndNotVariantCreation(AttributeResponse attributeResponse) {
    return (AttributeType.DESCRIPTIVE_ATTRIBUTE.name().equals(attributeResponse.getAttributeType())
        || AttributeType.PREDEFINED_ATTRIBUTE.name().equals(attributeResponse.getAttributeType())) && !attributeResponse
        .isVariantCreation();
  }

  public static void generateProductItemNames(List<List<Map<String, Object>>> validRows, Map<String, AttributeResponse> attributes) {
    List<AttributeResponse> definingAttributes = new ArrayList<AttributeResponse>();
    for (Map.Entry<String, AttributeResponse> entry : attributes.entrySet()) {
      AttributeResponse attribute = entry.getValue();
      if (BulkCreationCommonUtil.isDefiningOrVariantCreation(attribute)) {
        definingAttributes.add(attribute);
      }
    }
    // Sort variant attribute by attribute id
    Collections.sort(definingAttributes, new Comparator<AttributeResponse>() {
      @Override
      public int compare(AttributeResponse arg0, AttributeResponse arg1) {
        return arg0.getId().compareTo(arg1.getId());
      }
    });

    // Generate product item names
    for (List<Map<String, Object>> rows : validRows) {
      String productName = String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(rows.get(0),
          BulkCnCreationHeaderNames.HEIGHT_ID_HEADER_NAME,
          BulkCnCreationHeaderNames.PRODUCT_DESCRIPTION_EN_HEADER_NAME));
      for (Map<String, Object> row : rows) {
        StringBuilder productItemName = new StringBuilder(productName);
        for (AttributeResponse attributeResponse : definingAttributes) {
          String attributeValue = String.valueOf(
              BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(row, attributeResponse.getName(),
                  attributeResponse.getNameEnglish()));
          String attValue = StringUtils.EMPTY;
          if (StringUtils.isNotBlank(attributeValue)) {
            attValue = StringUtils.SPACE + attributeValue;
          }
          productItemName.append(attValue);
        }
        row.put(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME, productItemName.toString());
      }
    }
  }

  public static void initializeItemDetails(Map<String, AttributeResponse> attributes,
      ProductCreationRequest productCollectionRequest, Map<String, Object> currentRow, Map<String, Object> parentRaw,
      AtomicInteger buyableFlagUpdatedCount, MerchantStatusType merchantStatusType, String ppNameDelimiter,
      boolean cncForWarehouseFeatureSwitch, boolean instoreSeller) {
    List<AttributeResponse> definingAttributes = BulkCreationCommonUtil.getVariantCreatingAttribute(attributes.values());
    boolean hasWarnaValue = false;
    TreeMap<String, String> attributeMap = new TreeMap<>();
    ProductItemCreationRequest productItemRequest = new ProductItemCreationRequest();
    List<PickupPointCreateRequest> pickupPoints=new ArrayList<>();
    for (AttributeResponse attributeResponse : definingAttributes) {
      String attributeValue = String.valueOf(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(currentRow, attributeResponse.getName(),
              attributeResponse.getNameEnglish()));
      if (StringUtils.isNotBlank(attributeValue)) {
        if (Constant.WARNA.equals(attributeResponse.getName())) {
          hasWarnaValue = true;
        }
        attributeMap.putIfAbsent(attributeResponse.getAttributeCode(), attributeValue);
      }
    }

    AttributeResponse familyColourAttributeResponse = attributes.get(Constant.COLOUR_FAMILY);
    List<ProductItemAttributeValueRequest> itemAttributeValueRequests =
        getItemAttributeRequest(currentRow, productCollectionRequest, familyColourAttributeResponse, hasWarnaValue);
    productItemRequest.setAttributesMap(attributeMap);
    productItemRequest.setProductItemAttributeValueRequests(itemAttributeValueRequests);
    setItemDetails(productItemRequest, String.valueOf(currentRow.get(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME)),
        currentRow, parentRaw, buyableFlagUpdatedCount, ppNameDelimiter);
    pickupPoints.add(
        setItemPickupPoint(String.valueOf(currentRow.get(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME)), currentRow,
            parentRaw, buyableFlagUpdatedCount, merchantStatusType, ppNameDelimiter, cncForWarehouseFeatureSwitch,
            instoreSeller));
    productItemRequest.setPickupPoints(pickupPoints);
    productCollectionRequest.getProductItemRequests().add(productItemRequest);
  }

  public static List<ProductItemAttributeValueRequest> getItemAttributeRequest(Map<String, Object> currentRow,
      ProductCreationRequest productCollectionRequest, AttributeResponse familyColourAttributeResponse,
      boolean hasWarnaValue) {
    List<ProductItemAttributeValueRequest> itemAttributeValueRequests = new ArrayList<>();
    if (hasWarnaValue && Objects.nonNull(familyColourAttributeResponse)) {
      AttributeRequest attributeRequest = getAttributeRequest(familyColourAttributeResponse);
      String attributeValue = String.valueOf(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(currentRow, familyColourAttributeResponse.getName(),
              familyColourAttributeResponse.getNameEnglish()));
      itemAttributeValueRequests.add(new ProductItemAttributeValueRequest(attributeRequest, attributeValue,
          productCollectionRequest.getStoreId()));
    }
    return itemAttributeValueRequests;
  }

  private static AttributeRequest getAttributeRequest(AttributeResponse attribute) {
    AttributeRequest attributeRequest = new AttributeRequest();
    BeanUtils.copyProperties(attribute, attributeRequest, "attributeType", "allowedAttributeValues",
        "predefinedAllowedAttributeValues");
    attributeRequest.setAttributeType(AttributeType.valueOf(attribute.getAttributeType()));
    attributeRequest.setId(attribute.getId());
    return attributeRequest;
  }

  public static PickupPointCreateRequest setItemPickupPoint(String itemGeneratedName, Map<String, Object> raw,
      Map<String, Object> parentRaw, AtomicInteger buyableFlagUpdatedCount, MerchantStatusType merchantStatusType,
      String ppNameDelimiter, boolean cncForWarehouseFeatureSwitch, boolean instoreSeller) {
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    Double parentPrice = Double.parseDouble(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw,
        BulkCnCreationHeaderNames.NORMAL_PRICE_ID_HEADER_NAME, BulkCnCreationHeaderNames.NORMAL_PRICE_EN_HEADER_NAME)));
    Double parentSalePrice = Double.parseDouble(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw, BulkCnCreationHeaderNames.SELLING_PRICE_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.SELLING_PRICE_EN_HEADER_NAME)));
    Integer parentStock = StringUtils.isBlank(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw,
            BulkCnCreationHeaderNames.AVAILABLE_STOCK_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.AVAILABLE_STOCK_EN_HEADER_NAME))) ?
        Integer.parseInt(Constant.DEFAULT_STOCK_VALUE) :
        Integer.parseInt(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw,
            BulkCnCreationHeaderNames.AVAILABLE_STOCK_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.AVAILABLE_STOCK_EN_HEADER_NAME)));
    Integer parentMinimumStock = StringUtils.isBlank(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw,
            BulkCnCreationHeaderNames.MINIMUM_STOCK_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.MINIMUM_STOCK_EN_HEADER_NAME))) ?
        Integer.parseInt(Constant.DEFAULT_STOCK_VALUE) :
        Integer.parseInt(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw,
            BulkCnCreationHeaderNames.MINIMUM_STOCK_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.MINIMUM_STOCK_EN_HEADER_NAME)));
    String pickupPointCode = String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.PICKUP_POINT_CODE_ID_HEADER,
            BulkCnCreationHeaderNames.PICKUP_POINT_CODE_EN_HEADER));
    pickupPointCode = getPickupPointCodeFromPickupPointCodeAndName(ppNameDelimiter, pickupPointCode);
    String deliveryStatusValue = String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
        BulkCnCreationHeaderNames.PRODUCT_DELIVERY_STATUS_ID_HEADER_NAME,
        BulkCnCreationHeaderNames.PRODUCT_DELIVERY_STATUS_EN_HEADER_NAME));
    int deliveryStatus = getDeliveryStatusValue(deliveryStatusValue, instoreSeller);

    if (itemGeneratedName.equals(raw.get(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME))) {
      Double price = parentPrice;
      if (!StringUtils.isBlank(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
          BulkCnCreationHeaderNames.NORMAL_PRICE_ID_HEADER_NAME, BulkCnCreationHeaderNames.NORMAL_PRICE_EN_HEADER_NAME)))) {
        try {
          price = Double.parseDouble(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
              BulkCnCreationHeaderNames.NORMAL_PRICE_ID_HEADER_NAME, BulkCnCreationHeaderNames.NORMAL_PRICE_EN_HEADER_NAME)));
        } catch (Exception e) {
          log.warn(Constant.WARNING_PREFIX, e);
        }
      }
      Double salePrice = parentSalePrice;
      if (!StringUtils.isBlank(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
          BulkCnCreationHeaderNames.SELLING_PRICE_ID_HEADER_NAME, BulkCnCreationHeaderNames.SELLING_PRICE_EN_HEADER_NAME)))) {
        try {
          salePrice =
              Double.parseDouble(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
                  BulkCnCreationHeaderNames.SELLING_PRICE_ID_HEADER_NAME, BulkCnCreationHeaderNames.SELLING_PRICE_EN_HEADER_NAME)));
        } catch (Exception e) {
          log.warn(Constant.WARNING_PREFIX, e);
        }
      }
      Integer stock = parentStock;
      if (!StringUtils.isBlank(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
          BulkCnCreationHeaderNames.AVAILABLE_STOCK_ID_HEADER_NAME, BulkCnCreationHeaderNames.AVAILABLE_STOCK_EN_HEADER_NAME)))) {
        try {
          stock = Integer.parseInt(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
              BulkCnCreationHeaderNames.AVAILABLE_STOCK_ID_HEADER_NAME, BulkCnCreationHeaderNames.AVAILABLE_STOCK_EN_HEADER_NAME)));
        } catch (Exception e) {
          log.warn(Constant.WARNING_PREFIX, e);
        }
      }

      Integer minimumStock = parentMinimumStock;
      if (!StringUtils.isBlank(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
          BulkCnCreationHeaderNames.MINIMUM_STOCK_ID_HEADER_NAME, BulkCnCreationHeaderNames.MINIMUM_STOCK_EN_HEADER_NAME)))) {
        try {
          minimumStock =
              Integer.parseInt(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
                  BulkCnCreationHeaderNames.MINIMUM_STOCK_ID_HEADER_NAME, BulkCnCreationHeaderNames.MINIMUM_STOCK_EN_HEADER_NAME)));
        } catch (Exception e) {
          log.warn(Constant.WARNING_PREFIX, e);
        }
      }
      pickupPointCreateRequest.setPickupPointId(pickupPointCode);
      pickupPointCreateRequest.setPrice(price);
      pickupPointCreateRequest.setSalePrice(salePrice);
      pickupPointCreateRequest.setStock(stock);
      pickupPointCreateRequest.setMinimumStock(minimumStock);
      pickupPointCreateRequest.setBuyable(deliveryStatus == 1);
      pickupPointCreateRequest.setDisplay(deliveryStatus == 1);
      if (merchantStatusType.getType() == MerchantStatusType.DELIVERY_AND_CNC.getType()
          || merchantStatusType.getType() == MerchantStatusType.BFB_AND_CNC.getType()) {
        Integer cncStatus = StringUtils.isBlank(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
            BulkCnCreationHeaderNames.PRODUCT_CNC_STATUS_HEADER_NAME,
            BulkCnCreationHeaderNames.PRODUCT_CNC_STATUS_HEADER_NAME))) ?
            Integer.parseInt(Constant.DEFAULT_CNC_STATUS) :
            Integer.parseInt(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
                BulkCnCreationHeaderNames.PRODUCT_CNC_STATUS_HEADER_NAME,
                BulkCnCreationHeaderNames.PRODUCT_CNC_STATUS_HEADER_NAME)));
        boolean cncActivated = cncStatus == 1;
        if (cncForWarehouseFeatureSwitch) {
          pickupPointCreateRequest.setCncBuyable(cncActivated);
          pickupPointCreateRequest.setCncDisplay(cncActivated);
        } else {
          pickupPointCreateRequest.setCncActive(cncActivated);
        }
      }
      if (price > 0 && ((((price - salePrice) / price) * 100) > Constant.MAXIMUM_DISCOUNT_VALUE)) {
        buyableFlagUpdatedCount.getAndIncrement();
        pickupPointCreateRequest.setBuyable(false);
        pickupPointCreateRequest.setCncBuyable(false);
      }
    }
    Map<String, String> rawBfbValues =
        BulkCreationCommonUtil.getRawBfbValues(raw, BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_ID,
            BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_EN, BulkCnCreationHeaderNames.BFB_MANAGED_HEADER_ID,
            BulkCnCreationHeaderNames.BFB_MANAGED_HEADER_EN, BulkCnCreationHeaderNames.BFB_STATUS_HEADER);
    setB2bFieldInProductCreationRequest(merchantStatusType, pickupPointCreateRequest, rawBfbValues);
    return pickupPointCreateRequest;
  }

  private static Integer getDeliveryStatusValue(String deliveryStatusValue, boolean instoreSeller) {
    int deliveryStatus;
    if (StringUtils.isEmpty(deliveryStatusValue)) {
      if (instoreSeller) {
        deliveryStatus = Integer.parseInt(Constant.INSTORE_SELLER_DEFAULT_DELIVERY_STATUS);
      } else {
        deliveryStatus = Integer.parseInt(Constant.DEFAULT_DELIVERY_STATUS);
      }
    } else {
      deliveryStatus = Integer.parseInt(deliveryStatusValue);
    }
    return deliveryStatus;
  }

  public static void setB2bFieldInProductCreationRequest(MerchantStatusType merchantStatusType,
      PickupPointCreateRequest pickupPointCreateRequest, Map<String, String> rawBfbValues) {
    if (merchantStatusType.getType() >= MerchantStatusType.BFB.getType()) {
      Integer bfbBuyableStatus = StringUtils.isEmpty(rawBfbValues.get(BFB_STATUS)) ?
          Integer.parseInt(Constant.DEFAULT_BFB_BUYABLE_FLAG) :
          Integer.parseInt(rawBfbValues.get(BFB_STATUS));
      Integer bfbManagedStatus = StringUtils.isEmpty(rawBfbValues.get(BFB_MANAGED)) ?
          Integer.parseInt(Constant.DEFAULT_BFB_MANAGED_FLAG) :
          Integer.parseInt(rawBfbValues.get(BFB_MANAGED));
      Double bfbBasePrice = StringUtils.isEmpty(rawBfbValues.get(BFB_BASE_PRICE)) ?
          null :
          Double.parseDouble(rawBfbValues.get(BFB_BASE_PRICE));
      B2bDetailsDTO b2bDetailsDTO =
          new B2bDetailsDTO(bfbBasePrice, Constant.ONLINE == bfbBuyableStatus, Constant.ONLINE == bfbBuyableStatus,
              Constant.ONLINE == bfbManagedStatus);
      pickupPointCreateRequest.setB2bFields(b2bDetailsDTO);
    }
  }

  private static void setItemDetails(ProductItemCreationRequest itemCreationRequest, String itemGeneratedName,
      Map<String, Object> raw, Map<String, Object> parentRaw, AtomicInteger buyableFlagUpdatedCount,
      String ppNameDelimiter) {
    // Get item stock/price to use it in items if item stock/price is null
    Double parentPrice = Double.parseDouble(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw,
        BulkCnCreationHeaderNames.NORMAL_PRICE_ID_HEADER_NAME, BulkCnCreationHeaderNames.NORMAL_PRICE_EN_HEADER_NAME)));
    Double parentSalePrice = Double.parseDouble(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw,
            BulkCnCreationHeaderNames.NORMAL_PRICE_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.NORMAL_PRICE_EN_HEADER_NAME)));
    Integer parentStock = StringUtils.isBlank(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw,
            BulkCnCreationHeaderNames.AVAILABLE_STOCK_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.AVAILABLE_STOCK_EN_HEADER_NAME))) ?
        Integer.parseInt(Constant.DEFAULT_STOCK_VALUE) :
        Integer.parseInt(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw,
            BulkCnCreationHeaderNames.AVAILABLE_STOCK_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.AVAILABLE_STOCK_EN_HEADER_NAME)));
    Integer parentMinimumStock = StringUtils.isBlank(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw,
            BulkCnCreationHeaderNames.MINIMUM_STOCK_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.MINIMUM_STOCK_EN_HEADER_NAME))) ?
        Integer.parseInt(Constant.DEFAULT_STOCK_VALUE) :
        Integer.parseInt(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw,
            BulkCnCreationHeaderNames.MINIMUM_STOCK_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.MINIMUM_STOCK_EN_HEADER_NAME)));

    // Fecth product type and pickup point from parent since, it is same across all items
    Integer productType = (Integer) BulkUploadOption.valueFrom(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw,
            BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME))).getValue();
    String pickupPointCode = String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
        BulkCnCreationHeaderNames.PICKUP_POINT_CODE_ID_HEADER, BulkCnCreationHeaderNames.PICKUP_POINT_CODE_EN_HEADER));
    pickupPointCode = getPickupPointCodeFromPickupPointCodeAndName(ppNameDelimiter, pickupPointCode);
    if (itemGeneratedName.equals(raw.get(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME))) {
      Double price = parentPrice;
      if (!StringUtils.isBlank(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
          BulkCnCreationHeaderNames.NORMAL_PRICE_ID_HEADER_NAME, BulkCnCreationHeaderNames.NORMAL_PRICE_EN_HEADER_NAME)))) {
        try {
          price = Double.parseDouble(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
              BulkCnCreationHeaderNames.NORMAL_PRICE_ID_HEADER_NAME, BulkCnCreationHeaderNames.NORMAL_PRICE_EN_HEADER_NAME)));
        } catch (Exception e) {
          log.warn(Constant.WARNING_PREFIX, e);
        }
      }
      Double salePrice = parentSalePrice;
      if (!StringUtils.isBlank(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
          BulkCnCreationHeaderNames.SELLING_PRICE_ID_HEADER_NAME, BulkCnCreationHeaderNames.SELLING_PRICE_EN_HEADER_NAME)))) {
        try {
          salePrice =
              Double.parseDouble(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
                  BulkCnCreationHeaderNames.SELLING_PRICE_ID_HEADER_NAME, BulkCnCreationHeaderNames.SELLING_PRICE_EN_HEADER_NAME)));
        } catch (Exception e) {
          log.warn(Constant.WARNING_PREFIX, e);
        }
      }
      Integer stock = parentStock;
      if (!StringUtils.isBlank(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
          BulkCnCreationHeaderNames.AVAILABLE_STOCK_ID_HEADER_NAME, BulkCnCreationHeaderNames.AVAILABLE_STOCK_EN_HEADER_NAME)))) {
        try {
          stock = Integer.parseInt(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
              BulkCnCreationHeaderNames.AVAILABLE_STOCK_ID_HEADER_NAME, BulkCnCreationHeaderNames.AVAILABLE_STOCK_EN_HEADER_NAME)));
        } catch (Exception e) {
          log.warn(Constant.WARNING_PREFIX, e);
        }
      }

      Integer minimumStock = parentMinimumStock;
      if (!StringUtils.isBlank(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
          BulkCnCreationHeaderNames.MINIMUM_STOCK_ID_HEADER_NAME,
          BulkCnCreationHeaderNames.MINIMUM_STOCK_EN_HEADER_NAME)))) {
        try {
          minimumStock = Integer.parseInt(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
              BulkCnCreationHeaderNames.MINIMUM_STOCK_ID_HEADER_NAME,
              BulkCnCreationHeaderNames.MINIMUM_STOCK_EN_HEADER_NAME)));
        } catch (Exception e) {
          log.warn(Constant.WARNING_PREFIX, e);
        }
      }
      itemCreationRequest.setBundleRecipe(getBundleRecipe(raw));
      itemCreationRequest.setItemGeneratedName(String.valueOf(raw.get(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME)));
      itemCreationRequest.setProductType(productType);
      itemCreationRequest.setPrice(price);
      itemCreationRequest.setSalePrice(salePrice);
      itemCreationRequest.setStock(stock);
      itemCreationRequest.setMinimumStock(minimumStock);
      itemCreationRequest.setPickupPointId(pickupPointCode);
      // PAR--937
      if (price > 0 && ((((price - salePrice) / price) * 100) > Constant.MAXIMUM_DISCOUNT_VALUE)) {
        buyableFlagUpdatedCount.getAndIncrement();
        itemCreationRequest.setBuyable(false);

      }
      itemCreationRequest.setMerchantSku(String.valueOf(raw.get(BulkCnCreationHeaderNames.SELLER_SKU)));
      itemCreationRequest.setUpcCode(String.valueOf(raw.get(BulkCnCreationHeaderNames.EAN_UPC_CODE_HEADER)));
      itemCreationRequest.setImages(new ArrayList<>());
    }
  }

  public static Set<BundleRecipeRequest> getBundleRecipe(Map<String, Object> raw) {
    String childSkus = String.valueOf(Optional.ofNullable(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.CHILD_SKU_ID,
            BulkCnCreationHeaderNames.CHILD_SKU_EN)).orElse(StringUtils.EMPTY));
    String quantity = String.valueOf(Optional.ofNullable(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.QUANTITY_ID,
            BulkCnCreationHeaderNames.QUANTITY_EN)).orElse(StringUtils.EMPTY));
    if (StringUtils.isBlank(childSkus)) {
      return new HashSet<>();
    }
    List<String> childSkuList = Arrays.asList(childSkus.split(Constants.COMMA));
    List<String> quantityList = Arrays.asList(quantity.split(Constants.COMMA));
    return IntStream.range(0, childSkuList.size()).mapToObj(iterator -> {
      BundleRecipeRequest bundleRecipeRequest = new BundleRecipeRequest();
      bundleRecipeRequest.setItemSku(StringUtils.trimToEmpty(childSkuList.get(iterator)));
      bundleRecipeRequest.setQuantity(Integer.parseInt(StringUtils.trimToEmpty(quantityList.get(iterator))));
      return bundleRecipeRequest;
    }).collect(Collectors.toSet());
  }

  public static void initializeVariantCreationAttributes(Map<String, AttributeResponse> attributes,
      BulkProcess bulkProcess, boolean isInternationalMerchant, List<Map<String, Object>> groupRaw,
      ProductCreationRequest productCollectionRequest) {
    List<AttributeResponse> definingAttribute = BulkCreationCommonUtil.getVariantCreatingAttribute(attributes.values());
    for (AttributeResponse attributeResponse : definingAttribute) {
      Map<String, String> attributeValueInsensitiveAttributeValueMap = new HashMap<>();
      // Search defining and variant creation attribute values per column
      for (Map<String, Object> raw : groupRaw) {
        String attributeValue = String.valueOf(
            BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, attributeResponse.getName(),
                attributeResponse.getNameEnglish()));
        String lowerCaseAttributeValue = StringUtils.lowerCase(attributeValue);
        if (StringUtils.isNotBlank(attributeValue) && !attributeValueInsensitiveAttributeValueMap.containsKey(
            lowerCaseAttributeValue)) {
          attributeValueInsensitiveAttributeValueMap.put(lowerCaseAttributeValue, attributeValue);
        }
      }

      if (!org.springframework.util.CollectionUtils.isEmpty(attributeValueInsensitiveAttributeValueMap.values())) {
        ProductAttributeRequest productAttributeRequest =
            setValuesForProductAttributeRequest(bulkProcess.getStoreId(), attributeResponse, isInternationalMerchant);
        productAttributeRequest.setProductAttributeValues(new ArrayList<>());
        for (String attributeValue : attributeValueInsensitiveAttributeValueMap.values()) {
          ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
          productAttributeValueRequest.setStoreId(bulkProcess.getStoreId());
          initializeAttributes(attributeResponse, productAttributeValueRequest, attributeValue);
          productAttributeRequest.getProductAttributeValues().add(productAttributeValueRequest);
        }
        productCollectionRequest.getProductAttributes().add(productAttributeRequest);
      }
    }
  }

  public static void setBrandAttributeValue(Map<String, AttributeResponse> attributes,
      BulkProcess bulkProcess, boolean isInternationalMerchant, Map<String, Object> parentRaw,
      ProductCreationRequest productCollectionRequest) {
    String brandValue = String.valueOf(parentRaw.get(BulkCnCreationHeaderNames.BRAND_HEADER_NAME));
    String cleanBrandValue;
    if (brandValue.contains(Constant.IN_REVIEW)) {
      cleanBrandValue = brandValue.substring(0, brandValue.indexOf(Constant.IN_REVIEW_BRAND_SUFFIX));
    } else {
      cleanBrandValue = brandValue;
    }
    AttributeResponse brandResponse =
        attributes.get(BulkCnCreationHeaderNames.BRAND_HEADER_NAME);
    Optional<PredefinedAllowedAttributeValueResponse> optionalBrandValueResponse =
        brandResponse.getPredefinedAllowedAttributeValues().stream()
            .filter(predefinedValue -> predefinedValue.getValue().equals(cleanBrandValue)).findFirst();
    ProductAttributeValueRequest brandAttributeValueRequest = new ProductAttributeValueRequest();
    brandAttributeValueRequest.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
    brandAttributeValueRequest.setPredefinedAllowedAttributeValue(new PredefinedAllowedAttributeValueRequest());
    brandAttributeValueRequest.getPredefinedAllowedAttributeValue()
        .setId(optionalBrandValueResponse.map(PredefinedAllowedAttributeValueResponse::getId).orElse(StringUtils.EMPTY));
    brandAttributeValueRequest.getPredefinedAllowedAttributeValue().setValue(
        optionalBrandValueResponse.map(PredefinedAllowedAttributeValueResponse::getValue).orElse(StringUtils.EMPTY));
    ProductAttributeRequest brandAttributeRequest =
        setValuesForProductAttributeRequest(bulkProcess.getStoreId(), brandResponse, isInternationalMerchant);
    brandAttributeRequest.setProductAttributeValues(new ArrayList<>());
    brandAttributeRequest.getProductAttributeValues().add(brandAttributeValueRequest);
    productCollectionRequest.getProductAttributes().add(brandAttributeRequest);
    productCollectionRequest
        .setBrand(optionalBrandValueResponse.map(PredefinedAllowedAttributeValueResponse::getValue).orElse(null));
    productCollectionRequest.setBrandCode(
        optionalBrandValueResponse.map(PredefinedAllowedAttributeValueResponse::getPredefinedAllowedAttributeCode)
            .orElse(null));
    brandAttributeValueRequest.getPredefinedAllowedAttributeValue().setPredefinedAllowedAttributeCode(
        optionalBrandValueResponse.map(PredefinedAllowedAttributeValueResponse::getPredefinedAllowedAttributeCode)
            .orElse(null));
    if (brandValue.contains(Constant.IN_REVIEW)) {
      productCollectionRequest.setBrandApprovalStatus(BrandApprovalStatus.DRAFT.name());
    } else {
      productCollectionRequest.setBrandApprovalStatus(
          optionalBrandValueResponse.map(PredefinedAllowedAttributeValueResponse::getBrandApprovalStatus).orElse(null));
    }
  }

  public static void initializeNonVariantCreationAttributes(Map<String, AttributeResponse> attributes,
      BulkProcess bulkProcess, boolean isInternationalMerchant, Map<String, Object> parentRaw,
      ProductCreationRequest productCollectionRequest) {
    List<AttributeResponse> nonVariantCreatintAttributes =
        BulkCreationCommonUtil.getNonVariantCreatingAttribute(attributes.values());
    for (AttributeResponse attributeResponse : nonVariantCreatintAttributes) {
      if (!Constant.BRAND.equals(attributeResponse.getName()) && !Constant.COLOUR_FAMILY.equals(
          attributeResponse.getName())) {
        String attributeValue = String.valueOf(
            BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw, attributeResponse.getName(), attributeResponse.getNameEnglish()));
        ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
        productAttributeValueRequest.setStoreId(bulkProcess.getStoreId());
        initializeAttributes(attributeResponse, productAttributeValueRequest, attributeValue);
        ProductAttributeRequest productAttributeRequest = setValuesForProductAttributeRequest(bulkProcess.getStoreId(), attributeResponse, isInternationalMerchant);
        productAttributeRequest.setProductAttributeValues(new ArrayList<>());
        productAttributeRequest.getProductAttributeValues().add(productAttributeValueRequest);
        productCollectionRequest.getProductAttributes().add(productAttributeRequest);
      }
    }
  }

  public static ProductCreationRequest prepareProductRequest(BulkProcess bulkProcess, Map<String, Object> parentRaw,
      ProfileResponse businessPartner, CategoryDetailResponse categoryDetailResponse, boolean instoreSeller,
      List<Map<String, Object>> groupRaw) {
    boolean pureInstoreProduct = BulkCreationCommonUtil.isPureInstoreProduct(groupRaw, instoreSeller, false);
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setStoreId(bulkProcess.getStoreId());
    productCreationRequest.setCreatedBy(bulkProcess.getCreatedBy());
    productCreationRequest.setCreatedDate(Calendar.getInstance().getTime());
    productCreationRequest.setUpdatedBy(bulkProcess.getUpdatedBy());
    productCreationRequest.setUpdatedDate(new Date());
    productCreationRequest.setName(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw,
        BulkCnCreationHeaderNames.PRODUCT_NAME_ID_HEADER_NAME, BulkCnCreationHeaderNames.PRODUCT_NAME_EN_HEADER_NAME)));
    productCreationRequest.setUrl(String.valueOf(parentRaw.get(BulkCnCreationHeaderNames.YOUTUBE_URL_HEADER)));
    setProductDimension(parentRaw, productCreationRequest, pureInstoreProduct);
    String formattedDescription = getParagraphString(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw,
            BulkCnCreationHeaderNames.PRODUCT_DESCRIPTION_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.PRODUCT_DESCRIPTION_EN_HEADER_NAME)));
    productCreationRequest.setDescription(formattedDescription.getBytes());
    productCreationRequest.setLongDescription(formattedDescription.getBytes());
    productCreationRequest.setUniqueSellingPoint(getParagraphString(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw,
            BulkCnCreationHeaderNames.PRODUCT_USP_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.PRODUCT_USP_EN_HEADER_NAME))));
    // Default value specification detail property
    productCreationRequest.setSpecificationDetail(GenericBulkParameters.SEPARATOR);
    // Default value uom property = PC
    productCreationRequest.setUom(GenericBulkParameters.DEFAULT_UOM);
    productCreationRequest.setProductAttributes(new ArrayList<>());
    productCreationRequest.setProductCategories(new ArrayList<>());
    productCreationRequest.setBusinessPartnerCode(businessPartner.getBusinessPartnerCode());
    productCreationRequest.setBusinessPartnerName(businessPartner.getCompany().getBusinessPartnerName());
    if (businessPartner.getCompany().isInternationalFlag() && StringUtils
        .isNotEmpty(categoryDetailResponse.getNameEnglish())) {
      productCreationRequest.setCategoryName(categoryDetailResponse.getNameEnglish());
    } else {
      productCreationRequest.setCategoryName(categoryDetailResponse.getName());
    }
    if (instoreSeller) {
      Set<ProductSalesChannelType> productSalesChannelTypes = getProductSalesChannel(instoreSeller, groupRaw, false);
      productCreationRequest.setB2cActivated(productSalesChannelTypes.contains(ProductSalesChannelType.B2C_PRODUCT));
      productCreationRequest.setOff2OnChannelActive(
          productSalesChannelTypes.contains(ProductSalesChannelType.INSTORE_PRODUCT));
    }
    return productCreationRequest;
  }

  private static void setProductDimension(Map<String, Object> parentRaw, ProductCreationRequest productCreationRequest,
      boolean pureInstoreProduct) {
    double length = 0.0;
    double width = 0.0;
    double height = 0.0;
    double weight = 0.0;
    if (isShippingTypeNotBopis(parentRaw, BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME,
        BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME)) {
      length = parseDimensionForRequestFormation(String.valueOf(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw, BulkCnCreationHeaderNames.LENGTH_ID_HEADER_NAME,
              BulkCnCreationHeaderNames.LENGTH_EN_HEADER_NAME)));
      width = parseDimensionForRequestFormation(String.valueOf(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw, BulkCnCreationHeaderNames.WIDTH_ID_HEADER_NAME,
              BulkCnCreationHeaderNames.WIDTH_EN_HEADER_NAME)));
      height = parseDimensionForRequestFormation(String.valueOf(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw, BulkCnCreationHeaderNames.HEIGHT_ID_HEADER_NAME,
              BulkCnCreationHeaderNames.HEIGHT_EN_HEADER_NAME)));
      weight = getKilogramFromGram(parseDimensionForRequestFormation(String.valueOf(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(parentRaw, BulkCnCreationHeaderNames.WEIGHT_ID_HEADER_NAME,
              BulkCnCreationHeaderNames.WEIGHT_EN_HEADER_NAME))));
      if (pureInstoreProduct && Stream.of(length, width, height, weight)
          .anyMatch(dimension -> Double.compare(dimension, MINIMUM_VALUE) == 0)) {
        length = 0.0;
        width = 0.0;
        height = 0.0;
        weight = 0.0;
      }
    }
    productCreationRequest.setLength(length);
    productCreationRequest.setWidth(width);
    productCreationRequest.setHeight(height);
    productCreationRequest.setWeight(weight);
  }

  public static boolean validateFamilyColour(int idx, List<Object> row, Map<String, List<String>> attributeValuesMap) {
    boolean result = true;
    String value = String.valueOf(row.get(idx));
    if (StringUtils.isNotBlank(value)) {
      List<String> attributeValues = attributeValuesMap.get(Constant.COLOUR_FAMILY);
      if (Objects.nonNull(attributeValues) && !attributeValues.contains(value)) {
        result = false;
      }
    }
    return result;
  }

  public static boolean validateGroupRowAttributes(Map<String, Object> raw, Map<String, AttributeResponse> attributes,
      BulkProcessNotes bulkProcessNotes, BulkUploadErrorCounter bulkUploadErrorCounter,
      boolean isInternationalMerchant) {
    String resultErrorMessage;
    boolean result = true;
    StringBuilder validationErrorMessage = new StringBuilder();
    List<AttributeResponse> nonVariantCreatingAttribute =  getNonVariantCreatingAttribute(attributes.values());
    String columnRowInformation = String.valueOf(raw.get(BulkCnCreationHeaderNames.ROW_NUMBER));
    for (AttributeResponse attributeResponse : nonVariantCreatingAttribute) {
      //checking only for mandatory fields
      if (StringUtils.isBlank(String.valueOf(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, attributeResponse.getName(),
              attributeResponse.getNameEnglish()))) && attributeResponse.isMandatory()) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.ATTRIBUTE_VALUE_INVALID_EN,
            BulkProcessValidationErrorMessages.ATTRIBUTE_VALUE_INVALID);
        bulkUploadErrorCounter.incrementFeature();
        addValidationErrorMessage(bulkUploadErrorCounter.getFeature(), validationErrorMessage,
            errorMessage(getAttributeName(isInternationalMerchant, attributeResponse) + " - ", resultErrorMessage,
                StringUtils.EMPTY));
        log.error("Bulk process: {}, Row {} - {}", bulkProcessNotes.getBulkProcessCode(),
            (Integer.parseInt(columnRowInformation) + 1), BulkProcessValidationErrorMessages.ATTRIBUTE_VALUE_INVALID);
        result = false;
        continue;
      }
      if (attributeResponse.isMandatory() && AttributeType.PREDEFINED_ATTRIBUTE.name()
          .equals(attributeResponse.getAttributeType()) && Constant.HYPHEN.equals(String.valueOf(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, attributeResponse.getName(),
              attributeResponse.getNameEnglish())))) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.ATTRIBUTE_VALUE_INVALID_EN,
            BulkProcessValidationErrorMessages.ATTRIBUTE_VALUE_INVALID);
        bulkUploadErrorCounter.incrementFeature();
        addValidationErrorMessage(bulkUploadErrorCounter.getFeature(), validationErrorMessage,
            errorMessage(getAttributeName(isInternationalMerchant, attributeResponse) + " - ", resultErrorMessage,
                StringUtils.EMPTY));
        log.error("Bulk process: {}, Row {} - {}", bulkProcessNotes.getBulkProcessCode(),
            (Integer.parseInt(columnRowInformation) + 1), BulkProcessValidationErrorMessages.ATTRIBUTE_VALUE_INVALID);
        result = false;
      }
    }
    //check whether mandatory feature attributes value is empty or not
    if (!result) {
      bulkProcessNotes.setNotes(validationErrorMessage.toString());
    }
    return result;
  }

  public static boolean validateExcelRows(ValidateExcelRowsRequest validateExcelRowsRequest) {
    Map<String, Object> raw = validateExcelRowsRequest.getRaw();
    BulkProcessNotes bulkProcessNotes = validateExcelRowsRequest.getBulkProcessNotes();
    BulkUploadErrorCounter bulkUploadErrorCounter = validateExcelRowsRequest.getBulkUploadErrorCounter();
    boolean isInternationalMerchant = validateExcelRowsRequest.isInternationalMerchant();
    Integer minimumPrice = validateExcelRowsRequest.getMinimumPrice();
    long maxStockLimit = validateExcelRowsRequest.getMaxStockLimit();
    MerchantStatusType merchantStatusType = validateExcelRowsRequest.getMerchantStatusType();
    int productBundlingMaxNumberOfSkus = validateExcelRowsRequest.getProductBundlingMaxNumberOfSkus();
    boolean productBundlingEnabled = validateExcelRowsRequest.isProductBundlingEnabled();
    String merchantType = validateExcelRowsRequest.getMerchantType();
    String productBundlingEligibleMerchantTypes = validateExcelRowsRequest.getProductBundlingEligibleMerchantTypes();
    String commonImageErrorMessage = validateExcelRowsRequest.getCommonImageErrorMessage();
    boolean bopisCncRestrictionEnabled = validateExcelRowsRequest.isBopisCncRestrictionEnabled();
    String primaryIdentifier = validateExcelRowsRequest.getPrimaryIdentifier();
    boolean instoreSeller = validateExcelRowsRequest.isInstoreSeller();
    boolean pureInstoreProduct = validateExcelRowsRequest.isPureInstoreProduct();
    String resultErrorMessage;
    String columnRowInformation = String.valueOf(raw.get(BulkCnCreationHeaderNames.ROW_NUMBER));
    boolean result = true;
    Double price = 0.0;
    Double salePrice = 0.0;
    Double stock = 0.0;
    Double minimumStock = 0.0;
    Double weight = 0.0;
    Double length = 0.0;
    Double width = 0.0;
    Double height = 0.0;
    String productName = String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.PRODUCT_NAME_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.PRODUCT_NAME_EN_HEADER_NAME));
    StringBuilder validationErrorMessage = new StringBuilder();
    if (StringUtils.isBlank(productName)) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.PRODUCT_NAME_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.PRODUCT_NAME_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementProductName();
      addValidationErrorMessage(bulkUploadErrorCounter.getProductName(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row {}. Product Name - empty/blank. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1),
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.PRODUCT_NAME_MUST_NOT_BE_BLANK);
    } else if (productName.length() > 150) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.PRODUCT_NAME_MUST_NOT_BE_MORE_THAN_150_CHARACTERS_EN,
          BulkProcessValidationErrorMessages.PRODUCT_NAME_MUST_NOT_BE_MORE_THAN_150_CHARACTERS);
      bulkUploadErrorCounter.incrementProductName();
      addValidationErrorMessage(bulkUploadErrorCounter.getProductName(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error(
          "Validation error bulk process : {}, Row {}. Product Name - exceeded maximum characters length. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1),
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.PRODUCT_NAME_MUST_NOT_BE_MORE_THAN_150_CHARACTERS);
    }
    if (String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
        BulkCnCreationHeaderNames.PRODUCT_DESCRIPTION_ID_HEADER_NAME,
        BulkCnCreationHeaderNames.PRODUCT_DESCRIPTION_EN_HEADER_NAME)).length() > 5000) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.DESCRIPTION_MUST_NOT_BE_MORE_THAN_5000_CHARACTERS_EN,
          BulkProcessValidationErrorMessages.DESCRIPTION_MUST_NOT_BE_MORE_THAN_5000_CHARACTERS);
      bulkUploadErrorCounter.incrementDescription();
      addValidationErrorMessage(bulkUploadErrorCounter.getDescription(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error(
          "Validation error bulk process : {}, Row {}. Description - exceeded maximum characters length. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1),
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.DESCRIPTION_MUST_NOT_BE_MORE_THAN_5000_CHARACTERS);
    }
    if (String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.PRODUCT_USP_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.PRODUCT_USP_EN_HEADER_NAME)).length() > 400) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS_EN,
          BulkProcessValidationErrorMessages.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS);
      bulkUploadErrorCounter.incrementUniqueSellingPoint();
      addValidationErrorMessage(bulkUploadErrorCounter.getUniqueSellingPoint(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error(
          "Validation error bulk process : {}, Row {}. Unique selling point - exceeded maximum characters length. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1),
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS);
    }
    result = validateCommonImagesForEachVariant(bulkProcessNotes, bulkUploadErrorCounter, commonImageErrorMessage,
        columnRowInformation, result, productName, validationErrorMessage);

    overrideEmptyProductType(validateExcelRowsRequest, raw, primaryIdentifier,
        BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME, BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME);

    Pair<StringBuilder, Boolean> deliveryStatusValidation =
        validateDeliveryStatusByMerchantType(raw, productName, columnRowInformation, bulkProcessNotes,
            bulkUploadErrorCounter, result, validationErrorMessage, isInternationalMerchant, merchantStatusType,
            bopisCncRestrictionEnabled, primaryIdentifier, instoreSeller);
    validationErrorMessage = deliveryStatusValidation.getLeft();
    result = deliveryStatusValidation.getRight();

    if (StringUtils.isBlank(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME)))) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.PRODUCT_TYPE_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.PRODUCT_TYPE_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementProductType();
      addValidationErrorMessage(bulkUploadErrorCounter.getProductType(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row {}. Product Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.PRODUCT_TYPE_MUST_NOT_BE_BLANK);
    } else {
      String productType = String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME,
          BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME));
      List<String> shippingTypeEligibleValues;
      if (isInternationalMerchant) {
        shippingTypeEligibleValues = BulkParameters.BULK_OPTION_UPLOAD_SUPPORT_EN;
      } else {
        shippingTypeEligibleValues = BulkParameters.BULK_OPTION_UPLOAD_SUPPORT;
      }
      if (!shippingTypeEligibleValues.contains(productType)) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.PRODUCT_TYPE_INVALID_EN,
            BulkProcessValidationErrorMessages.PRODUCT_TYPE_INVALID);
        bulkUploadErrorCounter.incrementProductType();
        addValidationErrorMessage(bulkUploadErrorCounter.getProductType(), validationErrorMessage,
           errorMessage(StringUtils.EMPTY,resultErrorMessage,StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process: {}, Row {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.PRODUCT_TYPE_INVALID);
      }
    }
    if (StringUtils.isBlank(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.NORMAL_PRICE_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.NORMAL_PRICE_EN_HEADER_NAME)))) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.PRICE_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.PRICE_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementHarga();
      addValidationErrorMessage(bulkUploadErrorCounter.getHarga(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process: {}, Row {}. Product Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.PRICE_MUST_NOT_BE_BLANK);
    } else {
      try {
        price = Double.parseDouble(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
            BulkCnCreationHeaderNames.NORMAL_PRICE_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.NORMAL_PRICE_EN_HEADER_NAME)));
      } catch (Exception e) {
        resultErrorMessage =
            errorMessageBasedOnMerchant(isInternationalMerchant, BulkProcessValidationErrorMessages.PRICE_INVALID_EN,
                BulkProcessValidationErrorMessages.PRICE_INVALID);
        log.warn(Constant.WARNING_PREFIX, e);
        bulkUploadErrorCounter.incrementHarga();
        addValidationErrorMessage(bulkUploadErrorCounter.getHarga(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process: {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.PRICE_INVALID);
      }
      if (price % 1 != 0) {
        raw.put(
            BulkCreationCommonUtil.getAttributeNameBasedOnHeader(BulkCnCreationHeaderNames.NORMAL_PRICE_EN_HEADER_NAME,
                BulkCnCreationHeaderNames.NORMAL_PRICE_ID_HEADER_NAME, isInternationalMerchant),
            (int) Math.round(price));
      }
    }
    if (StringUtils.isBlank(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.SELLING_PRICE_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.SELLING_PRICE_EN_HEADER_NAME)))) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.SALE_PRICE_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.SALE_PRICE_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementHargaPenjualan();
      addValidationErrorMessage(bulkUploadErrorCounter.getHargaPenjualan(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process: {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.SALE_PRICE_MUST_NOT_BE_BLANK);
    } else {
      try {
        salePrice = Double.parseDouble(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
            BulkCnCreationHeaderNames.SELLING_PRICE_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.SELLING_PRICE_EN_HEADER_NAME)));
      } catch (Exception e) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.SALE_PRICE_INVALID_EN,
            BulkProcessValidationErrorMessages.SALE_PRICE_INVALID);
        log.warn(Constant.WARNING_PREFIX, e);
        bulkUploadErrorCounter.incrementHargaPenjualan();
        addValidationErrorMessage(bulkUploadErrorCounter.getHargaPenjualan(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process: {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.SALE_PRICE_INVALID);
      }
      if ((price < minimumPrice)) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.MINIMUM_PRICE_VALUE_INVALID_EN,
            BulkProcessValidationErrorMessages.MINIMUM_PRICE_VALUE_INVALID);
        bulkUploadErrorCounter.incrementHarga();
        addValidationErrorMessage(bulkUploadErrorCounter.getHarga(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, Double.toString((double) minimumPrice)));
        result = false;
        log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.MINIMUM_PRICE_VALUE_INVALID);
      }
      if ((salePrice < minimumPrice)) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.MINIMUM_SALE_PRICE_VALUE_INVALID_EN,
            BulkProcessValidationErrorMessages.MINIMUM_SALE_PRICE_VALUE_INVALID);
        bulkUploadErrorCounter.incrementHargaPenjualan();
        addValidationErrorMessage(bulkUploadErrorCounter.getHargaPenjualan(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, Double.toString((double) minimumPrice)));
        result = false;
        log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.MINIMUM_PRICE_VALUE_INVALID);
      }
      if (salePrice > price) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.MAXIMUM_SALE_PRICE_VALUE_INVALID_EN,
            BulkProcessValidationErrorMessages.MAXIMUM_SALE_PRICE_VALUE_INVALID);
        bulkUploadErrorCounter.incrementHargaPenjualan();
        addValidationErrorMessage(bulkUploadErrorCounter.getHargaPenjualan(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.MAXIMUM_SALE_PRICE_VALUE_INVALID);
      }
      if (salePrice % 1 != 0) {
        raw.put(BulkCreationCommonUtil.getAttributeNameBasedOnHeader(BulkCnCreationHeaderNames.SELLING_PRICE_EN_HEADER_NAME,
                BulkCnCreationHeaderNames.SELLING_PRICE_ID_HEADER_NAME, isInternationalMerchant), (int) Math.round(salePrice));
      }
    }
    String bfbBasePriceHeader =
        BulkCreationCommonUtil.getAttributeNameBasedOnHeader(BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_EN,
            BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_ID, isInternationalMerchant);
    Pair<StringBuilder, Boolean> basePriceValidationResult =
        validateBfbBasePrice(raw, productName, columnRowInformation, bulkProcessNotes, bulkUploadErrorCounter, result,
            validationErrorMessage, isInternationalMerchant, merchantStatusType, String.valueOf(
                BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
                    BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_ID,
                    BulkCnCreationHeaderNames.BFB_BASE_PRICE_HEADER_EN)), bfbBasePriceHeader);
    validationErrorMessage = basePriceValidationResult.getLeft();
    result = basePriceValidationResult.getRight();

      try {
        String stockValue = String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
            BulkCnCreationHeaderNames.AVAILABLE_STOCK_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.AVAILABLE_STOCK_EN_HEADER_NAME));
        if (StringUtils.isBlank(stockValue)) {
          stockValue = Constant.DEFAULT_STOCK_VALUE;
        }
        stock = Double.parseDouble(stockValue);
      } catch (Exception e) {
        resultErrorMessage =
            errorMessageBasedOnMerchant(isInternationalMerchant, BulkProcessValidationErrorMessages.STOCK_INVALID_EN,
                BulkProcessValidationErrorMessages.STOCK_INVALID);
        log.warn(Constant.WARNING_PREFIX, e);
        bulkUploadErrorCounter.incrementStock();
        addValidationErrorMessage(bulkUploadErrorCounter.getStock(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, : Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.STOCK_INVALID);
      }
      if (stock < 0) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.MINIMUM_STOCK_VALUE_INVALID_EN,
            BulkProcessValidationErrorMessages.MINIMUM_STOCK_VALUE_INVALID);
        bulkUploadErrorCounter.incrementStock();
        addValidationErrorMessage(bulkUploadErrorCounter.getStock(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process: {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.MINIMUM_STOCK_VALUE_INVALID);
      }
      if (stock > maxStockLimit) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            String.format(BulkProcessValidationErrorMessages.MAXIMUM_STOCK_VALUE_INVALID_EN, maxStockLimit),
            String.format(BulkProcessValidationErrorMessages.MAXIMUM_STOCK_VALUE_INVALID, maxStockLimit));
        bulkUploadErrorCounter.incrementStock();
        addValidationErrorMessage(bulkUploadErrorCounter.getStock(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process: {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.MAXIMUM_STOCK_VALUE_INVALID);
      }
      try {
        String minStockValue = String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
            BulkCnCreationHeaderNames.MINIMUM_STOCK_EN_HEADER_NAME,
            BulkCnCreationHeaderNames.MINIMUM_STOCK_ID_HEADER_NAME));
        if (StringUtils.isBlank(minStockValue)) {
          minStockValue = Constant.DEFAULT_STOCK_VALUE;
        }
        minimumStock = Double.parseDouble(minStockValue);
      } catch (Exception e) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.MINIMUM_STOCK_INVALID_EN,
            BulkProcessValidationErrorMessages.MINIMUM_STOCK_INVALID);
        log.warn(Constant.WARNING_PREFIX, e);
        bulkUploadErrorCounter.incrementStock();
        addValidationErrorMessage(bulkUploadErrorCounter.getStock(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.MINIMUM_STOCK_INVALID);
      }
      if (minimumStock < 0) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.MINIMUM_MIN_STOCK_VALUE_INVALID_EN,
            BulkProcessValidationErrorMessages.MINIMUM_MIN_STOCK_VALUE_INVALID);
        bulkUploadErrorCounter.incrementStock();
        addValidationErrorMessage(bulkUploadErrorCounter.getStock(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));

        result = false;
        log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.MINIMUM_MIN_STOCK_VALUE_INVALID);
      }
    if (StringUtils.isBlank(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
        BulkCnCreationHeaderNames.PICKUP_POINT_CODE_ID_HEADER,
        BulkCnCreationHeaderNames.PICKUP_POINT_CODE_EN_HEADER)))) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementPickupPoint();
      addValidationErrorMessage(bulkUploadErrorCounter.getPickupPoint(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row : {}. Product Name " + "- {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    }
    if (isShippingTypeNotBopis(raw, BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME,
        BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME)) {
      result = validateProductDimension(raw, bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant,
          columnRowInformation, result, weight, length, width, height, productName, validationErrorMessage,
          pureInstoreProduct);
    }
    if (StringUtils.isBlank(String.valueOf(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
        BulkCnCreationHeaderNames.PRODUCT_DESCRIPTION_ID_HEADER_NAME,
        BulkCnCreationHeaderNames.PRODUCT_DESCRIPTION_EN_HEADER_NAME)))) && !pureInstoreProduct) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.DESCRIPTION_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.DESCRIPTION_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementDescription();
      addValidationErrorMessage(bulkUploadErrorCounter.getDescription(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY,resultErrorMessage,StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.DESCRIPTION_MUST_NOT_BE_BLANK);
    }
    if (ExcelTemplateUtil.isEligibleForBundleCreation(merchantType, productBundlingEligibleMerchantTypes,
        productBundlingEnabled)) {
      result = BulkCreationCommonUtil.validateBundlingInfo(raw, bulkProcessNotes, bulkUploadErrorCounter,
          isInternationalMerchant, productBundlingMaxNumberOfSkus, validationErrorMessage, result);
    }
    result = validateInstoreStatus(
        getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.INSTORE, BulkCnCreationHeaderNames.INSTORE),
        bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant, columnRowInformation, productName,
        validationErrorMessage, result, instoreSeller);
    if (!result) {
      bulkProcessNotes.setNotes(validationErrorMessage.toString());
    }
    return result;
  }

  public static void overrideEmptyProductType(ValidateExcelRowsRequest validateExcelRowsRequest,
      Map<String, Object> raw, String primaryIdentifier, String headerName, String headerNameEn) {
    if (validateExcelRowsRequest.isOverrideEmptyProductTypeBulkCreation() && StringUtils.isBlank(
        String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, headerNameEn, headerName)))) {
      log.info("Overriding empty productType for row : {}", raw);
      raw.put(headerNameEn, BulkUploadOption.SHIPPING_TYPE_BOPIS.getdescription());
      raw.put(headerName, BulkUploadOption.SHIPPING_TYPE_BOPIS.getdescription());

      if (!validateExcelRowsRequest.isSellerBopisEligible() || (isExternalUserOnly(primaryIdentifier)
          && Optional.ofNullable(validateExcelRowsRequest.getBopisCategoryValidationForSellerTypes())
          .orElse(StringUtils.EMPTY).contains(validateExcelRowsRequest.getMerchantType())
          && !validateExcelRowsRequest.isCategoryBopisEligible())) {
        raw.put(headerNameEn, BulkUploadOption.SHIPPING_TYPE_ASSIGNED_BY_BLIBLI_EN.getdescription());
        raw.put(headerName, BulkUploadOption.SHIPPING_TYPE_ASSIGNED_BY_BLIBLI.getdescription());
      }
    }
  }

  private static boolean validateProductDimension(Map<String, Object> raw, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String columnRowInformation,
      boolean result, Double weight, Double length, Double width, Double height, String productName,
      StringBuilder validationErrorMessage, boolean pureInstoreProduct) {
    String resultErrorMessage;
    if (StringUtils.isBlank(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.LENGTH_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.LENGTH_EN_HEADER_NAME))) && !pureInstoreProduct) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.LENGTH_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.LENGTH_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementLength();
      addValidationErrorMessage(bulkUploadErrorCounter.getLength(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row : {}. Product Name " + "- {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.LENGTH_MUST_NOT_BE_BLANK);
    } else {
      try {
        length = parseDimensionForValidation(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
            BulkCnCreationHeaderNames.LENGTH_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.LENGTH_EN_HEADER_NAME)));
      } catch (Exception e) {
        resultErrorMessage =
            errorMessageBasedOnMerchant(isInternationalMerchant, BulkProcessValidationErrorMessages.LENGTH_INVALID_EN,
                BulkProcessValidationErrorMessages.LENGTH_INVALID);
        log.warn(Constant.WARNING_PREFIX, e);
        bulkUploadErrorCounter.incrementLength();
        addValidationErrorMessage(bulkUploadErrorCounter.getLength(), validationErrorMessage, isInternationalMerchant ?
            errorMessage(StringUtils.EMPTY, BulkProcessValidationErrorMessages.LENGTH_INVALID_EN, StringUtils.EMPTY) :
            errorMessage(StringUtils.EMPTY, BulkProcessValidationErrorMessages.LENGTH_INVALID, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.LENGTH_INVALID);
      }
      if (length < 0) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.MINIMUM_LENGTH_VALUE_INVALID_EN,
            BulkProcessValidationErrorMessages.MINIMUM_LENGTH_VALUE_INVALID);
        bulkUploadErrorCounter.incrementLength();
        addValidationErrorMessage(bulkUploadErrorCounter.getLength(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, : Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.MINIMUM_LENGTH_VALUE_INVALID);
      }
    }

    if (StringUtils.isBlank(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.WIDTH_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.WIDTH_EN_HEADER_NAME))) && !pureInstoreProduct) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.WIDTH_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.WIDTH_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementWidth();
      addValidationErrorMessage(bulkUploadErrorCounter.getWidth(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row : {}. Product " + "Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.WIDTH_MUST_NOT_BE_BLANK);
    } else {
      try {
        width = parseDimensionForValidation(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
            BulkCnCreationHeaderNames.WIDTH_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.WIDTH_EN_HEADER_NAME)));
      } catch (Exception e) {
        resultErrorMessage =
            errorMessageBasedOnMerchant(isInternationalMerchant, BulkProcessValidationErrorMessages.WIDTH_INVALID_EN,
                BulkProcessValidationErrorMessages.WIDTH_INVALID);
        log.warn(Constant.WARNING_PREFIX, e);
        bulkUploadErrorCounter.incrementWidth();
        addValidationErrorMessage(bulkUploadErrorCounter.getWidth(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, : Row : {}. Product " + "Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.WIDTH_INVALID);
      }
      if (width < 0) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.MINIMUM_WIDTH_VALUE_INVALID_EN,
            BulkProcessValidationErrorMessages.MINIMUM_WIDTH_VALUE_INVALID);
        bulkUploadErrorCounter.incrementWidth();
        addValidationErrorMessage(bulkUploadErrorCounter.getWidth(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, Row : {}. Product " + "Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.MINIMUM_WIDTH_VALUE_INVALID);
      }
    }
    if (StringUtils.isBlank(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.HEIGHT_EN_HEADER_NAME,
            BulkCnCreationHeaderNames.HEIGHT_ID_HEADER_NAME))) && !pureInstoreProduct) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.HEIGHT_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.HEIGHT_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementHeight();
      addValidationErrorMessage(bulkUploadErrorCounter.getHeight(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.HEIGHT_MUST_NOT_BE_BLANK);
    } else {
      try {
        height = parseDimensionForValidation(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
            BulkCnCreationHeaderNames.HEIGHT_EN_HEADER_NAME,
            BulkCnCreationHeaderNames.HEIGHT_ID_HEADER_NAME)));
      } catch (Exception e) {
        resultErrorMessage =
            errorMessageBasedOnMerchant(isInternationalMerchant, BulkProcessValidationErrorMessages.HEIGHT_INVALID_EN,
                BulkProcessValidationErrorMessages.HEIGHT_INVALID);
        log.warn(Constant.WARNING_PREFIX, e);
        bulkUploadErrorCounter.incrementHeight();
        addValidationErrorMessage(bulkUploadErrorCounter.getHeight(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.HEIGHT_INVALID);
      }
      if (height < 0) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.MINIMUM_HEIGHT_VALUE_INVALID_EN,
            BulkProcessValidationErrorMessages.MINIMUM_HEIGHT_VALUE_INVALID);
        bulkUploadErrorCounter.incrementHeight();
        addValidationErrorMessage(bulkUploadErrorCounter.getHeight(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, Row : {}. Product " + "Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.MINIMUM_HEIGHT_VALUE_INVALID);
      }
    }
    if (StringUtils.isBlank(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.WEIGHT_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.WEIGHT_EN_HEADER_NAME))) && !pureInstoreProduct) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.WEIGHT_MUST_NOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.WEIGHT_MUST_NOT_BE_BLANK);
      bulkUploadErrorCounter.incrementWeight();
      addValidationErrorMessage(bulkUploadErrorCounter.getWeight(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.WEIGHT_MUST_NOT_BE_BLANK);
    } else {
      try {
        weight = parseDimensionForValidation(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
            BulkCnCreationHeaderNames.WEIGHT_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.WEIGHT_EN_HEADER_NAME)));
      } catch (Exception e) {
        resultErrorMessage =
            errorMessageBasedOnMerchant(isInternationalMerchant, BulkProcessValidationErrorMessages.WEIGHT_INVALID_EN,
                BulkProcessValidationErrorMessages.WEIGHT_INVALID);
        log.warn(Constant.WARNING_PREFIX, e);
        bulkUploadErrorCounter.incrementWeight();
        addValidationErrorMessage(bulkUploadErrorCounter.getWeight(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.WEIGHT_INVALID);
      }
      if (weight < Constant.WEIGHT && !BulkUploadOption.SHIPPING_TYPE_BOPIS.getdescription().equals(String.valueOf(String.valueOf(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.PRODUCT_TYPE_ID_HEADER_NAME,
              BulkCnCreationHeaderNames.PRODUCT_TYPE_EN_HEADER_NAME))))) {
        resultErrorMessage =
            errorMessageBasedOnMerchant(isInternationalMerchant, BulkProcessValidationErrorMessages.WRONG_WEIGHT_EN,
                BulkProcessValidationErrorMessages.WRONG_WEIGHT);
        bulkUploadErrorCounter.incrementWeight();
        addValidationErrorMessage(bulkUploadErrorCounter.getWeight(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(), BulkProcessValidationErrorMessages.WRONG_WEIGHT);
      }
    }
    return result;
  }

  private static boolean validateCommonImagesForEachVariant(BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, String commonImageErrorMessage, String columnRowInformation,
      boolean result, String productName, StringBuilder validationErrorMessage) {
    //If Common images are empty or incorrect in the sheet for first row, fail all the rows of the multi-variant product
    if (StringUtils.isNotBlank(commonImageErrorMessage)) {
      bulkUploadErrorCounter.incrementImage();
      validationErrorMessage.append(commonImageErrorMessage).append(Constant.PERIOD);
      result = false;
      log.error("Validation error bulk process : {}, Row {}. Product Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
          BulkErrorCategory.INPUT_ERROR.getDescription(), commonImageErrorMessage);
    }
    return result;
  }

  public static Pair<StringBuilder, Boolean> validateBfbBasePrice(Map<String, Object> raw, String productName,
      String columnRowInformation, BulkProcessNotes bulkProcessNotes, BulkUploadErrorCounter bulkUploadErrorCounter,
      boolean result, StringBuilder validationErrorMessage, boolean isInternationalMerchant,
      MerchantStatusType merchantStatusType, String rawBfbBasePrice, String bfbBasePriceHeader) {
    String resultErrorMessage;
    Double price = null;
    if (merchantStatusType.getType() >= MerchantStatusType.BFB.getType()) {
      if (StringUtils.isBlank(rawBfbBasePrice)) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.BFB_BASE_PRICE_MUST_NOT_BE_BLANK_EN,
            BulkProcessValidationErrorMessages.BFB_BASE_PRICE_MUST_NOT_BE_BLANK_EN);
        bulkUploadErrorCounter.incrementHarga();
        addValidationErrorMessage(bulkUploadErrorCounter.getHarga(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process: {}, Row {}. Product Name - {}. Error msg - {} : {}.",
            bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
            BulkErrorCategory.INPUT_ERROR.getDescription(),
            BulkProcessValidationErrorMessages.BFB_BASE_PRICE_MUST_NOT_BE_BLANK_EN);
        return Pair.of(validationErrorMessage, result);
      } else {
        try {
          price = Double.parseDouble(rawBfbBasePrice);
        } catch (Exception e) {
          resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
              BulkProcessValidationErrorMessages.BFB_BASE_PRICE_INVALID_EN,
              BulkProcessValidationErrorMessages.BFB_BASE_PRICE_INVALID_EN);
          log.warn(Constant.WARNING_PREFIX, e);
          bulkUploadErrorCounter.incrementHarga();
          addValidationErrorMessage(bulkUploadErrorCounter.getHarga(), validationErrorMessage,
              errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
          result = false;
          log.error("Validation error bulk process: {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
              bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
              BulkErrorCategory.INPUT_ERROR.getDescription(),
              BulkProcessValidationErrorMessages.BFB_BASE_PRICE_INVALID_EN);
          return Pair.of(validationErrorMessage, result);
        }
        if (price <= Constant.ZERO) {
          resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
              BulkProcessValidationErrorMessages.BFB_BASE_PRICE_CANNOT_BE_ZERO_EN,
              BulkProcessValidationErrorMessages.BFB_BASE_PRICE_CANNOT_BE_ZERO_EN);
          bulkUploadErrorCounter.incrementHargaPenjualan();
          addValidationErrorMessage(bulkUploadErrorCounter.getHargaPenjualan(), validationErrorMessage,
              errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
          result = false;
          log.error("Validation error bulk process : {}, Row : {}. Product Name - {}. Error msg - {} : {}.",
              bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1), productName,
              BulkErrorCategory.INPUT_ERROR.getDescription(),
              BulkProcessValidationErrorMessages.MAXIMUM_SALE_PRICE_VALUE_INVALID);
          return Pair.of(validationErrorMessage, result);
        }
        if (price % 1 != 0) {
          raw.put(bfbBasePriceHeader, (int) Math.round(price));
        }
      }
    }
    return Pair.of(validationErrorMessage, result);
  }

  public static Pair<StringBuilder, Boolean> validateDeliveryStatusByMerchantType(Map<String, Object> raw,
      String productName, String columnRowInformation, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean result, StringBuilder validationErrorMessage,
      boolean isInternationalMerchant, MerchantStatusType merchantStatusType, boolean bopisCncRestrictionEnabled,
      String primaryIdentifier, boolean instoreSeller) {
    result = validateDeliveryStatus(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw,
            BulkCnCreationHeaderNames.PRODUCT_DELIVERY_STATUS_ID_HEADER_NAME,
            BulkCnCreationHeaderNames.PRODUCT_DELIVERY_STATUS_EN_HEADER_NAME), bulkProcessNotes, bulkUploadErrorCounter,
        isInternationalMerchant, columnRowInformation, productName, validationErrorMessage, result, instoreSeller);
    if (merchantStatusType.getType() >= MerchantStatusType.BFB.getType()) {
      result = validateBfbBuyableStatus(raw.get(BulkCnCreationHeaderNames.BFB_STATUS_HEADER), bulkProcessNotes,
          bulkUploadErrorCounter, isInternationalMerchant, columnRowInformation, productName, validationErrorMessage,
          result);
      result = validateBfbManagedStatus(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.BFB_MANAGED_HEADER_ID,
              BulkCnCreationHeaderNames.BFB_MANAGED_HEADER_EN), bulkProcessNotes, bulkUploadErrorCounter,
          isInternationalMerchant, columnRowInformation, productName, validationErrorMessage, result);
    }
    if (merchantStatusType.getType() == MerchantStatusType.DELIVERY_AND_CNC.getType()
        || merchantStatusType.getType() == MerchantStatusType.BFB_AND_CNC.getType()) {
      result = validateCncStatus(raw.get(BulkCnCreationHeaderNames.PRODUCT_CNC_STATUS_HEADER_NAME), bulkProcessNotes,
          bulkUploadErrorCounter, isInternationalMerchant, columnRowInformation, productName, validationErrorMessage,
          result, merchantStatusType.getType(), bopisCncRestrictionEnabled,
          bopisCncRestrictionEnabled && getProductType(raw).equals(ProductType.BOPIS.getCode()));
    }
    return Pair.of(validationErrorMessage, result);
  }

  public static boolean validateExcelPickupPoints(Map<String, Object> raw,
      Map<String, PickupPointResponse> pickupPoints, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, String ppNameDelimiter,
      Set<String> accessiblePickupPoints) {
    boolean result = true;
    StringBuilder validationErrorMessage = new StringBuilder();
    String columnRowInformation = String.valueOf(raw.get(BulkCnCreationHeaderNames.ROW_NUMBER));
    String excelPickupPoint = getPickupPointCodeFromPickupPointCodeAndName(ppNameDelimiter, String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.PICKUP_POINT_CODE_ID_HEADER,
            BulkCnCreationHeaderNames.PICKUP_POINT_CODE_EN_HEADER)));
    if (!pickupPoints.containsKey(excelPickupPoint)) {
      String resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_INVALID_EN,
          BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_INVALID);
      bulkUploadErrorCounter.incrementPickupPoint();
      addValidationErrorMessage(bulkUploadErrorCounter.getPickupPoint(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, : Row : {}. Product Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1),
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.PRODUCT_NAME_ID_HEADER_NAME,
              BulkCnCreationHeaderNames.PRODUCT_NAME_EN_HEADER_NAME), BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.PICKUP_POINT_CODE_INVALID);
    }
    if (CollectionUtils.isNotEmpty(accessiblePickupPoints) && !accessiblePickupPoints.contains(excelPickupPoint)) {
      String resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.INACCESSIBLE_PICKUP_POINTS_EN,
          BulkProcessValidationErrorMessages.INACCESSIBLE_PICKUP_POINTS);
      bulkUploadErrorCounter.incrementPickupPoint();
      addValidationErrorMessage(bulkUploadErrorCounter.getPickupPoint(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
      result = false;
      log.error("Validation error bulk process : {}, : Row : {}. Product Name - {}. Error msg - {} : {}.",
          bulkProcessNotes.getBulkProcessCode(), (Integer.parseInt(columnRowInformation) + 1),
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.PRODUCT_NAME_ID_HEADER_NAME,
              BulkCnCreationHeaderNames.PRODUCT_NAME_EN_HEADER_NAME), BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.INACCESSIBLE_PICKUP_POINTS);
    }
    if (!result) {
      bulkProcessNotes.setNotes(validationErrorMessage.toString());
    }
    return result;
  }

  public static boolean validateExcelAttributes(Map<String, Object> raw, Map<String, List<String>> attributesValues,
      CategoryDetailResponse category, BulkProcess bulkProcess, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant,  boolean brandFetchIgnoreCase) throws Exception {
    boolean result = true;
    String resultErrorMessage;
    Integer indexColumnRowInformation = (Integer) raw.get(BulkCnCreationHeaderNames.ROW_NUMBER);
    StringBuilder validationErrorMessage = new StringBuilder();
    Map<String, AttributeResponse> skuValueTrueAttributes = new HashMap<>();
    Map<String, AttributeResponse> allAttributes = new HashMap<>();

    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (!categoryAttribute.isMarkForDelete()) {
        String attributeName = categoryAttribute.getAttribute().getName();
        if (isInternationalMerchant && Objects.nonNull(categoryAttribute.getAttribute().getNameEnglish())) {
          attributeName = categoryAttribute.getAttribute().getNameEnglish();
        }
        allAttributes.put(attributeName, categoryAttribute.getAttribute());
        if (categoryAttribute.getAttribute().isSkuValue()) {
          skuValueTrueAttributes.put(attributeName, categoryAttribute.getAttribute());
        }
      }
    }

    List<AttributeResponse> predefinedAttributes = getPredefinedAttribute(allAttributes.values());
    for (AttributeResponse attributeResponse : predefinedAttributes) {
      String attributeName =
          getAttributeNameBasedOnHeader(attributeResponse.getNameEnglish(), attributeResponse.getName(),
              isInternationalMerchant);
      if (!allAttributes.get(attributeName).isMandatory() && StringUtils.isBlank(String.valueOf(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, attributeResponse.getName(),
              attributeResponse.getNameEnglish())))) {
        continue;
      }

      List<String> attributeValues = Optional.ofNullable(attributesValues.get(attributeName)).orElse(new ArrayList<>());
      if (!BulkCnCreationHeaderNames.BRAND_HEADER_NAME.equals(attributeName)
        && !attributeValues.contains(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, attributeResponse.getName(),
          attributeResponse.getNameEnglish())))) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID_EN,
            BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID);
        bulkUploadErrorCounter.incrementFeature();
        addValidationErrorMessage(bulkUploadErrorCounter.getFeature(), validationErrorMessage,
            errorMessage(attributeName + StringUtils.SPACE, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Bulk process : {}, Row : {} . Error msg - {}. Attribute Name - {}.",
            bulkProcess.getBulkProcessCode(), indexColumnRowInformation,
            BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID, attributeName + ".");
      }
    }

    List<AttributeResponse> specialAttributes = getSpecialAttribute(allAttributes.values());
    for (AttributeResponse attributeResponse : specialAttributes) {
      String attributeName =
          getAttributeNameBasedOnHeader(attributeResponse.getNameEnglish(), attributeResponse.getName(),
              isInternationalMerchant);
      if (StringUtils.isBlank(String.valueOf(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, attributeResponse.getName(),
              attributeResponse.getNameEnglish()))) && attributeResponse.isMandatory()) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK_EN,
            BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK);
        bulkUploadErrorCounter.incrementFeature();
        addValidationErrorMessage(bulkUploadErrorCounter.getFeature(), validationErrorMessage,
            errorMessage(attributeName + StringUtils.SPACE, resultErrorMessage, StringUtils.EMPTY));
        result = false;
        log.error("Validation error bulk process : {}, Row : {} . Error msg - {}. Attribute Name - {}.",
            bulkProcess.getBulkProcessCode(), indexColumnRowInformation + 1,
            BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK, attributeName);
      }
    }

    result = BulkCreationCommonUtil.validateBrand(raw, attributesValues, bulkProcessNotes, bulkUploadErrorCounter,
        isInternationalMerchant, result, validationErrorMessage, allAttributes, brandFetchIgnoreCase);

    if (!result) {
      bulkProcessNotes.setNotes(validationErrorMessage.toString());
    }
    return result;
  }

  public static boolean validateBrand(Map<String, Object> raw, Map<String, List<String>> attributesValues,
      BulkProcessNotes bulkProcessNotes, BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant,
      boolean result, StringBuilder validationErrorMessage, Map<String, AttributeResponse> allAttributes,  boolean brandFetchIgnoreCase) {
    String resultErrorMessage;
    String brandValue = String.valueOf(raw.get(BulkCnCreationHeaderNames.BRAND_HEADER_NAME));
    String cleanBrandValue = brandValue.contains(Constant.IN_REVIEW) ?
        brandValue.substring(0, brandValue.indexOf(Constant.IN_REVIEW_BRAND_SUFFIX)) :
        brandValue;
    AttributeResponse attribute = allAttributes.get(GenericBulkParameters.BRAND);
    String columnRowInformation = String.valueOf(raw.get(BulkCnCreationHeaderNames.ROW_NUMBER));
    List<String> brandValueList = attributesValues.getOrDefault(GenericBulkParameters.BRAND, new ArrayList<>());
    //checking for brand value
    if (brandFetchIgnoreCase) {
      overrideBrandIgnoreCase(raw, brandValueList, cleanBrandValue, false);
      cleanBrandValue = String.valueOf(raw.get(BulkCnCreationHeaderNames.BRAND_HEADER_NAME));
    }
    if ((StringUtils.isBlank(cleanBrandValue))) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.BRAND_CANNOT_BE_BLANK_EN,
          BulkProcessValidationErrorMessages.BRAND_CANNOT_BE_BLANK);
      bulkUploadErrorCounter.incrementFeature();
      addValidationErrorMessage(bulkUploadErrorCounter.getFeature(), validationErrorMessage,
          errorMessage(getAttributeName(isInternationalMerchant, attribute) + StringUtils.SPACE, resultErrorMessage,
              StringUtils.EMPTY));
      log.error("Bulk process: {}, Row {} - {}", bulkProcessNotes.getBulkProcessCode(),
          (Integer.parseInt(columnRowInformation) + 1), BulkProcessValidationErrorMessages.ATTRIBUTE_VALUE_INVALID);
      result = false;
    } else if (!brandValueList.contains(cleanBrandValue)) {
      resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID_EN,
          BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID);
      bulkUploadErrorCounter.incrementFeature();
      addValidationErrorMessage(bulkUploadErrorCounter.getFeature(), validationErrorMessage,
          errorMessage(getAttributeName(isInternationalMerchant, attribute) + StringUtils.SPACE, resultErrorMessage,
              StringUtils.EMPTY));
      log.error("Bulk process: {}, Row {} - {}", bulkProcessNotes.getBulkProcessCode(),
          (Integer.parseInt(columnRowInformation) + 1), BulkProcessValidationErrorMessages.ATTRIBUTE_VALUE_INVALID);
      result = false;
    }
    return result;
  }


  public static Map<String, Integer> getIndexColumnGroups(CategoryDetailResponse category, int incrementValue,
      int decrementValue) {
    boolean isColourFamily = false;
    //counting feature attributes
    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if ((!categoryAttribute.isMarkForDelete()) && (isDescriptiveOrPredefinedAndNotVariantCreation(
          categoryAttribute.getAttribute())) && (Constant.COLOUR_FAMILY
          .equals(categoryAttribute.getAttribute().getName()))) {
        isColourFamily = true;
      }
    }
    int numberOfVariantCreationAttributes = 0;
    //counting variant attributes
    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if ((!categoryAttribute.isMarkForDelete()) && isDefiningOrVariantCreation(categoryAttribute.getAttribute())) {
        numberOfVariantCreationAttributes++;
      }
    }
    if (isColourFamily) {
      numberOfVariantCreationAttributes++;
    }
    //checking Garansi atributes

    Map<String, Integer> indexColumnGroups = new HashMap<>();
    setColumnsIndex(indexColumnGroups, numberOfVariantCreationAttributes, incrementValue, decrementValue);
    return indexColumnGroups;
  }

  private static void setColumnsIndex(Map<String, Integer> indexColumnGroups, int variantEndIndex, int incrementValue,
      int decrementValue) {
    indexColumnGroups.put(BulkUploadFileHeaders.PRODUCT_INFO, 0);
    indexColumnGroups.put(BulkUploadFileHeaders.VARIANT, BulkUploadFileHeaders.VARIANT_START_INDEX + decrementValue);
    //Adding 1 for Parent column
    indexColumnGroups.put(BulkUploadFileHeaders.IMAGES,
        BulkUploadFileHeaders.VARIANT_START_INDEX + decrementValue + BulkUploadFileHeaders.PARENT_COLUMN_COUNT
            + variantEndIndex);
    // IMAGES_END_INDEX = 9, because 8 images + 1 URL
    indexColumnGroups.put(BulkUploadFileHeaders.SHIPMENT_INFO,
        BulkUploadFileHeaders.VARIANT_START_INDEX + decrementValue + BulkUploadFileHeaders.PARENT_COLUMN_COUNT
            + variantEndIndex + BulkUploadFileHeaders.IMAGES_END_INDEX);
    indexColumnGroups.put(BulkUploadFileHeaders.PRICE_AND_STOCK,
        BulkUploadFileHeaders.VARIANT_START_INDEX + decrementValue + BulkUploadFileHeaders.PARENT_COLUMN_COUNT
            + variantEndIndex + BulkUploadFileHeaders.IMAGES_END_INDEX + BulkUploadFileHeaders.SHIPMENT_END_INDEX);
    indexColumnGroups.put(BulkUploadFileHeaders.SKU_VISIBILITY,
        BulkUploadFileHeaders.VARIANT_START_INDEX + decrementValue + BulkUploadFileHeaders.PARENT_COLUMN_COUNT
            + variantEndIndex + BulkUploadFileHeaders.IMAGES_END_INDEX + BulkUploadFileHeaders.SHIPMENT_END_INDEX
            + BulkUploadFileHeaders.PRICE_STOCK_END_INDEX);
    //Recommended attributes
    indexColumnGroups.put(BulkUploadFileHeaders.ATTRIBUTES,
        BulkUploadFileHeaders.VARIANT_START_INDEX + decrementValue + BulkUploadFileHeaders.PARENT_COLUMN_COUNT
            + variantEndIndex + BulkUploadFileHeaders.IMAGES_END_INDEX + BulkUploadFileHeaders.SHIPMENT_END_INDEX
            + BulkUploadFileHeaders.PRICE_STOCK_END_INDEX + incrementValue);
  }

  public static List<ProductCreationRequest> convertGroupRowsToProductCollectionRequestsForCategoryUpload(
      List<List<Map<String, Object>>> groupRaws, Map<String, AttributeResponse> attributes,
      CategoryDetailResponse category, BulkProcess bulkProcess, ProfileResponse businessPartner,
      AtomicInteger buyableFlagUpdatedCount, MerchantStatusType merchantStatusType, String ppNameDelimiter,
      boolean cncForWarehouseFeatureSwitch, boolean instoreSeller) {
    boolean isInternationalMerchant = bulkProcess.getInternationalMerchant();
    boolean isColourFamilyPresent = attributes.containsKey(Constant.COLOUR_FAMILY);
    List<ProductCreationRequest> productCreationRequests = new ArrayList<>();
    for (List<Map<String, Object>> groupRaw : groupRaws) {
      Map<String, Object> parentRaw = groupRaw.get(0);
      // Initialize product request
      ProductCreationRequest productCreationRequest =
          prepareProductRequest(bulkProcess, parentRaw, businessPartner, category, instoreSeller, groupRaw);

      if (isColourFamilyPresent) {
        productCreationRequest.setProductItems(new ArrayList<>());
      }

      // Initialize product attributes with attribute type DESCRIPTIVE_ATTRIBUTE and PREDEFINED_ATTRIBUTE
      initializeNonVariantCreationAttributes(attributes, bulkProcess, isInternationalMerchant, parentRaw, productCreationRequest);

      // Set value product brand
      setBrandAttributeValue(attributes, bulkProcess, isInternationalMerchant, parentRaw, productCreationRequest);

      // Initialize product attributes with attribute type DEFINING_ATTRIBUTE and variant creation
      initializeVariantCreationAttributes(attributes, bulkProcess, isInternationalMerchant, groupRaw, productCreationRequest);

      //Initialize items
      for (Map<String, Object> currentRow : groupRaw) {
        initializeItemDetails(attributes, productCreationRequest, currentRow, parentRaw, buyableFlagUpdatedCount,
            merchantStatusType, ppNameDelimiter, cncForWarehouseFeatureSwitch, instoreSeller);
      }

      productCreationRequest.setBundleProduct(CollectionUtils.isNotEmpty(
          productCreationRequest.getProductItemRequests().stream().findFirst().orElse(new ProductItemCreationRequest())
              .getBundleRecipe()));
      productCreationRequest.setPrioritySeller(getPriority(bulkProcess));
      // Initialize product categories
      BulkCreationCommonUtil.initializeProductCategory(bulkProcess.getStoreId(), productCreationRequest, category);
      productCreationRequests.add(productCreationRequest);
    }
    return productCreationRequests;
  }

  public static int getPriority(BulkProcess bulkProcess) {
    if (BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue().equals(bulkProcess.getBulkProcessType())) {
      return 1;
    } else if (BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue().equals(bulkProcess.getBulkProcessType())) {
      return 2;
    }
    return 0;
  }

  public static boolean isDescriptiveVariant(AttributeResponse attribute) {
    return AttributeType.DESCRIPTIVE_ATTRIBUTE.name().equals(attribute.getAttributeType()) && attribute
        .isVariantCreation();
  }

  public static void generateProductBusinessPartnerRequestForCategoryUpload(List<Map<String, Object>> groupRaw,
      CategoryDetailResponse category, ProductCreationRequest productCreationRequest, BulkProcess bulkProcess,
      boolean isInternationalMerchant) {
    Map<String, Object> parentRaw = groupRaw.get(0);
    List<ProductBusinessPartnerAttributeRequest> productBusinessPartnerAttributeRequests = new ArrayList<>();
    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      // Add SKU value = true attributes
      if (!categoryAttribute.isMarkForDelete() && categoryAttribute.getAttribute().isSkuValue()) {
        String categoryName;
        if (isInternationalMerchant) {
          categoryName = categoryAttribute.getAttribute().getNameEnglish();
        } else {
          categoryName = categoryAttribute.getAttribute().getName();
        }
        ProductBusinessPartnerAttributeRequest productBusinessPartnerAttributeRequest =
            new ProductBusinessPartnerAttributeRequest();
        productBusinessPartnerAttributeRequest.setStoreId(bulkProcess.getStoreId());
        productBusinessPartnerAttributeRequest.setCreatedDate(Calendar.getInstance().getTime());
        productBusinessPartnerAttributeRequest.setCreatedBy(bulkProcess.getCreatedBy());
        productBusinessPartnerAttributeRequest.setUpdatedDate(new Date());
        productBusinessPartnerAttributeRequest.setUpdatedBy(bulkProcess.getUpdatedBy());
        productBusinessPartnerAttributeRequest.setAttributeId(categoryAttribute.getAttribute().getId());
        String specialAttributeValue = String.valueOf(parentRaw.get(categoryName));
        if (StringUtils.isBlank(specialAttributeValue)) {
          productBusinessPartnerAttributeRequest.setValue(Constant.HYPHEN);
        } else {
          productBusinessPartnerAttributeRequest.setValue(specialAttributeValue);
        }
        productBusinessPartnerAttributeRequests.add(productBusinessPartnerAttributeRequest);
      }
    }
    productCreationRequest.setProductBusinessPartnerAttributes(productBusinessPartnerAttributeRequests);
  }

  public static void addValidationErrorMessage(int errorCount, StringBuilder validationErrorMessage,
      String errorMessage) {
    if (errorCount <= Constant.ERROR_COUNT) {
      validationErrorMessage.append(errorMessage).append(Constant.PERIOD);
    }
  }

  /**
   * get sla notification message
   *
   * @return returns sla notification message string
   */
  public static String getSLANotification(boolean isInternationalFlag, int internalActivationPeriod, String status) {
    String language = isInternationalFlag ? Constant.ENGLISH : Constant.BAHASA;
    if (Constant.POST_LIVE.equals(status)) {
      return MessageUtil.getMessage(Constant.POST_LIVE_PRODUCT_UPLOAD_SLA_NOTIFICATION, language);
    } else {
      int internalActivationIntervalInDays = (int) Math.ceil((internalActivationPeriod / 24.0));
      return MessageUtil.getMessage(Constant.PRODUCT_UPLOAD_SLA_NOTIFICATION, language)
          .replace(Constant.NOTIFICATION_PLACEHOLDER, String.valueOf(internalActivationIntervalInDays));
    }
  }

  public static void setDescriptionForCn(BulkProcess bulkProcess, String status, String slaNotificationMsg) {
    String[] excelFileName = bulkProcess.getDescription().split("\\. ");
    String conjunction;
    String appendingText;
    if (bulkProcess.getInternationalMerchant()) {
      conjunction = Constant.OUT_OF;
      appendingText = Constant.HAS_BEEN_UPLOADED;
    } else {
      conjunction = Constant.DARI;
      appendingText = Constant.TELAH_DI_UPLOAD;
    }
    String successMessageProductUpload;
    if (Constant.POST_LIVE.equals(status)) {
      successMessageProductUpload =
          new StringBuilder().append(bulkProcess.getSuccessCount()).append(Constant.SPACE).append(conjunction)
              .append(Constant.SPACE).append(bulkProcess.getTotalCount()).append(Constant.SPACE)
              .append(slaNotificationMsg).toString();
      bulkProcess.setDescription(new StringBuilder(excelFileName[0]).append(Constant.SPACE).append(appendingText)
          .append(successMessageProductUpload).toString());
    } else {
      if (bulkProcess.getSuccessCount() != 0) {
        successMessageProductUpload =
            new StringBuilder(Constant.DESCRIPTION_SUCCESS).append(bulkProcess.getSuccessCount()).append(Constant.SPACE)
                .append(Constant.OUT_OF).append(Constant.SPACE).append(bulkProcess.getTotalCount())
                .append(Constant.SPACE).append(Constant.DESCRIPTION_PRODUCTS).append(slaNotificationMsg).toString();
      } else {
        successMessageProductUpload = StringUtils.EMPTY;
      }
      bulkProcess.setDescription(
          new StringBuilder(excelFileName[0]).append(Constant.SPACE).append(Constant.DESCRIPTION_BULK_UPLOAD_SUFFIX)
              .append(Constant.SPACE).append(successMessageProductUpload).toString());
    }
  }

  public static ProductCreationRequest toProductCreationRequestForCopyStore(Map<String, AttributeResponse> attributes,
      CategoryDetailResponse category, ProfileResponse businessPartner, ProductDetailResponse productDetailResponse,
      ProductAndItemInfoResponseV2 firstProductAndItemInfoResponseV2, List<ProductAndItemInfoResponseV2> productAndItemInfoResponseV2List,
      Map<String, LinkedHashMap<String, Object>> requestItems, boolean bpBopisRestrictionEnabled, boolean valueTypeAdditionForDefiningAttributes)
      throws Exception {
    boolean isInternationalMerchant = businessPartner.getCompany().isInternationalFlag();

    // Initialize product request
    ProductCreationRequest productCreationRequest =
        prepareProductRequest(productDetailResponse, businessPartner, category);

    Map<String, ProductAttributeResponse> existingAttributes = new HashMap<>();
    for (ProductAttributeResponse productAttributeResponse : productDetailResponse.getProductAttributeResponses()) {
      AttributeResponse attributeResponse = productAttributeResponse.getAttribute();
      existingAttributes.put(attributeResponse.getAttributeCode(), productAttributeResponse);
    }

    // Initialize product attributes with attribute type DESCRIPTIVE_ATTRIBUTE and PREDEFINED_ATTRIBUTE
    initializeNonVariantCreationAttributes(attributes, isInternationalMerchant, productCreationRequest,
        existingAttributes);

    // Initialize product attributes with attribute type DEFINING_ATTRIBUTE and variant creation
    initializeVariantCreationAttributesAndItems(productDetailResponse, existingAttributes, productCreationRequest,
        firstProductAndItemInfoResponseV2, productAndItemInfoResponseV2List, requestItems,
        businessPartner, bpBopisRestrictionEnabled, valueTypeAdditionForDefiningAttributes);

    // Initialize product categories
    BulkCreationCommonUtil.initializeProductCategory(Constant.STORE_ID, productCreationRequest, category);
    productCreationRequest.setProductCreationType(ProductCreationType.STORE_COPY_FLOW_2);
    return productCreationRequest;
  }

  public static ProductCreationRequest prepareProductRequest(ProductDetailResponse productDetailResponse,
      ProfileResponse businessPartner, CategoryDetailResponse categoryDetailResponse) {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    productCreationRequest.setStoreId(productDetailResponse.getStoreId());
    productCreationRequest.setCreatedDate(Calendar.getInstance().getTime());
    productCreationRequest.setName(productDetailResponse.getName());
    productCreationRequest.setUrl(productDetailResponse.getUrl());
    productCreationRequest.setLength(Double.parseDouble(String.valueOf(productDetailResponse.getLength())));
    productCreationRequest.setWidth(Double.parseDouble(String.valueOf(productDetailResponse.getWidth())));
    productCreationRequest.setHeight(Double.parseDouble(String.valueOf(productDetailResponse.getHeight())));
    productCreationRequest.setWeight(Double.parseDouble(String.valueOf(productDetailResponse.getWeight())));
    productCreationRequest.setShippingWeight(Double.parseDouble(String.valueOf(productDetailResponse.getShippingWeight())));
    productCreationRequest.setDescription(productDetailResponse.getDescription());
    productCreationRequest.setLongDescription(productDetailResponse.getLongDescription());
    productCreationRequest.setUniqueSellingPoint(productDetailResponse.getUniqueSellingPoint());
    productCreationRequest.setSpecificationDetail(productDetailResponse.getSpecificationDetail());
    productCreationRequest.setUom(productDetailResponse.getUom());
    productCreationRequest.setProductAttributes(new ArrayList<>());
    productCreationRequest.setProductCategories(new ArrayList<>());
    productCreationRequest.setBusinessPartnerCode(businessPartner.getBusinessPartnerCode());
    productCreationRequest.setBusinessPartnerName(businessPartner.getCompany().getBusinessPartnerName());
    if (businessPartner.getCompany().isInternationalFlag() && StringUtils.isNotEmpty(
        categoryDetailResponse.getNameEnglish())) {
      productCreationRequest.setCategoryName(categoryDetailResponse.getNameEnglish());
    } else {
      productCreationRequest.setCategoryName(categoryDetailResponse.getName());
    }
    return productCreationRequest;
  }

  public static void initializeNonVariantCreationAttributes(Map<String, AttributeResponse> categoryAttributes,
      boolean isInternationalMerchant, ProductCreationRequest productCreationRequest,
      Map<String, ProductAttributeResponse> existingAttributes) throws Exception {
    for (Map.Entry<String, AttributeResponse> entry : categoryAttributes.entrySet()) {
      AttributeResponse attribute = entry.getValue();
      if (isDefiningOrVariantCreation(attribute) || Constant.COLOUR_FAMILY.equalsIgnoreCase(attribute.getName())) {
        continue;
      }

      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(attribute, attributeRequest, "allowedAttributeValues",
          "predefinedAllowedAttributeValues");

      if (Objects.nonNull(existingAttributes.get(attribute.getAttributeCode()))) {
        ProductAttributeResponse productAttributeResponse = existingAttributes.get(attribute.getAttributeCode());
        ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
        BeanUtils.copyProperties(productAttributeResponse, productAttributeRequest, "attribute",
            "productAttributeValues", "id");
        productAttributeRequest.setAttribute(attributeRequest);
        List<ProductAttributeValueRequest> values = new ArrayList<>();
        for (ProductAttributeValueResponse productAttributeValueResponse : productAttributeResponse.getProductAttributeValues()) {
          boolean valuePresent = false;
          ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
          if (Objects.nonNull(productAttributeValueResponse.getAllowedAttributeValue())) {
            AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
            BeanUtils.copyProperties(productAttributeValueResponse.getAllowedAttributeValue(),
                allowedAttributeValueRequest);
            productAttributeValueRequest.setAllowedAttributeValue(allowedAttributeValueRequest);
            valuePresent = true;
          }
          if (Objects.nonNull(productAttributeValueResponse.getPredefinedAllowedAttributeValue())) {
            PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
                new PredefinedAllowedAttributeValueRequest();
            BeanUtils.copyProperties(productAttributeValueResponse.getPredefinedAllowedAttributeValue(),
                predefinedAllowedAttributeValueRequest);
            productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
            valuePresent = true;
          }
          if (StringUtils.isNotBlank(productAttributeValueResponse.getDescriptiveAttributeValue())) {
            productAttributeValueRequest.setDescriptiveAttributeValue(
                productAttributeValueResponse.getDescriptiveAttributeValue());
            valuePresent = true;
          }
          if (attribute.isMandatory() && !valuePresent) {
            throw new ApplicationException(ErrorCategory.VALIDATION,
                "Mandatory attribute cannot be empty : " + attribute.getAttributeCode());
          }
          if (!valuePresent) {
            initializeAttributes(attribute, productAttributeValueRequest, null);
          }
          productAttributeValueRequest.setDescriptiveAttributeValueType(
              productAttributeValueResponse.getDescriptiveAttributeValueType());
          values.add(productAttributeValueRequest);
        }
        productAttributeRequest.setProductAttributeValues(values);
        productCreationRequest.getProductAttributes().add(productAttributeRequest);

        if (Constant.BRAND.equalsIgnoreCase(attribute.getName())) {
          String value = values.get(0).getPredefinedAllowedAttributeValue().getValue();
          Optional<PredefinedAllowedAttributeValueResponse> optionalBrandValueResponse =
              attribute.getPredefinedAllowedAttributeValues().stream()
                  .filter(predefinedValue -> predefinedValue.getValue().equals(value)).findFirst();
          productCreationRequest.setBrand(value);
          productCreationRequest.setBrandCode(
              values.get(0).getPredefinedAllowedAttributeValue().getPredefinedAllowedAttributeCode());
          productCreationRequest.setBrandApprovalStatus(
              optionalBrandValueResponse.map(PredefinedAllowedAttributeValueResponse::getBrandApprovalStatus)
                  .orElse(null));
        }
      } else if (!attribute.isMandatory()) {
        String attributeValue = Constant.HYPHEN;
        ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
        productAttributeValueRequest.setStoreId(Constant.STORE_ID);
        initializeAttributes(attribute, productAttributeValueRequest, attributeValue);
        ProductAttributeRequest productAttributeRequest =
            setValuesForProductAttributeRequest(Constant.STORE_ID, attribute, isInternationalMerchant);
        productAttributeRequest.setProductAttributeValues(new ArrayList<>());
        productAttributeRequest.getProductAttributeValues().add(productAttributeValueRequest);
        productCreationRequest.getProductAttributes().add(productAttributeRequest);
      } else {
        throw new ApplicationException(ErrorCategory.VALIDATION,
            "Mandatory attribute cannot be empty  : " + attribute.getAttributeCode());
      }
    }
  }

  public static Map<String, String> getAttributeIdAndValuesMap(List<AttributeResponse> attributeResponses,
      Map<String, ProductAttributeResponse> existingAttributes) {
    Map<String, String> attributeCodeAndValuesMap = new HashMap<>();
    for (AttributeResponse attribute : attributeResponses) {
      if (isDefiningOrVariantCreation(attribute) || Constant.COLOUR_FAMILY.equalsIgnoreCase(attribute.getName())) {
        continue;
      }
      ProductAttributeResponse productAttributeResponse = existingAttributes.get(attribute.getAttributeCode());
      if (Objects.nonNull(productAttributeResponse) && Constant.BRAND.equalsIgnoreCase(attribute.getName())) {
        attributeCodeAndValuesMap.put(attribute.getId(),
            productAttributeResponse.getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue()
                .getValue());
      } else if (Objects.isNull(productAttributeResponse) && !attribute.isMandatory()) {
        attributeCodeAndValuesMap.put(attribute.getId(), Constants.HYPHEN);
      }
    }
    return attributeCodeAndValuesMap;
  }

  public static void generateProductBusinessPartnerRequest(List<ProductSpecialAttributeDTO> specialAttributeDTOS,
      CategoryDetailResponse category, ProductCreationRequest productCreationRequest) {
    Map<String, String> specialAttributesMap = specialAttributeDTOS.stream().collect(
        Collectors.toMap(ProductSpecialAttributeDTO::getAttributeCode, ProductSpecialAttributeDTO::getAttributeValue));
    List<ProductBusinessPartnerAttributeRequest> productBusinessPartnerAttributeRequests = new ArrayList<>();
    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      // Add SKU value = true attributes
      if (!categoryAttribute.isMarkForDelete() && categoryAttribute.getAttribute().isSkuValue()) {
        ProductBusinessPartnerAttributeRequest productBusinessPartnerAttributeRequest =
            new ProductBusinessPartnerAttributeRequest();
        productBusinessPartnerAttributeRequest.setStoreId(Constant.STORE_ID);
        productBusinessPartnerAttributeRequest.setCreatedDate(Calendar.getInstance().getTime());
        productBusinessPartnerAttributeRequest.setCreatedBy(Constant.SYSTEM);
        productBusinessPartnerAttributeRequest.setUpdatedDate(new Date());
        productBusinessPartnerAttributeRequest.setUpdatedBy(Constant.SYSTEM);
        productBusinessPartnerAttributeRequest.setAttributeId(categoryAttribute.getAttribute().getId());
        String specialAttributeValue = specialAttributesMap.get(categoryAttribute.getAttribute().getAttributeCode());
        if (StringUtils.isBlank(specialAttributeValue)) {
          productBusinessPartnerAttributeRequest.setValue(Constant.HYPHEN);
        } else {
          productBusinessPartnerAttributeRequest.setValue(specialAttributeValue);
        }
        productBusinessPartnerAttributeRequests.add(productBusinessPartnerAttributeRequest);
      }
    }
    productCreationRequest.setProductBusinessPartnerAttributes(productBusinessPartnerAttributeRequests);
  }

  public static void initializeVariantCreationAttributesAndItems(ProductDetailResponse productDetailResponse,
      Map<String, ProductAttributeResponse> existingAttributes, ProductCreationRequest productCreationRequest,
      ProductAndItemInfoResponseV2 firstProductAndItemInfoResponseV2,
      List<ProductAndItemInfoResponseV2> productAndItemInfoResponseV2List,
      Map<String, LinkedHashMap<String, Object>> requestItems, ProfileResponse profileResponse,
      boolean bpBopisRestrictionEnabled, boolean valueTypeAdditionForDefiningAttributes)
      throws Exception {
    Map<String, ItemInfoResponseV2> existingItemsFromXProduct = productAndItemInfoResponseV2List.stream().collect(
        Collectors.toMap(productAndItemInfoResponseV2 -> productAndItemInfoResponseV2.getItem().getItemSku(),
            ProductAndItemInfoResponseV2::getItem, (value1, value2) -> value2));
    Map<String, ProductItemResponse> existingItemsFromPCB = productDetailResponse.getProductItemResponses().stream()
        .collect(Collectors.toMap(ProductItemResponse::getSkuCode, Function.identity()));
    List<ProductAttributeDTO> existingDefiningAttributes = firstProductAndItemInfoResponseV2.getProduct().getDefiningAttributes();
    Map<String, Set<String>> variantAttributesMap = new HashMap<>();
    List<ProductItemCreationRequest> itemCreationRequests = new ArrayList<>();
    for (ProductAttributeDTO productAttributeDTO : existingDefiningAttributes) {
      String existingItemSku = productAttributeDTO.getItemSku();
      if (Objects.isNull(requestItems.get(existingItemSku))) {
        log.warn("Item Sku not found in request rows : {}", existingItemSku);
        continue;
      }
      LinkedHashMap<String, Object> requestItem = requestItems.get(existingItemSku);
      ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
      ItemInfoResponseV2 itemInfoResponseV2 = existingItemsFromXProduct.get(existingItemSku);
      ProductItemResponse productItemResponse = existingItemsFromPCB.get(itemInfoResponseV2.getItemCode());
      productItemCreationRequest.setSourceItemCode(itemInfoResponseV2.getItemCode());
      TreeMap<String, String> attributeMap = new TreeMap<>();
      TreeMap<String, String> attributesValueTypeMap = new TreeMap<>();
      Map<String, Map<String, String>> attributeCodeAndValueAndValueTypeMap = new HashMap<>();
      if (valueTypeAdditionForDefiningAttributes) {
        attributeCodeAndValueAndValueTypeMap =
            getAttributeCodeAndValueTypeMap(productDetailResponse);
      }
      List<ProductAttributeDetailDTO> variantAttributes = productAttributeDTO.getProductAttributeDetails();
      if (CollectionUtils.isNotEmpty(variantAttributes)) {
        for (ProductAttributeDetailDTO productAttributeDetailDTO : variantAttributes) {
          String attributeCode = productAttributeDetailDTO.getAttributeCode();
          String value = productAttributeDetailDTO.getAttributeValue();
          attributeMap.put(attributeCode, value);
          setAttributeValueTypeMap(valueTypeAdditionForDefiningAttributes, attributeCodeAndValueAndValueTypeMap,
              attributeCode, value, attributesValueTypeMap);
          if (variantAttributesMap.containsKey(attributeCode)) {
            Set<String> values = variantAttributesMap.get(attributeCode);
            values.add(value);
            variantAttributesMap.put(attributeCode, values);
          } else {
            Set<String> values = new HashSet<>();
            values.add(value);
            variantAttributesMap.put(attributeCode, values);
          }
        }
      }
      productItemCreationRequest.setAttributesMap(attributeMap);
      productItemCreationRequest.setAttributesValueTypeMap(attributesValueTypeMap);
      productItemCreationRequest.setStock((Integer) requestItem.get(StoreCopyConstants.FIELD_STOCK));
      productItemCreationRequest.setMinimumStock((Integer) requestItem.get(StoreCopyConstants.FIELD_MINIMUM_STOCK));
      productItemCreationRequest.setPrice((Double) requestItem.get(StoreCopyConstants.FIELD_LIST_PRICE));
      productItemCreationRequest.setSalePrice((Double) requestItem.get(StoreCopyConstants.FIELD_OFFER_PRICE));
      productItemCreationRequest.setPickupPointId((String) requestItem.get(StoreCopyConstants.FIELD_PICKUP_POINT_CODE));
      productItemCreationRequest.setMerchantSku((String) requestItem.get(StoreCopyConstants.FIELD_SELLER_SKU));
      Integer status = (Integer) requestItem.get(StoreCopyConstants.FIELD_STATUS);
      if (status == 1) {
        productItemCreationRequest.setBuyable(true);
        productItemCreationRequest.setDisplay(true);
      } else if (status == 2) {
        productItemCreationRequest.setBuyable(false);
        productItemCreationRequest.setDisplay(true);
      } else if (status == 3) {
        productItemCreationRequest.setBuyable(true);
        productItemCreationRequest.setDisplay(false);
      }
      if (Objects.nonNull(productItemCreationRequest.getSalePrice()) && Objects.nonNull(
          productItemCreationRequest.getPrice()) && (productItemCreationRequest.getSalePrice()
          > productItemCreationRequest.getPrice())) {
        throw new ApplicationException(ErrorCategory.VALIDATION, "Sale price cannot be greater than normal price");
      }
      Integer productType =
          (Integer) BulkUploadOption.valueFrom(String.valueOf(requestItem.get(StoreCopyConstants.FIELD_SHIPPING_TYPE)))
              .getValue();
      if (!ConverterUtil.checkIfEligibleForShippingType(productType, profileResponse,
        bpBopisRestrictionEnabled)) {
        throw new ApplicationException(ErrorCategory.VALIDATION, SHIPPING_TYPE_INELIGIBLE);
      }
      productItemCreationRequest.setProductType(productType);
      String existingProductName = (String) requestItem.get(StoreCopyConstants.FIELD_PRODUCT_NAME);
      String copyProductName = (String) requestItem.get(StoreCopyConstants.FIELD_COPY_PRODUCT_NAME);
      if (StringUtils.isNotBlank(copyProductName) && !existingProductName.equalsIgnoreCase(copyProductName)) {
        productCreationRequest.setName(copyProductName);
        productItemCreationRequest.setContentChanged(true);
      }
      productItemCreationRequest.setUpcCode(productItemResponse.getUpcCode());
      List<Image> images = new ArrayList<>();
      for(Image image : productItemResponse.getImages()) {
        Image newImage = new Image();
        BeanUtils.copyProperties(image, newImage, "id");
        images.add(newImage);
      }
      productItemCreationRequest.setPickupPoints(Collections.singletonList(
          PickupPointCreateRequest.builder().pickupPointId(requestItem.get(StoreCopyConstants.FIELD_PICKUP_POINT_CODE).toString())
              .stock((Integer) requestItem.get(StoreCopyConstants.FIELD_STOCK))
              .minimumStock((Integer) requestItem.get(StoreCopyConstants.FIELD_MINIMUM_STOCK))
              .price((Double) requestItem.get(StoreCopyConstants.FIELD_LIST_PRICE))
              .salePrice((Double) requestItem.get(StoreCopyConstants.FIELD_OFFER_PRICE)).build()));
      productItemCreationRequest.setImages(images);
      productItemCreationRequest.setItemGeneratedName(productItemResponse.getGeneratedItemName());
      productItemCreationRequest.setProductItemAttributeValueRequests(new ArrayList<>());
      if (CollectionUtils.isNotEmpty(productItemResponse.getProductItemAttributeValueResponses())) {
        for (ProductItemAttributeValueResponse productItemAttributeValueResponse : productItemResponse.getProductItemAttributeValueResponses()) {
          if (Constant.BRAND.equalsIgnoreCase(productItemAttributeValueResponse.getAttributeResponse().getName())) {
            continue;
          }
          ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
          BeanUtils.copyProperties(productItemAttributeValueResponse, productItemAttributeValueRequest, "id");
          AttributeRequest attributeRequest = new AttributeRequest();
          BeanUtils.copyProperties(productItemAttributeValueResponse.getAttributeResponse(), attributeRequest);
          productItemAttributeValueRequest.setAttribute(attributeRequest);
          productItemCreationRequest.getProductItemAttributeValueRequests().add(productItemAttributeValueRequest);
        }
      }
      itemCreationRequests.add(productItemCreationRequest);
    }
    productCreationRequest.setProductItemRequests(itemCreationRequests);
    if (MapUtils.isNotEmpty(variantAttributesMap)) {
      for (Map.Entry<String, Set<String>> entry : variantAttributesMap.entrySet()) {
        String attributeCode = entry.getKey();
        List<String> values = new ArrayList<>(entry.getValue());
        ProductAttributeResponse productAttributeResponse = existingAttributes.get(attributeCode);
        AttributeResponse attributeResponse = productAttributeResponse.getAttribute();
        ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
        AttributeRequest attributeRequest = new AttributeRequest();
        BeanUtils.copyProperties(attributeResponse, attributeRequest);
        attributeRequest.setAttributeType(AttributeType.valueOf(attributeResponse.getAttributeType()));
        productAttributeRequest.setAttribute(attributeRequest);
        productAttributeRequest.setProductAttributeName(attributeRequest.getName());
        productAttributeRequest.setSequence(0);
        productAttributeRequest.setProductAttributeValues(new ArrayList<>());
        for (String value : values) {
          ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
          productAttributeValueRequest.setDescriptiveAttributeValue(value);
          if (AttributeType.DESCRIPTIVE_ATTRIBUTE == attributeRequest.getAttributeType()) {
            productAttributeValueRequest.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
          } else {
            productAttributeValueRequest.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
            for (ProductAttributeValueResponse productAttributeValueResponse : productAttributeResponse.getProductAttributeValues()) {
              AllowedAttributeValueResponse allowedAttributeValueResponse =
                  productAttributeValueResponse.getAllowedAttributeValue();
              if (value.equals(allowedAttributeValueResponse.getValue())) {
                AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
                BeanUtils.copyProperties(allowedAttributeValueResponse, allowedAttributeValueRequest);
                productAttributeValueRequest.setAllowedAttributeValue(allowedAttributeValueRequest);
                break;
              }
            }
          }
          productAttributeRequest.getProductAttributeValues().add(productAttributeValueRequest);
        }
        productCreationRequest.getProductAttributes().add(productAttributeRequest);
      }
    }
  }

  public static void setAttributeValueTypeMap(boolean valueTypeAdditionForDefiningAttributes,
      Map<String, Map<String, String>> attributeCodeAndValueAndValueTypeMap, String attributeCode,
      String value, TreeMap<String, String> attributesValueTypeMap) {
    if (valueTypeAdditionForDefiningAttributes) {
      String valueType =
          getValueTypeByAttributeCodeAndValue(attributeCodeAndValueAndValueTypeMap, attributeCode,
              value);
      if (StringUtils.isNotBlank(valueType)) {
        attributesValueTypeMap.put(attributeCode, valueType);
      }
    }
  }

  public static Map<String, Map<String, String>> getAttributeCodeAndValueTypeMap(
      ProductDetailResponse productDetailResponse) {
    Map<String, Map<String, String>> attributeCodeAndValueAndValueTypeMap = new HashMap<>();
    for (ProductAttributeResponse productAttributeResponse : Optional.ofNullable(
        productDetailResponse.getProductAttributeResponses()).orElse(new ArrayList<>())) {
      if (Objects.nonNull(productAttributeResponse.getAttribute()) && CollectionUtils.isNotEmpty(
          productAttributeResponse.getProductAttributeValues())
          && AttributeType.DEFINING_ATTRIBUTE.name()
          .equals(productAttributeResponse.getAttribute().getAttributeType())) {
        for (ProductAttributeValueResponse productAttributeValueResponse :
            productAttributeResponse.getProductAttributeValues()) {
          Map<String, String> valueAndValueTypeMap =
              attributeCodeAndValueAndValueTypeMap.getOrDefault(
                  productAttributeResponse.getAttribute().getAttributeCode(), new HashMap<>());
          if (Objects.nonNull(productAttributeValueResponse.getAllowedAttributeValue())) {
            valueAndValueTypeMap.put(
                productAttributeValueResponse.getAllowedAttributeValue().getValue(),
                productAttributeValueResponse.getAllowedAttributeValue().getValueType());
            attributeCodeAndValueAndValueTypeMap.put(
                productAttributeResponse.getAttribute().getAttributeCode(), valueAndValueTypeMap);
          }
        }
      }
    }
    return attributeCodeAndValueAndValueTypeMap;
  }

  public static String getValueTypeByAttributeCodeAndValue(
      Map<String, Map<String, String>> attributeCodeAndValueAndValueTypeMap, String attributeCode,
      String attributeValue) {
    return attributeCodeAndValueAndValueTypeMap.getOrDefault(attributeCode, new HashMap<>())
        .get(attributeValue);
  }

  public static Map<String, String> prepareProtectedBrandNameCodeMap(
    List<ProtectedBrandResponse> protectedBrandList) {
      return protectedBrandList.stream().collect(Collectors
        .toMap(ProtectedBrandResponse::getBrandName, ProtectedBrandResponse::getBrandCode));
  }

  public static List<com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest> toItemSkuAndPPCodeRequest(
    List<Map<String, String>> cleanDatas) {
    List<com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest>
      itemPickupPointRequestList = new ArrayList<>();
    for (Map<String, String> cleanDataMap : cleanDatas) {
      com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest itemPickupPointRequest =
        new com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest();
      itemPickupPointRequest.setItemSku(cleanDataMap.get(BulkParameters.BLIBLI_SKU));
      itemPickupPointRequest.setPickupPointCode(cleanDataMap.get(BulkParameters.PICKUP_POINT_CODE));
      itemPickupPointRequestList.add(itemPickupPointRequest);
    }
    return itemPickupPointRequestList;
  }

  public static List<ItemDetailsDto> toItemListDtoList(List<Map<String, String>> cleanDatas) {
    List<ItemDetailsDto> itemDetailsDtoList = new ArrayList<>();
    cleanDatas.forEach(cleanDataMap -> itemDetailsDtoList.add(
      ItemDetailsDto.builder().itemSku(cleanDataMap.get(BulkParameters.BLIBLI_SKU))
        .pickUpPointCode(cleanDataMap.get(BulkParameters.PICKUP_POINT_CODE)).build()));
    return itemDetailsDtoList;
  }

  public static ProductLevel3SummaryResponse toProductLevel3SummaryResponse(
    ItemSummaryListResponse itemSummaryListResponse){
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    ProductLevel3PriceResponse productLevel3PriceResponse = new ProductLevel3PriceResponse();
    productLevel3PriceResponse.setSalePrice(itemSummaryListResponse.getPrice().iterator().next().getOfferPrice());
    productLevel3SummaryResponse.setBrand(itemSummaryListResponse.getBrandName());
    productLevel3SummaryResponse.setCategoryCode(itemSummaryListResponse.getMasterCategoryCode());
    productLevel3SummaryResponse.setMerchantCode(itemSummaryListResponse.getMerchantCode());
    productLevel3SummaryResponse.setPrices(Collections.singletonList(productLevel3PriceResponse));
    return productLevel3SummaryResponse;
  }

  public static List<Map<String, String>> toAddDetailsForCampaignV2(
    Map<String, ItemSummaryListResponse> itemSkuPPCodeSummaryMap,
    List<Map<String, String>> cleanDatas) {
    for (Map<String, String> cleanValues : cleanDatas) {
      ItemSummaryListResponse itemSummaryListResponse =
        itemSkuPPCodeSummaryMap.getOrDefault(cleanValues.get(BulkParameters.ITEM_SKU_PP_CODE),null);
      if(Objects.nonNull(itemSummaryListResponse)) {
        cleanValues.put(BulkParameters.MERCHANT_CODE, itemSummaryListResponse.getMerchantCode());
        cleanValues.put(BulkParameters.PRODUCT_SKU, itemSummaryListResponse.getProductSku());
        cleanValues.put(BulkParameters.PRODUCT_CODE, itemSummaryListResponse.getProductCode());
        cleanValues.put(BulkParameters.CATEGORY, itemSummaryListResponse.getMasterCategoryCode());
        cleanValues.put(BulkParameters.SKU_NAME, itemSummaryListResponse.getItemName());
        cleanValues.put(BulkParameters.NAMA_PRODUK, itemSummaryListResponse.getProductName());
        if (itemSummaryListResponse.isMerchantPromoDiscount() && Optional
          .ofNullable(itemSummaryListResponse.getPrice().iterator().next().getMerchantPromoDiscountPrice())
          .map(DiscountPriceDTO::getDiscountPrice).isPresent()) {
          DiscountPriceDTO discountPrice =
            itemSummaryListResponse.getPrice().iterator().next().getMerchantPromoDiscountPrice();
          if (validMerchantPromoDiscountPrice(discountPrice)) {
            cleanValues.put(BulkParameters.SALE_PRICE, String.valueOf(
              itemSummaryListResponse.getPrice().iterator().next().getMerchantPromoDiscountPrice()
                .getDiscountPrice()));
          } else {
            cleanValues.put(BulkParameters.SALE_PRICE,
              String.valueOf(itemSummaryListResponse.getPrice().iterator().next().getOfferPrice()));
          }
        } else {
          cleanValues.put(BulkParameters.SALE_PRICE,
            String.valueOf(itemSummaryListResponse.getPrice().iterator().next().getOfferPrice()));
        }
        cleanValues.put(BulkParameters.NORMAL_SELLING_PRICE,
          String.valueOf(itemSummaryListResponse.getPrice().iterator().next().getOfferPrice()));
      }
    }
    return cleanDatas;
  }

  private static boolean validMerchantPromoDiscountPrice(
    DiscountPriceDTO merchantPromoDiscountPrice) {
    Date now = new Date();
    return (merchantPromoDiscountPrice.getStartDateTime().before(now) && merchantPromoDiscountPrice
      .getEndDateTime().after(now));
  }

  public static String getProductCreationBulkProcessTypeForPrioritySellers(boolean priorityQueueEnabled,
      ProfileResponse profileResponse) {
    if (priorityQueueEnabled && isTrustedSeller(profileResponse)) {
      return BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY.getValue();
    } else {
      return BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue();
    }
  }

  public static boolean isConvertedProductCreationUpload(Map<String, String> args) {
    return Objects.nonNull(args.get(BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue()))
        && Boolean.parseBoolean(args.get(BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue()));
  }

  private static boolean isTrustedSeller(ProfileResponse profileResponse) {
    return profileResponse.isTrustedSeller();
  }

  public static String getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(String bulkProcessType, int userInputRows,
      int trustedSellerRowCount, int regularSellerMinRowCount, int regularSellerMaxCount,
      boolean priorityQueueEnabled) {
    boolean isTrustedSeller = BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY.getValue().equals(bulkProcessType);
    if (priorityQueueEnabled) {
      if (isTrustedSeller) {
        if (userInputRows <= trustedSellerRowCount) {
          return BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue();
        } else {
          return BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue();
        }
      } else {
        if (userInputRows <= regularSellerMinRowCount) {
          return BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue();
        } else if (userInputRows <= regularSellerMaxCount) {
          return BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue();
        } else {
          return bulkProcessType;
        }
      }
    } else {
      return bulkProcessType;
    }
  }

  public static String getImageDownloadStatusByPriority(String bulkProcessType, boolean priorityQueueEnabled) {
    if (priorityQueueEnabled) {
      if (BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue().equals(bulkProcessType)) {
        return BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_1;
      } else if (BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue().equals(bulkProcessType)) {
        return BulkProcess.STATUS_IMAGE_PROCESSING_PRIORITY_2;
      } else {
        return BulkProcess.STATUS_IMAGE_PROCESSING;
      }
    } else {
      return BulkProcess.STATUS_IMAGE_PROCESSING;
    }
  }

  public static String getBasicInfoImageDownloadStatusByPriority(String bulkProcessType) {
    if (BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue().equals(bulkProcessType)) {
      return BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_1;
    } else if (BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue().equals(bulkProcessType)) {
      return BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING_PRIORITY_2;
    } else {
      return BulkProcess.STATUS_IMAGE_AND_VIDEO_PROCESSING;
    }
  }

  public static Set<String> getBulkProcessTypesForCreation(String bulkProcessType) {
    if (BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue().equals(bulkProcessType)) {
      return ImmutableSet.of(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue(),
          BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY.getValue(),
          BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue(),
          BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue());
    }
    if (BulkProcessType.PRODUCT_LEVEL_3.getValue().equals(bulkProcessType)) {
      return ImmutableSet.of(BulkProcessType.PRODUCT_LEVEL_3.getValue(),
          BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue(),
          BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue());
    }
    if (BulkProcessType.PRODUCT_BASIC_INFO.getValue().equals(bulkProcessType)) {
      return ImmutableSet.of(BulkProcessType.PRODUCT_BASIC_INFO.getValue(),
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue(),
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue());
    }
    return ImmutableSet.of(bulkProcessType);
  }

  public static List<AttributeResponse> getSpecialAttribute(Collection<AttributeResponse> attributes) {
    return Optional.ofNullable(attributes).orElse(new ArrayList<>()).stream().filter(AttributeResponse::isSkuValue)
        .collect(Collectors.toList());
  }

  public static List<AttributeResponse> getPredefinedAttribute(Collection<AttributeResponse> attributes) {
    return Optional.ofNullable(attributes).orElse(new ArrayList<>()).stream().filter(
            attributeResponse -> AttributeType.PREDEFINED_ATTRIBUTE.name().equals(attributeResponse.getAttributeType()))
        .collect(Collectors.toList());
  }

  public static List<AttributeResponse> getNonVariantCreatingAttribute(Collection<AttributeResponse> attributes) {
    return Optional.ofNullable(attributes).orElse(new ArrayList<>()).stream()
        .filter(Predicate.not(AttributeResponse::isVariantCreation)).collect(Collectors.toList());
  }

  public static List<AttributeResponse> getVariantCreatingAttribute(Collection<AttributeResponse> attributes) {
    return Optional.ofNullable(attributes).orElse(new ArrayList<>()).stream()
        .filter(AttributeResponse::isVariantCreation).collect(Collectors.toList());
  }

  public static Map<String, Object> removeMandatoryCharactersFromHeaders(Map<String, Object> raw) {
    Map<String, Object> modifiedRow = new LinkedHashMap<>();
    for (Map.Entry<String, Object> entry : raw.entrySet()) {
      modifiedRow.put(entry.getKey().replaceAll("\\*",""), entry.getValue());
    }
    return modifiedRow;
  }

  public static String getAttributeNameBasedOnHeader(String enName, String idName,
      boolean isInternationalMerchant) {
    if (isInternationalMerchant && StringUtils.isNotBlank(enName)) {
      return enName;
    }
    return idName;
  }


  public static Object getValueBasedOnEnOrIdHeader(Map<String, Object> raw, String engHeader, String idHeader) {
    Object data = raw.getOrDefault(engHeader, raw.get(idHeader));
    if (Constant.BRAND.equalsIgnoreCase(engHeader) || Constant.BRAND.equalsIgnoreCase(idHeader)) {
      return String.valueOf(data).replace(Constant.BRAND_IN_REVIEW, StringUtils.EMPTY);
    }
    return data;
  }

  public static boolean validateFamilyColour(String headerNameEn, String headerNameId, Map<String, Object> row,
      Map<String, List<String>> attributeValuesMap) {
    boolean result = true;
    String value = String.valueOf(getValueBasedOnEnOrIdHeader(row, headerNameEn, headerNameId));
    if (StringUtils.isNotBlank(value)) {
      List<String> attributeValues =
          Optional.ofNullable(attributeValuesMap.get(Constant.COLOUR_FAMILY)).orElse(new ArrayList<>());
      if (!attributeValues.contains(value)) {
        result = false;
      }
    }
    return result;
  }

  public static boolean validateEANValue(Map<String, Object> raw, BulkUploadErrorCounter bulkUploadErrorCounter,
      boolean isInternationalMerchant, boolean valid, StringBuilder validationErrorMessage,
    Set<String> eanUpcSet, List<Integer> eanUpcValidLength) {
    String resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
        BulkProcessValidationErrorMessages.EAN_UPC_VALUES_INVALID_EN,
        BulkProcessValidationErrorMessages.EAN_UPC_VALUES_INVALID);
    String columnRowInformation = String.valueOf(raw.get(BulkCnCreationHeaderNames.ROW_NUMBER));
    boolean validType = true;
    if (StringUtils.isNotBlank(String.valueOf(raw.get(BulkCnCreationHeaderNames.EAN_UPC_CODE_HEADER)))) {
      try {
        Long.parseLong(String.valueOf(raw.get(BulkCnCreationHeaderNames.EAN_UPC_CODE_HEADER)));
      } catch (NumberFormatException nfe) {
        bulkUploadErrorCounter.incrementVariation();
        addValidationErrorMessage(bulkUploadErrorCounter.getVariation(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        valid = false;
        validType = false;
        log.error("Row : {} . Error msg - {}. Attribute Name - {}.", (Integer.parseInt(columnRowInformation) + 1),
            BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID, BulkCnCreationHeaderNames.EAN_UPC_CODE_HEADER);
      }

      int length = String.valueOf(raw.get(BulkCnCreationHeaderNames.EAN_UPC_CODE_HEADER)).length();
      if (!(eanUpcValidLength.contains(length)) && validType) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.EAN_UPC_VALUES_INVALID_EN,
            BulkProcessValidationErrorMessages.EAN_UPC_VALUES_INVALID);
        bulkUploadErrorCounter.incrementVariation();
        addValidationErrorMessage(bulkUploadErrorCounter.getVariation(), validationErrorMessage,
            errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        valid = false;
        log.error("Row : {} . Error msg - {}. Attribute Name - {}.", (Integer.parseInt(columnRowInformation) + 1),
            BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID, BulkCnCreationHeaderNames.EAN_UPC_CODE_HEADER);
      }
      if (valid && eanUpcSet.contains(
        String.valueOf(raw.get(BulkCnCreationHeaderNames.EAN_UPC_CODE_HEADER)))) {
        resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.DUPLICATE_EAN_UPC_VALUES_INVALID_EN,
          BulkProcessValidationErrorMessages.DUPLICATE_EAN_UPC_VALUES_INVALID);
        bulkUploadErrorCounter.incrementVariation();
        addValidationErrorMessage(bulkUploadErrorCounter.getVariation(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        valid = false;
        log.error("Row : {} . Error msg - {}. Attribute Name - {}.",
          (Integer.parseInt(columnRowInformation) + 1),
          BulkProcessValidationErrorMessages.DUPLICATE_EAN_UPC_VALUES_INVALID_EN,
          BulkCnCreationHeaderNames.EAN_UPC_CODE_HEADER);
      } else {
        eanUpcSet.add(String.valueOf(raw.get(BulkCnCreationHeaderNames.EAN_UPC_CODE_HEADER)));
      }
    }
    return valid;
  }

  public static Pair validateFamilyColourAttribute(Map<String, Object> raw, String attributeName,
      AttributeResponse attributeResponse, Map<String, List<String>> attributesValues,
      boolean isInternationalMerchant) {
    boolean valid = true;
    StringBuilder validationErrorMessage = new StringBuilder();
    if (Constant.COLOUR_FAMILY.equals(
        BulkCreationCommonUtil.getAttributeNameBasedOnHeader(attributeResponse.getNameEnglish(),
            attributeResponse.getName(), isInternationalMerchant)) && !BulkCreationCommonUtil.validateFamilyColour(
        attributeResponse.getName(), attributeResponse.getNameEnglish(), raw, attributesValues)) {
      valid = false;
      log.error("Row : {} . Error msg - {}. Attribute Name - {}.",
          (Integer) raw.get(BulkCnCreationHeaderNames.ROW_NUMBER) + 1,
          BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID, String.valueOf(attributeName));
      validationErrorMessage.append(isInternationalMerchant ?
          errorMessage(attributeName + StringUtils.SPACE,
              BulkProcessValidationErrorMessages.OPTIONAL_ATTRIBUTE_VALUE_EN, StringUtils.EMPTY) :
          errorMessage(attributeName + StringUtils.SPACE, BulkProcessValidationErrorMessages.OPTIONAL_ATTRIBUTE_VALUE,
              StringUtils.EMPTY)).append(Constant.PERIOD);
    }
    return Pair.of(validationErrorMessage, valid);
  }

  public static boolean isAttributeNotFamilyColorNorEanUPC(boolean valid, String attributeName,
      AttributeResponse attributeResponse) {
    return valid && !Constant.COLOUR_FAMILY.equals(attributeName) && !Constant.EAN_UPC.equals(attributeName)
        && Objects.nonNull(attributeResponse);
  }

  public static boolean validateDefiningAttributeValues(Map<String, Object> raw, AttributeResponse attributeResponse,
      List<String> attributeValues, String sizeChartDelimiter) {
    boolean isNotBlank = StringUtils.isNotBlank(String.valueOf(
        BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, attributeResponse.getName(),
            attributeResponse.getNameEnglish())));
    if (isNotBlank) {
      return checkValuePresentInListOfValues(raw, attributeResponse, attributeValues, sizeChartDelimiter);
    } else {
      return attributeResponse.isMandatory();
    }
  }

  private static boolean checkValuePresentInListOfValues(Map<String, Object> raw, AttributeResponse attributeResponse,
      List<String> attributeValues, String sizeChartDelimiter) {
    String value = String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, attributeResponse.getName(),
        attributeResponse.getNameEnglish()));
    if (attributeValues.contains(value)) {
      return false;
    } else {
      Map<String, List<String>> valueAndValueTypeMap =
          getAttributeValueAndValueTypeMap(attributeValues, sizeChartDelimiter);
      if (valueAndValueTypeMap.containsKey(value) && valueAndValueTypeMap.get(value).size() == Constant.ONE) {
        updateDefiningAttributeValue(raw, attributeResponse, valueAndValueTypeMap, value);
        return false;
      } else {
        return true;
      }
    }
  }

  private static void updateDefiningAttributeValue(Map<String, Object> raw, AttributeResponse attributeResponse,
      Map<String, List<String>> valueAndValueTypeMap, String value) {
    if (raw.containsKey(attributeResponse.getName())) {
      raw.put(attributeResponse.getName(), valueAndValueTypeMap.get(value).get(0));
    } else {
      raw.put(attributeResponse.getNameEnglish(), valueAndValueTypeMap.get(value).get(0));
    }
  }

  private static Map<String, List<String>> getAttributeValueAndValueTypeMap(Collection<String> attributeValues,
      String sizeChartDelimiter) {
    return attributeValues.stream()
        .collect(Collectors.groupingBy(value -> getValueFromValueAndValueType(value, sizeChartDelimiter)));
  }

  private static String getValueFromValueAndValueType(String value, String sizeChartDelimiter) {
    String[] tokens = Optional.ofNullable(value).orElse(StringUtils.EMPTY).split(sizeChartDelimiter);
    return tokens[tokens.length - 1];
  }

  public static MerchantStatusType getMerchantType(ProfileResponse businessPartner) {
    List<String> salesChannel =
        Optional.ofNullable(businessPartner).map(ProfileResponse::getCompany).map(CompanyDTO::getSalesChannel)
            .orElse(new ArrayList<>());
    if (salesChannel.contains(Constant.B2B_SELLER_CHANNEL)) {
      if (businessPartner.getCompany().isCncActivated()) {
        return MerchantStatusType.BFB_AND_CNC;
      }
      return MerchantStatusType.BFB;
    } else {
      if (Objects.nonNull(businessPartner.getCompany()) && businessPartner.getCompany().isCncActivated()) {
        return MerchantStatusType.DELIVERY_AND_CNC;
      }
      return MerchantStatusType.PURE_DELIVERY;
    }
  }

  public static boolean checkBpBopisEligibility(Map<String, Object> raw, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean bpBopisRestrictionEnabled, ProfileResponse profileResponse,
      boolean isInternationalMerchant, CategoryDetailResponse category, boolean bopisCategoryRestrictionEnabled,
      String bopisCategoryValidationForSellerTypes, String primaryIdentifier) {
    if (bpBopisRestrictionEnabled) {
      Integer productType = getProductType(raw);
      log.info("Validating seller eligibility for the Product Type : {}, Business Partner Code {} ",
        productType, profileResponse.getBusinessPartnerCode());
      StringBuilder validationErrorMessage = new StringBuilder();
      String columnRowInformation = String.valueOf(raw.get(GenericBulkHeaders.ROW_NUMBER));
      boolean isBigProduct = productType.equals(ProductType.BIG_PRODUCT.getCode());
      boolean isBopisProduct = productType.equals(ProductType.BOPIS.getCode());
      boolean sellerBPFlag = Objects.nonNull(profileResponse.getBigProductFlag()) ?
        profileResponse.getBigProductFlag() :
        true;
      boolean sellerBopisFlag =
        Objects.nonNull(profileResponse.getBopisFlag()) ? profileResponse.getBopisFlag() : true;
      if ((isBigProduct && !sellerBPFlag) || (isBopisProduct && !sellerBopisFlag)) {
        String resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
          BulkProcessValidationErrorMessages.BP_BOPIS_ELIGIBILITY_ERROR_EN,
          BulkProcessValidationErrorMessages.BP_BOPIS_ELIGIBILITY_ERROR);
        log.error(
          "Validation error bulk process : {}, : Row : {}. Product Name - {}. Error msg - {} : Seller {} is not eligible for the Product Type: {}.",
          bulkProcessNotes.getBulkProcessCode(), columnRowInformation,
          raw.get(GenericBulkParameters.PRODUCT_NAME_COLUMN),
          BulkErrorCategory.INPUT_ERROR.getDescription(), profileResponse.getBusinessPartnerCode(),
          productType);
        addValidationErrorMessage(bulkUploadErrorCounter.getProductType(), validationErrorMessage,
          errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
        bulkProcessNotes.setNotes(validationErrorMessage.toString());
        return false;
      }
    }
    Set<String> sellerTypeValidationForBopisCategoryRestriction =
        Optional.ofNullable(bopisCategoryValidationForSellerTypes)
            .map(value -> Arrays.stream(value.split(Constant.COMMA)).map(String::trim).collect(Collectors.toSet()))
            .orElse(Collections.emptySet());
    if (bopisCategoryRestrictionEnabled && !category.isBopisEligible()
        && sellerTypeValidationForBopisCategoryRestriction.contains(profileResponse.getCompany().getMerchantType())) {
      Integer productType = getProductType(raw);
      boolean isExternalUserOnly = isExternalUserOnly(primaryIdentifier);
      if (productType.equals(ProductType.BOPIS.getCode()) && isExternalUserOnly) {
        return populateErrorMessageAndFailValidation(raw, bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant, category, productType);
      }
    }
    return true;
  }

  private static boolean populateErrorMessageAndFailValidation(Map<String, Object> raw, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant, CategoryDetailResponse category,
      Integer productType) {
    StringBuilder validationErrorMessage = new StringBuilder();
    String columnRowInformation = String.valueOf(raw.get(GenericBulkHeaders.ROW_NUMBER));
    String resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
        BulkProcessValidationErrorMessages.BOPIS_ELIGIBILITY_ERROR_EN,
        BulkProcessValidationErrorMessages.BOPIS_ELIGIBILITY_ERROR);
    log.error(
        "Validation error bulk process : {}, : Row : {}. Product Name - {}. Error msg - {} : Category {} is not eligible for the Product Type: {}.",
        bulkProcessNotes.getBulkProcessCode(), columnRowInformation,
        raw.get(GenericBulkParameters.PRODUCT_NAME_COLUMN), BulkErrorCategory.INPUT_ERROR.getDescription(),
        category.getCategoryCode(), productType);
    addValidationErrorMessage(bulkUploadErrorCounter.getProductType(), validationErrorMessage,
        errorMessage(StringUtils.EMPTY, resultErrorMessage, StringUtils.EMPTY));
    bulkProcessNotes.setNotes(validationErrorMessage.toString());
    return false;
  }

  private static Integer getProductType(Map<String, Object> raw) {
    return (Integer) BulkUploadOption.valueFrom(String.valueOf(Objects.nonNull(
      getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.PRODUCT_TYPE_NAME,
        GenericBulkHeaders.PRODUCT_TYPE_NAME_EN)) ?
      getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.PRODUCT_TYPE_NAME,
        GenericBulkHeaders.PRODUCT_TYPE_NAME_EN) :
      getValueBasedOnEnOrIdHeader(raw, GenericBulkHeaders.PRODUCT_TYPE,
        GenericBulkHeaders.PRODUCT_TYPE_EN))).getValue();
  }

  public static boolean evaluateProcessorConditions(boolean resultRaw, boolean resultAttribute, boolean resultPickup,
      boolean resultVariant, boolean resultCategoryOrGroupraw, boolean resultProtectedBrand,
      boolean resultBpBopisEligibility) {
    return resultRaw && resultAttribute && resultPickup && resultVariant && resultCategoryOrGroupraw
        && resultProtectedBrand && resultBpBopisEligibility;
  }

  public static TreeSet getImageAllowedTypeList(String imageTypesAllowed) {
    List<String> allowedImageTypeList =
      Stream.of(StringUtils.split(imageTypesAllowed, ",")).collect(Collectors.toList());
    return new TreeSet<String>(String.CASE_INSENSITIVE_ORDER) {
      {
        this.addAll(allowedImageTypeList);
      }
    };
  }

  public static String getImageType(String imageType) {
    String type = StringUtils.EMPTY;
    if (StringUtils.containsIgnoreCase(imageType, JPEG)) {
      type = JPEG;
    } else if (StringUtils.containsIgnoreCase(imageType, JPG)) {
      type = JPG;
    } else if (StringUtils.containsIgnoreCase(imageType, PNG)) {
      type = PNG;
    } else if (StringUtils.containsIgnoreCase(imageType, WEBP)) {
      type = WEBP;
    }
    return type;
  }

  public  static BulkProcess getWorkOrderBulkProcess(String storeId, String requestId, String bulkProcessCode,
      BulkUpdateProcessDTO bulkWorkOrderDTO, int successCount, int errorCount) throws Exception {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setStartDate(Calendar.getInstance().getTime());
    bulkProcess
        .setBulkProcessType(BulkProcessType.getBulkProcessType(bulkWorkOrderDTO.getBulkProcessType()).getValue());
    bulkProcess.setBusinessPartnerCode(bulkWorkOrderDTO.getBusinessPartnerCode());
    bulkProcess.setBulkProcessCode(bulkProcessCode);
    bulkProcess.setStoreId(storeId);
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);
    bulkProcess.setRequestId(requestId);
    bulkProcess.setSuccessCount(successCount);
    bulkProcess.setErrorCount(errorCount);
    bulkProcess.setInputErrorCount(0);
    bulkProcess.setSystemErrorCount(0);
    return bulkProcess;
  }

  public static BulkUpdateQueue getWorkOrderBulkUpdateQueue(String storeId, String requestId, String bulkProcessCode,
      BulkUpdateProcessDTO bulkWorkOrderDTO) {
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setFileName(bulkWorkOrderDTO.getFileName());
    bulkUpdateQueue.setStoreId(storeId);
    bulkUpdateQueue.setBusinessPartnerCode(bulkWorkOrderDTO.getBusinessPartnerCode());
    bulkUpdateQueue.setBulkProcessType(bulkWorkOrderDTO.getBulkProcessType());
    bulkUpdateQueue.setBulkProcessCode(bulkProcessCode);
    bulkUpdateQueue.setPrivilegedMap(bulkWorkOrderDTO.getPrivilegedMap());
    bulkUpdateQueue.setUpdatedBy(bulkWorkOrderDTO.getUpdatedBy());
    bulkUpdateQueue.setClientHost(bulkWorkOrderDTO.getClientHost());
    bulkUpdateQueue.setRequestId(requestId);
    return bulkUpdateQueue;
  }

  public static String setNotesForEmptyFileUploaded(BulkProcess bulkProcess, String excelFileName) {
    String notes;
    notes = excelFileName.concat(Constant.PERIOD).concat(BulkCreationCommonUtil.emptyFileErrorMessage.get(
            Optional.ofNullable(bulkProcess.getInternationalMerchant()).orElse(Boolean.FALSE))
        .concat(StringUtils.SPACE));
    bulkProcess.setDescription(notes);
    return notes;
  }

  public static Map<String, String> getRawBfbValues(Map<String, Object> raw, String basePriceHeaderId,
      String basePriceHeaderEn, String managedHeaderId, String managedHeaderEn, String statusHeaderId) {
    Map<String, String> rawBfbValues = new HashMap<>();
    rawBfbValues.put(BFB_BASE_PRICE,
        String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, basePriceHeaderId, basePriceHeaderEn)));
    rawBfbValues.put(BFB_MANAGED,
        String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, managedHeaderId, managedHeaderEn)));
    rawBfbValues.put(BFB_STATUS,
        String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, statusHeaderId, statusHeaderId)));
    return rawBfbValues;
  }

  public static void getBulkInternalProcess(BulkUpdateProcessDTO bulkUpdateProcessDTO,
      BulkInternalProcess bulkInternalProcess) {
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.getInternalBulkProcessType(bulkUpdateProcessDTO.getBulkProcessType()).getValue());
    bulkInternalProcess.setStatus(ProcessStatus.PENDING.name());
    bulkInternalProcess.setSellerCode(bulkUpdateProcessDTO.getBusinessPartnerCode());
    bulkInternalProcess.setSellerName(bulkUpdateProcessDTO.getUpdatedBy());
    bulkInternalProcess.setCreatedBy(bulkUpdateProcessDTO.getUpdatedBy());
    bulkInternalProcess.setErrorCount(0);
    bulkInternalProcess.setSuccessCount(0);
    bulkInternalProcess.setStoreId(Constant.STORE_ID);
    bulkInternalProcess.setFileName(bulkUpdateProcessDTO.getFileName());
  }

  public static Set<ProductSalesChannelType> getProductSalesChannel(boolean instoreSeller,
      List<Map<String, Object>> rows, boolean genericFlow) {
    Set<ProductSalesChannelType> productSalesChannelTypes = new HashSet<>();
    String deliveryStatusHeader = DELIVERY_STATUS_HEADER.get(genericFlow);
    String deliveryStatusEnHeader = DELIVERY_STATUS_EN_HEADER.get(genericFlow);
    String cncHeader = CNC_HEADER.get(genericFlow);
    String cncEnHeader = CNC_EN_HEADER.get(genericFlow);
    String instoreHeader = INSTORE_HEADER.get(genericFlow);
    if (instoreSeller) {
      boolean online = false;
      boolean cnc = false;
      boolean instore = false;
      for (Map<String, Object> row : rows) {
        int deliveryValue = parseStatus(row, deliveryStatusHeader, deliveryStatusEnHeader);
        if (deliveryValue == 1) {
          online = true;
        }
        int cncValue = parseStatus(row, cncHeader, cncEnHeader);
        if (cncValue == 1) {
          cnc = true;
        }
        int instoreValue = parseStatus(row, instoreHeader, instoreHeader);
        if (instoreValue == 1) {
          instore = true;
        }
        if (online || cnc) {
          productSalesChannelTypes.add(ProductSalesChannelType.B2C_PRODUCT);
        }
      }
      if (instore) {
        productSalesChannelTypes.add(ProductSalesChannelType.INSTORE_PRODUCT);
      }
    }
    return productSalesChannelTypes;
  }

  private static int parseStatus(Map<String, Object> row, String header, String headerEn) {
    String status = String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(row, header, headerEn));
    if (StringUtils.isBlank(status)) {
      return 0;
    }
    try {
      return Integer.parseInt(status);
    } catch (NumberFormatException e) {
      return 0;
    }
  }

  public static boolean isPureInstoreProduct(List<Map<String, Object>> rows, boolean instoreSeller,
      boolean genericFlow) {
    boolean pureInstoreProduct = false;
    Set<ProductSalesChannelType> salesChannels =
        BulkCreationCommonUtil.getProductSalesChannel(instoreSeller, rows, genericFlow);
    if (salesChannels.size() == 1 && salesChannels.contains(ProductSalesChannelType.INSTORE_PRODUCT)) {
      pureInstoreProduct = true;
    }
    return pureInstoreProduct;
  }

  private static double parseDimensionForRequestFormation(String dimensionStr) {
    if (StringUtils.isBlank(dimensionStr)) {
      return 0.0;
    }
    try {
      return Double.parseDouble(dimensionStr);
    } catch (NumberFormatException e) {
      return 0.0;
    }
  }

  private static double parseDimensionForValidation(String dimensionStr) {
    if (StringUtils.isBlank(dimensionStr)) {
      return 0.0;
    }
    return Double.parseDouble(dimensionStr);
  }

  public static void checkIfPPCodeIsCncActivated(boolean resetCncFLagForNonCncPP, Map<String, Object> row,
      List<PickupPointResponse> pickupPointResponseList, boolean genericFlow) {
    if (resetCncFLagForNonCncPP && CollectionUtils.isNotEmpty(pickupPointResponseList) && !pickupPointResponseList.get(
        0).isCncActivated()) {
      String cncHeader = BulkCreationCommonUtil.CNC_HEADER.get(genericFlow);
      String cncEnHeader = BulkCreationCommonUtil.CNC_EN_HEADER.get(genericFlow);
      updateCncValue(row, cncHeader, cncEnHeader);
    }
  }

  public static void updateCncValue(Map<String, Object> row, String cncHeader, String cncEnHeader) {
    if (row.containsKey(cncHeader)) {
      row.put(cncHeader, Constant.ZERO);
    } else if (row.containsKey(cncEnHeader)) {
      row.put(cncEnHeader, Constant.ZERO);
    }
  }

}

