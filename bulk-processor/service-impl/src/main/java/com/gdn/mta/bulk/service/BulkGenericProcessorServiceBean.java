package com.gdn.mta.bulk.service;

import static com.gdn.mta.bulk.helper.ProductLevel3ProcessorValidationMessageHelper.errorMessage;
import static com.gdn.mta.bulk.helper.ProductLevel3ProcessorValidationMessageHelper.errorMessageBasedOnMerchant;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
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
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;
import com.gdn.mta.bulk.dto.ValidateExcelRowsRequest;
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.entity.BulkCreateProductEventModel;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessImage;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.helper.ExcelTemplateUtil;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.repository.AttributeRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductRepository;
import com.gdn.mta.bulk.repository.generator.GeneratorRepository;
import com.gdn.mta.bulk.util.BulkCreationCommonUtil;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.mta.bulk.util.GenericBulkHeaders;
import com.gdn.mta.bulk.util.GenericBulkParameters;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailAndShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeResponse;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BulkGenericProcessorServiceBean implements BulkGenericProcessorService {

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private AttributeRepository attributeRepository;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private GeneratorRepository generatorRepository;

  @Autowired
  private SystemParameter systemParameter;

  @Autowired
  private TrackerService trackerService;

  @Autowired
  private PCBOutboundService pcbOutboundService;

  @Autowired
  private PBPOutboundServiceBean pbpOutboundServiceBean;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Autowired
  private BulkProcessImageService bulkProcessImageService;

  @Autowired
  private ProtectedBrandValidationService protectedBrandValidationService;

  @Autowired
  private BulkCreationCommonUtil bulkCreationCommonUtil;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private PickupPointService pickupPointService;

  @Value("${system.parameter.max.stock.value}")
  private long maxStockLimit;

  @Value("${variant.attribute.switch}")
  private boolean variantAttributeSwitch;

  @Value("${pickup.point.name.delimiter}")
  private String ppNameDelimiter;

  @Value("${bp.bopis.restriction.enabled}")
  private boolean bpBopisRestrictionEnabled;

  @Value("${product.bundling.enabled}")
  private boolean productBundlingEnabled;

  @Value("${bopis.category.restriction.feature.switch}")
  private boolean bopisCategoryRestrictionEnabled;

  @Value("${bopis.cnc.restriction.feature.switch}")
  private boolean bopisCncRestrictionEnabled;

  @Value("${override.empty.product.type.bulk.creation}")
  private boolean overrideEmptyProductTypeBulkCreation;

  @Value("${system.parameter.bopis.unsupported.merchant.types}")
  private String bopisCategoryValidationForSellerTypes;

  @Value("${product.bundling.eligible.merchants}")
  private String productBundlingEligibleMerchantTypes;

  @Value("${product.bundling.max.number.of.skus}")
  private int productBundlingMaxNumberOfSkus;

  @Value("${generic.file.header.validation.en}")
  private boolean genericFileHeaderValidationEn;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${size.chart.value.type.delimiter}")
  private String sizeChartValueTypeDelimiter;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  @Value("${reset.cnc.flag.for.non.cnc.pp}")
  private boolean resetCncFLagForNonCncPP;

  @Value("${override.pickup.point.in.creation}")
  private boolean overridePickupPointInCreation;

  @Value("${fetch.brand.ignore.case.in.creation}")
  private boolean fetchBrandIgnoreCaseInCreation;

  @Value("${ean.upc.valid.length}")
  private List<Integer> eanUpcValidLength;

  public static final int HEADER_VALIDATION_START_INDEX = 3;
  public static final int ATTRIBUTE_COLUMN_NUMBERS = 10;
  private static final int PARENT_COLUMN_NUMBER = 14;
  private static final int SELLER_TYPE_DELIVERY = 1;
  private static final int SELLER_TYPE_CNC = 2;
  private static final int SELLER_TYPE_BFB = 3;
  private static final int SELLER_TYPE_BFB_AND_CNC = 4;
  private static final int SELLER_TYPE_DELIVERY_INSTORE_COLUMN_INDEX = 32;
  private static final int SELLER_TYPE_CNC_INSTORE_COLUMN_INDEX = 33;
  private static final int SELLER_TYPE_BFB_INSTORE_COLUMN_INDEX = 35;
  private static final int SELLER_TYPE_BFB_AND_CNC_INSTORE_COLUMN_INDEX = 36;

  List<Integer> nonBfbSellerValues = Arrays.asList(1,2);

  public static final ImmutableMap<Integer, Integer> SELLER_TYPE_INSTORE_COLUMN_INDEX_MAP =
      ImmutableMap.<Integer, Integer>builder().put(SELLER_TYPE_DELIVERY, SELLER_TYPE_DELIVERY_INSTORE_COLUMN_INDEX)
          .put(SELLER_TYPE_CNC, SELLER_TYPE_CNC_INSTORE_COLUMN_INDEX)
          .put(SELLER_TYPE_BFB, SELLER_TYPE_BFB_INSTORE_COLUMN_INDEX)
          .put(SELLER_TYPE_BFB_AND_CNC, SELLER_TYPE_BFB_AND_CNC_INSTORE_COLUMN_INDEX).build();

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void  processBulkGenericEvent(BulkCreateProductEventModel bulkCreateProductRequest) throws Exception {
    log.info("Bulk process in progress, generic create for process id : {}, product : {}",
        bulkCreateProductRequest.getBulkProcessCode(), bulkCreateProductRequest.getParentProduct());
    BulkProcess bulkProcess = bulkProcessService
        .findByBulkProcessCode(bulkCreateProductRequest.getStoreId(), bulkCreateProductRequest.getBulkProcessCode());
    List<BulkProcessData> bulkProcessDataList = bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(bulkCreateProductRequest.getStoreId(), bulkProcess,
            bulkCreateProductRequest.getParentProduct(), BulkProcessData.STATUS_PENDING);
    Set<String> accessiblePickupPointCodes = new HashSet<>();
    String notes = bulkProcessDataList.stream().map(BulkProcessData::getNotes).findFirst()
        .orElse(StringUtils.EMPTY);
    if (!notes.isEmpty()) {
      accessiblePickupPointCodes.addAll(
          Arrays.stream(notes.split(Constant.COMMA)).map(String::trim).collect(Collectors.toSet()));
    }

    try {
      if (CollectionUtils.isNotEmpty(bulkProcessDataList)) {
        log.info("Start processing for rows : {}", bulkProcessDataList.size());
        List<Map<String, Object>> userInputRows = new ArrayList<>();
        List<BulkProcessData> updatedBlpDataList = new ArrayList<>();
        for (BulkProcessData bulkProcessData : bulkProcessDataList) {
          userInputRows.add(setBlpInitialData(bulkProcessData));
          updatedBlpDataList.add(bulkProcessData);
        }
        bulkProcessDataService.saveBulkProcessData(updatedBlpDataList);
        BulkUploadErrorCounter bulkUploadErrorCounter = new BulkUploadErrorCounter();
        Set<String> parentCategoryCodes = new HashSet<>();
        Map<String, String> imageUrlAndLocationMap = new HashMap<>();
        ProfileResponse businessPartner =
            this.businessPartnerRepository.filterByBusinessPartnerCodeV2(bulkCreateProductRequest.getStoreId(),
                bulkCreateProductRequest.getBusinessPartnerCode());
        MerchantStatusType merchantStatusType = BulkCreationCommonUtil.getMerchantType(businessPartner);
        Map<String, String> userInputCategoryTreeAndChildCategory =
            BulkCreationCommonUtil.getUserInputCategoryTreeAndChildCategory(userInputRows, parentCategoryCodes);
        boolean isInternationalMerchant = businessPartner.getCompany().isInternationalFlag();
        boolean ignoreB2bExclusive = nonBfbSellerValues.stream()
          .anyMatch(sellerValue -> sellerValue.equals(merchantStatusType.getType()));
        List<CategoryTreeResponse> response =
          pcbOutboundService.getGenericTemplateCategories(true, ignoreB2bExclusive);
        BulkCreationCommonUtil
            .getMatchingCategoryHierarchyDelimitedAndCategoryIdMap(userInputCategoryTreeAndChildCategory,
                parentCategoryCodes, response, isInternationalMerchant);
        log.info("userInputCategoryTreeAndChildCategory : {} blpCode : {} ", userInputCategoryTreeAndChildCategory,
            bulkProcess.getBulkProcessCode());

        BulkCreationCommonUtil.addCnCategoryIdToEachRow(userInputCategoryTreeAndChildCategory, userInputRows);
        String brandInputValue = String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(
            userInputRows.stream().findFirst().orElse(new HashMap<>()), GenericBulkHeaders.BRAND,
            GenericBulkHeaders.BRAND_EN));
        String cleanBrandValue = brandInputValue.contains(Constant.IN_REVIEW) ?
          brandInputValue.substring(0, brandInputValue.indexOf(Constant.IN_REVIEW_BRAND_SUFFIX)) :
          brandInputValue;
        Map<String, CategoryDetailAndShippingResponse> categoryIdAndDetailMap =
            getCategoryDetailResponseByCategoryId(userInputCategoryTreeAndChildCategory.values());
        Map<String, String> attributeNameAndIdMap = new HashMap<>();
        Map<String, String> variantCreationNameAndColumnName = new HashMap<>();
        Map<String, Map<String, String>> categoryVariantAttributeIdAndNameMap = new HashMap<>();
        Map<String, AttributeResponse> attributeIdAndResponse =
            getAttributeInformation(bulkCreateProductRequest.getStoreId(), categoryIdAndDetailMap,
                attributeNameAndIdMap, cleanBrandValue);
        Map<String, List<String>> categoryVariantCreationAttributes = BulkCreationCommonUtil
            .getCategoryVariantCreationAttributes(categoryIdAndDetailMap, variantCreationNameAndColumnName,
                categoryVariantAttributeIdAndNameMap, isInternationalMerchant);
        List<Map<String, Object>> invalidRows = new ArrayList<>();
        List<Integer> validRowNumbers = new ArrayList<>();
        boolean instoreSeller = CommonUtils.isInstoreEligibleSellerWithSwitch(instoreNewFlowEnabled, businessPartner);
        List<Map<String, Object>> validUserRows =
            validateSingleProduct(userInputRows, bulkProcess, attributeIdAndResponse, bulkUploadErrorCounter,
                businessPartner, categoryVariantAttributeIdAndNameMap, categoryIdAndDetailMap, imageUrlAndLocationMap,
                invalidRows, validRowNumbers, merchantStatusType, accessiblePickupPointCodes, instoreSeller);


        if (CollectionUtils.isNotEmpty(validUserRows)) {
          BulkCreationCommonUtil.generateProductItemNamesForGenericCreation(validUserRows, categoryVariantCreationAttributes);
          GenerateShippingWeightRequest generateShippingWeightRequest = new GenerateShippingWeightRequest();
          ProductCreationRequest productCollectionRequest =
              BulkCreationCommonUtil.convertUserRowsToProductCollectionRequests(validUserRows, attributeIdAndResponse,
                  bulkProcess, businessPartner, categoryIdAndDetailMap, attributeNameAndIdMap,
                  variantCreationNameAndColumnName, isInternationalMerchant, generateShippingWeightRequest,
                  merchantStatusType, ppNameDelimiter, cncForWarehouseFeatureSwitch, instoreSeller);
          GenerateShippingWeightResponse generateShippingWeightResponse;
          if (CommonUtils.isEligibleForShippingWeightGeneration(generateShippingWeightRequest)) {
            generateShippingWeightResponse =
                this.generatorRepository.generateShippingWeight(generateShippingWeightRequest);
          } else {
            generateShippingWeightResponse = new GenerateShippingWeightResponse(Constant.DEFAULT_SHIPPING_WEIGHT);
          }
          productCollectionRequest.setShippingWeight(generateShippingWeightResponse.getShippingWeight());
          String productCode =
              productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()).getValue();
          try {
            String autoUploadUrlSubstring = systemParameterConfigService.findValueByStoreIdAndVariable(
                Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING).getValue();
            bulkCreationCommonUtil.setProductCreationRequest(productCollectionRequest, bulkProcess, productCode,
                validUserRows, imageUrlAndLocationMap, this.systemParameter.getMtaImageSource(), categoryIdAndDetailMap,
                autoUploadUrlSubstring, isInternationalMerchant);
            this.pbpOutboundServiceBean.createNewProduct(bulkProcess.getRequestId(), bulkProcess.getCreatedBy(),
                productCollectionRequest);
            this.trackerService.sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
                TrackerConstants.SUBMIT, TrackerConstants.SUCCESS,
                productCollectionRequest.getProductCategories().get(0).getCategory().getCategoryCode());
          } catch (ApplicationException ex) {
            bulkUploadErrorCounter.incrementSystemError();
            log.error(
                "Error occurred while post processing request of productCode : {}, bulkProcessCode : {} for Bulk Upload ",
                productCode, bulkCreateProductRequest.getBulkProcessCode(), ex);
            productCollectionRequest.setProductCode(productCode);
            bulkProcessDataList.forEach(bulkProcessData -> {
              BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
              bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
              bulkProcessNotes.setBulkProcess(bulkProcess);
              bulkProcessNotes.setNotes(
                  Constant.ROW + bulkProcessData.getRowNumber() + Constant.PERIOD + StringUtils.replace(ex.getMessage(),
                      ErrorCategory.UNSPECIFIED.getMessage(), StringUtils.EMPTY) + StringUtils.SPACE
                      + productCollectionRequest.getName());
              bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
              bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
              bulkProcessData.setSystemErrorCount(1);
              bulkProcessData.setEndDate(new Date());
              bulkProcessData.setErrorMessage(bulkProcessNotes.getNotes());
            });
            trackerService.trackProductCreationFailure(bulkCreateProductRequest.getBulkProcessCode(),
                productCollectionRequest, ex.getMessage());
            log.error("{} Error for product Code {} and productCollectionRequest : {}",
                bulkProcess.getBulkProcessCode(), productCode, productCollectionRequest);
          } catch (Exception ex) {
            bulkUploadErrorCounter.incrementSystemError();
            log.error(
                "Error occurred while post processing request of productCode : {}, bulkProcessCode : {} for Bulk Upload ",
                productCode, bulkCreateProductRequest.getBulkProcessCode(), ex);
            productCollectionRequest.setProductCode(productCode);
            bulkProcessDataList.forEach(bulkProcessData -> {
              BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
              bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
              bulkProcessNotes.setBulkProcess(bulkProcess);
              bulkProcessNotes.setNotes(Constant.ROW + bulkProcessData.getRowNumber() + Constant.PERIOD
                  + Constant.DESCRIPTION_UPLOADING_FAILED + productCollectionRequest.getName());
              bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
              bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
              bulkProcessData.setSystemErrorCount(1);
              bulkProcessData.setEndDate(new Date());
              bulkProcessData.setErrorMessage(bulkProcessNotes.getNotes());
            });
            trackerService
                .trackProductCreationFailure(bulkCreateProductRequest.getBulkProcessCode(), productCollectionRequest,
                    ex.getMessage());
            log.error("{} Error for product Code {} and productCollectionRequest : {}",
                bulkProcess.getBulkProcessCode(), productCode, productCollectionRequest);
          }
        }
        Map<Integer, String> rowNumberErrorMap = invalidRows.stream().collect(Collectors
            .toMap(row -> Integer.valueOf(String.valueOf(row.get(GenericBulkHeaders.ROW_NUMBER))),
                row -> String.valueOf(row.get(GenericBulkHeaders.BULK_PROCESS_NOTES))));
        log.info("Saving valid and invalid rows in BLP_BULK_PROCESS_DATA with error {} ",
            bulkProcess.getBulkProcessCode());
        updatedBlpDataList = new ArrayList<>();
        for (BulkProcessData bulkProcessData : bulkProcessDataList) {
          BulkCreationCommonUtil
              .setDataFinalStatus(bulkProcessData, rowNumberErrorMap, bulkUploadErrorCounter.getSystemError());
          updatedBlpDataList.add(bulkProcessData);
        }
        bulkProcessDataService.saveBulkProcessData(bulkProcessDataList);
      }
    } catch (Exception ex) {
      bulkProcessDataList.forEach(bulkProcessData -> {
        BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
        bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
        bulkProcessNotes.setBulkProcess(bulkProcess);
        bulkProcessNotes.setNotes(
            Constant.ROW + bulkProcessData.getRowNumber() + Constant.PERIOD + Constant.DESCRIPTION_UPLOADING_FAILED);
        bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
        bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
        bulkProcessData.setSystemErrorCount(1);
        bulkProcessData.setEndDate(new Date());
        bulkProcessData.setErrorMessage(bulkProcessNotes.getNotes());
      });
      bulkProcessDataService.saveBulkProcessData(bulkProcessDataList);
      log.error("{} System Error for product : {} ", bulkProcess.getBulkProcessCode(),
          bulkCreateProductRequest.getParentProduct(), ex);
    }
  }

  private Map<String, Object> setBlpInitialData(BulkProcessData bulkProcessData) throws IOException {
    bulkProcessData.setStatus(BulkProcessData.STATUS_IN_PROGRESS);
    bulkProcessData.setStartDate(new Date());
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    LinkedHashMap<String, Object> rowDataJson = objectMapper.readValue(bulkProcessData.getBulkRequestData(), typeRef);
    rowDataJson.put(GenericBulkHeaders.ROW_NUMBER, bulkProcessData.getRowNumber());
    return rowDataJson;
  }

  private Integer getMinimumPrice(String storeId) {
    SystemParameterConfig minimumPrice =
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId, Constant.MINIMUM_PRICE);
    if (Objects.nonNull(minimumPrice)) {
      return Integer.parseInt(minimumPrice.getValue());
    }
    return null;
  }

  private List<Map<String, Object>> validateSingleProduct(List<Map<String, Object>> rows, BulkProcess bulkProcess,
      Map<String, AttributeResponse> attributesIdAndAttributeResponse,
      BulkUploadErrorCounter bulkUploadErrorCounter, ProfileResponse profileResponse,
      Map<String, Map<String, String>> categoryVariantAttributeIdAndNameMap,
      Map<String, CategoryDetailAndShippingResponse> categoryIdAndDetailMap, Map<String, String> imageUrlAndLocationMap,
      List<Map<String, Object>> invalidRows, List<Integer> validRowNumbers, MerchantStatusType merchantStatusType,
      Set<String> accessiblePickupPointCodes, boolean instoreSeller) throws Exception {
    String commonImageErrorMessage;
    boolean isInternationalMerchant = profileResponse.getCompany().isInternationalFlag();
    String merchantType =
        Optional.ofNullable(profileResponse.getCompany()).map(CompanyDTO::getMerchantType).orElse(StringUtils.EMPTY);
    Map<String, Boolean> variantValuesPopulated = new HashMap<>();
    List<Map<String, Object>> validRows = new ArrayList<>();
    int minimumPrice = getMinimumPrice(bulkProcess.getStoreId());
    Map<String, String> protectedBrandNameCodeMap =
      protectedBrandValidationService.fetchProtectedBrandNameCodeMap(bulkProcess.getStoreId());
    Map<String, List<String>> attributesIdAndPossibleValues =
        BulkCreationCommonUtil.getAttributesValues(attributesIdAndAttributeResponse);
    Set<String> parentToVariantCreationSet = new HashSet<>();
    Set<String> eanUpcSet = new HashSet<>();
    List<BulkProcessImage> bulkProcessImageList =
        bulkProcessImageService.findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    commonImageErrorMessage =
        bulkCreationCommonUtil.validateExcelImagesForSingleRow(rows, bulkProcess, bulkUploadErrorCounter,
            isInternationalMerchant, imageUrlAndLocationMap, bulkProcessImageList, systemParameter.getImageMaxSize());
    Map<String, PickupPointResponse> pickupPointPickupPointResponse = new HashMap<>();
    checkAndResetCncFlag(rows, profileResponse, pickupPointPickupPointResponse);
    // Compute if it is pure instore
    boolean pureInstoreProduct = BulkCreationCommonUtil.isPureInstoreProduct(rows, instoreSeller, true);
    for (Map<String, Object> row : rows) {
      String pickupPointCode = String.valueOf(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(row, GenericBulkHeaders.PICKUP_POINT,
              GenericBulkHeaders.PICKUP_POINT_EN));
      pickupPointCode =
          BulkCreationCommonUtil.getPickupPointCodeFromPickupPointCodeAndName(ppNameDelimiter, pickupPointCode);
      List<PickupPointResponse> pickupPointResponseList;
      if (pickupPointPickupPointResponse.containsKey(pickupPointCode)) {
        pickupPointResponseList = Collections.singletonList(pickupPointPickupPointResponse.get(pickupPointCode));
      } else {
        pickupPointResponseList = this.pickupPointService.getPickupPointSummaryFilter(0,
            PickupPointFilterRequest.builder().businessPartnerCode(profileResponse.getBusinessPartnerCode())
                .codes(new HashSet<>(Collections.singletonList(pickupPointCode))).build());
      }
      //Check if only 1 pp code then default it
      overrideIfSinglePickupPoint(profileResponse, row, pickupPointResponseList, pickupPointPickupPointResponse);

      Map<String, PickupPointResponse> pickupPoints = BulkCreationCommonUtil.getPickupPointsV2(pickupPointResponseList);
      BulkCreationCommonUtil.checkIfPPCodeIsCncActivated(resetCncFLagForNonCncPP, row, pickupPointResponseList, true);
      BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
      bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessNotes.setBulkProcess(bulkProcess);
      bulkProcessNotes.setNotes(Constant.ROW +  row.get(GenericBulkHeaders.ROW_NUMBER) + Constant.PERIOD);


      boolean resultRaw = false, resultImage = false, resultAttribute = false, resultPickup = false, resultVariant =
          false, resultCategory = false, resultProtectedBrand = false, resultBpBopisEligibility =
        false;


      log.info("Row : {}, bulkProcessCode : {}", row, bulkProcess.getBulkProcessCode());
      if (StringUtils.isNotBlank(String.valueOf(row.get(GenericBulkHeaders.CN_CATEGORY_ID)))) {
        resultCategory = BulkCreationCommonUtil.validateCategory(
            categoryIdAndDetailMap.get(String.valueOf(row.get(GenericBulkHeaders.CN_CATEGORY_ID))),
            bulkUploadErrorCounter);

        resultVariant =
            this.validateVariantAttributes(row, attributesIdAndPossibleValues, attributesIdAndAttributeResponse,
                bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant, categoryVariantAttributeIdAndNameMap,
                variantValuesPopulated, parentToVariantCreationSet, eanUpcSet);

        ValidateExcelRowsRequest validateExcelRowsRequest =
            getValidateExcelRowsRequest(bulkProcess, bulkUploadErrorCounter, profileResponse, categoryIdAndDetailMap,
                merchantStatusType, instoreSeller, row, commonImageErrorMessage, minimumPrice, isInternationalMerchant,
                pureInstoreProduct, bulkProcessNotes, merchantType);

        resultRaw = BulkCreationCommonUtil.validateExcelRow(row, bulkProcessNotes, bulkUploadErrorCounter,
            isInternationalMerchant, minimumPrice, maxStockLimit, merchantStatusType, productBundlingMaxNumberOfSkus,
            productBundlingEnabled, merchantType, productBundlingEligibleMerchantTypes, commonImageErrorMessage,
            bopisCncRestrictionEnabled, pureInstoreProduct, instoreSeller, validateExcelRowsRequest);


        resultPickup =
            BulkCreationCommonUtil.validateExcelPickupPointsForGeneriCreation(row, pickupPoints, bulkProcessNotes,
                bulkUploadErrorCounter, isInternationalMerchant, ppNameDelimiter,
                accessiblePickupPointCodes);

        resultAttribute =
          BulkCreationCommonUtil.validateExcelAttributes(row, bulkProcess, bulkProcessNotes,
            bulkUploadErrorCounter, isInternationalMerchant, categoryIdAndDetailMap,
            attributesIdAndPossibleValues, fetchBrandIgnoreCaseInCreation);

        resultProtectedBrand =
            protectedBrandValidationService.validateProtectedBrand(row, bulkProcess, protectedBrandNameCodeMap);

        if (!resultProtectedBrand) {
          StringBuilder validationErrorMessage = new StringBuilder();
          String resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.NOT_AUTHORISED_TO_CREATE_PRODUCT_UNDER_THIS_BRAND_EN,
            BulkProcessValidationErrorMessages.NOT_AUTHORISED_TO_CREATE_PRODUCT_UNDER_THIS_BRAND_IN);
          bulkUploadErrorCounter.incrementFeature();
          BulkCreationCommonUtil
            .addValidationErrorMessage(bulkUploadErrorCounter.getFeature(), validationErrorMessage,
              errorMessage("Brand " + StringUtils.SPACE, resultErrorMessage,
                StringUtils.EMPTY));
          bulkProcessNotes.setNotes(resultErrorMessage);
        }

        if (resultRaw) {
          resultBpBopisEligibility =
            BulkCreationCommonUtil.checkBpBopisEligibility(row, bulkProcessNotes,
              bulkUploadErrorCounter, bpBopisRestrictionEnabled, profileResponse, isInternationalMerchant,
                categoryIdAndDetailMap.get(String.valueOf(row.get(GenericBulkHeaders.CN_CATEGORY_ID))),
                bopisCategoryRestrictionEnabled, bopisCategoryValidationForSellerTypes,
                bulkProcess.getPrimaryIdentifier());
        }

      } else {
        bulkProcessNotes.setNotes(isInternationalMerchant ?
            errorMessage(StringUtils.EMPTY, BulkProcessValidationErrorMessages.CATEGORY_INVALID_EN, StringUtils.EMPTY) :
            errorMessage(StringUtils.EMPTY, BulkProcessValidationErrorMessages.CATEGORY_INVALID, StringUtils.EMPTY)
                .concat(Constant.PERIOD));
      }
      if (BulkCreationCommonUtil.evaluateProcessorConditions(resultRaw, resultAttribute, resultPickup, resultVariant,
          resultCategory, resultProtectedBrand, resultBpBopisEligibility)) {
        validRows.add(row);
      } else {
        //adding error message to each row
        row.put(GenericBulkHeaders.BULK_PROCESS_NOTES, bulkProcessNotes.getNotes());
        invalidRows.add(row);
        bulkUploadErrorCounter.incrementInputErrorCount();
        bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
      }
    }
    validRows = BulkCreationCommonUtil.adjustValidAndInvalidRowsBasedOnParentId(bulkProcess, validRows, invalidRows,
        validRowNumbers, isInternationalMerchant);
    return validRows;
  }

  private ValidateExcelRowsRequest getValidateExcelRowsRequest(BulkProcess bulkProcess,
      BulkUploadErrorCounter bulkUploadErrorCounter, ProfileResponse profileResponse,
      Map<String, CategoryDetailAndShippingResponse> categoryIdAndDetailMap, MerchantStatusType merchantStatusType,
      boolean instoreSeller, Map<String, Object> row, String commonImageErrorMessage, int minimumPrice,
      boolean isInternationalMerchant, boolean pureInstoreProduct, BulkProcessNotes bulkProcessNotes,
      String merchantType) {
    ValidateExcelRowsRequest validateExcelRowsRequest =
        CommonUtils.getValidateExcelRowsRequest(bulkProcess, bulkUploadErrorCounter, maxStockLimit, merchantStatusType,
            commonImageErrorMessage, minimumPrice, isInternationalMerchant, instoreSeller, pureInstoreProduct, row,
            bulkProcessNotes, merchantType);
    validateExcelRowsRequest.setProductBundlingMaxNumberOfSkus(productBundlingMaxNumberOfSkus);
    validateExcelRowsRequest.setProductBundlingEnabled(productBundlingEnabled);
    validateExcelRowsRequest.setProductBundlingEligibleMerchantTypes(productBundlingEligibleMerchantTypes);
    validateExcelRowsRequest.setBopisCncRestrictionEnabled(bopisCncRestrictionEnabled);
    validateExcelRowsRequest.setOverrideEmptyProductTypeBulkCreation(overrideEmptyProductTypeBulkCreation);
    validateExcelRowsRequest.setBopisCategoryValidationForSellerTypes(bopisCategoryValidationForSellerTypes);
    validateExcelRowsRequest.setSellerBopisEligible(
        Objects.nonNull(profileResponse.getBopisFlag()) ? profileResponse.getBopisFlag() : true);
    validateExcelRowsRequest.setCategoryBopisEligible(
        categoryIdAndDetailMap.get(String.valueOf(row.get(GenericBulkHeaders.CN_CATEGORY_ID))).isBopisEligible());
    return validateExcelRowsRequest;
  }

  private void overrideIfSinglePickupPoint(ProfileResponse profileResponse, Map<String, Object> row,
    List<PickupPointResponse> pickupPointResponseList,
    Map<String, PickupPointResponse> pickupPointPickupPointResponse) throws ApplicationException {
    String pickupPointCode;
    if (overridePickupPointInCreation && CollectionUtils.isEmpty(pickupPointResponseList)) {
      Page<PickupPointResponse> pickupPointResponses =
        pickupPointService.getSinglePickupPointSummaryFilter(0, 1,
          PickupPointFilterRequest.builder()
            .businessPartnerCode(profileResponse.getBusinessPartnerCode()).build());
      if (pickupPointResponses.getTotalElements() == Constant.ONE) {
        List<PickupPointResponse> content = pickupPointResponses.getContent();
        pickupPointResponseList.addAll(content);
        pickupPointCode = content.getFirst().getCode();
        // Update the row object with the overridden pickup point code
        row.put(GenericBulkHeaders.PICKUP_POINT, pickupPointCode);
        row.put(GenericBulkHeaders.PICKUP_POINT_EN, pickupPointCode);
        log.info("Overriding pp code: {} for seller {}", pickupPointCode,
          profileResponse.getBusinessPartnerCode());
        pickupPointPickupPointResponse.put(pickupPointCode, content.getFirst());
      }
    }
  }

  private void checkAndResetCncFlag(List<Map<String, Object>> rows, ProfileResponse profileResponse,
      Map<String, PickupPointResponse> pickupPointPickupPointResponse) throws ApplicationException {
    if (resetCncFLagForNonCncPP) {
      for (Map<String, Object> row : rows) {
        String pickupPointCode = String.valueOf(
            BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(row, GenericBulkHeaders.PICKUP_POINT,
                GenericBulkHeaders.PICKUP_POINT_EN));
        pickupPointCode =
            BulkCreationCommonUtil.getPickupPointCodeFromPickupPointCodeAndName(ppNameDelimiter, pickupPointCode);
        List<PickupPointResponse> pickupPointResponseList;
        if (pickupPointPickupPointResponse.containsKey(pickupPointCode)) {
          pickupPointResponseList = Collections.singletonList(pickupPointPickupPointResponse.get(pickupPointCode));
        } else {
          pickupPointResponseList = this.pickupPointService.getPickupPointSummaryFilter(0,
              PickupPointFilterRequest.builder().businessPartnerCode(profileResponse.getBusinessPartnerCode())
                  .codes(new HashSet<>(Collections.singletonList(pickupPointCode))).build());
          if (CollectionUtils.isNotEmpty(pickupPointResponseList)) {
            pickupPointPickupPointResponse.put(pickupPointCode, pickupPointResponseList.get(0));
          }
        }
        BulkCreationCommonUtil.checkIfPPCodeIsCncActivated(resetCncFLagForNonCncPP, row, pickupPointResponseList, true);
      }
    }
  }

  private Map<String, CategoryDetailAndShippingResponse> getCategoryDetailResponseByCategoryId(
      Collection<String> categoryIds) {
    return categoryIds.stream().distinct().filter(StringUtils::isNotBlank)
        .map(categoryId -> pcbOutboundService.getCategoryInfoByCategoryId(categoryId))
        .collect(Collectors.toMap(CategoryDetailAndShippingResponse::getId, Function.identity()));
  }

  private Map<String, AttributeResponse> getAttributeInformation(String storeId,
      Map<String, CategoryDetailAndShippingResponse> categoryIdAndDetailMap, Map<String, String> attributeNameAndIdMap,
      String brandInputValue) throws Exception {
    Map<String, AttributeResponse> attributeResponseMap = new HashMap<>();
    Set<String> attributeIdSet =
        categoryIdAndDetailMap.values().stream().map(CategoryDetailAndShippingResponse::getCategoryAttributes)
            .flatMap(Collection::stream)
            .filter(categoryAttributeResponse -> !categoryAttributeResponse.isMarkForDelete())
            .map(CategoryAttributeResponse::getAttribute)
            .filter(attributeResponse -> !Constant.BRAND.equals(attributeResponse.getName()))
            .map(AttributeResponse::getId).collect(Collectors.toSet());
    String brandAttributeId =
        categoryIdAndDetailMap.values().stream().map(CategoryDetailAndShippingResponse::getCategoryAttributes)
            .flatMap(Collection::stream)
            .filter(categoryAttributeResponse -> !categoryAttributeResponse.isMarkForDelete())
            .map(CategoryAttributeResponse::getAttribute)
            .filter(attributeResponse -> Constant.BRAND.equals(attributeResponse.getName()))
            .map(AttributeResponse::getId).findFirst().orElse(StringUtils.EMPTY);

    for (String attributeId : attributeIdSet) {
      AttributeResponse response = attributeRepository.findOne(storeId, attributeId);
      attributeResponseMap.putIfAbsent(attributeId, response);
      attributeNameAndIdMap.putIfAbsent(response.getName(), attributeId);
    }
    if (StringUtils.isNotBlank(brandAttributeId) && StringUtils.isNotBlank(brandInputValue)) {
      AttributeResponse response;
      if (fetchBrandIgnoreCaseInCreation) {
        response = pcbOutboundService.getAttributeDetailByIdIgnoreCase(storeId, brandAttributeId,
          brandInputValue);
      } else {
        response =
          pcbOutboundService.getAttributeDetailById(storeId, brandAttributeId, brandInputValue);
      }
      attributeResponseMap.putIfAbsent(brandAttributeId, response);
      attributeNameAndIdMap.putIfAbsent(response.getName(), brandAttributeId);
    }
    return attributeResponseMap;
  }

  private boolean validateVariantAttributes(Map<String, Object> raw, Map<String, List<String>> attributesValues,
      Map<String, AttributeResponse> attributes, BulkProcessNotes bulkProcessNotes,
      BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant,
      Map<String, Map<String, String>> categoryVariantAttributeIdAndNameMap, Map<String, Boolean> variantValuesPopulated,
      Set<String> parentToVariantCreationSet, Set<String> eanUpcSet) throws Exception {
    boolean valid = true;
    StringBuilder validationErrorMessage = new StringBuilder();
    String columnRowInformation = String.valueOf(raw.get(GenericBulkHeaders.ROW_NUMBER));
    Map<String, String> categoryDefiningAttributes =
        categoryVariantAttributeIdAndNameMap.get(raw.get(GenericBulkHeaders.CN_CATEGORY_ID));
    String allAttribute = StringUtils.EMPTY;

    Map<String, Object> modifiedRow = BulkCreationCommonUtil.removeMandatoryCharactersFromHeaders(raw);

    for (Map.Entry<String, String> definingAttributeColumn : categoryDefiningAttributes.entrySet()) {
      AttributeResponse attribute = attributes.get(definingAttributeColumn.getKey());
      String attributeName = attribute.getName();
      List<String> attributeValues = attributesValues.get(attribute.getId());
      String attributeValue = String.valueOf(modifiedRow.get(definingAttributeColumn.getValue()));
      allAttribute = allAttribute.concat(attributeValue);

      if (BulkCreationCommonUtil.isDefiningOrVariantCreation(attribute) && StringUtils.isBlank(attributeValue)
          && attribute.isMandatory()) {
        valid = false;
        log.error("{} Row : {} . Error msg - {}.", bulkProcessNotes.getBulkProcessCode(), columnRowInformation,
            BulkProcessValidationErrorMessages.INVALID_WARNA_VALUE);
        BulkCreationCommonUtil.addValidationErrorMessage(bulkUploadErrorCounter.getVariation(), validationErrorMessage,
            isInternationalMerchant ?
                errorMessage(
                    BulkCreationCommonUtil.getAttributeName(isInternationalMerchant, attribute) + StringUtils.SPACE,
                    BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK_EN, StringUtils.EMPTY) :
                errorMessage(
                    BulkCreationCommonUtil.getAttributeName(isInternationalMerchant, attribute) + StringUtils.SPACE,
                    BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK, StringUtils.EMPTY));
        continue;
      }

      if (MapUtils.isNotEmpty(attributesValues)) {
        if (((Constant.WARNA.equalsIgnoreCase(attributeName) || Constant.COLOR.equalsIgnoreCase(attributeName))
            && !BulkCreationCommonUtil.isWarnaAndDescriptive(attribute, attributeName)) && (!attributeValues
            .contains(attributeValue))) {
          valid = validateWarnaAndDescriptive(attributeValue, isInternationalMerchant, columnRowInformation,
              attributeValues, attribute, bulkProcessNotes.getBulkProcessCode(), bulkUploadErrorCounter,
              validationErrorMessage, valid);
        }
        if (BulkCreationCommonUtil.isDefiningOrVariantCreation(attribute) && !AttributeType.DESCRIPTIVE_ATTRIBUTE.name()
            .equals(attribute.getAttributeType())) {
          valid = BulkCreationCommonUtil
              .validateDefiningOrVariantCreation(modifiedRow, attributeValue, isInternationalMerchant, columnRowInformation,
                  attributeValues, attribute, bulkProcessNotes.getBulkProcessCode(), bulkUploadErrorCounter,
                  validationErrorMessage, attributeName, valid, sizeChartValueTypeDelimiter);
        }
      } else {
        bulkUploadErrorCounter.incrementVariation();
        BulkCreationCommonUtil.addValidationErrorMessage(bulkUploadErrorCounter.getVariation(), validationErrorMessage,
            isInternationalMerchant ?
                errorMessage(StringUtils.EMPTY, BulkProcessValidationErrorMessages.CATEGORY_INVALID_EN,
                    StringUtils.EMPTY) :
                errorMessage(StringUtils.EMPTY, BulkProcessValidationErrorMessages.CATEGORY_INVALID,
                    StringUtils.EMPTY));
        valid = false;
        log.error("{} Row : {} . Error msg - {} ", bulkProcessNotes.getBulkProcessCode(), columnRowInformation,
            BulkProcessValidationErrorMessages.CATEGORY_INVALID_EN);
      }


      if (valid) {
        // Add additional check for optional variant creation attributes
        // If variant creation attribute is filled for one item, then it has to be filled for all
        if (!variantValuesPopulated.containsKey(definingAttributeColumn.getValue())) {
          variantValuesPopulated.putIfAbsent(definingAttributeColumn.getValue(),
              StringUtils.isNotBlank(String.valueOf(modifiedRow.get(definingAttributeColumn.getValue()))));
        } else {
          Boolean valueFilled = variantValuesPopulated.get(definingAttributeColumn.getValue());
          // Validation failure when defining attribute value of few items belonging to the parent is filled,
          // and few are left empty
          if (!valueFilled.equals(
              StringUtils.isNotBlank(String.valueOf(modifiedRow.get(definingAttributeColumn.getValue()))))) {
            valid = false;
            log.error("{} Row : {} . Error msg - {}. Attribute Name - {}.", bulkProcessNotes.getBulkProcessCode(),
                columnRowInformation, BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID, attributeName);
            validationErrorMessage.append(isInternationalMerchant ?
                    errorMessage(
                        BulkCreationCommonUtil.getAttributeName(isInternationalMerchant, attribute) + StringUtils.SPACE,
                        BulkProcessValidationErrorMessages.OPTIONAL_ATTRIBUTE_VALUE_EN, StringUtils.EMPTY) :
                    errorMessage(
                        BulkCreationCommonUtil.getAttributeName(isInternationalMerchant, attribute) + StringUtils.SPACE,
                        BulkProcessValidationErrorMessages.OPTIONAL_ATTRIBUTE_VALUE, StringUtils.EMPTY))
                .append(Constant.PERIOD);
          }
        }
      }
    }

    if (variantAttributeSwitch) {
      if (parentToVariantCreationSet.contains(allAttribute)) {
        valid = false;
        log.error("{} Row : {} . Error msg - {}. ", bulkProcessNotes.getBulkProcessCode(),
          columnRowInformation,
          BulkProcessValidationErrorMessages.DUPLICATE_VARIANT_CREATING_ATTRIBUTE);
        validationErrorMessage.append(isInternationalMerchant ?
          errorMessage(StringUtils.EMPTY,
            BulkProcessValidationErrorMessages.DUPLICATE_VARIANT_CREATING_ATTRIBUTE_EN,
            StringUtils.EMPTY) :
          errorMessage(StringUtils.EMPTY,
            BulkProcessValidationErrorMessages.DUPLICATE_VARIANT_CREATING_ATTRIBUTE,
            StringUtils.EMPTY)).append(Constant.PERIOD);
      }
      parentToVariantCreationSet.add(allAttribute);
    }
    valid = BulkCreationCommonUtil
        .validateEANValue(modifiedRow, bulkUploadErrorCounter, isInternationalMerchant, valid, validationErrorMessage,
            columnRowInformation, bulkProcessNotes.getBulkProcessCode(), eanUpcSet, eanUpcValidLength);
    if (!valid) {
      bulkProcessNotes.setNotes(validationErrorMessage.toString());
    }
    return valid;
  }

  private boolean validateWarnaAndDescriptive(String attributeValue, boolean isInternationalMerchant,
      String columnRowInformation, List<String> attributeValues, AttributeResponse attribute, String blpCode,
      BulkUploadErrorCounter bulkUploadErrorCounter, StringBuilder validationErrorMessage, boolean valid)
      throws Exception {
    if (StringUtils.isBlank(attributeValue) && attribute.isMandatory()) {
      valid = false;
      BulkCreationCommonUtil.addValidationErrorMessage(bulkUploadErrorCounter.getVariation(), validationErrorMessage,
          isInternationalMerchant ?
              errorMessage(
                  BulkCreationCommonUtil.getAttributeName(isInternationalMerchant, attribute) + StringUtils.SPACE,
                  BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK_EN, StringUtils.EMPTY) :
              errorMessage(
                  BulkCreationCommonUtil.getAttributeName(isInternationalMerchant, attribute) + StringUtils.SPACE,
                  BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK, StringUtils.EMPTY));
      log.error("{} Row : {} . Error msg - {}.", blpCode, (Integer.parseInt(columnRowInformation) + 1),
          BulkProcessValidationErrorMessages.INVALID_WARNA_VALUE);
    } else {
      if (StringUtils.isNotBlank(attributeValue)) {
        AttributeValueResponse response =
            attributeRepository.addNewAttribute(blpCode, attributeValue, attribute.getAttributeCode());
        if (Objects.nonNull(response)) {
          attributeValues.add(response.getValue());
        }
      }
    }
    return valid;
  }

  @Override
  public int generateBulkProcessDataAndImage(BulkProcess bulkProcess, List<List<Object>> userInputRows,
      MerchantStatusType merchantStatusType, String merchantType, List<Object> excelBahasaHeaderList,
      List<Object> excelEnglishHeaderList, List<Integer> failedExcelRows, String accessiblePickupPoints,
      boolean instoreEligible, Map<String, String> args) throws Exception {
    int maxRowProcessingSize = Integer.parseInt(systemParameterConfigService
      .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE)
      .getValue());
    boolean convertedProductCreationUpload = BulkCreationCommonUtil.isConvertedProductCreationUpload(args);
    boolean priorityQueueEnabled = Boolean.parseBoolean(
        systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED).getValue());
    int trustedSellerMaxRowSize = 0;
    int regularSellerMaxRowSize = 0;
    int regularSellerMinRowSize = 0;
    if (!convertedProductCreationUpload) {
      trustedSellerMaxRowSize = Integer.parseInt(
          systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
              SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE).getValue());
      regularSellerMaxRowSize = Integer.parseInt(
          systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
              SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE).getValue());
      regularSellerMinRowSize = Integer.parseInt(
          systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
              SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE).getValue());
    }
    log.info("Inserting data and image rows for bulk process code : {} ", bulkProcess.getBulkProcessCode());
    boolean isInternationalMerchant = bulkProcess.getInternationalMerchant();
    List<BulkProcessData> requestData = new ArrayList<>();
    Set<String> imageURLs = new HashSet<>();
    int rowNumber = 1;
    List<String> headers = null;
    headers = getHeaderList(merchantStatusType, isInternationalMerchant, merchantType, instoreEligible);
    if (genericFileHeaderValidationEn) {
      validateExcelHeaders(merchantType, excelBahasaHeaderList, excelEnglishHeaderList, bulkProcess,
          merchantStatusType, instoreEligible);
    }

    Set<String> distinctParent = new HashSet<>();
    Set<String> failedParentColumns = extractFailedParentColumnsByMergedCells(userInputRows, failedExcelRows, headers);
    for (List<Object> input : userInputRows) {
      BulkProcessData bulkProcessData = new BulkProcessData();
      bulkProcessData.setBulkProcessId(bulkProcess.getId());
      bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessData.setStoreId(bulkProcess.getStoreId());
      bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
      bulkProcessData.setRequestId(bulkProcess.getRequestId());
      bulkProcessData.setNotes(accessiblePickupPoints);
      Map<String, Object> dataMap = new LinkedHashMap<>();
      int index = 0;
      for (String header : headers) {
        if (GenericBulkHeaders.PARENT.equalsIgnoreCase(header)) {
          String parent = String.valueOf(input.get(index));
          validateAndFailMergedCellRows(rowNumber, parent, failedExcelRows, headers, input, failedParentColumns,
              bulkProcess, bulkProcessData);
          if (StringUtils.isBlank(parent)) {
            parent = bulkProcess.getBulkProcessCode() + rowNumber;
          }
          dataMap.put(header, parent);
          bulkProcessData.setParentProduct(parent);
          distinctParent.add(parent);
          index++;
          continue;
        }
        if (GenericBulkHeaders.IMAGE_PREFIX_SET.stream().anyMatch(header::startsWith)) {
          String image = String.valueOf(input.get(index));
          if (image.startsWith(Constant.HTTPS_PREFIX) || image.startsWith(Constant.HTTP_PREFIX)) {
            imageURLs.add(image);
          }
        }
        dataMap.put(header, input.get(index));
        index++;
      }
      bulkProcessData.setRowNumber(rowNumber);
      rowNumber = rowNumber + 1;
      bulkProcessData.setBulkRequestData(objectMapper.writeValueAsString(dataMap));
      requestData.add(bulkProcessData);
    }
    bulkProcessDataService.saveBulkProcessData(requestData);

    List<BulkProcessImage> bulkProcessImageList = new ArrayList<>();
    int index = 1;
    for (String url : imageURLs) {
      BulkProcessImage bulkProcessImage = new BulkProcessImage();
      bulkProcessImage.setBulkProcessId(bulkProcess.getId());
      bulkProcessImage.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessImage.setCompleted(false);
      bulkProcessImage.setImageURL(url);
      bulkProcessImage.setStoreId(bulkProcess.getStoreId());
      bulkProcessImage.setSequence(index);
      index = index + 1;
      bulkProcessImageList.add(bulkProcessImage);
    }
    bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    if (!convertedProductCreationUpload) {
      bulkProcess.setBulkProcessType(BulkCreationCommonUtil.getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(
          bulkProcess.getBulkProcessType(), userInputRows.size(), trustedSellerMaxRowSize, regularSellerMinRowSize,
          regularSellerMaxRowSize, priorityQueueEnabled));
    }

    if (!org.springframework.util.CollectionUtils.isEmpty(bulkProcessImageList)) {
      bulkProcessImageService.saveBulkProcessImage(bulkProcessImageList);
      List<String> imageURLsList = new ArrayList<>(imageURLs);
      int imageDownloadBatchSize = Integer.parseInt(systemParameterConfigService
          .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.IMAGE_DOWNLOAD_BATCH_SIZE).getValue());
      List<List<String>> imageURLsSublist = Lists.partition(imageURLsList, imageDownloadBatchSize);
      for (List<String> subList : imageURLsSublist) {
        bulkProcessService.publishBulkImageDownloadEventModel(bulkProcess.getBulkProcessCode(),
            bulkProcess.getBulkProcessType(), subList, priorityQueueEnabled);
      }
      bulkProcess.setStatus(BulkCreationCommonUtil.getImageDownloadStatusByPriority(bulkProcess.getBulkProcessType(), priorityQueueEnabled));
    }
    bulkProcess.setTotalCount(distinctParent.size());
    if (distinctParent.size() > maxRowProcessingSize) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        isInternationalMerchant ?
          (ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_EN) :
          (ProductUpdateErrorMessages.MAXIMUM_ROW_ERROR_IN) + maxRowProcessingSize);
    }
    bulkProcessService.saveOperation(bulkProcess);
    return maxRowProcessingSize;
  }

  private static Set<String> extractFailedParentColumnsByMergedCells(List<List<Object>> userInputRows,
      List<Integer> failedExcelRows, List<String> headers) {
    //Fetch all the unique parent columns for all the rows that are failed because of cells merged,
    // to fail for their corresponding items
    Set<String> failedParentColumns = new HashSet<>();
    Set<Integer> failedRowsSet = new HashSet<>(failedExcelRows);
    IntStream.range(0, userInputRows.size()).filter(row -> failedRowsSet.contains(row + 1)).forEach(row -> {
      List<Object> input = userInputRows.get(row);
      IntStream.range(0, headers.size()).filter(index -> GenericBulkHeaders.PARENT.equalsIgnoreCase(headers.get(index)))
          .mapToObj(index -> String.valueOf(input.get(index))).filter(StringUtils::isNotBlank)
          .forEach(failedParentColumns::add);
    });
    return failedParentColumns;
  }

  public void validateAndFailMergedCellRows(int rowNumber, String parent, List<Integer> failedExcelRows,
      List<String> headers, List<Object> input, Set<String> failedParentColumns, BulkProcess bulkProcess,
      BulkProcessData bulkProcessData) {
    // Case : 1 Parent column is empty, fail the row and fill empty values for headers after
    // parent column in data table
    if (StringUtils.isBlank(parent) && failedExcelRows.contains(rowNumber)) {
      setBulkProcessDataErrorMessageAndNotes(bulkProcessData, bulkProcess, rowNumber,
          BulkProcessValidationErrorMessages.MERGED_CELLS_ERROR);
      int emptyCellValues = headers.size() - PARENT_COLUMN_NUMBER;
      input.addAll(Collections.nCopies(emptyCellValues, StringUtils.EMPTY));
    }
    // Case : 2 Parent column is non-empty, fail all the items with same parent column
    if (StringUtils.isNotBlank(parent) && failedParentColumns.contains(parent)) {
      setBulkProcessDataErrorMessageAndNotes(bulkProcessData, bulkProcess, rowNumber,
          BulkProcessValidationErrorMessages.INVALID_ROW_BY_PARENT_ID_FOR_MERGED_CELL);
      HashSet<Object> distinctValues = new HashSet<>(input);
      // Case : 3 Parent column non-empty and cells are merged, gill empty values for headers after
      // parent column in data table
      if (distinctValues.size() < headers.size()) {
        int emptyCellValues = headers.size() - PARENT_COLUMN_NUMBER;
        input.addAll(Collections.nCopies(emptyCellValues, StringUtils.EMPTY));
      }
    }
  }

  private static void setBulkProcessDataErrorMessageAndNotes(BulkProcessData bulkProcessData, BulkProcess bulkProcess,
      int rowNumber, String errorMessage) {
    bulkProcessData.setInputErrorCount(1);
    BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
    bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkProcessNotes.setBulkProcess(bulkProcess);
    bulkProcessNotes.setNotes(Constant.ROW + rowNumber + Constant.PERIOD + errorMessage);
    bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
    bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
    bulkProcessData.setEndDate(new Date());
    bulkProcessData.setErrorMessage(bulkProcessNotes.getNotes());
  }

  private void validateExcelHeaders(String merchantType, List<Object> excelBahasaHeaderList,
      List<Object> excelEnglishHeaderList, BulkProcess bulkProcess, MerchantStatusType merchantStatusType,
      boolean instoreEligible) throws ApplicationException {
    //Fetch valid english headers
    List<String> validEnglishHeaderList =
        new ArrayList<>(getHeaderList(merchantStatusType, true, merchantType, instoreEligible));
    //Fetch valid Bahasa headers
    List<String> validBahasaHeaderList =
        new ArrayList<>(getHeaderList(merchantStatusType, false, merchantType, instoreEligible));

    //Limiting excel header validation by ignoring category hierarchy columns
    validEnglishHeaderList =
        validEnglishHeaderList.subList(HEADER_VALIDATION_START_INDEX, validBahasaHeaderList.size());
    validBahasaHeaderList = validBahasaHeaderList.subList(HEADER_VALIDATION_START_INDEX, validBahasaHeaderList.size());
    excelBahasaHeaderList = excelBahasaHeaderList.subList(HEADER_VALIDATION_START_INDEX, excelBahasaHeaderList.size());
    excelEnglishHeaderList =
        excelEnglishHeaderList.subList(HEADER_VALIDATION_START_INDEX, excelEnglishHeaderList.size());

    int startIndexOfAttributeColumn = validEnglishHeaderList.size() - ATTRIBUTE_COLUMN_NUMBERS;
    int endIndexOfAttributeColumn = validEnglishHeaderList.size();
    //If eligible for bundling, adjust the indices for attributes column
    if (ExcelTemplateUtil.isEligibleForBundleCreation(merchantType, productBundlingEligibleMerchantTypes,
        productBundlingEnabled)) {
      startIndexOfAttributeColumn -= GenericBulkParameters.BUNDLE_COLUMN_NUMBERS;
      endIndexOfAttributeColumn -= GenericBulkParameters.BUNDLE_COLUMN_NUMBERS;
    }

    //Remove attributes column from headers
    ExcelTemplateUtil.removeAttributeColumnsFromHeader(excelBahasaHeaderList, excelEnglishHeaderList,
        validEnglishHeaderList, validBahasaHeaderList, startIndexOfAttributeColumn, endIndexOfAttributeColumn);

    if (ExcelTemplateUtil.isExcelHeaderInvalid(excelBahasaHeaderList, excelEnglishHeaderList, validEnglishHeaderList,
        validBahasaHeaderList)) {
      log.error("Header validation failed for bulk process code: {}" , bulkProcess.getBulkProcessCode());
      throw new ApplicationException(ErrorCategory.INVALID_FORMAT,
          ProductLevel3GenericProcessorServiceBean.EXCEL_HEADER_INVALID);
    }
  }

  private List<String> getHeaderList(MerchantStatusType merchantStatusType, boolean isInternationalMerchant,
      String merchantType, boolean instoreEligible) {
    List<String> headers;
    if (merchantStatusType.getType() == 1) {
      if (isInternationalMerchant) {
        headers = GenericBulkHeaders.HEADER_DATA_LIST_EN_NON_CNC_MERCHANT;
      } else {
        headers = GenericBulkHeaders.HEADER_DATA_LIST_NON_CNC_MERCHANT;
      }
    } else if (merchantStatusType.getType() == 2) {
      if (isInternationalMerchant) {
        headers = GenericBulkHeaders.HEADER_DATA_LIST_EN_CNC_MERCHANT;
      } else {
        headers = GenericBulkHeaders.HEADER_DATA_LIST_CNC_MERCHANT;
      }
    } else if (merchantStatusType.getType() == 3) {
      if (isInternationalMerchant) {
        headers = GenericBulkHeaders.HEADER_DATA_LIST_EN_BFB_NON_CNC_MERCHANT;
      } else {
        headers = GenericBulkHeaders.HEADER_DATA_LIST_ID_BFB_NON_CNC_MERCHANT;
      }
    } else {
      if (isInternationalMerchant) {
        headers = GenericBulkHeaders.HEADER_DATA_LIST_EN_BFB_CNC_MERCHANT;
      } else {
        headers = GenericBulkHeaders.HEADER_DATA_LIST_ID_BFB_CNC_MERCHANT;
      }
    }
    if (instoreEligible) {
      headers = new ArrayList<>(headers);
      headers.add(SELLER_TYPE_INSTORE_COLUMN_INDEX_MAP.get(merchantStatusType.getType()), GenericBulkHeaders.INSTORE);
    }
    if (ExcelTemplateUtil.isEligibleForBundleCreation(merchantType, productBundlingEligibleMerchantTypes,
        productBundlingEnabled)) {
      headers = new ArrayList<>(headers);
      headers.add(GenericBulkHeaders.CHILD_SKU.get(isInternationalMerchant));
      headers.add(GenericBulkHeaders.QUANTITY.get(isInternationalMerchant));
    }
    return headers;
  }
}
