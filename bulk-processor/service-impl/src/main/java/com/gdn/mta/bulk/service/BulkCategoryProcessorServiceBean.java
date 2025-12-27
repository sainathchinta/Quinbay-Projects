package com.gdn.mta.bulk.service;

import static com.gdn.mta.bulk.helper.ProductLevel3ProcessorValidationMessageHelper.errorMessage;
import static com.gdn.mta.bulk.helper.ProductLevel3ProcessorValidationMessageHelper.errorMessageBasedOnMerchant;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
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
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightRequest;
import com.gda.mta.product.dto.generator.GenerateShippingWeightResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.CnExcelHeaderNames;
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
import com.gdn.mta.bulk.models.BulkErrorCategory;
import com.gdn.mta.bulk.models.BulkProcessReport;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.repository.AttributeRepository;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.CategoryRepository;
import com.gdn.mta.bulk.repository.ProductRepository;
import com.gdn.mta.bulk.repository.generator.GeneratorRepository;
import com.gdn.mta.bulk.util.BulkCreationCommonUtil;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.mta.bulk.util.GenericBulkParameters;
import com.gdn.mta.bulk.util.ImageUtil;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.partners.bulk.util.BulkCnCreationHeaderNames;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BulkCategoryProcessorServiceBean implements BulkCategoryProcessorService {

  private static final Set<String>
      IMAGE_PREFIX_SET =
      ImmutableSet.of(CnExcelHeaderNames.FOTO_PREFIX, CnExcelHeaderNames.IMAGE_PREFIX,
          BulkCnCreationHeaderNames.VARIANT_IMAGE_EN, BulkCnCreationHeaderNames.VARIANT_IMAGE_ID);
  private static final Set<String> ALLOWED_IMAGE_PREFIXES =
      ImmutableSet.of(Constant.HTTPS_PREFIX, Constant.HTTP_PREFIX);

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private AttributeRepository attributeRepository;

  @Autowired
  private CategoryRepository categoryRepository;

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
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private BulkProcessDataService bulkProcessDataService;

  @Autowired
  private BulkProcessImageService bulkProcessImageService;

  @Autowired
  private ProtectedBrandValidationService protectedBrandValidationService;

  @Autowired
  private PBPOutboundServiceBean pbpOutboundServiceBean;

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

  @Value("${product.bundling.max.number.of.skus}")
  private int productBundlingMaxNumberOfSkus;

  @Value("${bopis.category.restriction.feature.switch}")
  private boolean bopisCategoryRestrictionEnabled;

  @Value("${bopis.cnc.restriction.feature.switch}")
  private boolean bopisCncRestrictionEnabled;

  @Value("${override.empty.product.type.bulk.creation}")
  private boolean overrideEmptyProductTypeBulkCreation;

  @Value("${system.parameter.bopis.unsupported.merchant.types}")
  private String bopisCategoryValidationForSellerTypes;

  @Value("${product.bundling.enabled}")
  private boolean productBundlingEnabled;

  @Value("${product.bundling.eligible.merchants}")
  private String productBundlingEligibleMerchantTypes;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${size.chart.value.type.delimiter}")
  private String sizeChartValueTypeDelimiter;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  @Value("${reset.cnc.flag.for.non.cnc.pp}")
  private boolean resetCncFLagForNonCncPP;

  @Value("${product.suitability.feature.enabled}")
  private boolean productSuitabilityFeatureEnabled;

  @Value("${allowed.image.extensions}")
  private List<String> allowedImageExtensions;

  @Value("${override.pickup.point.in.creation}")
  private boolean overridePickupPointInCreation;

  @Value("${fetch.brand.ignore.case.in.creation}")
  private boolean fetchBrandIgnoreCaseInCreation;

  @Value("${ean.upc.valid.length}")
  private List<Integer> eanUpcValidLength;

  @Override
  public void processEvent(BulkCreateProductEventModel bulkCreateProductRequest) throws Exception {
    log.info("Bulk process in progress, generic create for process id : {}, product : {}",
        bulkCreateProductRequest.getBulkProcessCode(), bulkCreateProductRequest.getParentProduct());
    BulkProcess bulkProcess = bulkProcessService
        .findByBulkProcessCode(bulkCreateProductRequest.getStoreId(), bulkCreateProductRequest.getBulkProcessCode());
    List<BulkProcessData> bulkProcessDataList = bulkProcessDataService
        .findByBulkProcessCodeAndParentProductAndStatus(bulkCreateProductRequest.getStoreId(), bulkProcess,
            bulkCreateProductRequest.getParentProduct(), BulkProcessData.STATUS_PENDING);
    if (CollectionUtils.isEmpty(bulkProcessDataList)) {
      log.warn("No rows found in pending state for the bulk process code  : {}, product : {}",
          bulkCreateProductRequest.getBulkProcessCode(), bulkCreateProductRequest.getParentProduct());
      return;
    }
    log.info("Start processing for rows : {}", bulkProcessDataList.size());
    List<Map<String, Object>> userInputRows = new ArrayList<>();
    List<Object> excelHeaders = new ArrayList<>();
    Map<Integer, BulkProcessData> updatedBulkDataMap = new HashMap<>();
    Set<String> accessiblePickupPoints = new HashSet<>();
    for (BulkProcessData bulkProcessData : bulkProcessDataList) {
      Map<String, Object> row = setBlpInitialData(bulkProcessData, excelHeaders);
      row = BulkCreationCommonUtil.removeMandatoryCharactersFromHeaders(row);
      userInputRows.add(row);
      if (StringUtils.isNotBlank(bulkProcessData.getNotes())) {
        accessiblePickupPoints.addAll(
            Arrays.stream(bulkProcessData.getNotes().split(Constant.COMMA)).map(String::trim).collect(Collectors.toSet()));
      }
      bulkProcessData.setNotes(null);
      bulkProcessData.setStartDate(new Date());
      updatedBulkDataMap.put((Integer) row.get(BulkCnCreationHeaderNames.ROW_NUMBER), bulkProcessDataService.saveOperation(bulkProcessData));
    }
    try {
      String categoryCode = bulkProcess.getNotes();
      ProfileResponse businessPartner = this.businessPartnerRepository
          .filterByBusinessPartnerCodeV2(bulkCreateProductRequest.getStoreId(),
              bulkCreateProductRequest.getBusinessPartnerCode());
      MerchantStatusType merchantStatusType = BulkCreationCommonUtil.getMerchantType(businessPartner);
      String brandInputValue = String.valueOf(userInputRows.get(0).get(BulkCnCreationHeaderNames.BRAND_HEADER_NAME));
      String cleanBrandValue = brandInputValue.contains(Constant.IN_REVIEW) ?
        brandInputValue.substring(0, brandInputValue.indexOf(Constant.IN_REVIEW_BRAND_SUFFIX)) :
        brandInputValue;
      CategoryDetailResponse category = this.categoryRepository
          .findByStoreIdAndCategoryCodeAndMarkDeleteFalse(bulkProcess.getStoreId(), categoryCode);
      CommonUtils.filterHideFromSellerAttributes(productSuitabilityFeatureEnabled, category);
      Map<String, AttributeResponse> attributes =
          this.getAttributes(bulkProcess.getStoreId(), category, bulkProcess.getInternationalMerchant(), cleanBrandValue);

      List<List<Map<String, Object>>> listOfRows = Arrays.asList(userInputRows);

      Map<String, String> imageUrlAndLocationMap = new HashMap<>();
      BulkUploadErrorCounter bulkUploadErrorCounter = new BulkUploadErrorCounter();
      Map<String, Object> validAndInvalid =
          this.validateAllRows(listOfRows, bulkProcess, category, attributes, bulkUploadErrorCounter,
              businessPartner, imageUrlAndLocationMap, updatedBulkDataMap,
              maxStockLimit, merchantStatusType, accessiblePickupPoints);

      List<BulkProcessData> invalidRows = (List<BulkProcessData>) validAndInvalid.get("invalidRows");
      if (CollectionUtils.isNotEmpty(invalidRows)) {
        for (BulkProcessData bulkProcessData : invalidRows) {
          bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
          bulkProcessData.setEndDate(new Date());
          bulkProcessDataService.saveOperation(bulkProcessData);
          updatedBulkDataMap.remove(bulkProcessData.getRowNumber());
        }

        if (MapUtils.isNotEmpty(updatedBulkDataMap)) {
          for (Map.Entry<Integer, BulkProcessData> entry : updatedBulkDataMap.entrySet()) {
            BulkProcessData bulkProcessData = entry.getValue();
            bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
            bulkProcessData.setEndDate(new Date());
            BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
            bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
            bulkProcessNotes.setBulkProcess(bulkProcess);
            bulkProcessNotes.setNotes(Constant.ROW + bulkProcessData.getRowNumber() + Constant.PERIOD);
            bulkProcessNotes.setNotes(bulkProcess.getInternationalMerchant() ?
                errorMessage(StringUtils.EMPTY, BulkProcessValidationErrorMessages.INVALID_ROW_BY_PARENT_ID_MESSAGE_EN,
                    StringUtils.EMPTY) :
                errorMessage(StringUtils.EMPTY, BulkProcessValidationErrorMessages.INVALID_ROW_BY_PARENT_ID_MESSAGE,
                    StringUtils.EMPTY));
            bulkProcessData.setErrorMessage(bulkProcessNotes.getNotes());
            bulkProcessData.setInputErrorCount(1);
            bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
            bulkProcessDataService.saveOperation(bulkProcessData);
          }
        }

        bulkProcessService.saveOperation(bulkProcess);
        return;
      }

      List<List<Map<String, Object>>> validUserRows = (List<List<Map<String, Object>>>) validAndInvalid.get("groupRaws1");
      BulkCreationCommonUtil.generateProductItemNames(validUserRows, attributes);
      AtomicInteger buyableFlagUpdatedCount = new AtomicInteger(0);
      boolean instoreSeller = CommonUtils.isInstoreEligibleSellerWithSwitch(instoreNewFlowEnabled, businessPartner);
      List<ProductCreationRequest> productCreationRequests =
          BulkCreationCommonUtil.convertGroupRowsToProductCollectionRequestsForCategoryUpload(validUserRows, attributes,
              category, bulkProcess, businessPartner, buyableFlagUpdatedCount, merchantStatusType, ppNameDelimiter,
              cncForWarehouseFeatureSwitch, instoreSeller);

      BulkProcessReport bulkProcessReport = new BulkProcessReport();
      //Looping per product
      for (ProductCreationRequest productCreationRequest : productCreationRequests) {
        initializeShippingWeight(category, productCreationRequest);
        String productCode =
            productRepository.generateProductCode(bulkProcess.getRequestId(), bulkProcess.getCreatedBy()).getValue();
        productCreationRequest.setProductCode(productCode);
        productCreationRequest.setProductCreationType((ProductCreationType.CATEGORY_BULK_UPLOAD));
        try {
          Map<String, String> mappingImageFiles =
              this.generateImageFilename(validUserRows.get(0), productCreationRequest, imageUrlAndLocationMap);
          BulkCreationCommonUtil
              .generateProductBusinessPartnerRequestForCategoryUpload(validUserRows.get(0), category,
                  productCreationRequest, bulkProcess, bulkProcess.getInternationalMerchant());
          this.uploadImageFiles(mappingImageFiles, bulkProcess);
          this.pbpOutboundServiceBean.createNewProduct(bulkProcess.getRequestId(), bulkProcess.getCreatedBy(),
              productCreationRequest);
          bulkProcessReport.setSuccessCount(bulkProcessReport.getSuccessCount() + 1);
          this.trackSubmitWithStock(productCreationRequest, categoryCode);
          this.trackerService.sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT,
              TrackerConstants.CREATE_FLOW1_WEB, TrackerConstants.SUBMIT,
              TrackerConstants.SUCCESS, bulkProcess.getNotes());
          updatedBulkDataMap.values().forEach(bulkProcessData -> {
            bulkProcessData.setStatus(BulkProcessData.STATUS_SUCCESS);
            bulkProcessData.setEndDate(new Date());
            bulkProcessDataService.saveOperation(bulkProcessData);
          });
        } catch (ApplicationException ex) {
          bulkUploadErrorCounter.incrementSystemError();
          log.error(
              "Error occurred while post processing request of productCode : {}, bulkProcessCode() for Bulk Upload: ",
              productCode, bulkProcess.getBulkProcessCode(), ex);
          productCreationRequest.setProductCode(productCode);
          updatedBulkDataMap.values().forEach(bulkProcessData -> {
            BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
            bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
            bulkProcessNotes.setBulkProcess(bulkProcess);
            bulkProcessNotes.setNotes(Constant.ROW + bulkProcessData.getRowNumber() + Constant.PERIOD);
            bulkProcessNotes.setNotes(
                StringUtils.replace(ex.getMessage(), ErrorCategory.UNSPECIFIED.getMessage(), StringUtils.EMPTY)
                    + StringUtils.SPACE + productCreationRequest.getName());
            bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
            bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
            bulkProcessData.setSystemErrorCount(1);
            bulkProcessData.setEndDate(new Date());
            bulkProcessData.setErrorMessage(bulkProcessNotes.getNotes());
            bulkProcessDataService.saveOperation(bulkProcessData);
          });
          trackerService.trackProductCreationFailure(bulkProcess.getBulkProcessCode(), productCreationRequest,
              ex.getMessage());
          log.error("Error for product Code {} and productCreationRequest : {}", productCode, productCreationRequest);
          bulkProcessService.saveOperation(bulkProcess);
        } catch (Exception e) {
          bulkUploadErrorCounter.incrementSystemError();
          log.error(
              "Error occurred while post processing request of productCode : {}, bulkProcessCode() for Bulk Upload: ",
              productCode, bulkProcess.getBulkProcessCode(), e);
          productCreationRequest.setProductCode(productCode);
          updatedBulkDataMap.values().forEach(bulkProcessData -> {
            BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
            bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
            bulkProcessNotes.setBulkProcess(bulkProcess);
            bulkProcessNotes.setNotes(Constant.ROW + bulkProcessData.getRowNumber() + Constant.PERIOD);
            bulkProcessNotes.setNotes(Constant.DESCRIPTION_UPLOADING_FAILED + productCreationRequest.getName());
            bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
            bulkProcessData.setStatus(BulkProcessData.STATUS_FAIL);
            bulkProcessData.setSystemErrorCount(1);
            bulkProcessData.setEndDate(new Date());
            bulkProcessData.setErrorMessage(bulkProcessNotes.getNotes());
            bulkProcessDataService.saveOperation(bulkProcessData);
          });
          trackerService
              .trackProductCreationFailure(bulkProcess.getBulkProcessCode(), productCreationRequest, e.getMessage());
          log.error("Error for product Code {} and productCreationRequest : {}", productCode, productCreationRequest);
          bulkProcessService.saveOperation(bulkProcess);
        }
      }
    } catch (Exception ex) {
      updatedBulkDataMap.values().forEach(bulkProcessData -> {
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
        bulkProcessDataService.saveOperation(bulkProcessData);
      });
      log.error("{} System Error for product : {} ", bulkProcess.getBulkProcessCode(),
          bulkCreateProductRequest.getParentProduct(), ex);
    }
  }

  private MerchantStatusType getMerchantStatus(ProfileResponse businessPartner) {
    if (businessPartner.getCompany().isCncActivated()) {
      return MerchantStatusType.DELIVERY_AND_CNC;
    }
    return MerchantStatusType.PURE_DELIVERY;
  }

  private void uploadImageFiles(Map<String, String> mappingImageFiles, BulkProcess bulkProcess)
    throws ApplicationException {
    for (Map.Entry<String, String> entry : mappingImageFiles.entrySet()) {
      try {
        fileStorageService.uploadImageFilesToSourceLocation(entry, bulkProcess, systemParameter.getMtaImageSource());
      } catch (Exception e) {
        log.error("Error at uploadImageFiles, data not found for : {}, {},{}", bulkProcess, entry, e);
        throw new ApplicationException(ErrorCategory.UNSPECIFIED, e.getMessage());
      }
    }
  }

  private void trackSubmitWithStock(ProductCreationRequest request, String categoryCode) {
    boolean isNotEmptyStock = false;
    for (ProductItemCreationRequest productItem : request.getProductItemRequests()) {
      if (productItem.getStock() > 0) {
        isNotEmptyStock = true;
      }
    }

    if (isNotEmptyStock) {
      this.trackerService
          .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
              TrackerConstants.SUBMIT_WITH_STOCK, TrackerConstants.SUCCESS, categoryCode);
    }
  }

  private void initializeShippingWeight(CategoryDetailResponse category,
      ProductCreationRequest productCollectionRequest) throws Exception {
    GenerateShippingWeightRequest generateShippingWeightRequest = new GenerateShippingWeightRequest();
    GenerateShippingWeightResponse generateShippingWeightResponse;
    generateShippingWeightRequest.setLength(productCollectionRequest.getLength());
    generateShippingWeightRequest.setWidth(productCollectionRequest.getWidth());
    generateShippingWeightRequest.setHeight(productCollectionRequest.getHeight());
    generateShippingWeightRequest.setWeight(productCollectionRequest.getWeight());
    generateShippingWeightRequest.setCategoryCode(category.getCategoryCode());
    if (CommonUtils.isEligibleForShippingWeightGeneration(generateShippingWeightRequest)) {
      generateShippingWeightResponse = this.generatorRepository.generateShippingWeight(generateShippingWeightRequest);
    } else {
      generateShippingWeightResponse = new GenerateShippingWeightResponse(Constant.DEFAULT_SHIPPING_WEIGHT);
    }
    productCollectionRequest.setShippingWeight(generateShippingWeightResponse.getShippingWeight());
  }

  private Map<String, Object> setBlpInitialData(BulkProcessData bulkProcessData, List<Object> excelHeaders)
      throws IOException {
    bulkProcessData.setStatus(BulkProcessData.STATUS_IN_PROGRESS);
    TypeReference<LinkedHashMap<String, Object>> typeRef = new TypeReference<LinkedHashMap<String, Object>>() {
    };
    LinkedHashMap<String, Object> rowDataJson = objectMapper.readValue(bulkProcessData.getBulkRequestData(), typeRef);
    if (CollectionUtils.isEmpty(excelHeaders)) {
      excelHeaders.addAll(rowDataJson.keySet());
    }
    rowDataJson.put(BulkCnCreationHeaderNames.ROW_NUMBER, bulkProcessData.getRowNumber());
    return rowDataJson;
  }

  private Integer getMinimumPrice(String storeId) {
    SystemParameterConfig minimumPrice =
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId, Constant.MINIMUM_PRICE);
    return Integer.parseInt(minimumPrice.getValue());
  }

  private Map<String, Object> validateAllRows(List<List<Map<String, Object>>> listOfRows, BulkProcess bulkProcess,
      CategoryDetailResponse category, Map<String, AttributeResponse> attributes, BulkUploadErrorCounter bulkUploadErrorCounter,
      ProfileResponse businessPartner, Map<String, String> imageUrlAndLocationMap, Map<Integer, BulkProcessData> updatedBulkDataMap, long maxStockLimit,
      MerchantStatusType merchantStatusType, Set<String> accessiblePickupPoints) throws Exception {
    String commonImageErrorMessage;
    Map<String, List<String>> attributesValues = BulkCreationCommonUtil.getAttributesValues(attributes);
    Map<String, Object> validAndInValid = new HashMap<>();
    List<List<Map<String, Object>>> groupRaws1 = new ArrayList<>();
    List<BulkProcessData> invalidRows = new ArrayList<>();
    int minimumPrice = getMinimumPrice(bulkProcess.getStoreId());
    Map<String, String> protectedBrandNameCodeMap =
      protectedBrandValidationService.fetchProtectedBrandNameCodeMap(bulkProcess.getStoreId());
    Set<String> parentToVariantCreationSet = new HashSet<>();
    boolean isInternationalMerchant =
        Optional.ofNullable(businessPartner.getCompany()).orElse(new CompanyDTO()).isInternationalFlag();
    List<Map<String, Object>> rowList = listOfRows.stream().findFirst().orElse(new ArrayList<>());
    commonImageErrorMessage =
        validateExcelImagesForSingleRow(rowList, bulkProcess,
            isInternationalMerchant, imageUrlAndLocationMap, bulkUploadErrorCounter);
    Map<String, PickupPointResponse> pickupPointPickupPointResponse = new HashMap<>();
    checkAndResetCncFlag(listOfRows, bulkProcess, pickupPointPickupPointResponse);
    boolean instoreSeller = CommonUtils.isInstoreEligibleSellerWithSwitch(instoreNewFlowEnabled, businessPartner);
    boolean pureInstoreProduct = BulkCreationCommonUtil.isPureInstoreProduct(rowList, instoreSeller, false);

    for (List<Map<String, Object>> rows : listOfRows) {
      List<Map<String, Object>> validRows = new ArrayList<>();
      Map<String, Boolean> variantValuesPopulated = new HashMap<>();
      Set<String> eanUpcSet = new HashSet<>();
      for (Map<String, Object> row : rows) {
        String pickupPointCode = String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(row,
            BulkCnCreationHeaderNames.PICKUP_POINT_CODE_ID_HEADER,
            BulkCnCreationHeaderNames.PICKUP_POINT_CODE_EN_HEADER));
        pickupPointCode =
            BulkCreationCommonUtil.getPickupPointCodeFromPickupPointCodeAndName(ppNameDelimiter, pickupPointCode);
        List<PickupPointResponse> pickupPointResponseList;
        if (pickupPointPickupPointResponse.containsKey(pickupPointCode)) {
          pickupPointResponseList = Collections.singletonList(pickupPointPickupPointResponse.get(pickupPointCode));
        } else {
          pickupPointResponseList = this.pickupPointService.getPickupPointSummaryFilter(0,
              PickupPointFilterRequest.builder().businessPartnerCode(bulkProcess.getBusinessPartnerCode())
                  .codes(new HashSet<>(Collections.singletonList(pickupPointCode))).build());
        }

        overrideIfSinglePickupPoint(businessPartner, row, pickupPointResponseList, pickupPointPickupPointResponse);
        Map<String, PickupPointResponse> pickupPoints =
            BulkCreationCommonUtil.getPickupPointsV2(pickupPointResponseList);
        int rowNumber = (Integer) row.get(BulkCnCreationHeaderNames.ROW_NUMBER);
        BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
        bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
        bulkProcessNotes.setBulkProcess(bulkProcess);
        bulkProcessNotes.setNotes(Constant.ROW + rowNumber + Constant.PERIOD);
        String merchantType =
            Optional.ofNullable(businessPartner.getCompany()).map(CompanyDTO::getMerchantType).orElse(StringUtils.EMPTY);
        boolean resultRaw, resultAttribute, resultPickup, resultGroupraw, resultVariant,
            resultProtectedBrand;
        boolean resultBpBopisEligibility = false;
        resultVariant =
            this.validateVariantAttributes(row, attributesValues, attributes, bulkProcessNotes, bulkUploadErrorCounter,
                isInternationalMerchant, variantValuesPopulated, parentToVariantCreationSet, eanUpcSet);
        resultGroupraw =
            BulkCreationCommonUtil.validateGroupRowAttributes(row, attributes, bulkProcessNotes, bulkUploadErrorCounter,
                isInternationalMerchant);
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
            Objects.nonNull(businessPartner.getBopisFlag()) ? businessPartner.getBopisFlag() : true);
        validateExcelRowsRequest.setCategoryBopisEligible(category.isBopisEligible());
        resultRaw = BulkCreationCommonUtil.validateExcelRows(validateExcelRowsRequest);
        resultPickup = BulkCreationCommonUtil.validateExcelPickupPoints(row, pickupPoints, bulkProcessNotes,
            bulkUploadErrorCounter, isInternationalMerchant, ppNameDelimiter, accessiblePickupPoints);
        resultAttribute = BulkCreationCommonUtil.validateExcelAttributes(row, attributesValues, category, bulkProcess,
            bulkProcessNotes, bulkUploadErrorCounter, isInternationalMerchant, fetchBrandIgnoreCaseInCreation);
        resultProtectedBrand =
            protectedBrandValidationService.validateProtectedBrandForCn(row, bulkProcess, protectedBrandNameCodeMap);

        if (resultRaw) {
          resultBpBopisEligibility =
              BulkCreationCommonUtil.checkBpBopisEligibility(row, bulkProcessNotes, bulkUploadErrorCounter,
                  bpBopisRestrictionEnabled, businessPartner, isInternationalMerchant, category,
                  bopisCategoryRestrictionEnabled, bopisCategoryValidationForSellerTypes,
                  bulkProcess.getPrimaryIdentifier());
        }

        if (!resultProtectedBrand) {
          StringBuilder validationErrorMessage = new StringBuilder();
          String resultErrorMessage = errorMessageBasedOnMerchant(isInternationalMerchant,
            BulkProcessValidationErrorMessages.NOT_AUTHORISED_TO_CREATE_PRODUCT_UNDER_THIS_BRAND_EN,
            BulkProcessValidationErrorMessages.NOT_AUTHORISED_TO_CREATE_PRODUCT_UNDER_THIS_BRAND_IN);
          bulkUploadErrorCounter.incrementFeature();
          BulkCreationCommonUtil
            .addValidationErrorMessage(bulkUploadErrorCounter.getFeature(), validationErrorMessage,
              errorMessage(BulkCnCreationHeaderNames.BRAND_HEADER_NAME + StringUtils.SPACE, resultErrorMessage,
                StringUtils.EMPTY));
          bulkProcessNotes.setNotes(resultErrorMessage);
        }
        if (BulkCreationCommonUtil.evaluateProcessorConditions(resultRaw, resultAttribute, resultPickup, resultGroupraw,
            resultVariant, resultProtectedBrand, resultBpBopisEligibility)) {
          validRows.add(row);
        } else {
          row.remove((row.size() - 1));
          row.put(BulkCnCreationHeaderNames.BULK_PROCESS_NOTES, bulkProcessNotes.getNotes());
          BulkProcessData bulkProcessData = updatedBulkDataMap.get(rowNumber);
          bulkProcessData.setInputErrorCount(1);
          bulkProcessData.setErrorMessage(bulkProcessNotes.getNotes());
          invalidRows.add(bulkProcessData);
          bulkUploadErrorCounter.incrementInputErrorCount();

          if (bulkProcessNotes.getNotes().length() > 12) {
            bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
          }
        }
      }
      if (CollectionUtils.isNotEmpty(validRows)) {
        groupRaws1.add(validRows);
      }
    }

    validAndInValid.put("groupRaws1", groupRaws1);
    validAndInValid.put("invalidRows", invalidRows);
    return validAndInValid;
  }

  private void overrideIfSinglePickupPoint(ProfileResponse businessPartner, Map<String, Object> row,
    List<PickupPointResponse> pickupPointResponseList,
    Map<String, PickupPointResponse> pickupPointPickupPointResponse) throws ApplicationException {
    String pickupPointCode;
    if (overridePickupPointInCreation && CollectionUtils.isEmpty(pickupPointResponseList)) {
      Page<PickupPointResponse> pickupPointResponses =
        pickupPointService.getSinglePickupPointSummaryFilter(0, 1,
          PickupPointFilterRequest.builder()
            .businessPartnerCode(businessPartner.getBusinessPartnerCode()).build());
      if (pickupPointResponses.getTotalElements() == Constant.ONE) {
        List<PickupPointResponse> content = pickupPointResponses.getContent();
        pickupPointResponseList.addAll(content);
        pickupPointCode = content.getFirst().getCode();
        // Update the row object with the overridden pickup point code
        row.put(BulkCnCreationHeaderNames.PICKUP_POINT_CODE_ID_HEADER, pickupPointCode);
        row.put(BulkCnCreationHeaderNames.PICKUP_POINT_CODE_EN_HEADER, pickupPointCode);
        log.info("Overriding pp code: {} for seller {}", pickupPointCode,
          businessPartner.getBusinessPartnerCode());
        pickupPointPickupPointResponse.put(pickupPointCode, content.getFirst());
      }
    }
  }

  private void checkAndResetCncFlag(List<List<Map<String, Object>>> listOfRows, BulkProcess bulkProcess,
      Map<String, PickupPointResponse> pickupPointPickupPointResponse) throws ApplicationException {
    if (resetCncFLagForNonCncPP) {
      for (List<Map<String, Object>> rows : listOfRows) {
        for (Map<String, Object> row : rows) {
          String pickupPointCode = String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(row,
              BulkCnCreationHeaderNames.PICKUP_POINT_CODE_ID_HEADER,
              BulkCnCreationHeaderNames.PICKUP_POINT_CODE_EN_HEADER));
          pickupPointCode =
              BulkCreationCommonUtil.getPickupPointCodeFromPickupPointCodeAndName(ppNameDelimiter, pickupPointCode);
          List<PickupPointResponse> pickupPointResponseList;
          if (pickupPointPickupPointResponse.containsKey(pickupPointCode)) {
            pickupPointResponseList = Collections.singletonList(pickupPointPickupPointResponse.get(pickupPointCode));
          } else {
            pickupPointResponseList = this.pickupPointService.getPickupPointSummaryFilter(0,
                PickupPointFilterRequest.builder().businessPartnerCode(bulkProcess.getBusinessPartnerCode())
                    .codes(new HashSet<>(Collections.singletonList(pickupPointCode))).build());
            if (CollectionUtils.isNotEmpty(pickupPointResponseList)) {
              pickupPointPickupPointResponse.put(pickupPointCode, pickupPointResponseList.get(0));
            }
          }
          BulkCreationCommonUtil.checkIfPPCodeIsCncActivated(resetCncFLagForNonCncPP, row, pickupPointResponseList,
              false);
        }
      }
    }
  }

  private boolean validateVariantAttributes(Map<String, Object> raw,
      Map<String, List<String>> attributesValues, Map<String, AttributeResponse> attributes,
      BulkProcessNotes bulkProcessNotes, BulkUploadErrorCounter bulkUploadErrorCounter, boolean isInternationalMerchant,
      Map<String, Boolean> variantValuesPopulated, Set<String> parentToVariantCreationSet,
    Set<String> eanUpcSet) throws Exception {

    boolean valid = true;
    StringBuilder validationErrorMessage = new StringBuilder();
    String allAttribute = StringUtils.EMPTY;
    List<AttributeResponse> variantCreationAttribute = BulkCreationCommonUtil.getVariantCreatingAttribute(attributes.values());

    for (AttributeResponse attributeResponse : variantCreationAttribute) {
      allAttribute = allAttribute.concat(String.valueOf(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, attributeResponse.getName(),
              attributeResponse.getNameEnglish())));
      String attributeName = BulkCreationCommonUtil.getAttributeNameBasedOnHeader(attributeResponse.getNameEnglish(), attributeResponse.getName(), isInternationalMerchant);
      List<String> attributeValues = attributesValues.get(attributeName);
      if (BulkCreationCommonUtil.isDescriptiveVariant(attributeResponse) && StringUtils.isEmpty(String.valueOf(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, attributeResponse.getName(),
              attributeResponse.getNameEnglish()))) && attributeResponse.isMandatory()) {
        valid = false;
        log.error("Row : {} . Error msg - {}.", (Integer) raw.get(BulkCnCreationHeaderNames.ROW_NUMBER) + 1,
            BulkProcessValidationErrorMessages.INVALID_WARNA_VALUE);
        validationErrorMessage.append(isInternationalMerchant ?
                errorMessage(attributeName + StringUtils.SPACE,
                    BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK_EN, StringUtils.EMPTY) :
                errorMessage(attributeName + StringUtils.SPACE,
                    BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK, StringUtils.EMPTY))
            .append(Constant.PERIOD);
        continue;
      }

      if (!org.springframework.util.CollectionUtils.isEmpty(attributesValues)) {
        if (((Constant.WARNA.equalsIgnoreCase(attributeName) || Constant.COLOR.equalsIgnoreCase(attributeName))
            && !BulkCreationCommonUtil.isDescriptiveVariant(attributeResponse)) && (!attributeValues.contains(
            String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, attributeResponse.getName(),
                attributeResponse.getNameEnglish()))))) {
          if (StringUtils.isBlank(String.valueOf(
              BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, attributeResponse.getName(),
                  attributeResponse.getNameEnglish())).trim()) && attributeResponse.isMandatory()) {
            valid = false;
            log.error("Row : {} . Error msg - {}.", (Integer) raw.get(BulkCnCreationHeaderNames.ROW_NUMBER) + 1,
                BulkProcessValidationErrorMessages.INVALID_WARNA_VALUE);
            validationErrorMessage.append(isInternationalMerchant ?
                errorMessage(attributeName + StringUtils.SPACE,
                    BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK_EN, StringUtils.EMPTY) :
                errorMessage(attributeName + StringUtils.SPACE,
                    BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK, StringUtils.EMPTY));
          } else {
            if (StringUtils.isNotBlank(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, attributeResponse.getName(),
                    attributeResponse.getNameEnglish())).trim())) {
              AttributeValueResponse response =
                  attributeRepository.addNewAttribute(bulkProcessNotes.getBulkProcessCode(), String.valueOf(
                      BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, attributeResponse.getName(),
                          attributeResponse.getNameEnglish())), attributeResponse.getAttributeCode());
              AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
              if (Objects.nonNull(response)) {
                attributeValues.add(response.getValue());
                allowedAttributeValueResponse.setAllowedAttributeCode(response.getAllowedAttributeCode());
                allowedAttributeValueResponse.setSequence(response.getSequence());
              }
            }
          }
        }

        if (!Constant.COLOUR_FAMILY.equals(attributeName) && !Constant.EAN_UPC.equals(attributeName)
            && !BulkCreationCommonUtil.isDescriptiveVariant(attributeResponse)) {
          String columnRowInformation = String.valueOf(raw.get(BulkCnCreationHeaderNames.ROW_NUMBER));
          if (StringUtils.isBlank(String.valueOf(
              BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, attributeResponse.getName(),
                  attributeResponse.getNameEnglish()))) && attributeResponse.isMandatory()) {
            bulkUploadErrorCounter.incrementVariation();
            BulkCreationCommonUtil.addValidationErrorMessage(bulkUploadErrorCounter.getVariation(),
                validationErrorMessage, isInternationalMerchant ?
                    errorMessage(attributeName + StringUtils.SPACE,
                        BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK_EN, StringUtils.EMPTY) :
                    errorMessage(attributeName + StringUtils.SPACE,
                        BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_MUST_NOT_BE_BLANK, StringUtils.EMPTY));
            valid = false;
          } else if (BulkCreationCommonUtil.validateDefiningAttributeValues(raw, attributeResponse, attributeValues,
              sizeChartValueTypeDelimiter)) {
            bulkUploadErrorCounter.incrementVariation();
            BulkCreationCommonUtil.addValidationErrorMessage(bulkUploadErrorCounter.getVariation(),
                validationErrorMessage, isInternationalMerchant ?
                    errorMessage(attributeName + StringUtils.SPACE,
                        BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID_EN, StringUtils.EMPTY) :
                    errorMessage(attributeName + StringUtils.SPACE,
                        BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID, StringUtils.EMPTY));
            valid = false;
            log.error("Row : {} . Error msg - {}. Attribute Name - {}.", (Integer.parseInt(columnRowInformation) + 1),
                BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID, attributeName);
          }
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
      }
      if (BulkCreationCommonUtil.isAttributeNotFamilyColorNorEanUPC(valid, attributeName, attributeResponse)) {
        // Add additional check for optional variant creation attributes
        // If variant creation attribute is filled for one item, then it has to be filled for all
        if (!variantValuesPopulated.containsKey(
            BulkCreationCommonUtil.getAttributeNameBasedOnHeader(attributeResponse.getNameEnglish(), attributeResponse.getName(), isInternationalMerchant))) {
          variantValuesPopulated.putIfAbsent(
              BulkCreationCommonUtil.getAttributeNameBasedOnHeader(attributeResponse.getNameEnglish(), attributeResponse.getName(), isInternationalMerchant),
              StringUtils.isNotBlank(String.valueOf(
                  BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, attributeResponse.getName(),
                      attributeResponse.getNameEnglish()))));
        } else {
          Boolean valueFilled = variantValuesPopulated.get(BulkCreationCommonUtil.getAttributeNameBasedOnHeader(attributeResponse.getNameEnglish(), attributeResponse.getName(), isInternationalMerchant));
          // Validation failure when defining attribute value of few items belonging to the parent is filled,
          // and few are left empty
          if (!valueFilled.equals(StringUtils.isNotBlank(String.valueOf(
              BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, attributeResponse.getNameEnglish(),
                  attributeResponse.getName()))))) {
            valid = false;
            log.error("Row : {} . Error msg - {}. Attribute Name - {}.",
                (Integer) raw.get(BulkCnCreationHeaderNames.ROW_NUMBER) + 1,
                BulkProcessValidationErrorMessages.ATTRIBUTE_VALUES_INVALID, String.valueOf(attributeName));
            validationErrorMessage.append(isInternationalMerchant ?
                    errorMessage(attributeName + StringUtils.SPACE, BulkProcessValidationErrorMessages.OPTIONAL_ATTRIBUTE_VALUE_EN, StringUtils.EMPTY) :
                    errorMessage(attributeName + StringUtils.SPACE, BulkProcessValidationErrorMessages.OPTIONAL_ATTRIBUTE_VALUE, StringUtils.EMPTY))
                .append(Constant.PERIOD);
          }
        }
      }
      Pair<StringBuilder, Boolean
          > resultPair =
          BulkCreationCommonUtil.validateFamilyColourAttribute(raw, attributeName, attributeResponse, attributesValues,
              isInternationalMerchant);
      valid = valid && resultPair.getRight();
      validationErrorMessage.append(resultPair.getLeft());
    }
    if (variantAttributeSwitch) {
      if (parentToVariantCreationSet.contains(allAttribute)) {
        valid = false;
        log.error("{} Row : {} . Error msg - {}. ", bulkProcessNotes.getBulkProcessCode(),
            (Integer) raw.get(BulkCnCreationHeaderNames.ROW_NUMBER) + 1,
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
        .validateEANValue(raw, bulkUploadErrorCounter, isInternationalMerchant, valid,
          validationErrorMessage, eanUpcSet, eanUpcValidLength);
    if (!valid) {
      bulkProcessNotes.setNotes(validationErrorMessage.toString());
    }
    return valid;
  }

  public Map<String, AttributeResponse> getAttributes(String storeId, CategoryDetailResponse category,
      boolean isInternationalMerchant, String brandInputValue) throws Exception {
    Map<String, AttributeResponse> attributes = new HashMap<String, AttributeResponse>();
    for (CategoryAttributeResponse categoryAttribute : category.getCategoryAttributes()) {
      if (!categoryAttribute.isMarkForDelete()) {
        AttributeResponse attribute = new AttributeResponse();
        if (Constant.BRAND.equals(categoryAttribute.getAttribute().getName()) && StringUtils
          .isNotBlank(brandInputValue)) {
          if (fetchBrandIgnoreCaseInCreation) {
            attribute = pcbOutboundService.getAttributeDetailByIdIgnoreCase(storeId,
              categoryAttribute.getAttribute().getId(), brandInputValue);
          }
          else {
            attribute = pcbOutboundService.getAttributeDetailById(storeId,
              categoryAttribute.getAttribute().getId(), brandInputValue);
          }
        } else {
          attribute = this.attributeRepository.findOne(storeId, categoryAttribute.getAttribute().getId());
        }
        String attributeName = attribute.getName();
        if (isInternationalMerchant && Objects.nonNull(attribute.getNameEnglish())) {
          attributeName = attribute.getNameEnglish();
        }
        attributes.put(attributeName, attribute);
      }
    }
    return attributes;
  }

  @Override
  public void generateBulkProcessDataAndImage(BulkProcess bulkProcess, List<List<Object>> userInputRows,
      List<Object> headers, String accessiblePickupPoints) throws Exception {
    boolean isInternationalMerchant = bulkProcess.getInternationalMerchant();
    int maxRowProcessingSize = Integer.parseInt(systemParameterConfigService
      .findValueByStoreIdAndVariable(Constant.STORE_ID, SystemParameterConfigNames.BULK_GENERIC_UPLOAD_MAXIMUM_SIZE)
      .getValue());
    boolean priorityQueueEnabled = Boolean.parseBoolean(
        systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
            SystemParameterConfigNames.PRIORITY_QUEUE_ENABLED).getValue());
    int trustedSellerMaxRowSize = Integer.parseInt(
        systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
            SystemParameterConfigNames.TRUSTED_SELLER_MAX_ROW_SIZE).getValue());
    int regularSellerMaxRowSize = Integer.parseInt(
        systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
            SystemParameterConfigNames.REGULAR_SELLER_MAX_ROW_SIZE).getValue());
    int regularSellerMinRowSize = Integer.parseInt(
        systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
            SystemParameterConfigNames.REGULAR_SELLER_MIN_ROW_SIZE).getValue());
    log.info("Inserting data and image rows for bulk process code : {}", bulkProcess.getBulkProcessCode());
    List<BulkProcessData> requestData = new ArrayList<>();
    Set<String> imageURLs = new HashSet<>();
    int rowNumber = 1;

    Set<String> distinctParent = new HashSet<>();
    for (List<Object> input : userInputRows) {
      BulkProcessData bulkProcessData = new BulkProcessData();
      bulkProcessData.setBulkProcessId(bulkProcess.getId());
      bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessData.setStoreId(bulkProcess.getStoreId());
      bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
      bulkProcessData.setRequestId(bulkProcess.getRequestId());
      Map<String, Object> dataMap = new LinkedHashMap<>();
      int index = 0;
      for (Object header : headers) {
        String headerValue = String.valueOf(header);
        if (BulkParameters.WAREHOUSE_STOCK_HEADER.equalsIgnoreCase(String.valueOf(header))) {
          index++;
          continue;
        }
        if (CnExcelHeaderNames.PARENT.equalsIgnoreCase(headerValue)) {
          String parent = String.valueOf(input.get(index));
          if (StringUtils.isBlank(parent)) {
            parent = bulkProcess.getBulkProcessCode() + rowNumber;
          }
          dataMap.put(headerValue, parent);
          index++;
          bulkProcessData.setParentProduct(parent);
          distinctParent.add(parent);
          continue;
        }
        if (IMAGE_PREFIX_SET.stream().anyMatch(headerValue::startsWith)) {
          String image = String.valueOf(input.get(index));
          if (image.startsWith(Constant.HTTPS_PREFIX) || image.startsWith(Constant.HTTP_PREFIX)) {
            imageURLs.add(image);
          }
        }
        dataMap.put(headerValue, input.get(index));
        index = index + 1;
      }
      bulkProcessData.setRowNumber(rowNumber);
      rowNumber = rowNumber + 1;
      if (StringUtils.isNotBlank(accessiblePickupPoints)) {
        bulkProcessData.setNotes(accessiblePickupPoints);
      }
      bulkProcessData.setBulkRequestData(objectMapper.writeValueAsString(dataMap));
      requestData.add(bulkProcessData);
    }
    bulkProcessDataService.saveBulkProcessData(requestData);

    List<BulkProcessImage> bulkProcessImageList = new ArrayList<>();
    int index = 1;
    for (String url : imageURLs) {
      BulkProcessImage bulkProcessImage = new BulkProcessImage();
      bulkProcessImage.setStoreId(bulkProcess.getStoreId());
      bulkProcessImage.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessImage.setBulkProcessId(bulkProcess.getId());
      bulkProcessImage.setCompleted(false);
      bulkProcessImage.setImageURL(url);
      bulkProcessImage.setSequence(index);
      index = index + 1;
      bulkProcessImageList.add(bulkProcessImage);
    }

    bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    bulkProcess.setBulkProcessType(
        BulkCreationCommonUtil.getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(bulkProcess.getBulkProcessType(),
            userInputRows.size(), trustedSellerMaxRowSize, regularSellerMinRowSize, regularSellerMaxRowSize, priorityQueueEnabled));

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
      bulkProcess.setStatus(BulkCreationCommonUtil.getImageDownloadStatusByPriority(bulkProcess.getBulkProcessType(),
          priorityQueueEnabled));
    }
    bulkProcess.setTotalCount(distinctParent.size());
    if (distinctParent.size() > maxRowProcessingSize) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        isInternationalMerchant ?
          (ProductUpdateErrorMessages.MAXIMUM_PRODUCT_ERROR_EN) :
          (ProductUpdateErrorMessages.MAXIMUM_PRODUCT_ERROR_IN) + maxRowProcessingSize);
    }
    bulkProcessService.saveOperation(bulkProcess);
  }


  private String validateExcelImagesForSingleRow(List<Map<String, Object>> listOfRows, BulkProcess bulkProcess,
      boolean isInternationalMerchant, Map<String, String> imageUrlAndLocationMap,
      BulkUploadErrorCounter bulkUploadErrorCounter) throws Exception {
    boolean result = true;
    //Group rows contain all rows with same parent column , sort group rows based on row number to validate the
    //first row to process common images and ignore for rest of the rows
    listOfRows.sort(Comparator.comparingInt(row -> (int) row.getOrDefault(BulkCnCreationHeaderNames.ROW_NUMBER, 0)));
    Map<String, Object> firstRow = listOfRows.stream().findFirst().orElse(Collections.emptyMap());
    Map<String, String> imageAndImageUrlReverseMap = new HashMap<>();
    String columnRowInformation = String.valueOf(firstRow.get(BulkCnCreationHeaderNames.ROW_NUMBER));
    List<String> images = new ArrayList<>();
    StringBuilder validationErrorMessage = new StringBuilder();
    List<BulkProcessImage> imageList =
        bulkProcessImageService.findByStoreIdAndBulkProcess(Constant.STORE_ID, bulkProcess);
    Map<String, BulkProcessImage> imageURLMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(imageList)) {
      imageURLMap = imageList.stream().collect(Collectors.toMap(BulkProcessImage::getImageURL, Function.identity()));
    }
    for (int imageNumber = 1; imageNumber <= GenericBulkParameters.NUMBER_OF_IMAGES; imageNumber++) {
      String image = String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(firstRow,
          BulkCnCreationHeaderNames.IMAGE_ID_HEADER_NAME + imageNumber,
          BulkCnCreationHeaderNames.IMAGE_EN_HEADER_NAME + imageNumber));
      if (StringUtils.isNotBlank(image) && !images.contains(image)) {
        if (!image.startsWith(Constant.HTTPS_PREFIX) && !image.startsWith(Constant.HTTP_PREFIX)) {
          images.add(image);
        } else {
          log.info("Uploaded image : {}", image);
          BulkProcessImage bulkProcessImage = imageURLMap.get(image);
          if (Objects.nonNull(bulkProcessImage)) {
            if (StringUtils.isNotBlank(bulkProcessImage.getErrorMessage())) {
              result = false;
              log.error("Bulk process: {}, Row {} - {} : {} ", bulkProcess.getBulkProcessCode(),
                  (Integer.parseInt(columnRowInformation) + 1), BulkErrorCategory.INPUT_ERROR.getDescription(),
                  bulkProcessImage.getErrorMessage());
              validationErrorMessage.append(bulkProcessImage.getErrorMessage()).append(Constant.PERIOD);
              continue;
            }
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
    if (images.isEmpty() && result) {
      result = false;
      BulkCreationCommonUtil.addValidationErrorMessage(bulkUploadErrorCounter.getImage(), validationErrorMessage,
          isInternationalMerchant ?
              errorMessage(StringUtils.EMPTY, BulkProcessValidationErrorMessages.IMAGES_MUST_NOT_BE_BLANK_EN,
                  StringUtils.EMPTY) :
              errorMessage(StringUtils.EMPTY, BulkProcessValidationErrorMessages.IMAGES_MUST_NOT_BE_BLANK,
                  StringUtils.EMPTY));
      log.error("Bulk process: {}, Row {} - {} : {} ", bulkProcess.getBulkProcessCode(),
          (Integer.parseInt(columnRowInformation) + 1), BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.IMAGES_MUST_NOT_BE_BLANK);
    }
    //Validate variant images for all the variants
    for (Map<String, Object> row : listOfRows) {
      String image = String.valueOf(Optional.ofNullable(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(row, BulkCnCreationHeaderNames.VARIANT_IMAGE_EN,
              BulkCnCreationHeaderNames.VARIANT_IMAGE_ID)).orElse(StringUtils.EMPTY));
      if (StringUtils.isNotBlank(image) && !images.contains(image)) {
        if (ALLOWED_IMAGE_PREFIXES.stream().noneMatch(image::startsWith)) {
          images.add(image);
        } else {
          log.info("Uploaded image : {}", image);
          BulkProcessImage bulkProcessImage = imageURLMap.get(image);
          if (Objects.nonNull(bulkProcessImage)) {
            if (StringUtils.isNotBlank(bulkProcessImage.getErrorMessage())) {
              result = false;
              log.error("Bulk process: {}, Row {} - {} : {} ", bulkProcess.getBulkProcessCode(),
                  (Integer.parseInt(columnRowInformation) + 1), BulkErrorCategory.INPUT_ERROR.getDescription(),
                  bulkProcessImage.getErrorMessage());
              validationErrorMessage.append(bulkProcessImage.getErrorMessage()).append(Constant.PERIOD);
              continue;
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
    if (result) {
      result = fileStorageService.downloadAndValidateProductCreationImages(bulkProcess,
        bulkUploadErrorCounter, imageAndImageUrlReverseMap, validationErrorMessage, images,
        columnRowInformation, systemParameter.getImageMaxSize(), isInternationalMerchant);
    }
    if (StringUtils.isBlank(String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(firstRow,
        BulkCnCreationHeaderNames.FIRST_IMAGE_ID_HEADER_NAME, BulkCnCreationHeaderNames.FIRST_IMAGE_EN_HEADER_NAME)))) {
      result = false;
      BulkCreationCommonUtil.addValidationErrorMessage(bulkUploadErrorCounter.getImage(), validationErrorMessage,
          isInternationalMerchant ?
              errorMessage(StringUtils.EMPTY, BulkProcessValidationErrorMessages.FIRST_IMAGE_MUST_NOT_BE_BLANK_EN,
                  StringUtils.EMPTY) :
              errorMessage(StringUtils.EMPTY, BulkProcessValidationErrorMessages.FIRST_IMAGE_MUST_NOT_BE_BLANK,
                  StringUtils.EMPTY));
      log.error("Bulk process: {}, Row {} - {} : {} ", bulkProcess.getBulkProcessCode(),
          (Integer.parseInt(columnRowInformation) + 1), BulkErrorCategory.INPUT_ERROR.getDescription(),
          BulkProcessValidationErrorMessages.FIRST_IMAGE_MUST_NOT_BE_BLANK);
    }
    return validationErrorMessage.toString();
  }

  @Override
  public Map<String, String> generateImageFilename(List<Map<String, Object>> groupRaw,
      ProductCreationRequest productCreationRequest, Map<String, String> imageUrlAndLocationMap) {
    // Generate product and product item image filename
    String autoUploadUrlSubstring = systemParameterConfigService.findValueByStoreIdAndVariable(
        Constant.STORE_ID, Constant.AUTO_UPLOAD_URL_SUBSTRING).getValue();
    Map<String, String> mappingImageFilenames = new LinkedHashMap<>();
    Map<String, String> imageAndImageUrlReverseMap = new HashMap<>();
    //Group rows contain all rows with same parent column , sort group rows based on row number to fetch the
    //first row to process common images and ignore for rest of the rows
    groupRaw.sort(Comparator.comparingInt(row -> (int) row.getOrDefault(BulkCnCreationHeaderNames.ROW_NUMBER, 0)));
    //Process common images for first row only
    Map<String, Object> firstRaw = groupRaw.stream().findFirst().orElse(Collections.emptyMap());
      for (int imageNumber = 1; imageNumber <= GenericBulkParameters.NUMBER_OF_IMAGES; imageNumber++) {
        String imageFilename = String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(firstRaw,
            BulkCnCreationHeaderNames.IMAGE_ID_HEADER_NAME + imageNumber,
            BulkCnCreationHeaderNames.IMAGE_EN_HEADER_NAME + imageNumber));
        if (StringUtils.isNotBlank(imageFilename) && !imageFilename.startsWith(Constant.HTTPS_PREFIX)) {
          mappingImageFilenames.put(imageFilename, null);
        } else if ((StringUtils.isNotBlank(imageFilename)) && (imageUrlAndLocationMap.containsKey(imageFilename))) {
          if (imageFilename.contains(autoUploadUrlSubstring)) {
            productCreationRequest.setProductCreationType(ProductCreationType.AUTO_UPLOAD);
          }
          mappingImageFilenames.put(imageUrlAndLocationMap.get(imageFilename), null);
          imageAndImageUrlReverseMap.putIfAbsent(imageUrlAndLocationMap.get(imageFilename), imageFilename);
        }
      }
      //Populate variant images in mappingImageFilenames map
    for (Map<String, Object> raw : groupRaw) {
      String imageFilename = String.valueOf(Optional.ofNullable(
          BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.VARIANT_IMAGE_EN,
              BulkCnCreationHeaderNames.VARIANT_IMAGE_ID)).orElse(StringUtils.EMPTY));
      if (StringUtils.isNotBlank(imageFilename) && !imageFilename.startsWith(Constant.HTTPS_PREFIX)) {
        mappingImageFilenames.put(imageFilename, null);
      } else if ((StringUtils.isNotBlank(imageFilename)) && (imageUrlAndLocationMap.containsKey(imageFilename))) {
        if (imageFilename.contains(autoUploadUrlSubstring)) {
          productCreationRequest.setProductCreationType(ProductCreationType.AUTO_UPLOAD);
        }
        mappingImageFilenames.put(imageUrlAndLocationMap.getOrDefault(imageFilename, StringUtils.EMPTY), null);
        imageAndImageUrlReverseMap.putIfAbsent(imageUrlAndLocationMap.get(imageFilename), imageFilename);
      }
    }

    StringBuilder validImageFilename = new StringBuilder(productCreationRequest.getProductCode()).append(File.separator)
        .append(BulkCreationCommonUtil.toLowerCaseName(productCreationRequest.getBrand()));
    validImageFilename.append("_");
    validImageFilename.append(BulkCreationCommonUtil.toLowerCaseName(productCreationRequest.getBrand()));
    validImageFilename.append("_full");
    int j = 1;
    for (Map.Entry<String, String> entry : mappingImageFilenames.entrySet()) {
      String k;
      if (j < 9) {
        k = "0" + j;
      } else {
        k = String.valueOf(j);
      }
      String[] splitImageFilename = entry.getKey().split("\\.");
      String imageFiletype = splitImageFilename[splitImageFilename.length - 1];
      if (!allowedImageExtensions.contains(imageFiletype.toLowerCase())) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            BulkProcessValidationErrorMessages.IMAGE_FILE_TYPE_INVALID);
      }
      entry.setValue(validImageFilename.toString() + k + "." + imageFiletype.toLowerCase());
      j++;
    }
    // Generate product image filename
    j = 0;
    for (Map.Entry<String, String> entry : mappingImageFilenames.entrySet()) {
      Image image = new Image();
      image.setLocationPath(entry.getValue());
      image.setMainImages(j == 0);
      image.setSequence(j);
      image.setHashCode(ImageUtil.generateHashcodeByLocationPath(entry.getValue()));
      image.setOriginalImage(Boolean.TRUE);
      image.setStoreId(productCreationRequest.getStoreId());
      if (imageAndImageUrlReverseMap.containsKey(entry.getKey())) {
        image.setUrlPath(imageAndImageUrlReverseMap.get(entry.getKey()));
      }
      productCreationRequest.getImages().add(image);
      j++;
    }
    // Generate product item image filename
    boolean containsUrlImages = false;
    Set<Image> commonImageSet = new HashSet<>();
    Set<String> uniqueCommonImagesSet = new HashSet<>();
    for (int imageNumber = 1; imageNumber <= GenericBulkParameters.NUMBER_OF_IMAGES; imageNumber++) {
      String imageFilename = String.valueOf(BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(firstRaw,
          BulkCnCreationHeaderNames.IMAGE_ID_HEADER_NAME + imageNumber,
          BulkCnCreationHeaderNames.IMAGE_EN_HEADER_NAME + imageNumber));
      containsUrlImages =
          validateAndProcessCommonImages(productCreationRequest, imageUrlAndLocationMap, mappingImageFilenames,
              containsUrlImages, commonImageSet, uniqueCommonImagesSet, imageNumber, imageFilename);
    }
    //Item level image request formation along with common images
    containsUrlImages = prepareProductItemCreationRequest(groupRaw, productCreationRequest, imageUrlAndLocationMap,
        mappingImageFilenames, containsUrlImages, commonImageSet, uniqueCommonImagesSet);
    productCreationRequest.setCommonImages(new ArrayList<>(commonImageSet));
    productCreationRequest.setContainsUrlImage(containsUrlImages);
    return mappingImageFilenames;
  }

  public static boolean validateAndProcessCommonImages(ProductCreationRequest productCreationRequest,
      Map<String, String> imageUrlAndLocationMap, Map<String, String> mappingImageFilenames, boolean containsUrlImages,
      Set<Image> commonImageSet, Set<String> uniqueCommonImagesSet, int imageNumber, String imageFilename) {
    boolean urlImage = false;
    boolean isMainImage = imageNumber == 1;
    int sequence = imageNumber - 1;
    if (imageFilename.startsWith(Constant.HTTPS_PREFIX) && imageUrlAndLocationMap.containsKey(imageFilename)) {
      imageFilename = imageUrlAndLocationMap.get(imageFilename);
      urlImage = true;
    }
    if (StringUtils.isNotBlank(imageFilename) && !uniqueCommonImagesSet.contains(imageFilename)) {
      Image image =
          ImageUtil.getImage(productCreationRequest.getStoreId(), mappingImageFilenames, imageFilename, isMainImage,
              sequence, true);
      if (urlImage) {
        image.setUrlPath(imageFilename);
        containsUrlImages = true;
      }
      commonImageSet.add(image);
      uniqueCommonImagesSet.add(imageFilename);
    }
    return containsUrlImages;
  }

  public static boolean prepareProductItemCreationRequest(List<Map<String, Object>> groupRaw,
      ProductCreationRequest productCreationRequest, Map<String, String> imageUrlAndLocationMap,
      Map<String, String> mappingImageFilenames, boolean containsUrlImages, Set<Image> commonImageSet,
      Set<String> uniqueCommonImagesSet) {
    for (ProductItemCreationRequest productItemRequest : productCreationRequest.getProductItemRequests()) {
      Set<Image> itemImageSet = new HashSet<>();
      for (Map<String, Object> raw : groupRaw) {
        if (String.valueOf(raw.get(BulkCnCreationHeaderNames.GENERATED_ITEM_NAME))
            .equals(productItemRequest.getItemGeneratedName())) {
          String imageFilename = String.valueOf(Optional.ofNullable(
              BulkCreationCommonUtil.getValueBasedOnEnOrIdHeader(raw, BulkCnCreationHeaderNames.VARIANT_IMAGE_EN,
                  BulkCnCreationHeaderNames.VARIANT_IMAGE_ID)).orElse(StringUtils.EMPTY));
          boolean urlImage = false;
          if (imageFilename.startsWith(Constant.HTTPS_PREFIX) && imageUrlAndLocationMap.containsKey(imageFilename)) {
            imageFilename = imageUrlAndLocationMap.get(imageFilename);
            urlImage = true;
          }
          if (StringUtils.isNotBlank(imageFilename) && !uniqueCommonImagesSet.contains(imageFilename)) {
            Image image =
                ImageUtil.getImage(productCreationRequest.getStoreId(), mappingImageFilenames, imageFilename, true, 0,
                    false);
            if (urlImage) {
              image.setUrlPath(imageFilename);
              containsUrlImages = true;
            }
            itemImageSet.add(image);
            //Add common images to itemImageSet
            int sequence = 1;
            ImageUtil.addCommonImagesToProductItemRequest(commonImageSet, itemImageSet, sequence, true);
          } else {
            //if no variant image or already present in common image , add only common images to item request
            int sequence = 0;
            ImageUtil.addCommonImagesToProductItemRequest(commonImageSet, itemImageSet, sequence, false);
          }
          productItemRequest.setImages(new ArrayList<>(itemImageSet));
          break;
        }
      }
    }
    return containsUrlImages;
  }
}
