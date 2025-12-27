package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.mta.domain.event.modal.AddDeleteVariantRetryPublishEventModel;
import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.AddProductToVendorCombinedEventModel;
import com.gdn.mta.domain.event.modal.AddRevisedProductToPDTEvent;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductDistributionTaskRepositoryBean;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.partners.pbp.commons.constants.EditedReviewTypeConstants;
import com.gdn.partners.pbp.outbound.productAnalytics.ProductAnalyticsOutbound;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;
import com.gdn.x.businesspartner.dto.BusinessPartnerCodesRequest;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ImageQcRequestDomainEvent;
import com.gdn.mta.domain.event.modal.ProductMigrationRequestEvent;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductImageQcBacklog;
import com.gdn.mta.product.enums.ImageQcStatus;
import com.gdn.mta.product.enums.MigrationStatus;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ScheduledJobServiceImpl implements ScheduledJobService {

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private ProductService productService;

  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Autowired
  @Lazy
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private ProductSystemParameterService productSystemParameterService;

  @Autowired
  private ProductMigrationWrapperService productMigrationWrapperService;

  @Autowired
  private ProductMigrationService productMigrationService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private ProductImageQcBacklogService productImageQcBacklogService;

  @Autowired
  private ProductPublisherService productPublisherService;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;
  @Autowired
  private ProductAnalyticsOutbound productAnalyticsOutbound;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private ProductDistributionTaskRepositoryBean productDistributionTaskRepositoryBean;

  @Value("${fetch.active.products.size}")
  private int fetchActiveProductsSize;

  @Value("${fetch.pre.live.products.size}")
  private int fetchPreLiveProductsSize;

  @Value("${price.info.vendor.revised.enabled}")
  private boolean priceInfoVendorRevisedEnabled;

  @Value("${price.info.max.variant.limit}")
  private int priceInfoMaxVariantLimit;

  @Value("${price.info.vendor.edited.enabled}")
  private boolean priceInfoVendorEditedEnabled;

  private static final int PAGE_SIZE = 50;
  private static final int FIRST_PAGE = 0;
  private static final String CONTENT = "CONTENT";
  private static final String IMAGE = "IMAGE";
  private static final String NEED_CORRECTION = "NEED_CORRECTION";
  private static final String IN_PROGRESS = "IN_PROGRESS";

  @Trace(dispatcher=true)
  @Async
  @Override
  public void runPostLiveConfigChanges(
      String storeId, String requestId, String username, long dateInMillis) throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    List<ConfigurationStatusResponse> configurationStatusResponseList
        = productOutbound.getReviewConfigurationChanges(dateInMillis);
    if(CollectionUtils.isNotEmpty(configurationStatusResponseList)) {
      Map<String, String> categoryCodeConfigMap = new HashMap<>();
      Map<String, String> businessPartnerCodeConfigMap = new HashMap<>();
      setCategoryAndMerchantConfigMaps(
          configurationStatusResponseList, categoryCodeConfigMap, businessPartnerCodeConfigMap);
      Pageable pageable = PageRequest.of(FIRST_PAGE, PAGE_SIZE);
      Page<ProductCollection> draftProducts;
      do {
        draftProducts = productService.getDraftProducts(GdnMandatoryRequestParameterUtil.getStoreId(), pageable);
        for (ProductCollection productCollection : draftProducts.getContent()) {
          try {
            log.debug("Checking post live eligibility for product : {} ", productCollection.getProductCode());
            if (isProductEligibleForPostLiveChange(categoryCodeConfigMap, businessPartnerCodeConfigMap,
                productCollection)) {
              log.info("Changing product {} to postLive", productCollection.getProductCode());
              productServiceWrapper.updatePostLiveFlagAndSkipScreening(productCollection);
            }
          } catch (Exception e) {
            log.error("Error while running scheduler changes for product : {} ", productCollection.getProductCode());
          }
        }
        pageable = pageable.next();
      } while (draftProducts.hasNext());
    }
    log.info("runPostLiveConfigChanges job is successful");
  }

  @Override
  @Async
  public void migrateProducts(String storeId, List<String> productCodes) {
    setMandatoryParameters();
    int migratedProductCount = 0;
    long startTime = Calendar.getInstance().getTimeInMillis();
    try {
      if (CollectionUtils.isNotEmpty(productCodes)) {
        for (String productCode : productCodes) {
          migratedProductCount = productMigrationWrapperService.migrateProductByProductCode(productCode, false);
        }
      } else {
        migratedProductCount = migrateProducts(storeId);
      }
      long endtime = Calendar.getInstance().getTimeInMillis();
      log.info(
          "Product migration successful. Total number of products migrated : {} and time taken in milliseconds: {}",
          migratedProductCount, endtime - startTime);
    } catch (Exception e) {
      log.error("Error while migrating products.  Total number of products migrated : {} ", migratedProductCount, e);
    }
  }

  private Integer migrateProducts(String storeId) throws Exception {
    int migrationCount = 0;
    int productMigrationBatchSize = Integer.parseInt(productSystemParameterService.findByStoreIdAndVariable(storeId,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATIONS_BATCH_SIZE).getValue());
    int queryLimit = Integer.parseInt(productSystemParameterService.findByStoreIdAndVariable(storeId,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_QUERY_LIMIT).getValue());
    int threads = Integer.parseInt(productSystemParameterService.findByStoreIdAndVariable(storeId,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_THREAD_COUNT).getValue());
    int sleepTime = Integer.parseInt(productSystemParameterService.findByStoreIdAndVariable(storeId,
        com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_SLEEP_TIME_IN_MS).getValue());
    log.info("Migration details productMigrationBatchSize : {}, queryLimit : {}, threads : {}",
        productMigrationBatchSize, queryLimit, threads);
    do {
      List<String> productCodes = productMigrationService.findDistinctProductCodesForMigration(queryLimit);
      List<String> productSkus = new ArrayList<>();
      if (CollectionUtils.isEmpty(productCodes)) {
        productSkus = productMigrationService.findProductForMigrationWithNullProductCode(queryLimit);
      }
      if (CollectionUtils.isEmpty(productSkus) && CollectionUtils.isEmpty(productCodes)) {
        break;
      }

      if (CollectionUtils.isNotEmpty(productCodes)) {
        migrationCount = migrationCount + productCodes.size();
        productMigrationService
            .updateProductMigrationStatusByProductCodes(productCodes, MigrationStatus.PENDING.getMigrationStatus());
      }

      if (CollectionUtils.isNotEmpty(productSkus)) {
        migrationCount = migrationCount + productSkus.size();
        productMigrationService.updateProductMigrationStatusByProductSkus(productSkus, MigrationStatus.PENDING.getMigrationStatus());
      }

      ProductMigrationRequestEvent requestEvent = new ProductMigrationRequestEvent();
      requestEvent.setProductCodes(productCodes);
      requestEvent.setProductSkuList(productSkus);
      requestEvent.setThreadCount(threads);
      kafkaProducer.send(DomainEventName.MIGRATE_PRODUCTS_EVENT, requestEvent);
      Thread.sleep(sleepTime);
    } while (migrationCount < productMigrationBatchSize);
    return migrationCount;
  }

  @Override
  @Async
  public void retryFailedMigratedProducts(String storeId, List<String> productCodes) {
    try {
      setMandatoryParameters();
      if (CollectionUtils.isNotEmpty(productCodes)) {
        productMigrationWrapperService.retryFailedMigratedProductsByProductCodes(productCodes);
      } else {
        int batchSize = Integer.valueOf(productSystemParameterService
            .findByStoreIdAndVariable(storeId, SystemParameterConstants.PRODUCT_MIGRATION_RETRY_BATCH_SIZE).getValue());
        int retryCount = Integer.valueOf(productSystemParameterService
            .findByStoreIdAndVariable(storeId, SystemParameterConstants.PRODUCT_MIGRATION_RETRY_COUNT).getValue());
        productMigrationWrapperService.retryFailedMigratedProducts(batchSize, retryCount);
      }
    } catch (Exception e) {
      log.error("Error while retrying the migrating of products", e);
    }
  }

  @Override
  public void updateMigrationStatus(String storeId) {
    try {
      int minutes = Integer.parseInt(productSystemParameterService.findByStoreIdAndVariable(storeId,
          com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_MIGRATION_BEFORE_MINUTES).getValue());
      Date updatedDate = getEarlierDateByMinutes(minutes);
      log.info("Updating the migration status of the products with updated date before {} minutes", minutes);
      productMigrationService.updateProductMigrationStatus(MigrationStatus.IN_PROGRESS.getMigrationStatus(),
          MigrationStatus.FAILED.getMigrationStatus(), updatedDate);
      productMigrationService
          .updateProductMigrationStatus(MigrationStatus.PENDING.getMigrationStatus(), null, updatedDate);
      log.info("Updated the migration status of the products with updated date before {} minutes", minutes);
    } catch (Exception e) {
      log.error("Error while updating the migration status of products", e);
    }
  }

  private void setCategoryAndMerchantConfigMaps(
      List<ConfigurationStatusResponse> configurationStatusResponseList,
      Map<String, String> categoryCodeConfigMap, Map<String, String> businessPartnerCodeConfigMap) {
    for (ConfigurationStatusResponse configurationStatusResponse : configurationStatusResponseList) {
      if (Objects.nonNull(configurationStatusResponse.getCategoryCode())) {
        categoryCodeConfigMap.put(configurationStatusResponse.getCategoryCode(),
            configurationStatusResponse.getReviewConfig());
      } else if (Objects.nonNull(configurationStatusResponse.getMerchantCode())) {
        businessPartnerCodeConfigMap.put(configurationStatusResponse.getMerchantCode(),
            configurationStatusResponse.getReviewConfig());
      }
    }
  }

  private boolean isProductEligibleForPostLiveChange(
      Map<String, String> categoryCodeConfigMap, Map<String, String> businessPartnerCodeConfigMap,
      ProductCollection productCollection) throws Exception {
    if(!isProductEligibleForConfigChange(categoryCodeConfigMap, businessPartnerCodeConfigMap, productCollection)) {
      return false;
    }
    if (!productServiceWrapper.checkIfProductIsEligibleForScreeningSkip(productCollection)) {
      return false;
    } else if (StringUtils.equals(Constants.POST_LIVE_STATUS,
        businessPartnerCodeConfigMap.get(productCollection.getBusinessPartnerCode()))) {
      return true;
    } else if (StringUtils.isBlank(businessPartnerCodeConfigMap.get(productCollection.getBusinessPartnerCode())) ||
        StringUtils.isBlank(categoryCodeConfigMap.get(productCollection.getCategoryCode()))) {
      return Constants.POST_LIVE_STATUS.equals(getReviewStatus(productCollection));
    }
    return StringUtils.equals(Constants.POST_LIVE_STATUS, categoryCodeConfigMap.get(productCollection.getCategoryCode()))
        && !StringUtils.equals(Constants.PRE_LIVE_FLAG,
        businessPartnerCodeConfigMap.get(productCollection.getBusinessPartnerCode()));
  }

  private boolean isProductEligibleForConfigChange(
      Map<String, String> categoryCodeConfigMap, Map<String, String> businessPartnerCodeConfigMap,
      ProductCollection productCollection) {
    return StringUtils.isNotBlank(businessPartnerCodeConfigMap.get(productCollection.getBusinessPartnerCode())) ||
        StringUtils.isNotBlank(categoryCodeConfigMap.get(productCollection.getCategoryCode()));
  }

  private String getReviewStatus(ProductCollection productCollection) {
    ConfigurationStatusRequest configurationStatusRequest = ConfigurationStatusRequest.builder()
        .categoryCode(productCollection.getCategoryCode()).businessPartnerCode(productCollection.getBusinessPartnerCode())
        .build();
    List<ConfigurationStatusResponse> responses = null;
    try {
       responses = productOutbound.getReviewConfiguration(Collections.singletonList(configurationStatusRequest));
    } catch (Exception e) {
      log.error("Error while getting configuration for product : {}", productCollection.getProductCode(), e);
    }
    if(CollectionUtils.isNotEmpty(responses)) {
      return responses.get(0).getReviewConfig();
    } else {
      return StringUtils.EMPTY;
    }
  }

  private void setMandatoryParameters() {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, com.gdn.partners.pbp.commons.constants.Constants.DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, com.gdn.partners.pbp.commons.constants.Constants.DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, com.gdn.partners.pbp.commons.constants.Constants.DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, com.gdn.partners.pbp.commons.constants.Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, com.gdn.partners.pbp.commons.constants.Constants.DEFAULT_CHANNEL_ID);
  }

  public static Date getEarlierDateByMinutes(int minutes) {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.MINUTE, -minutes);
    return calendar.getTime();
  }


  @Override
  @Trace(dispatcher = true)
  public void publishImageQcBacklogProducts(String storeId, String orderProductsBy, String orderProductsIn) {
    int batchSize = Integer.parseInt(productSystemParameterService
        .findByStoreIdAndVariable(storeId, com.gdn.partners.pbp.commons.constants.Constants.IMAGE_QC_BATCH_SIZE)
        .getValue());
    List<ProductImageQcBacklog> productImageQcBacklogList = productImageQcBacklogService
        .findProductImageQcBacklogByStoreIdAndStatus(storeId, ImageQcStatus.PENDING.getImageQcStatus(), orderProductsBy,
            orderProductsIn, batchSize);
    if (CollectionUtils.isNotEmpty(productImageQcBacklogList)) {
      for (ProductImageQcBacklog productImageQcBacklog : productImageQcBacklogList) {
        try {
          productImageQcBacklog.setStatus(ImageQcStatus.IN_PROGRESS.getImageQcStatus());
          productImageQcBacklog = productImageQcBacklogService.saveProductImageQcBacklog(productImageQcBacklog);
          ImageQcRequestDomainEvent imageQcRequestDomainEvent =
              productServiceWrapper.getImageQcRequestDomainEvent(productImageQcBacklog.getProductCode());
          imageQcRequestDomainEvent.setProductCode(
              com.gdn.partners.pbp.commons.constants.Constants.BACKLOG + productImageQcBacklog.getProductCode());
          kafkaProducer.send(DomainEventName.IMAGE_QC_PREDICTION_REQUEST, productImageQcBacklog.getProductCode(),
              imageQcRequestDomainEvent);
        } catch (Exception e) {
          log.error("Exception when trying to process image qc backlog for product : {} ",
              productImageQcBacklog.getProductCode(), e);
          productImageQcBacklog.setStatus(ImageQcStatus.FAILED.getImageQcStatus());
          productImageQcBacklogService.saveProductImageQcBacklog(productImageQcBacklog);
        }
      }
    }
  }

  @Override
  @Async
  public void syncInReviewProducts(String storeId) throws Exception {
    List<ProductCollection> productCollectionList = fetchProductsToSync(storeId);
    ProductSystemParameter productSystemParameter = productSystemParameterService
      .findByStoreIdAndVariable(storeId,
        SystemParameterConstants.IS_REVISED_PUBLISH_ENABLED);
    List<ProductCollection> editProductsCollectionList =
      productCollectionList.stream().filter(ProductCollection::isEdited)
      .filter(productCollection -> !NEED_CORRECTION.equalsIgnoreCase(productCollection.getState()))
      .filter(productCollection -> productCollection.getResubmitCount() == 0)
      .collect(Collectors.toList());
    compareStatesAndPublishImage(storeId, editProductsCollectionList, fetchProductFromPDT(
      editProductsCollectionList.stream().map(ProductCollection::getProductCode)
        .collect(Collectors.toList())));
    if (Boolean.parseBoolean(productSystemParameter.getValue())) {
      List<ProductCollection> revisedProductsCollectionList = productCollectionList.stream()
        .filter(productCollection -> productCollection.getResubmitCount() != 0)
        .filter(productCollection -> IN_PROGRESS.equalsIgnoreCase(productCollection.getState()))
        .collect(Collectors.toList());
      fetchAndPublishRevisedProducts(revisedProductsCollectionList);
    }
  }

  private List<ProductCollection> fetchProductsToSync(String storeId) {
    int beforeMin = Integer.parseInt(productSystemParameterService
      .findByStoreIdAndVariable(storeId, SystemParameterConstants.SYNC_PRODUCTS_BEFORE_MIN)
      .getValue());
    int differenceMin = Integer.parseInt(productSystemParameterService
      .findByStoreIdAndVariable(storeId, SystemParameterConstants.SYNC_PRODUCTS_DIFFERENCE_MIN)
      .getValue());
    //this will give us the between time to fetch products
    Date afterDate = getEarlierDateByMinutes(beforeMin);
    Date beforeDate = getEarlierDateByMinutes(beforeMin - differenceMin);
    log.info("Fetching products to sync from {} and till {} ",afterDate,beforeDate);
    return productService.fetchProductsToSync(storeId, afterDate, beforeDate);
  }

  @Async
  @Trace(dispatcher = true)
  @Override
  public void syncActiveProducts(String storeId) {
    setMandatoryParameters();
    try {
      int beforeMin = Integer.parseInt(productSystemParameterService.findByStoreIdAndVariable(storeId,
          SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_BEFORE_MIN).getValue());
      int differenceMin = Integer.parseInt(productSystemParameterService.findByStoreIdAndVariable(storeId,
          SystemParameterConstants.SYNC_ACTIVE_PRODUCTS_DIFFERENCE_MIN).getValue());
      //this will give us the between time to fetch products
      Date afterDate = getEarlierDateByMinutes(beforeMin);
      Date beforeDate = getEarlierDateByMinutes(beforeMin - differenceMin);
      log.info("Fetching active products to sync from {} and till {} ", afterDate, beforeDate);
      Pageable pageable = PageRequest.of(FIRST_PAGE, fetchActiveProductsSize);
      Page<ProductCollection> activeProductsPage = null;
      do {
        activeProductsPage = productService.fetchActiveProductsToSync(storeId, afterDate, beforeDate, pageable);
        for (ProductCollection productCollection : activeProductsPage.getContent()) {
          List<ProductBusinessPartner> productBusinessPartners =
              productBusinessPartnerService.findInactiveProductBusinessPartnersOfActiveL1(storeId,
                  productCollection.getProductId());
          try {
            if (CollectionUtils.isNotEmpty(productBusinessPartners)) {
              log.info("Trying to recreate product L3 and L4's for productCode = {} ",
                  productCollection.getProductCode());
              productBusinessPartnerService.retryCreate(storeId, productBusinessPartners.get(0).getId(),
                  productBusinessPartners.get(0));
              kafkaProducer.send(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY, productCollection.getProductCode(),
                  ConverterUtil.convertToProductDataAutoFixHistoryListRequest(productCollection.getProductCode(),
                      EditedReviewTypeConstants.RECONCILE_ACTIVE_PRODUCTS_TYPE, StringUtils.EMPTY));
            }
          } catch (Exception e) {
            log.error("Error while retry creating L3 and L4's for active L1 having productCode = {} ",
                productCollection.getProductCode(), e);
          }
        }
        pageable = pageable.next();
      } while (activeProductsPage.hasNext());
      log.info("Reconcilation job is completed for products between startDate = {} and endDate = {} ", afterDate,
          beforeDate);
    } catch (Exception e) {
      log.error("Error while reconcilation of active products. ", e);
    }
  }

  private void fetchAndPublishRevisedProducts(List<ProductCollection> revisedProductList)
    throws Exception {
    for (ProductCollection productCollection : revisedProductList) {
      PDTProductDomainEventModel pdtResponse;
      try {
        pdtResponse = productService.getPDTDomainModelResponseByCode(productCollection.getProductCode());
      } catch (Exception e) {
        log.error("Error when trying to get data from PDT for reconcile revised job : {} ",
            productCollection.getProductCode(), e);
        continue;
      }
      if (Objects.isNull(pdtResponse)) {
        log.info("Publishing revised product event for product not found in PDT {}. ",
          productCollection.getProductCode());
        publishEvent(productCollection);

      } else if (WorkflowState.NEED_CORRECTION.equals(pdtResponse.getState())) {
        log.info("Publishing revised product event for need correction product {}.",
          productCollection.getProductCode());
        publishEvent(productCollection);

      } else if (WorkflowState.PASSED.equals(pdtResponse.getState())) {
        log.info("Updating product state {}.", productCollection.getProductCode());
        productDistributionTaskRepositoryBean.productRetryStatusUpdate(
          productCollection.getProductCode(),
          ProductRetryStatusUpdate.builder().state(WorkflowState.NEED_CORRECTION.name())
            .markForDelete(true).reviewType(ReviewType.CONTENT_AND_IMAGE.name())
            .revised(pdtResponse.isRevised()).build());
        log.info("Publishing revised product event for passed product {}.",
          productCollection.getProductCode());
        publishEvent(productCollection);
      }
    }
  }

  private void publishEvent(ProductCollection productCollection) throws Exception {
    SellerDetailResponse sellerDetailResponse =
      fetchSellerDetailResponse(productCollection.getBusinessPartnerCode());
    boolean trustedSeller = fetchTrustedSellerStatus(productCollection.getBusinessPartnerCode());
    ProductBusinessPartner productBusinessPartner =
      productBusinessPartnerService.findFirstByStoreIdAndProductId(productCollection.getStoreId(),
        productCollection.getProductId());
    AddRevisedProductToPDTEvent addRevisedProductToPDTEvent =
        ConverterUtil.toAddRevisedProductToPDTEvent(productCollection,
            com.gdn.partners.pbp.commons.constants.Constants.DEFAULT_USERNAME, trustedSeller,
            productBusinessPartner, false, null, priceInfoVendorRevisedEnabled,
            priceInfoMaxVariantLimit);
    if (Objects.nonNull(sellerDetailResponse)) {
      addRevisedProductToPDTEvent.setSellerBadge(sellerDetailResponse.getSellerBadge());
    }
    kafkaProducer.send(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY,
      productCollection.getProductCode(),
      ConverterUtil.convertToProductDataAutoFixHistoryListRequest(
        productCollection.getProductCode(), EditedReviewTypeConstants.RECONCILE_REVISED_TYPE,
        StringUtils.EMPTY));
    log.info("Publishing Kafka event for reconciled revised product of product : {} ",
      productCollection.getProductCode());
    kafkaProducer.send(kafkaTopicProperties.getVendorCombinedEventNoPriority(),
      addRevisedProductToPDTEvent.getProductCode(), AddProductToVendorCombinedEventModel.builder()
        .addRevisedProductToPDTEvent(addRevisedProductToPDTEvent).build());
  }

  @Override
  public SellerDetailResponse fetchSellerDetailResponse(String businessPartnerCode) {
    SellerDetailResponse response = null;
    try {
      response = productAnalyticsOutbound.getSellerDetail(businessPartnerCode);
    } catch (Exception e) {
      log.error(
        "Exception while getting sellerDetail Response from ProductAnalytics for businessPartnerCode : {} ",
        businessPartnerCode, e);
    }
    return response;
  }

  private boolean fetchTrustedSellerStatus(String businessPartnerCode) throws Exception {
    return Optional.ofNullable(
        businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode))
      .map(ProfileResponse::isTrustedSeller).orElse(false);
  }

  private Map<String, String> fetchProductFromPDT(List<String> productCodeList) throws Exception {
    Map<String, String> pdtReviewTypeMap = new HashMap<>();
    log.info("Fetching data from PDT for product code list {} ",productCodeList.toString());
    for (String productCode : productCodeList) {
      PDTProductDomainEventModel pdtResponse = null;
      try {
        pdtResponse = productService.getPDTDomainModelResponseByCode(productCode);
      } catch (Exception e) {
        log.error("Error when trying to get data from PDT for reconcile edited job : {} ", productCode, e);
      }
      if (Objects.nonNull(pdtResponse) && Objects.nonNull(pdtResponse.getReviewType()) && !pdtResponse
          .isMarkForDelete() && pdtResponse.isEdited()) {
        pdtReviewTypeMap.put(productCode, pdtResponse.getReviewType().toString());
      } else {
        pdtReviewTypeMap.put(productCode, StringUtils.EMPTY);
      }
    }
    return pdtReviewTypeMap;
  }

  private void compareStatesAndPublishImage(String storeId,
    List<ProductCollection> productCollectionList,
    Map<String, String> productCodeReviewTypePDTMap) throws Exception {
    Map<String, ProfileResponse> merchantCodeProfileResponseMap =
      businessPartnerRepository.filterDetailsByBusinessPartnerCodeList(
        new BusinessPartnerCodesRequest(
          productCollectionList.stream().map(ProductCollection::getBusinessPartnerCode).distinct()
            .collect(Collectors.toList()))).stream().collect(
        Collectors.toMap(ProfileResponse::getBusinessPartnerCode, Function.identity(),
          (a, b) -> a));
    for (ProductCollection productCollection : productCollectionList) {
      if (StringUtils.isNotEmpty(productCollection.getReviewType()) && productCollection
        .getReviewType().contains(CONTENT) && (!productCodeReviewTypePDTMap
        .getOrDefault(productCollection.getProductCode(), StringUtils.EMPTY).contains(CONTENT))) {
        log.info("Publishing content event for product code : {} ",
          productCollection.getProductCode());
        ProductBusinessPartner productBusinessPartner = productBusinessPartnerService
            .findFirstByStoreIdAndProductId(productCollection.getStoreId(), productCollection.getProductId());
        AddEditedProductToPDTEvent addEditedProductToPDTEvent =
          ConverterUtil.toAddEditedProductToPDTEvent(storeId,
            EditedReviewTypeConstants.CONTENT_EDIT, productCollection, null,
            merchantCodeProfileResponseMap.getOrDefault(productCollection.getBusinessPartnerCode(),
              null), productBusinessPartner, priceInfoVendorEditedEnabled, priceInfoMaxVariantLimit);
        SellerDetailResponse sellerDetailResponse =
          fetchSellerDetailResponse(productCollection.getBusinessPartnerCode());
        if (Objects.nonNull(sellerDetailResponse)) {
          addEditedProductToPDTEvent.setSellerBadge(sellerDetailResponse.getSellerBadge());
        }
        kafkaProducer.send(kafkaTopicProperties.getVendorCombinedEventNoPriority(), productCollection.getProductCode(),
            AddProductToVendorCombinedEventModel.builder().addEditedProductToPDTEvent(addEditedProductToPDTEvent)
                .build());
        kafkaProducer.send(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY, productCollection.getProductCode(),
            ConverterUtil.convertToProductDataAutoFixHistoryListRequest(productCollection.getProductCode(),
                EditedReviewTypeConstants.RECONCILE_EDITED_TYPE, EditedReviewTypeConstants.RECONCILE_CONTENT));
        log.info("Publishing Kafka event for reconciled content edited product of product : {} ",
            productCollection.getProductCode());
      }
      if (StringUtils.isNotEmpty(productCollection.getReviewType()) && productCollection
        .getReviewType().contains(IMAGE) && (!productCodeReviewTypePDTMap
        .getOrDefault(productCollection.getProductCode(), StringUtils.EMPTY).contains(IMAGE))) {
        log.info("Publishing image event for product code : {} ",
          productCollection.getProductCode());
        ProductBusinessPartner productBusinessPartner = productBusinessPartnerService
            .findFirstByStoreIdAndProductId(productCollection.getStoreId(), productCollection.getProductId());
        AddEditedProductToPDTEvent addEditedProductToPDTEvent =
          ConverterUtil.toAddEditedProductToPDTEvent(storeId, EditedReviewTypeConstants.IMAGE_EDIT,
            productCollection, null,
            merchantCodeProfileResponseMap.getOrDefault(productCollection.getBusinessPartnerCode(),
              null), productBusinessPartner, priceInfoVendorEditedEnabled, priceInfoMaxVariantLimit);
        SellerDetailResponse sellerDetailResponse =
          fetchSellerDetailResponse(productCollection.getBusinessPartnerCode());
        if (Objects.nonNull(sellerDetailResponse)) {
          addEditedProductToPDTEvent.setSellerBadge(sellerDetailResponse.getSellerBadge());
        }
        kafkaProducer
          .send(kafkaTopicProperties.getVendorCombinedEventNoPriority(), productCollection.getProductCode(),
            AddProductToVendorCombinedEventModel.builder().addEditedProductToPDTEvent(addEditedProductToPDTEvent).build());
        kafkaProducer.send(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY, productCollection.getProductCode(),
            ConverterUtil.convertToProductDataAutoFixHistoryListRequest(productCollection.getProductCode(),
                EditedReviewTypeConstants.RECONCILE_EDITED_TYPE, EditedReviewTypeConstants.RECONCILE_IMAGE));
        log.info("Publishing Kafka event for reconciled image edited product of product : {} ",
            productCollection.getProductCode());
      }
    }
  }

  @Override
  public void syncPreLiveProducts(String storeId) {
    int beforeMin = Integer.parseInt(productSystemParameterService
        .findByStoreIdAndVariable(storeId, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_BEFORE_MIN).getValue());
    int differenceMin = Integer.parseInt(productSystemParameterService
        .findByStoreIdAndVariable(storeId, SystemParameterConstants.SYNC_PRE_LIVE_PRODUCTS_DIFFERENCE_MIN).getValue());
    Date afterDate = getEarlierDateByMinutes(beforeMin);
    Date beforeDate = getEarlierDateByMinutes(beforeMin - differenceMin);
    log.info("Fetching pre-live products to sync from {} and till {} ", afterDate, beforeDate);
    Pageable pageable = PageRequest.of(FIRST_PAGE, fetchPreLiveProductsSize);
    Page<ProductCollection> preLiveProductsPage = null;
    do {
      preLiveProductsPage = productService
          .fetchPreLiveProductsToSync(storeId, IN_PROGRESS, false, false, 0, afterDate, beforeDate, pageable);
      for (ProductCollection productCollection : preLiveProductsPage.getContent()) {
        try {
          boolean ifProductExistsInPDT = false;
          try {
            log.info("Checking if product exists in PDT for productCode : {} ", productCollection.getProductCode());
            ifProductExistsInPDT = productService.checkIfProductExistsInPDT(productCollection.getProductCode(), true);
          } catch (Exception e) {
            log.error("Exception while getting data from PDT for productCode : {} ", productCollection.getProductCode(),
                e);
            continue;
          }
          if (!ifProductExistsInPDT) {
            Boolean trustedSeller = Optional.ofNullable(
                businessPartnerRepository.filterDetailByBusinessPartnerCode(
                  productCollection.getBusinessPartnerCode())).map(ProfileResponse::isTrustedSeller)
              .orElse(false);
            productPublisherService
                .publish(productCollection.getProductCode(), productCollection.getBusinessPartnerCode(),
                    productCollection.getBusinessPartnerName(), productCollection.getUpdatedBy(),
                    productCollection.isPostLive(), productCollection.isRestrictedKeywordsPresent(),
                    productCollection.getRestrictedKeywordsDetected(), productCollection.getPrioritySeller(),
                  trustedSeller, productCollection.getProductId(), productCollection);
            kafkaProducer.send(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY, productCollection.getProductCode(),
                ConverterUtil.convertToProductDataAutoFixHistoryListRequest(productCollection.getProductCode(),
                    EditedReviewTypeConstants.RECONCILE_PRE_LIVE, StringUtils.EMPTY));
          }
        } catch (Exception e) {
          log.error("Exception when trying to sync pre-live product for productCode : {} ",
              productCollection.getProductCode(), e);
        }
      }
      pageable = pageable.next();
    } while (preLiveProductsPage.hasNext());
    log.info("Reconcile job for pre-live products to sync from {} and till {} is completed", afterDate, beforeDate);
  }

  @Override
  public void addDeleteVariantRetryPublishEvents(String storeId, String productCode, String requestId, String userName)
      throws Exception {
    List<String> productCodesList =
        productService.findProductsByAddDeleteVariantByPendingStatus(storeId, productCode, requestId, userName);
    sendRetryPublishEvent(productCodesList);
  }

  private void sendRetryPublishEvent(List<String> productCodesList) {
    for(String productCode : productCodesList){
      AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEvent =
          new AddDeleteVariantRetryPublishEventModel();
      addDeleteVariantRetryPublishEvent.setProductCode(productCode);
      kafkaProducer.send(kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent(),productCode,
          addDeleteVariantRetryPublishEvent);
      log.info("Message published, from topic {}, for product code :{}",
          kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent(), productCode);
    }
  }

}