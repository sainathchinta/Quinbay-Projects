package com.gdn.x.mta.distributiontask.service.impl;

import com.gda.mta.product.dto.ProductDataAutoFixHistoryDto;
import com.gda.mta.product.dto.response.ProductDataAutoFixHistoryListRequest;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.time.DateUtils;
import com.gdn.x.mta.distributiontask.dao.api.ProductRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.dao.api.feign.PBPFeign;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.CommonImageBackfillingEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTNeedRevisionEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductDeleteEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductActionRetry;
import com.gdn.x.mta.distributiontask.model.ProductAutoApproval;
import com.gdn.x.mta.distributiontask.model.ProductMigration;
import com.gdn.x.mta.distributiontask.model.SystemParameterConfig;
import com.gdn.x.mta.distributiontask.model.enums.ActionRetryStatus;
import com.gdn.x.mta.distributiontask.model.enums.AutoApprovalStatus;
import com.gdn.x.mta.distributiontask.model.enums.ProductMigrationStatus;
import com.gdn.x.mta.distributiontask.model.enums.ProductMigrationType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductCodeListRequest;
import com.gdn.x.mta.distributiontask.service.api.ProductActionRetryService;
import com.gdn.x.mta.distributiontask.service.api.ProductAutoApprovalService;
import com.gdn.x.mta.distributiontask.service.api.ProductMigrationService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import com.gdn.x.mta.distributiontask.service.api.ScheduledJobService;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import com.gdn.x.mta.distributiontask.service.api.SystemParameterConfigService;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static com.gdn.x.mta.distributiontask.model.Constants.ACTIVE;
import static com.gdn.x.mta.distributiontask.model.Constants.AUTO_NEED_REVISION;
import static com.gdn.x.mta.distributiontask.model.Constants.BATCH_SIZE_TO_PUBLISH_COMMON_IMAGES_MIGRATION_RECORDS;
import static com.gdn.x.mta.distributiontask.model.Constants.DESC;
import static com.gdn.x.mta.distributiontask.model.Constants.INTERNAL_BUSINESS_PARTNER;
import static com.gdn.x.mta.distributiontask.model.Constants.IN_PROGRESS;
import static com.gdn.x.mta.distributiontask.model.Constants.MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_COMMON_IMAGE_MIGRATION;
import static com.gdn.x.mta.distributiontask.model.Constants.MAX_ALLOWED_PRODUCT_AUTO_APPROVALS;
import static com.gdn.x.mta.distributiontask.model.Constants.MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS;
import static com.gdn.x.mta.distributiontask.model.Constants.PDT_AUTO_REJECTION;
import static com.gdn.x.mta.distributiontask.model.Constants.PDT_RETRY_DELETE;
import static com.gdn.x.mta.distributiontask.model.Constants.THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY;
import static com.gdn.x.mta.distributiontask.model.Constants.UPDATED_DATE;

@Slf4j
@Service
public class ScheduledJobServiceImpl implements ScheduledJobService {

  private static final String RETRY_JOB_INELIGIBLE = "Product is ineligible for retry job";
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductServiceRepository productServiceRepository;

  @Autowired
  private SolrVendorCollectionService solrVendorCollectionService;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private ProductAutoApprovalService productAutoApprovalService;

  @Autowired
  private ProductActionRetryService productActionRetryService;

  @Autowired
  private ProductMigrationService productMigrationService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private ProductWrapperService productWrapperService;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @Value("${need.correction.product.size.for.pagination}")
  private int needCorrectionProductSizeForPagination;

  @Value("${product.delete.event.payload.size}")
  private int productDeleteEventPayloadSize;

  private static final int FIRST_PAGE = 0;
  private static final int TIME_ELAPSED_IN_HOURS = 1;

  @Async
  @Override
  public void runPostLiveConfigChanges(String storeId, String requestId, String username, long dateInMillis) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    List<ConfigurationStatusResponse> configurationStatusResponseList
        = productServiceRepository.getReviewConfigurationChanges(dateInMillis);
    if(CollectionUtils.isNotEmpty(configurationStatusResponseList)) {
      Map<String, String> categoryCodeConfigMap = new HashMap<>();
      Map<String, String> businessPartnerCodeConfigMap = new HashMap<>();
      setCategoryAndMerchantConfigMaps(
          configurationStatusResponseList, categoryCodeConfigMap, businessPartnerCodeConfigMap);
      List<Product> unAssignedProducts;
      SystemParameterConfig maxNoOfDaysToAllowConfigChanges = systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
              com.gdn.x.mta.distributiontask.model.Constants.MAX_NO_OF_DAYS_TO_ALLOW_CONFIG_CHANGES);
      SystemParameterConfig pageSizeForConfigChanges = systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
          com.gdn.x.mta.distributiontask.model.Constants.PAGE_SIZE_FOR_CONFIG_CHANGES);
      int pageSize = Integer.parseInt(pageSizeForConfigChanges.getValue());
      Calendar date = Calendar.getInstance();
      date.add(Calendar.DATE, -Integer.parseInt(maxNoOfDaysToAllowConfigChanges.getValue()));
      Date fetchByDate = date.getTime();
      do {
        unAssignedProducts = productService.getUnassignedProductsByStoreIdAndStateAndMarkForDeleteAndPostLive(
            GdnMandatoryRequestParameterUtil.getStoreId(), Collections.singletonList(WorkflowState.IN_REVIEW),
            false, false, fetchByDate, pageSize);
        for (Product product: unAssignedProducts) {
          if(isProductEligibleForPostLive(categoryCodeConfigMap, businessPartnerCodeConfigMap, product)) {
            log.info("Changing product {} to postLive", product.getProductCode());
            changeProductToPostLive(product);
            solrVendorCollectionService.updatePostLiveFlag(product.getProductCode(), true);
          }
        }
        if ((!unAssignedProducts.isEmpty())) {
          fetchByDate = unAssignedProducts.get(unAssignedProducts.size() - 1).getUpdatedDate();
        }
      } while (unAssignedProducts.size() == pageSize);
    }
    log.info("RunPostLiveConfigChanges done successfully");
  }

  private void changeProductToPostLive(Product product) {
    try {
      productServiceRepository.updateProductCollectionAsPostLive(
          GdnMandatoryRequestParameterUtil.getStoreId(), product.getProductCode());
      productService.updateProductAsPostLiveTrue(product.getProductCode());
    } catch (Exception e) {
      log.error("Error while changing product {} to postLive", product.getProductCode(), e);
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

  private boolean isProductEligibleForPostLive(
      Map<String, String> categoryCodeConfigMap, Map<String, String> businessPartnerCodeConfigMap, Product product) {
    if(!isProductEligibleForConfigChange(categoryCodeConfigMap, businessPartnerCodeConfigMap, product)) {
      return false;
    }
    if(product.isRestrictedKeywordsPresent()) {
      return false;
    }
    if (StringUtils.equals(Constants.POST_LIVE_STATUS,
        businessPartnerCodeConfigMap.get(product.getBusinessPartnerCode()))) {
      return true;
    } else if (StringUtils.isBlank(businessPartnerCodeConfigMap.get(product.getBusinessPartnerCode())) ||
        StringUtils.isBlank(categoryCodeConfigMap.get(product.getCategoryCode()))) {
      return Constants.POST_LIVE_STATUS.equals(getReviewStatus(product));
    }
    return StringUtils.equals(Constants.POST_LIVE_STATUS, categoryCodeConfigMap.get(product.getCategoryCode()))
        && !StringUtils.equals(Constants.PRE_LIVE_FLAG,
        businessPartnerCodeConfigMap.get(product.getBusinessPartnerCode()));
  }

  private boolean isProductEligibleForConfigChange(
      Map<String, String> categoryCodeConfigMap, Map<String, String> businessPartnerCodeConfigMap, Product product) {
    return !StringUtils.equalsIgnoreCase(product.getBusinessPartnerCode(), INTERNAL_BUSINESS_PARTNER) &&
        (StringUtils.isNotBlank(businessPartnerCodeConfigMap.get(product.getBusinessPartnerCode())) ||
            StringUtils.isNotBlank(categoryCodeConfigMap.get(product.getCategoryCode())));
  }

  private String getReviewStatus(Product product) {
    ConfigurationStatusRequest configurationStatusRequest = ConfigurationStatusRequest.builder()
        .categoryCode(product.getCategoryCode()).businessPartnerCode(product.getBusinessPartnerCode())
        .build();
    List<ConfigurationStatusResponse> responses = null;
    try {
      responses = productServiceRepository.getReviewConfiguration(Collections.singletonList(configurationStatusRequest));
    } catch (Exception e) {
      log.error("Error while getting configuration for product : {}", product.getProductCode(), e);
    }
    if(CollectionUtils.isNotEmpty(responses)) {
      return responses.get(0).getReviewConfig();
    } else {
      return StringUtils.EMPTY;
    }
  }

  @Override
  @Async
  public void deleteProducts(String storeId, int days, int batchSize, int maxBatchSize, boolean eventDeleteFlow) {
    if (eventDeleteFlow) {
      deleteProductsNewFlow(storeId, days, batchSize, maxBatchSize);
      return;
    }
    long startTime = Calendar.getInstance().getTimeInMillis();
    Pageable pageable;
    Date updateDate = DateUtils.addDays(new Date(), -days);
    log.info("Deleting products older than updates date : {} batch size : {}", updateDate, batchSize);
    int count = 0;
    int page = 0;
    Slice<Object[]> products = null;
    do {
      try {
        List<String> productIds = new ArrayList<>();
        List<String> productCodes = new ArrayList<>();
        pageable = PageRequest.of(page, batchSize);
        products = productService.findAllProductIdsBeforeUpdatedDateAndMarkForDeleteTrue(updateDate, pageable);
        if (CollectionUtils.isEmpty(products.getContent())) {
          break;
        }
        for (Object[] object : products.getContent()) {
          productIds.add((String) object[0]);
          productCodes.add((String) object[1]);
        }
        count = count + productCodes.size();
        log.info("Deleting products for products : {}", productIds);
        productService.deleteProducts(storeId, productIds, productCodes, batchSize);
      } catch (Exception e) {
        log.error("Error while deleting the products from PDT. Total number of products deleted : {} ", count, e);
        return;
      }
      page++;
    } while ((count < maxBatchSize) && products.hasNext());
    long endtime = Calendar.getInstance().getTimeInMillis();
    log.info("Total time taken for deleting {} products in milliseconds : {}", count, endtime - startTime);
  }

  public void deleteProductsNewFlow(String storeId, int days, int batchSize, int maxBatchSize) {
    long startTime = Calendar.getInstance().getTimeInMillis();
    String uuid = UUID.randomUUID().toString();
    Pageable pageable;
    Date updateDate = DateUtils.addDays(new Date(), -days);
    log.info("Deleting products older than updates date : {} batch size : {} , identifier : {} ", updateDate, batchSize,
        uuid);
    int count = 0;
    int page = 0;
    Slice<Product> products = null;
    do {
      try {
        pageable = PageRequest.of(page, batchSize);
        products = productService.findProductIdsBeforeUpdatedDateAndMarkForDeleteTrueAndPickedForDeleteFalse(updateDate,
            pageable);
        if (CollectionUtils.isEmpty(products.getContent())) {
          break;
        }
        count = count + products.getSize();
        products.getContent().forEach(product -> product.setPickedForDeletion(true));
        productService.saveBulkProducts(products.getContent());
        List<List<Product>> productDeletePartition =
            Lists.partition(products.getContent(), productDeleteEventPayloadSize);
        for (List<Product> productList : productDeletePartition) {
          ProductDeleteEventModel productDeleteEventModel = new ProductDeleteEventModel();
          productDeleteEventModel.setStoreId(storeId);
          productDeleteEventModel.setIdentifier(uuid);
          productDeleteEventModel.setProductCodeList(
              productList.stream().map(Product::getProductCode).collect(Collectors.toList()));
          log.info("Publishing event for products : {}",
              productList.stream().map(Product::getProductCode).collect(Collectors.toList()));
          kafkaProducer.send(kafkaTopicPropertiesConsumer.getDeleteProductEvent(),
              productDeleteEventModel.getProductCodeList().get(0), productDeleteEventModel);
        }
      } catch (Exception e) {
        log.error("Error while deleting the products from PDT. Total number of products deleted : {} identifier : {} ",
            count, uuid, e);
        return;
      }
      page++;
    } while ((count < maxBatchSize) && products.hasNext());
    long endtime = Calendar.getInstance().getTimeInMillis();
    log.info("Total time taken for deleting {} products in milliseconds : {} , identifier : {} ", count,
        endtime - startTime, uuid);
  }

  private List<ProductAutoApproval> productsToAutoApprove(String storeId, String orderProductsBy,
      String orderProductsIn, AutoApprovalStatus autoApprovalStatus, int maximumAllowedProductAutoApprovalsValue) {
    if (UPDATED_DATE.equalsIgnoreCase(orderProductsBy) && DESC.equalsIgnoreCase(orderProductsIn)) {
      return productAutoApprovalService.findProductsToAutoApprovalOrderByUpdatedDateDesc(storeId,
          autoApprovalStatus, maximumAllowedProductAutoApprovalsValue);
    } else if (UPDATED_DATE.equalsIgnoreCase(orderProductsBy)) {
      return productAutoApprovalService.findProductsToAutoApprovalOrderByUpdatedDateAsc(storeId,
          autoApprovalStatus, maximumAllowedProductAutoApprovalsValue);
    } else if (DESC.equalsIgnoreCase(orderProductsIn)) {
      return productAutoApprovalService.findProductsToAutoApprovalOrderByCreatedDateDesc(storeId,
          autoApprovalStatus, maximumAllowedProductAutoApprovalsValue);
    } else {
      return productAutoApprovalService.findProductsToAutoApprovalOrderByCreatedDateAsc(storeId,
          autoApprovalStatus, maximumAllowedProductAutoApprovalsValue);
    }
  }

  @Async
  @Override
  public void autoApprovePendingProducts(String storeId, String orderProductsBy, String orderProductsIn, String status) {
    try {
      SystemParameterConfig maximumAllowedProductAutoApprovals = systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
          MAX_ALLOWED_PRODUCT_AUTO_APPROVALS);
      SystemParameterConfig maxNumberOfDaysToApproveProducts = systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
          MAX_NUMBER_OF_DAYS_TO_APPROVE_ASSIGNED_PRODUCTS);
      int maximumAllowedProductAutoApprovalsValue;
      maximumAllowedProductAutoApprovalsValue = Integer.parseInt(maximumAllowedProductAutoApprovals.getValue());
      int maxNumberOfDaysToApproveAssignedProducts = Integer.parseInt(maxNumberOfDaysToApproveProducts.getValue());
      AutoApprovalStatus autoApprovalStatus = AutoApprovalStatus.PENDING;
      try {
        autoApprovalStatus = AutoApprovalStatus.valueOf(status);
      } catch (Exception e){
        log.warn("Warning during AutoApprovalStatus conversion while auto approving pending products {} ", e);
      }
      List<ProductAutoApproval> productAutoApprovalList =
          productsToAutoApprove(storeId, orderProductsBy, orderProductsIn, autoApprovalStatus, maximumAllowedProductAutoApprovalsValue);
      productService.autoApprovePendingProducts(storeId, productAutoApprovalList, maxNumberOfDaysToApproveAssignedProducts);
    } catch (Exception e) {
      log.error("Error while auto approving pending products {} ", e);
    }
  }

  private List<ProductActionRetry> productsToRetryAction(String storeId, String orderProductsBy,
      String orderProductsIn, ActionRetryStatus status, String action, int maximumAllowedProductForActionRetryValue) {
    if (UPDATED_DATE.equalsIgnoreCase(orderProductsBy) && DESC.equalsIgnoreCase(orderProductsIn)) {
      return productActionRetryService.findProductsToRetryActionOrderByUpdatedDateDesc(storeId,
          action, status, maximumAllowedProductForActionRetryValue);
    } else if (UPDATED_DATE.equalsIgnoreCase(orderProductsBy)) {
      return productActionRetryService.findProductsToRetryActionOrderByUpdatedDateAsc(storeId,
          action, status, maximumAllowedProductForActionRetryValue);
    } else if (DESC.equalsIgnoreCase(orderProductsIn)) {
      return productActionRetryService.findProductsToRetryActionOrderByCreatedDateDesc(storeId,
          action, status, maximumAllowedProductForActionRetryValue);
    } else {
      return productActionRetryService.findProductsToRetryActionOrderByCreatedDateAsc(storeId,
          action, status, maximumAllowedProductForActionRetryValue);
    }
  }

  @Async
  @Override
  public void retryProductsByAction(String storeId, String orderProductsBy, String orderProductsIn, String action) {
    try {
      SystemParameterConfig maximumAllowedProductForActionRetry = systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
          THRESHOLD_VALUE_FOR_AUTO_NEED_REVISION_RETRY);
      int maximumAllowedProductForActionRetryValue = Integer.parseInt(maximumAllowedProductForActionRetry.getValue());
      ActionRetryStatus actionRetryStatus = ActionRetryStatus.PENDING;
      List<ProductActionRetry> productActionRetryList =
          productsToRetryAction(storeId, orderProductsBy, orderProductsIn, actionRetryStatus, action, maximumAllowedProductForActionRetryValue);
      log.info(
          "Number of products : {} , and the list of products picked by scheduler for retry job for action :  {} are {} ",
          productActionRetryList.size(), action,
          productActionRetryList.stream().map(ProductActionRetry::getProductCode).collect(Collectors.toList()));
      if (AUTO_NEED_REVISION.equals(action)) {
        productService.autoNeedReviseForPendingProducts(storeId, productActionRetryList);
        log.info("Performing retry job for auto need revision for productActionRetryList : {} ", productActionRetryList);
      } else if (PDT_AUTO_REJECTION.equals(action)) {
        productWrapperService.pdtAutoRejectForPendingProducts(productActionRetryList);
        log.info("Performing retry job for auto rejection for productActionRetryList : {} ", productActionRetryList);
      }
      else if (PDT_RETRY_DELETE.equals(action)) {
        log.info("Performing retry job for PDT_RETRY_DELETE action");
        List<ProductActionRetry> productActionRetries = new ArrayList<>();
        List<ProductDataAutoFixHistoryDto> productDataAutoFixHistoryDtoList = new ArrayList<>();
        for (ProductActionRetry productActionRetry : productActionRetryList) {
          try {
            log.info("Fetching basic product details from PCB for productCode : {} ",
                productActionRetry.getProductCode());
            ProductResponse productResponse = productServiceRepository.getProductBasicDetailByProductCode(productActionRetry.getProductCode());
            log.info("Fetching product details from PDT_PRODUCT for productCode : {} ",
                productActionRetry.getProductCode());
            Product product = productRepository.findByProductCode(productActionRetry.getProductCode());
            if (!productActionRetry.isMarkForDelete() && !productResponse.isReviewPending() && !product.isMarkForDelete()) {
              ProductDataAutoFixHistoryDto productDataAutoFixHistoryDto = new ProductDataAutoFixHistoryDto();
              log.info("Deleting the product from PDT and deleting the original images for productCode : {} ",
                  productActionRetry.getProductCode());
              productWrapperService.removeProductAndDeleteOriginalImages(new RemoveProductRequest(productActionRetry.getProductCode()));
              productDataAutoFixHistoryDto.setProductCode(productActionRetry.getProductCode());
              productDataAutoFixHistoryDto.setType(PDT_RETRY_DELETE);
              productDataAutoFixHistoryDtoList.add(productDataAutoFixHistoryDto);
              productActionRetry.setStatus(ActionRetryStatus.SUCCESS);
              productActionRetries.add(productActionRetry);
              log.info("PDT retry job done for productCode : {}", productActionRetry.getProductCode());
            } else {
              log.info("productCode : {} is not eligible for PDT retry job , updating its status to success",
                  productActionRetry.getProductCode());
              productActionRetry.setData(RETRY_JOB_INELIGIBLE);
              productActionRetry.setStatus(ActionRetryStatus.SUCCESS);
              productActionRetries.add(productActionRetry);
            }
          }catch (Exception e){
            log.error("Exception caught while retrying auto need revision of pending products for productCode : {} ",
                productActionRetry.getProductCode(), e);
            productActionRetry.setStatus(ActionRetryStatus.FAILED);
            productActionRetries.add(productActionRetry);
          }
        }
        if(CollectionUtils.isNotEmpty(productActionRetries)){
          log.info("PDT retry job done for productCodes : {} updating the status in PDT_PRODUCT_ACTION_RETRY ",
              productActionRetries.stream().map(ProductActionRetry::getProductCode).collect(Collectors.toList()));
          productActionRetryService.saveProductActionRetryList(productActionRetries);
        }
        if (CollectionUtils.isNotEmpty(productDataAutoFixHistoryDtoList)) {
          kafkaProducer
              .send(DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY, productActionRetryList.get(0).getProductCode(),
                  new ProductDataAutoFixHistoryListRequest(productDataAutoFixHistoryDtoList));
          log.info("Publishing the event : {} , productDataAutoFixHistoryListRequest : {} ",
              DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY, productDataAutoFixHistoryDtoList);
        }
      }
    } catch (Exception e) {
      log.error("Error while retrying auto need revision of pending products ", e);
    }
  }

  @Async
  @Override
  public void addProductsForAutoApprovalOnConfigChange(String storeId) {
    productAutoApprovalService.processPendingAutoQcConfigChange(storeId);
  }

  @Async
  @Override
  public void publishPendingProductMigrationRecords(String storeId, String migrationType,
      ProductCodeListRequest productCodeListRequest) {
    if (CollectionUtils.isNotEmpty(productCodeListRequest.getProductList())) {
      processListOfProductCodesForMigration(storeId, migrationType, productCodeListRequest);
    } else {
      processBatchOfProductsForMigration(storeId, migrationType);
    }
  }

  private void processBatchOfProductsForMigration(String storeId, String migrationType) {
    if (ProductMigrationType.COMMON_IMAGE_MIGRATION.name().equals(migrationType)) {
      SystemParameterConfig maxNumberOfRecordsToFetch =
          getSystemParameterConfig(storeId, MAXIMUM_NUMBER_OF_RECORDS_TO_FETCH_FOR_COMMON_IMAGE_MIGRATION);
      SystemParameterConfig batchSize =
          getSystemParameterConfig(storeId, BATCH_SIZE_TO_PUBLISH_COMMON_IMAGES_MIGRATION_RECORDS);
      int limit = Integer.parseInt(maxNumberOfRecordsToFetch.getValue());
      int batch = Integer.parseInt(batchSize.getValue());
      int page = 0;
      while (page * batch < limit) {
        List<ProductMigration> commonImageMigrationList =
            productMigrationService.findProductMigrationMigrationByStoreIdAndStatus(storeId, migrationType, batch);
        publishCommonImageFlagBackfillingEvent(storeId, commonImageMigrationList);
        page++;
      }
    }
  }

  private void processListOfProductCodesForMigration(String storeId, String migrationType,
      ProductCodeListRequest productCodeListRequest) {
    List<ProductMigration> productMigrationList =
        fetchProductMigrationByProductType(storeId, migrationType, productCodeListRequest);
    if (ProductMigrationType.COMMON_IMAGE_MIGRATION.name().equals(migrationType)) {
      publishCommonImageFlagBackfillingEvent(storeId, productMigrationList);
    }
  }

  private List<ProductMigration> fetchProductMigrationByProductType(String storeId, String migrationType,
      ProductCodeListRequest productCodeListRequest) {
    List<ProductMigration> productMigrationList =
        productMigrationService.findProductMigrationByProductCodes(storeId, migrationType,
            productCodeListRequest.getProductList());
    List<String> productCodesFromDb =
        productMigrationList.stream().map(ProductMigration::getProductCode).collect(Collectors.toList());
    List<String> productCodesInRequest = new ArrayList<>(productCodeListRequest.getProductList());
    publishCommonImageFlagBackfillingEvent(storeId, productMigrationList);
    productCodesInRequest.removeAll(productCodesFromDb);
    for (String productCode : productCodesInRequest) {
      ProductMigration productMigration =
          new ProductMigration(productCode, ProductMigrationStatus.PENDING.name(), migrationType);
      productMigration.setStoreId(storeId);
      productMigrationList.add(productMigration);
    }
    return productMigrationList;
  }

  private SystemParameterConfig getSystemParameterConfig(String storeId, String variableName) {
    SystemParameterConfig systemParameterConfig =
        systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
            variableName);
    return systemParameterConfig;
  }

  private void publishCommonImageFlagBackfillingEvent(String storeId,
      List<ProductMigration> commonImageMigrationList) {
    for (ProductMigration commonImageMigration : commonImageMigrationList) {
      commonImageMigration.setStatus(ProductMigrationStatus.PUBLISHED.name());
      productMigrationService.saveProductMigration(commonImageMigration);
      log.info("Publishing event = {}  for productCode = {} ",
          DomainEventName.PRODUCT_AND_ITEM_COMMON_IMAGE_BACKFILLING, commonImageMigration.getProductCode());
      kafkaProducer.send(DomainEventName.PRODUCT_AND_ITEM_COMMON_IMAGE_BACKFILLING,
          commonImageMigration.getProductCode(),
          new CommonImageBackfillingEventModel(storeId, commonImageMigration.getProductCode()));
    }
  }

  @Async
  @Override
  public void syncNeedCorrectionProducts(String storeId, int dataWindowInHours) {
    try {
      Calendar calendar = Calendar.getInstance();
      calendar.add(Calendar.HOUR_OF_DAY, -TIME_ELAPSED_IN_HOURS);
      Date endTime = calendar.getTime();

      calendar.setTime(endTime);
      calendar.add(Calendar.HOUR_OF_DAY, -dataWindowInHours);
      Date startTime = calendar.getTime();

      log.info("Fetching need correction products from {} and till {} ", startTime, endTime);
      Pageable pageable = PageRequest.of(FIRST_PAGE, needCorrectionProductSizeForPagination);
      Page<Product> needCorrectionProductsPage = new PageImpl<>(new ArrayList<>(), pageable, 0);

      do {
        needCorrectionProductsPage = productService.fetchNeedCorrectionProducts(storeId, startTime, endTime, pageable);
        log.info("no. of products in need correction and mfd true state {} ",
            needCorrectionProductsPage.getTotalElements());
        for (Product product : needCorrectionProductsPage.getContent()) {
          try {
            String statusFromPbp = pbpFeign.getProductStatus(storeId, GdnMandatoryRequestParameterUtil.getChannelId(),
                GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
                GdnMandatoryRequestParameterUtil.getUsername(), product.getProductCode()).getValue().getResult();
            if (IN_PROGRESS.equals(statusFromPbp) || ACTIVE.equals(statusFromPbp)) {
              publishNeedCorrectionEvent(storeId, product.getProductCode());
            }
          } catch (Exception e) {
            log.error("Error while fetching product from PBP where productCode {}", product.getProductCode(), e);
          }
        }
        pageable = pageable.next();
      } while (needCorrectionProductsPage.hasNext());
    } catch (Exception e) {
      log.error("Error while reconciliation of need correction products", e);
    }
  }

  private void publishNeedCorrectionEvent(String storeId, String productCode) {
    log.info("Publishing Kafka event : {}, for reconciliation of productCode : {} ",
        DomainEventName.PRODUCT_REVISED_TASK_EVENT_NAME, productCode);
    kafkaProducer.send(DomainEventName.PRODUCT_REVISED_TASK_EVENT_NAME, productCode,
        new PDTNeedRevisionEventModel(storeId, productCode));
  }
}
