package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.ProductL3RetryListRequest;
import com.gda.mta.product.dto.ProductL3RetryRequest;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductLevel3FailedEntity;
import com.gdn.mta.product.enums.ProductLevel3RetryStatus;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionService;
import com.gdn.partners.product.orchestrator.constant.ProductLevel1State;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.response.SuspensionProductResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.PbpStockAlert;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductStockAlertRepository;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductLeve3ServiceWrapperBean implements ProductLevel3Wrapper {

  private static final String CM_MERCHANT = "CM";
  private static final String NON_CM_MERCHANT = "NON-CM MERCHANT";
  private static final String FAILED_SUSPENSION_ACTION = "Failed to do suspension action for productCodes : {}";
  private static final String AUTHENTICATION_ID = "com.gdn.mta.orchestration";

  @Autowired
  private ProductLevel3Service productLevel3Service;

  @Autowired
  private ProductStockAlertRepository productStockAlertRepository;

  @Autowired
  private ProductMailEventService productMailEventService;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ProductLevel3RetryService productLevel3RetryService;

  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Autowired
  private ProductLevel1CollectionService productLevel1CollectionService;

  @Autowired
  private ProductSystemParameterService productSystemParameterService;

  @Autowired
  private EmailNotificationService emailNotificationService;

  @Value("${max.l3.retry.count:3}")
  private int maxRetryCount;

  @Override
  @Trace(dispatcher = true)
  @Async
  public void bulkArchiveOldOosProducts(String storeId, long bulkArchiveSize, int monthsToArchiveFor,
      Boolean archiveWithoutSendingMail) throws Exception {
    Pageable pageable = PageRequest.of(0, 100);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.MONTH, -(monthsToArchiveFor));
    Page<PbpStockAlert> stockAlerts;
    Map<String, List<List<String>>> emails = new HashMap<>();
    Map<String, String> businessPartnerTypeMapping = new HashMap<>();
    Map<String, ProfileResponse> profileResponseMap = new HashMap<>();
    Long archivedItemsCount = 0L;
    Long archivedItemsCountTillPreviousPage = 0L;
    try {
      GdnPreconditions.checkArgument(bulkArchiveSize > 0, Constants.BULK_ARCHIVE_LIMIT_ERROR_MESSAGE);
      do {
        stockAlerts = this.productStockAlertRepository
            .findByStoreIdAndIsOosTrueAndUpdatedDateLessThanAndMarkForDeleteFalseOrderByUpdatedDateAsc(storeId,
                calendar.getTime(), pageable);
        int failureCount = 0;

        List<String> distinctBusinessPartnerCodes =
            stockAlerts.getContent().stream().map(stockAlert -> stockAlert.getBusinessPartnerCode()).distinct()
                .collect(Collectors.toList());
        filterBusinessPartnerCodes(distinctBusinessPartnerCodes, businessPartnerTypeMapping, profileResponseMap);
        for (PbpStockAlert pbpStockAlert : stockAlerts) {
          try {
            if (distinctBusinessPartnerCodes.contains(pbpStockAlert.getBusinessPartnerCode())
                && archivedItemsCount < bulkArchiveSize) {
              productLevel3Service.toggleArchiveItem(pbpStockAlert.getGdnSku(), Boolean.TRUE);
              archivedItemsCount++;
              if (!archiveWithoutSendingMail) {
                generateEmailsData(emails, pbpStockAlert);
              }
            }
          } catch (Exception e) {
            log.error("Exception caught while archiving item of gdn sku : {}", pbpStockAlert.getGdnSku(), e);
            failureCount++;
          }
        }
        log.info("Successfully archived {} itemSkus, failure count : {}",
            archivedItemsCount - archivedItemsCountTillPreviousPage, failureCount);
        pageable = stockAlerts.nextPageable();
        archivedItemsCountTillPreviousPage = archivedItemsCount;
      } while (stockAlerts.hasNext() && archivedItemsCount < bulkArchiveSize);
      if (!emails.isEmpty()) {
        productMailEventService.sendMailForArchivedItemSkuDueToOos(emails, profileResponseMap);
      }
    } catch (Exception e) {
      log.error("Exception caught while handling the stock alerts", e);
    }
  }

  private void filterBusinessPartnerCodes(List<String> distinctBusinessPartnerCodes,
      Map<String, String> businessPartnerTypeMapping, Map<String, ProfileResponse> profileResponseMap)
      throws Exception {
    ProfileResponse profileResponse = new ProfileResponse();
    Iterator<String> iter = distinctBusinessPartnerCodes.iterator();
    while (iter.hasNext()) {
      String distinctBusinessPartnerCode = iter.next();
      if (!businessPartnerTypeMapping.containsKey(distinctBusinessPartnerCode)) {
        profileResponse =
          businessPartnerRepository.filterDetailByBusinessPartnerCode(distinctBusinessPartnerCode);
        if (CM_MERCHANT.equals(profileResponse.getCompany().getMerchantType()) && !profileResponse.getCompany()
            .isOfflineToOnlineFlag()) {
          profileResponseMap.put(distinctBusinessPartnerCode, profileResponse);
          businessPartnerTypeMapping.put(distinctBusinessPartnerCode, CM_MERCHANT);
        } else {
          iter.remove();
          businessPartnerTypeMapping.put(distinctBusinessPartnerCode, NON_CM_MERCHANT);
        }
      } else {
        if (!CM_MERCHANT.equals(businessPartnerTypeMapping.get(distinctBusinessPartnerCode))) {
          iter.remove();
        }
      }
    }
  }
  private void generateEmailsData(Map<String, List<List<String>>> emails, PbpStockAlert pbpStockAlert) {
    if (emails.containsKey(pbpStockAlert.getBusinessPartnerCode())) {
      emails.get(pbpStockAlert.getBusinessPartnerCode()).get(0).add(pbpStockAlert.getGdnSku());
      emails.get(pbpStockAlert.getBusinessPartnerCode()).get(1).add(pbpStockAlert.getProductName());
    } else {
      List<List<String>> itemData = new ArrayList<>();
      List<String> itemSku = new ArrayList<>();
      itemSku.add(pbpStockAlert.getGdnSku());
      List<String> itemName = new ArrayList<>();
      itemName.add(pbpStockAlert.getProductName());
      itemData.add(itemSku);
      itemData.add(itemName);
      emails.put(pbpStockAlert.getBusinessPartnerCode(), itemData);
    }
  }


  @Override
  public List<SuspensionProductResponse> doBulkProductSuspension(String storeId, String username,
      List<SuspensionProductRequest> suspensionProductRequestList) {
    List<SuspensionProductResponse> failedResponse = new ArrayList<>();
    for (SuspensionProductRequest suspensionProductRequest : suspensionProductRequestList) {
      String productCode = suspensionProductRequest.getProducts().get(0).getProductCode();
      String merchantCode = suspensionProductRequest.getProducts().get(0).getBusinessPartnerCode();
      String productName = suspensionProductRequest.getProducts().get(0).getProductName();
      log.info("Fetching product for suspensionProductRequest : {}", suspensionProductRequest);
      try {
        List<ProductResponse> productResponses =
            productLevel3Service.getProductsByProductCodeAndMerchantCode(productCode, merchantCode);
        if (CollectionUtils.isEmpty(productResponses)) {
          failedResponse.add(
              new SuspensionProductResponse(productCode, productName, merchantCode, Constants.INVALID_PRODUCT_CODE));
          log.error(Constants.INVALID_PRODUCT_CODE, suspensionProductRequest);
          continue;
        }
        addProductListToSuspensionRequest(productResponses, suspensionProductRequest);
        productLevel3Service.doSuspensionProductsActions(storeId, username, suspensionProductRequest);
      } catch (ApplicationRuntimeException e) {
        log.error(FAILED_SUSPENSION_ACTION, suspensionProductRequest, e);
        failedResponse
            .add(new SuspensionProductResponse(productCode, productName, merchantCode, Constants.INVALID_PRODUCT_CODE));
      } catch (Exception e) {
        log.error(FAILED_SUSPENSION_ACTION, suspensionProductRequest, e);
        failedResponse
            .add(new SuspensionProductResponse(productCode, productName, merchantCode, Constants.RETRY_SUSPENSION));
      }
    }
    return failedResponse;
  }

  @Async
  @Override
  public void retryL3CreationJob(String storeId, String requestId, String username, String clientId,
    String channelId) {
    try {
      setMDCParams(storeId, requestId, username, clientId, channelId);
      int maxLimitForL3Retry = Integer.parseInt(
        this.productSystemParameterService.findByStoreIdAndVariable(storeId,
          SystemParameterConstants.MAX_LIMIT_L3_RETRY).getValue());
      List<ProductLevel3FailedEntity> productLevel3FailedEntityList =
        productLevel3RetryService.findProductsForRetryJob(storeId, maxRetryCount,
          maxLimitForL3Retry);
      List<ProductBusinessPartner> productBusinessPartnerList = new ArrayList<>();
      if (CollectionUtils.isNotEmpty(productLevel3FailedEntityList)) {
        productBusinessPartnerList =
          this.productBusinessPartnerService.findByProductSkuList(storeId,
            productLevel3FailedEntityList.stream().map(ProductLevel3FailedEntity::getProductSku)
              .collect(Collectors.toList()));
      }
      for (ProductBusinessPartner productBusinessPartner : productBusinessPartnerList) {
        try {
          ProductCollection productCollection = this.productLevel1CollectionService.findByProductId(
            productBusinessPartner.getProductId());
          if (Objects.nonNull(productCollection) && !productCollection.isMarkForDelete()
            && !productBusinessPartner.isMarkForDelete() && ProductLevel1State.ACTIVE.equals(
            productCollection.getState())) {
            productBusinessPartnerService.retryCreate(storeId, productBusinessPartner.getId(),
              productBusinessPartner);
          } else {
            productLevel3RetryService.updateCompletedOrOmittedState(storeId,
              productBusinessPartner.getGdnProductSku(), ProductLevel3RetryStatus.OMITTED.name());
          }
        } catch (Exception e) {
          log.error("Error while retry of L3 creation, productSku : {} error -",
            productBusinessPartner.getGdnProductSku(), e);
          this.productLevel3RetryService.upsertProductLevel3FailureLog(storeId,
            productBusinessPartner.getGdnProductSku());
        }
      }
      int thresholdLimitForMailer = Integer.parseInt(
        this.productSystemParameterService.findByStoreIdAndVariable(storeId,
          SystemParameterConstants.THRESHOLD_LIMIT_MAILER).getValue());
      List<ProductLevel3FailedEntity> failedRetryProductList =
        productLevel3RetryService.findProductsForSendingMail(storeId, maxRetryCount);
      if (failedRetryProductList.size() >= thresholdLimitForMailer) {
        emailNotificationService.sendFailedRetryProductsMail(failedRetryProductList);
      }
    } catch (Exception e) {
      log.error("Error on L3 retry job, error - ", e);
    }
  }

  @Override
  public void overrideL3Retry(String storeId, String requestId, String username,
    ProductL3RetryListRequest productL3RetryListRequest) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId),
      ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    for (ProductL3RetryRequest productL3RetryRequest : productL3RetryListRequest.getProductL3RetryRequestList()) {
      try {
        productLevel3RetryService.updateRetryProduct(storeId, productL3RetryRequest.getProductSku(),
          productL3RetryRequest.getRetryCount(), productL3RetryRequest.getState());
      } catch (Exception e) {
        log.error("Error on updating L3 retry entry for sku : {}, error - ",
          productL3RetryRequest.getProductSku(), e);
      }
    }
  }

  private void setMDCParams(String storeId, String requestId, String username, String clientId, String channelId) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, channelId);
    MDC.put(GdnMandatoryRequestParameterUtil.AUTHENTICATOR_KEY, AUTHENTICATION_ID);
  }

  private void addProductListToSuspensionRequest(List<ProductResponse> productResponses,
      SuspensionProductRequest suspensionProductRequest) {
    List<ProductLevel3Request> list = new ArrayList<>();
    for (ProductResponse productResponse : productResponses) {
      ProductLevel3Request productLevel3Request = new ProductLevel3Request();
      productLevel3Request.setProductSku(productResponse.getProductSku());
      productLevel3Request.setProductCode(productResponse.getProductCode());
      productLevel3Request.setBusinessPartnerCode(productResponse.getMerchantCode());
      list.add(productLevel3Request);
    }
    suspensionProductRequest.setProducts(list);
  }
}
