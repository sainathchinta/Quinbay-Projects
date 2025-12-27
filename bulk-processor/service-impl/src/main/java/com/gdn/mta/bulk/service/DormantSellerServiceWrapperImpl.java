package com.gdn.mta.bulk.service;

import com.gda.mta.product.dto.response.InProgressProductResponse;
import com.gdn.mta.bulk.DormantSellerStatus;
import com.gdn.mta.bulk.SellerProcessType;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.entity.DormantSellerEvent;
import com.gdn.mta.bulk.entity.DormantSellerProduct;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.util.CommonUtils;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
@Slf4j
public class DormantSellerServiceWrapperImpl implements DormantSellerServiceWrapper {

  public static final int DEFAULT_PAGE_SIZE = 50;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private DormantSellerService dormantSellerService;

  @Autowired
  private XProductOutboundService xProductOutboundService;

  @Autowired
  private PBPOutboundService pbpOutboundService;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Value("${remove.archive.filter.in.seller.termination.flow}")
  private boolean removeArchiveFilterInSellerTerminationFlow;

  @Override
  @Async
  @Trace(dispatcher = true)
  public void processPendingDormantSellerEvent(String storeId, String requestId, String username,
    String processType) {
    String businessPartnerCode = StringUtils.EMPTY;
    try {
      Integer fetchBatchSize = getFetchBatchSizeProcessPendingEvent(storeId, processType);
      Page<DormantSellerEvent> pendingDormantSellerEvents =
        this.dormantSellerService.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(storeId,
          DormantSellerStatus.PENDING.name(), processType, fetchBatchSize);
      pendingDormantSellerEvents.getContent().forEach(
          pendingDormantSellerEvent -> pendingDormantSellerEvent.setStatus(
              DormantSellerStatus.FETCHED.name()));
      List<DormantSellerEvent> fetchedDormantSellerEvents =
          dormantSellerService.saveDormantSellerEvents(pendingDormantSellerEvents.getContent());
      for (DormantSellerEvent dormantSellerEvent : fetchedDormantSellerEvents) {
        try {
          log.info("Starting with seller to fetch products : {} ", dormantSellerEvent.getBusinessPartnerCode());
          businessPartnerCode = dormantSellerEvent.getBusinessPartnerCode();
          ProfileResponse profileResponse =
              businessPartnerRepository.filterByBusinessPartnerCodeV2(storeId, businessPartnerCode);
          if(StringUtils.equalsIgnoreCase(processType, SellerProcessType.DORMANT.name()) && !CommonUtils.isDormantSeller(
              profileResponse)) {
            log.error("Business partner is not dormant : {} ", businessPartnerCode);
            dormantSellerEvent.setStatus(DormantSellerStatus.FAILED.name());
            dormantSellerService.upsertDormantSellerEvent(dormantSellerEvent);
          } else if (Constant.ACTIVE.equals(profileResponse.getMerchantStatus()) && !StringUtils.equalsIgnoreCase(
              processType, SellerProcessType.DORMANT.name())) {
            log.error("Business partner is activated again : {} ", businessPartnerCode);
            dormantSellerEvent.setStatus(DormantSellerStatus.SKIPPED.name());
            dormantSellerService.upsertDormantSellerEvent(dormantSellerEvent);
          } else {
            log.info("Fetching active products of products for seller : {}",
                dormantSellerEvent.getBusinessPartnerCode());
            List<DormantSellerProduct> activeDormantSellerProduct =
                fetchAndSaveActiveProducts(storeId, requestId, username, dormantSellerEvent,
                    processType);
            log.info("Fetching in-progress products of products for seller : {}",
                dormantSellerEvent.getBusinessPartnerCode());
            List<DormantSellerProduct> inProgressDormantSellerProduct =
                fetchAndSaveInProgressProducts(storeId, requestId, username, dormantSellerEvent,
                    processType);
            log.info("Completed fetch of products for seller : {}",
                dormantSellerEvent.getBusinessPartnerCode());
            if (CollectionUtils.isNotEmpty(activeDormantSellerProduct)) {
              log.info("Saving active products for seller : {}",
                  dormantSellerEvent.getBusinessPartnerCode());
              dormantSellerService.saveCollectionInput(activeDormantSellerProduct);
            }
            if (CollectionUtils.isNotEmpty(inProgressDormantSellerProduct)) {
              log.info("Saving in-progress products for seller : {}",
                  dormantSellerEvent.getBusinessPartnerCode());
              dormantSellerService.saveCollectionInput(inProgressDormantSellerProduct);
            }
            log.info("Completed save of products for seller : {}",
                dormantSellerEvent.getBusinessPartnerCode());
            dormantSellerEvent.setStatus(DormantSellerStatus.IN_PROGRESS.name());
            dormantSellerService.upsertDormantSellerEvent(dormantSellerEvent);
          }
        } catch (Exception e) {
          log.error("Error while processing dormant seller event code {} ",
              dormantSellerEvent.getBusinessPartnerCode(), e);
          dormantSellerEvent.setStatus(DormantSellerStatus.FAILED.name());
          dormantSellerService.upsertDormantSellerEvent(dormantSellerEvent);
        }
      }
    } catch (Exception e) {
      log.error("Error while fetching items for status update, businessPartnerCode : {}, error - ",
        businessPartnerCode, e);
    }
  }

  private Integer getFetchBatchSizeProcessPendingEvent(String storeId, String processType) {
    if (StringUtils.equalsIgnoreCase(processType, SellerProcessType.DORMANT.name())) {
      return Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
          SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE).getValue());
    }
    if (StringUtils.equalsIgnoreCase(processType, SellerProcessType.TERMINATED.name())) {
      return Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
          SystemParameterConfigNames.TERMINATED_SELLER_EVENT_FETCH_BATCH_SIZE).getValue());
    }
    if (StringUtils.equalsIgnoreCase(processType, SellerProcessType.SUSPEND.name())) {
      return Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.SUSPEND_SELLER_EVENT_FETCH_BATCH_SIZE).getValue());
    }
    return null;
  }

  private List<DormantSellerProduct> fetchAndSaveInProgressProducts(String storeId,
    String requestId, String username, DormantSellerEvent dormantSellerEvent, String processType) {
    int page = 0;
    Pageable pageable = PageRequest.of(page, DEFAULT_PAGE_SIZE);
    Page<InProgressProductResponse> inProgressProducts =
        this.pbpOutboundService.fetchInProgressProductsByMerchantCode(requestId, username,
          dormantSellerEvent.getBusinessPartnerCode(), pageable);
    return RequestHelper.toDormantSellerInProgressProductList(storeId,
      inProgressProducts.getContent(), dormantSellerEvent.getId(),
      dormantSellerEvent.getBusinessPartnerCode(), processType);
  }

  private List<DormantSellerProduct> fetchAndSaveActiveProducts(String storeId, String requestId,
    String username, DormantSellerEvent dormantSellerEvent, String processType) {
    int page = 0;
    ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();
    if (removeArchiveFilterInSellerTerminationFlow) {
      if (SellerProcessType.DORMANT.name().equalsIgnoreCase(processType)
          || SellerProcessType.SUSPEND.name().equalsIgnoreCase(processType)) {
        // For TERMINATED process fetch products regardless of archived
        productSummaryRequest.setArchived(false);
      }
    } else {
      productSummaryRequest.setArchived(false);
    }
    productSummaryRequest.setMerchantCode(dormantSellerEvent.getBusinessPartnerCode());
    List<ProductL3SummaryResponse> activeProductList = new ArrayList<>();
    Page<ProductL3SummaryResponse> productL3SummaryResponsePage;
    do {
      productL3SummaryResponsePage =
        this.xProductOutboundService.getProductL3SummaryResponse(productSummaryRequest, page,
          DEFAULT_PAGE_SIZE, requestId, username);
      activeProductList.addAll(productL3SummaryResponsePage.getContent());
      page++;
    } while (productL3SummaryResponsePage.hasNext());
    return RequestHelper.toSuspendSellerActiveProductList(storeId, activeProductList,
      dormantSellerEvent, processType);
  }
}
