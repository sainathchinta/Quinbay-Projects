package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoRequestDTO;
import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoResponseDTO;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.partners.pbp.dto.common.ListRequestDTO;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gda.mta.product.dto.B2bFieldsRequest;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3QuickEditV2Request;
import com.gda.mta.product.dto.ProductLevel3ViewConfigStockRequest;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gda.mta.product.dto.response.InProgressProductResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.DormantSellerProductStatus;
import com.gdn.mta.bulk.DormantSellerStatus;
import com.gdn.mta.bulk.SellerProcessType;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.DormantSellerProductUpdateRequest;
import com.gdn.mta.bulk.entity.DormantSellerEvent;
import com.gdn.mta.bulk.entity.DormantSellerProduct;
import com.gdn.mta.bulk.models.DormantSellerItemDetail;
import com.gdn.mta.bulk.repository.DormantSellerEventRepository;
import com.gdn.mta.bulk.repository.DormantSellerProductRepository;
import com.gdn.mta.bulk.repository.ProductLevel3Repository;
import com.gdn.mta.bulk.util.RequestHelper;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class DormantSellerServiceBean implements DormantSellerService {

  public static final int DEFAULT_PAGE_SIZE = 50;
  private static final String EMAIL_OBJECT = "obj";
  private static final String OBJECT = "object";
  private static final Set<String> DORMANT_SELLER_INCOMPLETE_STATUSES =
    Set.of(DormantSellerStatus.PENDING.name(), DormantSellerStatus.IN_PROGRESS.name(),
      DormantSellerStatus.FETCHED.name(), DormantSellerStatus.PROCESSING.name());

  @Autowired
  private DormantSellerEventRepository dormantSellerEventRepository;

  @Autowired
  private DormantSellerProductRepository dormantSellerProductRepository;

  @Autowired
  private PBPOutboundService pbpOutboundService;

  @Autowired
  private XProductOutboundService xProductOutboundService;

  @Autowired
  private SystemParameterConfigService systemParameterConfigService;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private ProductLevel3Repository level3Repository;

  @Autowired
  private InventoryOutboundService inventoryOutboundService;

  @Autowired
  private ApplicationContext applicationContext;

  @Autowired
  private MailDeliveryService mailDeliveryService;

  @Value("${termination.seller.new.flow}")
  private boolean terminationSellerNewFlow;

  @Value("${dormant.seller.retry.status}")
  private List<String> dormantSellerRetryStatusList;

  @Value("${dormant.seller.notify.status}")
  private List<String> dormantSellerNotifyStatusList;

  @Value("${dormant.seller.abort.processes}")
  private List<String> dormantSellerAbortProcesses;

  @Value("${dormant.seller.abort.statuses}")
  private List<String> dormantSellerAbortStatuses;

  @Value("${abort.dormant.seller.stuck.process.after.time.in.seconds}")
  private int abortDormantSellerStuckProcessAfterTimeInSeconds;

  @Value("${dormant.seller.retry.page.size}")
  private int dormantSellerRetryPageSize;

  @Value("${dormant.seller.retry.threshold}")
  private int dormantSellerRetryThreshold;

  @Value("${dormant.seller.failure.notification.mail}")
  private String dormantSellerFailureNotificationMail;

  @Override
  public void processSellerDeactivate(String businessPartnerCode, String sellerProcessType) throws ApplicationException {
    if (StringUtils.isEmpty(businessPartnerCode)) {
      log.error("Error while listening and processing DormantSellerDeactivate businessPartnerCode is empty");
      throw new ApplicationException(ErrorCategory.INVALID_FORMAT,
          "Error while listening and processing " + "processDormantSellerDeactivate businessPartnerCode is empty : "
              + businessPartnerCode);
    }
    log.info("processing Dormant Seller Deactivate for businessPartnerCode : {}", businessPartnerCode);
    DormantSellerEvent dormantSellerEvent = generateBaseObject(Constant.STORE_ID,
      businessPartnerCode);
    dormantSellerEvent.setProcessType(sellerProcessType);
    dormantSellerEventRepository.save(dormantSellerEvent);
  }

  @Async
  @Override
  @Transactional(readOnly = false)
  @Deprecated
  public void processPendingDormantSellerEvent(String storeId, String requestId, String username) {
    try {
      Integer fetchBatchSize = Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.DORMANT_SELLER_EVENT_FETCH_BATCH_SIZE).getValue());
      Page<DormantSellerEvent> pendingDormantSellerEvents =
        this.dormantSellerEventRepository.findByStoreIdAndStatusAndMarkForDeleteFalse(storeId,
          DormantSellerStatus.PENDING.name(), PageRequest.of(0, fetchBatchSize));
      for (DormantSellerEvent dormantSellerEvent : pendingDormantSellerEvents) {
        fetchAndSaveActiveProducts(storeId, requestId, username, dormantSellerEvent);
        fetchAndSaveInProgressProducts(storeId, requestId, username, dormantSellerEvent);
        dormantSellerEvent.setStatus(DormantSellerStatus.IN_PROGRESS.name());
        dormantSellerEventRepository.save(dormantSellerEvent);
      }
    } catch (Exception e) {
      log.error("Error while fetching items for status update, error - ", e);
    }
  }

  @Override
  public void updateProductItemViewConfig(DormantSellerItemDetail dormantSellerItemDetail) throws Exception {
    log.info("Update product Item viewConfig for itemSku : {} status : {}", dormantSellerItemDetail.getItemSku(),
        dormantSellerItemDetail.getItemStatus());
    if (StringUtils.isEmpty(dormantSellerItemDetail.getItemSku()) || StringUtils.isEmpty(
        dormantSellerItemDetail.getItemStatus())) {
      log.error("ItemSku : {} or status : {} is empty", dormantSellerItemDetail.getItemSku(),
          dormantSellerItemDetail.getItemStatus());
      throw new ApplicationException(ErrorCategory.VALIDATION, "ItemSku or status is empty");
    }
    DormantSellerProduct dormantSellerProduct = dormantSellerProductRepository
        .findByItemSkuAndStatusAndProcessTypeAndProductStatusAndDormantSellerEventIdAndMarkForDeleteFalse(
            dormantSellerItemDetail.getItemSku(), DormantSellerStatus.IN_PROGRESS.name(),
            dormantSellerItemDetail.getProcessType(), dormantSellerItemDetail.getItemStatus(),
            dormantSellerItemDetail.getDormantSellerEventId());
    if (Objects.isNull(dormantSellerProduct)) {
      log.error(
          "Duplicate/Invalid Dormant Product found for itemSku : {} status : {} processType : {} "
              + "dormantSellerEventId : {}", dormantSellerItemDetail.getItemSku(),
          dormantSellerItemDetail.getItemStatus(), dormantSellerItemDetail.getProcessType(),
          dormantSellerItemDetail.getDormantSellerEventId());
      throw new ApplicationException(ErrorCategory.VALIDATION,
          BulkProcessValidationErrorMessages.INVALID_DORMANT_SELLER_PRODUCT);
    }
    dormantSellerProduct.setStatus(DormantSellerStatus.PROCESSING.name());
    dormantSellerProduct.setUpdatedDate(new Date());
    dormantSellerProduct = dormantSellerProductRepository.save(dormantSellerProduct);
    boolean success = updateItemViewConfigInPBPOrXproduct(dormantSellerItemDetail);
    dormantSellerProduct.setUpdatedDate(new Date());
    if (success) {
      dormantSellerProduct.setStatus(DormantSellerStatus.COMPLETED.name());
    } else {
      dormantSellerProduct.setStatus(DormantSellerStatus.FAILED.name());
    }
    dormantSellerProductRepository.save(dormantSellerProduct);
  }

  private boolean updateItemViewConfigInPBPOrXproduct(DormantSellerItemDetail dormantSellerItemDetail) {
    boolean success = false;
    if (StringUtils.equalsIgnoreCase(dormantSellerItemDetail.getItemStatus(),
        DormantSellerProductStatus.ACTIVE.name())) {
      if (SellerProcessType.DORMANT.name().equals(dormantSellerItemDetail.getProcessType())) {
        ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest =
            ItemPickupPointViewConfigBaseRequest.builder().buyable(false).discoverable(false).cncActivated(false)
                .build();
        try {
          xProductOutboundService.updateItemPickupPointViewConfigWithProductStatus(dormantSellerItemDetail.getItemSku(),
              itemPickupPointViewConfigBaseRequest);
          success = true;
        } catch (Exception e) {
          log.error(
              "Error while updating viewConfig in x-product itemSku : {} , dormantSellerEventId : {} , businessPartnerCode : {}, error - ",
              dormantSellerItemDetail.getItemSku(), dormantSellerItemDetail.getDormantSellerEventId(),
              dormantSellerItemDetail.getBusinessPartnerCode(), e);
        }
      } else if (SellerProcessType.SUSPEND.name().equals(dormantSellerItemDetail.getProcessType())) {
        try {
          xProductOutboundService.archiveByProductSku(dormantSellerItemDetail.getItemSku(), true, true);
          success = true;
        } catch (Exception e) {
          log.error(
              "Error while updating archive flag for itemSku : {}, dormantSellerEventId : {} , businessPartnerCode : {}, error - ",
              dormantSellerItemDetail.getItemSku(), dormantSellerItemDetail.getDormantSellerEventId(),
              dormantSellerItemDetail.getBusinessPartnerCode(), e);
        }
      } else if (SellerProcessType.TERMINATED.name()
        .equals(dormantSellerItemDetail.getProcessType())) {
        log.info("resetting product status flag for terminated merchant itemSku : {}",
          dormantSellerItemDetail.getItemSku());
        if (!updateProductStatusForTerminatedMerchant(dormantSellerItemDetail)) {
          return false;
        }
        success = true;
      }
    } else {
      ProductLevel3ViewConfigStockRequest productLevel3ViewConfigStockRequest =
          new ProductLevel3ViewConfigStockRequest();
      productLevel3ViewConfigStockRequest.setDisplay(false);
      productLevel3ViewConfigStockRequest.setBuyable(false);
      productLevel3ViewConfigStockRequest.setCncActive(false);
      productLevel3ViewConfigStockRequest.setB2bBuyable(false);
      productLevel3ViewConfigStockRequest.setB2bDisplay(false);
      if (SellerProcessType.TERMINATED.name().equals(dormantSellerItemDetail.getProcessType())) {
        productLevel3ViewConfigStockRequest.setAvailableStock(0);
      }
      productLevel3ViewConfigStockRequest.setChannelId(Constant.CHANNEL_ID);
      try {
        if (terminationSellerNewFlow && SellerProcessType.TERMINATED.name()
            .equals(dormantSellerItemDetail.getProcessType())) {
          pbpOutboundService.deleteTerminatedSellerProducts(dormantSellerItemDetail.getItemSku());
        } else {
          pbpOutboundService.updateProductItemViewConfig(dormantSellerItemDetail.getItemSku(),
              productLevel3ViewConfigStockRequest);
          if (SellerProcessType.TERMINATED.name().equals(dormantSellerItemDetail.getProcessType())) {
            pbpOutboundService.deleteTerminatedSellerProducts(dormantSellerItemDetail.getItemSku());
          }
        }
        success = true;
      } catch (Exception e) {
        log.error(
            "Error while updating viewConfig in pbp itemSku : {} , dormantSellerEventId : {} , businessPartnerCode : {}, error - ",
            dormantSellerItemDetail.getItemSku(), dormantSellerItemDetail.getDormantSellerEventId(),
            dormantSellerItemDetail.getBusinessPartnerCode(), e);
      }
    }
    return success;
  }

  private boolean updateProductStatusForTerminatedMerchant(DormantSellerItemDetail dormantSellerItemDetail) {
    boolean sharedProduct = false;
    if (terminationSellerNewFlow) {
      sharedProduct = xProductOutboundService.isSharedProduct(Constant.STORE_ID,
        dormantSellerItemDetail.getItemSku(), dormantSellerItemDetail.getBusinessPartnerCode());
    }
    if (terminationSellerNewFlow && !sharedProduct) {
      return deleteProduct(dormantSellerItemDetail);
    } else {
    try {
      ItemPickupPointListingRequest itemPickupPointListingRequest =
        ItemPickupPointListingRequest.builder().pickupPointCodes(Collections.emptySet())
          .businessPartnerCode(dormantSellerItemDetail.getBusinessPartnerCode())
          .productSku(dormantSellerItemDetail.getItemSku()).build();
      Page<ItemPickupPointListingResponse> itemPickupPointList;
      Pageable pageable = PageRequest.of(0, DEFAULT_PAGE_SIZE);
      do {
        itemPickupPointList = this.xProductOutboundService.getItemPickupPointList(pageable,
          itemPickupPointListingRequest);
        if (CollectionUtils.isEmpty(itemPickupPointList.getContent())) {
          return true;
        }
        List<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTOList =
          itemPickupPointList.getContent().stream().map(RequestHelper::inventoryDetailInfoRequestDTO).collect(Collectors.toList());
        Map<String, InventoryDetailInfoResponseDTO> itemPickupPointToInventoryMap = Optional.ofNullable(
            this.inventoryOutboundService.findDetailByWebMerchantCodeAndWebItemSku(new ListRequestDTO<>(inventoryDetailInfoRequestDTOList))).orElse(new ArrayList<>())
          .stream().filter(inventoryDetailInfoResponseDTO -> Objects.nonNull(
            inventoryDetailInfoResponseDTO.getWebInventoryResponse())).collect(Collectors.toMap(
            inventoryDetailInfoResponseDTO -> RequestHelper.toL5Id(inventoryDetailInfoResponseDTO.getWebItemSku(),
              inventoryDetailInfoResponseDTO.getWebInventoryResponse().getPickupPointCode()),
            Function.identity()));
        List<QuickEditV2Request> quickEditV2RequestList = new ArrayList<>();
        for (ItemPickupPointListingResponse itemPickupPointListingResponse : itemPickupPointList) {
          QuickEditV2Request quickEditV2Request = new QuickEditV2Request();
          quickEditV2Request.setPrice(new ProductLevel3PriceRequest());
          quickEditV2Request.setStatus(ProductLevel3Status.OFFLINE);
          quickEditV2Request.setSellerSku(itemPickupPointListingResponse.getMerchantSku());
          if (itemPickupPointToInventoryMap.containsKey(RequestHelper.toL5Id(itemPickupPointListingResponse.getItemSku(),
            itemPickupPointListingResponse.getPickUpPointCode()))) {
            quickEditV2Request.setDeltaStock(-itemPickupPointToInventoryMap.get(RequestHelper.toL5Id(itemPickupPointListingResponse.getItemSku(),
              itemPickupPointListingResponse.getPickUpPointCode())).getWebInventoryResponse().getAvailableStock());
          }
          quickEditV2Request.setItemSku(itemPickupPointListingResponse.getItemSku());
          quickEditV2Request.setPickupPointCode(itemPickupPointListingResponse.getPickUpPointCode());
          quickEditV2Request.setCncActive(false);
          if (Objects.nonNull(itemPickupPointListingResponse.getB2bFields())) {
            B2bFieldsRequest b2bFieldsRequest = new B2bFieldsRequest();
            b2bFieldsRequest.setBasePrice(itemPickupPointListingResponse.getB2bFields().getBasePrice());
            b2bFieldsRequest.setManaged(itemPickupPointListingResponse.getB2bFields().isManaged());
            b2bFieldsRequest.setStatus(ProductLevel3Status.OFFLINE);
            quickEditV2Request.setB2bFieldsRequest(b2bFieldsRequest);
          }
          quickEditV2RequestList.add(quickEditV2Request);
        }
        ProductLevel3QuickEditV2Request productLevel3QuickEditV2Request = new ProductLevel3QuickEditV2Request();
        productLevel3QuickEditV2Request.setQuickEditV2Requests(quickEditV2RequestList);
        pbpOutboundService.listingUpdate(productLevel3QuickEditV2Request, dormantSellerItemDetail.getItemSku());
        pageable = itemPickupPointList.nextPageable();
      } while (itemPickupPointList.hasNext());
      pbpOutboundService.deleteTerminatedSellerProducts(dormantSellerItemDetail.getItemSku());
      return true;
    } catch (Exception e) {
      log.error("Error on terminated seller's item update : {}, error - ",
        dormantSellerItemDetail.getItemSku(), e);
      return false;
    }
    }
  }

  private boolean deleteProduct(DormantSellerItemDetail dormantSellerItemDetail) {
    try {
      pbpOutboundService.deleteTerminatedSellerProducts(dormantSellerItemDetail.getItemSku());
    } catch (Exception e) {
      log.error("Error on terminated seller's deletion process : {}, error - ", dormantSellerItemDetail.getItemSku(),
          e);
      return false;
    }
    return true;
  }

  @Async
  @Override
  @Trace(dispatcher = true)
  public void updateDormantSellerStatus(String storeId, String requestId, String username) {
    try {
      int sellerStatusUpdateLimit = Integer.parseInt(this.systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.DORMANT_SELLER_STATUS_UPDATE_BATCH_SIZE).getValue());
      Page<DormantSellerEvent> dormantSellerEventBatch =
        dormantSellerEventRepository.findByStoreIdAndStatusAndMarkForDeleteFalse(storeId,
          DormantSellerStatus.IN_PROGRESS.name(), PageRequest.of(0, sellerStatusUpdateLimit));
      List<DormantSellerEvent> updatedDormantSellerEventList = new ArrayList<>();
      if (CollectionUtils.isEmpty(dormantSellerEventBatch.getContent())) {
        log.warn("All the seller status are up to date, No update required");
        return;
      }
      for (DormantSellerEvent dormantSellerEvent : dormantSellerEventBatch.getContent()) {
        log.info("Updating status for Business partner code {}, status : {}", dormantSellerEvent.getBusinessPartnerCode(),
          dormantSellerEvent.getStatus());
        checkProductStatusAndUpdateSellerEvent(username, updatedDormantSellerEventList,
          dormantSellerEvent);
      }
      if (CollectionUtils.isNotEmpty(updatedDormantSellerEventList)) {
        dormantSellerEventRepository.saveAll(updatedDormantSellerEventList);
      }
    } catch (Exception e) {
      log.error("Error updating seller status. Error - ", e);
    }
  }

  @Trace(dispatcher=true)
  @Override
  @Async
  public void deleteDormantSellerProduct(String storeId, String requestId, String username, int pageSize, int batchSize,
      int days) {
    Pageable pageable = PageRequest.of(0, pageSize);
    Date updateDate = DateUtils.addDays(new Date(), -days);
    log.info("Deleting products older than updates date : {} page size : {} batch size : {}", updateDate, pageSize,
        batchSize);
    int count = 0;
    do {
      Page<DormantSellerProduct> dormantSellerProductList =
          dormantSellerProductRepository.findByUpdatedDateAndMarkForDeleteTrue(updateDate, pageable);
      if (CollectionUtils.isEmpty(dormantSellerProductList.getContent())) {
        log.info("No more product to delete older than {} days username : {}", days, username);
        break;
      }
      count = count + dormantSellerProductList.getContent().size();
      dormantSellerProductRepository.deleteAll(dormantSellerProductList.getContent());
    } while (count < batchSize);
    log.info("Deletion of dormant sellers products is done");
  }

  @Override
  public void overrideDormantSellerProductStatus(String storeId, String requestId, String username,
    List<DormantSellerProductUpdateRequest> dormantSellerProductUpdateRequestList) {
    if (CollectionUtils.isEmpty(dormantSellerProductUpdateRequestList)) {
      return;
    }
    Map<String, String> itemSkuToStatusMap = dormantSellerProductUpdateRequestList.stream().collect(
      Collectors.toMap(DormantSellerProductUpdateRequest::getItemSku, DormantSellerProductUpdateRequest::getStatus));
    List<DormantSellerProduct> dormantSellerProductList =
      this.dormantSellerProductRepository.findByItemSkuIn(new ArrayList<>(itemSkuToStatusMap.keySet()));
    dormantSellerProductList.forEach(
      dormantSellerProduct -> dormantSellerProduct.setStatus(itemSkuToStatusMap.get(dormantSellerProduct.getItemSku())));
    dormantSellerProductRepository.saveAll(dormantSellerProductList);
  }

  @Override
  public void updateDormantSellerEvent(String storeId, String requestId, String username, String status,
      List<String> businessPartnerCodes, boolean abortDataEntries) {
    log.info("Started updateDormantSellerEvent requestId : {}", requestId);
    if (abortDataEntries) {
      List<DormantSellerProduct> dormantSellerProducts =
          dormantSellerProductRepository.findByStoreIdAndStatusInAndProcessTypeInAndMarkForDeleteFalseAndUpdatedDateBefore(
              storeId, dormantSellerAbortStatuses, dormantSellerAbortProcesses,
              DateUtils.addSeconds(new Date(), -abortDormantSellerStuckProcessAfterTimeInSeconds));
      dormantSellerProducts = dormantSellerProducts.stream()
          .map(DormantSellerServiceBean::setMarkForDeleteTrueAndStatusFailed).toList();
      dormantSellerProductRepository.saveAll(dormantSellerProducts);
    } else {
      List<DormantSellerEvent> dormantSellerEventList =
          dormantSellerEventRepository.findByStoreIdAndBusinessPartnerCodeIn(storeId,
              businessPartnerCodes);
      dormantSellerEventList.stream()
          .forEach(dormantSellerEvent -> dormantSellerEvent.setStatus(status));
      dormantSellerEventRepository.saveAll(dormantSellerEventList);
    }
    log.info("Done updateDormantSellerEvent requestId : {}", requestId);
  }

  private static DormantSellerProduct setMarkForDeleteTrueAndStatusFailed(
      DormantSellerProduct dormantSellerProduct) {
    dormantSellerProduct.setMarkForDelete(true);
    dormantSellerProduct.setStatus(DormantSellerStatus.FAILED.name());
    dormantSellerProduct.setUpdatedBy(Constant.DORMANT_SELLER_ABORTED_BY);
    return dormantSellerProduct;
  }

  private void checkProductStatusAndUpdateSellerEvent(String username,
      List<DormantSellerEvent> updatedDormantSellerEventList, DormantSellerEvent dormantSellerEvent) {
    List<DormantSellerProduct> dormantSellerProductList =
        dormantSellerProductRepository.findByDormantSellerEventIdAndMarkForDeleteFalse(dormantSellerEvent.getId());
    if (CollectionUtils.isEmpty(dormantSellerProductList)) {
      log.warn("No active product found for the businessPartner : {} ", dormantSellerEvent.getBusinessPartnerCode());
      dormantSellerEvent.setUpdatedDate(new Date());
      dormantSellerEvent.setUpdatedBy(username);
      dormantSellerEvent.setStatus(DormantSellerStatus.COMPLETED.name());
      dormantSellerEvent.setMarkForDelete(true);
      updatedDormantSellerEventList.add(dormantSellerEvent);
      return;
    }
    List<String> dormantSellerProductStatusList =
        dormantSellerProductList.stream().map(DormantSellerProduct::getStatus).distinct().collect(Collectors.toList());
    if (dormantSellerProductStatusList.stream()
      .anyMatch(DORMANT_SELLER_INCOMPLETE_STATUSES::contains)) {
      return;
    }
    if (dormantSellerProductStatusList.contains(DormantSellerStatus.COMPLETED.name())) {
      if (!dormantSellerProductStatusList.contains(DormantSellerStatus.FAILED.name())) {
        dormantSellerEvent.setStatus(DormantSellerStatus.COMPLETED.name());
        dormantSellerEvent.setMarkForDelete(true);
      } else {
        dormantSellerEvent.setStatus(DormantSellerStatus.PARTIAL_COMPLETED.name());
      }
    } else {
      dormantSellerEvent.setStatus(DormantSellerStatus.FAILED.name());
    }
    dormantSellerEvent.setUpdatedDate(new Date());
    dormantSellerEvent.setUpdatedBy(username);
    if (dormantSellerEvent.isMarkForDelete()) {
      dormantSellerProductList.forEach(dormantSellerProduct -> dormantSellerProduct.setMarkForDelete(true));
      dormantSellerProductRepository.saveAll(dormantSellerProductList);
    }
    updatedDormantSellerEventList.add(dormantSellerEvent);
  }

  private void fetchAndSaveInProgressProducts(String storeId, String requestId, String username,
    DormantSellerEvent dormantSellerEvent) {
    int page = 0;
    List<InProgressProductResponse> inProgressProductResponseList = new ArrayList<>();
    Page<InProgressProductResponse> inProgressProducts = null;
    Pageable pageable = PageRequest.of(page, DEFAULT_PAGE_SIZE);
    do {
      inProgressProducts = this.pbpOutboundService.fetchInProgressProductsByMerchantCode(requestId, username,
        dormantSellerEvent.getBusinessPartnerCode(), pageable);
      inProgressProductResponseList.addAll(inProgressProducts.getContent());
      pageable = PageRequest.of(inProgressProducts.getNumber() + 1, inProgressProducts.getSize());
    } while (CollectionUtils.isNotEmpty(inProgressProducts.getContent()));
    List<DormantSellerProduct> dormantSellerProducts =
      RequestHelper.toDormantSellerInProgressProductList(storeId, inProgressProductResponseList,
        dormantSellerEvent.getId(), dormantSellerEvent.getBusinessPartnerCode(), null);
    dormantSellerProductRepository.saveAll(dormantSellerProducts);
  }

  private void fetchAndSaveActiveProducts(String storeId, String requestId, String username, DormantSellerEvent dormantSellerEvent) {
    int page = 0;
    List<ItemSummaryResponse> activeItemList = new ArrayList<>();
    ItemSummaryRequest itemSummaryRequest = new ItemSummaryRequest();
    itemSummaryRequest.setArchived(false);
    itemSummaryRequest.setMerchantCode(dormantSellerEvent.getBusinessPartnerCode());
    Page<ItemSummaryResponse> itemSummaryResponses = null;
    Pageable pageable = PageRequest.of(page, DEFAULT_PAGE_SIZE);
    do {
      itemSummaryResponses = xProductOutboundService.getItemSummaryByFilter(requestId, username, pageable, itemSummaryRequest);
      activeItemList.addAll(itemSummaryResponses.getContent());
      pageable = itemSummaryResponses.nextPageable();
    } while (itemSummaryResponses.hasNext());
    List<DormantSellerProduct> dormantSellerProducts =
      RequestHelper.toDormantSellerActiveProductList(storeId, activeItemList, dormantSellerEvent.getId(),
        dormantSellerEvent.getBusinessPartnerCode(), dormantSellerEvent.getProcessType());
    dormantSellerProductRepository.saveAll(dormantSellerProducts);
  }

  @Async
  @Override
  @Trace(dispatcher = true)
  public void updateViewConfigForItemsOfDormantSeller(String storeId, String requestId,
    String username, String processType) {
    try {
      Integer fetchBatchSize = getFetchBatchSize(storeId, processType);
      Integer maxToProcess = getMaxToProcess(storeId, processType);
      Pageable pageable;
      Page<DormantSellerProduct> dormantSellerProductPage;
      int publishedCount = 0;
      do {
        if (fetchBatchSize > maxToProcess - publishedCount) {
          fetchBatchSize = maxToProcess - publishedCount;
        }
        pageable = PageRequest.of(0, fetchBatchSize);
        dormantSellerProductPage =
          this.dormantSellerProductRepository.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(
            storeId, DormantSellerStatus.PENDING.name(), processType, pageable);
        publishViewConfigEventAndUpdateStatus(dormantSellerProductPage);
        publishedCount += fetchBatchSize;
      } while (dormantSellerProductPage.hasNext() && (publishedCount < maxToProcess));
    } catch (Exception e) {
      log.error("Error while publishing events for PENDING items, error - ", e);
    }
  }

  private Integer getMaxToProcess(String storeId, String processType) {
    if (SellerProcessType.DORMANT.name().equals(processType)) {
      return Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.DORMANT_SELLER_PRODUCT_UPDATE_MAX_SIZE).getValue());
    }
    if (SellerProcessType.SUSPEND.name().equals(processType)) {
      return Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.SUSPEND_SELLER_PRODUCT_UPDATE_MAX_SIZE).getValue());
    }
    if (SellerProcessType.TERMINATED.name().equals(processType)) {
      return Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.TERMINATED_SELLER_PRODUCT_UPDATE_MAX_SIZE).getValue());
    }
    return null;
  }

  private Integer getFetchBatchSize(String storeId, String processType) {
    if (SellerProcessType.DORMANT.name().equals(processType)) {
      return Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.DORMANT_SELLER_PRODUCT_FETCH_BATCH_SIZE).getValue());
    }
    if (SellerProcessType.SUSPEND.name().equals(processType)) {
      return Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.SUSPEND_SELLER_PRODUCT_FETCH_BATCH_SIZE).getValue());
    }
    if (SellerProcessType.TERMINATED.name().equals(processType)) {
      return Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.TERMINATED_SELLER_PRODUCT_FETCH_BATCH_SIZE).getValue());
    }
    return null;
  }

  private void publishViewConfigEventAndUpdateStatus(Page<DormantSellerProduct> dormantSellerProductPage) {
    dormantSellerProductPage.getContent().forEach(
      dormantSellerProduct -> dormantSellerProduct.setStatus(DormantSellerStatus.FETCHED.name()));
    List<DormantSellerProduct> updatedDormantSellerProductList =
      this.dormantSellerProductRepository.saveAll(dormantSellerProductPage.getContent());
    List<DormantSellerItemDetail> dormantSellerItemDetailList = new ArrayList<>();
    for (DormantSellerProduct dormantSellerProduct : updatedDormantSellerProductList) {
      DormantSellerItemDetail dormantSellerItemDetail =
          DormantSellerItemDetail.builder().itemSku(dormantSellerProduct.getItemSku())
              .dormantSellerEventId(dormantSellerProduct.getDormantSellerEventId())
              .itemStatus(dormantSellerProduct.getProductStatus()).processType(dormantSellerProduct.getProcessType())
              .businessPartnerCode(dormantSellerProduct.getBusinessPartnerCode()).build();
      dormantSellerProduct.setStatus(DormantSellerStatus.IN_PROGRESS.name());
      dormantSellerItemDetailList.add(dormantSellerItemDetail);
    }
    dormantSellerProductRepository.saveAll(updatedDormantSellerProductList);
    dormantSellerItemDetailList.forEach(
        dormantSellerItemDetail -> kafkaProducer.send(kafkaTopicProperties.getDormantSellerItemSkuViewConfigUpdate(),
            dormantSellerItemDetail));
  }

  @Override
  public void processResignSellerEvent(String storeId, String businessPartnerCode)
    throws ApplicationException {
    if (StringUtils.isEmpty(businessPartnerCode)) {
      log.error("Error while listening and processing ResignSeller businessPartnerCode is empty");
      throw new ApplicationException(ErrorCategory.INVALID_FORMAT,
        "Error while listening and processing " + "processDormantSellerDeactivate businessPartnerCode is empty : "
          + businessPartnerCode);
    }
    log.info("processing Dormant Seller Deactivate for businessPartnerCode : {}", businessPartnerCode);
    DormantSellerEvent dormantSellerEvent = generateBaseObject(storeId, businessPartnerCode);
    dormantSellerEvent.setProcessType(SellerProcessType.SUSPEND.name());
    dormantSellerEventRepository.save(dormantSellerEvent);
  }

  @Override
  public Page<DormantSellerEvent> findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(String storeId,
    String status, String processType, Integer fetchBatchSize) {
    return this.dormantSellerEventRepository.findByStoreIdAndStatusAndProcessTypeAndMarkForDeleteFalse(
      storeId, DormantSellerStatus.PENDING.name(), processType, PageRequest.of(0, fetchBatchSize));
  }

  @Override
  @Transactional(readOnly = false)
  public void saveCollectionInput(List<DormantSellerProduct> dormantSellerProductList) {
    this.dormantSellerProductRepository.saveAll(dormantSellerProductList);
  }

  @Override
  @Transactional(readOnly = false)
  public DormantSellerEvent upsertDormantSellerEvent(DormantSellerEvent dormantSellerEvent) {
    return this.dormantSellerEventRepository.save(dormantSellerEvent);
  }

  @Override
  public DormantSellerEvent findByBusinessPartnerCodeAndProcessType(String businessPartnerCode, String processType) {
    return dormantSellerEventRepository.findFirstByBusinessPartnerCodeAndProcessType(businessPartnerCode, processType);
  }

  @Async
  @Override
  public void retryDormantSeller(String storeId) {
    int retryThresholdInMinutes = Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.DORMANT_SELLER_RETRY_THRESHOLD_TIME_IN_MINUTES).getValue());
    List<DormantSellerEvent> dormantSellerEventList = getDormantSellerEvents(storeId, retryThresholdInMinutes, dormantSellerRetryStatusList,
        false);
    List<DormantSellerEvent> updatedSellerEventList = updateDormantSellerIfApplicable(dormantSellerEventList);
    log.info("Retry for dormant seller is completed, below process were sent back to PENDING state : {} ",
        updatedSellerEventList);
  }

  private List<DormantSellerEvent> getDormantSellerEvents(String storeId, int thresholdInMinutes,
      List<String> states, boolean fetchRecordsUpdatedAfterThreshold) {
    Pageable pageable = PageRequest.of(0, dormantSellerRetryPageSize);
    Date updateDate = DateUtils.addMinutes(new Date(), -thresholdInMinutes);
    log.info(
        "Fetching dormant seller based on updated date : {} page size : {} , fetchRecordsUpdatedAfterThreshold : {}",
        updateDate, dormantSellerRetryPageSize, fetchRecordsUpdatedAfterThreshold);
    Page<DormantSellerEvent> dormantSellerEventPage = null;
    List<DormantSellerEvent> dormantSellerEventList = new ArrayList<>();
    do {
      if (fetchRecordsUpdatedAfterThreshold) {
        dormantSellerEventPage =
            dormantSellerEventRepository.findByStoreIdAndStatesInAndUpdatedDateAfterAndMarkForDeleteFalse(storeId,
                states, updateDate, pageable);
      } else {
        dormantSellerEventPage =
            dormantSellerEventRepository.findByStoreIdAndStatesInAndUpdatedDateAndMarkForDeleteFalse(storeId, states,
                updateDate, pageable);
      }
      dormantSellerEventList.addAll(dormantSellerEventPage.getContent());
      pageable = PageRequest.of(dormantSellerEventPage.getNumber() + 1, dormantSellerRetryPageSize);
    } while (dormantSellerEventPage.hasNext());
    return dormantSellerEventList;
  }

  private List<DormantSellerEvent> updateDormantSellerIfApplicable(List<DormantSellerEvent> dormantSellerEventList) {
    List<DormantSellerEvent> updatedSellerEventList = new ArrayList<>();
    for (DormantSellerEvent dormantSellerEvent : dormantSellerEventList) {
      if (dormantSellerEvent.getRetryCount() < dormantSellerRetryThreshold) {
        dormantSellerEvent.setRetryCount(dormantSellerEvent.getRetryCount() + 1);
        dormantSellerEvent.setStatus(DormantSellerStatus.PENDING.name());
        updatedSellerEventList.add(dormantSellerEvent);
      }
    }
    if (CollectionUtils.isNotEmpty(updatedSellerEventList)) {
      getDormantSellerServiceBean().saveDormantSellerEvents(updatedSellerEventList);
    }
    return updatedSellerEventList;
  }

  private DormantSellerServiceBean getDormantSellerServiceBean() {
    return applicationContext.getBean(DormantSellerServiceBean.class);
  }

  @Override
  public List<DormantSellerEvent> findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(String storeId,
      String businessPartnerCode, String processType) {
    return dormantSellerEventRepository
        .findByStoreIdAndBusinessPartnerCodeAndProcessTypeAndMarkForDeleteFalse(storeId, businessPartnerCode,
            processType);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public List<DormantSellerEvent> saveDormantSellerEvents(List<DormantSellerEvent> dormantSellerEvents) {
    return dormantSellerEventRepository.saveAll(dormantSellerEvents);
  }

  private DormantSellerEvent generateBaseObject(String storeId, String businessPartnerCode) {
    DormantSellerEvent dormantSellerEvent = new DormantSellerEvent();
    dormantSellerEvent.setBusinessPartnerCode(businessPartnerCode);
    dormantSellerEvent.setStatus(DormantSellerStatus.PENDING.name());
    dormantSellerEvent.setCreatedBy(Constant.USER_NAME);
    dormantSellerEvent.setCreatedDate(new Date());
    dormantSellerEvent.setUpdatedBy(Constant.USER_NAME);
    dormantSellerEvent.setUpdatedDate(new Date());
    dormantSellerEvent.setMarkForDelete(false);
    dormantSellerEvent.setStoreId(storeId);
    return dormantSellerEvent;
  }

  @Override
  @Async
  public void notifyStuckDormantProcess(String storeId) {
    int notifyThreshold = Integer.parseInt(systemParameterConfigService.findValueByStoreIdAndVariable(storeId,
        SystemParameterConfigNames.DORMANT_SELLER_NOTIFY_THRESHOLD_TIME_IN_MINUTES).getValue());
    List<DormantSellerEvent> dormantSellerEventList =
        getDormantSellerEvents(storeId, notifyThreshold, dormantSellerNotifyStatusList, true);
    if (CollectionUtils.isNotEmpty(dormantSellerEventList)) {
      Map<String, Object> mailObjectWrapper = new HashMap<>();
      Map<String, Object> emailObject = new HashMap<>();
      mailObjectWrapper.put(OBJECT, dormantSellerEventList);
      emailObject.put(EMAIL_OBJECT, mailObjectWrapper);
      log.info("Mail sent for stuck bulk process, id  : {} , subject : {} , object : {} ",
          EmailConstants.DORMANT_SELLER_NOTIFICATION, EmailConstants.DORMANT_SELLER_STUCK_PROCESS, emailObject);
      mailDeliveryService.sendMail(emailObject, EmailConstants.DORMANT_SELLER_NOTIFICATION,
          EmailConstants.DORMANT_SELLER_STUCK_PROCESS, dormantSellerFailureNotificationMail, Constant.SYSTEM);
    }
  }
}
