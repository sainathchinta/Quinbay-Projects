package com.gdn.mta.bulk.service;

import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.response.AuditTrailListRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@Service
public class UpdateProductHistoryServiceImpl implements UpdateProductHistoryService {

  public static final String HYPHEN = "-";
  @Autowired
  private XProductOutboundService xProductOutboundService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Override
  public void updateProductHistoryDetailList(List<UpdatedProductHistory> updatedProductHistoryList,
      UpdateProductActivity activity) {
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    simpleListStringRequest.setValue(updatedProductHistoryList.stream().map(UpdatedProductHistory::getGdnSku).toList());
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus =
        xProductOutboundService.getItemPickupPointCodeByItemSkus(simpleListStringRequest);

    Map<String, String> itemSkuAndPickupPointCodeMap = new HashMap<>();
    itemPickupPointCodeByItemSkus.getContent().forEach(
        (ItemSkuPickupPointCodeResponse response) -> itemSkuAndPickupPointCodeMap.put(response.getItemSku(),
            response.getPickupPointCode()));

    updatedProductHistoryList.forEach((UpdatedProductHistory updatedProductHistory) -> {
      updatedProductHistory.setOnlineStatus(true);
      updatedProductHistory.setPickupPointCode(
          itemSkuAndPickupPointCodeMap.getOrDefault(updatedProductHistory.getGdnSku(), HYPHEN));
    });

    updatedProductHistoryList.forEach(updatedProductHistory -> {
      AuditTrailListRequest auditTrailListRequest = buildAuditRequest(updatedProductHistory, activity);
      log.info("Publishing the event {} with message {} for updating history of productSku {} ",
          ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, auditTrailListRequest,
          updatedProductHistory.getProductSku());
      kafkaPublisher.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, updatedProductHistory.getProductSku(),
          auditTrailListRequest);
    });

  }

  private AuditTrailListRequest buildAuditRequest(UpdatedProductHistory history, UpdateProductActivity activity) {
    AuditTrailListRequest auditTrailListRequest = new AuditTrailListRequest();
    AuditTrailDto request = getAuditTrailDto(history, activity);
    auditTrailListRequest.setChangedBy(history.getChangedBy());
    auditTrailListRequest.setRequestId(history.getRequestId());
    auditTrailListRequest.setClientId(history.getClientHost());
    auditTrailListRequest.setAccessChannel(Constant.CHANNEL_ID);
    auditTrailListRequest.setUpdateDirectly(true);
    auditTrailListRequest.setAuditTrailResponseList(List.of(request));
    return auditTrailListRequest;
  }

  private static AuditTrailDto getAuditTrailDto(UpdatedProductHistory history, UpdateProductActivity activity) {
    AuditTrailDto request = new AuditTrailDto();
    request.setBusinessPartnerCode(history.getBusinessPartnerCode());
    request.setActionKey(activity.name());
    request.setGdnSku(history.getGdnSku());
    request.setProductSku(history.getProductSku());
    request.setName(history.getGdnName());
    request.setOldValue(history.getOldValues());
    request.setNewValue(history.getNewValues());
    request.setPickupPointCode(history.getPickupPointCode());
    request.setOnlineStatus(Boolean.TRUE);
    return request;
  }
}
