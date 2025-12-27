package com.gdn.mta.product.service.domainevent;

import static com.gdn.mta.product.util.CommonUtils.getUpdateHistoryFromAuditTrailRequest;

import java.util.List;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.response.AuditTrailListRequest;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.service.UpdatedProductHistoryService;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductSkuUpdateHistorySubscriber {

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Autowired
  private ObjectMapper objectMapper;

  @Value("${history.solr.update.new.event}")
  private boolean historySolrUpdateNewEvent;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    AuditTrailListRequest auditTrailRequest = objectMapper.readValue(message, AuditTrailListRequest.class);
    if (auditTrailRequest.isUpdateDirectlyToDB()) {
      List<UpdatedProductHistory> updatedProductHistoryList = getUpdateHistoryFromAuditTrailRequest(auditTrailRequest);
      log.info("Saving history with audit trail request {} ", auditTrailRequest);
      List<UpdatedProductHistory> audit =
          updatedProductHistoryService.createAudit(updatedProductHistoryList, historySolrUpdateNewEvent);
      if (historySolrUpdateNewEvent) {
        productServiceWrapper.publishSolrHistoryUpdateEvent(audit);
      }
    } else {
      log.info("Consume event {} with message : {}", ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY,
          auditTrailRequest.toString());
      try {
        setMandatoryParameters(auditTrailRequest.getChangedBy(), auditTrailRequest.getAccessChannel(),
            auditTrailRequest.getClientId(), auditTrailRequest.getRequestId());
        setTraceParameter(auditTrailRequest.getChangedBy(), auditTrailRequest.getAccessChannel(),
            auditTrailRequest.getClientId(), auditTrailRequest.getRequestId());
        this.productServiceWrapper.updateProductHistoryLevel3Audit(auditTrailRequest.getAuditTrailResponseList(),
            auditTrailRequest.getAccessChannel(), auditTrailRequest.isUpdateDirectly(), historySolrUpdateNewEvent);
      } catch (Exception exp) {
        log.error("Exception caught while processing event {} for user : {} ",
            ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, auditTrailRequest.getChangedBy(), exp);
      }
    }
  }

  private void setMandatoryParameters(String userName,String channelId,String clientId, String requestId) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, Constants.DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, userName);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, channelId);
  }

  private void setTraceParameter(String userName,String channelId,String clientId, String requestId){
    mandatoryParameterHelper.setParameter(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, Constants.DEFAULT_STORE_ID);
    mandatoryParameterHelper.setParameter(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    mandatoryParameterHelper.setParameter(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, userName);
    mandatoryParameterHelper.setParameter(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    mandatoryParameterHelper.setParameter(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, channelId);
  }
}
