package com.gdn.partners.pbp.service.listener;

import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3Service;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.VatUpdateHistoryDomainEventModel;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class VatUpdateExternalHistoryListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductLevel3Service productLevel3Service;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.VAT_UPDATE_EXTERNAL_HISTORY_PUBLISH, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Vat external history event consumed. topic = {}, message = {} ",
        DomainEventName.VAT_UPDATE_EXTERNAL_HISTORY_PUBLISH, message);
    VatUpdateHistoryDomainEventModel vatUpdateHistoryDomainEventModel =
        objectMapper.readValue(message, VatUpdateHistoryDomainEventModel.class);
    try {
      GdnPreconditions
          .checkArgument(Objects.nonNull(vatUpdateHistoryDomainEventModel), ErrorMessages.VAT_HISTORY_EVENT_NULL);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(vatUpdateHistoryDomainEventModel.getProductItemId()),
          ErrorMessages.PRODUCT_ITEM_ID_EMPTY);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(vatUpdateHistoryDomainEventModel.getUpdatedBy()),
          ErrorMessages.UPDATED_BY_EMPTY);
      setMandatoryParameters(vatUpdateHistoryDomainEventModel.getRequestId(),
          vatUpdateHistoryDomainEventModel.getUpdatedBy(), vatUpdateHistoryDomainEventModel.getStoreId());
      productLevel3Service.addVatUpdateExternalHistory(vatUpdateHistoryDomainEventModel);
    } catch (Exception e) {
      log.error("Error on vat external history. Payload received : {}, error - ", vatUpdateHistoryDomainEventModel, e);
    }
  }

  private void setMandatoryParameters(String requestId, String username, String storeId) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.PCB_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constants.WEB_CHANNEL_ID);
    mandatoryParameterHelper.setParameter(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    mandatoryParameterHelper.setParameter(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    mandatoryParameterHelper.setParameter(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.PCB_CLIENT_ID);
  }

}
