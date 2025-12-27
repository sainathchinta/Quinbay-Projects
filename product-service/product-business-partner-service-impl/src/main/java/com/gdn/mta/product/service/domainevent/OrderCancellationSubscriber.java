package com.gdn.mta.product.service.domainevent;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.UUID;

import org.slf4j.MDC;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.OrderCancellationDto;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.service.ProductLevel3V2Wrapper;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.x.neo.order.domain.event.config.DomainEventName;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.partners.pbp.commons.constants.Constants;
import lombok.extern.slf4j.Slf4j;
import com.gdn.x.neo.order.domain.event.model.OrderItemStatusChangedToOfflineCancel;
import com.newrelic.api.agent.Trace;

@Slf4j
@Service
public class OrderCancellationSubscriber {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductLevel3V2Wrapper productLevel3V2Wrapper;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.ORDER_ITEM_STATUS_CHANGED_TO_OFF, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Received event : {} , message : {} ", DomainEventName.ORDER_ITEM_STATUS_CHANGED_TO_OFF, message);
    try {
      OrderItemStatusChangedToOfflineCancel orderCancellationEventModel =
          objectMapper.readValue(message, OrderItemStatusChangedToOfflineCancel.class);
      checkArgument(StringUtils.isNotEmpty(orderCancellationEventModel.getMerchantCommissionType()),
          ErrorMessages.MERCHANT_COMMISSION_TYPE_SHOULD_NOT_BE_NULL_OR_EMPTY);
      if (orderCancellationEventModel.getMerchantCommissionType().equalsIgnoreCase(Constants.CM_MERCHANT)) {
        checkArgument(StringUtils.isNotBlank(orderCancellationEventModel.getStoreId()),
            ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
        setMandatoryParameters(orderCancellationEventModel.getStoreId());
        OrderCancellationDto orderCancellationDto = new OrderCancellationDto();
        BeanUtils.copyProperties(orderCancellationEventModel, orderCancellationDto);
        productLevel3V2Wrapper.updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
      }
    } catch (Exception e) {
      log.error("Error occurred while processing the event : {} , message : {} , error - ",
          DomainEventName.ORDER_ITEM_STATUS_CHANGED_TO_OFF, message, e);
    }
  }

  private void setMandatoryParameters(String storeId) {
    String requestId = UUID.randomUUID().toString();
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constants.DEFAULT_CHANNEL_ID);
    mandatoryParameterHelper.setParameter(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    mandatoryParameterHelper.setParameter(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    mandatoryParameterHelper.setParameter(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    mandatoryParameterHelper.setParameter(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constants.DEFAULT_CHANNEL_ID);
  }
}
