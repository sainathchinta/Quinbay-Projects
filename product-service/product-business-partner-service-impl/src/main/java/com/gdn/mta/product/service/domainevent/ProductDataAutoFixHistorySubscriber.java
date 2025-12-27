package com.gdn.mta.product.service.domainevent;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.response.ProductDataAutoFixHistoryListRequest;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.product.service.ProductDataAutoFixHistoryService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductDataAutoFixHistorySubscriber {

  @Autowired
  private ProductDataAutoFixHistoryService productDataAutoFixHistoryService;
  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    if (StringUtils.isEmpty(message)) {
      log.error("The kafka message is null or empty");
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND);
    }
    try {
      ProductDataAutoFixHistoryListRequest productDataAutoFixHistoryListRequest =
          objectMapper.readValue(message, ProductDataAutoFixHistoryListRequest.class);
      MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, Constants.DEFAULT_STORE_ID);
      log.info("Consume event {} with message : {}", DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY,
          productDataAutoFixHistoryListRequest);
      this.productDataAutoFixHistoryService
          .saveHistory(productDataAutoFixHistoryListRequest.getProductDataAutoFixHistoryDtoList());
    } catch (ApplicationRuntimeException e) {
      log.error(
          "Exception caught while processing event {} , Kafka message received is empty with error {} with error stack trace ",
          DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY, ErrorCategory.DATA_NOT_FOUND, e);
    } catch (Exception exp) {
      log.error("Exception caught while processing event {} with error stack trace ",
          DomainEventName.PRODUCT_DATA_AUTO_FIX_HISTORY, exp);
    }
  }
}
