package com.gdn.mta.product.service;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ProductActionRetryEvent;
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductDistributionServiceImpl implements ProductDistributionService {
  private static final Logger LOGGER = LoggerFactory.getLogger(ProductDistributionServiceImpl.class);

  @Autowired
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Override
  public void removeProductFromPDT(String requestId, String username, RemoveProductRequest request) throws Exception {
    try {
      productDistributionTaskRepository.removeProductFromPDT(requestId, username, request);
    } catch (Exception e) {
      LOGGER.error("Removal of Product from PDT failed due to Internal Error for productCode : {} ",
          request.getProductCode(), e);
      kafkaProducer.send(DomainEventName.ADD_PRODUCT_TO_PDT_RETRY,
          request.getProductCode(),
          new ProductActionRetryEvent(Constants.DEFAULT_STORE_ID, request.getProductCode(), Constants.PDT_RETRY_DELETE,
              StringUtils.EMPTY));
      log.info("Publish event com.gdn.pbp.product.pdt.retry for productCode : {} ",
          request.getProductCode());
    }
  }
}
