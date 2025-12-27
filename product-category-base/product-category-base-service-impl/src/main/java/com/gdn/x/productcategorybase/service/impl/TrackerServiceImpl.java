package com.gdn.x.productcategorybase.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.x.productcategorybase.domain.event.model.ProductCreationFailureDomainEventModel;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.TrackerService;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class TrackerServiceImpl implements TrackerService {

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Override
  public void trackProductCreationFailure(String requestId, ProductRequest productRequest, String errorMessage) {
    try {
      ProductCreationFailureDomainEventModel productCreationFailureDomainEventModel =
          ConverterUtil.toProductCreationFailureDomainEventModel(productRequest);
      productCreationFailureDomainEventModel.setRequestId(requestId);
      productCreationFailureDomainEventModel.setErrorMessage(errorMessage);
      domainEventPublisherService.publishProductFailure(productCreationFailureDomainEventModel);
    } catch (Exception e) {
      log.error("Exception while publishing product creation failure event : {}", e.getMessage(), e);
    }
  }
}
