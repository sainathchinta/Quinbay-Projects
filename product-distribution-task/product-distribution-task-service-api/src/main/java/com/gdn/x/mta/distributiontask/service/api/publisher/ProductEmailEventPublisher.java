package com.gdn.x.mta.distributiontask.service.api.publisher;

import com.gdn.x.mta.distributiontask.domain.event.model.ProductEmailDomainEvent;

public interface ProductEmailEventPublisher {

  /**
   * Publish product mail event for evidence requested product
   *
   * @param productEmailDomainEvent Non null event model
   */
  void publishProductMailDomainEventForIprEvidenceRequestedProduct(
    ProductEmailDomainEvent productEmailDomainEvent);
}
