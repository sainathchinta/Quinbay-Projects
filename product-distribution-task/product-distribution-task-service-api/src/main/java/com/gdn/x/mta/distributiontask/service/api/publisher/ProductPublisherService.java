package com.gdn.x.mta.distributiontask.service.api.publisher;

import com.gdn.x.mta.distributiontask.domain.event.model.ProductForcedRollbackSLAExceedDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductRejectedEventModel;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;

public interface ProductPublisherService {
  ProductForcedRollbackSLAExceedDomainEventModel publishEvent(
      ProductDistributionTask productDistributionTaskExceedSLA);

  ProductRejectedEventModel rejectedProductPublisher(String productCode);
}
