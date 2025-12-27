package com.gdn.mta.product.service.domainevent.publisher;

import com.gdn.mta.domain.event.modal.AddDeleteVariantRetryPublishEventModel;

public interface AddDeleteVariantRetryPublishService {

  /**
   * process AddDelete Variant Retry Events
   *
   * @param addDeleteVariantRetryPublishEventModel
   */
  void processAddDeleteVariantRetryEvents(AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel);
}
