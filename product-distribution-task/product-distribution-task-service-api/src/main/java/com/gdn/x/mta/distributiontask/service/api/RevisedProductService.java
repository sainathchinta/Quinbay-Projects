package com.gdn.x.mta.distributiontask.service.api;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gdn.mta.domain.event.modal.AddRevisedProductToPDTEvent;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;

public interface RevisedProductService {

  /**
   * Add revised product for vendor review
   *
   * @param addRevisedProductToPDTEvent
   * @param imageQcProcessedAndBrandResponse
   * @throws Exception
   */
  ProductAndReviewerDetailsDTO addRevisedProductOnSubmit(AddRevisedProductToPDTEvent addRevisedProductToPDTEvent,
      ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse) throws Exception;

}
