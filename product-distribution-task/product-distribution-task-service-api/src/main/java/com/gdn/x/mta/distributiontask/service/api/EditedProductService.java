package com.gdn.x.mta.distributiontask.service.api;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.x.mta.distributiontask.model.Product;

public interface EditedProductService {

  /**
   * Add product with just content or Image data for vendor review
   * @param productCode
   * @param addEditedProductToPDTEvent
   * @param imageQcProcessedAndBrandResponse
   * @throws Exception
   *
   * */
  Product editProductDetails(String productCode, AddEditedProductToPDTEvent addEditedProductToPDTEvent,
      ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse) throws Exception;

}
