package com.gdn.x.product.service.api;

import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;

public interface AllowedAttributeValuesService {

  /**
   * for updating attribute value sequence
   * @param attributeDomainEventModel
   * @throws Exception
   */
  void updateAttributeValueSequenceByAttributeCode(AttributeDomainEventModel attributeDomainEventModel)
      throws Exception;

  /**
   * for sorting the attributes
   * @param storeId
   * @param productAndItemsResponse
   */
  void sortDefiningAttribute(String storeId, ProductAndItemsResponse productAndItemsResponse);

}
