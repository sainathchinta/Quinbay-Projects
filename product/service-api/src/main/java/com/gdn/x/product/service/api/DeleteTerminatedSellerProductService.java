package com.gdn.x.product.service.api;

import com.gdn.x.product.domain.event.model.DeleteTerminatedSellerProductEventModel;

public interface DeleteTerminatedSellerProductService {

  /**
   * delete terminated seller product all product, item and itemPickupPoint collection
   * @param deleteTerminatedSellerProductEventModel
   */
  void deleteTerminatedSellerProductData(DeleteTerminatedSellerProductEventModel deleteTerminatedSellerProductEventModel);

}
