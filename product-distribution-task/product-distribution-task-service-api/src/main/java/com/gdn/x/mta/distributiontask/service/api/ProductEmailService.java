package com.gdn.x.mta.distributiontask.service.api;

import com.gdn.x.mta.distributiontask.domain.event.model.ProductEmailEventModel;

import java.util.Date;

public interface ProductEmailService {

  /**
   * Send mail to business partners for evidence request of products from IPR
   * @param storeId Non null store id
   * @param productEmailType Non null email type
   * @throws Exception
   */
  void sendProductMailEventsToBusinessPartnersForSuspension(String storeId,
    String productEmailType);

  /**
   * Add/delete entry from product email table
   * @param productEmailEventModel
   */
  void addProductToEmailProcess(ProductEmailEventModel productEmailEventModel);

}
