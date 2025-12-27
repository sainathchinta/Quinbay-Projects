package com.gdn.x.mta.distributiontask.service.api.publisher;

import com.gdn.mta.domain.event.config.ProductApprovalDetailStatus;
import com.gdn.mta.domain.event.modal.ProductApprovalDetailStatusEvent;

/**
 * Created by Akshay Bhatt 17/04/2018
 */
public interface ProductApprovalStatusPublisherService {

  /**
   * publish product approval detaial state in product collection
   *
   * @param productId
   * @param status
   * @return
   */
  ProductApprovalDetailStatusEvent updateProductApprovalDetailStatus(String productId,
      ProductApprovalDetailStatus status);
}
