package com.gdn.mta.product.service.partners.pbp.distributiontask;

import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTRevisedProductVendorApprovedEventModel;

public interface ProductDistributionTaskQCService {

  /**
   * process workflows (approve qc, content, image) and publish message to MTA to process image)
   * @param pdtProductDomainEventModel product distribution information (updated content, image location)
   * @param prioritySeller
   * @throws Exception
   */
  void processProductDistributionTaskQCKafkaConsumer(PDTProductDomainEventModel pdtProductDomainEventModel, int prioritySeller) throws Exception;

  /**
   * process workflows (approve qc, content, image) and publish message to MTA to process image)
   * @param pdtProductDomainEventModel product distribution information (updated content, image location)
   * @param approvalType
   * @throws Exception
   */
  void processVendorApprovalEventForEditedProducts(PDTProductDomainEventModel pdtProductDomainEventModel, String approvalType) throws Exception;


  /**
   * process workflows (approve qc, content, image) and publish message to MTA to process image)
   * @param pdtProductDomainEventModel product distribution information (updated content, image location)
   * @param pdtRevisedProductVendorApprovedEventModel
   * @throws Exception
   */
  void processVendorApprovalEventForRevisedProducts(PDTProductDomainEventModel pdtProductDomainEventModel, PDTRevisedProductVendorApprovedEventModel pdtRevisedProductVendorApprovedEventModel) throws Exception;


}
