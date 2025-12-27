package com.gdn.x.mta.distributiontask.service.api.publisher;

import com.gdn.x.mta.distributiontask.domain.event.model.PDTAutoApprovalEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTEditedProductVendorApprovedEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductVendorApprovedEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTRevisedProductVendorApprovedEventModel;
import com.gdn.x.mta.distributiontask.model.Product;

/**
 * Created by virajjasani on 02/10/16.
 */
public interface ApprovedProductPublisherService {

  /**
   * publish product details for mta to consume and perform final approval
   *
   * @param product
   * @param reviewPending
   * @return
   * @throws Exception
   */
  PDTProductDomainEventModel publishUpdatedProductForFinalApproval(Product product, boolean reviewPending)
      throws Exception;

  /**
   * Publish vendor approval event
   *
   * @param product
   * @param reviewPending
   * @return
   * @throws Exception
   */
  PDTProductVendorApprovedEventModel publishVendorApprovedEvent(Product product, boolean reviewPending)
      throws Exception;

  /**
   * Publish vendor approval event for edited product
   *
   * @param product
   * @return
   * @throws Exception
   */
  PDTEditedProductVendorApprovedEventModel publishEditedVendorApprovedEvent(Product product);

  /**
   * Publish vendor approval event for revised product
   *
   * @param product
   * @return
   * @throws Exception
   */
  PDTRevisedProductVendorApprovedEventModel publishRevisedVendorApprovedEvent(Product product, boolean reviewPending);

  /**
   * Convert product to product domain event model
   *
   * @param product
   * @param reviewPending
   * @return
   * @throws Exception
   */
  PDTProductDomainEventModel convertProductToProductDomainEventModel(Product product, boolean reviewPending)
      throws Exception;

  /**
   * Publish auto approval event for new products
   * @param pdtAutoApprovalEventModel
   * @return
   */
  PDTAutoApprovalEventModel publishAutoApprovalEvent(PDTAutoApprovalEventModel pdtAutoApprovalEventModel);
}
