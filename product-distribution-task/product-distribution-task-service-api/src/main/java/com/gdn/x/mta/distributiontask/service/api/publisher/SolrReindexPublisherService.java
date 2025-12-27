package com.gdn.x.mta.distributiontask.service.api.publisher;

import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductUpdateProductToSolrEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrBatchAddDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrDeleteDomainEventModel;

public interface SolrReindexPublisherService {

  /**
   * Publish event to reindex pdt solr with batched product
   *
   * @param pdtProductSolrBatchAddDomainEventModel
   * @return
   */
  PDTProductSolrBatchAddDomainEventModel publishPDTProductSolrBatchAddDomainEventModelForReindex(
      PDTProductSolrBatchAddDomainEventModel pdtProductSolrBatchAddDomainEventModel);


  /**
   * Publish event to delete products from pdt solr
   *
   * @param pdtProductSolrDeleteDomainEventModel
   * @return
   */
  PDTProductSolrDeleteDomainEventModel publishPDTProductSolrBatchDeleteDomainEventModelForReindex(
      PDTProductSolrDeleteDomainEventModel pdtProductSolrDeleteDomainEventModel);

  /**
   * @param productAddProductToSolrEventModel
   * @return
   */
  PDTProductUpdateProductToSolrEventModel publishPDTProductApprovalToSolr(
      PDTProductUpdateProductToSolrEventModel productAddProductToSolrEventModel);
}
