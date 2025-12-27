package com.gdn.x.mta.distributiontask.service.api;

import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.dto.ProductDTO;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;

/**
 * Created by Poornima on 9/25/16.
 */
public interface QCTaskService {

    /**
     * To Reject product by QC
     *
     * @param product
     * @param reason
     * @param state
     * @param isAssignedToVendor
     */
    void qcRejectProduct(Product product, boolean isAssignedToVendor, String reason,
        WorkflowState state) throws Exception;

    /**
     * Approve product by QC
     *
     * @param productDistributionTask
     * @throws Exception
     */
    void approveProductByQC(ProductDistributionTask productDistributionTask)
        throws Exception;

    /**
     * <p>Move back passed product to QC because something wrong</p>
     *
     * @param productDistributionTask
     */
    public void moveFailedProductToQC(ProductDistributionTask productDistributionTask) throws Exception;

  /**
   * @param qcRetryCount
   * @param deltaHours
   * @param batchSize
   */
  void retryQcProducts(int qcRetryCount, int deltaHours, int batchSize);

  /**
   *
   * Get product summary from solr
   * @param status
   * @param productListRequest
   * @param pageable
   *
   * @return
   */
  Page<ProductAndReviewerDetailsDTO> getFilterProductSummaryFromSolr(String storeId, String status, ProductListRequest productListRequest,
      Pageable pageable) throws Exception;
}
