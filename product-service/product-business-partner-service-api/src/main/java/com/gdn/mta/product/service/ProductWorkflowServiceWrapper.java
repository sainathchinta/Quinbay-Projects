package com.gdn.mta.product.service;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gdn.partners.pbp.dto.workflow.product.ProductResubmitRequest;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3Wip;
import com.gdn.x.businesspartner.dto.ProfileResponse;

public interface ProductWorkflowServiceWrapper {

  /**
   * return for correction
   *
   * @param storeId
   * @param productCode
   * @param notes
   */
  void returnForCorrection(String storeId, String productCode, String notes) throws Exception;

  /**
   * resubmit product
   *
   * @param storeId
   * @param productRequest
   * @param updateProductLevel3Wip
   * @throws Exception
   */
  void resubmit(String storeId, ProductResubmitRequest productRequest, UpdateProductLevel3Wip updateProductLevel3Wip) throws Exception;

  /**
   * delete product collection on double upload
   *
   * @param storeId
   * @param productCode
   * @param notes
   * @param needEmailNotification
   * @param deleteFromPdt
   * @throws Exception
   */
  void deleteProductCollection(String storeId, String productCode, String notes, boolean needEmailNotification,
      boolean deleteFromPdt)
      throws Exception;

  /**
   * Approve draft for screening products
   *
   * @param storeId
   * @param productCode
   */
  void approveDraft(String storeId, String productCode) throws Exception;

  /**
   * Create product
   *
   * @param storeId
   * @param productCreationRequest
   * @param isSkipNotification
   * @param MPPFlow
   * @param profileResponse
   * @throws Exception
   */
  boolean create(String storeId, ProductCreationRequest productCreationRequest, boolean isSkipNotification,
      boolean MPPFlow, ProfileResponse profileResponse) throws Exception;
}
