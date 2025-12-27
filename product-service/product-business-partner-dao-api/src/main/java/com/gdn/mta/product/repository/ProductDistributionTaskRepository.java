package com.gdn.mta.product.repository;

import com.gda.mta.product.dto.AppealProductRequest;
import com.gda.mta.product.dto.AppealProductResponse;
import com.gdn.common.exception.ApplicationException;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.AutoNeedRevisionRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ChangeBrandRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;

public interface ProductDistributionTaskRepository {

  /**
   * <p>Move failed product back to QC</p>
   *
   * @param requestId
   * @param username
   * @param productCode
   * @throws Exception
   */
  void moveFailedProductToQC(String requestId, String username, String productCode) throws Exception;

  /**
   * get distribution task product details
   *
   * @param requestId
   * @param username
   * @param productCode
   * @return
   * @throws Exception
   */
  DistributionProductDetailResponse getDetailsForAnyProduct(String requestId,
      String username, String productCode) throws Exception;

  /**
   * remove product from PDT after product activation
   *
   * @param requestId
   * @param username
   * @param request
   * @throws Exception
   */
  void removeProductFromPDT(String requestId, String username, RemoveProductRequest request)
      throws Exception;

  /**
   * if pbp throws exception, then retry approve qc process
   *
   * @param requestId
   * @param username
   * @param productCode
   * @throws Exception
   */
  PDTProductDomainEventModel getPDTDomainModelResponseByCode(String requestId, String username, String productCode) throws Exception;

  /**
   * Check if product exists in PDT
   *
   * @param productCode
   * @param allProducts
   * @return
   */
  boolean checkIfProductExistsInPDT(String productCode, boolean allProducts);

  /**
   * Get product image feedback
   *
   * @param storeId
   * @param productCode
   * @return
   */
  ProductImageQcFeedbackResponse getProductImageQcFeedback(String storeId, String productCode);

  /**
   * Send products back to vendor
   *
   * @param storeId
   * @param productCode
   */
  void sendProductBackToVendor(String storeId, String productCode);

  /**
   * Send product to auto need revision
   *
   * @param autoNeedRevisionRequest
   * @param validateAssignment
   */
  void sendProductToAutoNeedRevision(AutoNeedRevisionRequest autoNeedRevisionRequest, boolean validateAssignment);

  /**
   * Update state in PDT
   *
   * @param productCode
   * @param productRetryStatusUpdate
   */
  void productRetryStatusUpdate(String productCode, ProductRetryStatusUpdate productRetryStatusUpdate);

  /**
   * Update brand in PDT
   *
   * @param changeBrandRequest
   * @throws Exception
   */
  void updateProductBrand(ChangeBrandRequest changeBrandRequest) throws Exception;

  /**
   *
   * @param appealProductRequest
   * @return Response from PDT
   * @throws ApplicationException
   */
  AppealProductResponse updateAppealProduct(AppealProductRequest appealProductRequest)
    throws ApplicationException;
}
