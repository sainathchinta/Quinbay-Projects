package com.gdn.mta.product.service;

import java.util.Map;

import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.x.mta.distributiontask.rest.model.request.AutoNeedRevisionRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public interface NeedCorrectionService {

  /**
   * Mark product for need correction
   * @param productCode
   * @param autoNeedRevision
   * @param autoNeedRevisionRequest
   * @param overrideDataFromPDT
   */
  void sendForNeedCorrectionByProductCode(String productCode, boolean autoNeedRevision,
      AutoNeedRevisionRequest autoNeedRevisionRequest, boolean overrideDataFromPDT) throws Exception;

  /**
   * Take down product for need revision
   *
   * @param productCode
   * @param itemCodeIdMap
   * @param productDetailResponse
   * @throws Exception
   */
  void takeDownNeedForCorrectionProduct(String productCode, Map<String, String> itemCodeIdMap,
      ProductDetailResponse productDetailResponse)
      throws Exception;

  /**
   * Update state in PBP and send to need revision
   *
   * @param productCode
   * @param notes
   * @param revisionNotes
   * @param autoNeedRevision
   * @param screeningAction
   * @param validateDraftState
   * @param productId
   * @throws Exception
   */
  void updateStateInPBPAndSendProductToNeedRevision(String productCode, String notes, NeedRevisionNotes revisionNotes,
      boolean autoNeedRevision, boolean screeningAction, boolean validateDraftState, ProductCollection productCollection)
      throws Exception;
}
