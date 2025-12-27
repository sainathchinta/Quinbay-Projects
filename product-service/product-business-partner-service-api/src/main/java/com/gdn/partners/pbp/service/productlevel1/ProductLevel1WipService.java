package com.gdn.partners.pbp.service.productlevel1;

import java.util.Date;

import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;

public interface ProductLevel1WipService {

  void create(String businessPartnerCode, String businessPartnerName, String productCreationType, ProductRequest request) throws Exception;

  ProductCollection approveDraft(String productCode, ProfileResponse profileResponse) throws Exception;

  void approveContent(String productCode) throws Exception;

  void approveImage(String productCode) throws Exception;

  void activate(String productCode) throws Exception;

  void delete(String productCode, String notes) throws Exception;

  /**
   * update for rejected product in ProductCollection
   *
   * @param productCode
   * @throws Exception
   */
  void updateRejectedProduct(String productCode) throws Exception;

  /**
   * return product for correction
   * @param productCode
   * @param needRevisionNotes
   * @param autoNeedRevision
   * @param screeningAction
   * @param validateDraftState
   * @throws Exception
   */
  void returnDraftForCorrection(String productCode, NeedRevisionNotes needRevisionNotes, boolean autoNeedRevision,
      boolean screeningAction, boolean validateDraftState) throws Exception;

  void resubmit(ProductRequest productRequest, Date submitDate) throws Exception;
}
