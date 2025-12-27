package com.gdn.partners.pbp.service.productlevel1;

import java.util.Date;
import java.util.Set;

import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.NeedRevisionEligibilityRequest;
import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.domain.event.modal.ProductApprovalDetailStatusEvent;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.partners.pbp.dto.productlevel3.PostLiveProductCountResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.ReplaceProductImagesRequest;
import com.gdn.x.productcategorybase.dto.response.GeneratedProductImagesPathResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public interface ProductLevel1CollectionService {

  void create(String businessPartnerCode, String businessPartnerName, String productCode, String brandCode,
      String brandApprovalStatus, boolean postLive, String productCreationType) throws Exception;

  /**
   * create product collection
   *
   * @param businessPartnerCode must not blank
   * @param businessPartnerName must not blank
   * @param productData must not null
   * @param brandCode
   * @param brandApprovalStatus
   * @param postLive
   * @return
   * @throws Exception
   */
  ProductCollection create(String businessPartnerCode, String businessPartnerName, ProductDetailResponse productData,
      String brandCode, String brandApprovalStatus, boolean postLive, String productCreationType) throws Exception;

  /**
   * create product collection
   *
   * @param businessPartnerCode must not blank
   * @param businessPartnerName must not blank
   * @param productData         must not null
   * @param brandCode
   * @param brandApprovalStatus
   * @param postLive
   * @param skipReview
   * @param productCreationType
   * @param skipScreening
   * @return
   * @throws Exception
   */
  ProductCollection create(String businessPartnerCode, String businessPartnerName, ProductDetailResponse productData,
      String brandCode, String brandApprovalStatus, boolean postLive, boolean skipReview, String productCreationType,
      boolean skipScreening, int prioritySeller)
      throws Exception;

  void update(String productCode) throws Exception;

  ProductCollection approveDraft(String productCode, ProfileResponse profileResponse) throws Exception;

  void activate(String productCode) throws Exception;

  void delete(String productCode) throws Exception;

  /**
   * delete product collection
   * @param productCollection
   * @throws Exception
   */
  void delete(ProductCollection productCollection) throws Exception;

  void resubmit(ProductRequest product, Date submitDate) throws Exception;
  
  /**
   * update for rejected product in required table
   *
   * @param productCode
   * @throws Exception
   */
  void updateRejectedProduct(String productCode) throws Exception;

  /**
   * return product for revision
   * @param productCode
   * @param needRevisionNotes
   * @param autoNeedRevision
   * @param screeningAction
   * @param validateDraftState
   * @throws Exception
   */
  void returnDraftForCorrection(String productCode, NeedRevisionNotes needRevisionNotes, boolean autoNeedRevision,
      boolean screeningAction, boolean validateDraftState) throws Exception;

  ProductCollection findByProductId(String productId);

  /**
   * update state of product while product move from QC to Active state
   *
   * @param storeId
   * @param productApprovalDetailStatusEvent
   */
  void updateProductStatus(String storeId, ProductApprovalDetailStatusEvent productApprovalDetailStatusEvent);

  /**
   * find product collection by product code and storeId
   * @param storeId
   * @param productCode
   * @return
   */
   ProductCollection findByProductCode(String storeId, String productCode);

  /**
   * get list of productCodes by passing set of product_id
   *
   * @param storeId             must not blank
   * @param productIds          set of productIds. should not blank
   * @param businessPartnerCode
   * @return
   */
  PostLiveProductCountResponse findProductCodesByStoreIdAndProductIdsInAndMarkForDeleteFalse(String storeId,
      Set<String> productIds, String username, String DEFAULT_NOTE, String businessPartnerCode)
      throws ApplicationRuntimeException;

  /**
   * Get top 1 product history
   *
   * @param storeId
   * @param productId
   * @return
   */
  String findTop1ProductHistoryNotes(String storeId, String productId);

  /**
   * Update product data and send product to need correction state
   *
   * @param productCollection
   */
  void updateAndSendForCorrection(ProductCollection productCollection);

  /**
   * Update productwork state
   *
   * @param productCode
   */
  void updateProductWorkFlowStateForNeedRevision(String productCode);

  Integer getProductTypeBasedOnProductCodeOrId(String productCode, String productId);
}
