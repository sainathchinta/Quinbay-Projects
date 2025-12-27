package com.gdn.partners.pbp.workflow.product;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.generator.StuckProductResponse;
import com.gdn.micro.graphics.domain.event.model.ScaleImageResponse;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.partners.pbp.entity.workflow.product.ProductWorkflowStatus;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3Wip;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.entity.Product;

public interface ProductWfService {

  Map<String, List<String>> FLOWS = new HashMap<String, List<String>>();

  String create(String businessPartnerCode, String businessPartnerName, ProductCreationType productCreationType,
      ProductRequest request) throws Exception;

  void createDirect(String productCode) throws Exception;

  void approveDraft(String productCode) throws Exception;

  void approveQC(String productCode) throws Exception;

  void approveContent(String productCode) throws Exception;

  void processImage(String productCode) throws Exception;

  void approveImage(String productCode) throws Exception;

  void rejectImage(String productCode) throws Exception;

  /**
   * delete product flow
   *
   * @param productCode must not blank
   * @param notes must not blank
   * @param needEmailNotification false, if needs not send email notification on delete product
   * @throws Exception
   */
  void delete(String productCode, String notes, boolean needEmailNotification) throws Exception;

  void resubmit(ProductRequest productRequest, UpdateProductLevel3Wip updateProductLevel3Wip) throws Exception;

  ProductWorkflowStatus status(String productCode) throws Exception;

  /**
   * update rejected product to ready for screening
   *
   * @param request
   * @throws Exception
   */
  void updateRejectedProduct(ProductRequest request) throws Exception;

  /**
   * update and publish the product
   *
   * @param request
   * @param notes
   * @param brandCode
   * @param brandApprovalStatus
   * @throws Exception
   */
  void updateAndPublish(Product request, String notes, String brandCode, String brandApprovalStatus) throws Exception;

  /**
   * Need correction api
   * @param productCodes
   * @param notes
   * @param revisionNotes
   * @param screeningAction
   * @throws Exception
   */
  void returnForCorrection(List<String> productCodes, String notes, NeedRevisionNotes revisionNotes,
      boolean screeningAction)
      throws Exception;

  /**
   * Product need revision
   *
   * @param productCode
   * @param notes
   * @param revisionNotes
   * @param autoNeedRevision
   * @param screeningAction
   * @param validateDraftState
   * @throws Exception
   */
  void returnForCorrection(String productCode, String notes, NeedRevisionNotes revisionNotes, boolean autoNeedRevision,
      boolean screeningAction, boolean validateDraftState) throws Exception;

  /**
   * publish product creation data created directly
   * @param productCode
   * @throws Exception
   */
  void publishDirectProductCreationData(String productCode) throws Exception;

  /**
   * delete all wip by product codes
   *
   * @param productCodes
   * @param notes
   * @throws Exception
   */
  void delete(List<String> productCodes, String notes) throws Exception;

  /**
   * create new product
   *
   * @param request must not null
   * @param isSkipNotification
   * @param MPPFlow
   * @throws Exception
   */
  void create(ProductCreationRequest request, boolean isSkipNotification, boolean MPPFlow) throws Exception;

  /**
   * get product work-flows by list of product codes
   *
   * @param productCodes must not blank
   * @return List<ProductWorkflowStatus>
   * @throws Exception
   */
  List<ProductWorkflowStatus> getProductWorkFlowByProductCodes(List<String> productCodes)
      throws Exception;

  /**
   *  get productCode and state of stuck products
   *
   * @param retryBatchSizeCount
   * @param cronJobUpdatedTimeLimit
   * @return StuckProductResponse
   */
  StuckProductResponse getStuckProducts(int retryBatchSizeCount,
      int cronJobUpdatedTimeLimit);

  /**
   * Update resubmit count on product resubmission
   *
   * @param productCollection
   */
  void updateResubmitCountOnProductResubmission(ProductCollection productCollection);

  /**
   *
   * @param productCode
   * @return
   */
  ProductCollection getProductCollectionByProductCode(String productCode);

  /**
   *
   * @param productCode
   */
  void retryResizeEditedImages(String productCode) throws Exception;


  /**
   * Api to approve Image for revised products
   *
   * @param storeId
   * @param productCode
   * @param imageResponses
   * @param isCategoryChanged
   * @param categoryResponseList
   */
  void approveImageForRevisedProduct(String storeId, String productCode, List<ScaleImageResponse> imageResponses,
      boolean isCategoryChanged, List<ProductCategoryResponse> categoryResponseList) throws Exception;

  /**
   * Delete all existing workflows and add draft state
   *  @param storeId
   * @param productCode
   * @param state
   */
  void deleteAllExistingWorkFlowAndCreateNewState(String storeId, String productCode, String state);
}
