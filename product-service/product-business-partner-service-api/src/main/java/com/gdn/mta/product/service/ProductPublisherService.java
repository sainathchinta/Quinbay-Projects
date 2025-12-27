package com.gdn.mta.product.service;

import com.gdn.mta.domain.event.modal.AddRevisedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.EditedImageResizeEvent;
import com.gdn.mta.domain.event.modal.ImageResizeEvent;
import com.gdn.mta.domain.event.modal.PDTDimensionRefreshEventModel;
import com.gdn.mta.domain.event.modal.ProductQCRetryEvent;
import com.gdn.mta.domain.event.modal.ProductStatusDomainEvent;
import com.gdn.mta.domain.event.modal.ScreeningProductApprovalEvent;
import com.gdn.mta.product.entity.ProductCollection;

public interface ProductPublisherService {

  /**
   * Product screening approval publish event
   *
   * @param productCode
   * @param businessPartnerCode
   * @param businessPartnerName
   * @param updatedBy
   * @param postLive
   * @param restrictedKeywordsPresent
   * @param restrictedKeywordsByFieldJson
   * @param trustedSeller
   * @param productCollection
   * @return
   * @throws Exception
   */
  ScreeningProductApprovalEvent publish(String productCode, String businessPartnerCode, String businessPartnerName,
      String updatedBy, boolean postLive, boolean restrictedKeywordsPresent,
      String restrictedKeywordsByFieldJson, int prioritySeller, boolean trustedSeller, String productId,
      ProductCollection productCollection) throws Exception;

  /**
   * Publish image resize event
   *
   * @param productCode
   * @param storeId
   * @return
   */
  ImageResizeEvent publishProductImageResizeEvent(String productCode, String storeId);

  /**
   * Publish product status event
   *
   * @param productStatusDomainEvent
   * @return
   */
  ProductStatusDomainEvent publishProductStatusDomainEvent(ProductStatusDomainEvent productStatusDomainEvent);

  /**
   *
   * Publish the resize event for edited images
   * @param editedImageResizeEvent
   * @return
   */
  EditedImageResizeEvent publishEditImageResizeEvent(EditedImageResizeEvent editedImageResizeEvent);

  /**
   * publish revise image resize event
   * @param editedImageResizeEvent
   * @return
   */
  EditedImageResizeEvent publishReviseImageResizeEvent(EditedImageResizeEvent editedImageResizeEvent);

  /**
   * publish revised product to PDT
   * @param addRevisedProductToPDTEvent
   * @return
   */
  AddRevisedProductToPDTEvent publishRevisedProductToPDT(AddRevisedProductToPDTEvent addRevisedProductToPDTEvent);

  /**
   * Publish product qc retry event
   *
   * @param productQCRetryEvent
   * @return
   */
  ProductQCRetryEvent publishProductQCRetryEvent(ProductQCRetryEvent productQCRetryEvent);

  /**
   * publish dimension refresh event tovendor in case of internal ans external dimension update for in review products
   * @param pdtDimensionRefreshEventModel
   * @return
   */
  PDTDimensionRefreshEventModel publishProductDimensionRefreshEvent(PDTDimensionRefreshEventModel pdtDimensionRefreshEventModel);

  /**
   * publish the resize event based on prioritySeller
   *
   * @param productCode
   * @param storeId
   * @param prioritySeller
   * @return
   */
  ImageResizeEvent publishProductImageResizeEventForPrioritySeller(String productCode, String storeId,
      int prioritySeller);
}