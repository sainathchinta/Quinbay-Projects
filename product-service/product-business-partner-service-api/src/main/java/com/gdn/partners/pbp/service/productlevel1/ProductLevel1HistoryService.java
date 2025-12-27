package com.gdn.partners.pbp.service.productlevel1;

import java.util.List;

import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.partners.pbp.dto.workflow.product.ProductResubmitRequest;

public interface ProductLevel1HistoryService {

  void create(String productCode, String processCode, String notes);

  /**
   * save product level history for need revision
   *
   * @param productCode
   * @param processCode
   * @param notes
   * @param revisionNotes
   * @param username
   */
  void createForNeedRevision(String productCode, String processCode, String notes,
      NeedRevisionNotes revisionNotes, String username) throws Exception;

  /**
   * save product level history
   *
   * @param productCollection must not null
   * @param processCode process code
   * @param notes notes on product
   */
  void create(ProductCollection productCollection, String processCode, String notes);

  /**
   *
   * @param storeId
   * @param productId
   * @return
   */
  List<ProductHistory> findByStoreIdAndProductIdAndMarkForDeleteFalse(String storeId, String productId);

  /**
   * Add product history
   *
   * @param productCodes
   * @param assignedTo
   * @param assignedBy
   */
  void addProductHistoryForProductAssignment(List<String> productCodes, String assignedTo, String assignedBy);

  /**
   * Save product history
   *
   * @param productCode
   * @param updatedBy
   * @param description
   * @param notes
   */
  void saveProductHistory(String productCode, String updatedBy, String description, String notes);

  /**
   * Add product history when product is resubmitted
   *
   * @param productCreationRequest
   * @throws Exception
   */
  void addHistoryForProductResubmitted(ProductCreationRequest productCreationRequest) throws Exception;

  /**
   * Add product history when product is resubmitted
   *
   * @param productResubmitRequest
   * @throws Exception
   */
  void addHistoryForProductResubmissionDueToContentOrImageRejection(ProductResubmitRequest productResubmitRequest) throws Exception;

  /**
   * Update product id in history for revised products
   *
   * @param oldProductId
   * @param newProductId
   */
  void updateProductIdForRevisedProducts(String oldProductId, String newProductId);

  /**
   * delete entries from product history repo by storeId and productId
   * @param storeId
   * @param productId
   */
  void deleteProductHistoryByStoreIdAndProductId(String storeId, String productId);
}
