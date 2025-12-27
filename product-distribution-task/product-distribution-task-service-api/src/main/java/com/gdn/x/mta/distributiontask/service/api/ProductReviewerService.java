package com.gdn.x.mta.distributiontask.service.api;

import java.util.Date;
import java.util.List;
import java.util.Map;

import com.gdn.x.mta.distributiontask.model.ProductReviewer;

public interface ProductReviewerService {

  /**
   *
   * @param storeId
   * @param productCode
   */
  ProductReviewer findProductReviewerByStoreIdAndProductCodeMarkForDeleteFalse(String storeId, String productCode);

  /**
   * @param productCode
   * @return
   */
  ProductReviewer findProductReviewerByProductCode(String productCode);

  /**
   * @param productReviewer
   */
  void save(ProductReviewer productReviewer);

  /**
   * @param productReviewer
   */
  void saveAndFlush(ProductReviewer productReviewer);

  /**
   *
   * @param storeId
   * @param productCode
   * @return
   */
  ProductReviewer addNewProduct(String storeId, String productCode);

  /**
   *
   * @param productReviewer
   * @param clearAssignee
   * @return
   */
  ProductReviewer resetAssignmentData(ProductReviewer productReviewer, boolean clearAssignee);

  /**
   *
   * @param storeId
   * @param productCodes
   * @return
   */
  List<ProductReviewer> findProductReviewerByProductCodesAndMarkForDeleteFalse(String storeId, List<String> productCodes);

  /**
   *
   * @param storeId
   * @param productCode
   * @return
   */
  ProductReviewer findProductReviewerByStoreIdAndProductCode(String storeId, String productCode);

  /**
   * Clear current reviewer in database
   *
   * @param productCode
   */
  void clearExistingReviewDatesDetails(String productCode);

  /**
   * Mark for delete true for reviewer once review complete
   *
   * @param productCode
   * @param approverAssignee
   */
  void markForDeleteByProductCode(String productCode, String approverAssignee);

  /**
   * Set all reviewer details to null
   *
   * @param productCode
   */
  void clearAllReviewerDetails(String productCode);

  /**
   * Update product image assignment
   *
   * @param assignedTo
   * @param date
   * @param productCodes
   */
  void updateProductAssignment(String assignedTo, Date date, List<String> productCodes);

  /**
   * Delete entry from table by product code
   *
   * @param productCodeList
   */
  void deleteByProductCodesIn(List<String> productCodeList);

  /**
   * @param productReviewers
   * @return
   */
  List<ProductReviewer> saveAll(List<ProductReviewer> productReviewers);

  /**
   * Find productReviewers by product code list
   *
   * @param storeId
   * @param productCodes
   * @return
   */
  List<ProductReviewer> findProductReviewerByProductCodes(String storeId, List<String> productCodes);

  /**
   *
   * Find productReviewers map by product code list
   *
   * @param storeId
   * @param productCodes
   * @return
   */
  Map<String, ProductReviewer> findProductReviewerMapByProductCodes(String storeId, List<String> productCodes);
}
