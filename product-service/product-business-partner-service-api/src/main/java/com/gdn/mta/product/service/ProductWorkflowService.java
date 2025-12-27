package com.gdn.mta.product.service;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pbp.entity.workflow.product.ProductWf;

import java.util.List;
import java.util.Map;

public interface ProductWorkflowService {
  
  void processImage(String storeId, String productCode, boolean retryProcessImage) throws Exception;
  
  void rejectProcessImage(String storeId, String productCode) throws Exception;

  boolean approveImage(String storeId, String productCode, boolean retryApproveImage)
      throws Exception;

  boolean approveContent(String storeId, String productCode, boolean retryApproveContent)
      throws Exception;
  
  void create(String storeId, String productCode) throws ApplicationException, ApplicationRuntimeException;
  
  void submit(String storeId, String productCode) throws Exception;

  /**
   * get a map of product code and ProductWf by list of product Codes
   *
   * @param productCodes must not null
   */
  Map<String, List<ProductWf>> getProductWfByProductCodes(List<String> productCodes);

  /**
   * get a map of product id and notes
   *
   * @param productIds
   */
  Map<String, String> getRejectedNotesByProductIds(List<String> productIds);

  /**
   * delete entries from product workflow repo by storeId and productCode
   * @param storeId
   * @param productCode
   */
  void deleteProductWorkflowByStoreIdAndProductCode(String storeId, String productCode);
}
