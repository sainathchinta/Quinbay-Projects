package com.gdn.partners.pcu.internal.service;

import com.gdn.partners.pcu.internal.web.model.request.AutoApprovedProductsActionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.AutoApprovedProductsDownloadWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.ProductDetailWebResponse;
import org.springframework.web.multipart.MultipartFile;

public interface AutoApprovedServiceWrapper {


  /**
   *
   * @param productCode
   * @param autoApprovedProductsActionWebRequest
   * @throws Exception
   */
  void performActionOnAutoApprovedProducts(String productCode,
      AutoApprovedProductsActionWebRequest autoApprovedProductsActionWebRequest) throws Exception;

  /**
   * to fetch auto approved product detail
   * 
   * @param productCode
   * @param clientId
   * @return ProductDetailWebResponse
   */
  ProductDetailWebResponse fetchAutoApprovedProductDetail(String productCode, String clientId);

  /**
   * download auto approved products
   *
   * @param username
   * @param request
   */
  void downloadItemsForAutoApprovedProducts(String username, AutoApprovedProductsDownloadWebRequest request);

  /**
   * to upload bulk assign file for auto approved products
   * 
   * @param file
   * @param requestId
   * @param storeId
   * @param username
   * @param vendorCode
   * @throws Exception
   */
  void uploadBulkAssignFile(MultipartFile file, String requestId, String storeId, String username,
      String vendorCode) throws Exception;
}
