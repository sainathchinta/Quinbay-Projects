package com.gdn.partners.pcu.internal.service;

import org.springframework.data.domain.Page;

import com.gdn.partners.pcu.internal.web.model.request.ProductCenterListingActionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductCenterSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.request.SalesCategoryMappingWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCenterSummaryWebResponse;

public interface ProductCenterService {

  /**
   *
   * @param storeId
   * @param requestId
   * @param productCenterSummaryWebRequest
   * @param page
   * @param size
   * @return
   * @throws Exception
   */
  Page<ProductCenterSummaryWebResponse> getProductCenterFilterSummary(String storeId, String requestId,
      ProductCenterSummaryWebRequest productCenterSummaryWebRequest, Integer page, Integer size) throws Exception;

  /**
   *
   * @param storeId
   * @param requestId
   * @param productCenterListingActionWebRequest
   * @throws Exception
   */
  void updateProductCenterListing(String storeId, String requestId,
      ProductCenterListingActionWebRequest productCenterListingActionWebRequest) throws Exception;

  /**
   * @param storeId
   * @param requestId
   * @param productSku
   * @throws Exception
   */
  ProductCenterDetailWebResponse getProductCenterDetail(String storeId, String requestId, String productSku) throws Exception;

  /**
   * @param storeId
   * @param requestId
   * @param productSku
   * @param request
   * @throws Exception
   */
  void updateProductCenterDetail(String storeId, String requestId, String productSku,
      SalesCategoryMappingWebRequest request) throws Exception;

  /**
   * Fetch pageable product center history by product sku
   *
   * @param productSku
   * @param page
   * @param size
   * @return
   */
  Page<ProductCenterHistoryWebResponse> getProductCenterHistoryByProductSku(String productSku, int page, int size);
  /**
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param parentCategoryCode
   * @param language
   * @throws Exception
   */
  void downloadUnmappedSkus(String storeId, String requestId, String username,
      String parentCategoryCode, String language) throws Exception;

}
