package com.gdn.x.productcategorybase.service.brand;

import java.util.List;

import com.gdn.x.productcategorybase.dto.brand.BrandAuthBulkDownloadResponse;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.domain.Page;

import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateResponse;
import com.gdn.x.productcategorybase.dto.brand.ProductBrandValidationRequest;
import com.gdn.x.productcategorybase.dto.response.BrandAuthorisationDetailResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisation;

public interface BrandAuthorisationService {

  /**
   * Check if seller is authorised to sell brand
   *
   * @param storeId
   * @param sellerCode
   * @param brandCode
   * @param toTakeDown
   * @return
   */
  boolean checkBrandAuthBySellerCode(String storeId, String sellerCode, String brandCode,
    boolean toTakeDown) throws Exception;

  /**
   * Get brand auth by seller code and brand code
   *
   * @param storeId
   * @param brandCode
   * @return
   */
  List<BrandAuthorisation> findBrandAuthorisationBySellerCodeAndBrandCode(String storeId, String brandCode);


  /**
   *
   * @param storeId
   * @param request
   * @param page
   * @param size
   * @return
   */
  Page<BrandAuthFilterResponse> findBrandAuthorisationByFilter(String storeId, BrandAuthFilterRequest request, int page, int size);

  /**
   *
   * to check the product is to be taken down based on DS model prediction
   * @param storeId
   * @param productBrandValidationRequest
   * @return
   * @throws Exception
   */
  boolean takeDownProductBasedOnBrand(String storeId, ProductBrandValidationRequest productBrandValidationRequest)
      throws Exception;

  /**
   * Get brand auth details by brand code and Seller Code
   *
   * @param storeId
   * @param brandCode
   * @return
   */
  BrandAuthorisationDetailResponse getBrandAuthDetailByBrandCodeAndSellerCode(String storeId,
    String brandCode, String sellerCode) throws Exception;

  /**
   * Update brand name in brand authorisation using brand code
   *
   * @param oldBrandName
   * @param newBrandName
   * @param brandCode
   */
  void updateBrandNameByBrandCode(String oldBrandName, String newBrandName, String brandCode);

  /**
   * Create new brand auth for a seller
   *
   * @param brandCreateAuthorisationRequest
   * @param storeId
   * @param username
   * @return
   */
  Pair<BrandAuthCreateResponse, BrandAuthorisation> create(
    BrandAuthCreateRequest brandCreateAuthorisationRequest, String storeId, String username)
    throws Exception;

  /**
   * delete brand auth mapping by brand code and Seller Code
   *
   * @param storeId
   * @param username
   * @param sellerCode
   * @param brandCode
   * @param status
   * @param id
   * @return
   * @throws
   */
  String deleteMappingByBrandCodeAndSellerCode(String storeId, String username, String sellerCode,
    String brandCode, String status, String id) throws Exception;

  /**
   * Return brand auth response from ids
   * @param ids
   * @return
   */
  List<BrandAuthBulkDownloadResponse> getBrandAuthBulkResponse(String storeId, List<String> ids);

  /**
   * Edit brand auth mapping by Update Request
   *
   * @param storeId
   * @param updateRequest
   * @param savedBrandAuthData
   * @return
   * @throws
   */
  BrandAuthorisation editBrandAuthDetails(String storeId, BrandAuthorisation savedBrandAuthData,
    BrandAuthUpdateRequest updateRequest) throws Exception;
}
