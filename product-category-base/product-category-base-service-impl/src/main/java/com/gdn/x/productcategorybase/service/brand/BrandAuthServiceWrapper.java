package com.gdn.x.productcategorybase.service.brand;

import java.util.List;

import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthDeleteRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateWipRequest;

public interface BrandAuthServiceWrapper {
  /**
   * Service wrapper method for updating brand auth data and Evict
   *
   * @param storeId
   * @param updateRequest
   * @throws Exception
   */
  void editBrandAuthDetailsAndEvictCache(String storeId, BrandAuthUpdateRequest updateRequest,
      String username)
    throws Exception;
  /**
   * Service wrapper method for marking brand auth data as mfd true
   *
   * @param storeId
   * @param brandAuthDeleteRequestList
   * @throws Exception
   */
  void deleteBrandAuthAndEvictCache(String storeId, String username,
    List<BrandAuthDeleteRequest> brandAuthDeleteRequestList) throws Exception;

  /**
   * Create new brand auth for a brand & seller code
   *
   * @param brandAuthCreateRequest Brand Auth Creation Request
   * @param storeId Non null storeId
   * @param username Non null username
   * @return BrandAuthCreateResponse
   */
  BrandAuthCreateResponse createBrandAuthAndEvictCache(BrandAuthCreateRequest brandAuthCreateRequest,
    String storeId, String username) throws Exception;

  /**
   * Create new brand auth for a brand & seller code
   *
   * @param brandAuthCreateWipRequest Brand Auth Creation Request
   * @param storeId Non null storeId
   * @param username Non null username
   * @return BrandAuthCreateWipResponse
   */
  BrandAuthCreateWipResponse createBrandAuthWip(BrandAuthCreateWipRequest brandAuthCreateWipRequest,
    String storeId, String username) throws Exception;
}
