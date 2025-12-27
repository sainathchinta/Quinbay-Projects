package com.gdn.x.product.service.api;

import com.gdn.x.product.model.vo.MasterDataCacheVo;

public interface PCBMasterDataService {

  /**
   * To get product detail from PCB
   *
   * @param requestId
   * @param username
   * @param productCode
   * @param inAllProducts
   * @return
   */
  MasterDataCacheVo getProductDetailByProductCodeForAllProductsCached(String requestId, String username,
      String productCode, boolean inAllProducts) throws Exception;

  /**
   *
   * @param requestId
   * @param username
   * @param productCode
   * @param inAllProducts
   * @return
   * @throws Exception
   */
  MasterDataCacheVo getProductDetailByProductCodeForAllProductsWithoutCache(String requestId, String username,
      String productCode, boolean inAllProducts) throws Exception;
}
