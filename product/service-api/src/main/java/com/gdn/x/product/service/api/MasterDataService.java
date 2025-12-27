package com.gdn.x.product.service.api;

import java.util.Map;
import java.util.Set;

import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public interface MasterDataService {

  /**
   * @param requestId
   * @param username
   * @param itemCodes
   * @return
   * @throws Exception
   */
  Map<String, MasterDataItem> getMasterDataItems(String storeId, String username, String requestId,
      Set<String> itemCodes) throws Exception;

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @param productCodes
   * @param inAllProducts
   * @return
   */
  Map<String, MasterDataProductAndItemsVO> getMasterDataProductDetailResponse(String storeId,
      String username, String requestId, Set<String> productCodes, boolean inAllProducts) throws Exception;

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @param productCodes
   * @param inAllProducts
   * @return
   */
  Map<String, MasterDataProductAndItemsVO> getMasterDataProductDetailResponseWithoutCache(
      String storeId, String username, String requestId, Set<String> productCodes, boolean inAllProducts) throws Exception;

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @param productCodes
   * @return
   */
  Map<String, MasterDataProduct> getMasterDataProducts(String storeId, String username,
      String requestId, Set<String> productCodes) throws Exception;

  /**
   * @param username
   * @param requestId
   * @param productCode
   * @return
   * @throws Exception
   */
  ProductDetailResponse getProductDetailFromMasterData(String username, String requestId,
      String productCode) throws Exception;

}
