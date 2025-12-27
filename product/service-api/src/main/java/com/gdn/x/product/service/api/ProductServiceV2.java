package com.gdn.x.product.service.api;

import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Set;

import com.gdn.common.exception.ApplicationException;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.rest.web.model.request.ProductSkuAndProductCodeRequest;
import com.gdn.x.product.rest.web.model.request.DistributionInfoByOmniChannelSkusRequest;
import com.gdn.x.product.rest.web.model.response.DistributionInfoByOmniChannelSkusResponse;
import com.gdn.x.product.rest.web.model.response.BasicProductAndItemDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.DuplicateProductDetailsResponse;
import com.gdn.x.product.rest.web.model.response.HalalProductResponse;
import com.gdn.x.product.rest.web.model.response.PrdProductResponse;
import com.gdn.x.product.rest.web.model.response.PriceRangeResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsSummaryResponseV2;

public interface ProductServiceV2 {

  /**
   * get product and items by itemSku and pickupPointCode
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param itemSku
   * @param pickupPointCode
   * @param includeMarkForDelete
   * @return
   */
  ProductItemsVo getProductAndSingleItemByItemSkuAndPickupPointCode(String storeId, String username, String requestId,
      String itemSku, String pickupPointCode, boolean includeMarkForDelete) throws Exception;

  /**
   * get product and items by itemSku
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param itemSku
   * @param includeMarkForDelete
   * @return
   */
  ProductItemsVo getProductAndSingleItemByItemSku(String storeId, String username, String requestId, String itemSku,
      boolean includeMarkForDelete) throws Exception;

  /**
   * get product and items by itemSku
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param itemSkus
   * @param pristine
   * @param pristine
   * @param off2On
   * @return
   */
  List<ProductItemsVo> getProductAndItemsByItemSkus(String storeId, String username, String requestId,
      Set<String> itemSkus, boolean pristine, boolean off2On) throws Exception;

  /**
   *
   * update cnc at product and item level
   *
   * @param storeId
   * @param username
   * @param itemSkus
   */
  void updateCncFlagAtProductAndItemLevel(String storeId, String username, List<String> itemSkus) throws Exception;

  /**
   *
   * fetch product And items details by product-sku and pickup-point-code for PDP
   *
   * @param storeId must not be empty
   * @param requestId
   * @param username
   * @param productSku must not be empty
   * @param pickupPointCode must not be empty
   * @param showDeleted must not be empty
   * @param combineOthersBundlings
   * @param off2On required = false
   * @param needProductData default : true
   * @param includeForceReview default : false
   * @return
   * @throws Exception
   */
  ProductAndItemsSummaryResponseV2 getProductAndItemsForView(String storeId, String requestId, String username,
    String productSku, String pickupPointCode, boolean showDeleted, boolean combineOthersBundlings,
    boolean off2On, boolean needProductData, boolean includeForceReview) throws Exception;


  /**
   *
   * validate duplicate product by seller sku
   *
   * @param storeId
   * @param merchantCode
   * @param sellerSku
   * @return
   */
  DuplicateProductDetailsResponse validateDuplicateProductBySellerSku(String storeId, String merchantCode, String sellerSku);

  /**
   * get prd_product by productSku or productCode
   *
   * @param storeId
   * @param request
   * @return
   */
  List<PrdProductResponse> getPrdProductDetailByProductSkuOrProductCode(String storeId,
      ProductSkuAndProductCodeRequest request);

  /**
   * get basic product and item details
   * @param storeId
   * @param username
   * @param requestId
   * @param itemSku
   * @param pickupPointCode
   * @param specificationNeeded
   * @param descriptionNeeded
   * @return
   */
  BasicProductAndItemDTO getBasicProductAndItemDetails(String storeId, String username, String requestId,
      String itemSku, String pickupPointCode, boolean specificationNeeded, boolean descriptionNeeded)
      throws Exception;

  /**
   * get basic product and item details
   * @param storeId storeId must not be null
   * @param merchantCode
   * @param skuList should pass either l3 list or l4 list
   * @return
   */

  List<PriceRangeResponse> getPriceRangeForSkus(String storeId, String merchantCode, List<String> skuList);

  /**
   * get halal product response by productsku list
   * @param storeId
   * @param productSkuList
   * @return
   */
  List<HalalProductResponse> getHalalProductResponseByProductSkus(String storeId, List<String> productSkuList)
      throws InvocationTargetException, IllegalAccessException;

  /**
   * update Halal config of product
   *
   * @param storeId
   * @param productSku
   * @param curationStatus
   * @param userName
   */
  void updateHalalConfigOfProduct(String storeId, String productSku, String curationStatus, String userName);

  /**
   * Fetching basic product info based on product sku
   *
   * @param storeId
   * @param productSku
   * @param sharedProductInfoNeeded
   * @return
   */
  BasicProductResponse getBasicProductDetails(String storeId, String productSku, boolean sharedProductInfoNeeded);

  /**
   * back fill special attributes in prd producdt
   *
   * @param productCode
   * @param attributeCode
   * @param attributeName
   * @param attributeValue
   */
  void backFillSpecialAttributesInProduct(String productCode, String attributeCode, String attributeName,
      String attributeValue);

  /**
   * Check if provided omni-channel skus exist under given seller.
   */
  DistributionInfoByOmniChannelSkusResponse checkOmniChannelSkusInSeller(String storeId, String requestId, String username,
      DistributionInfoByOmniChannelSkusRequest request) throws ApplicationException;
}
