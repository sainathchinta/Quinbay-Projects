package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemResponseVo;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsV2ResponseVo;
import com.gdn.x.product.model.vo.SimpleMasterDataProductVO;
import com.gdn.x.product.model.vo.SimpleProductAndItemsAndItemPickupPointV0;
import com.gdn.x.product.model.vo.SimpleProductAndItemsVO;
import com.gdn.x.product.model.vo.SimpleProductRequestVo;

public interface ProductSearchHelperService {

  MasterDataDetailWithProductAndItemsResponseVo getMasterDataProductAndMasterDataItemForReindex(
      String storeId, String username, String requestId, Set<String> productCodes,
      boolean inAllProducts) throws Exception;

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @param combineOthersBundlings
   * @param off2On
   * @return
   * @throws Exception
   */
  MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsWithMasterDataDetail(String storeId, String username,
      String requestId, List<Product> products, boolean combineOthersBundlings, boolean off2On) throws Exception;

  /**
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param products
   * @param itemSku
   * @param combineOthersBundlings
   * @param off2On
   * @return
   * @throws Exception
   */
  MasterDataDetailWithProductAndItemResponseVo getProductAndItemWithMasterDataDetail(
      String storeId, String username, String requestId, List<Product> products, String itemSku,
      boolean combineOthersBundlings, boolean off2On) throws Exception;

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @param products
   * @param combineOthersBundlings
   * @param off2On
   * @return
   * @throws Exception
   */
  MasterDataDetailWithProductAndItemsResponseVo
  getProductAndItemsWithMasterDataDetailByDefaultProduct(String storeId, String username, String requestId, List<Product> products,
      boolean combineOthersBundlings, boolean off2On) throws Exception;

  /**
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param products
   * @param itemSku
   * @param combineOthersBundlings
   * @param off2On
   * @return
   * @throws Exception
   */
  MasterDataDetailWithProductAndItemResponseVo getProductAndItemWithMasterDataDetailByDefaultProduct(
      String storeId, String username, String requestId, List<Product> products, String itemSku,
      boolean combineOthersBundlings, boolean off2On) throws Exception;

  /**
   * Get Simple Master Data Detail Response
   * @param storeId
   * @param username
   * @param requestId
   * @param products
   * @param combineOthersBundlings
   * @param items
   * @param pickupPointCode
   * @return
   * @throws Exception
   */
  SimpleMasterDataDetailWithProductAndItemsResponseVo
  getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(String storeId, String username, String requestId, List<Product> products,
      boolean combineOthersBundlings, List<Item> items, String pickupPointCode) throws Exception;

  /**
   * Get Simple Master Data Detail Response
   * @param storeId
   * @param username
   * @param requestId
   * @param products
   * @param items
   * @param itemPickupPoints
   * @return
   * @throws Exception
   */
  SimpleMasterDataDetailWithProductAndItemsV2ResponseVo getSimplifiedProductAndItemsWithMasterDataDetailByDefaultProduct(
      String storeId, String username, String requestId, List<Product> products, List<Item> items,
      List<ItemPickupPoint> itemPickupPoints) throws Exception;

  MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsWithMasterDataDetailForReindexBySimpleProducts(
      String storeId, String username, String requestId,
      List<SimpleProductRequestVo> simpleProductRequests) throws Exception;

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @return
   * @throws Exception
   */
  MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsWithMasterDataDetailForReindexBySolrResult(
      String storeId, String username, String requestId,
      List<ProductAndItemSolr> productAndItemsSolr) throws Exception;

  void setItemCatalogs(String storeId, String username, String requestId,
      boolean needCategoryHierarchy, List<ProductAndItemsVO> productAndItems,
      Map<String, MasterDataProduct> masterDataProduct) throws Exception;

  /**
   * set ItemCatalogs in Simple MasterDataProductDetail Response
   * @param storeId
   * @param username
   * @param requestId
   * @param productAndItems
   * @param mapOfMasterDataProduct
   * @throws Exception
   */
  void setSimpleItemCatalogs(String storeId, String username, String requestId,
      List<SimpleProductAndItemsVO> productAndItems,
      Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct) throws Exception ;

  /**
   * set ItemCatalogs in Simple MasterDataProductDetail Response
   * @param storeId
   * @param username
   * @param requestId
   * @param productAndItemsAndItemPickupPointV0s
   * @param mapOfMasterDataProduct
   */
  void setSimpleItemCatalogsInMasterData(String storeId, String username, String requestId,
      List<SimpleProductAndItemsAndItemPickupPointV0> productAndItemsAndItemPickupPointV0s,
      Map<String, SimpleMasterDataProductVO> mapOfMasterDataProduct);

  void validateParameters(String storeId, Set<String> param);

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @param items
   * @param products
   * @param defaultProductCode
   * @param productCodes
   * @return
   * @throws Exception
   */
  MasterDataDetailWithProductAndItemsResponseVo
  getProductAndItemsWithMasterDataDetailAndPristineDetail(
      String storeId, String username, String requestId, List<Item> items, List<Product> products,
      String defaultProductCode, Set<String> productCodes) throws Exception;

  /**
   * Get ProductMasterDataDetail By productCode
   * @param storeId
   * @param username
   * @param requestId
   * @param productCode
   * @return
   * @throws Exception
   */
  Map<String, MasterDataProductAndItemsVO> getProductMasterDataDetailByProductCode(String storeId, String username, String requestId,
      String productCode) throws Exception;

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @param products
   * @return
   * @throws Exception
   */
  MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsWithMasterDataDetailWithoutL5Details(String storeId, String username,
      String requestId, List<Product> products, List<Item> items) throws Exception;

}
