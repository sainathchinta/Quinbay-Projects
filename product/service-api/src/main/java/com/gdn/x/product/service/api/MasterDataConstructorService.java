package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Map;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;

public interface MasterDataConstructorService {

  /**
   * @param masterDataItem
   * @param masterDataProduct
   * @return
   */
  MasterDataItem constructItemDimensionFields(MasterDataItem masterDataItem,
      MasterDataProduct masterDataProduct);

  /**
   * @param productAndItems
   * @return
   */
  ProductAndItemsVO constructItemsDimensionFields(ProductAndItemsVO productAndItems);

  /**
   * 
   * @param storeId
   * @param username
   * @param requestId
   * @param product
   * @param items
   * @return
   * @throws Exception
   */
  ProductAndItemsVO constructProductAndItemWithMasterData(String storeId, String username,
      String requestId, Product product, List<Item> items) throws Exception;

  /**
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param product
   * @param items
   * @param isPvSwitchEnabled
   * @param inAllProducts
   * @return
   * @throws Exception
   */
  ProductAndItemsVO constructProductAndItemWithMasterData(String storeId, String username,
      String requestId, Product product, List<Item> items, boolean isPvSwitchEnabled, boolean inAllProducts) throws Exception;

  /**
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param product
   * @param items
   * @param inAllProducts
   * @return
   * @throws Exception
   */
  ProductAndItemsVO constructProductAndItemWithMasterData(String storeId, String username, String requestId,
      Product product, List<Item> items, boolean inAllProducts) throws Exception;

  /**
   * 
   * @param storeId
   * @param username
   * @param requestId
   * @param productMap
   * @param items
   * @param inAllProducts
   * @return
   * @throws Exception
   */
  List<ProductAndItemsVO> constructProductsAndItemsWithMasterData(String storeId, String username,
      String requestId, Map<String, Product> productMap, List<Item> items, boolean inAllProducts) throws Exception;
  /**
   * 
   * @param storeId
   * @param username
   * @param requestId
   * @param product
   * @param items
   * @return
   * @throws Exception
   */
  ProductAndItemsVO constructProductAndItemWithMasterDataAndEvictCaches(String storeId, String username,
      String requestId, Product product, List<Item> items) throws Exception;

  /**
   *
   * @param item
   * @return
   * @throws Exception
   */
   Item constructPristineDataItemWithMasterData(Item item) throws Exception;

  /**
   * Fetch map of master data to sku code for transaction API
   *
   *
   * @param storeId
   * @param itemCodeList
   * @return
   */
  Map<String, ProductMasterDataResponse> fetchMasterDataMapForTransaction(String storeId,
    List<String> itemCodeList);
}
