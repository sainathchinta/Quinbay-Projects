package com.gdn.x.product.service.api;

import java.util.List;

import com.gdn.x.product.model.vo.ItemPriceVO;
import com.gdn.x.product.model.vo.ItemSkuVO;
import com.gdn.x.product.model.vo.ProductForTransactionVO;

public interface ProductForTransactionService {


  /**
   * @param storeId must not be blank
   * @param requestId must not be blank
   * @param itemSku must not be blank
   * @return CartItem with given itemSku, null if not found
   * @throws Exception if fail
   */
  ProductForTransactionVO findProductForTransactionForAllItems(String storeId, String requestId,
      String username, String itemSku) throws Exception;

  /**
   * If channel is blank, the default price will be returned
   *
   * @param storeId must not be blank
   * @param itemSkuList if null or empty will return empty
   * @param channel must not be blank
   * @return never return null, return empty if nothing found
   * @throws Exception if fail
   */
  List<ItemPriceVO> getProductPriceForTransaction(String storeId, List<String> itemSkuList,
      String channel) throws Exception;

  List<ItemPriceVO> findProductPriceForOnlineAndOfflineTransaction(String storeId, List<ItemSkuVO> itemSkuList,
     String channel, String requestId, String username) throws Exception;

  /**
   * Find Product detail for transaction by item sku list
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param itemSkus
   * @return
   */
  List<ProductForTransactionVO> findProductForTransactionByItemSkus(String storeId, String requestId,
      String username, List<String> itemSkus) throws Exception;
}
