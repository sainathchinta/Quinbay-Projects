package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Map;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;

public interface SolrIndexService {

  /**
   * @param storeId
   * @param productSku
   * @return
   * @throws Exception
   */
  List<ProductAndItemsVO> indexBulkProduct(String storeId, List<String> productSku,
      boolean clearCache) throws Exception;

  /**
   * @param storeId
   * @param productSkus
   * @param mapOfMasterDataProduct
   * @param mapOfMasterDataItem
   * @return
   */
  MasterDataDetailWithProductAndItemsResponseVo indexBulkProductWithMasterData(String storeId,
      List<String> productSkus, Map<String, MasterDataProduct> mapOfMasterDataProduct,
      Map<String, MasterDataItem> mapOfMasterDataItem, boolean clearCache) throws Exception;

  Item updateSolrAndClearCache(String storeId, Item item);

  MasterDataDetailWithProductAndItemsResponseVo getMasterDataDetailWithProductAndItemsResponseVo(
      String storeId, List<String> productSkus,
      Map<String, MasterDataProduct> mapOfMasterDataProduct,
      Map<String, MasterDataItem> mapOfMasterDataItem, boolean clearCache);


}
