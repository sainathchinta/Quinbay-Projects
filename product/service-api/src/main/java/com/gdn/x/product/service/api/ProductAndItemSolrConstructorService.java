package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataItemImage;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.rest.web.model.FieldValueObject;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;

public interface ProductAndItemSolrConstructorService {

  /**
   * @param productDomainEventModel
   * @param productAndTotalScoreMap
   * @return
   */
  Map<String, FieldValueObject> constructByMasterDataChangeModel(ProductAndItemSolr productAndItemSolr,
      ProductDomainEventModel productDomainEventModel, Map<String, Double> productAndTotalScoreMap);

  /**
   * @param productItemSolr
   * @param item
   * @param getMasterData
   */
  void constructItem(ProductAndItemSolr productItemSolr, Item item, boolean getMasterData);

  /**
   * @param buyables
   * @param discoverables
   * @return
   */
  Set<ItemViewConfig> constructItemViewConfigs(List<String> buyables, List<String> discoverables);

  /**
   * @param catalogString
   * @return
   */
  MasterCatalog constructMasterCatalog(String catalogString);

  /**
   * @param imageString
   * @return
   */
  List<MasterDataItemImage> constructMasterDataItemMainImages(List<String> imageString);

  /**
   * @param productItemSolr
   * @param product
   */
  void constructProduct(ProductAndItemSolr productItemSolr, Product product, boolean getMasterData);

  /**
   * @param catalogString
   * @return
   */
  List<SalesCatalog> constructSalesCatalogs(List<String> catalogString);

  /**
   * @param productItemSolr
   */
  void constructOfflinePrices(ProductAndItemSolr productItemSolr);

  /**
   * @param itemImages
   * @return
   */

  List<MasterDataItemImage> constructMasterDataItemImages(List<String> itemImages);

  /**
   *
   * @param storeId
   * @param itemSku
   * @return
   */
  List<String> getOfflinePricesByStoreIdAndItemSku(String storeId, String itemSku);
}
