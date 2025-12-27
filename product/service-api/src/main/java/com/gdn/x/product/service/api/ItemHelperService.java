package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Set;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.AddVariantRequestVo;
import com.gdn.x.product.model.vo.OfflineItemPriceVO;

public interface ItemHelperService {

  List<OfflineItemPriceVO> convertOfflineItemPrices(
      String pickupPointCodeFilter, ProductAndItemSolr productAndItem);

  List<OfflineItemPriceVO> convertOfflineItemPrices(String pickupPointCodeFilter,
      List<String> pickupPointCodesFilter, ProductAndItemSolr productAndItem);

  /**
   * Return matched itemViewConfig by channel, if not found it will throw
   * ApplicationRuntimeException
   *
   * @param itemViewConfigs
   * @param channelParam
   * @return
   * @throws ApplicationRuntimeException
   */
  ItemViewConfig getItemViewConfigs(Set<ItemViewConfig> itemViewConfigs, String channelParam)
      throws ApplicationRuntimeException;

  /**
   * @param item
   * @param prices
   * @param username
   * @return
   */
  Item setItemPriceByChannel(Item item, Set<Price> prices, String username);

  /**
   * @param price
   * @return discount value
   */
  double getDiscountPrice(Price price);

  /**
   *
   *
   * @param prices
   * @return
   */
  DiscountPrice processDiscountPricesByPriority(List<DiscountPrice> prices);


  /**
   * Get category name corresponding to the category code from cache
   *
   * @param requestId
   * @param username
   * @param categoryCode
   * @return
   */
  String getCategoryNameByCategoryCode(String requestId, String username, String categoryCode);

  /**
   * getting newly added variant request
   *
   * @param addVariantRequestVo
   * @param product
   * @param mandatoryRequestParam
   * @return
   */
  Item convertToItem(AddVariantRequestVo addVariantRequestVo, Product product,
      MandatoryRequestParam mandatoryRequestParam);

  /**
   * @param storeId
   * @param pristineDataItem
   * @param pickupPointCode
   * @return
   */
  Set<String> findActivePromoBundlingsByStoreIdAndPristineAndPickupPointCode(String storeId,
      PristineDataItem pristineDataItem, String pickupPointCode);

  /**
   * @param storeId
   * @param itemCode
   * @param pickupPointCode
   * @return
   */
  Set<String> findActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCode(String storeId, String itemCode,
      String pickupPointCode);

  /**
   * get basic item details by item codes
   *
   * @param storeId
   * @param itemCodes
   * @return
   */
  List<Item> getBasicItemDetailsByItemCodes(String storeId, Set<String> itemCodes);
}
