package com.gdn.partners.pbp.service.productlevel3;

import java.util.List;

import com.gda.mta.product.dto.ItemPickupPointDto;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;

public interface ProductItemWholesalePriceService {

  /**
   * Save whole sale price rules per item
   *
   * @param productItemWholesalePrices
   */
  void saveWholesalePrice(List<ProductItemWholesalePrice> productItemWholesalePrices);

  /**
   *
   * @param productItemWholesalePrices
   */
  void saveWholesalePriceNew(List<ProductItemWholesalePrice> productItemWholesalePrices);

  /**
   * Get wholesale prices by list of gdnskus
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<ProductItemWholesalePrice> findByStoreIdAndItemSkus(String storeId, List<String> itemSkus);

  /**
   * Get wholesale prices by list of product item ids
   * @param storeId
   * @param productItemIds
   * @return
   */
  List<ProductItemWholesalePrice> findByStoreIdAndProductItemId(String storeId, List<String> productItemIds);

  /**
   * get product item wholesale price by itemSku and pickupPointCode
   * @param storeId
   * @param itemPickupPointDtoList
   * @return
   */
  List<ProductItemWholesalePrice> findByStoreIdAndItemSkuAndPickupPointCode(String storeId,
      List<ItemPickupPointDto> itemPickupPointDtoList);

  /**
   * Fetch L5 wholesale entry by itemSku and pickupPointCode
   *
   * @param itemSku
   * @param pickupPointCode
   * @return
   */
  ProductItemWholesalePrice findOneByItemSkuAndPickupPointCode(String itemSku, String pickupPointCode);

  /**
   * Fetch L5 wholesale entry by storeId,ItemSku and pickupPointCode
   *
   * @param storeId
   * @param itemSku
   * @param pickupPointCode
   * @return
   */
  ProductItemWholesalePrice findOneByStoreIdAndItemSkuAndPickupPointCode(String storeId, String itemSku,
      String pickupPointCode);
}
