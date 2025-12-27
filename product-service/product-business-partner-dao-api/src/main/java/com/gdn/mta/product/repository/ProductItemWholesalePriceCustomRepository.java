package com.gdn.mta.product.repository;

import java.util.List;

import com.gda.mta.product.dto.ItemPickupPointDto;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;

public interface ProductItemWholesalePriceCustomRepository {

  /**
   * fetch item wholesale price by itemSku and pickupPointCode
   * @param storeId
   * @param itemPickupPointDtoList
   * @return
   */
  List<ProductItemWholesalePrice> findProductItemWholesalePriceByItemSkuAndPickupPointCode(String storeId, List<ItemPickupPointDto> itemPickupPointDtoList);

}
