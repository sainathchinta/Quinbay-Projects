package com.gdn.mta.product.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.mta.product.entity.ProductItemWholesalePrice;

public interface ProductItemWholesalePriceRepository extends JpaRepository<ProductItemWholesalePrice, String> {

  List<ProductItemWholesalePrice> findByStoreIdAndItemSkuIn(String storeId, List<String> itemSkus);

  List<ProductItemWholesalePrice> findByStoreIdAndProductItemIdIn(String storeId, List<String> itemSkus);

  ProductItemWholesalePrice findFirstByItemSkuAndPickupPointCode(String itemSku, String pickupPointCode);

  ProductItemWholesalePrice findFirstByStoreIdAndItemSkuAndPickupPointCode(String storeId, String itemSku,
      String pickupPointCode);
}
