package com.gdn.mta.product.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;

public interface ProductBusinessPartnerAttributeRepository
    extends JpaRepository<ProductBusinessPartnerAttribute, String> {

  @Modifying
  @Query(value = "DELETE FROM PRD_PRODUCT_BUSINESS_PARTNER_ATTRIBUTE WHERE product_business_partner_id IN (SELECT id "
      + "FROM PRD_PRODUCT_BUSINESS_PARTNER WHERE store_id = ?1 AND product_id = ?2)", nativeQuery = true)
  void deleteByStoreIdAndProductId(String storeId, String productId);
}
