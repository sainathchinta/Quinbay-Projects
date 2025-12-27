package com.gdn.mta.product.repository;

import com.gdn.mta.product.entity.ProductBusinessPartnerCounter;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface ProductBusinessPartnerCounterRepository
  extends JpaRepository<ProductBusinessPartnerCounter, String> {

  /**
   * @param storeId             storeId
   * @param businessPartnerCode non-null business partner code
   * @return ProductBusinessPartnerCounter
   */
  ProductBusinessPartnerCounter findByStoreIdAndBusinessPartnerCode(String storeId,
    String businessPartnerCode);
}
