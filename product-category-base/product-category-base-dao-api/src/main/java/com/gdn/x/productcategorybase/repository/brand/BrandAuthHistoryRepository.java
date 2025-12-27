package com.gdn.x.productcategorybase.repository.brand;

import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface BrandAuthHistoryRepository
  extends JpaRepository<BrandAuthorisationHistory, String> {

  /**
   *
   * @param storeId
   * @param brandCode
   * @param sellerCode
   * @param pageable
   * @return
   */
  Page<BrandAuthorisationHistory> findByStoreIdAndBrandCodeAndSellerCodeOrderByCreatedDateDesc(
    String storeId, String brandCode, String sellerCode, Pageable pageable);

}
