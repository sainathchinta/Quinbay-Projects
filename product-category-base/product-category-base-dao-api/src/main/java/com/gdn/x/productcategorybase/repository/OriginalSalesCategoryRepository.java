package com.gdn.x.productcategorybase.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.productcategorybase.entity.OriginalSalesCategory;

public interface OriginalSalesCategoryRepository
    extends JpaRepository<OriginalSalesCategory, String>, OriginalSalesCategoryRepositoryCustom {

  OriginalSalesCategory findByStoreIdAndIdAndMarkForDeleteFalseAndActivatedTrue(String storeId, String id);

  OriginalSalesCategory findByStoreIdAndIdAndMarkForDeleteFalse(String storeId, String id);

  OriginalSalesCategory findByOscCode(String oscCode);
}
