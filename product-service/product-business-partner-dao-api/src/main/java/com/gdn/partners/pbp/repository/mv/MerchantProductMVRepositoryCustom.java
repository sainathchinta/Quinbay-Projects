package com.gdn.partners.pbp.repository.mv;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.entity.mv.MerchantProductMV;

public interface MerchantProductMVRepositoryCustom {

  Page<MerchantProductMV> findByFilter(ProductLevel3SummaryFilter filter, Pageable pageable,
      SortOrder sort);
  
  boolean isInitialized();
}
