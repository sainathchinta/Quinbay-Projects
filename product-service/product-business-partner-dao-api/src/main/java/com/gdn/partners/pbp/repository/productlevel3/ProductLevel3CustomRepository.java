package com.gdn.partners.pbp.repository.productlevel3;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipSummaryRequest;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Wip;

public interface ProductLevel3CustomRepository {

  Page<ProductLevel3Wip> findSummaryByFilterWithState(String storeId, ProductLevel3WipSummaryRequest request,
      Pageable pageable);
}
