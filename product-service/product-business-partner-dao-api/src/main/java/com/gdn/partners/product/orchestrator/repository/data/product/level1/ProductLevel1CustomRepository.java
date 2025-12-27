package com.gdn.partners.product.orchestrator.repository.data.product.level1;

import com.gdn.partners.product.orchestrator.entity.product.level1.ProductLevel1;
import com.gdn.partners.product.orchestrator.model.product.level1.ProductLevel1Filter;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface ProductLevel1CustomRepository {

  Page<ProductLevel1> findByStoreIdAndFilterAndMarkForDeleteFalse(String storeId,
      ProductLevel1Filter filter, Pageable pageable);

}
