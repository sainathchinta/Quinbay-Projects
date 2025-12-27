package com.gdn.partners.product.orchestrator.service.product.level1;

import com.gdn.partners.product.orchestrator.dto.product.level1.ProductLevel1FilterResponse;
import com.gdn.partners.product.orchestrator.model.product.level1.ProductLevel1Filter;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface ProductLevel1Service {

  Page<ProductLevel1FilterResponse> findByFilter(ProductLevel1Filter filter, Pageable pageable)
      throws Exception;

}
