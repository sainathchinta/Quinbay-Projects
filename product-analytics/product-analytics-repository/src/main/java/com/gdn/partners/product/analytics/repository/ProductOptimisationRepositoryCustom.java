package com.gdn.partners.product.analytics.repository;

import com.gdn.partners.product.analytics.entity.ProductOptimisationDetails;
import com.gdn.partners.product.analytics.web.model.request.ProductOptimisationListRequest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;


public interface ProductOptimisationRepositoryCustom {

  Page<ProductOptimisationDetails> fetchProductOptimisationListWithFilterApplied(String storeId,
      ProductOptimisationListRequest productOptimisationListRequest, Pageable pageable);
}