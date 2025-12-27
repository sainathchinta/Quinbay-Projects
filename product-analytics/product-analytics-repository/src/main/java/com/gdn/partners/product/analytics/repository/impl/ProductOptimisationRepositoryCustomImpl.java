package com.gdn.partners.product.analytics.repository.impl;

import com.gdn.partners.product.analytics.entity.ProductOptimisationDetails;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.model.ProductOptimisationDetailsFields;
import com.gdn.partners.product.analytics.repository.ProductOptimisationRepositoryCustom;
import com.gdn.partners.product.analytics.web.model.request.ProductOptimisationListRequest;
import io.micrometer.common.util.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.support.PageableExecutionUtils;

public class ProductOptimisationRepositoryCustomImpl
    implements ProductOptimisationRepositoryCustom {

  @Autowired
  private MongoTemplate mongoTemplate;

  @Override
  public Page<ProductOptimisationDetails> fetchProductOptimisationListWithFilterApplied(
      String storeId, ProductOptimisationListRequest productOptimisationListRequest,
      Pageable pageable) {
    Query query = new Query();
    query.addCriteria(Criteria.where(Constants.MARK_FOR_DELETE).is(false));
    query.addCriteria(Criteria.where(Constants.STORE_ID).is(storeId));
    query.with(Sort.by(Sort.Direction.ASC, ProductOptimisationDetailsFields.STATUS_IN_DB));
    if (StringUtils.isNotBlank(productOptimisationListRequest.getCategoryCode())) {
      query.addCriteria(Criteria.where(ProductOptimisationDetailsFields.CATEGORY_CODE)
          .is(productOptimisationListRequest.getCategoryCode()));
    }
    if (StringUtils.isNotBlank(productOptimisationListRequest.getKeyword())) {
      query.addCriteria(new Criteria().orOperator(
          Criteria.where(ProductOptimisationDetailsFields.PRODUCT_SKU)
              .is(productOptimisationListRequest.getKeyword()),
          Criteria.where(ProductOptimisationDetailsFields.PRODUCT_NAME)
              .is(productOptimisationListRequest.getKeyword())));
    }
    query.addCriteria(Criteria.where(ProductOptimisationDetailsFields.SELLER_CODE)
        .is(productOptimisationListRequest.getSellerCode()));
    long count = mongoTemplate.count(query, ProductOptimisationDetails.class);
    query.with(pageable);
    return PageableExecutionUtils.getPage(
        mongoTemplate.find(query, ProductOptimisationDetails.class), pageable, () -> count);
  }
}