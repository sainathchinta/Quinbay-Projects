package com.gdn.x.product.dao.impl;

import static org.springframework.data.mongodb.core.query.Criteria.where;
import com.gdn.x.product.dao.api.ProductRetryEventPublishRepositoryCustom;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.RetryPublishStatus;
import com.gdn.x.product.model.entity.ProductRetryEventPublish;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Map;

@Repository
public class ProductRetryEventPublishRepositoryImpl implements ProductRetryEventPublishRepositoryCustom {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private Map<String, MongoTemplate> mongoTemplateFactory;

  @Override
  public List<ProductRetryEventPublish> findByStateForRetryPublish(RetryPublishStatus state, int limit) {
    Query query = new Query(
        where(ProductFieldNames.RETRY_PUBLISH_STATUS).is(state).and(ProductFieldNames.MARK_FOR_DELETE).is(false));
    query.limit(limit);
    query.with(Sort.by(Sort.Direction.ASC, ProductFieldNames.CREATED_DATE));
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, ProductRetryEventPublish.class);
  }
}
