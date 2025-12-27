package com.gdn.x.product.dao.impl;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Repository;

import com.gdn.x.product.dao.api.SystemParameterRepositoryCustom;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;

@Repository
public class SystemParameterRepositoryImpl implements
SystemParameterRepositoryCustom {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private Map<String, MongoTemplate> mongoTemplateFactory;

  @Override
  public SystemParameter findAndUpdate(SystemParameter systemParameter) {
    Query query = new Query();
    query.addCriteria(Criteria.where(ProductFieldNames.VARIABLE)
        .is(systemParameter.getVariable()).and(ProductFieldNames.STORE_ID)
        .is(systemParameter.getStoreId()));
    Update update =
        new Update().set(ProductFieldNames.VALUE, systemParameter.getValue()).set(ProductFieldNames.DESCRIPTION,
            systemParameter.getDescription());

    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .findAndModify(query, update, SystemParameter.class);
  }
}
