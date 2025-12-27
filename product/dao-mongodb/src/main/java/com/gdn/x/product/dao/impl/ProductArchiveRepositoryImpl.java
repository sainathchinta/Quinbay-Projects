package com.gdn.x.product.dao.impl;

import java.util.Date;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;

import com.gdn.x.product.dao.api.ProductArchiveCustomRepository;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.entity.ProductArchive;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;

import org.springframework.stereotype.Repository;


@Repository
public class ProductArchiveRepositoryImpl implements ProductArchiveCustomRepository {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private Map<String, MongoTemplate> mongoTemplateFactory;

  @Override
  public List<ProductArchive> fetchArchivedListOfProduct(Date date, int batchSize) {
    Criteria criteria = Criteria.where(ProductFieldNames.CREATED_DATE).lt(date);
    Query query = new Query(criteria).limit(batchSize);
    query.fields().include(ProductFieldNames.PRODUCT_SKU);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ProductArchive.class);
  }

  @Override
  public void deleteByProductSku(List<String> productSkus) {
    Criteria criteria = Criteria.where(ProductFieldNames.PRODUCT_SKU).in(productSkus);
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .remove(new Query(criteria), ProductArchive.class);
  }
}
