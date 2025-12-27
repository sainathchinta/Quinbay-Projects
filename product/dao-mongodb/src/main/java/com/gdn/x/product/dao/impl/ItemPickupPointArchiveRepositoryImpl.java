package com.gdn.x.product.dao.impl;

import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;

import com.gdn.x.product.dao.api.ItemPickupPointArchiveCustomRepository;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.entity.ItemPickupPointArchive;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;

import org.springframework.stereotype.Repository;

@Repository
public class ItemPickupPointArchiveRepositoryImpl implements ItemPickupPointArchiveCustomRepository {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private Map<String, MongoTemplate> mongoTemplateFactory;


  @Override
  public void deleteByProductSku(List<String> productSkus) {
    Criteria criteria = Criteria.where(ProductFieldNames.PRODUCT_SKU).in(productSkus);
    mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .remove(new Query(criteria), ItemPickupPointArchive.class);
  }
}
