package com.gdn.x.product.dao.impl;

import com.gdn.x.product.dao.api.ItemPickupPointMigrationRepositoryCustom;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.entity.ItemPickupPointMigration;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;
import java.util.Map;

import static org.springframework.data.mongodb.core.query.Criteria.where;

@Repository
public class ItemPickupPointMigrationRepositoryImpl implements ItemPickupPointMigrationRepositoryCustom {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private Map<String, MongoTemplate> mongoTemplateFactory;

  @Override
  public List<ItemPickupPointMigration> getItemsByStatusAndLimit(String status, int limit) {
    Query query = new Query(where(ProductFieldNames.EVENT_STATUS).is(status)).limit(limit);
    return mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPointMigration.class);
  }

  @Override
  public void updateStatusByItemSkus(List<String> itemSkuList, String status) {
    Query query = new Query(where(ProductFieldNames.ITEM_SKU).in(itemSkuList));
    Update update = new Update();
    update.set(ProductFieldNames.EVENT_STATUS, status);
    update.set(ProductFieldNames.END_TIME, new Date());
    mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .updateMulti(query, update, ItemPickupPointMigration.class);
  }
}
