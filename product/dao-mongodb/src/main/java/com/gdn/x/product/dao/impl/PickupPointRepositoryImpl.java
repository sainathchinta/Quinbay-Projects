package com.gdn.x.product.dao.impl;

import com.gdn.x.product.dao.api.PickupPointRepositoryCustom;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.entity.PickupPoint;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.Map;

import static org.springframework.data.mongodb.core.query.Criteria.where;

@Repository
public class PickupPointRepositoryImpl implements PickupPointRepositoryCustom {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private Map<String, MongoTemplate> mongoTemplateFactory;

  @Override
  public void upsertPickupPoint(String storeId, PickupPoint pickupPoint, String username) {
    Query query = new Query(where(ProductFieldNames.STORE_ID).is(storeId)
        .and(ProductFieldNames.PICKUP_POINT_CODE).is(pickupPoint.getPickupPointCode()));
    Update update = new Update();
    update.setOnInsert(ProductFieldNames.PICKUP_POINT_CODE, pickupPoint.getPickupPointCode())
        .set(ProductFieldNames.CNC_ACTIVATED, pickupPoint.isCncActivated())
        .setOnInsert(ProductFieldNames.CREATED_BY, username)
        .setOnInsert(ProductFieldNames.CREATED_DATE, new Date())
        .set(ProductFieldNames.UPDATED_BY, username)
        .set(ProductFieldNames.UPDATED_DATE, new Date());

    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).upsert(query, update, PickupPoint.class);
  }

}
