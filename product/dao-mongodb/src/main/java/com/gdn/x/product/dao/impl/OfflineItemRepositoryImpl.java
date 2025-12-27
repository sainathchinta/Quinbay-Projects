package com.gdn.x.product.dao.impl;

import static org.springframework.data.mongodb.core.query.Criteria.where;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.dao.api.OfflineItemRepositoryCustom;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Repository
public class OfflineItemRepositoryImpl implements OfflineItemRepositoryCustom {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private Map<String, MongoTemplate> mongoTemplateFactory;

  @Override
  public void upsertOfflineItem(MandatoryRequestParam mandatoryRequestParam, OfflineItem offlineItem, boolean markForDelete) {
    Query query = new Query(where(ProductFieldNames.STORE_ID).is(mandatoryRequestParam.getStoreId())
        .and(ProductFieldNames.MERCHANT_CODE).is(offlineItem.getMerchantCode())
        .and(ProductFieldNames.ITEM_SKU).is(offlineItem.getItemSku())
        .and(ProductFieldNames.PICKUP_POINT_CODE).is(offlineItem.getPickupPointCode()));
    Update update = new Update();
    update.setOnInsert(ProductFieldNames.OFFLINE_ITEM_ID, offlineItem.getOfflineItemId())
        .set(ProductFieldNames.MERCHANT_SKU, offlineItem.getMerchantSku())
        .set(ProductFieldNames.LIST_PRICE, offlineItem.getListPrice())
        .set(ProductFieldNames.OFFER_PRICE, offlineItem.getOfferPrice())
        .setOnInsert(ProductFieldNames.CREATED_BY, mandatoryRequestParam.getUsername())
        .setOnInsert(ProductFieldNames.CREATED_DATE, new Date())
        .set(ProductFieldNames.MARK_FOR_DELETE, markForDelete)
        .set(ProductFieldNames.UPDATED_BY, mandatoryRequestParam.getUsername())
        .set(ProductFieldNames.UPDATED_DATE, new Date());

    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).upsert(query, update, OfflineItem.class);
  }

  @Override
  public void updatePriceByItemSku(MandatoryRequestParam mandatoryRequestParam, String merchantCode,
      OfflineItem offlineItem) {
    Query query = new Query(where(ProductFieldNames.STORE_ID).is(mandatoryRequestParam.getStoreId())
        .and(ProductFieldNames.MERCHANT_CODE).is(merchantCode).and(ProductFieldNames.ITEM_SKU)
        .is(offlineItem.getItemSku()));

    Update update = new Update();
    update.set(ProductFieldNames.LIST_PRICE, offlineItem.getListPrice());
    update.set(ProductFieldNames.OFFER_PRICE, offlineItem.getOfferPrice());
    update.set(ProductFieldNames.UPDATED_BY, mandatoryRequestParam.getUsername());
    update.set(ProductFieldNames.UPDATED_DATE, new Date());

    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).updateMulti(query, update, OfflineItem.class);
  }

  @Override
  public List<OfflineItem> findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndLimit(String storeId, Set<String> itemSkus,
      int limit) {
    Query query = new Query(where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).in(itemSkus)
        .and(ProductFieldNames.MARK_FOR_DELETE).is(false));
    query.limit(limit);
    query.with(Sort.by(Sort.Direction.ASC, ProductFieldNames.OFFER_PRICE));
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, OfflineItem.class);
  }
}
