package com.gdn.x.product.dao.impl;

import static java.lang.Boolean.FALSE;
import static org.springframework.data.mongodb.core.query.Criteria.where;

import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.FindAndModifyOptions;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Repository;

import com.gdn.x.product.constants.CommonConstants;
import com.gdn.x.product.dao.api.ItemRepositoryCustom;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.service.api.SkuValidator;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;
import com.mongodb.client.result.UpdateResult;

@Repository
public class ItemRepositoryImpl implements ItemRepositoryCustom {

  private static final Logger LOGGER = LoggerFactory.getLogger(ItemRepositoryImpl.class);

  private static final String DOT_SEPARATOR = ".";

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private Map<String, MongoTemplate> mongoTemplateFactory;

  @Autowired
  private SkuValidator skuValidator;

  @Override
  public Item addActivePromoBundling(String storeId, String itemSku, String promoBundlingType) {
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku));

    Update update = new Update();
    update.addToSet(ProductFieldNames.ACTIVE_PROMO_BUNDLINGS, promoBundlingType);

    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).findAndModify(query, update,
        new FindAndModifyOptions().returnNew(true), Item.class);
  }

  @Override
  public List<Item> assignTicketTemplate(String storeId, List<String> itemSkus,
      String ticketTemplateCode) {
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).in(itemSkus));
    Update update = new Update();
    update.set(ProductFieldNames.TICKET_TEMPLATE_CODE, ticketTemplateCode);
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).updateMulti(query, update, Item.DOCUMENT_NAME);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public List<Item> getItemAvailability(String storeId, Set<String> productSkus) {
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU)
            .in(productSkus).and(ProductFieldNames.MARK_FOR_DELETE).is(false).and(ProductFieldNames.IS_ARCHIVED).is(false));
    query.fields().include(ProductFieldNames.ITEM_SKU)
        .include(ProductFieldNames.PRODUCT_SKU)
        .include(ProductFieldNames.ITEM_CODE)
        .include(ProductFieldNames.ITEM_VIEW_CONFIGS)
        .include(ProductFieldNames.CNC_ACTIVATED)
        .include(String.join(ItemRepositoryImpl.DOT_SEPARATOR, ProductFieldNames.MASTER_DATA_ITEM,
            ProductFieldNames.ITEM_DELIVERY_WEIGHT));
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class, Item.DOCUMENT_NAME);
  }

  @Override
  public List<Item> getPriceAndOff2OnChannelActive(String storeId, List<String> itemSkus) {
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).in(itemSkus));
    query.fields().include(ProductFieldNames.ITEM_SKU).include(ProductFieldNames.PRODUCT_SKU)
        .include(ProductFieldNames.PICKUP_POINT_CODE).include(ProductFieldNames.PRICE)
        .include(ProductFieldNames.OFF2ON_CHANNEL_ACTIVE);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public List<Item> getPriceAndViewConfigs(String storeId, List<String> itemSkus) {
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).in(itemSkus));
    query.fields().include(ProductFieldNames.ITEM_SKU).include(ProductFieldNames.PRODUCT_SKU)
        .include(ProductFieldNames.MERCHANT_SKU).include(ProductFieldNames.PRICE)
        .include(ProductFieldNames.ITEM_VIEW_CONFIGS).include(ProductFieldNames.MERCHANT_PROMO_DISCOUNT)
        .include(ProductFieldNames.OFF2ON_CHANNEL_ACTIVE).include(ProductFieldNames.PROMO_BUNDLING)
        .include(ProductFieldNames.IS_ARCHIVED).include(ProductFieldNames.FORCE_REVIEW)
        .include(ProductFieldNames.VERSION).include(ProductFieldNames.ACTIVE_PROMO_BUNDLINGS)
        .include(ProductFieldNames.PICKUP_POINT_CODE);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public List<Item> getItemViewConfigsByItemSkus(String storeId, Set<String> itemSkus) {
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.MARK_FOR_DELETE).is(false)
            .and(ProductFieldNames.ITEM_SKU).in(itemSkus));
    query.fields().include(ProductFieldNames.ITEM_SKU).include(ProductFieldNames.ITEM_VIEW_CONFIGS)
        .include(ProductFieldNames.PRISTINE_DATA_ITEM);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public Item removeActivePromoBundling(String storeId, String itemSku,
      String promoBundlingType) {
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku));

    Update update = new Update();
    update.pull(ProductFieldNames.ACTIVE_PROMO_BUNDLINGS, promoBundlingType);

    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).findAndModify(query, update,
        new FindAndModifyOptions().returnNew(true), Item.class);
  }

  @Override
  public List<Item> findByStoreIdAndItemSkus(String storeId, Set<String> itemSkus, String... includes) {
    Query query =
        new Query(Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).in(itemSkus));
    Arrays.stream(includes).forEach(include -> query.fields().include(include));
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public List<Item> updateDangerousGoodsByItemSku(String storeId, Set<String> itemSkus,
      Integer dangerousLevel) {
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).in(itemSkus));
    Update update = new Update();
    update.set(ProductFieldNames.FULL_FIELD_DANGEROUS_LEVEL, dangerousLevel.intValue());
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).updateMulti(query, update, Item.DOCUMENT_NAME);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public Item updateFieldByItemSku(String storeId, String itemSku, String fieldName, Object value) {
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku));
    Update update = new Update();
    update.set(fieldName, value);
    Item result = this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .findAndModify(query, update, new FindAndModifyOptions().returnNew(true), Item.class);
    return result;
  }

  @Override
  public List<Item> updateFieldByItemSkus(String storeId, Collection<String> itemSkus, String fieldName, Object value) {
    Query query = new Query(where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).in(itemSkus));
    Update update = new Update().set(fieldName, value);
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).updateMulti(query, update, Item.class);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public Item updateOff2OnChannelActiveByItemSku(String storeId, String itemSku, boolean active) {
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku)
            .and(ProductFieldNames.OFF2ON_CHANNEL_ACTIVE).is(!active));
    Update update = new Update();
    update.set(ProductFieldNames.OFF2ON_CHANNEL_ACTIVE, active);
    Item result = this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .findAndModify(query, update, new FindAndModifyOptions().returnNew(true), Item.class);
    return result;
  }

  @Override
  public List<Item> updateOff2OnChannelActiveByProductSku(String storeId, String productSku,
      boolean active) {
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU)
            .is(productSku).and(ProductFieldNames.OFF2ON_CHANNEL_ACTIVE).is(!active));
    Update update = new Update();
    update.set(ProductFieldNames.OFF2ON_CHANNEL_ACTIVE, active);
    List<Item> result = this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
    if (!result.isEmpty()) {
      UpdateResult updateResult = this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).updateMulti(query,update,Item.class);
      if (updateResult.getModifiedCount() == result.size()) {
        for (Item item : result) {
          item.setOff2OnChannelActive(active);
        }
      }
    }
    return result;
  }

  @Override
  public List<Item> updateOff2OnChannelActiveByProductSkuAndUpdatedDateAndBy(String storeId,
    String productSku,
      boolean active, String userName) {
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU)
            .is(productSku).and(ProductFieldNames.OFF2ON_CHANNEL_ACTIVE).is(!active));
    Update update = new Update();
    update.set(ProductFieldNames.OFF2ON_CHANNEL_ACTIVE, active);
    update.set(ProductFieldNames.UPDATED_BY, userName);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    List<Item> result = this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
    if (!result.isEmpty()) {
      UpdateResult updateResult = this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).updateMulti(query, update, Item.class);
      if (updateResult.getModifiedCount() == result.size()) {
        for (Item item : result) {
          item.setOff2OnChannelActive(active);
        }
      }
    }
    return result;
  }

  @Override
  public List<String> getAllItemCodes(String storeId) {
    Query query = new Query(
        Criteria.where(ProductFieldNames.ITEM_CODE).exists(true).and(ProductFieldNames.STORE_ID)
            .is(storeId).and(ProductFieldNames.MARK_FOR_DELETE).is(false).and(ProductFieldNames.IS_ARCHIVED).is(false));
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).findDistinct(query, ProductFieldNames.ITEM_CODE, Item.DOCUMENT_NAME,
      String.class);
  }

  @Override
  public List<Item> getItemsWithPristineAvailable() {
    Query query = new Query(Criteria.where(ProductFieldNames.PRISTINE_DATA_ITEM).exists(true)
        .and(ProductFieldNames.MARK_FOR_DELETE).is(false).and(ProductFieldNames.IS_ARCHIVED).is(false));
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public Set<String> getItemCodesByPristine(String storeId, PristineDataItem pristineItem) {
    Query query = new Query(Criteria.where(String
        .join(ItemRepositoryImpl.DOT_SEPARATOR, ProductFieldNames.PRISTINE_DATA_ITEM,
            ProductFieldNames.ID)).is(pristineItem.getId()).and(ProductFieldNames.STORE_ID)
        .is(storeId).and(ProductFieldNames.MARK_FOR_DELETE).is(false).and(ProductFieldNames.IS_ARCHIVED).is(false));
    query.fields().include(ProductFieldNames.ITEM_CODE);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class).stream().map(item -> item.getItemCode())
        .collect(Collectors.toSet());
  }

  @Override
  public List<Item> findOneItemByPristine(String storeId, PristineDataItem pristineItem) {
    Query query = new Query(Criteria.where(String
        .join(ItemRepositoryImpl.DOT_SEPARATOR, ProductFieldNames.PRISTINE_DATA_ITEM,
            ProductFieldNames.ID)).is(pristineItem.getId()).and(ProductFieldNames.STORE_ID)
        .is(storeId).and(ProductFieldNames.MARK_FOR_DELETE).is(false).and(ProductFieldNames.IS_ARCHIVED).is(false));
    query.fields().include(ProductFieldNames.ITEM_CODE);
    query.limit(1);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public Set<String> findItemSkusByPristine(String storeId, PristineDataItem pristineItem) {
    Query query = new Query(Criteria.where(String
        .join(ItemRepositoryImpl.DOT_SEPARATOR, ProductFieldNames.PRISTINE_DATA_ITEM,
            ProductFieldNames.ID)).is(pristineItem.getId()).and(ProductFieldNames.STORE_ID)
        .is(storeId).and(ProductFieldNames.MARK_FOR_DELETE).is(false)
        .and(ProductFieldNames.IS_ARCHIVED).is(false));
    query.fields().include(ProductFieldNames.ITEM_SKU);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class).stream().map(Item::getItemSku)
        .collect(Collectors.toSet());
  }


  @Override
  public List<Item> getByProductSkuAndPristineDataItemExist(String storeId, String productSku) {
    Query query = new Query(Criteria.where(ProductFieldNames.PRISTINE_DATA_ITEM).exists(true)
        .and(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU)
        .is(productSku).and(ProductFieldNames.MARK_FOR_DELETE).is(false).and(ProductFieldNames.IS_ARCHIVED).is(false));
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public Item updateCncActivated(String storeId, String itemSku, boolean cncActivated, String username) {
    Query query = new Query(where(ProductFieldNames.STORE_ID).is(storeId)
        .and(ProductFieldNames.ITEM_SKU).is(itemSku));
    Update update = new Update();
    update.set(ProductFieldNames.UPDATED_BY, username);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    update.set(ProductFieldNames.CNC_ACTIVATED, cncActivated);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).findAndModify(query, update, new FindAndModifyOptions().returnNew(true), Item.class);
  }

  @Override
  public List<Item> updateCategoryCodeByItemSkus(String storeId, List<String> itemSkuList, String categoryCode) {
    Query query =
        new Query(where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).in(itemSkuList));
    Update update = new Update();
    update.set(ProductFieldNames.UPDATED_BY, CommonConstants.SYSTEM);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    update.set(ProductFieldNames.CATEGORY_CODE, categoryCode);
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).updateMulti(query, update, Item.class);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public void updateCncActivatedByMerchantCode(String storeId, String merchantCode, boolean cncActivated, String username) {
    Query query = new Query(where(ProductFieldNames.STORE_ID).is(storeId)
        .and(ProductFieldNames.MERCHANT_CODE).is(merchantCode));
    Update update = new Update();
    update.set(ProductFieldNames.UPDATED_BY, username);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    update.set(ProductFieldNames.CNC_ACTIVATED, cncActivated);
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).updateMulti(query, update, Item.class);
  }

  @Override
  public void updateCncActivatedByItemSkuInMarkForDeleteFalse(String storeId, Set<String> itemSkuSet,
      boolean cncActivated, String username) {
    Query query = new Query(where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).in(itemSkuSet)
        .and(ProductFieldNames.MARK_FOR_DELETE).is(FALSE));
    Update update = new Update();
    update.set(ProductFieldNames.UPDATED_BY, username);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    update.set(ProductFieldNames.CNC_ACTIVATED, cncActivated);
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).updateMulti(query, update, Item.class);
  }

  @Override
  public List<Item> findItemSkusByStoreIdAndPristineDataItemInAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalse(
      String storeId, List<PristineDataItem> pristineDataItems) {
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId)
            .and(ProductFieldNames.PRISTINE_DATA_ITEM).in(pristineDataItems)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(Boolean.FALSE)
            .and(ProductFieldNames.IS_ARCHIVED).is(Boolean.FALSE)
            .and(ProductFieldNames.CNC_ACTIVATED).is(Boolean.TRUE));
    query.fields().include(ProductFieldNames.ITEM_SKU);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public List<Item> findItemSkusByStoreIdAndItemCodeAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalseAndSynchronizedTrue(
      String storeId, String itemCode) {
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId)
            .and(ProductFieldNames.ITEM_CODE).in(itemCode)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(Boolean.FALSE)
            .and(ProductFieldNames.IS_ARCHIVED).is(Boolean.FALSE)
            .and(ProductFieldNames.CNC_ACTIVATED).is(Boolean.TRUE)
            .and(ProductFieldNames.IS_SYNCHRONIZED).is(Boolean.TRUE));
    query.fields().include(ProductFieldNames.ITEM_SKU);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public List<Item> findSpecificFieldsByStoreIdAndMarkForDeleteFalseAndIsArchivedFalseAndMerchantCode(
      String storeId, String merchantCode) {
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId)
            .and(ProductFieldNames.MERCHANT_CODE).in(merchantCode)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(Boolean.FALSE)
            .and(ProductFieldNames.IS_ARCHIVED).is(Boolean.FALSE));
    query.fields().include(ProductFieldNames.ITEM_SKU)
        .include(ProductFieldNames.PRODUCT_SKU)
        .include(ProductFieldNames.ITEM_CODE)
        .include(ProductFieldNames.PRISTINE_DATA_ITEM)
        .include(ProductFieldNames.MERCHANT_SKU)
        .include(ProductFieldNames.MERCHANT_CODE);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public Set<String> getAllActivePromoBundlingsByItemCode(String storeId, String itemCode) {
    Query query = new Query(
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_CODE)
            .is(itemCode));
    query.fields().include(ProductFieldNames.ACTIVE_PROMO_BUNDLINGS);

    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class).stream()
        .filter(item -> item.getActivePromoBundlings() != null)
        .flatMap(item -> item.getActivePromoBundlings().stream()).collect(Collectors.toSet());
  }

  @Override
  public Set<String> getAllActivePromoBundlingsByPristine(String storeId, PristineDataItem pristineItem) {
    Query query = new Query(Criteria.where(String
        .join(ItemRepositoryImpl.DOT_SEPARATOR, ProductFieldNames.PRISTINE_DATA_ITEM,
            ProductFieldNames.ID)).is(pristineItem.getId()).and(ProductFieldNames.STORE_ID)
        .is(storeId).and(ProductFieldNames.MARK_FOR_DELETE).is(false).and(ProductFieldNames.IS_ARCHIVED).is(false));
    query.fields().include(ProductFieldNames.ACTIVE_PROMO_BUNDLINGS);

    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class).stream()
        .filter(item -> item.getActivePromoBundlings() != null)
        .flatMap(item -> item.getActivePromoBundlings().stream()).collect(Collectors.toSet());
  }

  @Override
  public List<Item> findByStoreIdAndMarkForDeleteFalseAndProductSkus(String storeId, Set<String> productSkus,
      String... includes){
    Query query = new Query(Criteria.where(ProductFieldNames.STORE_ID).is(storeId)
        .and(ProductFieldNames.MARK_FOR_DELETE).is(FALSE).and(ProductFieldNames.PRODUCT_SKU)
        .in(productSkus).and(ProductFieldNames.IS_ARCHIVED).is(false));
    Arrays.stream(includes).forEach(include -> query.fields().include(include));
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);

  }

  @Override
  public List<Item> findItemCodesByStoreIdAndItemSkuIn(String storeId, Set<String> itemSkus){
    Query query = new Query(Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU)
        .in(itemSkus));
    query.fields().include(ProductFieldNames.ITEM_SKU)
        .include(ProductFieldNames.ITEM_CODE);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public List<Item> findItemCodeAndPristineDataItemByStoreIdAndItemSkuIn(String storeId, Set <String> itemSkus){
    Query query = new Query(
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU)
            .in(itemSkus));
    query.fields().include(ProductFieldNames.ITEM_SKU).include(ProductFieldNames.ITEM_CODE)
        .include(ProductFieldNames.PRISTINE_DATA_ITEM);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public Set<String> findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(String storeId, String itemCode){
    Query query = new Query(
        Criteria.where(ProductFieldNames.ITEM_CODE).is(itemCode).and(ProductFieldNames.STORE_ID)
            .is(storeId).and(ProductFieldNames.MARK_FOR_DELETE).is(false)
            .and(ProductFieldNames.IS_ARCHIVED).is(false));
    query.fields().include(ProductFieldNames.ITEM_SKU);

    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class).stream().map(item -> item.getItemSku())
        .collect(Collectors.toSet());

  }

  @Override
  public Set<String> findItemSkusByStoreIdAndItemCode(String storeId, String itemCode) {
    Query query =
        new Query(Criteria.where(ProductFieldNames.ITEM_CODE).is(itemCode).and(ProductFieldNames.STORE_ID).is(storeId));
    query.fields().include(ProductFieldNames.ITEM_SKU);

    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class).stream().map(item -> item.getItemSku()).collect(Collectors.toSet());
  }

  @Override
  public void updateItemDiscountPrice(String storeId, Item item){
    Query query = new Query(
        Criteria.where(ProductFieldNames.ITEM_SKU).is(item.getItemSku()).and(ProductFieldNames.STORE_ID)
            .is(storeId).and(ProductFieldNames.MARK_FOR_DELETE).is(false)
            .and(ProductFieldNames.IS_ARCHIVED).is(false));
    Update update = new Update();
    update.set(ProductFieldNames.PRICE, item.getPrice());
    try {
      this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).updateFirst(query, update, Item.class);
    }catch(Exception ex){
      LOGGER.error("Error while updating Promotion price for itemSku:{}", item.getItemSku(), ex);
    }
  }

  @Override
  public Item updateItemMerchantDiscountPrice(String storeId, Item item) {
    Query query = new Query(Criteria.where(ProductFieldNames.ITEM_SKU).is(item.getItemSku())
        .and(ProductFieldNames.STORE_ID).is(storeId));
    Update update = new Update();
    update.set(ProductFieldNames.PRICE, item.getPrice());
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .findAndModify(query, update, new FindAndModifyOptions().returnNew(true), Item.class);
  }

  @Override
  public void updatePristineDataItem(String storeId, Item item){
      Query query = new Query(Criteria.where(ProductFieldNames.ITEM_SKU).is(item.getItemSku())
          .and(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.MARK_FOR_DELETE)
          .is(false));
      Update update = new Update();
      update.set(ProductFieldNames.PRISTINE_DATA_ITEM, item.getPristineDataItem());
      update.set(ProductFieldNames.UPDATED_DATE, new Date());
      this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).updateFirst(query, update, Item.class);
  }

  @Override
  public boolean updateMerchantPromoDiscountFlag(String storeId, String itemSku, boolean isPromoActive) {
    Query query = new Query(
        Criteria.where(ProductFieldNames.ITEM_SKU).is(itemSku).and(ProductFieldNames.STORE_ID)
            .is(storeId));
    Update update = new Update();
    update.set(ProductFieldNames.MERCHANT_PROMO_DISCOUNT, isPromoActive);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).updateFirst(query, update, Item.class).getModifiedCount() > 0;
  }

  @Override
  public List<Item> getItemPriceAndViewConfigsAndPromoDetails(String storeId, List<String> itemSkus) {
    Query query = new Query(where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).in(itemSkus));
    query.fields().include(ProductFieldNames.ITEM_SKU).include(ProductFieldNames.PRODUCT_SKU)
        .include(ProductFieldNames.PRICE).include(ProductFieldNames.ITEM_VIEW_CONFIGS)
        .include(ProductFieldNames.MERCHANT_PROMO_DISCOUNT).include(ProductFieldNames.OFF2ON_CHANNEL_ACTIVE)
        .include(ProductFieldNames.PROMO_BUNDLING).include(ProductFieldNames.IS_ARCHIVED)
        .include(ProductFieldNames.FORCE_REVIEW).include(ProductFieldNames.VERSION)
        .include(ProductFieldNames.ACTIVE_PROMO_BUNDLINGS).include(ProductFieldNames.IS_FLASH_SALE_ACTIVE)
        .include(ProductFieldNames.WHOLESALE_PRICE_EXISTS).include(ProductFieldNames.MERCHANT_SKU);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public List<Item> getItemDetailsByItemCodes(Set<String> itemCodes) {
    Query query = new Query(where(ProductFieldNames.ITEM_CODE).in(itemCodes));
    query.fields().include(ProductFieldNames.ITEM_SKU).include(ProductFieldNames.PRODUCT_SKU)
        .include(ProductFieldNames.MERCHANT_CODE).include(ProductFieldNames.ITEM_CODE);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public List<Item> getItemSkuByItemNameKeyword(String storeId, String productSku, String keyword) {
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU).is(productSku)
            .and(ProductFieldNames.GENERATED_ITEM_NAME).is(keyword).and(ProductFieldNames.MARK_FOR_DELETE)
            .is(Boolean.FALSE));
    query.fields().include(ProductFieldNames.ITEM_SKU);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public List<Item> getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(String storeId, String merchantCode,
      Set<String> itemCodes, String keyword) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.MERCHANT_CODE).is(merchantCode);

    if (CollectionUtils.isNotEmpty(itemCodes)) {
      criteria = criteria.and(ProductFieldNames.ITEM_CODE).in(itemCodes);
    }

    if (StringUtils.isNotBlank(keyword)) {
      criteria = criteria.and(ProductFieldNames.GENERATED_ITEM_NAME).is(keyword);
    }

    criteria = criteria.and(ProductFieldNames.MARK_FOR_DELETE).is(FALSE);

    Query query = new Query(criteria);
    query.fields().include(ProductFieldNames.ITEM_SKU);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public Item findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku,
      boolean readFromPrimary) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).findOne(query, Item.class);
  }

  @Override
  public Item findItemByStoreIdAndItemSku(String storeId, String itemSku, boolean readFromPrimary) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).findOne(query, Item.class);
  }

  @Override
  public List<Item> findItemByStoreIdAndItemSkuIn(String storeId, Collection<String> itemSkus, boolean readFromPrimary) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).in(itemSkus);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public List<Item> findByStoreIdAndItemCodeInAndMarkForDeleteFalse(String storeId, Collection<String> itemCodes,
      boolean readFromPrimary) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_CODE).in(itemCodes)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public List<Item> findItemsByStoreIdAndProductSkuAndMarkForDelete(String storeId, String productSku,
      boolean markForDelete, boolean readFromPrimary) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU).is(productSku)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(markForDelete);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
  }

  @Override
  public Long countByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(String storeId, String productSku,
      boolean cncActive, boolean readFromPrimary) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU).is(productSku)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false).and(ProductFieldNames.CNC_ACTIVATED).is(cncActive);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).count(query, Item.class);
  }

  @Override
  public Page<Item> getItemSummaryResponsesByItemCodes(String storeId, String itemCode,
    String searchKey, int page, int size, String sortBy, String orderBy) {
    Criteria criteria =
      Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_CODE)
        .is(itemCode).and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    if (StringUtils.isNotBlank(searchKey)) {
      Criteria searchCriteria;
      if (skuValidator.isProductSku(searchKey)) {
        searchCriteria = Criteria.where(ProductFieldNames.PRODUCT_SKU).is(searchKey);
      } else {
        searchCriteria = Criteria.where(ProductFieldNames.GENERATED_ITEM_NAME).is(searchKey);
      }
      criteria.andOperator(searchCriteria);
    }

    Query query = new Query(criteria);
    query.fields().include(ProductFieldNames.ITEM_SKU).include(ProductFieldNames.PRODUCT_SKU)
      .include(ProductFieldNames.MERCHANT_CODE).include(ProductFieldNames.ITEM_CODE)
      .include(ProductFieldNames.GENERATED_ITEM_NAME).include(ProductFieldNames.IS_ARCHIVED)
      .include(ProductFieldNames.MAIN_IMAGE_URL);

    if (StringUtils.isNotBlank(sortBy) && StringUtils.isNotBlank(orderBy)) {
      query.with(PageRequest.of(page, size, Sort.by(Sort.Direction.valueOf(orderBy), sortBy)));
    } else {
      query.with(PageRequest.of(page, size, Sort.by(Sort.Direction.ASC, ProductFieldNames.ITEM_SKU)));
    }
    List<Item> itemList = this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Item.class);
    long totalCount = getItemCountByItemCodes(storeId, itemCode);
    return new PageImpl<>(itemList, PageRequest.of(page, size), totalCount);
  }

  @Override
  public boolean existsByStoreIdAndProductSkuAndCncActivated(String storeId, String productSku,
      boolean cncActivated) {
    Criteria criteria = Criteria.where(ProductFieldNames.STORE_ID).is(storeId)
        .and(ProductFieldNames.MARK_FOR_DELETE).is(false).and(ProductFieldNames.PRODUCT_SKU)
        .is(productSku).and(ProductFieldNames.CNC_ACTIVATED).is(cncActivated);
    Query query = new Query(criteria);
    query.limit(1);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).exists(query, Item.class);
  }

  @Override
  public List<Item> fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndMarkForDeleteAndIsArchived(
      String storeId, String upcCode, Set<String> merchantCodes, boolean markForDelete,
      boolean archived) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.UPC_CODE)
            .is(upcCode).and(ProductFieldNames.MERCHANT_CODE).in(merchantCodes)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(markForDelete)
            .and(ProductFieldNames.IS_ARCHIVED).is(archived);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, Item.class);
  }

  private long getItemCountByItemCodes(String storeId, String itemCode) {
    Criteria criteria = Criteria.where(ProductFieldNames.STORE_ID).is(storeId)
      .and(ProductFieldNames.ITEM_CODE).is(itemCode)
      .and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).count(query, Item.class);
  }

}
