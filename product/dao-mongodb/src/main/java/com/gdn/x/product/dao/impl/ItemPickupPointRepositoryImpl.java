package com.gdn.x.product.dao.impl;

import static org.springframework.data.mongodb.core.query.Criteria.where;

import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.FindAndModifyOptions;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Repository;

import com.gdn.x.product.dao.api.ItemPickupPointRepositoryCustom;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.vo.FieldUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointListingRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointSummaryRequestVo;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Repository
public class ItemPickupPointRepositoryImpl implements ItemPickupPointRepositoryCustom {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private Map<String, MongoTemplate> mongoTemplateFactory;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Override
  public List<ItemPickupPoint> updateDiscountPriceInItemPickupPoint(ItemPickupPoint itemPickupPoint) {
    Query query = new Query(
        Criteria.where(ProductFieldNames.ITEM_SKU).is(itemPickupPoint.getItemSku()).and(ProductFieldNames.DELIVERY)
            .is(true).and(ProductFieldNames.MARK_FOR_DELETE).is(Boolean.FALSE));
    Update update = new Update();
    update.set(ProductFieldNames.PRICE, itemPickupPoint.getPrice());
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .updateFirst(query, update, ItemPickupPoint.class);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  @Override
  public List<ItemPickupPoint> updateDiscountPriceInItemPickupPointByItemSkuAndPickupPointCode(
      ItemPickupPoint itemPickupPoint) {
    Query query = new Query(Criteria.where(ProductFieldNames.OFFLINE_ITEM_ID).is(itemPickupPoint.getOfflineItemId()));
    Update update = new Update();
    update.set(ProductFieldNames.PRICE, itemPickupPoint.getPrice());
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    update.set(ProductFieldNames.MERCHANT_PROMO_DISCOUNT, itemPickupPoint.isMerchantPromoDiscount());
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .updateFirst(query, update, ItemPickupPoint.class);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  @Override
  public void updatePriceByOfflineItemIds(List<String> offlineItemIds, double listPrice, double offerPrice) {
    Query query = new Query(Criteria.where(ProductFieldNames.OFFLINE_ITEM_ID).in(offlineItemIds));
    Update update = new Update();
    update.set(ProductFieldNames.FIRST_LIST_PRICE, listPrice);
    update.set(ProductFieldNames.FIRST_OFFER_PRICE, offerPrice);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .updateMulti(query, update, ItemPickupPoint.class);
  }

  @Override
  public List<ItemPickupPoint> fetchItemPickupPointsByItemSkuAndPickupPointCode(String storeId,
      List<ItemPickupPointRequestVo> itemPickupPointRequestVoList, boolean inAllProducts) {
    Criteria[] itemSkuAndPickupPointCriterias = new Criteria[itemPickupPointRequestVoList.size()];
    int index = 0;
    for (ItemPickupPointRequestVo itemPickupPointRequestVo : itemPickupPointRequestVoList) {
      itemSkuAndPickupPointCriterias[index++] = new Criteria().andOperator(
          Criteria.where(ProductFieldNames.ITEM_SKU).is(itemPickupPointRequestVo.getItemSku()),
          Criteria.where(ProductFieldNames.PICKUP_POINT_CODE).is(itemPickupPointRequestVo.getPickupPointCode()));
    }
    Criteria criteria;
    if (inAllProducts) {
      criteria = new Criteria().andOperator(Criteria.where(ProductFieldNames.STORE_ID).is(storeId),
          new Criteria().orOperator(itemSkuAndPickupPointCriterias));
    } else {
      criteria = new Criteria().andOperator(
          Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.MARK_FOR_DELETE).is(false),
          new Criteria().orOperator(itemSkuAndPickupPointCriterias));
    }
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  @Override
  public Page<ItemPickupPoint> getItemPickupPointListingByProductSku(String storeId, int page, int size,
      ItemPickupPointListingRequestVo itemPickupPointListingRequestVo) {
    Criteria criteria =
        getFilterCriteriaForItemPickupPointListing(storeId, itemPickupPointListingRequestVo);
    Query query = new Query(criteria);
    Sort sort;
    if (itemPickupPointListingRequestVo.isFbbSortRequired()) {
      //Supporting FBB l5 and created date oldest sorting
      sort =
        Sort.by(Sort.Direction.DESC, ProductFieldNames.ITEM_SKU, ProductFieldNames.FBB_ACTIVATED,
            ProductFieldNames.CREATED_DATE, ProductFieldNames.ID)
          .and(Sort.by(Sort.Direction.ASC, ProductFieldNames.CREATED_DATE));
    } else {
      sort = Sort.by(Sort.Direction.DESC, ProductFieldNames.OFFLINE_ITEM_ID);
    }
    query.with(PageRequest.of(page, size, sort));
    List<ItemPickupPoint> itemPickupPointList =
        this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, ItemPickupPoint.class);
    query.limit(0);
    query.skip(0);
    long totalCount =
        this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).count(query, ItemPickupPoint.class);
    return new PageImpl<>(itemPickupPointList, PageRequest.of(page,size), totalCount);
  }

  @Override
  public List<ItemPickupPoint> updateMerchantPromoDiscountFlag(String storeId, String itemSku, boolean isPromoActive) {
    Query query = new Query(
        Criteria.where(ProductFieldNames.ITEM_SKU).is(itemSku).and(ProductFieldNames.STORE_ID).is(storeId)
            .and(ProductFieldNames.DELIVERY).is(true).and(ProductFieldNames.MARK_FOR_DELETE).is(Boolean.FALSE));
    Update update = new Update();
    update.set(ProductFieldNames.MERCHANT_PROMO_DISCOUNT, isPromoActive);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .updateFirst(query, update, ItemPickupPoint.class);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  @Override
  public List<ItemPickupPoint> updateFieldByItemSkusAndDelivery(String storeId, Collection<String> itemSkus, String fieldName,
      boolean delivery, Object value) {
    Query query = new Query(where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).in(itemSkus)
        .and(ProductFieldNames.DELIVERY).is(delivery).and(ProductFieldNames.MARK_FOR_DELETE).is(Boolean.FALSE));
    Update update = new Update().set(fieldName, value);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .updateMulti(query, update, ItemPickupPoint.class);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  @Override
  public List<ItemPickupPoint> updateFieldByItemSkusAndItemInfos(String storeId, List<ItemPickupPointRequestVo> itemPickupPointRequestVos,
      String fieldName, Object value) {
    Set<String> offlineItemIds = generateListOfOfflineItemIds(itemPickupPointRequestVos);
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.OFFLINE_ITEM_ID).in(offlineItemIds));
    Update update = new Update().set(fieldName, value);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .updateMulti(query, update, ItemPickupPoint.class);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  @Override
  public ItemPickupPoint updateFieldByItemSku(String storeId, String itemSku, String fieldName, Object value) {
    Query query = new Query(where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku)
        .and(ProductFieldNames.DELIVERY).is(Boolean.TRUE).and(ProductFieldNames.MARK_FOR_DELETE).is(Boolean.FALSE));
    Update update = new Update();
    update.set(fieldName, value);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    ItemPickupPoint result = null;
    try {
      result = this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
          .findAndModify(query, update, new FindAndModifyOptions().returnNew(true), ItemPickupPoint.class);
    } catch (Exception e) {
      log.error(
          "Error while updating itemPickupPoint using itemSku : {} , fieldName : {} , value : {} , Exception : {} ",
          itemSku, fieldName, value, e);
    }
    return result;
  }

  @Override
  public List<ItemPickupPoint> updateFieldsByItemSku(String storeId, String itemSku, String fieldName, Object value) {
    Query query = new Query(where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku));
    Update update = new Update().set(fieldName, value);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .updateMulti(query, update, ItemPickupPoint.class);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  @Override
  public Set<String> getActivePromoBundlingsByItemSkus(String storeId, List<String> itemSkus) {
    Query query = new Query(
        Criteria.where(ProductFieldNames.ITEM_SKU).in(itemSkus).and(ProductFieldNames.STORE_ID).is(storeId)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false).and(ProductFieldNames.DELIVERY).is(true));
    query.fields().include(ProductFieldNames.ACTIVE_PROMO_BUNDLINGS);

    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class).stream()
        .flatMap(itemPickupPoint -> itemPickupPoint.getActivePromoBundlings().stream()).collect(Collectors.toSet());
  }

  @Override
  public Set<String> getActivePromoBundlingsByItemSkusAndMarkForDeleteFalse(String storeId,
      List<String> itemSkus, String pickupPointCode) {
    Query query = new Query(
        Criteria.where(ProductFieldNames.ITEM_SKU).in(itemSkus).and(ProductFieldNames.STORE_ID).is(storeId)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false).and(ProductFieldNames.PICKUP_POINT_CODE).is(pickupPointCode));
    query.fields().include(ProductFieldNames.ACTIVE_PROMO_BUNDLINGS);

    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class).stream()
        .flatMap(itemPickupPoint -> itemPickupPoint.getActivePromoBundlings().stream()).collect(Collectors.toSet());
  }

  @Override
  public Page<ItemPickupPoint> getItemPickupPointListing(String storeId, int page, int size,
      ItemPickupPointSummaryRequestVo itemPickupPointSummaryRequestVo) {
    Query query = getFilterQueryForItemPickupPointSummary(storeId, page, size, itemPickupPointSummaryRequestVo);
    List<ItemPickupPoint> itemPickupPointList =
        this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, ItemPickupPoint.class);
    query.limit(0);
    query.skip(0);
    long totalCount =
        this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).count(query, ItemPickupPoint.class);
    return new PageImpl<>(itemPickupPointList, PageRequest.of(page, size), totalCount);
  }

  private Query getFilterQueryForItemPickupPointSummary(String storeId, int page, int size,
      ItemPickupPointSummaryRequestVo itemPickupPointSummaryRequestVo) {
    Criteria criteria = getFilterCriteriaForItemPickupPointSummary(storeId, itemPickupPointSummaryRequestVo);
    Query query = new Query(criteria);

    //Add sort field. Default offline itemId
    if (StringUtils.isNotBlank(itemPickupPointSummaryRequestVo.getSortField())) {
      if (StringUtils.isNotBlank(itemPickupPointSummaryRequestVo.getSortOrder())) {
        query.with(PageRequest.of(page, size,
          Sort.by(Sort.Direction.valueOf(itemPickupPointSummaryRequestVo.getSortOrder()),
            itemPickupPointSummaryRequestVo.getSortField())));
      } else {
        query.with(
          PageRequest.of(page,size, Sort.by(Sort.Direction.ASC,
            itemPickupPointSummaryRequestVo.getSortField())));
      }
    } else {
      if (StringUtils.isNotBlank(itemPickupPointSummaryRequestVo.getSortOrder())) {
        query.with(PageRequest.of(page, size,
          Sort.by(Sort.Direction.valueOf(itemPickupPointSummaryRequestVo.getSortOrder()),
            ProductFieldNames.OFFLINE_ITEM_ID)));
      } else {
        query.with(PageRequest.of(page,size, Sort.by(Sort.Direction.ASC,
          ProductFieldNames.OFFLINE_ITEM_ID)));
      }
    }

    return query;
  }

  private Criteria getFilterCriteriaForItemPickupPointListing(String storeId,
      ItemPickupPointListingRequestVo itemPickupPointListingRequestVo) {

    //Get default criteria with storeId and markForDelete false
    Criteria criteria = getDefaultCriteria(storeId);

    //Add productSku criteria
    criteria = criteria.and(ProductFieldNames.PRODUCT_SKU).is(itemPickupPointListingRequestVo.getProductSku());

    //Add merchant code in criteria
    criteria =
        criteria.and(ProductFieldNames.MERCHANT_CODE).is(itemPickupPointListingRequestVo.getBusinessPartnerCode());

    //Add itemSkus criteria
    if (CollectionUtils.isNotEmpty(itemPickupPointListingRequestVo.getItemSkus())) {
      criteria = criteria.and(ProductFieldNames.ITEM_SKU).in(itemPickupPointListingRequestVo.getItemSkus());
    }

    //Add pickupPointCodes in criteria
    if (CollectionUtils.isNotEmpty(itemPickupPointListingRequestVo.getPickupPointCodes())) {
      criteria =
          criteria.and(ProductFieldNames.PICKUP_POINT_CODE).in(itemPickupPointListingRequestVo.getPickupPointCodes());
    }
    return criteria;
  }

  private Criteria getFilterCriteriaForItemPickupPointSummary(String storeId,
      ItemPickupPointSummaryRequestVo itemPickupPointSummaryRequestVo) {

    //Get default criteria with storeId and markForDelete false
    Criteria criteria = getDefaultCriteria(storeId);

    //Add merchant code in criteria
    if (StringUtils.isNotBlank(itemPickupPointSummaryRequestVo.getMerchantCode())) {
     criteria = criteria.and(ProductFieldNames.MERCHANT_CODE).is(itemPickupPointSummaryRequestVo.getMerchantCode());
    }

    //Add productSku criteria
    if (CollectionUtils.isNotEmpty(itemPickupPointSummaryRequestVo.getProductSkuList())) {
      criteria = criteria.and(ProductFieldNames.PRODUCT_SKU)
          .in(new HashSet<>(itemPickupPointSummaryRequestVo.getProductSkuList()));
    }

    //Add itemSkus in criteria
    if (CollectionUtils.isNotEmpty(itemPickupPointSummaryRequestVo.getItemSkus())) {
      criteria = criteria.and(ProductFieldNames.ITEM_SKU)
          .in(new HashSet<>(itemPickupPointSummaryRequestVo.getItemSkus()));
    }

    //Add pickupPointCodes in criteria
    if (CollectionUtils.isNotEmpty(itemPickupPointSummaryRequestVo.getPickupPointCodes())) {
     criteria = criteria.and(ProductFieldNames.PICKUP_POINT_CODE)
          .in(new HashSet<>(itemPickupPointSummaryRequestVo.getPickupPointCodes()));
    }

    //Add itemSkusAndPickupPointCodes in criteria
    if (CollectionUtils.isNotEmpty(itemPickupPointSummaryRequestVo.getItemPickupPointCode())) {
     criteria = criteria.and(ProductFieldNames.OFFLINE_ITEM_ID)
          .in(generateListOfOfflineItemIds(itemPickupPointSummaryRequestVo.getItemPickupPointCode()));
    }

    //Add online criteria
    if (Objects.nonNull(itemPickupPointSummaryRequestVo.getOnline())) {
      if (itemPickupPointSummaryRequestVo.getOnline()) {
        criteria = new Criteria().andOperator(criteria,
            Criteria.where(ProductFieldNames.ITEM_VIEW_CONFIGS).elemMatch(
                Criteria.where(ProductFieldNames.CHANNEL).is(Constants.DEFAULT)
                    .and(ProductFieldNames.IS_BUYABLE).is(Boolean.TRUE)
                    .and(ProductFieldNames.IS_DISCOVERABLE).is(Boolean.TRUE)));
      } else {
        if (!cncForWarehouseFeatureSwitch) {
          criteria = criteria.and(ProductFieldNames.CNC_ACTIVE).is(Boolean.TRUE);
        } else {
          criteria = new Criteria().andOperator(criteria,
              Criteria.where(ProductFieldNames.ITEM_VIEW_CONFIGS)
                  .elemMatch(getCncBuyableViewConfigCriteria(Constants.CNC)));
        }
      }
    }

    //Add onlineOrCNC criteria
    if (Objects.nonNull(itemPickupPointSummaryRequestVo.getOnlineOrCnc())
        && itemPickupPointSummaryRequestVo.getOnlineOrCnc()) {
      if (cncForWarehouseFeatureSwitch) {
        criteria = new Criteria().andOperator(criteria, new Criteria().orOperator(
            Criteria.where(ProductFieldNames.ITEM_VIEW_CONFIGS)
                .elemMatch(getCncBuyableViewConfigCriteria(Constants.CNC)),
                Criteria.where(ProductFieldNames.ITEM_VIEW_CONFIGS).elemMatch(
                    Criteria.where(ProductFieldNames.CHANNEL).is(Constants.DEFAULT)
                        .and(ProductFieldNames.IS_BUYABLE).is(Boolean.TRUE)
                        .and(ProductFieldNames.IS_DISCOVERABLE).is(Boolean.TRUE))));
      } else {
        criteria = new Criteria().andOperator(criteria,
            new Criteria().orOperator(Criteria.where(ProductFieldNames.CNC_ACTIVE).is(Boolean.TRUE),
                Criteria.where(ProductFieldNames.ITEM_VIEW_CONFIGS).elemMatch(
                    Criteria.where(ProductFieldNames.IS_BUYABLE).is(Boolean.TRUE)
                        .and(ProductFieldNames.IS_DISCOVERABLE).is(Boolean.TRUE))));
      }
    }

    return criteria;
  }

  private Criteria getDefaultCriteria(String storeId) {
    return Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.MARK_FOR_DELETE).is(false);
  }

  private Set<String> generateListOfOfflineItemIds(List<ItemPickupPointRequestVo> itemPickupPointRequestVos) {
    return itemPickupPointRequestVos.stream().map(
        itemPickupPointRequestVo -> new StringBuilder(itemPickupPointRequestVo.getItemSku()).append(Constants.HYPHEN)
            .append(itemPickupPointRequestVo.getPickupPointCode()).toString()).collect(Collectors.toSet());
  }

  @Override
  public void updateFieldsByItemSkuAndPickupPointCodes(String storeId, String username,
      List<ItemPickupPointRequestVo> itemPickupPointRequestVoList,
      List<FieldUpdateRequestVo> fieldUpdateRequestVoList) {

    //generate criteria query
    Criteria[] itemSkuAndPickupPointCriterias = new Criteria[itemPickupPointRequestVoList.size()];
    int index = 0;
    for (ItemPickupPointRequestVo itemPickupPointRequestVo : itemPickupPointRequestVoList) {
      itemSkuAndPickupPointCriterias[index++] = new Criteria().andOperator(
          Criteria.where(ProductFieldNames.ITEM_SKU).is(itemPickupPointRequestVo.getItemSku()),
          Criteria.where(ProductFieldNames.PICKUP_POINT_CODE).is(itemPickupPointRequestVo.getPickupPointCode()));
    }
    Criteria criteria = new Criteria().andOperator(
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.MARK_FOR_DELETE).is(false),
        new Criteria().orOperator(itemSkuAndPickupPointCriterias));

    //generate update query
    Update update = new Update();
    update.set(ProductFieldNames.UPDATED_BY, username);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    for (FieldUpdateRequestVo fieldUpdateRequestVo : fieldUpdateRequestVoList) {
      if (Objects.nonNull(fieldUpdateRequestVo.getValue())) {
        update.set(fieldUpdateRequestVo.getField(), fieldUpdateRequestVo.getValue());
      } else  {
        update.unset(fieldUpdateRequestVo.getField());
      }
    }

    //fire update query
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .updateMulti(new Query(criteria), update, ItemPickupPoint.class);
  }

  @Override
  public void deleteItemPickupPointByItemSku(String storeId, String itemSku) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku);
    Update update = new Update();
    update.set(ProductFieldNames.MARK_FOR_DELETE, true);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    update.set(ProductFieldNames.UPDATED_BY, Constants.DEFAULT_USERNAME);
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .updateMulti(new Query(criteria), update, ItemPickupPoint.class);
  }

  @Override
  public List<ItemPickupPoint> getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(String storeId,
      String productSku, Set<String> ids) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU).is(productSku)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false).and(ProductFieldNames.ID).nin(ids);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(new Query(criteria), ItemPickupPoint.class);
  }

  @Override
  public ItemPickupPoint findL5BasedOnSkuAndOnlineOrCncFlag(String storeId, String productSku,
      String itemSku, boolean cncForWarehouseFeatureSwitch) {
    Criteria criteria =
        getFilterCriteriaBasedOnSkuAndOnlineOrCncFlag(storeId, productSku, itemSku, false, true,
            cncForWarehouseFeatureSwitch);
    ItemPickupPoint itemPickupPoint = this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .findOne(new Query(criteria), ItemPickupPoint.class);
    if (Objects.isNull(itemPickupPoint)) {
      criteria =
          getFilterCriteriaBasedOnSkuAndOnlineOrCncFlag(storeId, productSku, itemSku, true, false,
              cncForWarehouseFeatureSwitch);
      itemPickupPoint = this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
          .findOne(new Query(criteria), ItemPickupPoint.class);
    }
    if (Objects.isNull(itemPickupPoint)) {
      criteria =
          getFilterCriteriaBasedOnSkuAndOnlineOrCncFlag(storeId, productSku, itemSku, false, false,
              cncForWarehouseFeatureSwitch);
      itemPickupPoint = this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
          .findOne(new Query(criteria), ItemPickupPoint.class);
    }
    return itemPickupPoint;
  }

  @Override
  public List<ItemPickupPoint> updateFbbFlagByProductSkuAndPickupPointCode(String storeId, String productSku, String pickupPointCode,
    boolean fbbActivated) {
    Query query = new Query(
      Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU)
        .is(productSku).and(ProductFieldNames.PICKUP_POINT_CODE).is(pickupPointCode)
        .and(ProductFieldNames.MARK_FOR_DELETE).is(Boolean.FALSE));
    Update update = new Update();
    update.set(ProductFieldNames.FBB_ACTIVATED, fbbActivated);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .updateMulti(query, update, ItemPickupPoint.class);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  private Criteria getFilterCriteriaBasedOnSkuAndOnlineOrCncFlag(String storeId, String productSku,
      String itemSku, boolean cnc, boolean online, boolean cncForWarehouseFeatureSwitch) {
    Criteria criteria = getDefaultCriteria(storeId);

    if (StringUtils.isNotEmpty(productSku)) {
      criteria = criteria.and(ProductFieldNames.PRODUCT_SKU).is(productSku);
    } else {
      criteria = criteria.and(ProductFieldNames.ITEM_SKU).is(itemSku);
    }

    if (online) {
      criteria = new Criteria().andOperator(criteria, Criteria.where(ProductFieldNames.ITEM_VIEW_CONFIGS).elemMatch(
          Criteria.where(ProductFieldNames.IS_BUYABLE).is(Boolean.TRUE).and(ProductFieldNames.IS_DISCOVERABLE)
              .is(Boolean.TRUE)));
    }
    if (cnc) {
      if (cncForWarehouseFeatureSwitch) {
        criteria = criteria.and(ProductFieldNames.ITEM_VIEW_CONFIGS)
            .elemMatch(getCncBuyableViewConfigCriteria(Constants.CNC));
      } else {
        criteria = criteria.and(ProductFieldNames.CNC_ACTIVE).is(Boolean.TRUE);
      }
    }
    return criteria;
  }

  private Criteria getFilterCriteriaForOnlineFbbProducts(String storeId,
      boolean fbbActivated, List<String> itemSkus) {

    //Get default criteria with storeId and markForDelete false
    Criteria criteria = getDefaultCriteria(storeId);

    //Add itemSkus in criteria
      criteria = criteria.and(ProductFieldNames.ITEM_SKU)
          .in(new HashSet<>(itemSkus));

    //Add online criteria and fbb true
    criteria = new Criteria().andOperator(criteria,
        new Criteria().andOperator(Criteria.where(ProductFieldNames.FBB_ACTIVATED).is(fbbActivated),
            Criteria.where(ProductFieldNames.ITEM_VIEW_CONFIGS_WITH_CHANNEL).is(ProductFieldNames.DEFAULT_CHANNEL),
            new Criteria().orOperator(
                Criteria.where(ProductFieldNames.ITEM_VIEW_CONFIGS_WITH_IS_BUYABLE).is(Boolean.TRUE),
                Criteria.where(ProductFieldNames.ITEM_VIEW_CONFIGS_WITH_BUYABLE_SCHEDULES_IS_BUYABLE)
                    .is(Boolean.TRUE))));
    return criteria;
  }

  @Override
  public List<ItemPickupPoint> findFbbTrueOnlinePickupPointsAndItemSkusIn(String storeId, List<String> itemSkus) {
    Criteria criteria = getFilterCriteriaForOnlineFbbProducts(storeId, true, itemSkus);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(new Query(criteria), ItemPickupPoint.class);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndItemSkuInAndMarkForDeleteFalse(String storeId, List<String> itemSku,
      boolean primaryReadEnabled) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).in(itemSku)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  @Override
  public ItemPickupPoint findByStoreIdAndItemSkuAndPickupPointCode(String storeId, String itemSku,
      String pickupPointCode, boolean primaryReadEnabled) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku)
            .and(ProductFieldNames.PICKUP_POINT_CODE).is(pickupPointCode);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .findOne(query, ItemPickupPoint.class);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku,
      boolean primaryReadEnabled) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndItemSkuAndSubscribableAndMarkForDeleteFalse(String storeId,
      String itemSku, boolean subscribable, boolean primaryReadEnabled) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku)
            .and(ProductFieldNames.SUBSCRIBABLE).is(subscribable).and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndItemSkuInAndFbbActivatedAndMarkForDeleteFalse(String storeId,
      Collection<String> itemSku, boolean fbbActivated, boolean primaryReadEnabled) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).in(itemSku)
            .and(ProductFieldNames.FBB_ACTIVATED).is(fbbActivated).and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  @Override
  public ItemPickupPoint findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(String storeId, String itemSku,
      String pickupPointCode, boolean primaryReadEnabled) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku)
            .and(ProductFieldNames.PICKUP_POINT_CODE).is(pickupPointCode).and(ProductFieldNames.MARK_FOR_DELETE)
            .is(false);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .findOne(query, ItemPickupPoint.class);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndProductSku(String storeId, String productSku,
      boolean primaryReadEnabled) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU).is(productSku);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndProductSkuAndPickupPointCodeInAndMarkForDeleteFalse(String storeId,
      String productSku, Collection<String> pickupPointCode, boolean primaryReadEnabled) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU).is(productSku)
            .and(ProductFieldNames.PICKUP_POINT_CODE).in(pickupPointCode).and(ProductFieldNames.MARK_FOR_DELETE)
            .is(false);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku,
      boolean primaryReadEnabled) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU).is(productSku)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  @Override
  public Long countByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku,
      boolean primaryReadEnabled) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .count(query, ItemPickupPoint.class);
  }

  @Override
  public Long countByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(String storeId, String itemSku,
      boolean cncActive, boolean primaryReadEnabled) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku)
            .and(ProductFieldNames.CNC_ACTIVE).is(cncActive).and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .count(query, ItemPickupPoint.class);
  }

  @Override
  public List<ItemPickupPoint> fetchBasicDataByItemSkuAndPickupPointCode(String storeId,
    List<ItemPickupPointRequestVo> itemPickupPointRequestVoSubList) {
    // common criteria for storeId and markForDelete
    Criteria baseCriteria = Criteria.where(ProductFieldNames.STORE_ID).is(storeId)
      .and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    // Create a list for itemSku and pickupPointCode criteria
    List<Criteria> itemSkuAndPickupPointCriteriaList = itemPickupPointRequestVoSubList.stream()
      .map(item -> Criteria.where(ProductFieldNames.ITEM_SKU).is(item.getItemSku())
        .and(ProductFieldNames.PICKUP_POINT_CODE).is(item.getPickupPointCode()))
      .collect(Collectors.toList());
    // Combine base criteria with the OR combination of itemSku and pickupPointCode criteria
    Criteria combinedCriteria = new Criteria().andOperator(baseCriteria,
      new Criteria().andOperator(itemSkuAndPickupPointCriteriaList.toArray(new Criteria[0])));
    Query query = new Query(combinedCriteria);
    //projection only needed on view configs
    query.fields().include(ProductFieldNames.ITEM_VIEW_CONFIGS).include(ProductFieldNames.ITEM_SKU)
      .include(ProductFieldNames.PICKUP_POINT_CODE);

    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class).stream().collect(Collectors.toList());
  }

  @Override
  public ItemPickupPoint findFirstByItemSkuAndCncBuyableConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
      String storeId, String itemSku, Set<String> pickupPointCodes, boolean cncActivated) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU)
            .is(itemSku).and(ProductFieldNames.PICKUP_POINT_CODE).nin(pickupPointCodes)
            .and(ProductFieldNames.ITEM_VIEW_CONFIGS)
            .elemMatch(getCncTrueViewConfigCriteria(ProductFieldNames.CNC_CHANNEL, cncActivated))
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .findOne(query, ItemPickupPoint.class);
  }

  @Override
  public ItemPickupPoint findFirstByProductSkuAndCncBuyableConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
      String storeId, String productSku, Set<String> pickupPointCodes, boolean cncActivated) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU)
            .is(productSku).and(ProductFieldNames.PICKUP_POINT_CODE).nin(pickupPointCodes)
            .and(ProductFieldNames.ITEM_VIEW_CONFIGS)
            .elemMatch(getCncTrueViewConfigCriteria(ProductFieldNames.CNC_CHANNEL, cncActivated))
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .findOne(query, ItemPickupPoint.class);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndPickupPointCodeAndConfigFlagValueAndMarkForDeleteAndChannel(
      String storeId, String pickupPointCode, boolean configFlagValue, boolean markForDelete,
      String channel) {
    Criteria criteria = Criteria.where(ProductFieldNames.STORE_ID).is(storeId)
        .and(ProductFieldNames.PICKUP_POINT_CODE).is(pickupPointCode)
        .and(ProductFieldNames.ITEM_VIEW_CONFIGS)
        .elemMatch(getCncTrueViewConfigCriteria(channel, configFlagValue))
        .and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndMerchantCodeAndConfigFlagValueAndMarkForDeleteAndChannel(
      String storeId, String merchantCode, boolean configFlagValue, boolean markForDelete,
      String channel) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.MERCHANT_CODE)
            .is(merchantCode).and(ProductFieldNames.ITEM_VIEW_CONFIGS)
            .elemMatch(getCncTrueViewConfigCriteria(channel, configFlagValue))
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  private Criteria getCncTrueViewConfigCriteria(String cncChannel, boolean cncActivated) {
    return new Criteria().andOperator(Criteria.where(ProductFieldNames.CHANNEL).is(cncChannel),
        new Criteria().orOperator(Criteria.where(ProductFieldNames.IS_BUYABLE).is(cncActivated),
            Criteria.where(ProductFieldNames.IS_DISCOVERABLE).is(cncActivated)));
  }

  @Override
  public Long countByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(String storeId, String itemSku,
      String cncChannel, boolean readFromPrimary) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false)
            .and(ProductFieldNames.ITEM_VIEW_CONFIGS).elemMatch(getCncTrueViewConfigCriteria(cncChannel, true));
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).count(query, ItemPickupPoint.class);
  }

  @Override
  public boolean existsByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(String storeId, String itemSku,
      String cncChannel) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false)
            .and(ProductFieldNames.ITEM_VIEW_CONFIGS).elemMatch(getCncBuyableViewConfigCriteria(cncChannel));
    Query query = new Query(criteria);
    query.limit(1);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).exists(query, ItemPickupPoint.class);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(String storeId, String itemSku,
      String cncChannel) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.ITEM_SKU).is(itemSku)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false)
            .and(ProductFieldNames.ITEM_VIEW_CONFIGS).elemMatch(getCncBuyableViewConfigCriteria(cncChannel));
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, ItemPickupPoint.class);
  }

  @Override
  public List<ItemPickupPoint> findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(
      String storeId, Set<String> offlineItemIds, boolean buyable, String channel, int limit) {
    Criteria criteria;
    if(StringUtils.isNotBlank(channel)) {
      criteria = Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.OFFLINE_ITEM_ID)
          .in(offlineItemIds).and(ProductFieldNames.MARK_FOR_DELETE).is(false).and(ProductFieldNames.ITEM_VIEW_CONFIGS)
          .elemMatch(
              Criteria.where(ProductFieldNames.CHANNEL).is(channel).and(ProductFieldNames.IS_BUYABLE).is(buyable));
    } else {
      criteria = Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.OFFLINE_ITEM_ID)
          .in(offlineItemIds).and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    }
    Query query = new Query(criteria);
    query.limit(limit);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, ItemPickupPoint.class);
  }

  private Criteria getCncBuyableViewConfigCriteria(String cncChannel) {
    return new Criteria().andOperator(
        Criteria.where(ProductFieldNames.CHANNEL).is(cncChannel).and(ProductFieldNames.IS_BUYABLE).is(true));
  }

  @Override
  public Page<ItemPickupPoint> findByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncBuyable(
      String storeId, List<String> productSku, String cncChannel, int page, int size, Sort sort) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU)
            .in(productSku).and(ProductFieldNames.MARK_FOR_DELETE).is(false)
            .and(ProductFieldNames.ITEM_VIEW_CONFIGS)
            .elemMatch(getCncBuyableViewConfigCriteria(cncChannel));
    Query query = new Query(criteria);
    query.with(PageRequest.of(page, size, sort));
    List<ItemPickupPoint> itemPickupPointList =
        this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
            .find(query, ItemPickupPoint.class);
    query.limit(0);
    query.skip(0);
    long totalCount = this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .count(query, ItemPickupPoint.class);
    return new PageImpl<>(itemPickupPointList, PageRequest.of(page, size), totalCount);

  }

  @Override
  public ItemPickupPoint findFirstByProductSkuAndCncBuyableConfigAndMarkForDeleteFalse(
      String storeId, String productSku, boolean cncActivated) {
    Criteria criteria =
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU)
            .is(productSku)
            .and(ProductFieldNames.ITEM_VIEW_CONFIGS)
            .elemMatch(getCncTrueViewConfigCriteria(ProductFieldNames.CNC_CHANNEL, cncActivated))
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false);
    Query query = new Query(criteria);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .findOne(query, ItemPickupPoint.class);
  }
}
