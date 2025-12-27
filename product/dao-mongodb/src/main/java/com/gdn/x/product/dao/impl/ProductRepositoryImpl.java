package com.gdn.x.product.dao.impl;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;
import static org.springframework.data.mongodb.core.query.Criteria.where;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.FindAndModifyOptions;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Repository;

import com.gdn.x.product.dao.api.ProductRepositoryCustom;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;

@Repository
public class ProductRepositoryImpl implements ProductRepositoryCustom {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private Map<String, MongoTemplate> mongoTemplateFactory;

  @Override
  public List<Product> findByStoreIdAndProductSkusAndMarkForDeleteFalse(String storeId,
      List<String> productSkus, String... includes) {
    Query query = new Query(
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU)
            .in(productSkus).and(ProductFieldNames.MARK_FOR_DELETE).is(FALSE));
    for (String include : includes) {
      query.fields().include(include);
    }
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Product.class);
  }

  @Override
  public List<Product> findSyncProductByStoreIdAndProductCodesAndMarkForDeleteFalse(String storeId,
      Set<String> productCodes, String... includes) {
    Query query = new Query(Criteria.where(ProductFieldNames.STORE_ID).is(storeId)
        .and(ProductFieldNames.PRODUCT_CODE).in(productCodes).and(ProductFieldNames.IS_SYNCHRONIZED)
        .is(TRUE).and(ProductFieldNames.MARK_FOR_DELETE).is(FALSE));
    for (String include : includes) {
      query.fields().include(include);
    }
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Product.class);
  }

  @Override
  public void saveWithCustomAuditValue(Product product) {
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).save(product);
  }

  @Override
  public Product updateFieldByProductSku(String storeId, String productSku, String fieldName,
      Object value) {
    Query query = new Query(where(ProductFieldNames.STORE_ID).is(storeId)
        .and(ProductFieldNames.PRODUCT_SKU).is(productSku));
    Update update = new Update();
    update.set(fieldName, value);
    Product result = this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).findAndModify(query, update,
        new FindAndModifyOptions().returnNew(true), Product.class);
    return result;
  }

  @Override
  public void updateCncActivatedByMerchantCode(String storeId, String merchantCode, boolean cncActivated,
      String username) {
    Query query =
        new Query(where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.MERCHANT_CODE).is(merchantCode));
    Update update = new Update();
    update.set(ProductFieldNames.UPDATED_BY, username);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    update.set(ProductFieldNames.CNC_ACTIVATED, cncActivated);
    this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).updateMulti(query, update, Product.class);
  }

  @Override
  public List<Product> updateCncActivatedByProductSkusMarkForDeleteFalse(String storeId, Set<String> productSkuSet,
      boolean cncActivated, String username) {
    List<Product> updatedProducts = new ArrayList<>();
    Update update = new Update();
    update.set(ProductFieldNames.UPDATED_BY, username);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    update.set(ProductFieldNames.CNC_ACTIVATED, cncActivated);
    for (String productSku : productSkuSet) {
      updatedProducts.add(this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).findAndModify(new Query(
              where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU).is(productSku)
                  .and(ProductFieldNames.MARK_FOR_DELETE).is(FALSE)), update, new FindAndModifyOptions().returnNew(true),
          Product.class));
    }
    return updatedProducts;
  }

  @Override
  public Product updateOff2OnItemCountIncrementByProductSku(String storeId, String productSku,
      int increment) {
    Query query = new Query(where(ProductFieldNames.STORE_ID).is(storeId)
        .and(ProductFieldNames.PRODUCT_SKU).is(productSku));
    Update update = new Update();
    update.inc(ProductFieldNames.OFF2ON_ITEM_COUNT, increment);
    Product result = this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).findAndModify(query, update,
        new FindAndModifyOptions().returnNew(true), Product.class);
    return result;
  }

  @Override
  public Product updateOff2OnItemCountIncrementByProductSkuAndFlag(String storeId, String productSku, boolean activate,
      int itemsCount, String userName) {
    Query query = new Query(
        where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU).is(productSku));
    Update update = new Update();
    update.set(ProductFieldNames.OFF2ON_ITEM_COUNT, itemsCount);
    update.set(ProductFieldNames.OFF2ON_CHANNEL_ACTIVE, activate);
    update.set(ProductFieldNames.UPDATED_BY, userName);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).findAndModify(query, update, new FindAndModifyOptions().returnNew(true), Product.class);
  }

  @Override
  public Product updateSalesCatalog(String storeId, String productSku,
      List<SalesCatalog> catalogs, Date updatedDate) {
    Query query = new Query(Criteria.where(ProductFieldNames.STORE_ID).is(storeId)
        .and(ProductFieldNames.PRODUCT_SKU).is(productSku));
    Update update = new Update();
    update.set(ProductFieldNames.SALES_CATALOGS, catalogs);
    update.set(ProductFieldNames.PRODUCT_CENTER_UPDATED_DATE, updatedDate);
    update.set(ProductFieldNames.UPDATED_DATE, updatedDate);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).findAndModify(query, update,
        new FindAndModifyOptions().returnNew(true), Product.class);
  }

  @Override
  public Product updatePickupPointCodes(String storeId, String username, String productSku,
      Set<String> pickupPointCodes, boolean isFbbActivated) {
    Query query = new Query(
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU).is(productSku));
    Update update = new Update();
    update.set(ProductFieldNames.PICKUP_POINT_CODES, pickupPointCodes);
    update.set(ProductFieldNames.FBB_ACTIVATED, isFbbActivated);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    update.set(ProductFieldNames.UPDATED_BY, username);
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).findAndModify(query, update, new FindAndModifyOptions().returnNew(true), Product.class);
  }

  @Override
  public List<Product> findByStoreIdAndProductCodes(
      String storeId, Set<String> productCodes, String... includes) {
    Query query = new Query(Criteria.where(ProductFieldNames.STORE_ID).is(storeId)
        .and(ProductFieldNames.MARK_FOR_DELETE).is(FALSE).and(ProductFieldNames.PRODUCT_CODE)
        .in(productCodes));
    Arrays.stream(includes).forEach(include -> query.fields().include(include));
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Product.class);
  }

  @Override
  public List<Product> findByStoreIdAndProductSkus(String storeId, Set<String> productSkus, String[] includes,
      boolean showDeleted) {
    Query query = new Query(
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU).in(productSkus));
    if (!showDeleted) {
      query.addCriteria(Criteria.where(ProductFieldNames.MARK_FOR_DELETE).is(FALSE));
    }
    Arrays.stream(includes).forEach(include -> query.fields().include(include));
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Product.class);
  }
  @Override
  public Product updateFbbFlagAtProduct(String storeId, String productSku,
    boolean fbbActivated) {
    Query query = new Query(
      Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU).is(productSku));
    Update update = new Update();
    update.set(ProductFieldNames.FBB_ACTIVATED, fbbActivated);
    update.set(ProductFieldNames.UPDATED_DATE, new Date());
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).findAndModify(query, update, new FindAndModifyOptions().returnNew(true), Product.class);
  }

  @Override
  public Product findProductByStoreIdAndProductSku(String storeId, String productSku, boolean readFromPrimary) {
    Query query = new Query(
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU).is(productSku));
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).findOne(query, Product.class);
  }

  @Override
  public List<Product> findProductByStoreIdAndProductSkuIn(String storeId, Collection<String> productSku,
      boolean readFromPrimary) {
    Query query = new Query(
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU).in(productSku));
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).find(query, Product.class);
  }

  @Override
  public Product findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku,
      boolean readFromPrimary) {
    Query query = new Query(
        Criteria.where(ProductFieldNames.STORE_ID).is(storeId).and(ProductFieldNames.PRODUCT_SKU).is(productSku)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false));
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).findOne(query, Product.class);
  }

}
