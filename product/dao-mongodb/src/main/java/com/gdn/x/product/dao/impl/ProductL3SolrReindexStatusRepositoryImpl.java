package com.gdn.x.product.dao.impl;

import com.gdn.x.product.dao.api.ProductL3SolrReindexStatusRepositoryCustom;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.model.entity.ProductL3SolrReindexStatus;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Repository;

import java.util.Collections;
import java.util.List;
import java.util.Map;

@Slf4j
@Repository
public class ProductL3SolrReindexStatusRepositoryImpl implements
    ProductL3SolrReindexStatusRepositoryCustom {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private Map<String, MongoTemplate> mongoTemplateFactory;

  @Override
  public void updateProductReindexStatusByProductSku(List<String> productSkuList,
      ProductReindexStatus productReindexStatus) {
    Query query = new Query(Criteria.where(ProductFieldNames.PRODUCT_SKU).in(productSkuList));
    Update update = new Update();
    update.set(ProductFieldNames.PRODUCT_REINDEX_STATUS, productReindexStatus);
    try {
      this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
          .updateMulti(query, update, ProductL3SolrReindexStatus.class);
    } catch (Exception e) {
      log.error("Error updating product reindex status :{} of L3 : {}", productReindexStatus,
          productSkuList, e);
    }
  }

  @Override
  public List<ProductL3SolrReindexStatus> findByStoreIdAndProductReindexStatusAndMarkForDeleteFalseLimit(
      String storeId, ProductReindexStatus productReindexStatus, int limit) {
    Query query = new Query(Criteria.where(ProductFieldNames.STORE_ID).is(storeId)
        .and(ProductFieldNames.PRODUCT_REINDEX_STATUS).is(productReindexStatus)
        .and(ProductFieldNames.MARK_FOR_DELETE).is(false));
    query.limit(limit);
    try {
      return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
          .find(query, ProductL3SolrReindexStatus.class);
    } catch (Exception e) {
      log.error("Error to fetch list of products by reindex status : {}, error - ",
          productReindexStatus, e);
      return Collections.EMPTY_LIST;
    }
  }
}
