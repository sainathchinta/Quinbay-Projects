package com.gdn.partners.product.analytics.repository.impl;

import com.gdn.partners.product.analytics.entity.SellerAnalytics;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.model.FieldNames;
import com.gdn.partners.product.analytics.model.SellerAnalyticsFields;
import com.gdn.partners.product.analytics.repository.SellerAnalyticsRepositoryCustom;
import com.mongodb.bulk.BulkWriteResult;
import org.bson.Document;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.BulkOperations;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.CriteriaDefinition;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;

import java.util.Date;
import java.util.List;

public class SellerAnalyticsRepositoryImpl implements SellerAnalyticsRepositoryCustom {

  @Autowired
  private MongoTemplate mongoTemplate;

  private static final String SET = "$set";

  @Override
  public BulkWriteResult bulkWriteSellerAnalyticsDetail(List<SellerAnalytics> sellerAnalyticsList) {
    BulkOperations bulkWriteOperation =
        mongoTemplate.bulkOps(BulkOperations.BulkMode.UNORDERED, SellerAnalytics.COLLECTION_NAME);
    for (SellerAnalytics sellerAnalytics : sellerAnalyticsList) {
      Document dbObject = new Document();
      setMandatoryParams(sellerAnalytics);
      mongoTemplate.getConverter().write(sellerAnalytics, dbObject);
      CriteriaDefinition criteria =
          Criteria.where(SellerAnalyticsFields.SELLER_CODE).is(sellerAnalytics.getSellerCode());
      Document updateDoc = new Document(SET, dbObject);
      bulkWriteOperation.upsert(Query.query(criteria),
          Update.fromDocument(updateDoc, FieldNames.PRIMARY_KEY_ID));
    }
    return bulkWriteOperation.execute();
  }

  private void setMandatoryParams(SellerAnalytics sellerAnalytics) {
    sellerAnalytics.setStoreId(Constants.STORE_ID_VALUE);
    sellerAnalytics.setMarkForDelete(false);
    sellerAnalytics.setCreatedDate(new Date());
    sellerAnalytics.setUpdatedDate(new Date());
    sellerAnalytics.setCreatedBy(Constants.USERNAME);
    sellerAnalytics.setUpdatedBy(Constants.USERNAME);
    sellerAnalytics.setVersion(0L);
  }
}
