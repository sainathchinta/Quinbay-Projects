package com.gdn.partners.product.analytics.repository.impl;

import java.util.List;

import org.bson.Document;
import org.bson.conversions.Bson;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.BulkOperations;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.CriteriaDefinition;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;

import com.gdn.partners.product.analytics.entity.AutoQCDetail;
import com.gdn.partners.product.analytics.entity.SellerQCDetail;
import com.gdn.partners.product.analytics.model.FieldNames;
import com.gdn.partners.product.analytics.repository.SellerQcDetailRepositoryCustom;
import com.mongodb.bulk.BulkWriteResult;

public class SellerQCDetailRepositoryImpl implements SellerQcDetailRepositoryCustom {

  private static final String SET = "$set";

  @Autowired
  private MongoTemplate mongoTemplate;

  @Override
  public BulkWriteResult bulkWriteSellerQcDetail(List<SellerQCDetail> sellerQCDetailList) {
    BulkOperations bulkWriteOperation =
        mongoTemplate.bulkOps(BulkOperations.BulkMode.UNORDERED, AutoQCDetail.COLLECTION_NAME);
    for (SellerQCDetail sellerQCDetail : sellerQCDetailList) {
      Document dbObject = new Document();
      mongoTemplate.getConverter().write(sellerQCDetail, dbObject);
      CriteriaDefinition criteria =
          Criteria.where(FieldNames.BUSINESS_PARTNER_CODE).is(sellerQCDetail.getBusinessPartnerCode());
      bulkWriteOperation.upsert(Query.query(criteria),
          Update.fromDocument(dbObject, FieldNames.PRIMARY_KEY_ID));
    }
    return bulkWriteOperation.execute();
  }
}
