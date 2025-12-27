package com.gdn.partners.product.analytics.repository.impl;

import com.gdn.partners.product.analytics.entity.ProductAttributeExtractions;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.model.FieldNames;
import com.gdn.partners.product.analytics.model.ProductAttributeExtractionsFields;
import com.gdn.partners.product.analytics.model.enums.ProductAttributeExtractionsStatus;
import com.gdn.partners.product.analytics.repository.ProductAttributeExtractionsRepositoryCustom;
import com.gdn.partners.product.analytics.repository.helper.DataHelper;
import com.mongodb.bulk.BulkWriteResult;
import lombok.RequiredArgsConstructor;
import org.bson.Document;
import org.springframework.data.mongodb.core.BulkOperations;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.CriteriaDefinition;
import org.springframework.data.mongodb.core.query.Query;

import java.util.Date;
import java.util.List;

import static org.springframework.data.mongodb.core.query.Criteria.where;

@RequiredArgsConstructor
public class ProductAttributeExtractionsRepositoryCustomImpl
    implements ProductAttributeExtractionsRepositoryCustom {

  private final MongoTemplate mongoTemplate;

  @Override
  public BulkWriteResult bulkWriteProductAttributeExtractions(
      List<ProductAttributeExtractions> productAttributeExtractionsList) {
    BulkOperations bulkWriteOperation = mongoTemplate.bulkOps(BulkOperations.BulkMode.UNORDERED,
        ProductAttributeExtractions.COLLECTION_NAME);
    for (ProductAttributeExtractions productAttributeExtractions :
        productAttributeExtractionsList) {
      Document dbObject = new Document();
      setMandatoryParams(productAttributeExtractions);
      mongoTemplate.getConverter().write(productAttributeExtractions, dbObject);
      CriteriaDefinition criteria = Criteria.where(ProductAttributeExtractionsFields.PRODUCT_SKU)
          .is(productAttributeExtractions.getProductSku());
      bulkWriteOperation.upsert(Query.query(criteria),
          DataHelper.getUpdateFromDocument(dbObject, FieldNames.PRIMARY_KEY_ID));
    }
    return bulkWriteOperation.execute();
  }

  private void setMandatoryParams(ProductAttributeExtractions productAttributeExtractions) {
    productAttributeExtractions.setStoreId(Constants.STORE_ID_VALUE);
    productAttributeExtractions.setMarkForDelete(false);
    productAttributeExtractions.setCreatedDate(new Date());
    productAttributeExtractions.setUpdatedDate(new Date());
    productAttributeExtractions.setCreatedBy(Constants.USERNAME);
    productAttributeExtractions.setUpdatedBy(Constants.USERNAME);
    productAttributeExtractions.setVersion(Constants.ZERO);
  }

  @Override
  public List<ProductAttributeExtractions> fetchProductAttributeExtractions(String storeId,
      int batchSize) {
    Query query = new Query(where(ProductAttributeExtractionsFields.STATUS).is(
        ProductAttributeExtractionsStatus.NEW.name()).and(Constants.STORE_ID).is(storeId));
    query.limit(batchSize);
    return mongoTemplate.find(query, ProductAttributeExtractions.class);
  }
}