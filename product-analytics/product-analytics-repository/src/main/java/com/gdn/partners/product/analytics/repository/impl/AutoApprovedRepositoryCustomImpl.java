package com.gdn.partners.product.analytics.repository.impl;

import com.gdn.partners.product.analytics.entity.AutoApprovedProducts;
import com.gdn.partners.product.analytics.model.AutoApprovedFields;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.model.FieldNames;
import com.gdn.partners.product.analytics.repository.AutoApprovedRepositoryCustom;
import com.gdn.partners.product.analytics.repository.helper.DataHelper;
import com.gdn.partners.product.analytics.web.model.request.AutoApprovedWebRequest;
import com.mongodb.bulk.BulkWriteResult;
import org.apache.commons.lang3.StringUtils;
import org.bson.Document;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.BulkOperations;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.CriteriaDefinition;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.support.PageableExecutionUtils;

import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.regex.Pattern;

public class AutoApprovedRepositoryCustomImpl implements AutoApprovedRepositoryCustom {

  private static final String CASE_INSENSITIVE_PATTERN = "i";

  @Autowired
  private MongoTemplate mongoTemplate;

  @Override
  public Page<AutoApprovedProducts> fetchListOfAutoApprovedProducts(AutoApprovedWebRequest request,
    Pageable pageable, boolean isProductCode) {

    Query query = new Query();

    //adding mandatory sorting and mfd check
    query.addCriteria(Criteria.where(Constants.MARK_FOR_DELETE).is(false));
    if (Sort.Direction.DESC.name().equals(request.getSortOrder())) {
      query.with(Sort.by(Sort.Direction.DESC, AutoApprovedFields.ADDED_DATE));
    } else {
      query.with(Sort.by(Sort.Direction.ASC, AutoApprovedFields.ADDED_DATE));
    }

    if (StringUtils.isNotBlank(request.getKeyword()) && isProductCode) {
      query.addCriteria(Criteria.where(AutoApprovedFields.PRODUCT_CODE).is(request.getKeyword()));
    } else if (StringUtils.isNotBlank(request.getKeyword())) {
      String escapedKeyword = Pattern.quote(request.getKeyword());
      query.addCriteria(Criteria.where(AutoApprovedFields.PRODUCT_NAME)
        .regex(escapedKeyword, CASE_INSENSITIVE_PATTERN));
    }
    if (StringUtils.isNotBlank(request.getCategoryCode())) {
      query.addCriteria(
        Criteria.where(AutoApprovedFields.CATEGORY_CODE).is(request.getCategoryCode()));
    }
    if (StringUtils.isNotBlank(request.getAssignedTo())) {
      if (Constants.UNASSIGNED_FILTER.equals(request.getAssignedTo())) {
        query.addCriteria(
            new Criteria().orOperator(Criteria.where(AutoApprovedFields.ASSIGNED_TO).is(StringUtils.EMPTY),
                Criteria.where(AutoApprovedFields.ASSIGNED_TO).exists(false)));
      } else {
        query.addCriteria(Criteria.where(AutoApprovedFields.ASSIGNED_TO).is(request.getAssignedTo()));
      }
    }
    if (StringUtils.isNotBlank(request.getSellerCode())) {
      query.addCriteria(Criteria.where(AutoApprovedFields.SELLER_CODE).is(request.getSellerCode()));
    }
    if (Objects.nonNull(request.getB2bActivated())) {
      query.addCriteria(
        Criteria.where(AutoApprovedFields.B2B_ACTIVATED).is(request.getB2bActivated()));
    }

    //creating pageable response
    long count = mongoTemplate.count(query, AutoApprovedProducts.class);
    query.with(pageable);
    return PageableExecutionUtils.getPage(mongoTemplate.find(query, AutoApprovedProducts.class),
      pageable, () -> count);
  }

  @Override
  public BulkWriteResult bulkWriteAutoApprovedProducts(
    List<AutoApprovedProducts> autoApprovedProductsList) {
    BulkOperations bulkWriteOperation = mongoTemplate.bulkOps(BulkOperations.BulkMode.UNORDERED,
      AutoApprovedProducts.COLLECTION_NAME);
    for (AutoApprovedProducts approvedProduct : autoApprovedProductsList) {
      Document dbObject = new Document();
      setMandatoryParams(approvedProduct);
      mongoTemplate.getConverter().write(approvedProduct, dbObject);
      CriteriaDefinition criteria =
        Criteria.where(AutoApprovedFields.PRODUCT_CODE).is(approvedProduct.getProductCode());
      bulkWriteOperation.upsert(Query.query(criteria),
        DataHelper.getUpdateFromDocument(dbObject, FieldNames.PRIMARY_KEY_ID));
    }
    return bulkWriteOperation.execute();
  }

  @Override
  public long countNumberOfRecords() {
    Query query = new Query();
    query.addCriteria(Criteria.where(Constants.MARK_FOR_DELETE).is(false));
    return mongoTemplate.count(query, AutoApprovedProducts.class);
  }

  private void setMandatoryParams(AutoApprovedProducts autoApprovedProduct) {
    autoApprovedProduct.setStoreId(Constants.STORE_ID_VALUE);
    autoApprovedProduct.setCreatedDate(new Date());
    autoApprovedProduct.setUpdatedDate(new Date());
    autoApprovedProduct.setCreatedBy(Constants.USERNAME);
    autoApprovedProduct.setUpdatedBy(Constants.USERNAME);
    autoApprovedProduct.setVersion(0L);
  }
}
