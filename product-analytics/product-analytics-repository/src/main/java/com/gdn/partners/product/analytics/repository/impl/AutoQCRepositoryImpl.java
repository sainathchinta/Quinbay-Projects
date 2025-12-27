package com.gdn.partners.product.analytics.repository.impl;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.bson.Document;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.BulkOperations;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.CriteriaDefinition;
import org.springframework.data.mongodb.core.query.Query;

import com.gdn.partners.product.analytics.entity.AutoQCDetail;
import com.gdn.partners.product.analytics.model.FieldNames;
import com.gdn.partners.product.analytics.repository.AutoQCRepositoryCustom;
import com.gdn.partners.product.analytics.repository.helper.DataHelper;
import com.gdn.partners.product.analytics.web.model.ChangeFieldResponse;
import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;
import com.mongodb.bulk.BulkWriteResult;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class AutoQCRepositoryImpl implements AutoQCRepositoryCustom {

  private static final String SET = "$set";

  @Autowired
  private MongoTemplate mongoTemplate;

  @Override
  public BulkWriteResult bulkWriteAutoQcDetail(List<AutoQCDetail> autoQCDetailList,
      List<SellerFieldsChangeResponse> sellerFieldsChangeResponseList, List<String> fieldNameList) {
    BulkOperations bulkWriteOperation =
        mongoTemplate.bulkOps(BulkOperations.BulkMode.UNORDERED, AutoQCDetail.COLLECTION_NAME);
    for (AutoQCDetail autoQCDetail : autoQCDetailList) {
      Document dbObject = new Document();
      mongoTemplate.getConverter().write(autoQCDetail, dbObject);
      checkIfFieldValuesHaveChanged(sellerFieldsChangeResponseList, fieldNameList, autoQCDetail);
      CriteriaDefinition criteria =
          Criteria.where(FieldNames.BUSINESS_PARTNER_CODE).is(autoQCDetail.getBusinessPartnerCode())
              .and(FieldNames.C1_CODE).is(autoQCDetail.getC1Code());
      bulkWriteOperation.upsert(Query.query(criteria),
          DataHelper.getUpdateFromDocument(dbObject, FieldNames.PRIMARY_KEY_ID));
    }
    return bulkWriteOperation.execute();
  }

  private void checkIfFieldValuesHaveChanged(List<SellerFieldsChangeResponse> sellerFieldsChangeResponseList,
      List<String> fieldNameList, AutoQCDetail autoQCDetail) {
    try {
      Query query = new Query(Criteria.where(FieldNames.BUSINESS_PARTNER_CODE).is(autoQCDetail.getBusinessPartnerCode())
          .and(FieldNames.C1_CODE).is(autoQCDetail.getC1Code()));
      AutoQCDetail existingAutoQCDetail = mongoTemplate.findOne(query, AutoQCDetail.class);
      SellerFieldsChangeResponse sellerFieldsChangeResponse = new SellerFieldsChangeResponse();
      boolean dataChanged = false;
      if (Objects.isNull(existingAutoQCDetail)) {
        dataChanged = true;
        sellerFieldsChangeResponse.setNewData(true);
      } else {
        Map<String, ChangeFieldResponse> changeFieldResponseMap = new HashMap<>();
        dataChanged =
            isDataChanged(fieldNameList, autoQCDetail, existingAutoQCDetail, dataChanged, changeFieldResponseMap);
        sellerFieldsChangeResponse.setChangeFieldResponseMap(changeFieldResponseMap);
      }
      setSellerFieldChangeResponse(sellerFieldsChangeResponseList, autoQCDetail, sellerFieldsChangeResponse,
          dataChanged);
    } catch (Exception e) {
      log.error("Exception when checking if values are changed for seller {} and categoryCode {} ",
          autoQCDetail.getBusinessPartnerCode(), autoQCDetail.getC1Code(), e);
    }
  }

  private void setSellerFieldChangeResponse(List<SellerFieldsChangeResponse> sellerFieldsChangeResponseList,
      AutoQCDetail autoQCDetail, SellerFieldsChangeResponse sellerFieldsChangeResponse, boolean dataChanged) {
    if (dataChanged) {
      sellerFieldsChangeResponse.setSellerCode(autoQCDetail.getBusinessPartnerCode());
      sellerFieldsChangeResponse.setCategoryCode(autoQCDetail.getC1Code());
      log.info("Found change for following data {} ", sellerFieldsChangeResponse);
      sellerFieldsChangeResponseList.add(sellerFieldsChangeResponse);
    }
  }

  private boolean isDataChanged(List<String> fieldNameList, AutoQCDetail autoQCDetail,
      AutoQCDetail existingAutoQCDetail, boolean changeInData, Map<String, ChangeFieldResponse> changeFieldResponseMap)
      throws NoSuchFieldException, IllegalAccessException {
    for (String fieldName : fieldNameList) {
      Field oldField = existingAutoQCDetail.getClass().getDeclaredField(fieldName);
      oldField.setAccessible(true);

      Field newField = autoQCDetail.getClass().getDeclaredField(fieldName);
      newField.setAccessible(true);

      if (!String.valueOf(newField.get(autoQCDetail)).equals(String.valueOf(oldField.get(existingAutoQCDetail)))) {
        changeInData = true;
        changeFieldResponseMap.put(fieldName,
            new ChangeFieldResponse(String.valueOf(oldField.get(existingAutoQCDetail)),
                String.valueOf(newField.get(autoQCDetail))));
      }
    }
    return changeInData;
  }
}
