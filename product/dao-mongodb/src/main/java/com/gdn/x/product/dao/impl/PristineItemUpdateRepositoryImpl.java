package com.gdn.x.product.dao.impl;

import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;

import com.gdn.x.product.dao.api.PristineItemUpdateRepositoryCustom;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.entity.PristineDataItemUpdate;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;

import org.springframework.stereotype.Repository;

@Repository
public class PristineItemUpdateRepositoryImpl implements PristineItemUpdateRepositoryCustom {

  private static final Logger LOGGER = LoggerFactory.getLogger(PristineItemRepositoryImpl.class);

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private Map<String, MongoTemplate> mongoTemplateFactory;

  @Override
  public void updateIsUpdatedFlag(List<PristineDataItemUpdate> pristineDataItemUpdates) {
    LOGGER.info("updateIsUpdatedFlag");
    for (PristineDataItemUpdate pristineDataItemUpdate : pristineDataItemUpdates) {
      Query query = new Query(Criteria.where(ProductFieldNames.PRISTINE_ID).is(pristineDataItemUpdate.getPristineId()));
      Update update = new Update();
      update.set(ProductFieldNames.IS_UPDATED, Boolean.TRUE);
      try {
        this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).updateFirst(query, update, PristineDataItemUpdate.class);
      } catch (Exception ex) {
        LOGGER.error("Error while updating pristineMasterId and DPC PristineDataItem with pristineId :{}",
            pristineDataItemUpdate.getPristineId());
      }
    }
  }
}
