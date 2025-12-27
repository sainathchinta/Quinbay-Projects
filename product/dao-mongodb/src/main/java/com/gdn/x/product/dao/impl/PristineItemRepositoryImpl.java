package com.gdn.x.product.dao.impl;

import com.gdn.x.product.dao.api.PristineItemRepositoryCustom;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by keshashah on 18/12/17.
 */


@Repository
public class PristineItemRepositoryImpl implements PristineItemRepositoryCustom {

  private static final Logger LOGGER = LoggerFactory.getLogger(PristineItemRepositoryImpl.class);

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private Map<String, MongoTemplate> mongoTemplateFactory;

  @Override
  public List<String> getAllPristineIds() {
    return this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .getCollection(PristineDataItem.DOCUMENT_NAME)
      .distinct(ProductFieldNames.PRISTINE_ID, String.class).into(new ArrayList<>());
  }

  public void updateSalesCategorySequencesAndDPC(Set<PristineDataItem> pristineDataItems){
    pristineDataItems.forEach(pristineDataItem -> {
      Query query = new Query(
          Criteria.where(ProductFieldNames.PRISTINE_ID).is(pristineDataItem.getPristineId()));
      Update update = new Update();
      update.set(ProductFieldNames.SALES_CATEGORY_SEQUENCES,
          pristineDataItem.getSalesCategorySequences());
      update.set(ProductFieldNames.PRISTINE_CATEGORIES_HIERARCHY,
          pristineDataItem.getPristineCategoriesHierarchy());
      update.set(ProductFieldNames.PRISTINE_DEFAULT_PRODUCT_CODE,
          pristineDataItem.getDefaultProductCode());
      try {
        this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
            .updateFirst(query, update, PristineDataItem.class);
      }catch(Exception ex){
        LOGGER.error("Error while updating PristineDataItem with pristineId :{}",
            pristineDataItem.getPristineId());
      }
    });
  }

  @Override
  public void updatePristineMasterDPC(PristineDataItem pristineDataItem) {
    Query query = new Query(Criteria.where(ProductFieldNames.PRISTINE_ID).is(pristineDataItem.getPristineId()));
    Update update = new Update();
    update.set(ProductFieldNames.PRISTINE_DEFAULT_PRODUCT_CODE, pristineDataItem.getDefaultProductCode());
    update.set(ProductFieldNames.PRISTINE_MASTER_ID, pristineDataItem.getPristineMasterId());
    try {
      this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
          .updateFirst(query, update, PristineDataItem.class);
    } catch (Exception ex) {
      LOGGER.error("Error while updating pristineMasterId and DPC PristineDataItem with pristineId :{}",
          pristineDataItem.getPristineId());
    }
  }

  @Override
  public void updatePristineDPC(List<PristineDataItem> pristineDataItems) {
    for (PristineDataItem pristineDataItem : pristineDataItems) {
      Query query = new Query(Criteria.where(ProductFieldNames.PRISTINE_ID).is(pristineDataItem.getPristineId()));
      Update update = new Update();
      update.set(ProductFieldNames.PRISTINE_DEFAULT_PRODUCT_CODE, pristineDataItem.getDefaultProductCode());
      try {
        this.mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference()).updateFirst(query, update, PristineDataItem.class);
      } catch (Exception ex) {
        LOGGER.error("Error while updating DPC PristineDataItem with pristineMasterId :{}",
            pristineDataItem.getPristineMasterId());
      }
    }
  }
}
