package com.gdn.x.product.dao.impl;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Repository;

import com.gdn.x.product.dao.api.BusinessPartnerPickupPointCustomRepository;
import com.gdn.x.product.enums.BusinessPartnerPickupPointFieldNames;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;

@Repository
public class BusinessPartnerPickupPointRepositoryImpl implements BusinessPartnerPickupPointCustomRepository {

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private Map<String, MongoTemplate> mongoTemplateFactory;

  @Override
  public List<BusinessPartnerPickupPoint> findBusinessPartnerPickupPointData(String storeId, String businessPartnerCode,
      String keyword, Boolean cncActivated,Boolean fbbActivated, Set<String> pickupPointSet) {
    Criteria criteria =
      Criteria.where(ProductFieldNames.MARK_FOR_DELETE).is(false).and(BusinessPartnerPickupPointFieldNames.ARCHIVED)
        .is(false).and(BusinessPartnerPickupPointFieldNames.BUSINESS_PARTNER_CODE)
        .is(businessPartnerCode);

    if (Objects.nonNull(cncActivated)) {
      criteria = criteria.and(ProductFieldNames.CNC_ACTIVATED).is(cncActivated);
    }
    if (Objects.nonNull(fbbActivated) && fbbActivated) {
      criteria = criteria.and(ProductFieldNames.FBB_ACTIVATED).is(true);
    } else if (Objects.nonNull(fbbActivated)) {
      criteria = criteria.and(ProductFieldNames.FBB_ACTIVATED).ne(true);
    }
    if (StringUtils.isNotBlank(keyword)) {
      criteria = criteria.orOperator(Criteria.where(BusinessPartnerPickupPointFieldNames.CODE).is(keyword),
          Criteria.where(BusinessPartnerPickupPointFieldNames.NAME).is(keyword));
    }
    if (CollectionUtils.isNotEmpty(pickupPointSet)) {
      criteria = criteria.and(BusinessPartnerPickupPointFieldNames.CODE).in(pickupPointSet);
    }
    Query query = new Query(criteria);
    return mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, BusinessPartnerPickupPoint.class);
  }

  @Override
  public List<BusinessPartnerPickupPoint> findByBusinessPartnerCodeAndMarkForDeleteFalseAndArchivedFalse(
      String businessPartnerCode) {
    Query query = new Query(
        Criteria.where(BusinessPartnerPickupPointFieldNames.BUSINESS_PARTNER_CODE).is(businessPartnerCode)
            .and(ProductFieldNames.MARK_FOR_DELETE).is(false).and(BusinessPartnerPickupPointFieldNames.ARCHIVED)
            .is(false));
    query.fields().include(BusinessPartnerPickupPointFieldNames.CODE)
        .include(BusinessPartnerPickupPointFieldNames.CNC_ACTIVATED)
        .include(BusinessPartnerPickupPointFieldNames.FBB_ACTIVATED);
    return mongoTemplateFactory.get(mandatoryParameterHelper.getReadPreference())
        .find(query, BusinessPartnerPickupPoint.class);
  }
}
