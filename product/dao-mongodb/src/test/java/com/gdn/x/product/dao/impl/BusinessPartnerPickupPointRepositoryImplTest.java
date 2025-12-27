package com.gdn.x.product.dao.impl;

import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.mongodb.core.MongoTemplate;

import java.util.HashSet;
import java.util.List;
import java.util.Map;

public class BusinessPartnerPickupPointRepositoryImplTest {

  private static final String READ_PREFERENCE = "SECONDARY_PREFERRED";

  @InjectMocks
  private BusinessPartnerPickupPointRepositoryImpl pickupPointRepositoryImpl;

  @Mock
  private MongoTemplate mongoTemplate;

  @Mock
  private Map<String, MongoTemplate> mongoTemplateFactory;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String STORE_ID = "storeId";
  private static final String KEYWORD = "keyword";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    Mockito.when(mandatoryParameterHelper.getReadPreference()).thenReturn(READ_PREFERENCE);
    Mockito.when(mongoTemplateFactory.get(READ_PREFERENCE)).thenReturn(mongoTemplate);
  }

  @Test
  public void saveBusinessPartnerPickupPoint() {
    pickupPointRepositoryImpl.findByBusinessPartnerCodeAndMarkForDeleteFalseAndArchivedFalse(BUSINESS_PARTNER_CODE);
  }

  @Test
  public void findBusinessPartnerPickupPointDataTest() {
    List<BusinessPartnerPickupPoint> list = pickupPointRepositoryImpl
      .findBusinessPartnerPickupPointData(STORE_ID, BUSINESS_PARTNER_CODE, KEYWORD, true, true, new HashSet<>());
    Assertions.assertEquals(list.size(), 0);
  }

  @Test
  public void findBusinessPartnerPickupPointNullFbbDataTest() {
    List<BusinessPartnerPickupPoint> list = pickupPointRepositoryImpl
      .findBusinessPartnerPickupPointData(STORE_ID, BUSINESS_PARTNER_CODE, KEYWORD, true, null, new HashSet<>());
    Assertions.assertEquals(list.size(), 0);
  }

  @Test
  public void findBusinessPartnerPickupPointFalseFbbDataTest() {
    List<BusinessPartnerPickupPoint> list = pickupPointRepositoryImpl
      .findBusinessPartnerPickupPointData(STORE_ID, BUSINESS_PARTNER_CODE, KEYWORD, true, false, new HashSet<>());
    Assertions.assertEquals(list.size(), 0);
  }
}
