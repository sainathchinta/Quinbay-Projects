package com.gdn.partners.pbp.service.listener;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.common.SolrInputDocument;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Qualifier;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionAddEvent;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionAddEventFields;
import com.gdn.mta.product.repository.SolrReviewProductCollectionRepository;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;

public class SolrAddReviewProductCollectionKafkaSubscriberBeanTest {

  @Mock
  @Qualifier(value = "reviewProductCollectionClient")
  private CloudSolrClient cloudSolrClient;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private SolrReviewProductCollectionRepository solrReviewProductCollectionRepository;

  @InjectMocks
  private SolrAddReviewProductCollectionKafkaSubscriberBean solrAddReviewProductCollectionKafkaSubscriberBean;

  @Captor
  private ArgumentCaptor<List<SolrInputDocument>> solrInputDocumentArgumentCaptor;

  private static final String STORE_ID = "10001";
  private static final String PRODUCT_ID = "productId";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_NAME = "productName";
  private static final String BRAND = "brand";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String STATE = "state";
  private static final String CREATED_BY = "createdBy";
  private static final String UPDATED_BY = "updatedBy";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final Date CREATED_DATE = new Date();
  private static final Date SUBMITTED_DATE = new Date();
  private static final Date UPDATED_DATE = new Date();
  private static final Date UPDATED_STEP_DATE = new Date();
  private static final int RESUBMIT_COUNT = 1;

  private SolrReviewProductCollectionAddEvent solrProductCollectionAddEvent =
      SolrReviewProductCollectionAddEvent.builder().solrReviewProductCollectionAddEventFieldsList(new ArrayList<>())
          .build();
  private SolrReviewProductCollectionAddEventFields solrReviewProductCollectionAddEventFields;
  private ObjectMapper mapper;

  @BeforeEach
  public void init() {
    initMocks(this);
    solrReviewProductCollectionAddEventFields = SolrReviewProductCollectionAddEventFields.builder().storeId(STORE_ID)
        .productCode(PRODUCT_CODE).productId(PRODUCT_ID).productName(PRODUCT_NAME).brand(BRAND)
        .businessPartnerCode(BUSINESS_PARTNER_CODE).businessPartnerName(BUSINESS_PARTNER_NAME)
        .updatedStepDate(UPDATED_STEP_DATE).updatedDate(UPDATED_DATE).createdDate(CREATED_DATE)
        .createdBy(CREATED_BY).updatedBy(UPDATED_BY).assignedTo(ASSIGNED_TO).activated(false)
        .viewable(false).resubmitCount(RESUBMIT_COUNT).categoryCodes(Collections.singletonList(CATEGORY_CODE))
        .categoryNames(Collections.singletonList(CATEGORY_NAME)).submittedDate(SUBMITTED_DATE).state(STATE).build();
    solrProductCollectionAddEvent.getSolrReviewProductCollectionAddEventFieldsList()
        .add(solrReviewProductCollectionAddEventFields);
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(cloudSolrClient, objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    String message = mapper.writeValueAsString(solrProductCollectionAddEvent);
    Mockito.when(objectMapper.readValue(message, SolrReviewProductCollectionAddEvent.class))
        .thenReturn(solrProductCollectionAddEvent);
    this.solrAddReviewProductCollectionKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
    verify(objectMapper).readValue(message, SolrReviewProductCollectionAddEvent.class);
    assertEquals(STORE_ID,
        solrInputDocumentArgumentCaptor.getValue().get(0).getField(SolrFieldNames.STORE_ID).getValue());
    assertEquals(PRODUCT_CODE,
        solrInputDocumentArgumentCaptor.getValue().get(0).getField(SolrFieldNames.PRODUCT_CODE).getValue());
    assertEquals(PRODUCT_ID,
        solrInputDocumentArgumentCaptor.getValue().get(0).getField(SolrFieldNames.PRODUCT_ID).getValue());
    assertEquals(PRODUCT_NAME,
        solrInputDocumentArgumentCaptor.getValue().get(0).getField(SolrFieldNames.PRODUCT_NAME).getValue());
    assertEquals(ASSIGNED_TO,
        solrInputDocumentArgumentCaptor.getValue().get(0).getField(SolrFieldNames.ASSIGNED_TO).getValue());
    assertEquals(Collections.singletonList(CATEGORY_NAME),
        solrInputDocumentArgumentCaptor.getValue().get(0).getField(SolrFieldNames.CATEGORY_NAMES).getValue());
    assertEquals(Collections.singletonList(CATEGORY_CODE),
        solrInputDocumentArgumentCaptor.getValue().get(0).getField(SolrFieldNames.CATEGORY_CODES).getValue());
    assertEquals(STATE,
        solrInputDocumentArgumentCaptor.getValue().get(0).getField(SolrFieldNames.STATE).getValue());
    assertEquals(BUSINESS_PARTNER_CODE,
        solrInputDocumentArgumentCaptor.getValue().get(0).getField(SolrFieldNames.BUSINESS_PARTNER_CODE).getValue());
    assertEquals(BRAND,
        solrInputDocumentArgumentCaptor.getValue().get(0).getField(SolrFieldNames.BRAND).getValue());
  }

  @Test
  public void onDomainEventConsumedgetSolrInputDocumentListNotEmptyTest() throws Exception {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(STATE,"state");
    String message = mapper.writeValueAsString(solrProductCollectionAddEvent);
    Mockito.when(objectMapper.readValue(message, SolrReviewProductCollectionAddEvent.class))
        .thenReturn(solrProductCollectionAddEvent);
    this.solrAddReviewProductCollectionKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
    verify(objectMapper).readValue(message, SolrReviewProductCollectionAddEvent.class);
  }

  @Test
  public void onDomainEventConsumedgetSolrInputDocumentSwitchONTest() throws Exception {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(STATE,"state");
    Map<String ,Object> fieldValues = new HashMap<>();
    fieldValues.put("key","value");
    solrProductCollectionAddEvent.setFieldsAndValues(fieldValues);
    String message = mapper.writeValueAsString(solrProductCollectionAddEvent);
    Mockito.when(objectMapper.readValue(message, SolrReviewProductCollectionAddEvent.class))
        .thenReturn(solrProductCollectionAddEvent);
    this.solrAddReviewProductCollectionKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, SolrReviewProductCollectionAddEvent.class);
    verify(solrReviewProductCollectionRepository).atomicUpdateToSolr(fieldValues);
  }

  @Test
  public void onDomainEventConsumedgetSolrInputDocumentEmptyMapTest() throws Exception {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(STATE,"state");
    solrProductCollectionAddEvent.setFieldsAndValues(null);
    String message = mapper.writeValueAsString(solrProductCollectionAddEvent);
    Mockito.when(objectMapper.readValue(message, SolrReviewProductCollectionAddEvent.class))
        .thenReturn(solrProductCollectionAddEvent);
    this.solrAddReviewProductCollectionKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, SolrReviewProductCollectionAddEvent.class);
    verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
    assertEquals("state",solrInputDocumentArgumentCaptor.getValue().get(0).getFieldValue(STATE));
  }
}