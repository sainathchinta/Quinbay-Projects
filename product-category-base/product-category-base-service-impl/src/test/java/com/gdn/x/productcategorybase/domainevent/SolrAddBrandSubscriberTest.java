package com.gdn.x.productcategorybase.domainevent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.SolrAddBrandListDomainEventModel;
import com.gdn.x.productcategorybase.entity.solr.SolrBrandModel;
import com.gdn.x.productcategorybase.repository.SolrBrandRepository;

public class SolrAddBrandSubscriberTest {

  @Mock
  private SolrBrandRepository solrBrandRepository;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private SolrAddBrandSubscriber solrAddBrandSubscriber;

  @Captor
  private ArgumentCaptor<List<SolrBrandModel>> solrBrandModelArgumentCaptor;

  private static final String BRAND_CODE = "BR-00001";
  private static final String ID = "ID";
  private static final String BRAND_VALUE = "brand";
  private static final String MESSAGE = "message";
  private static final String BUSINESS_PARTNER_CODE = "BR-00001";
  private SolrAddBrandListDomainEventModel solrAddBrandListDomainEventModel;
  private SolrBrandModel solrBrandModel;
  private String message;

  @BeforeEach
  public void init() throws Exception {
    initMocks(this);
    solrBrandModel = SolrBrandModel.builder().brandCode(BRAND_CODE).id(ID).businessPartnerCode(BUSINESS_PARTNER_CODE)
        .brandValue(BRAND_VALUE).brandApproved(Boolean.FALSE).updatedDate(new Date()).build();
    solrAddBrandListDomainEventModel =
        SolrAddBrandListDomainEventModel.builder().solrBrandModels(Arrays.asList(solrBrandModel)).build();

    ObjectMapper mapper = new ObjectMapper();
    message = mapper.writeValueAsString(solrAddBrandListDomainEventModel);
    Mockito.when(objectMapper.readValue(message, SolrAddBrandListDomainEventModel.class))
        .thenReturn(solrAddBrandListDomainEventModel);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(solrBrandRepository);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    when(objectMapper.readValue(MESSAGE, SolrAddBrandListDomainEventModel.class))
        .thenReturn(solrAddBrandListDomainEventModel);
    doNothing().when(this.solrBrandRepository).addBrandsToBrandCollectionSolr(Mockito.anyList());
    solrAddBrandSubscriber.onDomainEventConsumed(message);
    verify(solrBrandRepository).addBrandsToBrandCollectionSolr(solrBrandModelArgumentCaptor.capture());
    assertEquals(BRAND_CODE, solrBrandModelArgumentCaptor.getValue().get(0).getBrandCode());
    verify(objectMapper).readValue(message, SolrAddBrandListDomainEventModel.class);
  }

  @Test
  public void onDomainEventConsumed_Exception() throws Exception {
    doThrow(RuntimeException.class).when(objectMapper).readValue(MESSAGE, SolrAddBrandListDomainEventModel.class);
    solrAddBrandSubscriber.onDomainEventConsumed(MESSAGE);
    verify(objectMapper).readValue(MESSAGE, SolrAddBrandListDomainEventModel.class);
  }
}