package com.gdn.x.productcategorybase.domainevent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Arrays;
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
import com.gdn.x.productcategorybase.domain.event.model.SolrUpdateBrandDomainEventModel;
import com.gdn.x.productcategorybase.entity.solr.SolrBrandModel;
import com.gdn.x.productcategorybase.entity.solr.SolrUpdateBrandModel;
import com.gdn.x.productcategorybase.repository.SolrBrandRepository;

public class SolrUpdateBrandSubscriberTest {

  @Mock
  private SolrBrandRepository solrBrandRepository;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private SolrUpdateBrandSubscriber solrUpdateBrandSubscriber;

  @Captor
  private ArgumentCaptor<List<SolrBrandModel>> solrBrandModelArgumentCaptor;

  private static final String BRAND_CODE = "BR-00001";
  private static final String ID = "ID";
  private static final String BUSINESS_PARTNER_CODE = "BR-00001";
  private SolrUpdateBrandDomainEventModel solrUpdateBrandDomainEventModel;
  private SolrUpdateBrandModel solrUpdateBrandModel;
  private static final String MESSAGE = "message";

  private String message;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    solrUpdateBrandModel =
        SolrUpdateBrandModel.builder().brandCode(BRAND_CODE).id(ID).businessPartnerCode(BUSINESS_PARTNER_CODE)
            .brandApproved(Boolean.TRUE).build();
    solrUpdateBrandDomainEventModel =
        SolrUpdateBrandDomainEventModel.builder().updateBrandModels(Arrays.asList(solrUpdateBrandModel)).build();
    ObjectMapper mapper = new ObjectMapper();
    message = mapper.writeValueAsString(solrUpdateBrandDomainEventModel);
    Mockito.when(objectMapper.readValue(message, SolrUpdateBrandDomainEventModel.class))
        .thenReturn(solrUpdateBrandDomainEventModel);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(solrBrandRepository);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    when(objectMapper.readValue(MESSAGE, SolrUpdateBrandDomainEventModel.class))
        .thenReturn(solrUpdateBrandDomainEventModel);
    doNothing().when(this.solrBrandRepository)
        .updateBrandApprovalStatusListInSolr(Mockito.anyList());
    solrUpdateBrandSubscriber.onDomainEventConsumed(MESSAGE);
    verify(solrBrandRepository).updateBrandApprovalStatusListInSolr(solrBrandModelArgumentCaptor.capture());
    assertEquals(BRAND_CODE, solrBrandModelArgumentCaptor.getValue().get(0).getBrandCode());
    verify(objectMapper).readValue(MESSAGE, SolrUpdateBrandDomainEventModel.class);

  }

  @Test
  public void onDomainEventConsumed_Exception() throws Exception {
    doThrow(RuntimeException.class).when(objectMapper).readValue(MESSAGE, SolrUpdateBrandDomainEventModel.class);
    solrUpdateBrandSubscriber.onDomainEventConsumed(MESSAGE);
    verify(objectMapper).readValue(MESSAGE, SolrUpdateBrandDomainEventModel.class);
  }
}