package com.gdn.x.productcategorybase.domainevent;


import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeleteBatchPcbProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.SolrDeletePcbProductDomainEventModel;
import com.gdn.x.productcategorybase.repository.SolrPcbRepository;

public class SolrDeletePcbProductSubscriberTest {

  @Mock
  private SolrPcbRepository solrPcbRepository;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private SolrDeletePcbProductSubscriber solrDeletePcbProductSubscriber;

  private static final String ID = "ID";
  private static final String MESSAGE = "message";

  private SolrDeleteBatchPcbProductDomainEventModel solrDeleteBatchPcbProductDomainEventModel;

  private String message;

  @Captor
  private ArgumentCaptor<List<String>> idsCaptor;

  @BeforeEach
  public void init() throws Exception {
    initMocks(this);
    SolrDeletePcbProductDomainEventModel solrDeletePcbProductDomainEventModel =
        SolrDeletePcbProductDomainEventModel.builder().id(ID).build();
    solrDeleteBatchPcbProductDomainEventModel = SolrDeleteBatchPcbProductDomainEventModel.builder()
        .solrDeletePcbProductDomainEventModels(Arrays.asList(solrDeletePcbProductDomainEventModel))
        .build();
    ObjectMapper mapper = new ObjectMapper();
    message = mapper.writeValueAsString(solrDeleteBatchPcbProductDomainEventModel);
    Mockito.when(objectMapper.readValue(message, SolrDeleteBatchPcbProductDomainEventModel.class))
        .thenReturn(solrDeleteBatchPcbProductDomainEventModel);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(solrPcbRepository);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    when(objectMapper.readValue(MESSAGE, SolrDeleteBatchPcbProductDomainEventModel.class))
        .thenReturn(solrDeleteBatchPcbProductDomainEventModel);
    solrDeletePcbProductSubscriber.onDomainEventConsumed(MESSAGE);
    verify(this.solrPcbRepository).deleteProductListFromPcbCollection(idsCaptor.capture());
    Assertions.assertTrue(idsCaptor.getValue().contains(ID));
    verify(objectMapper).readValue(MESSAGE, SolrDeleteBatchPcbProductDomainEventModel.class);
  }

  @Test
  public void onDomainEventConsumed_Exception() throws Exception {
    doThrow(RuntimeException.class).when(objectMapper).readValue(MESSAGE, SolrDeleteBatchPcbProductDomainEventModel.class);
    solrDeletePcbProductSubscriber.onDomainEventConsumed(MESSAGE);
    verify(objectMapper).readValue(MESSAGE, SolrDeleteBatchPcbProductDomainEventModel.class);
  }
}