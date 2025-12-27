package com.gdn.partners.pbp.service.listener;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.List;

import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Qualifier;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionDeleteEvent;

public class SolrDeleteReviewProductCollectionKafkaSubscriberBeanTest {

  @Mock
  @Qualifier(value = "reviewProductCollectionClient")
  private CloudSolrClient cloudSolrClient;

  @Mock
  private ObjectMapper objectMapper;

  @InjectMocks
  private SolrDeleteReviewProductCollectionKafkaSubscriberBean solrDeleteReviewProductCollectionKafkaSubscriberBean;

  private SolrReviewProductCollectionDeleteEvent solrReviewProductCollectionDeleteEvent;
  private static final String ID = "ID";
  List<String> IDS = new ArrayList();
  private ObjectMapper mapper;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    IDS.add(ID);
    solrReviewProductCollectionDeleteEvent = SolrReviewProductCollectionDeleteEvent.builder().ids(IDS).build();
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(cloudSolrClient, objectMapper);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    String message = mapper.writeValueAsString(solrReviewProductCollectionDeleteEvent);
    Mockito.when(objectMapper.readValue(message, SolrReviewProductCollectionDeleteEvent.class))
        .thenReturn(solrReviewProductCollectionDeleteEvent);
    this.solrDeleteReviewProductCollectionKafkaSubscriberBean.onDomainEventConsumed(message);
    verify(cloudSolrClient).deleteById(IDS);
    verify(objectMapper).readValue(message, SolrReviewProductCollectionDeleteEvent.class);
  }
}