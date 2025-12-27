package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.dao.api.IprProductSolrCollectionRepository;
import com.gdn.x.mta.distributiontask.domain.event.model.AddProductToIprSolrEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.solr.IPRProductSolr;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class IprProductSolrEventListenerTest {
  private static final String JSON = "{}";
  private static final String PRODUCT_SKU = "ProductSku";

  @InjectMocks
  private IprProductSolrEventListener iprProductSolrEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private IprProductSolrCollectionRepository iprProductSolrCollectionRepository;

  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  private AddProductToIprSolrEventModel addProductToIprSolrEventModel;
  private IPRProductSolr iprProductSolr;

  @BeforeEach
  public void setUp() {
    iprProductSolr = new IPRProductSolr();
    addProductToIprSolrEventModel = new AddProductToIprSolrEventModel();
    addProductToIprSolrEventModel.setIprProductSolr(iprProductSolr);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, iprProductSolrCollectionRepository,
        kafkaTopicPropertiesConsumer);
  }

  @Test
  void iprProductSolrEventListenerTest() throws Exception {
    Mockito.when(objectMapper.readValue(JSON, AddProductToIprSolrEventModel.class))
        .thenReturn(addProductToIprSolrEventModel);
    iprProductSolrEventListener.onDomainEventConsumed(JSON);
    Mockito.verify(objectMapper).readValue(JSON, AddProductToIprSolrEventModel.class);
    Mockito.verify(iprProductSolrCollectionRepository).addDocumentToIPRSolr(iprProductSolr);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddIprProductSolrEvent();
  }

  @Test
  void iprProductSolrEventListenerDeleteEventTest() throws Exception {
    addProductToIprSolrEventModel.setIprProductSolr(null);
    addProductToIprSolrEventModel.setDeleteSolrDocument(true);
    addProductToIprSolrEventModel.setProductSku(PRODUCT_SKU);
    Mockito.when(objectMapper.readValue(JSON, AddProductToIprSolrEventModel.class))
        .thenReturn(addProductToIprSolrEventModel);
    iprProductSolrEventListener.onDomainEventConsumed(JSON);
    Mockito.verify(objectMapper).readValue(JSON, AddProductToIprSolrEventModel.class);
    Mockito.verify(iprProductSolrCollectionRepository).deleteIprSolrDocument(PRODUCT_SKU);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddIprProductSolrEvent();
  }

  @Test
  void iprProductSolrEventListenerEmptyTest() throws Exception {
    addProductToIprSolrEventModel.setIprProductSolr(null);
    Mockito.when(objectMapper.readValue(JSON, AddProductToIprSolrEventModel.class))
        .thenReturn(addProductToIprSolrEventModel);
    iprProductSolrEventListener.onDomainEventConsumed(JSON);
    Mockito.verify(objectMapper).readValue(JSON, AddProductToIprSolrEventModel.class);
    Mockito.verify(iprProductSolrCollectionRepository, Mockito.never())
        .addDocumentToIPRSolr(iprProductSolr);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddIprProductSolrEvent();
  }

  @Test
  void iprProductSolrEventListenerExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(JSON, AddProductToIprSolrEventModel.class))
        .thenThrow(new ApplicationRuntimeException());
    iprProductSolrEventListener.onDomainEventConsumed(JSON);
    Mockito.verify(objectMapper).readValue(JSON, AddProductToIprSolrEventModel.class);
    Mockito.verify(kafkaTopicPropertiesConsumer, Mockito.times(2)).getAddIprProductSolrEvent();
  }
}