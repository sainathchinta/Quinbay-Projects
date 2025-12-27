package com.gdn.x.mta.distributiontask.service.impl.publisher;

import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductCombinedUpdateToSolrEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductUpdateProductToSolrEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrAddDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrBatchAddDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrDeleteDomainEventModel;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaTopicProperties;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@ExtendWith(MockitoExtension.class)
public class SolrReindexPublisherServiceImplTest {

  private static final String STORE_ID = "10001";
  private static final String PRODUCT_CODE = "productCode";

  private PDTProductSolrBatchAddDomainEventModel productSolrBatchAddDomainEventModel;
  private PDTProductSolrAddDomainEventModel productSolrAddDomainEventModel;
  private PDTProductSolrDeleteDomainEventModel pdtProductSolrDeleteDomainEventModel;
  private PDTProductUpdateProductToSolrEventModel productAddProductToSolrEventModel;

  @InjectMocks
  private SolrReindexPublisherServiceImpl solrReindexPublisherServiceImpl;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void init() {
    productSolrAddDomainEventModel =
        PDTProductSolrAddDomainEventModel.builder().productCode(PRODUCT_CODE).build();
    productSolrBatchAddDomainEventModel = PDTProductSolrBatchAddDomainEventModel.builder()
        .pdtProductSolrAddDomainEventModelList(Arrays.asList(productSolrAddDomainEventModel))
        .build();
    pdtProductSolrDeleteDomainEventModel =
        PDTProductSolrDeleteDomainEventModel.builder().productCodes(List.of(PRODUCT_CODE)).build();
    productAddProductToSolrEventModel = new PDTProductUpdateProductToSolrEventModel();
    productAddProductToSolrEventModel.setProductCode(PRODUCT_CODE);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(kafkaProducer, kafkaTopicProperties);
  }

  @Test
   void publishPDTProductSolrBatchAddDomainEventModelForReindexTest() {
    Mockito.when(kafkaTopicProperties.getPdtProductCombinedUpdateToSolrEvent())
        .thenReturn(DomainEventName.PDT_PRODUCT_COMBINED_UPDATE_TO_SOLR_EVENT_NAME);
    PDTProductSolrBatchAddDomainEventModel response =
        solrReindexPublisherServiceImpl.publishPDTProductSolrBatchAddDomainEventModelForReindex(
            productSolrBatchAddDomainEventModel);
    Assertions.assertEquals(PRODUCT_CODE,
        response.getPdtProductSolrAddDomainEventModelList().get(0).getProductCode());
    Mockito.verify(this.kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getPdtProductCombinedUpdateToSolrEvent()), Mockito.eq(PRODUCT_CODE),
            Mockito.any(PDTProductCombinedUpdateToSolrEventModel.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(3)).getPdtProductCombinedUpdateToSolrEvent();
  }

  @Test
   void publishPDTProductSolrBatchAddDomainEventModelForReindexEmptyListTest() {
    productSolrBatchAddDomainEventModel.setPdtProductSolrAddDomainEventModelList(new ArrayList<>());
    PDTProductSolrBatchAddDomainEventModel response =
        solrReindexPublisherServiceImpl.publishPDTProductSolrBatchAddDomainEventModelForReindex(
            productSolrBatchAddDomainEventModel);
  }

  @Test
   void publishPDTProductApprovalToSolrTest() {
    Mockito.when(kafkaTopicProperties.getPdtProductCombinedUpdateToSolrEvent())
        .thenReturn(DomainEventName.PDT_PRODUCT_COMBINED_UPDATE_TO_SOLR_EVENT_NAME);
    PDTProductUpdateProductToSolrEventModel response =
        solrReindexPublisherServiceImpl.publishPDTProductApprovalToSolr(productAddProductToSolrEventModel);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCode());
    Mockito.verify(this.kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getPdtProductCombinedUpdateToSolrEvent()), Mockito.eq(PRODUCT_CODE),
            Mockito.any(PDTProductCombinedUpdateToSolrEventModel.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(3)).getPdtProductCombinedUpdateToSolrEvent();
  }

  @Test
   void publishPDTProductSolrBatchDeleteDomainEventModelForReindexTest() {
    Mockito.when(kafkaTopicProperties.getPdtProductCombinedUpdateToSolrEvent())
        .thenReturn(DomainEventName.PDT_PRODUCT_COMBINED_UPDATE_TO_SOLR_EVENT_NAME);
    PDTProductSolrDeleteDomainEventModel response =
        solrReindexPublisherServiceImpl.publishPDTProductSolrBatchDeleteDomainEventModelForReindex(
            pdtProductSolrDeleteDomainEventModel);
    Assertions.assertEquals(PRODUCT_CODE, response.getProductCodes().get(0));
    Mockito.verify(this.kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getPdtProductCombinedUpdateToSolrEvent()), Mockito.eq(PRODUCT_CODE),
            Mockito.any(PDTProductCombinedUpdateToSolrEventModel.class));
    Mockito.verify(kafkaTopicProperties, Mockito.times(3)).getPdtProductCombinedUpdateToSolrEvent();
  }

  @Test
   void publishPDTProductSolrBatchDeleteDomainEventModelForReindexEmptyTest() {
    pdtProductSolrDeleteDomainEventModel.setProductCodes(new ArrayList<>());
    PDTProductSolrDeleteDomainEventModel response =
        solrReindexPublisherServiceImpl.publishPDTProductSolrBatchDeleteDomainEventModelForReindex(
            pdtProductSolrDeleteDomainEventModel);
  }
}
