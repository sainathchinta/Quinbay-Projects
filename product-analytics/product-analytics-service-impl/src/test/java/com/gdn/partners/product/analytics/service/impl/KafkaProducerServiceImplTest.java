package com.gdn.partners.product.analytics.service.impl;


import com.gdn.partners.product.analytics.service.impl.helper.KafkaPublisher;
import model.AttributeValueExtractionsEventModel;
import model.ProductAttributeExtractionsEventModel;
import model.TerminatedSellerDeletionEventModel;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;
import com.gdn.partners.product.analytics.web.model.config.DomainEventName;

import java.util.Date;
import java.util.List;

import static org.mockito.Mockito.verify;

public class KafkaProducerServiceImplTest {

  private SellerFieldsChangeResponse sellerFieldsChangeResponse;
  private TerminatedSellerDeletionEventModel terminatedSellerDeletionEventModel;
  private ProductAttributeExtractionsEventModel productAttributeExtractionsEventModel;

  @InjectMocks
  private KafkaProducerServiceImpl kafkaProducerService;

  @Mock
  private KafkaPublisher kafkaPublisher;

  private static final String TOPIC_NAME = "topicName";

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    sellerFieldsChangeResponse = new SellerFieldsChangeResponse();
    terminatedSellerDeletionEventModel = new TerminatedSellerDeletionEventModel();
    productAttributeExtractionsEventModel = new ProductAttributeExtractionsEventModel();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(kafkaPublisher);
  }

  @Test
  void publishMessageTest() {
    kafkaProducerService.publishMessage(sellerFieldsChangeResponse);
    verify(kafkaPublisher).send(DomainEventName.SELLER_AUTO_QC_DATA_UPDATE,
        sellerFieldsChangeResponse.getSellerCode() + sellerFieldsChangeResponse.getCategoryCode(),
        sellerFieldsChangeResponse);
  }

  @Test
  void publishMessageExceptionTest() {
    Mockito.doThrow(RuntimeException.class).when(kafkaPublisher)
        .send(DomainEventName.SELLER_AUTO_QC_DATA_UPDATE, sellerFieldsChangeResponse.getSellerCode()
            + sellerFieldsChangeResponse.getCategoryCode(), sellerFieldsChangeResponse);
    kafkaProducerService.publishMessage(sellerFieldsChangeResponse);
    verify(kafkaPublisher).send(DomainEventName.SELLER_AUTO_QC_DATA_UPDATE,
        sellerFieldsChangeResponse.getSellerCode() + sellerFieldsChangeResponse.getCategoryCode(),
        sellerFieldsChangeResponse);
  }

  @Test
  void publishMessageForProductDeletionTest() {
    terminatedSellerDeletionEventModel.setProductCode("PRODUCT_CODE");
    kafkaProducerService.publishMessageForProductDeletion(terminatedSellerDeletionEventModel,
        TOPIC_NAME);
    verify(kafkaPublisher).send(TOPIC_NAME, "PRODUCT_CODE", terminatedSellerDeletionEventModel);
  }

  @Test
  void publishMessageForProductAttributeExtractions_Success() {
    // Given
    String topicName = "test-topic";
    productAttributeExtractionsEventModel.setProductId("test-product-id");
    productAttributeExtractionsEventModel.setProductSku("test-sku");
    productAttributeExtractionsEventModel.setAddedDate(new Date());
    List<AttributeValueExtractionsEventModel> attributeValueExtractions = List.of(
        AttributeValueExtractionsEventModel.builder().attributeName("test-attribute")
            .value("test-value").build());
    productAttributeExtractionsEventModel.setAttributeValueExtractions(attributeValueExtractions);
    // When
    kafkaProducerService.publishMessageForProductAttributeExtractions(
        productAttributeExtractionsEventModel, topicName);
    // Then
    verify(kafkaPublisher).send(topicName, productAttributeExtractionsEventModel.getProductId(),
        productAttributeExtractionsEventModel);
  }

  @Test
  void publishMessageForProductAttributeExtractions_WithNullProductId() {
    // Given
    String topicName = "test-topic";
    productAttributeExtractionsEventModel.setProductSku("test-sku");
    productAttributeExtractionsEventModel.setAddedDate(new Date());
    List<AttributeValueExtractionsEventModel> attributeValueExtractions = List.of(
        AttributeValueExtractionsEventModel.builder().attributeName("test-attribute")
            .value("test-value").build());
    productAttributeExtractionsEventModel.setAttributeValueExtractions(attributeValueExtractions);
    // When
    kafkaProducerService.publishMessageForProductAttributeExtractions(
        productAttributeExtractionsEventModel, topicName);
    // Then
    verify(kafkaPublisher).send(topicName, null, productAttributeExtractionsEventModel);
  }

  @Test
  void publishMessageForProductAttributeExtractions_WithEmptyTopicName() {
    // Given
    String topicName = "";
    productAttributeExtractionsEventModel.setProductId("test-product-id");
    productAttributeExtractionsEventModel.setProductSku("test-sku");
    productAttributeExtractionsEventModel.setAddedDate(new Date());
    List<AttributeValueExtractionsEventModel> attributeValueExtractions = List.of(
        AttributeValueExtractionsEventModel.builder().attributeName("test-attribute")
            .value("test-value").build());
    productAttributeExtractionsEventModel.setAttributeValueExtractions(attributeValueExtractions);
    // When
    kafkaProducerService.publishMessageForProductAttributeExtractions(
        productAttributeExtractionsEventModel, topicName);
    // Then
    verify(kafkaPublisher).send(topicName, productAttributeExtractionsEventModel.getProductId(),
        productAttributeExtractionsEventModel);
  }
}