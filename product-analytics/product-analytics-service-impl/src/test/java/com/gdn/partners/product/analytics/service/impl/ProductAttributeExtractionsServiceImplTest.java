package com.gdn.partners.product.analytics.service.impl;

import com.gdn.partners.product.analytics.client.helper.ClientParameterHelper;
import com.gdn.partners.product.analytics.entity.AttributeValueExtractions;
import com.gdn.partners.product.analytics.entity.DSExtractionEntity;
import com.gdn.partners.product.analytics.entity.ProductAttributeExtractions;
import com.gdn.partners.product.analytics.entity.ProductAttributeFeedback;
import com.gdn.partners.product.analytics.model.enums.ProductAttributeExtractionsStatus;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.repository.ProductAttributeExtractionsRepository;
import com.gdn.partners.product.analytics.repository.ProductAttributeFeedbackRepository;
import com.gdn.partners.product.analytics.service.DsExtractedAttributeService;
import com.gdn.partners.product.analytics.service.KafkaProducerService;
import com.gdn.partners.product.analytics.service.cache.DsExtractedAttributeCacheableService;
import com.gdn.partners.product.analytics.service.impl.exception.ValidationException;
import model.AttributeValueExtractionsEventModel;
import model.ProductAttributeExtractionsEventModel;
import model.ProductAttributeFeedbackEventModel;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.BeanUtils;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.times;

@ExtendWith(MockitoExtension.class)
class ProductAttributeExtractionsServiceImplTest {

    @Mock
    private ProductAttributeExtractionsRepository productAttributeExtractionsRepository;

    @Mock
    private KafkaProducerService kafkaProducerService;

    @Mock
    private KafkaTopicProperties kafkaTopicProperties;

    @Mock
    private DsExtractedAttributeCacheableService dsExtractedAttributeCacheableService;

    @Mock
    private ClientParameterHelper clientParameterHelper;

    @Mock
    private ProductAttributeFeedbackRepository productAttributeFeedbackRepository;

    @Mock
    private DsExtractedAttributeService dsExtractedAttributeService;

    @InjectMocks
    private ProductAttributeExtractionsServiceImpl productAttributeExtractionsService;

    @Captor
    private ArgumentCaptor<ProductAttributeExtractionsEventModel> eventModelCaptor;

    private static final String STORE_ID = "10001";
    private static final int BATCH_SIZE = 100;
    private static final String PRODUCT_ID = "test-product-id";
    private static final String PRODUCT_SKU = "test-sku";
    private static final String ATTRIBUTE_NAME = "test-attribute";
    private static final String ATTRIBUTE_VALUE = "test-value";
    private static final String ATTRIBUTE_CODE = "AT-001";
    private static final String TOPIC_NAME = "test-topic";

    private ProductAttributeFeedbackEventModel eventModel;
    private ProductAttributeFeedback savedFeedback;

    @BeforeEach
    void setUp() {
        eventModel = new ProductAttributeFeedbackEventModel();
        eventModel.setProductCode(PRODUCT_ID);
        eventModel.setAttributeName("color");
        eventModel.setPreviousValue(List.of("red"));
        eventModel.setCurrentValue(List.of("blue"));

        savedFeedback = new ProductAttributeFeedback();
        BeanUtils.copyProperties(eventModel, savedFeedback);
    }

    @Test
    void publishEventsForProductAttributeExtractions_WithValidData_Success() {
        // Given
        ProductAttributeExtractions productAttributeExtractions = createProductAttributeExtractions(
            Arrays.asList(createAttributeValueExtraction()));
        List<ProductAttributeExtractions> productAttributeExtractionsList = Arrays.asList(productAttributeExtractions);

        when(productAttributeExtractionsRepository.fetchProductAttributeExtractions(STORE_ID, BATCH_SIZE))
            .thenReturn(productAttributeExtractionsList);
        when(kafkaTopicProperties.getProductAttributeExtractionsValidationEventName()).thenReturn(TOPIC_NAME);
        when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);

        // When
        productAttributeExtractionsService.publishEventsForProductAttributeExtractions(STORE_ID, BATCH_SIZE);

        // Then
        verify(productAttributeExtractionsRepository).fetchProductAttributeExtractions(STORE_ID, BATCH_SIZE);
        verify(kafkaProducerService).publishMessageForProductAttributeExtractions(eventModelCaptor.capture(), eq(TOPIC_NAME));
        verify(productAttributeExtractionsRepository).save(productAttributeExtractions);
        verify(clientParameterHelper).getStoreId();

        ProductAttributeExtractionsEventModel capturedModel = eventModelCaptor.getValue();
        assertNotNull(capturedModel);
        assertEquals(PRODUCT_ID, capturedModel.getProductId());
        assertEquals(PRODUCT_SKU, capturedModel.getProductSku());
        
        List<AttributeValueExtractionsEventModel> attributeValueExtractions = capturedModel.getAttributeValueExtractions();
        assertNotNull(attributeValueExtractions);
        assertEquals(1, attributeValueExtractions.size());
        assertEquals(ATTRIBUTE_NAME, attributeValueExtractions.get(0).getAttributeName());
        assertEquals(ATTRIBUTE_VALUE, attributeValueExtractions.get(0).getValue());
    }

    @Test
    void publishEventsForProductAttributeExtractions_WithEmptyList_NoProcessing() {
        // Given
        when(productAttributeExtractionsRepository.fetchProductAttributeExtractions(STORE_ID, BATCH_SIZE))
            .thenReturn(Collections.emptyList());

        // When
        productAttributeExtractionsService.publishEventsForProductAttributeExtractions(STORE_ID, BATCH_SIZE);

        // Then
        verify(productAttributeExtractionsRepository).fetchProductAttributeExtractions(STORE_ID, BATCH_SIZE);
        verify(kafkaProducerService, never()).publishMessageForProductAttributeExtractions(any(), any());
        verify(productAttributeExtractionsRepository, never()).save(any());
    }

    @Test
    void publishEventsForProductAttributeExtractions_WithEmptyAttributeValueExtractions_Success() {
        // Given
        ProductAttributeExtractions productAttributeExtractions = createProductAttributeExtractions(Collections.emptyList());
        List<ProductAttributeExtractions> productAttributeExtractionsList = Arrays.asList(productAttributeExtractions);

        when(productAttributeExtractionsRepository.fetchProductAttributeExtractions(STORE_ID, BATCH_SIZE))
            .thenReturn(productAttributeExtractionsList);
        when(kafkaTopicProperties.getProductAttributeExtractionsValidationEventName()).thenReturn(TOPIC_NAME);
        when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);

        // When
        productAttributeExtractionsService.publishEventsForProductAttributeExtractions(STORE_ID, BATCH_SIZE);

        // Then
        verify(productAttributeExtractionsRepository).fetchProductAttributeExtractions(STORE_ID, BATCH_SIZE);
        verify(kafkaProducerService).publishMessageForProductAttributeExtractions(eventModelCaptor.capture(), eq(TOPIC_NAME));
        verify(productAttributeExtractionsRepository).save(productAttributeExtractions);
        verify(clientParameterHelper).getStoreId();

        ProductAttributeExtractionsEventModel capturedModel = eventModelCaptor.getValue();
        assertNotNull(capturedModel);
        assertEquals(PRODUCT_ID, capturedModel.getProductId());
        assertEquals(PRODUCT_SKU, capturedModel.getProductSku());
        assertTrue(capturedModel.getAttributeValueExtractions().isEmpty());
    }

    @Test
    void publishEventsForProductAttributeExtractions_WhenExceptionOccurs_LogsErrorAndContinues() {
        // Given
        ProductAttributeExtractions productAttributeExtractions = createProductAttributeExtractions(
            Arrays.asList(createAttributeValueExtraction()));
        List<ProductAttributeExtractions> productAttributeExtractionsList = Arrays.asList(productAttributeExtractions);

        when(productAttributeExtractionsRepository.fetchProductAttributeExtractions(STORE_ID, BATCH_SIZE))
            .thenReturn(productAttributeExtractionsList);
        when(kafkaTopicProperties.getProductAttributeExtractionsValidationEventName()).thenReturn(TOPIC_NAME);
        doThrow(new RuntimeException("Kafka publishing failed"))
            .when(kafkaProducerService)
            .publishMessageForProductAttributeExtractions(any(ProductAttributeExtractionsEventModel.class), eq(TOPIC_NAME));

        // When
        productAttributeExtractionsService.publishEventsForProductAttributeExtractions(STORE_ID, BATCH_SIZE);

        // Then
        verify(productAttributeExtractionsRepository).fetchProductAttributeExtractions(STORE_ID, BATCH_SIZE);
        verify(kafkaProducerService).publishMessageForProductAttributeExtractions(any(ProductAttributeExtractionsEventModel.class), eq(TOPIC_NAME));
        verify(productAttributeExtractionsRepository, never()).save(any());
    }

    @Test
    void publishEventsForProductAttributeExtractionsByProductSku_WithValidData_Success() {
        // Given
        List<String> productSkuList = List.of(PRODUCT_SKU);
        ProductAttributeExtractions productAttributeExtractions = createProductAttributeExtractions(
            Arrays.asList(createAttributeValueExtraction()));
        List<ProductAttributeExtractions> productAttributeExtractionsList = List.of(productAttributeExtractions);

        when(productAttributeExtractionsRepository.findByStoreIdAndProductSkuInAndStatus(STORE_ID, productSkuList, ProductAttributeExtractionsStatus.NEW.name()))
            .thenReturn(productAttributeExtractionsList);
        when(kafkaTopicProperties.getProductAttributeExtractionsValidationEventName()).thenReturn(TOPIC_NAME);
        when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);

        // When
        productAttributeExtractionsService.publishEventsForProductAttributeExtractionsByProductSku(STORE_ID, productSkuList);

        // Then
        verify(productAttributeExtractionsRepository).findByStoreIdAndProductSkuInAndStatus(STORE_ID, productSkuList, ProductAttributeExtractionsStatus.NEW.name());
        verify(kafkaProducerService).publishMessageForProductAttributeExtractions(eventModelCaptor.capture(), eq(TOPIC_NAME));
        verify(productAttributeExtractionsRepository).save(productAttributeExtractions);
        verify(clientParameterHelper).getStoreId();

        ProductAttributeExtractionsEventModel capturedModel = eventModelCaptor.getValue();
        assertNotNull(capturedModel);
        assertEquals(PRODUCT_ID, capturedModel.getProductId());
        assertEquals(PRODUCT_SKU, capturedModel.getProductSku());
        List<AttributeValueExtractionsEventModel> attributeValueExtractions = capturedModel.getAttributeValueExtractions();
        assertNotNull(attributeValueExtractions);
        assertEquals(1, attributeValueExtractions.size());
        assertEquals(ATTRIBUTE_NAME, attributeValueExtractions.get(0).getAttributeName());
        assertEquals(ATTRIBUTE_VALUE, attributeValueExtractions.get(0).getValue());
    }

    @Test
    void publishEventsForProductAttributeExtractionsByProductSku_WithEmptyList_NoProcessing() {
        // Given
        List<String> productSkuList = List.of(PRODUCT_SKU);
        when(productAttributeExtractionsRepository.findByStoreIdAndProductSkuInAndStatus(STORE_ID, productSkuList, ProductAttributeExtractionsStatus.NEW.name()))
            .thenReturn(Collections.emptyList());

        // When
        productAttributeExtractionsService.publishEventsForProductAttributeExtractionsByProductSku(STORE_ID, productSkuList);

        // Then
        verify(productAttributeExtractionsRepository).findByStoreIdAndProductSkuInAndStatus(STORE_ID, productSkuList, ProductAttributeExtractionsStatus.NEW.name());
        verify(kafkaProducerService, never()).publishMessageForProductAttributeExtractions(any(), any());
        verify(productAttributeExtractionsRepository, never()).save(any());
    }

    @Test
    void validateAndPublishPCBEventsForProductAttributeExtractions_WithValidAttributes_Success()
        throws InvocationTargetException, IllegalAccessException {
        // Given
        AttributeValueExtractionsEventModel attributeModel = AttributeValueExtractionsEventModel.builder()
            .attributeName(ATTRIBUTE_NAME)
            .value(ATTRIBUTE_VALUE)
            .build();

        ProductAttributeExtractionsEventModel inputModel = ProductAttributeExtractionsEventModel.builder()
            .productId(PRODUCT_ID)
            .productSku(PRODUCT_SKU)
            .addedDate(new Date())
            .attributeValueExtractions(List.of(attributeModel))
            .build();

        DSExtractionEntity dsExtractionEntity = new DSExtractionEntity();
        dsExtractionEntity.setMarkForDelete(false);

        when(dsExtractedAttributeCacheableService.fetchDSExtractionsByName(ATTRIBUTE_NAME))
            .thenReturn(dsExtractionEntity);
        when(kafkaTopicProperties.getProductAttributeExtractionsEventName())
            .thenReturn(TOPIC_NAME);

        // When
        productAttributeExtractionsService.validateAndPublishPCBEventsForProductAttributeExtractions(inputModel);

        // Then
        verify(dsExtractedAttributeCacheableService).fetchDSExtractionsByName(ATTRIBUTE_NAME);
        verify(kafkaTopicProperties).getProductAttributeExtractionsEventName();
        verify(kafkaProducerService).publishMessageForProductAttributeExtractions(
            any(ProductAttributeExtractionsEventModel.class), 
            eq(TOPIC_NAME)
        );
    }

    @Test
    void validateAndPublishPCBEventsForProductAttributeExtractions_WithDeletedAttribute_FiltersOut()
        throws InvocationTargetException, IllegalAccessException {
        // Given
        AttributeValueExtractionsEventModel attributeModel = AttributeValueExtractionsEventModel.builder()
            .attributeName(ATTRIBUTE_NAME)
            .value(ATTRIBUTE_VALUE)
            .build();

        ProductAttributeExtractionsEventModel inputModel = ProductAttributeExtractionsEventModel.builder()
            .productId(PRODUCT_ID)
            .productSku(PRODUCT_SKU)
            .addedDate(new Date())
            .attributeValueExtractions(List.of(attributeModel))
            .build();

        DSExtractionEntity dsExtractionEntity = new DSExtractionEntity();
        dsExtractionEntity.setMarkForDelete(true);

        when(dsExtractedAttributeCacheableService.fetchDSExtractionsByName(ATTRIBUTE_NAME))
            .thenReturn(dsExtractionEntity);

        // When
        productAttributeExtractionsService.validateAndPublishPCBEventsForProductAttributeExtractions(inputModel);

        // Then
        verify(dsExtractedAttributeCacheableService).fetchDSExtractionsByName(ATTRIBUTE_NAME);
        verify(kafkaTopicProperties, never()).getProductAttributeExtractionsEventName();
        verify(kafkaProducerService, never()).publishMessageForProductAttributeExtractions(any(), any());
    }

    @Test
    void validateAndPublishPCBEventsForProductAttributeExtractions_WithNullAttribute_FiltersOut()
        throws InvocationTargetException, IllegalAccessException {
        // Given
        AttributeValueExtractionsEventModel attributeModel = AttributeValueExtractionsEventModel.builder()
            .attributeName(ATTRIBUTE_NAME)
            .value(ATTRIBUTE_VALUE)
            .build();

        ProductAttributeExtractionsEventModel inputModel = ProductAttributeExtractionsEventModel.builder()
            .productId(PRODUCT_ID)
            .productSku(PRODUCT_SKU)
            .addedDate(new Date())
            .attributeValueExtractions(List.of(attributeModel))
            .build();

        when(dsExtractedAttributeCacheableService.fetchDSExtractionsByName(ATTRIBUTE_NAME))
            .thenReturn(null);

        // When
        productAttributeExtractionsService.validateAndPublishPCBEventsForProductAttributeExtractions(inputModel);

        // Then
        verify(dsExtractedAttributeCacheableService).fetchDSExtractionsByName(ATTRIBUTE_NAME);
        verify(kafkaTopicProperties, never()).getProductAttributeExtractionsEventName();
        verify(kafkaProducerService, never()).publishMessageForProductAttributeExtractions(any(), any());
    }

    @Test
    void validateAndPublishPCBEventsForProductAttributeExtractions_WithEmptyAttributes_DoesNotPublish()
        throws InvocationTargetException, IllegalAccessException {
        // Given
        ProductAttributeExtractionsEventModel inputModel = ProductAttributeExtractionsEventModel.builder()
            .productId(PRODUCT_ID)
            .productSku(PRODUCT_SKU)
            .addedDate(new Date())
            .attributeValueExtractions(Collections.emptyList())
            .build();

        // When
        productAttributeExtractionsService.validateAndPublishPCBEventsForProductAttributeExtractions(inputModel);

        // Then
        verifyNoMoreInteractions(dsExtractedAttributeCacheableService, kafkaProducerService, kafkaTopicProperties);
    }

    @Test
    void updateFeedbackForProductAttributeTest()
      throws InvocationTargetException, IllegalAccessException {
        // Arrange
        eventModel.setAttributeCode(ATTRIBUTE_CODE);
        when(
          productAttributeFeedbackRepository.save(any(ProductAttributeFeedback.class))).thenReturn(
          savedFeedback);

        when(dsExtractedAttributeService.getDsExtractedAttributeByAttributeCode(
            eq(ATTRIBUTE_CODE))).thenReturn(new DSExtractionEntity());

        // Act
        productAttributeExtractionsService.updateFeedbackForProductAttribute(eventModel);

        // Assert
        verify(productAttributeFeedbackRepository, times(1)).save(
          any(ProductAttributeFeedback.class));
        verify(dsExtractedAttributeService).getDsExtractedAttributeByAttributeCode(eq(ATTRIBUTE_CODE));
    }

    @Test
    void updateFeedbackForProductAttributeTest_attributeNotPresent()
        throws InvocationTargetException, IllegalAccessException {
        // Arrange
        eventModel.setAttributeCode(ATTRIBUTE_CODE);
        when(
            productAttributeFeedbackRepository.save(any(ProductAttributeFeedback.class))).thenReturn(
            savedFeedback);

        when(dsExtractedAttributeService.getDsExtractedAttributeByAttributeCode(
            eq(ATTRIBUTE_CODE))).thenReturn(null);

        // Act
        productAttributeExtractionsService.updateFeedbackForProductAttribute(eventModel);

        // Assert
        verify(productAttributeFeedbackRepository, times(1)).save(
            any(ProductAttributeFeedback.class));
        verify(dsExtractedAttributeService).getDsExtractedAttributeByAttributeCode(eq(ATTRIBUTE_CODE));
    }

    @Test
    void updateFeedbackForProductAttributeWithEmptyProductCodeExceptionTest() {
        // Arrange
        eventModel.setProductCode("");

        // Act & Assert
        org.junit.jupiter.api.Assertions.assertThrows(ValidationException.class,
          () -> productAttributeExtractionsService.updateFeedbackForProductAttribute(eventModel));

        // Verify repository is not called
        verify(productAttributeFeedbackRepository, never()).save(
          any(ProductAttributeFeedback.class));
    }

    @Test
    void updateFeedbackForProductAttributeWithNullProductCodeExceptionTest() {
        // Arrange
        eventModel.setProductCode(null);

        // Act & Assert
        org.junit.jupiter.api.Assertions.assertThrows(ValidationException.class,
          () -> productAttributeExtractionsService.updateFeedbackForProductAttribute(eventModel));

        // Verify repository is not called
        verify(productAttributeFeedbackRepository, never()).save(
          any(ProductAttributeFeedback.class));
    }

    private ProductAttributeExtractions createProductAttributeExtractions(List<AttributeValueExtractions> attributeValueExtractions) {
        return ProductAttributeExtractions.builder()
            .productId(PRODUCT_ID)
            .productSku(PRODUCT_SKU)
            .attributeValueExtractions(attributeValueExtractions)
            .status(ProductAttributeExtractionsStatus.NEW.name())
            .addedDate(new Date())
            .build();
    }

    private AttributeValueExtractions createAttributeValueExtraction() {
        return AttributeValueExtractions.builder()
            .attributeName(ATTRIBUTE_NAME)
            .attributeValue(ATTRIBUTE_VALUE)
            .build();
    }
}