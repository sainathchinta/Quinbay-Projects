package com.gdn.partners.product.analytics.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.entity.AttributeValueExtractions;
import com.gdn.partners.product.analytics.entity.ProductAttributeExtractions;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.repository.ProductAttributeExtractionsRepository;
import com.mongodb.bulk.BulkWriteResult;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class DataFromBigQueryToProductAttributeExtractionsImplTest {

    @Mock
    private GCPProperties gcpProperties;

    @Mock
    private ObjectMapper objectMapper;

    @Mock
    private ProductAttributeExtractionsRepository productAttributeExtractionsRepository;

    @InjectMocks
    private DataFromBigQueryToProductAttributeExtractionsImpl dataFromBigQueryToProductAttributeExtractions;

    private static final int BATCH_SIZE = 2;
    private static final String PRODUCT_ID = "test-product-id";
    private static final String PRODUCT_SKU = "test-product-sku";
    private static final String ATTRIBUTE_NAME = "test-attribute";
    private static final String ATTRIBUTE_VALUE = "test-value";

    @Test
    void writeJsonDataFromFileToDB_WithValidData_ShouldProcessAndSaveData() throws IOException {
        // Given
        Path tempFile = createTempJsonFileWithTestData();
        BulkWriteResult mockBulkWriteResult = mock(BulkWriteResult.class);
        when(mockBulkWriteResult.getMatchedCount()).thenReturn(1);
        when(mockBulkWriteResult.getUpserts()).thenReturn(Collections.emptyList());
        when(mockBulkWriteResult.getModifiedCount()).thenReturn(1);

        ProductAttributeExtractions mockExtraction = createProductAttributeExtraction();

        // Mock line-by-line reading behavior
        when(objectMapper.readValue(any(String.class), eq(ProductAttributeExtractions.class)))
            .thenReturn(mockExtraction);

        when(productAttributeExtractionsRepository.bulkWriteProductAttributeExtractions(any()))
            .thenReturn(mockBulkWriteResult);

        // When
        List<?> result = dataFromBigQueryToProductAttributeExtractions.writeJsonDataFromFileToDB(tempFile.toString(), BATCH_SIZE);

        // Then
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(productAttributeExtractionsRepository, times(2)).bulkWriteProductAttributeExtractions(any());

        // Cleanup
        Files.delete(tempFile);
    }

    @Test
    void writeJsonDataFromFileToDB_WithEmptyData_ShouldNotProcessData() throws IOException {
        // Given
        Path tempFile = createTempJsonFileWithEmptyData();

        // When
        List<?> result = dataFromBigQueryToProductAttributeExtractions.writeJsonDataFromFileToDB(tempFile.toString(), BATCH_SIZE);

        // Then
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(productAttributeExtractionsRepository, never()).bulkWriteProductAttributeExtractions(any());

        // Cleanup
        Files.delete(tempFile);
    }

    @Test
    void writeJsonDataFromFileToDB_WhenRepositoryThrowsException_ShouldPropagateException() throws IOException {
        // Given
        Path tempFile = createTempJsonFileWithTestData();
        ProductAttributeExtractions mockExtraction = createProductAttributeExtraction();

        // Mock line-by-line reading behavior
        when(objectMapper.readValue(any(String.class), eq(ProductAttributeExtractions.class)))
            .thenReturn(mockExtraction);

        when(productAttributeExtractionsRepository.bulkWriteProductAttributeExtractions(any()))
            .thenThrow(new RuntimeException("Test Repository Exception"));

        // When & Then
        RuntimeException exception = assertThrows(RuntimeException.class, () -> 
            dataFromBigQueryToProductAttributeExtractions.writeJsonDataFromFileToDB(tempFile.toString(), BATCH_SIZE)
        );
        assertEquals("Test Repository Exception", exception.getMessage());

        // Cleanup
        Files.delete(tempFile);
    }

    @Test
    void writeJsonDataFromFileToDB_WhenBulkWriteResultIsNull_ShouldContinueProcessing() throws IOException {
        // Given
        Path tempFile = createTempJsonFileWithTestData();
        ProductAttributeExtractions mockExtraction = createProductAttributeExtraction();

        // Mock line-by-line reading behavior
        when(objectMapper.readValue(any(String.class), eq(ProductAttributeExtractions.class)))
            .thenReturn(mockExtraction);

        // Mock repository to return null for bulkWriteResult
        when(productAttributeExtractionsRepository.bulkWriteProductAttributeExtractions(any()))
            .thenReturn(null);

        // When
        List<?> result = dataFromBigQueryToProductAttributeExtractions.writeJsonDataFromFileToDB(tempFile.toString(), BATCH_SIZE);

        // Then
        assertNotNull(result);
        assertTrue(result.isEmpty());
        // Verify that bulkWrite was called twice (for the two batches of data)
        verify(productAttributeExtractionsRepository, times(2)).bulkWriteProductAttributeExtractions(any());

        // Cleanup
        Files.delete(tempFile);
    }

    @Test
    void getResultFileNameLocal_ShouldReturnCorrectValue() {
        // Given
        when(gcpProperties.getProductAttributeExtractionsResultFileNameLocal()).thenReturn("test-local.json");

        // When
        String result = dataFromBigQueryToProductAttributeExtractions.getResultFileNameLocal();

        // Then
        assertEquals("test-local.json", result);
    }

    @Test
    void getResultFileNamePrefix_ShouldReturnCorrectValue() {
        // Given
        when(gcpProperties.getProductAttributeExtractionsResultFileNamePrefix()).thenReturn("test-prefix");

        // When
        String result = dataFromBigQueryToProductAttributeExtractions.getResultFileNamePrefix();

        // Then
        assertEquals("test-prefix", result);
    }

    @Test
    void getResultDataSet_ShouldReturnCorrectValue() {
        // Given
        when(gcpProperties.getProductAttributeExtractionsResultDataSet()).thenReturn("test-dataset");

        // When
        String result = dataFromBigQueryToProductAttributeExtractions.getResultDataSet();

        // Then
        assertEquals("test-dataset", result);
    }

    @Test
    void getResultTablePrefix_ShouldReturnCorrectValue() {
        // Given
        when(gcpProperties.getProductAttributeExtractionsResultTablePrefix()).thenReturn("test-table");

        // When
        String result = dataFromBigQueryToProductAttributeExtractions.getResultTablePrefix();

        // Then
        assertEquals("test-table", result);
    }

    private ProductAttributeExtractions createProductAttributeExtraction() {
        return ProductAttributeExtractions.builder()
            .productId(PRODUCT_ID)
            .productSku(PRODUCT_SKU)
            .attributeValueExtractions(List.of(
                AttributeValueExtractions.builder()
                    .attributeName(ATTRIBUTE_NAME)
                    .attributeValue(ATTRIBUTE_VALUE)
                    .build()))
            .build();
    }

    private Path createTempJsonFileWithTestData() throws IOException {
        String jsonContent = String.format("""
                {"productId":"%s","productSku":"%s","attributeValueExtractions":[{"attributeName":"%s","attributeValue":"%s"}]}
                {"productId":"%s","productSku":"%s","attributeValueExtractions":[{"attributeName":"%s","attributeValue":"%s"}]}
                {"productId":"%s","productSku":"%s","attributeValueExtractions":[{"attributeName":"%s","attributeValue":"%s"}]}
                """, PRODUCT_ID, PRODUCT_SKU, ATTRIBUTE_NAME, ATTRIBUTE_VALUE, PRODUCT_ID,
            PRODUCT_SKU,
            ATTRIBUTE_NAME, ATTRIBUTE_VALUE, PRODUCT_ID, PRODUCT_SKU, ATTRIBUTE_NAME,
            ATTRIBUTE_VALUE);

        Path tempFile = Files.createTempFile("test-data", ".json");
        Files.write(tempFile, jsonContent.getBytes());
        return tempFile;
    }

    private Path createTempJsonFileWithEmptyData() throws IOException {
        String jsonContent = "";
        Path tempFile = Files.createTempFile("test-data", ".json");
        Files.write(tempFile, jsonContent.getBytes());
        return tempFile;
    }
} 