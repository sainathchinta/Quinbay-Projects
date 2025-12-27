package com.gdn.partners.product.analytics.repository.impl;

import com.gdn.partners.product.analytics.entity.AttributeValueExtractions;
import com.gdn.partners.product.analytics.entity.ProductAttributeExtractions;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.model.ProductAttributeExtractionsFields;
import com.gdn.partners.product.analytics.model.enums.ProductAttributeExtractionsStatus;
import com.gdn.partners.product.analytics.repository.helper.DataHelper;
import com.mongodb.bulk.BulkWriteResult;
import org.bson.Document;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.mongodb.core.BulkOperations;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.convert.MongoConverter;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class ProductAttributeExtractionsRepositoryCustomImplTest {

    @Mock
    private MongoTemplate mongoTemplate;

    @Mock
    private BulkOperations bulkOperations;

    @Mock
    private MongoConverter mongoConverter;

    @Mock
    private BulkWriteResult bulkWriteResult;

    @InjectMocks
    private ProductAttributeExtractionsRepositoryCustomImpl repository;

    @Captor
    private ArgumentCaptor<Query> queryCaptor;

    @Captor
    private ArgumentCaptor<Document> documentCaptor;

    @Captor
    private ArgumentCaptor<Update> updateCaptor;

    private static final String PRODUCT_ID = "test-product-id";
    private static final String PRODUCT_SKU = "test-product-sku";
    private static final String ATTRIBUTE_NAME = "test-attribute";
    private static final String ATTRIBUTE_VALUE = "test-value";
    private static final String STORE_ID = "10001";
    private static final int BATCH_SIZE = 100;

    @Test
    void bulkWriteProductAttributeExtractions_WithEmptyList_ShouldReturnBulkWriteResult() {
        // Given
        List<ProductAttributeExtractions> emptyList = new ArrayList<>();
        when(mongoTemplate.bulkOps(eq(BulkOperations.BulkMode.UNORDERED), eq(ProductAttributeExtractions.COLLECTION_NAME)))
            .thenReturn(bulkOperations);
        when(bulkOperations.execute()).thenReturn(bulkWriteResult);

        // When
        BulkWriteResult result = repository.bulkWriteProductAttributeExtractions(emptyList);

        // Then
        assertNotNull(result);
        verify(bulkOperations).execute();
        verify(bulkOperations, never()).upsert(any(Query.class), any(Update.class));
    }

    @Test
    void bulkWriteProductAttributeExtractions_WithSingleItem_ShouldSetMandatoryParamsAndUpsert() {
        // Given
        ProductAttributeExtractions extraction = createProductAttributeExtraction();
        List<ProductAttributeExtractions> extractionsList = Arrays.asList(extraction);

        when(mongoTemplate.bulkOps(eq(BulkOperations.BulkMode.UNORDERED), eq(ProductAttributeExtractions.COLLECTION_NAME)))
            .thenReturn(bulkOperations);
        when(mongoTemplate.getConverter()).thenReturn(mongoConverter);
        when(bulkOperations.execute()).thenReturn(bulkWriteResult);
        
        try (var mockedStatic = mockStatic(DataHelper.class)) {
            mockedStatic.when(() -> DataHelper.getUpdateFromDocument(any(Document.class), eq("_id")))
                .thenAnswer(invocation -> {
                    Document doc = invocation.getArgument(0);
                    Update update = new Update();
                    doc.forEach((key, value) -> {
                        if (!key.equals("_id")) {
                            update.set(key, value);
                        }
                    });
                    return update;
                });

            // When
            BulkWriteResult result = repository.bulkWriteProductAttributeExtractions(extractionsList);

            // Then
            assertNotNull(result);
            verify(mongoConverter).write(eq(extraction), any(Document.class));
            verify(bulkOperations).upsert(queryCaptor.capture(), any(Update.class));
            
            Query capturedQuery = queryCaptor.getValue();
            Document queryObject = capturedQuery.getQueryObject();
            assertEquals(PRODUCT_SKU, queryObject.get(ProductAttributeExtractionsFields.PRODUCT_SKU));
            
            assertEquals(Constants.STORE_ID_VALUE, extraction.getStoreId());
            assertFalse(extraction.isMarkForDelete());
            assertNotNull(extraction.getCreatedDate());
            assertNotNull(extraction.getUpdatedDate());
            assertEquals(Constants.USERNAME, extraction.getCreatedBy());
            assertEquals(Constants.USERNAME, extraction.getUpdatedBy());
            assertEquals(Constants.ZERO, extraction.getVersion());
        }
    }

    @Test
    void bulkWriteProductAttributeExtractions_WithMultipleItems_ShouldProcessAllItems() {
        // Given
        List<ProductAttributeExtractions> extractionsList = Arrays.asList(
            createProductAttributeExtraction(),
            createProductAttributeExtraction(),
            createProductAttributeExtraction()
        );

        when(mongoTemplate.bulkOps(eq(BulkOperations.BulkMode.UNORDERED), eq(ProductAttributeExtractions.COLLECTION_NAME)))
            .thenReturn(bulkOperations);
        when(mongoTemplate.getConverter()).thenReturn(mongoConverter);
        when(bulkOperations.execute()).thenReturn(bulkWriteResult);
        
        try (var mockedStatic = mockStatic(DataHelper.class)) {
            mockedStatic.when(() -> DataHelper.getUpdateFromDocument(any(Document.class), eq("_id")))
                .thenAnswer(invocation -> {
                    Document doc = invocation.getArgument(0);
                    Update update = new Update();
                    doc.forEach((key, value) -> {
                        if (!key.equals("_id")) {
                            update.set(key, value);
                        }
                    });
                    return update;
                });

            // When
            BulkWriteResult result = repository.bulkWriteProductAttributeExtractions(extractionsList);

            // Then
            assertNotNull(result);
            verify(mongoConverter, times(3)).write(any(ProductAttributeExtractions.class), any(Document.class));
            verify(bulkOperations, times(3)).upsert(any(Query.class), any(Update.class));
            verify(bulkOperations).execute();
        }
    }

    @Test
    void fetchProductAttributeExtractions_WithValidData_Success() {
        // Given
        ProductAttributeExtractions productAttributeExtractions = createProductAttributeExtraction();
        List<ProductAttributeExtractions> expectedList = Arrays.asList(productAttributeExtractions);
        when(mongoTemplate.find(queryCaptor.capture(), eq(ProductAttributeExtractions.class)))
            .thenReturn(expectedList);

        // When
        List<ProductAttributeExtractions> result = repository.fetchProductAttributeExtractions(STORE_ID, BATCH_SIZE);

        // Then
        assertNotNull(result);
        assertEquals(1, result.size());
        
        ProductAttributeExtractions fetched = result.get(0);
        assertEquals(PRODUCT_ID, fetched.getProductId());
        assertEquals(PRODUCT_SKU, fetched.getProductSku());
        assertEquals(ProductAttributeExtractionsStatus.NEW.name(), fetched.getStatus());
        
        Query capturedQuery = queryCaptor.getValue();
        assertNotNull(capturedQuery);
        assertEquals(BATCH_SIZE, capturedQuery.getLimit());
        assertEquals(ProductAttributeExtractionsStatus.NEW.name(), capturedQuery.getQueryObject().get("status"));
        assertEquals(STORE_ID, capturedQuery.getQueryObject().get("storeId"));
    }

    @Test
    void fetchProductAttributeExtractions_WithEmptyResult_ReturnsEmptyList() {
        // Given
        when(mongoTemplate.find(queryCaptor.capture(), eq(ProductAttributeExtractions.class)))
            .thenReturn(Arrays.asList());

        // When
        List<ProductAttributeExtractions> result = repository.fetchProductAttributeExtractions(STORE_ID, BATCH_SIZE);

        // Then
        assertNotNull(result);
        assertTrue(result.isEmpty());
        
        Query capturedQuery = queryCaptor.getValue();
        assertNotNull(capturedQuery);
        assertEquals(BATCH_SIZE, capturedQuery.getLimit());
        assertEquals(ProductAttributeExtractionsStatus.NEW.name(), capturedQuery.getQueryObject().get("status"));
        assertEquals(STORE_ID, capturedQuery.getQueryObject().get("storeId"));
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
            .status(ProductAttributeExtractionsStatus.NEW.name())
            .addedDate(new Date())
            .build();
    }
} 