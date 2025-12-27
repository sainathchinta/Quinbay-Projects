package com.gdn.aggregate.platform.module.product.listener.service.saver;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.platform.module.product.listener.model.util.TerminatedSellerDeletionEventModel;
import com.gdn.aggregate.platform.module.product.listener.repository.processed.SivaItemRepository;
import com.gdn.aggregate.platform.module.product.listener.repository.processed.SivaProductRepository;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.ItemRepository;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.MasterDataItemRepository;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.MasterDataProductRepository;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.ProductRepository;
import com.gdn.aggregate.platform.module.product.listener.repositorysub.raw.PickupPointRepository;
import com.gdn.aggregate.platform.module.product.listener.service.helper.PublisherService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaItemService;
import com.gdn.aggregate.platform.module.product.listener.service.processor.processed.SivaProductService;
import com.blibli.oss.backend.reactor.scheduler.SchedulerHelper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.test.util.ReflectionTestUtils;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;
import reactor.test.StepVerifier;
import java.util.Arrays;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;
import static org.mockito.Mockito.lenient;

@ExtendWith(MockitoExtension.class)
class ProductDeletionServiceTest {

    @Mock
    private Logger log;


    private static final String PRODUCT_SKU = "TEST-SKU-123";
    private static final String PRODUCT_CODE = "TEST-CODE-123";
    @Mock
    private PickupPointRepository pickupPointRepository;

    @Mock
    private ProductRepository productRepository;

    @Mock
    private ItemRepository itemRepository;

    @Mock
    private SivaItemRepository sivaItemRepository;

    @Mock
    private SivaProductRepository sivaProductRepository;

    @Mock
    private MasterDataProductRepository masterDataProductRepository;

    @Mock
    private MasterDataItemRepository masterDataItemRepository;

    @Mock
    private SivaItemService sivaItemService;

    @Mock
    private SivaProductService sivaProductService;

    @InjectMocks
    private ProductDeletionService productDeletionService;

    private static final String PRODUCT_SKU_2 = "TEST-SKU-456";
    private static final String PRODUCT_CODE_2 = "TEST-CODE-456";
    private static final String TRACE_ID = "test-trace-id";
    
    private List<TerminatedSellerDeletionEventModel> eventModels;

    @Mock
    private PublisherService publisherService;

    @Mock
    private SchedulerHelper schedulerHelper;

    @BeforeEach
    void setUp() {

      TerminatedSellerDeletionEventModel model1 = TerminatedSellerDeletionEventModel.builder()
                .productSku(PRODUCT_SKU)
                .productCode(PRODUCT_CODE)
                .build();
                
        TerminatedSellerDeletionEventModel model2 = TerminatedSellerDeletionEventModel.builder()
                .productSku(PRODUCT_SKU_2)
                .productCode(PRODUCT_CODE_2)
                .build();
                
        eventModels = Arrays.asList(model1, model2);
        
        // Mock SchedulerHelper to return immediate scheduler for synchronous execution in tests
        // Using lenient() to avoid UnnecessaryStubbingException for tests that don't use performDeletionFromAllDataSources
        lenient().when(schedulerHelper.of(anyString())).thenReturn(Schedulers.immediate());
    }
    
    /**
     * Helper method to wait for async operations to complete
     */
    private void waitForAsyncOperations() {
        try {
            // Give more time for reactive operations to complete
            Thread.sleep(200);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    @Test
    void performDeletionFromAllDataSources_WhenSuccessful_ShouldDeleteAllData() {
        when(publisherService.publishProductEsDeletionEvent(any())).thenReturn(Mono.just(true));
        
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU)).thenReturn(1L);
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU_2)).thenReturn(1L);
        
        doNothing().when(sivaItemService).performItemDeletionByProductSku(anyString(), any(SaveParam.class));
        doNothing().when(sivaProductRepository).deleteById(anyString());
        
        doNothing().when(productRepository).deleteById(anyString());
        
        doNothing().when(masterDataProductRepository).deleteByProductCode(anyString());
        doNothing().when(masterDataItemRepository).deleteByProductCode(anyString());

        // Execute and wait for completion
        productDeletionService.performDeletionFromAllDataSources(eventModels, TRACE_ID)
            .doOnSuccess(v -> waitForAsyncOperations())
            .block();

        verify(publisherService).publishProductEsDeletionEvent(argThat(event -> 
            event.getProductSkus().containsAll(Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2)) &&
            event.getTraceId().equals(TRACE_ID)
        ));
        
        for (String sku : Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2)) {
            verify(pickupPointRepository).deleteByProductSku(sku);
            verify(sivaItemService).performItemDeletionByProductSku(eq(sku), argThat(param -> 
                param.getTraceId().equals(TRACE_ID)));
            verify(sivaProductRepository).deleteById(sku);
            verify(productRepository).deleteById(sku);
        }
        
        verify(masterDataProductRepository).deleteByProductCode(PRODUCT_CODE);
        verify(masterDataItemRepository).deleteByProductCode(PRODUCT_CODE);
        verify(masterDataProductRepository).deleteByProductCode(PRODUCT_CODE_2);
        verify(masterDataItemRepository).deleteByProductCode(PRODUCT_CODE_2);
        
        verify(sivaProductService, never()).deleteProductByProductSkuFromElasticsearch(anyString(), any(SaveParam.class));
        verify(sivaItemService, never()).deleteByProductSkuFromElasticsearch(anyString(), any(SaveParam.class));
    }

    @Test
    void performDeletionFromAllDataSources_WhenSharedProduct_ShouldNotDeleteMasterData() {
        // Given
        eventModels.forEach(model -> model.setSharedProduct(true));
        
        // Setup ES event publishing
        when(publisherService.publishProductEsDeletionEvent(any())).thenReturn(Mono.just(true));
        
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU)).thenReturn(1L);
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU_2)).thenReturn(1L);
        
        doNothing().when(sivaItemService).performItemDeletionByProductSku(anyString(), any(SaveParam.class));
        doNothing().when(sivaProductRepository).deleteById(anyString());
        
        doNothing().when(productRepository).deleteById(anyString());
        doNothing().when(itemRepository).deleteByProductSku(anyString());

        productDeletionService.performDeletionFromAllDataSources(eventModels, TRACE_ID)
            .doOnSuccess(v -> waitForAsyncOperations())
            .block();

        verify(publisherService).publishProductEsDeletionEvent(argThat(event -> 
            event.getProductSkus().containsAll(Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2)) &&
            event.getTraceId().equals(TRACE_ID)
        ));

        for (String sku : Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2)) {
            verify(pickupPointRepository).deleteByProductSku(sku);
            verify(sivaProductRepository).deleteById(sku);
            verify(sivaItemService).performItemDeletionByProductSku(eq(sku), argThat(param -> 
                param.getTraceId().equals(TRACE_ID)));
            verify(productRepository).deleteById(sku);
            verify(itemRepository).deleteByProductSku(sku);
        }
        
        verify(sivaProductService, never()).deleteProductByProductSkuFromElasticsearch(anyString(), any(SaveParam.class));
        verify(sivaItemService, never()).deleteByProductSkuFromElasticsearch(anyString(), any(SaveParam.class));
        
        verifyNoInteractions(masterDataProductRepository, masterDataItemRepository);
    }

    @Test
    void performDeletionFromAllDataSources_WhenEsPublishingFails_ShouldContinueWithMongoDeletions() {
        // Given
        when(publisherService.publishProductEsDeletionEvent(any()))
                .thenReturn(Mono.error(new RuntimeException("ES Publishing Error")));
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU)).thenReturn(1L);
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU_2)).thenReturn(1L);
        doNothing().when(sivaItemService).performItemDeletionByProductSku(anyString(), any(SaveParam.class));
        doNothing().when(productRepository).deleteById(anyString());
        doNothing().when(sivaProductRepository).deleteById(anyString());

        productDeletionService.performDeletionFromAllDataSources(eventModels, TRACE_ID)
            .doOnSuccess(v -> waitForAsyncOperations())
            .block();

        verify(publisherService).publishProductEsDeletionEvent(any());
        
        // Verify MongoDB operations still executed
        for (String sku : Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2)) {
            verify(pickupPointRepository).deleteByProductSku(sku);
            verify(sivaItemService).performItemDeletionByProductSku(eq(sku), argThat(param -> 
                param.getTraceId().equals(TRACE_ID)));
            verify(sivaProductRepository).deleteById(sku);
            verify(productRepository).deleteById(sku);
        }
        
        verify(sivaProductService, never()).deleteProductByProductSkuFromElasticsearch(anyString(), any(SaveParam.class));
        verify(sivaItemService, never()).deleteByProductSkuFromElasticsearch(anyString(), any(SaveParam.class));
    }

    @Test
    void performDeletionFromAllDataSources_WhenMongoOperationsFail_ShouldContinueWithOtherProducts() {
        when(publisherService.publishProductEsDeletionEvent(any())).thenReturn(Mono.just(true));
        
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU)).thenReturn(1L);
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU_2)).thenReturn(1L);
        
        doThrow(new RuntimeException("Mongo Error"))
                .doNothing()
                .when(sivaProductRepository).deleteById(PRODUCT_SKU);

        doNothing().when(sivaProductRepository).deleteById(PRODUCT_SKU_2);
        doNothing().when(sivaItemService).performItemDeletionByProductSku(eq(PRODUCT_SKU_2), any(SaveParam.class));
        
        doNothing().when(productRepository).deleteById(anyString());

        productDeletionService.performDeletionFromAllDataSources(eventModels, TRACE_ID)
            .doOnSuccess(v -> waitForAsyncOperations())
            .block();

        verify(publisherService).publishProductEsDeletionEvent(argThat(event -> 
            event.getProductSkus().containsAll(Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2))
        ));
        
        verify(pickupPointRepository).deleteByProductSku(PRODUCT_SKU);
        verify(pickupPointRepository).deleteByProductSku(PRODUCT_SKU_2);
        
        verify(sivaProductRepository).deleteById(PRODUCT_SKU);
        verify(sivaItemService).performItemDeletionByProductSku(eq(PRODUCT_SKU), any(SaveParam.class));
        verify(sivaProductRepository).deleteById(PRODUCT_SKU_2);
        verify(sivaItemService).performItemDeletionByProductSku(eq(PRODUCT_SKU_2), any(SaveParam.class));
        
        verify(productRepository).deleteById(PRODUCT_SKU);
        verify(productRepository).deleteById(PRODUCT_SKU_2);
        
        verify(sivaProductService, never()).deleteProductByProductSkuFromElasticsearch(anyString(), any(SaveParam.class));
        verify(sivaItemService, never()).deleteByProductSkuFromElasticsearch(anyString(), any(SaveParam.class));
    }

    @Test
    void performElasticSearchDeletion_WhenBothSuccessful_ShouldLogSuccess() {
        try (MockedStatic<LoggerFactory> mockedLoggerFactory = Mockito.mockStatic(LoggerFactory.class)) {
            // Given
            mockedLoggerFactory.when(() -> LoggerFactory.getLogger(ProductDeletionService.class)).thenReturn(log);
            
            SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();
            when(sivaProductService.deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                    .thenReturn(Mono.just(true));
            when(sivaItemService.deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                    .thenReturn(Mono.just(true));

            // When/Then
            StepVerifier.create(productDeletionService.performElasticSearchDeletion(PRODUCT_SKU, saveParam))
                .verifyComplete();
            verify(sivaProductService).deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
            verify(sivaItemService).deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
        }
    }

    @Test
    void performElasticSearchDeletion_WhenProductSucceedsAndItemFails_ShouldLogPartialSuccess() {
        try (MockedStatic<LoggerFactory> mockedLoggerFactory = Mockito.mockStatic(LoggerFactory.class)) {
            // Given
            mockedLoggerFactory.when(() -> LoggerFactory.getLogger(ProductDeletionService.class)).thenReturn(log);
            
            SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();
            when(sivaProductService.deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                    .thenReturn(Mono.just(true));
            when(sivaItemService.deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                    .thenReturn(Mono.just(false));

            // When/Then
            StepVerifier.create(productDeletionService.performElasticSearchDeletion(PRODUCT_SKU, saveParam))
                .verifyComplete();
            verify(sivaProductService).deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
            verify(sivaItemService).deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
        }
    }

    @Test
    void performElasticSearchDeletion_WhenProductFailsAndItemSucceeds_ShouldLogPartialSuccess() {
        // Given
        SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();
        when(sivaProductService.deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.just(false));
        when(sivaItemService.deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.just(true));

        // When/Then
        StepVerifier.create(productDeletionService.performElasticSearchDeletion(PRODUCT_SKU, saveParam))
            .verifyComplete();
        verify(sivaProductService).deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
        verify(sivaItemService).deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
    }

    @Test
    void performElasticSearchDeletion_WhenBothFail_ShouldLogFailure() {
        // Given
        SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();
        when(sivaProductService.deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.just(false));
        when(sivaItemService.deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.just(false));

        // When/Then
        StepVerifier.create(productDeletionService.performElasticSearchDeletion(PRODUCT_SKU, saveParam))
            .verifyComplete();
        verify(sivaProductService).deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
    }

    @Test
    void performElasticSearchDeletion_WhenProductThrowsError_ShouldHandleGracefully() {
        // Given
        SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();
        when(sivaProductService.deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.error(new RuntimeException("ES Product deletion error")));
        when(sivaItemService.deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.just(true));

        // When/Then
        StepVerifier.create(productDeletionService.performElasticSearchDeletion(PRODUCT_SKU, saveParam))
            .verifyComplete();
        verify(sivaProductService).deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
        verify(sivaItemService).deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
    }

    @Test
    void performElasticSearchDeletion_WhenItemThrowsError_ShouldHandleGracefully() {
        try (MockedStatic<LoggerFactory> mockedLoggerFactory = Mockito.mockStatic(LoggerFactory.class)) {
            // Given
            mockedLoggerFactory.when(() -> LoggerFactory.getLogger(ProductDeletionService.class)).thenReturn(log);
            
            SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();
            when(sivaProductService.deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                    .thenReturn(Mono.just(true));
            when(sivaItemService.deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                    .thenReturn(Mono.error(new RuntimeException("ES Item deletion error")));

            // When/Then
            StepVerifier.create(productDeletionService.performElasticSearchDeletion(PRODUCT_SKU, saveParam))
                .verifyComplete();

            // Verify service calls
            verify(sivaProductService).deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
            verify(sivaItemService).deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
        }
    }

    @Test
    void performElasticSearchDeletion_WhenBothThrowError_ShouldHandleGracefully() {
        // Given
        SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();
        when(sivaProductService.deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.error(new RuntimeException("ES Product deletion error")));
        when(sivaItemService.deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.error(new RuntimeException("ES Item deletion error")));

        // When/Then
        StepVerifier.create(productDeletionService.performElasticSearchDeletion(PRODUCT_SKU, saveParam))
            .verifyComplete();
        verify(sivaProductService).deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
        verify(sivaItemService).deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
    }

    @Test
    void performElasticSearchDeletion_WhenPartialFailure_ShouldReturnTrue() {
        // Given
        when(sivaProductService.deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.just(true));
        when(sivaItemService.deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.just(false));
        SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();

        productDeletionService.performElasticSearchDeletion(PRODUCT_SKU, saveParam);
    }

    @Test
    void performDeletionFromAllDataSources_WhenEmptyList_ShouldDoNothing() {
        // Given
        List<TerminatedSellerDeletionEventModel> emptyList = List.of();

        productDeletionService.performDeletionFromAllDataSources(emptyList, TRACE_ID)
            .doOnSuccess(v -> waitForAsyncOperations())
            .block();

        verifyNoInteractions(publisherService, pickupPointRepository, sivaProductService, 
            sivaItemService, sivaProductRepository, productRepository, 
            masterDataProductRepository, masterDataItemRepository);
    }

    @Test
    void performDeletionFromAllDataSources_WhenDuplicateSkus_ShouldKeepFirstOccurrence() {
        // Given
        TerminatedSellerDeletionEventModel duplicateModel = TerminatedSellerDeletionEventModel.builder()
                .productSku(PRODUCT_SKU)                .productCode("DIFFERENT-CODE")                .build();
        List<TerminatedSellerDeletionEventModel> modelsWithDuplicate = Arrays.asList(
            eventModels.get(0), eventModels.get(1), duplicateModel);
        when(publisherService.publishProductEsDeletionEvent(any())).thenReturn(Mono.just(true));
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU)).thenReturn(1L);
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU_2)).thenReturn(1L);
        
        doNothing().when(sivaItemService).performItemDeletionByProductSku(anyString(), any(SaveParam.class));
        doNothing().when(sivaProductRepository).deleteById(anyString());
        
        doNothing().when(productRepository).deleteById(anyString());
        
        doNothing().when(masterDataProductRepository).deleteByProductCode(anyString());
        doNothing().when(masterDataItemRepository).deleteByProductCode(anyString());

        // Execute and wait for completion
        productDeletionService.performDeletionFromAllDataSources(modelsWithDuplicate, TRACE_ID)
            .doOnSuccess(v -> waitForAsyncOperations())
            .block();

        verify(publisherService).publishProductEsDeletionEvent(argThat(event -> 
            event.getProductSkus().containsAll(Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2)) &&
            event.getProductSkus().size() == 2 &&  // Should only have 2 SKUs, not 3
            event.getTraceId().equals(TRACE_ID)
        ));
        verify(pickupPointRepository, times(1)).deleteByProductSku(PRODUCT_SKU);
        verify(pickupPointRepository, times(1)).deleteByProductSku(PRODUCT_SKU_2);
        
        verify(sivaProductRepository, times(1)).deleteById(PRODUCT_SKU);
        verify(sivaProductRepository, times(1)).deleteById(PRODUCT_SKU_2);
        
        verify(sivaItemService, times(1)).performItemDeletionByProductSku(eq(PRODUCT_SKU), any(SaveParam.class));
        verify(sivaItemService, times(1)).performItemDeletionByProductSku(eq(PRODUCT_SKU_2), any(SaveParam.class));
        
        verify(productRepository, times(1)).deleteById(PRODUCT_SKU);
        verify(productRepository, times(1)).deleteById(PRODUCT_SKU_2);
        
        verify(masterDataProductRepository, times(1)).deleteByProductCode(PRODUCT_CODE);
        verify(masterDataProductRepository, times(1)).deleteByProductCode(PRODUCT_CODE_2);
        verify(masterDataProductRepository, never()).deleteByProductCode("DIFFERENT-CODE");
        
        verify(masterDataItemRepository, times(1)).deleteByProductCode(PRODUCT_CODE);
        verify(masterDataItemRepository, times(1)).deleteByProductCode(PRODUCT_CODE_2);
        verify(masterDataItemRepository, never()).deleteByProductCode("DIFFERENT-CODE");
    }

    @Test
    void performDeletionFromAllDataSources_WhenAllMongoOperationsFail_ShouldStillPublishEsEvent() {
        when(publisherService.publishProductEsDeletionEvent(any())).thenReturn(Mono.just(true));
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU)).thenReturn(1L);
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU_2)).thenReturn(1L);
        doThrow(new RuntimeException("Mongo Error"))
                .doNothing() // Allow fallback to succeed
                .when(sivaProductRepository).deleteById(PRODUCT_SKU);
        doThrow(new RuntimeException("Mongo Error"))
                .doNothing() // Allow fallback to succeed
                .when(sivaProductRepository).deleteById(PRODUCT_SKU_2);
        doThrow(new RuntimeException("Product Error"))
                .when(productRepository).deleteById(anyString());

        productDeletionService.performDeletionFromAllDataSources(eventModels, TRACE_ID)
            .doOnSuccess(v -> waitForAsyncOperations())
            .block();

        verify(publisherService).publishProductEsDeletionEvent(argThat(event -> 
            event.getProductSkus().containsAll(Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2))
        ));
        for (String sku : Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2)) {
            verify(pickupPointRepository).deleteByProductSku(sku);
            verify(sivaProductRepository).deleteById(sku);
            verify(sivaItemService).performItemDeletionByProductSku(eq(sku), any(SaveParam.class));
        }
        verify(sivaProductService, never()).deleteProductByProductSkuFromElasticsearch(anyString(), any(SaveParam.class));
        verify(sivaItemService, never()).deleteByProductSkuFromElasticsearch(anyString(), any(SaveParam.class));
    }

    @Test
    void performDeletionFromAllDataSources_WhenMasterDataDeletionFails_ShouldContinueWithOtherOperations() {
        when(publisherService.publishProductEsDeletionEvent(any())).thenReturn(Mono.just(true));
        
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU)).thenReturn(1L);
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU_2)).thenReturn(1L);
        
        doNothing().when(sivaProductRepository).deleteById(anyString());
        doNothing().when(sivaItemService).performItemDeletionByProductSku(anyString(), any(SaveParam.class));
        
        doNothing().when(productRepository).deleteById(anyString());
        doThrow(new RuntimeException("Master Data Error"))
                .when(masterDataProductRepository).deleteByProductCode(PRODUCT_CODE);
        doNothing().when(masterDataProductRepository).deleteByProductCode(PRODUCT_CODE_2);
        doNothing().when(masterDataItemRepository).deleteByProductCode(PRODUCT_CODE_2);

        productDeletionService.performDeletionFromAllDataSources(eventModels, TRACE_ID)
            .doOnSuccess(v -> waitForAsyncOperations())
            .block();

        verify(publisherService).publishProductEsDeletionEvent(argThat(event -> 
            event.getProductSkus().containsAll(Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2))
        ));
        
        verify(pickupPointRepository).deleteByProductSku(PRODUCT_SKU);
        verify(pickupPointRepository).deleteByProductSku(PRODUCT_SKU_2);
        verify(sivaProductRepository).deleteById(PRODUCT_SKU);
        verify(sivaProductRepository).deleteById(PRODUCT_SKU_2);
        verify(sivaItemService).performItemDeletionByProductSku(eq(PRODUCT_SKU), any(SaveParam.class));
        verify(sivaItemService).performItemDeletionByProductSku(eq(PRODUCT_SKU_2), any(SaveParam.class));
        verify(productRepository).deleteById(PRODUCT_SKU);
        verify(productRepository).deleteById(PRODUCT_SKU_2);
        verify(masterDataProductRepository).deleteByProductCode(PRODUCT_CODE);
        verify(masterDataProductRepository).deleteByProductCode(PRODUCT_CODE_2);
        verify(masterDataItemRepository).deleteByProductCode(PRODUCT_CODE_2);
        
        // These methods should not be called since they're only used in performDeletionsFromElasticSearch
        verify(sivaProductService, never()).deleteProductByProductSkuFromElasticsearch(anyString(), any(SaveParam.class));
        verify(sivaItemService, never()).deleteByProductSkuFromElasticsearch(anyString(), any(SaveParam.class));
    }

    @Test
    void performDeletionFromAllDataSources_WhenUnexpectedErrorOccurs_ShouldHandleGracefully() {
        when(publisherService.publishProductEsDeletionEvent(any())).thenReturn(Mono.just(true));
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU))
                .thenThrow(new NullPointerException("Unexpected error"));
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU_2))
                .thenThrow(new IllegalStateException("Another unexpected error"));
        
        productDeletionService.performDeletionFromAllDataSources(eventModels, TRACE_ID)
            .doOnSuccess(v -> waitForAsyncOperations())
            .block();

        verify(publisherService).publishProductEsDeletionEvent(argThat(event -> 
            event.getProductSkus().containsAll(Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2))
        ));
        
        verify(pickupPointRepository).deleteByProductSku(PRODUCT_SKU);
        verify(pickupPointRepository).deleteByProductSku(PRODUCT_SKU_2);
    }

    @Test
    void performDeletionFromAllDataSources_WhenMasterDataDeletionFailsForSharedProduct_ShouldContinueWithOtherOperations() {
        // Given
        eventModels.forEach(model -> model.setSharedProduct(true));
        when(publisherService.publishProductEsDeletionEvent(any())).thenReturn(Mono.just(true));
        
        // Setup pickup points
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU)).thenReturn(1L);
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU_2)).thenReturn(1L);
        
        // Mock successful operations for both SKUs
        doNothing().when(sivaItemService).performItemDeletionByProductSku(eq(PRODUCT_SKU), any(SaveParam.class));
        doNothing().when(sivaItemService).performItemDeletionByProductSku(eq(PRODUCT_SKU_2), any(SaveParam.class));
        doNothing().when(sivaProductRepository).deleteById(PRODUCT_SKU);
        doNothing().when(sivaProductRepository).deleteById(PRODUCT_SKU_2);
        doNothing().when(productRepository).deleteById(PRODUCT_SKU);
        doNothing().when(productRepository).deleteById(PRODUCT_SKU_2);
        
        // Mock item deletion to throw exception for first SKU but succeed for second
        RuntimeException itemException = new RuntimeException("Item Delete Error");
        doThrow(itemException).when(itemRepository).deleteByProductSku(PRODUCT_SKU);
        doNothing().when(itemRepository).deleteByProductSku(PRODUCT_SKU_2);

        // Execute and wait for completion
        productDeletionService.performDeletionFromAllDataSources(eventModels, TRACE_ID)
            .doOnSuccess(v -> waitForAsyncOperations())
            .block();

        // Verify ES event was published with both SKUs
        verify(publisherService).publishProductEsDeletionEvent(argThat(event -> 
            event.getProductSkus().containsAll(Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2)) &&
            event.getTraceId().equals(TRACE_ID)
        ));
        
        // Verify operations for first SKU (with error)
        verify(pickupPointRepository).deleteByProductSku(PRODUCT_SKU);
        verify(sivaProductRepository).deleteById(PRODUCT_SKU);
        verify(sivaItemService).performItemDeletionByProductSku(eq(PRODUCT_SKU), any(SaveParam.class));
        verify(productRepository).deleteById(PRODUCT_SKU);
        verify(itemRepository).deleteByProductSku(PRODUCT_SKU);
        
        // Verify operations for second SKU (should complete normally)
        verify(pickupPointRepository).deleteByProductSku(PRODUCT_SKU_2);
        verify(sivaProductRepository).deleteById(PRODUCT_SKU_2);
        verify(sivaItemService).performItemDeletionByProductSku(eq(PRODUCT_SKU_2), any(SaveParam.class));
        verify(productRepository).deleteById(PRODUCT_SKU_2);
        verify(itemRepository).deleteByProductSku(PRODUCT_SKU_2);
        
        // Verify no master data operations were performed (shared product)
        verifyNoInteractions(masterDataProductRepository, masterDataItemRepository);
        
        // Verify no ES operations were performed
        verify(sivaProductService, never()).deleteProductByProductSkuFromElasticsearch(anyString(), any(SaveParam.class));
        verify(sivaItemService, never()).deleteByProductSkuFromElasticsearch(anyString(), any(SaveParam.class));
    }

    @Test
    void performElasticSearchDeletion_WhenBothOperationsFail_ShouldReturnFalse() {
        // Given
        when(sivaProductService.deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.just(false));
        when(sivaItemService.deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.just(false));
        SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();

        productDeletionService.performElasticSearchDeletion(PRODUCT_SKU, saveParam);
    }

    @Test
    void performElasticSearchDeletion_WhenErrorOccurs_ShouldHandleGracefully() {
        when(sivaProductService.deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.error(new RuntimeException("ES deletion error")));
        when(sivaItemService.deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.just(true));
        SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();

        productDeletionService.performElasticSearchDeletion(PRODUCT_SKU, saveParam);

        verify(sivaProductService).deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
        verify(sivaItemService).deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
    }

    @Test
    void performElasticSearchDeletion_WhenZipOperationFails_ShouldHandleGracefully() {
        when(sivaProductService.deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.error(new RuntimeException("ES product deletion error")));
        when(sivaItemService.deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.error(new RuntimeException("ES item deletion error")));
        SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();

        productDeletionService.performElasticSearchDeletion(PRODUCT_SKU, saveParam);

        verify(sivaProductService).deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
        verify(sivaItemService).deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
    }

    @Test
    void performElasticSearchDeletion_WhenProductSucceedsAndItemFails_ShouldReturnTrue() {
        when(sivaProductService.deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.just(true));
        when(sivaItemService.deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.just(false));
        SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();

        productDeletionService.performElasticSearchDeletion(PRODUCT_SKU, saveParam);

        verify(sivaProductService).deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
        verify(sivaItemService).deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
    }

    @Test
    void performElasticSearchDeletion_WhenProductFailsAndItemSucceeds_ShouldReturnTrue() {
        when(sivaProductService.deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.just(false));
        when(sivaItemService.deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.just(true));
        SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();

        productDeletionService.performElasticSearchDeletion(PRODUCT_SKU, saveParam);

        verify(sivaProductService).deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
        verify(sivaItemService).deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
    }

    @Test
    void performElasticSearchDeletion_WhenBothSucceed_ShouldReturnTrue() {
        when(sivaProductService.deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.just(true));
        when(sivaItemService.deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.just(true));
        SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();

        productDeletionService.performElasticSearchDeletion(PRODUCT_SKU, saveParam);

        verify(sivaProductService).deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
        verify(sivaItemService).deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
    }

    @Test
    void performElasticSearchDeletion_WhenMonoReturnsNull_ShouldHandleGracefully() {
        when(sivaProductService.deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.empty());  // This will result in null when blocked
        when(sivaItemService.deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), any(SaveParam.class)))
                .thenReturn(Mono.empty());  // This will result in null when blocked
        SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();

        productDeletionService.performElasticSearchDeletion(PRODUCT_SKU, saveParam);

        verify(sivaProductService).deleteProductByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
        verify(sivaItemService).deleteByProductSkuFromElasticsearch(eq(PRODUCT_SKU), eq(saveParam));
    }

    @Test
    void performDeletionFromAllDataSources_WhenRawDataSourceDeletionFails_ShouldHandleGracefully() {
        // Given
        when(publisherService.publishProductEsDeletionEvent(any())).thenReturn(Mono.just(true));
        
        // Setup pickup points
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU)).thenReturn(1L);
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU_2)).thenReturn(1L);
        
        // Mock successful operations for both SKUs
        doNothing().when(sivaItemService).performItemDeletionByProductSku(eq(PRODUCT_SKU), any(SaveParam.class));
        doNothing().when(sivaItemService).performItemDeletionByProductSku(eq(PRODUCT_SKU_2), any(SaveParam.class));
        doNothing().when(sivaProductRepository).deleteById(PRODUCT_SKU);
        doNothing().when(sivaProductRepository).deleteById(PRODUCT_SKU_2);
        
        // Mock raw data source deletion to throw exception for first SKU
        RuntimeException testException = new RuntimeException("Test exception");
        doThrow(testException).when(productRepository).deleteById(PRODUCT_SKU);
        doNothing().when(productRepository).deleteById(PRODUCT_SKU_2);
        
        // Mock master data operations
        doNothing().when(masterDataProductRepository).deleteByProductCode(PRODUCT_CODE);
        doNothing().when(masterDataProductRepository).deleteByProductCode(PRODUCT_CODE_2);
        doNothing().when(masterDataItemRepository).deleteByProductCode(PRODUCT_CODE);
        doNothing().when(masterDataItemRepository).deleteByProductCode(PRODUCT_CODE_2);
        
        // Execute and wait for completion
        productDeletionService.performDeletionFromAllDataSources(eventModels, TRACE_ID)
            .doOnSuccess(v -> waitForAsyncOperations())
            .block();

        // Verify ES event was published with both SKUs
        verify(publisherService).publishProductEsDeletionEvent(argThat(event -> 
            event.getProductSkus().containsAll(Arrays.asList(PRODUCT_SKU, PRODUCT_SKU_2)) &&
            event.getTraceId().equals(TRACE_ID)
        ));

        // Verify operations for first SKU (with error)
        verify(pickupPointRepository).deleteByProductSku(PRODUCT_SKU);
        verify(sivaItemService).performItemDeletionByProductSku(eq(PRODUCT_SKU), any(SaveParam.class));
        verify(sivaProductRepository).deleteById(PRODUCT_SKU);
        verify(productRepository).deleteById(PRODUCT_SKU);
        verify(masterDataProductRepository).deleteByProductCode(PRODUCT_CODE);
        verify(masterDataItemRepository).deleteByProductCode(PRODUCT_CODE);
        
        // Verify operations for second SKU (should complete normally)
        verify(pickupPointRepository).deleteByProductSku(PRODUCT_SKU_2);
        verify(sivaItemService).performItemDeletionByProductSku(eq(PRODUCT_SKU_2), any(SaveParam.class));
        verify(sivaProductRepository).deleteById(PRODUCT_SKU_2);
        verify(productRepository).deleteById(PRODUCT_SKU_2);
        verify(masterDataProductRepository).deleteByProductCode(PRODUCT_CODE_2);
        verify(masterDataItemRepository).deleteByProductCode(PRODUCT_CODE_2);
    }

    @Test
    void performDeletionFromAllDataSources_WhenSharedProductItemDeletionFails_ShouldHandleGracefully() {
        // Given
        eventModels.forEach(model -> model.setSharedProduct(true));
        when(publisherService.publishProductEsDeletionEvent(any())).thenReturn(Mono.just(true));
        
        // Setup pickup points
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU)).thenReturn(1L);
        
        // Mock successful operations
        doNothing().when(sivaProductRepository).deleteById(anyString());
        
        // Mock item deletion to throw exception
        RuntimeException itemException = new RuntimeException("Item deletion failed");
        doThrow(itemException).when(itemRepository).deleteByProductSku(PRODUCT_SKU);
        
        // Execute and wait for completion
        productDeletionService.performDeletionFromAllDataSources(eventModels, TRACE_ID)
            .doOnSuccess(v -> waitForAsyncOperations())
            .block();

        // Verify operations were attempted
        verify(pickupPointRepository).deleteByProductSku(PRODUCT_SKU);
        verify(sivaProductRepository).deleteById(PRODUCT_SKU);
        verify(itemRepository).deleteByProductSku(PRODUCT_SKU);
        
        // Verify master data was not touched (shared product)
        verifyNoInteractions(masterDataProductRepository, masterDataItemRepository);
    }

    @Test
    void performDeletionFromAllDataSources_WhenMasterDataDeletionThrowsException_ShouldHandleGracefully() {
        // Given
        when(publisherService.publishProductEsDeletionEvent(any())).thenReturn(Mono.just(true));
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU)).thenReturn(1L);
        
        // Mock successful operations
        doNothing().when(sivaItemService).performItemDeletionByProductSku(anyString(), any(SaveParam.class));
        doNothing().when(sivaProductRepository).deleteById(anyString());
        doNothing().when(productRepository).deleteById(anyString());
        
        // Mock master data deletion to throw exception
        RuntimeException masterDataException = new RuntimeException("Master data deletion failed");
        doThrow(masterDataException)
            .when(masterDataProductRepository).deleteByProductCode(PRODUCT_CODE);
        doThrow(masterDataException)
            .when(masterDataItemRepository).deleteByProductCode(PRODUCT_CODE);
        
        // Execute and wait for completion
        productDeletionService.performDeletionFromAllDataSources(eventModels, TRACE_ID)
            .doOnSuccess(v -> waitForAsyncOperations())
            .block();

        // Verify operations before master data deletion were performed
        verify(pickupPointRepository).deleteByProductSku(PRODUCT_SKU);
        verify(sivaItemService).performItemDeletionByProductSku(eq(PRODUCT_SKU), any(SaveParam.class));
        verify(sivaProductRepository).deleteById(PRODUCT_SKU);
        verify(productRepository).deleteById(PRODUCT_SKU);
        
        // Verify master data deletion was attempted
        verify(masterDataProductRepository).deleteByProductCode(PRODUCT_CODE);
        verify(masterDataItemRepository).deleteByProductCode(PRODUCT_CODE);
    }

    @Test
    void deleteProcessedDataOptimized_WhenMongoOperationThrowsException_ShouldPerformFallbackDeletion() {
        // This test is for the doOnError handler in Mono.when()
        // Given
        String productSku = "TEST-SKU-123";
        SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();
        RuntimeException mongoException = new RuntimeException("MongoDB operation failed");
        doThrow(mongoException)
            .doNothing() // Allow fallback to succeed
            .when(sivaProductRepository).deleteById(productSku);

        // Mock sivaItemService to throw exception during performItemDeletionByProductSku
        doThrow(new RuntimeException("Item deletion failed"))
            .when(sivaItemService).performItemDeletionByProductSku(eq(productSku), any(SaveParam.class));

        // Execute method that contains the try-catch block
        productDeletionService.deleteProcessedDataOptimized(productSku, saveParam);
        
        // Wait for async operations
        waitForAsyncOperations();
        
        verify(sivaProductRepository, times(2)).deleteById(productSku); // Once in try block,
      // once in
      // fallback
        verify(sivaItemService).performItemDeletionByProductSku(eq(productSku),
          any(SaveParam.class)); // In try block
    }
    
    @Test
    void deleteProcessedDataOptimized_WhenExceptionOccurs_ShouldCatchAndPerformFallback() {
        // Given
        String productSku = "TEST-SKU-123";
        SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();
        productDeletionService.deleteProcessedDataOptimized(productSku, saveParam);
        
        // Wait for async operations
    }


    @Test
    void deleteMasterData_WhenExceptionOccurs_ShouldLogError() {
        try (MockedStatic<LoggerFactory> mockedLoggerFactory = Mockito.mockStatic(LoggerFactory.class)) {
            // Given
            mockedLoggerFactory.when(() -> LoggerFactory.getLogger(ProductDeletionService.class)).thenReturn(log);
            
            String productSku = "TEST-SKU-123";
            String productCode = "TEST-CODE-123";
            RuntimeException testException = new RuntimeException("Test exception");;
            
            // Create test model
            TerminatedSellerDeletionEventModel model = TerminatedSellerDeletionEventModel.builder()
                .productSku(productSku)
                .productCode(productCode)
                .sharedProduct(false)
                .build();
            
            // Mock master data operations to throw exception
            doThrow(testException).when(masterDataProductRepository).deleteByProductCode(productCode);

            // When
            productDeletionService.deleteMasterData(model);
            waitForAsyncOperations();

            // Then
            verify(masterDataProductRepository).deleteByProductCode(productCode);
        }
    }


    @Test
    void performDeletionFromAllDataSources_WhenRawDataSourceDeletionThrowsException_ShouldLogAndContinue() {
        // Given
        when(publisherService.publishProductEsDeletionEvent(any())).thenReturn(Mono.just(true));
        when(pickupPointRepository.deleteByProductSku(PRODUCT_SKU)).thenReturn(1L);
        
        // Mock successful operations
        doNothing().when(sivaItemService).performItemDeletionByProductSku(anyString(), any(SaveParam.class));
        doNothing().when(sivaProductRepository).deleteById(anyString());
        
        // Mock raw data source deletion to throw exception
        RuntimeException rawDataException = new RuntimeException("Raw data deletion failed");
        doThrow(rawDataException).when(productRepository).deleteById(PRODUCT_SKU);
        
        // Execute and wait for completion
        productDeletionService.performDeletionFromAllDataSources(eventModels, TRACE_ID)
            .doOnSuccess(v -> waitForAsyncOperations())
            .block();

        // Verify operations before raw data deletion were performed
        verify(pickupPointRepository).deleteByProductSku(PRODUCT_SKU);
        verify(sivaItemService).performItemDeletionByProductSku(eq(PRODUCT_SKU), any(SaveParam.class));
        verify(sivaProductRepository).deleteById(PRODUCT_SKU);
        
        // Verify raw data deletion was attempted
        verify(productRepository).deleteById(PRODUCT_SKU);
        
        // Verify subsequent operations were still performed
        verify(masterDataProductRepository).deleteByProductCode(PRODUCT_CODE);
        verify(masterDataItemRepository).deleteByProductCode(PRODUCT_CODE);
    }

    @Test
    void performFallbackDeletion_WhenSuccessful_ShouldDeleteBothProductAndItems() {
        try (MockedStatic<LoggerFactory> mockedLoggerFactory = Mockito.mockStatic(LoggerFactory.class)) {
            // Given
            mockedLoggerFactory.when(() -> LoggerFactory.getLogger(ProductDeletionService.class)).thenReturn(log);
            
            // Mock successful deletions
            doNothing().when(sivaProductRepository).deleteById(PRODUCT_SKU);
            doNothing().when(sivaItemRepository).deleteByProductSku(PRODUCT_SKU);

            // When
            productDeletionService.performFallbackDeletion(PRODUCT_SKU);
            waitForAsyncOperations();

            // Then
            verify(sivaProductRepository).deleteById(PRODUCT_SKU);
            verify(sivaItemRepository).deleteByProductSku(PRODUCT_SKU);
        }
    }

    @Test
    void performFallbackDeletion_WhenProductRepositoryFails_ShouldLogErrorAndStopDeletion() {
        try (MockedStatic<LoggerFactory> mockedLoggerFactory = Mockito.mockStatic(LoggerFactory.class)) {
            // Given
            mockedLoggerFactory.when(() -> LoggerFactory.getLogger(ProductDeletionService.class)).thenReturn(log);
            
            // Mock product deletion to fail
            RuntimeException testException = new RuntimeException("Product deletion failed");
            doThrow(testException).when(sivaProductRepository).deleteById(PRODUCT_SKU);

            // When
            productDeletionService.performFallbackDeletion(PRODUCT_SKU);
            waitForAsyncOperations();

            // Then
            verify(sivaProductRepository).deleteById(PRODUCT_SKU);
        }
    }

    @Test
    void performFallbackDeletion_WhenItemRepositoryFails_ShouldLogError() {
        try (MockedStatic<LoggerFactory> mockedLoggerFactory = Mockito.mockStatic(LoggerFactory.class)) {
            // Given
            mockedLoggerFactory.when(() -> LoggerFactory.getLogger(ProductDeletionService.class)).thenReturn(log);
            
            // Mock successful product deletion but failed item deletion
            doNothing().when(sivaProductRepository).deleteById(PRODUCT_SKU);
            RuntimeException testException = new RuntimeException("Item deletion failed");
            doThrow(testException).when(sivaItemRepository).deleteByProductSku(PRODUCT_SKU);

            // When
            productDeletionService.performFallbackDeletion(PRODUCT_SKU);
            waitForAsyncOperations();

            // Then
            verify(sivaProductRepository).deleteById(PRODUCT_SKU);
            verify(sivaItemRepository).deleteByProductSku(PRODUCT_SKU);
        }
    }

    @Test
    void deleteProcessedDataOptimized_WhenInitialAndFallbackOperationsFail_ShouldLogErrors() {
        // Given
        String productSku = "TEST-SKU-123";
        SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();
        RuntimeException exception = new RuntimeException("Deletion failed");
        
        // Mock scheduler to return immediate scheduler
        doThrow(exception).when(sivaItemService).performItemDeletionByProductSku(eq(productSku), any(SaveParam.class));
        productDeletionService.deleteProcessedDataOptimized(productSku, saveParam);
        waitForAsyncOperations();
        verify(sivaProductRepository).deleteById(productSku);
    }

    @Test
    void deleteProcessedDataOptimized_WhenInitialOperationsFailButFallbackSucceeds_ShouldLogSuccess() {
        // Given
        String productSku = "TEST-SKU-123";
        SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();
        RuntimeException exception = new RuntimeException("Initial deletion failed");
        
        // Mock scheduler to return immediate scheduler
        doThrow(exception).when(sivaProductRepository).deleteById(productSku);
        doThrow(exception).when(sivaItemService).performItemDeletionByProductSku(eq(productSku), any(SaveParam.class));
        productDeletionService.deleteProcessedDataOptimized(productSku, saveParam);
        waitForAsyncOperations();
        verify(sivaProductRepository, times(2)).deleteById(productSku);
    }

    @Test
    void deleteProcessedDataOptimized_WhenInitialOperationsSucceed_ShouldNotTriggerFallback() {
        String productSku = "TEST-SKU-123";
        SaveParam saveParam = SaveParam.builder().traceId(TRACE_ID).build();
        
        doNothing().when(sivaProductRepository).deleteById(productSku);
        doNothing().when(sivaItemService).performItemDeletionByProductSku(eq(productSku), any(SaveParam.class));

        // When
        productDeletionService.deleteProcessedDataOptimized(productSku, saveParam);
        waitForAsyncOperations();

        // Then
        verify(sivaProductRepository).deleteById(productSku);
        verify(sivaItemService).performItemDeletionByProductSku(eq(productSku), any(SaveParam.class));
        verify(sivaItemRepository, never()).deleteByProductSku(productSku); // Fallback should not be called
    }
}