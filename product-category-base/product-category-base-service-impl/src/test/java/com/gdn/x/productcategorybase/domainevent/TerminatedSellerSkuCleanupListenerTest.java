package com.gdn.x.productcategorybase.domainevent;

import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

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
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.domain.event.model.TerminatedSellerSkuCleanupEventModel;
import com.gdn.x.productcategorybase.dto.TerminatedSellerSkuCleanupStatusDTO;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.enums.TerminatedSellerSkuStatus;
import com.gdn.x.productcategorybase.service.ProductDeletionService;
import com.gdn.x.productcategorybase.service.ProductDeletionWrapperService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;

public class TerminatedSellerSkuCleanupListenerTest {

  @InjectMocks
  private TerminatedSellerSkuCleanupListener terminatedSellerSkuCleanupListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductDeletionService productDeletionService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ProductDeletionWrapperService productDeletionWrapperService;

  @Captor
  private ArgumentCaptor<TerminatedSellerSkuCleanupStatusDTO> statusDtoCaptor;

  private TerminatedSellerSkuCleanupEventModel terminatedSellerSkuCleanupEventModel;
  private TerminatedSellerSkuCleanupStatusDTO statusDTO;
  private Product product;

  private static final String PRODUCT_CODE = "productCode";
  private static final String SELLER_CODE = "sellerCode";
  private static final String MESSAGE = "message";

  @BeforeEach
  public void init() throws Exception {
    initMocks(this);
    terminatedSellerSkuCleanupEventModel = new TerminatedSellerSkuCleanupEventModel();
    terminatedSellerSkuCleanupEventModel.setSellerCode(SELLER_CODE);
    terminatedSellerSkuCleanupEventModel.setProductCode(PRODUCT_CODE);

    product = new Product();
    product.setProductCode(PRODUCT_CODE);

    statusDTO = new TerminatedSellerSkuCleanupStatusDTO();
    statusDTO.setProductCode(PRODUCT_CODE);
    statusDTO.setSellerCode(SELLER_CODE);
    statusDTO.setResult(TerminatedSellerSkuStatus.SUCCESS.name());
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(productDeletionService);
    verifyNoMoreInteractions(productDeletionWrapperService);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    statusDTO.setResult(TerminatedSellerSkuStatus.FAILED.name());
    statusDTO.setSellerCode(SELLER_CODE);
    terminatedSellerSkuCleanupEventModel.setSellerCode(SELLER_CODE);
    Mockito.when(objectMapper.readValue(MESSAGE, TerminatedSellerSkuCleanupEventModel.class))
        .thenReturn(terminatedSellerSkuCleanupEventModel);
    Mockito.when(productDeletionWrapperService.getProductDetails(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
        .thenReturn(product);
    Mockito.when(productDeletionService.pickedForDeletion(product)).thenReturn(false);
    Mockito.when(productDeletionService.isAllowedToDeleteImage(product.getProductItems())).thenReturn(true);
    terminatedSellerSkuCleanupListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, TerminatedSellerSkuCleanupEventModel.class);
    Mockito.verify(kafkaTopicProperties).getTerminatedSellerSkuCleanup();
    Mockito.verify(productDeletionWrapperService).getProductDetails(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productDeletionService).pickedForDeletion(product);
    Mockito.verify(productDeletionService).updatePickedForDeletionFlag(product, Boolean.TRUE);
    Mockito.verify(productDeletionService).isAllowedToDeleteImage(product.getProductItems());
    Mockito.verify(productDeletionWrapperService).hardDeleteProductAndClearCache(product.getStoreId(), product);
    Mockito.verify(productDeletionService).publishEventToUpdateStatusAndDeleteImage(statusDtoCaptor.capture());
    Assertions.assertTrue(statusDtoCaptor.getValue().isPublishImageDeletionEvent());
    Assertions.assertEquals(statusDtoCaptor.getValue().getResult(), TerminatedSellerSkuStatus.SUCCESS.name());
  }

  @Test
  public void onDomainEventConsumedDoPickedForDeletionTest() throws Exception {
    statusDTO.setResult(TerminatedSellerSkuStatus.FAILED.name());
    statusDTO.setSellerCode(SELLER_CODE);
    terminatedSellerSkuCleanupEventModel.setSellerCode(SELLER_CODE);
    Mockito.when(objectMapper.readValue(MESSAGE, TerminatedSellerSkuCleanupEventModel.class))
        .thenReturn(terminatedSellerSkuCleanupEventModel);
    Mockito.when(productDeletionWrapperService.getProductDetails(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
        .thenReturn(product);
    Mockito.when(productDeletionService.pickedForDeletion(product)).thenReturn(true);
    terminatedSellerSkuCleanupListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, TerminatedSellerSkuCleanupEventModel.class);
    Mockito.verify(kafkaTopicProperties).getTerminatedSellerSkuCleanup();
    Mockito.verify(productDeletionWrapperService).getProductDetails(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productDeletionService).pickedForDeletion(product);
  }

  @Test
  public void onDomainEventConsumedProductNullTest() throws Exception {
    statusDTO.setResult(TerminatedSellerSkuStatus.FAILED.name());
    statusDTO.setSellerCode(SELLER_CODE);
    terminatedSellerSkuCleanupEventModel.setSellerCode(SELLER_CODE);
    Mockito.when(objectMapper.readValue(MESSAGE, TerminatedSellerSkuCleanupEventModel.class))
        .thenReturn(terminatedSellerSkuCleanupEventModel);
    terminatedSellerSkuCleanupListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, TerminatedSellerSkuCleanupEventModel.class);
    Mockito.verify(kafkaTopicProperties).getTerminatedSellerSkuCleanup();
    Mockito.verify(productDeletionWrapperService).getProductDetails(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(productDeletionService).publishEventToUpdateStatusAndDeleteImage(statusDtoCaptor.capture());
    Assertions.assertFalse(statusDtoCaptor.getValue().isPublishImageDeletionEvent());
    Assertions.assertEquals(statusDtoCaptor.getValue().getResult(), TerminatedSellerSkuStatus.SUCCESS.name());
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    statusDTO.setResult(TerminatedSellerSkuStatus.FAILED.name());
    statusDTO.setSellerCode(null);
    terminatedSellerSkuCleanupEventModel.setSellerCode(null);
    Mockito.when(objectMapper.readValue(MESSAGE, TerminatedSellerSkuCleanupEventModel.class))
        .thenReturn(terminatedSellerSkuCleanupEventModel);
    terminatedSellerSkuCleanupListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, TerminatedSellerSkuCleanupEventModel.class);
    Mockito.verify(kafkaTopicProperties).getTerminatedSellerSkuCleanup();
    Mockito.verify(productDeletionService).publishEventToUpdateStatusAndDeleteImage(statusDtoCaptor.capture());
    Assertions.assertFalse(statusDtoCaptor.getValue().isPublishImageDeletionEvent());
    Assertions.assertEquals(statusDtoCaptor.getValue().getResult(), TerminatedSellerSkuStatus.FAILED.name());
  }

}
