package com.gdn.x.mta.distributiontask.service.impl;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.mta.domain.event.modal.ProductActionRetryEvent;
import com.gdn.x.mta.distributiontask.dao.api.ProductActionRetryRepository;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.ProductActionRetry;
import com.gdn.x.mta.distributiontask.model.enums.ActionRetryStatus;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class ProductActionRetryServiceTest {

  @InjectMocks
  private ProductActionRetryServiceImpl productActionRetryService;

  @Mock
  private ProductActionRetryRepository productActionRetryRepository;

  @Captor
  private ArgumentCaptor<ProductActionRetry> productActionRetryArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ProductActionRetry>> productActionRetryListArgumentCaptor;

  private static final String STORE_ID = "storeId";
  private static final String PRODUCT_CODE = "productCode";
  private static final String ACTION = "action";
  private static final Integer MAX_ALLOWED_PRODUCT_FOR_ACTION_RETRY = 100;

  private List<ProductActionRetry> productActionRetryList;
  private ProductActionRetry productActionRetry;
  private ProductActionRetryEvent productActionRetryEvent;

  @BeforeEach
  public void setUp() throws Exception {
    productActionRetryList = new ArrayList<>();
    productActionRetry = new ProductActionRetry();
    productActionRetry.setProductCode(PRODUCT_CODE);
    productActionRetry.setMarkForDelete(true);
    productActionRetry.setRetryCount(1);
    productActionRetryList.add(productActionRetry);

    productActionRetryEvent = new ProductActionRetryEvent();
    productActionRetryEvent.setProductCode(PRODUCT_CODE);
    productActionRetryEvent.setStoreId(STORE_ID);
    productActionRetryEvent.setAction(Constants.AUTO_NEED_REVISION);
    productActionRetryEvent.setData(ACTION);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productActionRetryRepository);
  }

  @Test
   void findProductsToRetryActionOrderByCreatedDateAscTest() {
    Mockito.when(productActionRetryRepository
        .findByStoreIdAndActionAndStatusOrderByCreatedDateAsc(eq(STORE_ID), eq(ACTION), eq(ActionRetryStatus.PENDING),
            any())).thenReturn(productActionRetryList);
    List<ProductActionRetry> productActionRetrys = productActionRetryService
        .findProductsToRetryActionOrderByCreatedDateAsc(STORE_ID, ACTION, ActionRetryStatus.PENDING,
            MAX_ALLOWED_PRODUCT_FOR_ACTION_RETRY);
    Mockito.verify(productActionRetryRepository)
        .findByStoreIdAndActionAndStatusOrderByCreatedDateAsc(eq(STORE_ID), eq(ACTION), eq(ActionRetryStatus.PENDING),
            any());
    Assertions.assertEquals(productActionRetryList, productActionRetrys);
  }

  @Test
   void findProductsToRetryActionOrderByCreatedDateDescTest() {
    Mockito.when(productActionRetryRepository
        .findByStoreIdAndActionAndStatusOrderByCreatedDateDesc(eq(STORE_ID), eq(ACTION), eq(ActionRetryStatus.PENDING),
            any())).thenReturn(productActionRetryList);
    List<ProductActionRetry> productActionRetrys = productActionRetryService
        .findProductsToRetryActionOrderByCreatedDateDesc(STORE_ID, ACTION, ActionRetryStatus.PENDING,
            MAX_ALLOWED_PRODUCT_FOR_ACTION_RETRY);
    Mockito.verify(productActionRetryRepository)
        .findByStoreIdAndActionAndStatusOrderByCreatedDateDesc(eq(STORE_ID), eq(ACTION), eq(ActionRetryStatus.PENDING),
            any());
    Assertions.assertEquals(productActionRetryList, productActionRetrys);
  }

  @Test
   void findProductsToRetryActionOrderByUpdatedDateAscTest() {
    Mockito.when(productActionRetryRepository
        .findByStoreIdAndActionAndStatusOrderByUpdatedDateAsc(eq(STORE_ID), eq(ACTION), eq(ActionRetryStatus.PENDING),
            any())).thenReturn(productActionRetryList);
    List<ProductActionRetry> productActionRetrys = productActionRetryService
        .findProductsToRetryActionOrderByUpdatedDateAsc(STORE_ID, ACTION, ActionRetryStatus.PENDING,
            MAX_ALLOWED_PRODUCT_FOR_ACTION_RETRY);
    Mockito.verify(productActionRetryRepository)
        .findByStoreIdAndActionAndStatusOrderByUpdatedDateAsc(eq(STORE_ID), eq(ACTION), eq(ActionRetryStatus.PENDING),
            any());
    Assertions.assertEquals(productActionRetryList, productActionRetrys);
  }

  @Test
   void findProductsToRetryActionOrderByUpdatedDateDescTest() {
    Mockito.when(productActionRetryRepository
        .findByStoreIdAndActionAndStatusOrderByUpdatedDateDesc(eq(STORE_ID), eq(ACTION), eq(ActionRetryStatus.PENDING),
            any())).thenReturn(productActionRetryList);
    List<ProductActionRetry> productActionRetrys = productActionRetryService
        .findProductsToRetryActionOrderByUpdatedDateDesc(STORE_ID, ACTION, ActionRetryStatus.PENDING,
            MAX_ALLOWED_PRODUCT_FOR_ACTION_RETRY);
    Mockito.verify(productActionRetryRepository)
        .findByStoreIdAndActionAndStatusOrderByUpdatedDateDesc(eq(STORE_ID), eq(ACTION), eq(ActionRetryStatus.PENDING),
            any());
    Assertions.assertEquals(productActionRetryList, productActionRetrys);
  }

  @Test
   void updateProductActionRetryDetailsStatusPendingTest() {
    productActionRetry.setStatus(ActionRetryStatus.PENDING);
    productActionRetryService.updateProductActionRetryDetails(productActionRetry);
    Mockito.verify(productActionRetryRepository).save(any(ProductActionRetry.class));
  }

  @Test
   void updateProductActionRetryDetailsStatusSuccessTest() {
    productActionRetry.setStatus(ActionRetryStatus.SUCCESS);
    productActionRetryService.updateProductActionRetryDetails(productActionRetry);
    Mockito.verify(productActionRetryRepository).save(any(ProductActionRetry.class));
  }

  @Test
   void getProductActionRetryByProductCodeAndActionTest() {
    Mockito.when(productActionRetryRepository.findByStoreIdAndProductCodeAndAction(STORE_ID, PRODUCT_CODE, ACTION))
        .thenReturn(productActionRetry);
    productActionRetryService.getProductActionRetryByProductCodeAndAction(STORE_ID, PRODUCT_CODE, ACTION);
    Mockito.verify(productActionRetryRepository).findByStoreIdAndProductCodeAndAction(STORE_ID, PRODUCT_CODE, ACTION);
  }

  @Test
   void upsertProductActionRetryNullTest() {
    productActionRetryService.upsertProductActionRetry(productActionRetryEvent);
    Mockito.verify(productActionRetryRepository)
        .findByStoreIdAndProductCodeAndAction(STORE_ID, PRODUCT_CODE, Constants.AUTO_NEED_REVISION);
    Mockito.verify(productActionRetryRepository).save(productActionRetryArgumentCaptor.capture());
    Assertions.assertFalse(productActionRetryArgumentCaptor.getValue().isMarkForDelete());
    Assertions.assertEquals(PRODUCT_CODE,
        productActionRetryArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(0, productActionRetryArgumentCaptor.getValue().getRetryCount());
    Assertions.assertEquals(ActionRetryStatus.PENDING,
        productActionRetryArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(ACTION, productActionRetryArgumentCaptor.getValue().getData());
  }

  @Test
   void upsertProductActionRetryTest() {
    Mockito.when(productActionRetryRepository
        .findByStoreIdAndProductCodeAndAction(STORE_ID, PRODUCT_CODE, Constants.AUTO_NEED_REVISION))
        .thenReturn(productActionRetry);
    productActionRetryService.upsertProductActionRetry(productActionRetryEvent);
    Mockito.verify(productActionRetryRepository)
        .findByStoreIdAndProductCodeAndAction(STORE_ID, PRODUCT_CODE, Constants.AUTO_NEED_REVISION);
    Mockito.verify(productActionRetryRepository).save(productActionRetryArgumentCaptor.capture());
    Assertions.assertFalse(productActionRetryArgumentCaptor.getValue().isMarkForDelete());
    Assertions.assertEquals(PRODUCT_CODE,
        productActionRetryArgumentCaptor.getValue().getProductCode());
    Assertions.assertEquals(0, productActionRetryArgumentCaptor.getValue().getRetryCount());
    Assertions.assertEquals(ActionRetryStatus.PENDING,
        productActionRetryArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(ACTION, productActionRetryArgumentCaptor.getValue().getData());
  }

  @Test
   void saveProductActionRetryListTest() {
    productActionRetryService.saveProductActionRetryList(productActionRetryList);
    Mockito.verify(productActionRetryRepository)
        .saveAll(productActionRetryListArgumentCaptor.capture());
    Assertions.assertEquals(productActionRetryListArgumentCaptor.getValue().size(), 1);
    Assertions.assertEquals(productActionRetryListArgumentCaptor.getValue().get(0).getProductCode(),
        PRODUCT_CODE);
    Assertions.assertEquals(productActionRetryListArgumentCaptor.getValue().get(0).getRetryCount(),
        1);
  }
}
