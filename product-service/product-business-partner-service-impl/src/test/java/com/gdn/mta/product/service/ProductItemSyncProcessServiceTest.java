package com.gdn.mta.product.service;

import java.util.Arrays;
import java.util.List;

import com.gdn.mta.product.entity.ProductItemSyncProcess;
import com.gdn.mta.product.repository.ProductItemSyncProcessRepository;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class ProductItemSyncProcessServiceTest {

  private static final String STORE_ID = "10001";

  @Mock
  private ProductItemSyncProcessRepository syncProcessRepository;

  @InjectMocks
  private ProductItemSyncProcessServiceImpl service;

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(syncProcessRepository);
  }

  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void save() {
    ProductItemSyncProcess syncProcess = new ProductItemSyncProcess();

    service.save(syncProcess);

    Mockito.verify(syncProcessRepository).save(syncProcess);
  }

  @Test
  public void findAllProcessEligibleForNotification() {

    ProductItemSyncProcess syncProcess = new ProductItemSyncProcess();

    Mockito.when(syncProcessRepository.findByStoreIdAndIsUserNotifiedFalseAndMarkForDeleteFalse(STORE_ID))
      .thenReturn(Arrays.asList(syncProcess));

    List<ProductItemSyncProcess> syncProcessList = service.findAllProcessEligibleForNotification(STORE_ID);

    Assertions.assertEquals(syncProcess, syncProcessList.get(0));

    Mockito.verify(syncProcessRepository).findByStoreIdAndIsUserNotifiedFalseAndMarkForDeleteFalse(STORE_ID);
  }

}