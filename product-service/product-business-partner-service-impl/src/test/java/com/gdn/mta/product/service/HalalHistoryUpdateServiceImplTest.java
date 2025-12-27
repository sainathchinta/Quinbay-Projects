package com.gdn.mta.product.service;

import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;


import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.HalalProductHistory;
import com.gdn.mta.product.repository.HalalProductHistoryRepository;
import com.gdn.x.product.domain.event.model.HalalHistoryUpdateEventModel;

public class HalalHistoryUpdateServiceImplTest {

  @InjectMocks
  HalaHistoryUpdateServiceImpl halaHistoryUpdateService;

  @Mock
  HalalProductHistoryRepository halalProductHistoryRepository;

  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static PageRequest pageRequest;
  private static final String STORE_ID = "storeId";
  private static final String PRODUCT_SKU = "productSku";
  private static final String ACTIVITY = "activity";
  private static final String PREVIOUS_VALUE = "previousValue";
  private static final String CURRENT_VALUE = "currentValue";
  private static final String CREATED_BY = "createdBy";
  private static final String USER_NAME = "userName";
  private static HalalProductHistory halalProductHistory;
  private HalalHistoryUpdateEventModel halalHistoryUpdateEventModel;

  @BeforeEach
  public void init() {
    initMocks(this);
    pageRequest = PageRequest.of(PAGE, SIZE);
    halalProductHistory = new HalalProductHistory();
    halalProductHistory.setProductSku(PRODUCT_SKU);
    halalProductHistory.setActivity(ACTIVITY);
    halalProductHistory.setPreviousValue(PREVIOUS_VALUE);
    halalProductHistory.setCurrentValue(CURRENT_VALUE);
    halalProductHistory.setCreatedBy(CREATED_BY);

    halalHistoryUpdateEventModel = new HalalHistoryUpdateEventModel();
    halalHistoryUpdateEventModel.setProductSku(PRODUCT_SKU);
    halalHistoryUpdateEventModel.setStoreId(STORE_ID);
    halalHistoryUpdateEventModel.setUserName(USER_NAME);
    halalHistoryUpdateEventModel.setActivity(ACTIVITY);
    halalHistoryUpdateEventModel.setCurrentValue(CURRENT_VALUE);
    halalHistoryUpdateEventModel.setPreviousValue(PREVIOUS_VALUE);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(halalProductHistoryRepository);
  }

  @Test
  public void getHalaProductHistoryTest() throws Exception {
    Page<HalalProductHistory> halalProductHistories = new PageImpl<>(List.of(halalProductHistory), pageRequest, 1);
    Mockito.when(
        halalProductHistoryRepository.findByStoreIdAndProductSkuOrderByCreatedDateDesc(STORE_ID,
            PRODUCT_SKU, pageRequest)).thenReturn(halalProductHistories);
    halaHistoryUpdateService.getHalalProductHistory(STORE_ID, PRODUCT_SKU, pageRequest);
    Mockito.verify(halalProductHistoryRepository)
        .findByStoreIdAndProductSkuOrderByCreatedDateDesc(STORE_ID, PRODUCT_SKU, pageRequest);
  }

  @Test
  public void getHalaProductHistoryProductSkuEmptyTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      halaHistoryUpdateService.getHalalProductHistory(STORE_ID, StringUtils.EMPTY, pageRequest);
    });
  }

  @Test
  public void getHalaProductHistoryStoreIdEmptyTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      halaHistoryUpdateService.getHalalProductHistory(StringUtils.EMPTY, PRODUCT_SKU, pageRequest);
    });
  }

  @Test
  public void saveHalalHistoryUpdateTest() throws Exception {
    halaHistoryUpdateService.saveHalalHistoryUpdate(halalHistoryUpdateEventModel);
    Mockito.verify(halalProductHistoryRepository).save(halalProductHistory);
    Assertions.assertEquals(PRODUCT_SKU, halalProductHistory.getProductSku());
    Assertions.assertEquals(PREVIOUS_VALUE, halalProductHistory.getPreviousValue());
  }

  @Test
  public void saveHalalHistoryUpdateStoreIdEmptyTest() throws Exception {
    halalHistoryUpdateEventModel.setStoreId(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      halaHistoryUpdateService.saveHalalHistoryUpdate(halalHistoryUpdateEventModel);
    });
  }

  @Test
  public void saveHalalHistoryUpdateUserNameEmptyTest() throws Exception {
    halalHistoryUpdateEventModel.setUserName(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      halaHistoryUpdateService.saveHalalHistoryUpdate(halalHistoryUpdateEventModel);
    });
  }

  @Test
  public void saveHalalHistoryUpdateProductSkuEmptyTest() throws Exception {
    halalHistoryUpdateEventModel.setProductSku(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      halaHistoryUpdateService.saveHalalHistoryUpdate(halalHistoryUpdateEventModel);
    });
  }

  @Test
  public void saveHalalHistoryUpdateActivityEmptyTest() throws Exception {
    halalHistoryUpdateEventModel.setActivity(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      halaHistoryUpdateService.saveHalalHistoryUpdate(halalHistoryUpdateEventModel);
    });
  }

  @Test
  public void saveHalalHistoryUpdatePreviousValueEmptyTest() throws Exception {
    halalHistoryUpdateEventModel.setPreviousValue(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      halaHistoryUpdateService.saveHalalHistoryUpdate(halalHistoryUpdateEventModel);
    });
  }
  @Test
  public void saveHalalHistoryUpdateCurrentValueEmptyTest() throws Exception {
    halalHistoryUpdateEventModel.setCurrentValue(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      halaHistoryUpdateService.saveHalalHistoryUpdate(halalHistoryUpdateEventModel);
    });
  }

}
