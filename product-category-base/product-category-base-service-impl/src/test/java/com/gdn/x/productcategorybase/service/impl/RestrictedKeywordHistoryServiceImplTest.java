package com.gdn.x.productcategorybase.service.impl;

import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordActivityHistory;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordHistoryEventModel;
import com.gdn.x.productcategorybase.entity.RestrictedKeywordHistory;
import com.gdn.x.productcategorybase.repository.RestrictedKeywordHistoryRepository;

public class RestrictedKeywordHistoryServiceImplTest {

  private static final String STORE_ID = "storeId";
  private static final String KEYWORD_ID = "keywordId";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static PageRequest pageRequest;
  private static RestrictedKeywordHistory restrictedKeywordHistory;
  private static final String ACTIVITY = "activity";
  private static final String OLD_VALUE = "oldValue";
  private static final String NEW_VALUE = "newValue";
  private static final String CREATED_BY = "createdBy";
  private static final String USER_NAME = "userName";
  private RestrictedKeywordActivityHistory restrictedKeywordActivityHistory;
  private RestrictedKeywordHistoryEventModel restrictedKeywordHistoryEventModel;

  @InjectMocks
  private RestrictedKeywordHistoryServiceImpl restrictedKeywordHistoryService;

  @Mock
  private RestrictedKeywordHistoryRepository restrictedKeywordHistoryRepository;

  @Captor
  private ArgumentCaptor<List<RestrictedKeywordHistory>> restrictedKeywordHistoryArgumentCaptor;

  @BeforeEach
  public void init() {
    initMocks(this);
    pageRequest = PageRequest.of(PAGE, SIZE);
    restrictedKeywordHistory = new RestrictedKeywordHistory();
    restrictedKeywordHistory.setKeywordId(KEYWORD_ID);
    restrictedKeywordHistory.setActivity(ACTIVITY);
    restrictedKeywordHistory.setOldValue(OLD_VALUE);
    restrictedKeywordHistory.setNewValue(NEW_VALUE);
    restrictedKeywordHistory.setCreatedBy(CREATED_BY);
    restrictedKeywordHistoryEventModel = new RestrictedKeywordHistoryEventModel();
    restrictedKeywordHistoryEventModel.setKeywordId(KEYWORD_ID);
    restrictedKeywordHistoryEventModel.setStoreId(STORE_ID);
    restrictedKeywordHistoryEventModel.setUserName(USER_NAME);
    restrictedKeywordActivityHistory = new RestrictedKeywordActivityHistory();
    restrictedKeywordActivityHistory.setActivity(ACTIVITY);
    restrictedKeywordActivityHistory.setNewValue(NEW_VALUE);
    restrictedKeywordActivityHistory.setOldValue(OLD_VALUE);
    restrictedKeywordHistoryEventModel.setRestrictedKeywordActivityHistoryList(
        Arrays.asList(restrictedKeywordActivityHistory));
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(restrictedKeywordHistoryRepository);
  }

  @Test
  public void getRestrictedKeywordHistoryTest() throws Exception {
    Page<RestrictedKeywordHistory> restrictedKeywordHistories =
        new PageImpl<>(Arrays.asList(restrictedKeywordHistory), pageRequest, 1);
    Mockito.when(
        restrictedKeywordHistoryRepository.findByStoreIdAndKeywordIdAndMarkForDeleteFalseOrderByCreatedDateDesc(
            STORE_ID, KEYWORD_ID, pageRequest)).thenReturn(restrictedKeywordHistories);
    restrictedKeywordHistoryService.getRestrictedKeywordHistory(STORE_ID, KEYWORD_ID, pageRequest);
    Mockito.verify(restrictedKeywordHistoryRepository)
        .findByStoreIdAndKeywordIdAndMarkForDeleteFalseOrderByCreatedDateDesc(STORE_ID, KEYWORD_ID, pageRequest);
  }

  @Test
  public void getRestrictedKeywordHistoryStoreIdEmptyTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> restrictedKeywordHistoryService.getRestrictedKeywordHistory(StringUtils.EMPTY, KEYWORD_ID, pageRequest));
  }

  @Test
  public void getRestrictedKeywordHistoryKeywordIdEmptyTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> restrictedKeywordHistoryService.getRestrictedKeywordHistory(STORE_ID, StringUtils.EMPTY, pageRequest));
  }

  @Test
  public void saveRestrictedKeywordHistoryTest() throws Exception {
    restrictedKeywordHistoryService.saveRestrictedKeywordHistory(restrictedKeywordHistoryEventModel);
    Mockito.verify(restrictedKeywordHistoryRepository).saveAll(restrictedKeywordHistoryArgumentCaptor.capture());
    Assertions.assertEquals(KEYWORD_ID, restrictedKeywordHistoryArgumentCaptor.getAllValues().get(0).get(0).getKeywordId());
    Assertions.assertEquals(OLD_VALUE, restrictedKeywordHistoryArgumentCaptor.getAllValues().get(0).get(0).getOldValue());
  }

  @Test
  public void saveRestrictedKeywordHistoryStoreIdEmptyTest() throws Exception {
    restrictedKeywordHistoryEventModel.setStoreId(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> restrictedKeywordHistoryService.saveRestrictedKeywordHistory(restrictedKeywordHistoryEventModel));
  }

  @Test
  public void saveRestrictedKeywordHistoryUserNameEmptyTest() throws Exception {
    restrictedKeywordHistoryEventModel.setUserName(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> restrictedKeywordHistoryService.saveRestrictedKeywordHistory(restrictedKeywordHistoryEventModel));
  }

  @Test
  public void saveRestrictedKeywordHistoryKeywordIdEmptyTest() throws Exception {
    restrictedKeywordHistoryEventModel.setKeywordId(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> restrictedKeywordHistoryService.saveRestrictedKeywordHistory(restrictedKeywordHistoryEventModel));
  }

  @Test
  public void saveRestrictedKeywordHistoryActivityEmptyTest() throws Exception {
    restrictedKeywordHistoryEventModel.getRestrictedKeywordActivityHistoryList().get(0).setActivity(StringUtils.EMPTY);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> restrictedKeywordHistoryService.saveRestrictedKeywordHistory(restrictedKeywordHistoryEventModel));
  }

}
