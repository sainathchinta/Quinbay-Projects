package com.gdn.x.productcategorybase.domainevent;

import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Arrays;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordActivityHistory;
import com.gdn.x.productcategorybase.domain.event.model.RestrictedKeywordHistoryEventModel;
import com.gdn.x.productcategorybase.service.RestrictedKeywordHistoryService;

public class RestrictedKeywordHistoryListenerTest {

  @InjectMocks
  private RestrictedKeywordHistoryListener restrictedKeywordHistoryListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private RestrictedKeywordHistoryService restrictedKeywordHistoryService;

  private RestrictedKeywordActivityHistory restrictedKeywordActivityHistory;
  private RestrictedKeywordHistoryEventModel restrictedKeywordHistoryEventModel;
  private static final String KEYWORD_ID = "keywordId";
  private static final String STORE_ID = "storeId";
  private static final String ACTIVITY = "activity";
  private static final String OLD_VALUE = "oldValue";
  private static final String NEW_VALUE = "newValue";
  private static final String USER_NAME = "userName";
  private String message;

  @BeforeEach
  public void init() throws Exception {
    initMocks(this);
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

    ObjectMapper mapper = new ObjectMapper();
    message = mapper.writeValueAsString(restrictedKeywordHistoryEventModel);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(restrictedKeywordHistoryService);
  }

  @Test
  public void RestrictedKeywordHistoryListenerTest() throws Exception {
    Mockito.when(objectMapper.readValue(message, RestrictedKeywordHistoryEventModel.class))
        .thenReturn(restrictedKeywordHistoryEventModel);
    restrictedKeywordHistoryListener.onDomainEventConsumed(message);
    Mockito.verify(restrictedKeywordHistoryService).saveRestrictedKeywordHistory(restrictedKeywordHistoryEventModel);
  }

  @Test
  public void RestrictedKeywordHistoryListenerExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(message, RestrictedKeywordHistoryEventModel.class))
        .thenThrow(new NullPointerException());
    restrictedKeywordHistoryListener.onDomainEventConsumed(message);
  }

}
