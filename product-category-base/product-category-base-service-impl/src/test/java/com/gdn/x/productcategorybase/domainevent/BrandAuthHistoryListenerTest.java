package com.gdn.x.productcategorybase.domainevent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.BrandAuthDomainEventModel;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.service.brand.BrandAuthHistoryService;

public class BrandAuthHistoryListenerTest {

  @InjectMocks
  private BrandAuthHistoryListener brandAuthHistoryListener;

  @Mock
  private BrandAuthHistoryService brandAuthHistoryService;

  @Mock
  private ObjectMapper objectMapper;

  @Captor
  private ArgumentCaptor<BrandAuthorisationHistory> historyArgumentCaptor;

  private BrandAuthDomainEventModel brandAuthDomainEventModel;
  private static final String OLD_VALUE_AUTH = "Inactive";
  private static final String NEW_VALUE_AUTH = "Active";
  private static final String ACTIVITY_AUTH = "Change status";
  private static final String DEFAULT_BRAND_CODE = "BRD-00001";
  private static final String DEFAULT_SELLER_CODE = "BRD-00001";
  private String message;

  @BeforeEach
  public void init() throws Exception {
    initMocks(this);
    brandAuthDomainEventModel = BrandAuthDomainEventModel.builder().brandCode(DEFAULT_BRAND_CODE)
      .sellerCode(DEFAULT_SELLER_CODE).activity(ACTIVITY_AUTH).newStatus(NEW_VALUE_AUTH)
      .oldStatus(OLD_VALUE_AUTH).build();

    ObjectMapper mapper = new ObjectMapper();
    message = mapper.writeValueAsString(brandAuthDomainEventModel);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(brandAuthHistoryService);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(message, BrandAuthDomainEventModel.class)).thenReturn
      (brandAuthDomainEventModel);
    brandAuthHistoryListener.onDomainEventConsumed(message);
    verify(brandAuthHistoryService).saveBrandAuthHistory(historyArgumentCaptor.capture());
    assertEquals(DEFAULT_BRAND_CODE, historyArgumentCaptor.getValue().getBrandCode());
    verify(objectMapper).readValue(message, BrandAuthDomainEventModel.class);
  }

  @Test
  public void onDomainEventConsumed_ExceptionTest() throws Exception {
    doThrow(RuntimeException.class).when(objectMapper).readValue(message, BrandAuthDomainEventModel.class);
    brandAuthHistoryListener.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, BrandAuthDomainEventModel.class);
  }

  @Test
  public void onDomainEventConsumedNullTest() throws Exception {
    Mockito.when(objectMapper.readValue(message, BrandAuthDomainEventModel.class)).thenReturn
      (null);
    brandAuthHistoryListener.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, BrandAuthDomainEventModel.class);
  }

}
