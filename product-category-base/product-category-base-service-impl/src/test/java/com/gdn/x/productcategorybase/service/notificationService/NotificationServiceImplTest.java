package com.gdn.x.productcategorybase.service.notificationService;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;

import com.gdn.partners.kafka.notification.NotificationKafka;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.config.MandatoryParameterHelper;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;

public class NotificationServiceImplTest {

  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final String DEFAULT_USER_NAME = "userName";
  private static final String STORE_ID = "10001";
  private static final String NOTES = "notes";
  private BrandWip brandWip;

  @InjectMocks
  private NotificationServiceImpl notificationService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    MDC.put("requestId", DEFAULT_REQUEST_ID);
    MDC.put("username", DEFAULT_USER_NAME);

    brandWip = new BrandWip();
    brandWip.setState(BrandWipState.APPROVED);
    //Span span = Span.builder().build();
    //span.setBaggageItem(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(mandatoryParameterHelper);
  }

  @Test
  public void sendWebNotificationTest() throws Exception {
    notificationService.createWebNotification(brandWip);
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), Mockito.any(NotificationKafka.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendWebNotificationTestWithNonNullStoreId() throws Exception {
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    notificationService.createWebNotification(brandWip);
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), Mockito.any(NotificationKafka.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }

  @Test
  public void sendWebNotificationRejectedBrandTest() throws Exception {
    brandWip.setState(BrandWipState.REJECTED);
    brandWip.setNotes(NOTES.getBytes());
    notificationService.createWebNotification(brandWip);
    Mockito.verify(kafkaProducer)
        .send(Mockito.any(), Mockito.any(), Mockito.any(NotificationKafka.class));
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getChannelId();
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getClientId();
  }
}