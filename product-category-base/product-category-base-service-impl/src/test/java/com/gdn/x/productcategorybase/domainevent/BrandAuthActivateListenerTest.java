package com.gdn.x.productcategorybase.domainevent;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.BrandAuthActivateEventModel;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationWipService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

@ExtendWith(MockitoExtension.class)
class BrandAuthActivateListenerTest {

  private static final String DEFAULT_BRAND_CODE = "BRD-00001";
  private static final String DEFAULT_SELLER_CODE = "SEL-00001";
  private static final String EVENT = "event";
  @InjectMocks
  public BrandAuthActivateListener brandAuthActivateListener;
  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private DomainEventPublisherService domainEventPublisherService;

  @Mock
  private BrandAuthorisationWipService brandAuthorisationWipService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;
  private String message;
  private BrandAuthActivateEventModel brandAuthActivateEventModel;
  private BrandAuthorisationHistory brandAuthorisationHistory;

  @BeforeEach
  public void setup() throws JsonProcessingException {
    brandAuthActivateEventModel =
      BrandAuthActivateEventModel.builder().brandCode(DEFAULT_BRAND_CODE)
        .sellerCode(DEFAULT_SELLER_CODE).build();
    brandAuthorisationHistory = new BrandAuthorisationHistory();
    brandAuthorisationHistory.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthorisationHistory.setSellerCode(DEFAULT_SELLER_CODE);

    ObjectMapper mapper = new ObjectMapper();
    message = mapper.writeValueAsString(brandAuthActivateEventModel);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(domainEventPublisherService, brandAuthorisationWipService,
      objectMapper, kafkaTopicProperties);
  }

  @Test
  void onDomainEventConsumedTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getBrandAuthActivateEvent()).thenReturn(EVENT);
    Mockito.when(objectMapper.readValue(message, BrandAuthActivateEventModel.class))
      .thenReturn(brandAuthActivateEventModel);
    Mockito.when(
        brandAuthorisationWipService.activateBrandAuthorisation(brandAuthActivateEventModel))
      .thenReturn(List.of(brandAuthorisationHistory));
    brandAuthActivateListener.onDomainEventConsumed(message);
    verify(brandAuthorisationWipService).activateBrandAuthorisation(brandAuthActivateEventModel);
    verify(domainEventPublisherService).publishBrandAuthHistoryEvent(DEFAULT_BRAND_CODE,
      DEFAULT_SELLER_CODE, brandAuthorisationHistory);
    verify(kafkaTopicProperties).getBrandAuthActivateEvent();
    verify(objectMapper).readValue(message, BrandAuthActivateEventModel.class);
  }

  @Test
  void onDomainEventConsumedNullTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getBrandAuthActivateEvent()).thenReturn(EVENT);
    Mockito.when(objectMapper.readValue(message, BrandAuthActivateEventModel.class))
      .thenReturn(null);
    brandAuthActivateListener.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, BrandAuthActivateEventModel.class);
  }

  @Test
  void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(objectMapper)
      .readValue(message, BrandAuthActivateEventModel.class);
    brandAuthActivateListener.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, BrandAuthActivateEventModel.class);
    verify(kafkaTopicProperties, times(2)).getBrandAuthActivateEvent();
  }
}
