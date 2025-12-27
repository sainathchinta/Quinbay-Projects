package com.gdn.mta.product.service.domainevent;

import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.OrderCancellationDto;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.mta.product.service.ProductLevel3V2Wrapper;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.neo.order.domain.event.model.OrderItemStatusChangedToOfflineCancel;


public class OrderCancellationSubscriberTest {

  @InjectMocks
  private OrderCancellationSubscriber orderCancellationSubscriber;

  @Mock
  private ProductLevel3V2Wrapper productLevel3V2Wrapper;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private ObjectMapper objectMapper;

  private ObjectMapper mapper;

  private static final String STORE_ID = "10001";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(productLevel3V2Wrapper);
    verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void orderCancellationSubscriberTest() throws Exception {
    OrderItemStatusChangedToOfflineCancel orderItemStatusChangedToOfflineCancel =
        new OrderItemStatusChangedToOfflineCancel();
    orderItemStatusChangedToOfflineCancel.setStoreId(STORE_ID);
    orderItemStatusChangedToOfflineCancel.setMerchantCommissionType(Constants.CM_MERCHANT);
    OrderCancellationDto orderCancellationDto = new OrderCancellationDto();
    orderCancellationDto.setStoreId(STORE_ID);
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(orderCancellationSubscriber),
        OrderItemStatusChangedToOfflineCancel.class)).thenReturn(orderItemStatusChangedToOfflineCancel);
    orderCancellationSubscriber.onDomainEventConsumed(mapper.writeValueAsString(orderCancellationSubscriber));
    Mockito.verify(productLevel3V2Wrapper)
        .updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(orderCancellationSubscriber),
        OrderItemStatusChangedToOfflineCancel.class);
  }

  @Test
  public void orderCancellationSubscriberMerchantTypeEmptyTest() throws Exception {
    OrderItemStatusChangedToOfflineCancel orderItemStatusChangedToOfflineCancel =
        new OrderItemStatusChangedToOfflineCancel();
    orderItemStatusChangedToOfflineCancel.setStoreId(STORE_ID);
    orderItemStatusChangedToOfflineCancel.setMerchantCommissionType(StringUtils.EMPTY);
    OrderCancellationDto orderCancellationDto = new OrderCancellationDto();
    orderCancellationDto.setStoreId(STORE_ID);
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(orderCancellationSubscriber),
        OrderItemStatusChangedToOfflineCancel.class)).thenReturn(orderItemStatusChangedToOfflineCancel);
    orderCancellationSubscriber.onDomainEventConsumed(mapper.writeValueAsString(orderCancellationSubscriber));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(orderCancellationSubscriber),
        OrderItemStatusChangedToOfflineCancel.class);
  }

  @Test
  public void orderCancellationSubscriberMerchantTypeCCTest() throws Exception {
    OrderItemStatusChangedToOfflineCancel orderItemStatusChangedToOfflineCancel =
        new OrderItemStatusChangedToOfflineCancel();
    orderItemStatusChangedToOfflineCancel.setStoreId(STORE_ID);
    orderItemStatusChangedToOfflineCancel.setMerchantCommissionType(Constants.CC_MERCHANT);
    OrderCancellationDto orderCancellationDto = new OrderCancellationDto();
    orderCancellationDto.setStoreId(STORE_ID);
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(orderCancellationSubscriber),
        OrderItemStatusChangedToOfflineCancel.class)).thenReturn(orderItemStatusChangedToOfflineCancel);
    orderCancellationSubscriber.onDomainEventConsumed(mapper.writeValueAsString(orderCancellationSubscriber));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(orderCancellationSubscriber),
        OrderItemStatusChangedToOfflineCancel.class);
  }

  @Test
  public void orderCancellationSubscriberSpanNullTest() throws Exception {
    OrderItemStatusChangedToOfflineCancel orderItemStatusChangedToOfflineCancel =
        new OrderItemStatusChangedToOfflineCancel();
    orderItemStatusChangedToOfflineCancel.setStoreId(STORE_ID);
    orderItemStatusChangedToOfflineCancel.setMerchantCommissionType(Constants.CM_MERCHANT);
    OrderCancellationDto orderCancellationDto = new OrderCancellationDto();
    orderCancellationDto.setStoreId(STORE_ID);
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(orderCancellationSubscriber),
        OrderItemStatusChangedToOfflineCancel.class)).thenReturn(orderItemStatusChangedToOfflineCancel);
    orderCancellationSubscriber.onDomainEventConsumed(mapper.writeValueAsString(orderCancellationSubscriber));
    Mockito.verify(productLevel3V2Wrapper)
        .updateL4AndL5ToOfflineForOrderCancellation(orderCancellationDto);
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(orderCancellationSubscriber),
        OrderItemStatusChangedToOfflineCancel.class);
  }

  @Test
  public void orderCancellationSubscriberExceptionTest() throws Exception {
    OrderItemStatusChangedToOfflineCancel orderItemStatusChangedToOfflineCancel =
        new OrderItemStatusChangedToOfflineCancel();
    orderItemStatusChangedToOfflineCancel.setStoreId(StringUtils.EMPTY);
    orderItemStatusChangedToOfflineCancel.setMerchantCommissionType(Constants.CM_MERCHANT);
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(orderCancellationSubscriber),
        OrderItemStatusChangedToOfflineCancel.class)).thenReturn(orderItemStatusChangedToOfflineCancel);
    orderCancellationSubscriber.onDomainEventConsumed(mapper.writeValueAsString(orderCancellationSubscriber));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(orderCancellationSubscriber),
        OrderItemStatusChangedToOfflineCancel.class);
  }
}
