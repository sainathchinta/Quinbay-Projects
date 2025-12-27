package com.gdn.mta.bulk.listener.kafka;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.DormantSellerProductStatus;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.DormantSellerItemDetail;
import com.gdn.mta.bulk.service.DormantSellerService;
import com.gdn.partners.bulk.util.Constant;


public class DormantSellerItemViewConfigUpdateListenerTest {

  private static final String ITEM_SKU = "item-sku";

  @InjectMocks
  private  DormantSellerItemViewConfigUpdateListener dormantSellerItemViewConfigUpdateListener;

  @Mock
  DormantSellerService dormantSellerService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void initialize() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(dormantSellerService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void testOnDomainEventConsumedTest() throws Exception {
    DormantSellerItemDetail dormantSellerItemDetail = DormantSellerItemDetail.builder().itemSku(ITEM_SKU).itemStatus(
        DormantSellerProductStatus.ACTIVE.name()).build();
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, DormantSellerItemDetail.class))
        .thenReturn(dormantSellerItemDetail);
    dormantSellerItemViewConfigUpdateListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(dormantSellerService).updateProductItemViewConfig(dormantSellerItemDetail);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, DormantSellerItemDetail.class);
    Mockito.verify(kafkaTopicProperties).getDormantSellerItemSkuViewConfigUpdate();
  }

  @Test
  public void testOnDomainEventConsumedExceptionTest() throws Exception {
    DormantSellerItemDetail dormantSellerItemDetail =
        DormantSellerItemDetail.builder().itemSku(ITEM_SKU).itemStatus(DormantSellerProductStatus.ACTIVE.name())
            .build();
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, DormantSellerItemDetail.class))
        .thenReturn(dormantSellerItemDetail);
    Mockito.doThrow(new ApplicationException()).when(dormantSellerService)
        .updateProductItemViewConfig(dormantSellerItemDetail);
    dormantSellerItemViewConfigUpdateListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(dormantSellerService).updateProductItemViewConfig(dormantSellerItemDetail);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, DormantSellerItemDetail.class);
    Mockito.verify(kafkaTopicProperties).getDormantSellerItemSkuViewConfigUpdate();
  }
}