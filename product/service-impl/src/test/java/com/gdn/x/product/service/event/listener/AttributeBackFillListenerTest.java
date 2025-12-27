package com.gdn.x.product.service.event.listener;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import com.gdn.x.product.service.api.MasterDataCacheService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.XProdAttributeMigrationEventModel;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.service.api.ProductServiceV2;
import com.gdn.x.product.service.properties.KafkaTopicProperties;

public class AttributeBackFillListenerTest {

  private XProdAttributeMigrationEventModel xProdAttributeMigrationEventModel;

  @InjectMocks
  private AttributeBackFillListener attributeBackFillListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductServiceV2 productServiceV2;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Mock
  private MasterDataCacheService masterDataCacheService;

  private final String MESSAGE = "message";


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    xProdAttributeMigrationEventModel = new XProdAttributeMigrationEventModel();
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.objectMapper);
    verifyNoMoreInteractions(this.productServiceV2);
    verifyNoMoreInteractions(this.kafkaTopicProperties, this.masterDataCacheService);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    when(this.objectMapper.readValue(MESSAGE, XProdAttributeMigrationEventModel.class)).thenReturn(
        xProdAttributeMigrationEventModel);
    attributeBackFillListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, XProdAttributeMigrationEventModel.class);
    Mockito.verify(kafkaTopicProperties).getXProductAttributeMigrationEvent();
    Mockito.verify(masterDataCacheService).evictMasterDataProduct(
      xProdAttributeMigrationEventModel.getProductCode());
    Mockito.verify(productCategoryBaseOutbound).updateStatusInPCBForBackFillAttributes(Mockito.any(),Mockito.any(),Mockito.any());
  }

  @Test
  public void onDomainEventConsumedWithSkuValueTest() throws Exception {
    when(this.objectMapper.readValue(MESSAGE, XProdAttributeMigrationEventModel.class)).thenReturn(
      xProdAttributeMigrationEventModel);
    xProdAttributeMigrationEventModel.setSkuValue(true);
    attributeBackFillListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, XProdAttributeMigrationEventModel.class);
    Mockito.verify(this.productServiceV2)
      .backFillSpecialAttributesInProduct(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(kafkaTopicProperties).getXProductAttributeMigrationEvent();
    Mockito.verify(masterDataCacheService).evictMasterDataProduct(
      xProdAttributeMigrationEventModel.getProductCode());
    Mockito.verify(productCategoryBaseOutbound).updateStatusInPCBForBackFillAttributes(Mockito.any(),Mockito.any(),Mockito.any());
  }

  @Test
  public void onDomainEventConsumed_exceptionTest() throws Exception {
    when(this.objectMapper.readValue(MESSAGE, XProdAttributeMigrationEventModel.class)).thenReturn(
        xProdAttributeMigrationEventModel);
    xProdAttributeMigrationEventModel.setSkuValue(true);
    Mockito.doThrow(RuntimeException.class).when(this.productServiceV2)
        .backFillSpecialAttributesInProduct(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
    attributeBackFillListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.productServiceV2)
      .backFillSpecialAttributesInProduct(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(this.objectMapper).readValue(MESSAGE, XProdAttributeMigrationEventModel.class);
    Mockito.verify(kafkaTopicProperties, times(2)).getXProductAttributeMigrationEvent();
    Mockito.verify(masterDataCacheService).evictMasterDataProduct(
      xProdAttributeMigrationEventModel.getProductCode());
    Mockito.verify(productCategoryBaseOutbound).updateStatusInPCBForBackFillAttributes(Mockito.any(),Mockito.any(),Mockito.any());
  }
}
