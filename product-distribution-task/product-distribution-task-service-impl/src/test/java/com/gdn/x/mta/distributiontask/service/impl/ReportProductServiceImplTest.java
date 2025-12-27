package com.gdn.x.mta.distributiontask.service.impl;

import com.gdn.x.mta.distributiontask.domain.event.model.AddCustomerProductToIPREventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.enums.ProductSourceIPR;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.MDC;

import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.dao.api.ReportProductRepository;
import com.gdn.x.mta.distributiontask.model.ReportProduct;
import com.gdn.x.mta.distributiontask.rest.model.request.ReportProductRequest;
import org.springframework.test.util.ReflectionTestUtils;

@ExtendWith(MockitoExtension.class)
public class ReportProductServiceImplTest {

  @InjectMocks
  private ReportProductServiceImpl reportProductService;

  @Mock
  private ReportProductRepository reportProductRepository;

  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicProperties;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Captor
  private ArgumentCaptor<ReportProduct> reportProductArgumentCaptor;

  private static final String STORE_ID = "10001";
  private static final String MEMBER_ID = "memberId";
  private static final String ITEM_SKU = "SUS-68973-21388-00002";
  private static final String PRODUCT_SKU = "SUS-68973-21388";
  private static final String REASON = "reason";
  private static final String REASON2 = "reason2";
  private static final String KAFKA_EVENT = "event";
  private static final String SOURCE = "CUSTOMER_REPORT";

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(reportProductRepository, kafkaProducer, kafkaTopicProperties);
  }

  @Test
   void addReportProductTest() {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    ReflectionTestUtils.setField(reportProductService, "iprProductReportReasons", REASON);
    ReportProductRequest reportProductRequest =
        ReportProductRequest.builder().memberId(MEMBER_ID).itemSku(ITEM_SKU).reason(REASON).build();
    Mockito.when(kafkaTopicProperties.getAddCustomerProductToIprEvent()).thenReturn(KAFKA_EVENT);
    reportProductService.addReportProduct(reportProductRequest);
    Mockito.verify(reportProductRepository)
        .findByStoreIdAndMemberIdAndItemSkuAndReason(STORE_ID, MEMBER_ID, ITEM_SKU, REASON);
    Mockito.verify(reportProductRepository).save(reportProductArgumentCaptor.capture());
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getAddCustomerProductToIprEvent();
    Mockito.verify(kafkaProducer)
        .send(KAFKA_EVENT, PRODUCT_SKU, new AddCustomerProductToIPREventModel(PRODUCT_SKU,
            ProductSourceIPR.CUSTOMER_REPORT.name(), STORE_ID, SOURCE));
    Assertions.assertEquals(MEMBER_ID, reportProductArgumentCaptor.getValue().getMemberId());
    Assertions.assertEquals(STORE_ID, reportProductArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(ITEM_SKU, reportProductArgumentCaptor.getValue().getItemSku());
    Assertions.assertEquals(REASON, reportProductArgumentCaptor.getValue().getReason());
  }

  @Test
  public void addReportProductEmptyReasonsTest() {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    ReflectionTestUtils.setField(reportProductService, "iprProductReportReasons",
        StringUtils.EMPTY);
    ReportProductRequest reportProductRequest =
        ReportProductRequest.builder().memberId(MEMBER_ID).itemSku(ITEM_SKU).reason(REASON).build();
    reportProductService.addReportProduct(reportProductRequest);
    Mockito.verify(reportProductRepository)
        .findByStoreIdAndMemberIdAndItemSkuAndReason(STORE_ID, MEMBER_ID, ITEM_SKU, REASON);
    Mockito.verify(reportProductRepository).save(reportProductArgumentCaptor.capture());
    Assertions.assertEquals(MEMBER_ID, reportProductArgumentCaptor.getValue().getMemberId());
    Assertions.assertEquals(STORE_ID, reportProductArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(ITEM_SKU, reportProductArgumentCaptor.getValue().getItemSku());
    Assertions.assertEquals(REASON, reportProductArgumentCaptor.getValue().getReason());
  }

  @Test
  public void addReportProductDifferentReasonsTest() {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    ReflectionTestUtils.setField(reportProductService, "iprProductReportReasons", REASON2);
    ReportProductRequest reportProductRequest =
        ReportProductRequest.builder().memberId(MEMBER_ID).itemSku(ITEM_SKU).reason(REASON).build();
    reportProductService.addReportProduct(reportProductRequest);
    Mockito.verify(reportProductRepository)
        .findByStoreIdAndMemberIdAndItemSkuAndReason(STORE_ID, MEMBER_ID, ITEM_SKU, REASON);
    Mockito.verify(reportProductRepository).save(reportProductArgumentCaptor.capture());
    Assertions.assertEquals(MEMBER_ID, reportProductArgumentCaptor.getValue().getMemberId());
    Assertions.assertEquals(STORE_ID, reportProductArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(ITEM_SKU, reportProductArgumentCaptor.getValue().getItemSku());
    Assertions.assertEquals(REASON, reportProductArgumentCaptor.getValue().getReason());
  }

  @Test
   void addReportProductItemSkuNullTest() {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    ReportProductRequest reportProductRequest =
        ReportProductRequest.builder().memberId(MEMBER_ID).itemSku(StringUtils.EMPTY).reason(REASON).build();
    Assertions.assertThrows(Exception.class,
      () ->  reportProductService.addReportProduct(reportProductRequest));
  }

  @Test
   void addReportProductMemberIdNullTest() {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    ReportProductRequest reportProductRequest =
        ReportProductRequest.builder().memberId(StringUtils.EMPTY).itemSku(ITEM_SKU).reason(REASON).build();
    Assertions.assertThrows(Exception.class,
      () -> reportProductService.addReportProduct(reportProductRequest));
  }

  @Test
   void addReportProductReasonNullTest() {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    ReportProductRequest reportProductRequest =
        ReportProductRequest.builder().itemSku(ITEM_SKU).memberId(MEMBER_ID).reason(StringUtils.EMPTY).build();
    Assertions.assertThrows(Exception.class,
      () -> reportProductService.addReportProduct(reportProductRequest));
  }

  @Test
   void addReportProductAlreadyExistsTest() {
    Mockito.when(reportProductRepository.findByStoreIdAndMemberIdAndItemSkuAndReason(
        STORE_ID, MEMBER_ID, ITEM_SKU, REASON)).thenReturn(new ReportProduct());
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    ReportProductRequest reportProductRequest =
        ReportProductRequest.builder().memberId(MEMBER_ID).itemSku(ITEM_SKU).reason(REASON).build();
    try {
      Assertions.assertThrows(Exception.class,
        () -> reportProductService.addReportProduct(reportProductRequest));
    } finally {
      Mockito.verify(reportProductRepository).findByStoreIdAndMemberIdAndItemSkuAndReason(
          STORE_ID, MEMBER_ID, ITEM_SKU, REASON);
    }
  }
}
