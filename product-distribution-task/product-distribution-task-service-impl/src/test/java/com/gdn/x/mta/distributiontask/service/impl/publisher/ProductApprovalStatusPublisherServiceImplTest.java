package com.gdn.x.mta.distributiontask.service.impl.publisher;

import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.config.ProductApprovalDetailStatus;
import com.gdn.mta.domain.event.modal.ProductApprovalDetailStatusEvent;
import com.gdn.x.mta.distributiontask.model.Constants;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Created by Akshay bhatt on 17/04/2018.
 */
@ExtendWith(MockitoExtension.class)
public class ProductApprovalStatusPublisherServiceImplTest {

  @Mock
  private KafkaPublisher kafkaProducer;

  @InjectMocks
  private ProductApprovalStatusPublisherServiceImpl productApprovalStatusPublisherService;

  private ProductApprovalDetailStatus productApprovalDetailStatus;

  private static final String PRODUCT_CODE = "MTA-0001";

  @BeforeEach public void setUp() throws Exception {
    productApprovalDetailStatus = ProductApprovalDetailStatus.PDT_Approve_Product_Kafka_Event_Published_PBP;
  }

  @Test
   void updateProductApprovalDetailStatusWithProductCode() {
    ProductApprovalDetailStatusEvent productApprovalDetailStatusEvent = productApprovalStatusPublisherService
        .updateProductApprovalDetailStatus(PRODUCT_CODE, productApprovalDetailStatus);
    Assertions.assertEquals(PRODUCT_CODE, productApprovalDetailStatusEvent.getProductCode());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_STATUS_UPDATE_TRACKER), Mockito.eq(Constants.PRODUCT_CODE),
            Mockito.any(ProductApprovalDetailStatusEvent.class));
  }

}
