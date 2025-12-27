package com.gdn.x.mta.distributiontask.service.impl.publisher;

import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductForcedRollbackSLAExceedDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductRejectedEventModel;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class ProductPublisherServiceImplTest {

  private static final String PRODUCT_CODE = "CODE";

  @InjectMocks
  private ProductPublisherServiceImpl instance;

  @Mock
  private KafkaPublisher kafkaProducer;

  @AfterEach
  public void tearDown() throws Exception {}

  @Test
   void testRejectedProductPublisher() {
    ProductRejectedEventModel productRejectedEventModel = this.instance.rejectedProductPublisher(PRODUCT_CODE);
    Assertions.assertNotNull(productRejectedEventModel);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_REJECTED_TASK_EVENT_NAME), Mockito.eq(PRODUCT_CODE),
            Mockito.any(ProductRejectedEventModel.class));
  }

  @Test
   void testPublishEvent() {
    Product product = new Product.Builder().productCode(PRODUCT_CODE).build();
    ProductDistributionTask productDistributionTask =
        new ProductDistributionTask("taskCode", new Vendor(), product, null, null);
    ProductForcedRollbackSLAExceedDomainEventModel productForcedRollbackSLAExceedDomainEventModel =
        this.instance.publishEvent(productDistributionTask);
    Assertions.assertNotNull(productForcedRollbackSLAExceedDomainEventModel);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_FORCE_CLOSE_TASK_EVENT_AND_ROLLBACK_PRODUCT_NAME), Mockito.eq(PRODUCT_CODE),
            Mockito.any(ProductForcedRollbackSLAExceedDomainEventModel.class));
  }

}
