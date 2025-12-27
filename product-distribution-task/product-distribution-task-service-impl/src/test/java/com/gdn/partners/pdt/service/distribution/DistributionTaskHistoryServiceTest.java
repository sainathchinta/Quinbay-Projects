package com.gdn.partners.pdt.service.distribution;

import java.util.ArrayList;
import java.util.List;

import com.gdn.x.mta.distributiontask.domain.event.InternalHistoryEventModel;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaTopicProperties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;

import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.dao.api.TaskHistoryRepository;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.TaskHistory;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

public class DistributionTaskHistoryServiceTest {

  @Mock
  private TaskHistoryRepository distributionTaskHistoryRepository;

  @Mock
  private ProductServiceRepository productServiceRepository;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private KafkaPublisher kafkaProducer;

  @InjectMocks
  private DistributionTaskHistoryServiceBean distributionTaskHistoryServiceBean;

  @SuppressWarnings("unchecked")
  @Test
   void create_2_Test() throws Exception {
    Mockito.when(kafkaTopicProperties.getInternalHistoryEventName()).thenReturn("EVENT");
    this.distributionTaskHistoryServiceBean.create(this.generateDistributionTasks());
    Mockito.verify(this.distributionTaskHistoryRepository).saveAll(Mockito.anyList());
    Mockito.verify(kafkaProducer)
      .send(Mockito.anyString(), Mockito.anyString(), Mockito.any(InternalHistoryEventModel.class));
    Mockito.verify(kafkaTopicProperties).getInternalHistoryEventName();
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.distributionTaskHistoryRepository);
    Mockito.verifyNoMoreInteractions(this.kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(this.kafkaProducer);
    MDC.clear();
  }

  private ProductDistributionTask generateDistributionTask(Vendor vendor) throws Exception {
    ProductDistributionTask distributionTask = new ProductDistributionTask();
    Product product = new Product();
    product.setProductCode("PRODUCT_CODE");
    distributionTask.setState(WorkflowState.UNASSIGNED);
    distributionTask.setProduct(product);
    distributionTask.setVendor(vendor);
    return distributionTask;
  }

  private List<ProductDistributionTask> generateDistributionTasks() throws Exception {
    List<ProductDistributionTask> distributionTasks = new ArrayList<ProductDistributionTask>();
    distributionTasks.add(this.generateDistributionTask(new Vendor()));
    return distributionTasks;
  }

  @SuppressWarnings("unchecked")
  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.openMocks(this);
    Mockito.when(this.distributionTaskHistoryRepository.save(Mockito.any(TaskHistory.class))).thenReturn(null);
    Mockito.when(this.distributionTaskHistoryRepository.saveAll(Mockito.anyList())).thenReturn(null);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, "requestId");
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, "System");
  }

}
