package com.gdn.partners.pdt.service.distribution;

import java.util.ArrayList;
import java.util.List;

import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaTopicProperties;
import com.gdn.x.mta.distributiontask.service.impl.util.ConverterUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.dao.api.TaskHistoryRepository;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.TaskHistory;

@Service
@Transactional(readOnly = true)
public class DistributionTaskHistoryServiceBean implements DistributionTaskHistoryService {

  @Autowired
  private TaskHistoryRepository distributionTaskHistoryRepository;

  @Autowired
  private ProductServiceRepository productServiceRepository;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void create(List<ProductDistributionTask> distributionTasks) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    String username = GdnMandatoryRequestParameterUtil.getUsername();
    List<TaskHistory> distributionTaskHistories = new ArrayList<TaskHistory>();
    for (ProductDistributionTask distributionTask : distributionTasks) {
      distributionTaskHistories
          .add(this.generateDistributionTaskHistory(storeId, username, distributionTask));
    }
    this.distributionTaskHistoryRepository.saveAll(distributionTaskHistories);
    for (TaskHistory taskHistory : distributionTaskHistories) {
      kafkaProducer.send(kafkaTopicProperties.getInternalHistoryEventName(),
        taskHistory.getProductCode(),
        ConverterUtil.convertToInternalHistoryModel(taskHistory.getStoreId(),
          taskHistory.getProductCode(), GdnMandatoryRequestParameterUtil.getUsername(),
          taskHistory.getState().getDesc(), taskHistory.getReason()));
    }
  }

  private TaskHistory generateDistributionTaskHistory(String storeId, String username,
      ProductDistributionTask distributionTask) throws Exception {
    String reason = distributionTask.getVendor() != null
        ? "In Review Assigned To Vendor " + distributionTask.getVendor().getName()
        : distributionTask.getState().name();
    TaskHistory distributionTaskHistory = new TaskHistory();
    distributionTaskHistory.setStoreId(storeId);
    distributionTaskHistory.setCreatedBy(username);
    distributionTaskHistory.setProductCode(distributionTask.getProduct().getProductCode());
    distributionTaskHistory.setProductName(distributionTask.getProduct().getProductName());
    distributionTaskHistory.setCategoryCode(distributionTask.getProduct().getCategoryCode());
    distributionTaskHistory.setCategoryName(distributionTask.getProduct().getCategoryName());
    distributionTaskHistory.setState(distributionTask.getState());
    distributionTaskHistory.setTaskCode(distributionTask.getTaskCode());
    distributionTaskHistory.setVendor(distributionTask.getVendor());
    distributionTaskHistory.setReason(reason);
    return distributionTaskHistory;
  }
}
