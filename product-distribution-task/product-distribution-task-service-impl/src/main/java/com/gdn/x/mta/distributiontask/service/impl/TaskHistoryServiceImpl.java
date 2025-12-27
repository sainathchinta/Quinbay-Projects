package com.gdn.x.mta.distributiontask.service.impl;

import com.gdn.x.mta.distributiontask.dao.api.TaskHistoryRepository;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTHistoryEventModel;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.TaskHistory;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.api.TaskHistoryService;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaTopicProperties;
import com.gdn.x.mta.distributiontask.service.impl.util.ConverterUtil;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Created by virajjasani on 25/09/16.
 */
@Service
public class TaskHistoryServiceImpl implements TaskHistoryService {

  @Autowired
  private TaskHistoryRepository taskHistoryRepository;

  @Value("${pdt.history.update.event}")
  private boolean pdtHistoryUpdateThroughEvent;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;


  @Override
  @Transactional
  public void createTaskHistory(String storeId, String executor, Product product, Vendor vendor,
      String reason, WorkflowState state, String taskCode) throws Exception {
    TaskHistory taskHistory = new TaskHistory(product.getProductCode(), product.getProductName(),
        product.getCategoryCode(), product.getCategoryName(), vendor, reason, state, storeId,
        executor, taskCode);
    if (!pdtHistoryUpdateThroughEvent) {
      this.taskHistoryRepository.save(taskHistory);
    }
    publishInternalHistoryEvent(product, state, reason);
  }

  @Override
  public Page<TaskHistory> getHistoryFromProductCode(String storeId, String productCode,
      Pageable page) throws Exception {
    return this.taskHistoryRepository.findAllByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId,
        productCode, page);
  }

  @Override
  public Page<TaskHistory> getHistoryFromTaskCode(String storeId, String taskCode, Pageable page)
      throws Exception {
    return this.taskHistoryRepository.findAllByStoreIdAndTaskCodeAndMarkForDeleteFalse(storeId,
        taskCode, page);
  }

  @Override
  public PDTHistoryEventModel generatePDTHistoryEventModel(String storeId, String executor, Product product,
      Vendor vendor, String reason, WorkflowState state, String taskCode) {
    PDTHistoryEventModel pdtHistoryEventModel = new PDTHistoryEventModel();
    pdtHistoryEventModel.setProductName(product.getProductName());
    pdtHistoryEventModel.setProductCode(product.getProductCode());
    pdtHistoryEventModel.setCategoryCode(product.getCategoryCode());
    pdtHistoryEventModel.setCategoryName(product.getCategoryName());
    pdtHistoryEventModel.setVendor(vendor);
    pdtHistoryEventModel.setReason(reason);
    pdtHistoryEventModel.setState(state);
    pdtHistoryEventModel.setStoreId(storeId);
    pdtHistoryEventModel.setExecutor(executor);
    pdtHistoryEventModel.setTaskCode(taskCode);
    return pdtHistoryEventModel;
  }

  private void publishInternalHistoryEvent(Product product, WorkflowState state, String reason) {
    kafkaProducer.send(kafkaTopicProperties.getInternalHistoryEventName(), product.getProductCode(),
      ConverterUtil.convertToInternalHistoryModel(product.getStoreId(), product.getProductCode(),
        GdnMandatoryRequestParameterUtil.getUsername(), state.getDesc(), reason));
  }
}
