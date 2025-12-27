package com.gdn.x.mta.distributiontask.service.impl;

import java.util.Date;
import java.util.List;

import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.api.ProductDistributionTaskService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.SlaCheckerService;
import com.gdn.x.mta.distributiontask.service.api.TaskHistoryService;
import com.gdn.x.mta.distributiontask.service.api.publisher.ProductPublisherService;

/**
 * @author febryo.lesmana
 */

@Service
@Slf4j
public class SlaCheckerServiceImpl implements SlaCheckerService {

  public static final String REJECTED_MESSAGE = "Rejected because more than SLA days";
  private static final String SYSTEM_USERNAME = "System";
  private static final boolean EXCEEDED_SLA = true;

  @Autowired
  private ProductDistributionTaskService productDistributionTaskService;

  @Autowired
  private ProductPublisherService productPublisherService;

  @Autowired
  private TaskHistoryService taskHistoryService;

  @Autowired
  private ProductService productService;

  @Override
  @Async
  public void execute() throws Exception {
    log.info("===== SlaCheckerServiceImpl Start =====");
    List<ProductDistributionTask> productDistributionTaskExceedSLAs =
        this.productDistributionTaskService.getListOfProductDistributionTaskSLA(new Date(), false);
    if (!CollectionUtils.isEmpty(productDistributionTaskExceedSLAs)) {
      log.info("SlaCheckerServiceImpl Executed size {}",
          productDistributionTaskExceedSLAs.size());
      for (ProductDistributionTask productDistributionTaskExceedSLA : productDistributionTaskExceedSLAs) {
        this.productPublisherService.publishEvent(productDistributionTaskExceedSLA);
      }
    }
    log.info("===== SlaCheckerServiceImpl End =====");
  }

  @Override
  @Transactional(readOnly = false)
  public void turnTaskToDistributionList(String taskId) throws Exception {
    log.info("EXECUTE SLA EXCEEDED TASK {}", taskId);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, SYSTEM_USERNAME);
    ProductDistributionTask productDistributionTask =
        this.productDistributionTaskService.findByIdMarkForDeleteFalseAndStillInReview(taskId);
    if (productDistributionTask.getState() == WorkflowState.IN_REVIEW) {
      this.productDistributionTaskService.removeProductDistributionTask(
          productDistributionTask.getProductId(), WorkflowState.EXCEEDED_SLA.name());
      this.productService.rejectAndDiscardProduct(productDistributionTask.getProductId(),
          EXCEEDED_SLA);
      this.productDistributionTaskService.slaExceedUpdate(productDistributionTask);
      this.taskHistoryService.createTaskHistory(productDistributionTask.getStoreId(),
          SYSTEM_USERNAME, productDistributionTask.getProduct(),
          productDistributionTask.getVendor(), REJECTED_MESSAGE, WorkflowState.EXCEEDED_SLA,
          productDistributionTask.getTaskCode());
    }
  }

}
