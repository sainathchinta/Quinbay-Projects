package com.gdn.partners.pbp.workflow.product;

import java.util.Map;

import com.gdn.mta.product.service.ProductMailEventService;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailEventsEnum;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;
import com.gdn.partners.pbp.workflow.WorkflowWorker;

@Component(value = DeleteWorkflowWorkerBean.BEAN_NAME + WorkflowWorker.SUFFIX_BEAN_NAME)
@Transactional(readOnly = true)
public class DeleteWorkflowWorkerBean implements WorkflowWorker {

  public static final String BEAN_NAME = "delete";

  private static final String CODE = "code";
  private static final String DELETE = "DELETE";
  private static final String NOTES = "notes";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PROCESS_CODE = "processCode";
  private static final String NEED_EMAIL_NOTIFICATION = "needEmailNotification";

  @Autowired
  private ProductLevel3WipService productLevel3WipService;

  @Autowired
  private ProductLevel1WipService productLevel1WipService;

  @Autowired
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void process(Map<String, Object> datas) throws Exception {
    String productCode = String.valueOf(datas.get(PRODUCT_CODE));
    String processCode = String.valueOf(datas.get(PROCESS_CODE));
    String notes = String.valueOf(datas.get(NOTES));
    this.productLevel1HistoryService.create(productCode, processCode, notes);
    this.productLevel3WipService.delete(productCode);
    this.productLevel1WipService.delete(productCode, notes);
  }
}
