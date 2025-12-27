package com.gdn.partners.pbp.workflow.product;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.workflow.WorkflowWorker;

@Component(value = ApproveQCWorkflowWorkerBean.BEAN_NAME + WorkflowWorker.SUFFIX_BEAN_NAME)
@Transactional(readOnly = true)
public class ApproveQCWorkflowWorkerBean implements WorkflowWorker {

  public static final String BEAN_NAME = "approveQC";
  
  private static final String PRODUCT_CODE = "productCode";
  private static final String PROCESS_CODE = "processCode";
  
  @Autowired
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void process(Map<String, Object> datas) throws Exception {
    String productCode = String.valueOf(datas.get(PRODUCT_CODE));
    String processCode = String.valueOf(datas.get(PROCESS_CODE));
    this.productLevel1HistoryService.create(productCode, processCode, null);
  }

}
