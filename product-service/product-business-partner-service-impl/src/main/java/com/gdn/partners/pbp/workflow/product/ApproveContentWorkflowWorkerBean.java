package com.gdn.partners.pbp.workflow.product;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;
import com.gdn.partners.pbp.workflow.WorkflowWorker;

@Component(value = ApproveContentWorkflowWorkerBean.BEAN_NAME + WorkflowWorker.SUFFIX_BEAN_NAME)
@Transactional(readOnly = true)
public class ApproveContentWorkflowWorkerBean implements WorkflowWorker {

  public static final String BEAN_NAME = "approveContent";
  
  private static final String PRODUCT_CODE = "productCode";
  private static final String PROCESS_CODE = "processCode";

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
    this.productLevel3WipService.update(productCode);
    this.productLevel1WipService.approveContent(productCode);
    this.productLevel1HistoryService.create(productCode, processCode, null);
  }

}
