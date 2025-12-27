package com.gdn.partners.pbp.workflow.product;

import java.util.Date;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3Wip;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;
import com.gdn.partners.pbp.workflow.WorkflowWorker;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;

@Component(value = ResubmitWorkflowWorkerBean.BEAN_NAME + WorkflowWorker.SUFFIX_BEAN_NAME)
@Transactional(readOnly = true)
public class ResubmitWorkflowWorkerBean implements WorkflowWorker {

  public static final String BEAN_NAME = "resubmit";

  @Autowired
  private ProductLevel1WipService productLevel1WipService;

  @Autowired
  private ProductLevel3WipService productLevel3WipService;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void process(Map<String, Object> datas) throws Exception {
    ProductRequest productRequest = (ProductRequest) datas.get("productRequest");
    UpdateProductLevel3Wip updateProductLevel3Wip = (UpdateProductLevel3Wip) datas.get("updateProductLevel3Wip");
    Date submitDate = new Date();
    this.productLevel1WipService.resubmit(productRequest, submitDate);
    this.productLevel3WipService.resubmit(productRequest, updateProductLevel3Wip, submitDate);
  }
}
