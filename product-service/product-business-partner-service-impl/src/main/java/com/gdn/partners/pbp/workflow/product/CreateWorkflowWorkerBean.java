package com.gdn.partners.pbp.workflow.product;

import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.partners.pbp.workflow.WorkflowWorker;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;

@Component(value = CreateWorkflowWorkerBean.BEAN_NAME + WorkflowWorker.SUFFIX_BEAN_NAME)
//@Transactional(readOnly = true)
public class CreateWorkflowWorkerBean implements WorkflowWorker {

  public static final String BEAN_NAME = "create";
  
  private static final String REQUEST = "request";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PROCESS_CODE = "processCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String PRODUCT_CREATION_TYPE = "productCreationType";
  
  @Autowired
  private ProductLevel1WipService productLevel1WipService;
  
  @Autowired
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Override
  //@Transactional(readOnly = false, rollbackFor = Exception.class)
  public void process(Map<String, Object> datas) throws Exception {
    String businessPartnerCode = String.valueOf(datas.get(BUSINESS_PARTNER_CODE));
    String businessPartnerName = String.valueOf(datas.get(BUSINESS_PARTNER_NAME));
    String productCode = String.valueOf(datas.get(PRODUCT_CODE));
    String processCode = String.valueOf(datas.get(PROCESS_CODE));
    String productCreationType = String.valueOf(datas.get(PRODUCT_CREATION_TYPE));
    ProductRequest request = (ProductRequest) datas.get(REQUEST);
    this.productLevel1WipService.create(businessPartnerCode, businessPartnerName, productCreationType, request);
    this.productLevel1HistoryService.create(productCode, processCode, null);
  }

}
