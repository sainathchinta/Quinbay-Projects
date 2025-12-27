package com.gdn.partners.pbp.workflow.product;

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.partners.pbp.publisher.Publisher;
import com.gdn.partners.pbp.publisher.product.ProductPublisherBean;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.partners.pbp.workflow.WorkflowWorker;

@Component(value = ApproveDraftWorkflowWorkerBean.BEAN_NAME + WorkflowWorker.SUFFIX_BEAN_NAME)
@Transactional(readOnly = true)
public class ApproveDraftWorkflowWorkerBean implements WorkflowWorker {

  public static final String BEAN_NAME = "approveDraft";
  
  private static final String PRODUCT_CODE = "productCode";
  private static final String PROCESS_CODE = "processCode";

  @Autowired
  private ProductLevel1WipService productLevel1WipService;
  
  @Autowired
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Autowired
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Autowired
  @Qualifier(value = ProductPublisherBean.BEAN_NAME + Publisher.SUFFIX_BEAN_NAME)
  private Publisher publisher;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void process(Map<String, Object> datas) throws Exception {
    String productCode = String.valueOf(datas.get(PRODUCT_CODE));
    String processCode = String.valueOf(datas.get(PROCESS_CODE));
    ProductCollection productCollection = this.productLevel1WipService.approveDraft(productCode, null);
    solrReviewProductCollectionService.addProductToReviewProductCollection(productCollection);
    this.productLevel1HistoryService.create(productCode, processCode, null);
    Map<String, Object> publisherDatas = new HashMap<String, Object>();
    publisherDatas.put("appName", "PDT");
    publisherDatas.put("productCode", productCode);
    this.publisher.publish(publisherDatas);
  }

}
