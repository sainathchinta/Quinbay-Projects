package com.gdn.partners.pbp.workflow.product;

import java.util.Map;

import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.service.ItemService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.stereotype.Component;

import com.gda.mta.product.dto.ProductCreationRequest;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.workflow.WorkflowWorker;

/**
 * Created by Akshay on 27/07/18.
 */
@Component(value = CreateProductDirectWorkflowWorkerBean.BEAN_NAME + WorkflowWorker.SUFFIX_BEAN_NAME)
public class CreateProductDirectWorkflowWorkerBean implements WorkflowWorker {

  private static final Logger LOGGER =
      LoggerFactory.getLogger(CreateProductDirectWorkflowWorkerBean.class);
  private static final String REQUEST = "request";
  public static final String BEAN_NAME = "createProductDirect";

  @Autowired
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Autowired
  private ProductWfService productWfService;

  @Autowired
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ItemService itemService;

  @Override
  public void process(Map<String, Object> datas) throws Exception {
    ProductCreationRequest request = (ProductCreationRequest) datas.get(REQUEST);
    WorkflowWorker workflowWorker = (WorkflowWorker) this.autowireCapableBeanFactory
        .getBean(CreateProductWorkflowWorkerBean.BEAN_NAME + WorkflowWorker.SUFFIX_BEAN_NAME);
    workflowWorker.process(datas);
    productRepository.updateActivated(request.getProductCode(), Boolean.TRUE);
    itemService.publishItemStatusEvent(request.getProductCode(), ProductStatus.ACTIVE);
    LOGGER.info("publish product data to PDT, productCode : {}", request.getProductCode());
  }
}
