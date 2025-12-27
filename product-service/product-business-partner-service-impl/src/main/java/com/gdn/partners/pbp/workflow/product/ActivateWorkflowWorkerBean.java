package com.gdn.partners.pbp.workflow.product;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;

import org.apache.log4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.service.ProductMailEventService;
import com.gdn.mta.product.service.ProductService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.entity.mailEvent.ProductMailEventsEnum;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.partners.pbp.workflow.WorkflowWorker;

@Component(value = ActivateWorkflowWorkerBean.BEAN_NAME + WorkflowWorker.SUFFIX_BEAN_NAME)
@Transactional(readOnly = true)
public class ActivateWorkflowWorkerBean implements WorkflowWorker {

  public static final String BEAN_NAME = "activate";
  
  private static final String PRODUCT_CODE = "productCode";
  private static final String PROCESS_CODE = "processCode";
  private static final String SYSTEM = "System";
  private static final String IS_SKIP_NOTIFICATION = "IS_SKIP_NOTIFICATION";

  @Autowired
  private ProductLevel1WipService productLevel1WipService;

  @Autowired
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductMailEventService productMailEventService;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void process(Map<String, Object> datas) throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, SYSTEM);
    String productCode = String.valueOf(datas.get(PRODUCT_CODE));
    String processCode = String.valueOf(datas.get(PROCESS_CODE));
    String storeId = String.valueOf(datas.get(Constants.STORE_ID));
    boolean isSkipNotification = Boolean.valueOf(String.valueOf(datas.get(IS_SKIP_NOTIFICATION)));
    ProductCollection productCollection =
        productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    this.productLevel1WipService.activate(productCode);
    if(!productCollection.isPostLive() || productCollection.isReviewPending()) {
      this.productService.createProductLevel3(productCode, isSkipNotification);
    }
    this.productLevel1HistoryService.create(productCode, processCode, null);
    SimpleDateFormat dateFormat = new SimpleDateFormat("dd MMM yyyy hh:mm:ss");
    if(productCollection.isPostLive() && productCollection.isReviewPending()) {
      this.productMailEventService.createAndSaveMailEvent(productCode , dateFormat.format(new Date()),
          ProductMailEventsEnum.POST_LIVE_REVIEW_APPROVED);
    } else if(!productCollection.isPostLive()) {
      this.productMailEventService.createAndSaveMailEvent(productCode, dateFormat.format(new Date()),
          ProductMailEventsEnum.APPROVED);
    }
  }
}
