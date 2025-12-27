package com.gdn.x.mta.distributiontask.service.impl;

import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.mta.distributiontask.service.api.ProductBusinessPartnerService;
import com.gdn.x.mta.distributiontask.service.api.ProductDistributionTaskService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.ProductWipService;
import com.gdn.x.mta.distributiontask.service.api.TaskHistoryService;

/**
 * Created by Vishal on 15/05/18.
 */
@Service
@Slf4j
public class ProductWipServiceImpl implements ProductWipService{

  private static final String STORE_ID = "10001";

  @Autowired
  private ProductService productService;
  @Autowired
  private ProductDistributionTaskService productDistributionTaskService;
  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;
  @Autowired
  private TaskHistoryService taskHistoryService;

  @Override
  @Transactional(rollbackFor = Exception.class)
  public void deleteProductWip(String productCode, String notes)
      throws Exception {
    String requestId = MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER);
    String updatedBy = MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
    RejectProductDTO rejectProductDTO = new RejectProductDTO(productCode, notes, null);
    log.info("deleting product WIP from PBP : productCode : {}", productCode);
    this.productBusinessPartnerService
        .deleteProductCollection(requestId, updatedBy, false, rejectProductDTO);
    Product product = productService.getProductByCode(productCode);
    if (product != null) {
      log.info("deleting product WIP from PDT : productCode : {}", productCode);
      productService.removeProductWithMarkForDelete(product, StringUtils.EMPTY);
    }
    ProductDistributionTask productDistributionTask = this.productDistributionTaskService
        .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(productCode);
    if (productDistributionTask != null) {
      log.info("deleting product distribution from PDT : productCode : {}", productCode);
      productDistributionTask.setMarkForDelete(true);
      productDistributionTaskService.saveProductDistributionTask(productDistributionTask);
      this.taskHistoryService.createTaskHistory(STORE_ID, updatedBy, product,
          productDistributionTask.getVendor(), notes, WorkflowState.DELETED,
          productDistributionTask.getTaskCode());
    }
  }
}
