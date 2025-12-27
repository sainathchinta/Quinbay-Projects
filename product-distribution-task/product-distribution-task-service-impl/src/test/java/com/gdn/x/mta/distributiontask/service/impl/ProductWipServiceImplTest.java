package com.gdn.x.mta.distributiontask.service.impl;

import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.mta.distributiontask.service.api.ProductBusinessPartnerService;
import com.gdn.x.mta.distributiontask.service.api.ProductDistributionTaskService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.TaskHistoryService;

/**
 * Created by Vishal on 16/05/18.
 */
public class ProductWipServiceImplTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String NOTE = "note";
  private static final String USER_NAME = "username";
  private static final String TASK_CODE = "taskCode";
  private static final String STORE_ID = "10001";
  private static final String REQUEST_ID = "requestId";

  @InjectMocks
  private ProductWipServiceImpl productWipService;
  @Mock
  private ProductService productService;
  @Mock
  private ProductDistributionTaskService productDistributionTaskService;
  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;
  @Mock
  private TaskHistoryService taskHistoryService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USER_NAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productService, productDistributionTaskService,
        productBusinessPartnerService, taskHistoryService);
    MDC.clear();
  }

  @Test
   void deleteProductWip_success() throws Exception {
    Product product = new Product();
    ProductDistributionTask distributionTask = generateProductDistributionTask();
    Mockito.when(productDistributionTaskService
        .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
        .thenReturn(distributionTask);
    Mockito.doNothing().when(productService)
        .removeProductWithMarkForDelete(Mockito.any(Product.class), Mockito.anyString());
    Mockito.doNothing().when(productDistributionTaskService)
        .saveProductDistributionTask(Mockito.any(ProductDistributionTask.class));
    Mockito.when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(product);
    Mockito.doNothing().when(taskHistoryService)
        .createTaskHistory(STORE_ID, USER_NAME, product, distributionTask.getVendor(), NOTE, null,
            TASK_CODE);
    Mockito.doNothing().when(productBusinessPartnerService)
        .deleteProductCollection(Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.eq(false),
            Mockito.any(RejectProductDTO.class));
    productWipService.deleteProductWip(PRODUCT_CODE, NOTE);
    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    product.setMarkForDelete(true);
    distributionTask.setMarkForDelete(true);
    Mockito.verify(productService).removeProductWithMarkForDelete(product, StringUtils.EMPTY);
    Mockito.verify(productDistributionTaskService)
        .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productDistributionTaskService).saveProductDistributionTask(distributionTask);
    Mockito.verify(taskHistoryService)
        .createTaskHistory(STORE_ID, USER_NAME, product, distributionTask.getVendor(), NOTE, WorkflowState.DELETED,
            TASK_CODE);
    Mockito.verify(productBusinessPartnerService)
        .deleteProductCollection(Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.eq(false),
            Mockito.any(RejectProductDTO.class));
  }

  @Test
   void deleteProductWip_withException() throws Exception {
    Mockito.doThrow(new Exception()).when(productBusinessPartnerService)
        .deleteProductCollection(Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.eq(false),
            Mockito.any(RejectProductDTO.class));
    try {
      productWipService.deleteProductWip(PRODUCT_CODE, NOTE);
    } catch (Exception e) {
      Mockito.verify(productBusinessPartnerService)
          .deleteProductCollection(Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.eq(false),
              Mockito.any(RejectProductDTO.class));
    }
  }

  @Test
   void deleteProductWip_withNoProduct() throws Exception {
    Mockito.when(productDistributionTaskService
        .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE))
        .thenReturn(null);
    Mockito.when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(null);
    Mockito.doNothing().when(productBusinessPartnerService)
        .deleteProductCollection(Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.eq(false),
            Mockito.any(RejectProductDTO.class));
    productWipService.deleteProductWip(PRODUCT_CODE, NOTE);
    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    Mockito.verify(productDistributionTaskService)
        .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerService)
        .deleteProductCollection(Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.eq(false),
            Mockito.any(RejectProductDTO.class));
  }

  @Test
   void deleteProductWip_withNoDistribution() throws Exception {
    Product product = new Product();
    Mockito.doNothing().when(productService)
        .removeProductWithMarkForDelete(Mockito.any(Product.class), Mockito.anyString());
    Mockito.when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(product);
    Mockito.doNothing().when(productBusinessPartnerService)
        .deleteProductCollection(Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.eq(false),
            Mockito.any(RejectProductDTO.class));
    productWipService.deleteProductWip(PRODUCT_CODE, NOTE);
    Mockito.verify(productService).getProductByCode(PRODUCT_CODE);
    product.setMarkForDelete(true);
    Mockito.verify(productService).removeProductWithMarkForDelete(product, StringUtils.EMPTY);
    Mockito.verify(productDistributionTaskService)
        .findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerService)
        .deleteProductCollection(Mockito.eq(REQUEST_ID), Mockito.eq(USER_NAME), Mockito.eq(false),
            Mockito.any(RejectProductDTO.class));
  }


  private ProductDistributionTask generateProductDistributionTask() {
    ProductDistributionTask distributionTask = new ProductDistributionTask();
    distributionTask.setTaskCode(TASK_CODE);
    distributionTask.setVendor(new Vendor());
    return distributionTask;
  }

}
