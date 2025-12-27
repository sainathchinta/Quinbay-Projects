package com.gdn.partners.pbp.workflow.product;

import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by vishal on 18/07/17.
 */
public class UpdateRejectedWorkflowWorkerBeanTest {


  private static final String DEFAULT_PRODUCT_CODE = "product-code";
  private static final String DEFAULT_PROCESS_CODE  = "process-code";

  @InjectMocks
  UpdateRejectedWorkflowWorkerBean updateRejectedWorkflowWorkerBean;

  @Mock
  private ProductLevel1WipService productLevel1WipService;

  @Mock
  private ProductLevel1HistoryService productLevel1HistoryService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productLevel1HistoryService);
    Mockito.verifyNoMoreInteractions(productLevel1WipService);
  }

  @Test
  public void testProcess_success() throws Exception {
    Map<String, Object> map = new HashMap<>();
    map.put("productCode", DEFAULT_PRODUCT_CODE);
    map.put("processCode", DEFAULT_PROCESS_CODE);
    updateRejectedWorkflowWorkerBean.process(map);
    Mockito.verify(productLevel1WipService)
        .updateRejectedProduct(Mockito.eq(DEFAULT_PRODUCT_CODE));
    Mockito.verify(productLevel1HistoryService)
        .create(Mockito.eq(DEFAULT_PRODUCT_CODE), Mockito.eq(DEFAULT_PROCESS_CODE),
            Mockito.any());
  }

  @Test
  public void testProcess_fail() throws Exception {
    Map<String, Object> map = new HashMap<>();
    map.put("productCode", DEFAULT_PRODUCT_CODE);
    map.put("processCode", DEFAULT_PROCESS_CODE);
    map.put("request", new ProductRequest());
    Mockito.doThrow(new Exception()).when(productLevel1WipService)
        .updateRejectedProduct(Mockito.eq(DEFAULT_PRODUCT_CODE));
    try {
      updateRejectedWorkflowWorkerBean.process(map);
    } catch (Exception e) {
      Mockito.verify(productLevel1WipService)
          .updateRejectedProduct(Mockito.eq(DEFAULT_PRODUCT_CODE));
    }
  }
}
