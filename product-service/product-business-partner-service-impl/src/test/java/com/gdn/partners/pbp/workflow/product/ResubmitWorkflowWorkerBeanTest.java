package com.gdn.partners.pbp.workflow.product;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.gdn.partners.pbp.model.productlevel3wip.UpdateProductLevel3Wip;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;

/**
 * Created by Vishal on 14/05/18.
 */
public class ResubmitWorkflowWorkerBeanTest {

  @Mock
  private ProductLevel1WipService productLevel1WipService;

  @Mock
  private ProductLevel3WipService productLevel3WipService;

  @InjectMocks
  private ResubmitWorkflowWorkerBean resubmitWorkflowWorkerBean;

  private Map<String, Object> data;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    data = new HashMap<>();
    data.put("productRequest", new ProductRequest());
    data.put("updateProductLevel3Wip", new UpdateProductLevel3Wip());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productLevel1WipService, productLevel3WipService);
  }

  @Test
  public void process() throws Exception {
    Mockito.doNothing().when(productLevel1WipService)
        .resubmit(Mockito.any(ProductRequest.class), Mockito.any(Date.class));
    Mockito.doNothing().when(productLevel3WipService)
        .resubmit(Mockito.any(ProductRequest.class), Mockito.any(UpdateProductLevel3Wip.class),
            Mockito.any(Date.class));
    this.resubmitWorkflowWorkerBean.process(data);
    Mockito.verify(productLevel1WipService)
        .resubmit(Mockito.any(ProductRequest.class), Mockito.any(Date.class));
    Mockito.verify(productLevel3WipService)
        .resubmit(Mockito.any(ProductRequest.class), Mockito.any(UpdateProductLevel3Wip.class),
            Mockito.any(Date.class));
  }

  @Test
  public void process_fail_whenThrowsExceptionForProductLevel3WipService() throws Exception {
    Mockito.doNothing().when(productLevel1WipService)
        .resubmit(Mockito.any(ProductRequest.class), Mockito.any(Date.class));
    Mockito.doThrow(new Exception()).when(productLevel3WipService)
        .resubmit(Mockito.any(ProductRequest.class), Mockito.any(UpdateProductLevel3Wip.class),
            Mockito.any(Date.class));
    try {
      this.resubmitWorkflowWorkerBean.process(data);
    } catch (Exception e) {
      Mockito.verify(productLevel1WipService)
          .resubmit(Mockito.any(ProductRequest.class), Mockito.any(Date.class));
      Mockito.verify(productLevel3WipService)
          .resubmit(Mockito.any(ProductRequest.class), Mockito.any(UpdateProductLevel3Wip.class),
              Mockito.any(Date.class));
    }
  }

  @Test
  public void process_fail_whenThrowsExceptionForProductLevel1WipService() throws Exception {
    Mockito.doThrow(new Exception()).when(productLevel1WipService)
        .resubmit(Mockito.any(ProductRequest.class), Mockito.any(Date.class));
    try {
      this.resubmitWorkflowWorkerBean.process(data);
    } catch (Exception e) {
      Mockito.verify(productLevel1WipService)
          .resubmit(Mockito.any(ProductRequest.class), Mockito.any(Date.class));
    }
  }

}
