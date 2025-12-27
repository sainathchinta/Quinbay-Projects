package com.gdn.partners.pbp.workflow.product;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;

public class ResubmitWorkflowWorkerTest {

  @InjectMocks
  private ResubmitWorkflowWorkerBean resubmitWorkflowWorkerBean;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductLevel1WipService productLevel1WipService;

  @Mock
  private ProductLevel3WipService productLevel3WipService;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void processTest() throws Exception {
    Map<String, Object> datas = new HashMap<>();
    datas.put("productRequest", new ProductRequest());
    this.resubmitWorkflowWorkerBean.process(datas);
  }
}
