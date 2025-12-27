package com.gdn.partners.pbp.workflow.product;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;

public class CreateWorkflowWorkerTest {

  private static final String bulkUploadType = "UNIFIED_BULK_UPLOAD";

  @Mock
  private ProductLevel1WipService productLevel1WipService;

  @Mock
  private ProductLevel1HistoryService productLevel1HistoryService;

  @InjectMocks
  private CreateWorkflowWorkerBean createWorkflowWorkerBean;

  private ProductRequest generateProductRequest() throws Exception {
    ProductRequest productRequest = new ProductRequest();
    return productRequest;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    Mockito.doNothing().when(this.productLevel1WipService)
        .create(Mockito.anyString(), Mockito.anyString(), Mockito.eq(bulkUploadType), Mockito.any(ProductRequest.class));
    Mockito.doNothing().when(this.productLevel1HistoryService)
        .create(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productLevel1WipService);
    Mockito.verifyNoMoreInteractions(this.productLevel1HistoryService);
  }

  @Test
  public void processTest() throws Exception {
    Map<String, Object> datas = new HashMap<String, Object>();
    datas.put("request", this.generateProductRequest());
    datas.put("productCreationType", ProductCreationType.UNIFIED_BULK_UPLOAD.getProductCreationType());
    this.createWorkflowWorkerBean.process(datas);
    Mockito.verify(this.productLevel1WipService)
        .create(Mockito.any(), Mockito.any(), Mockito.eq(bulkUploadType),
            Mockito.any(ProductRequest.class));
    Mockito.verify(this.productLevel1HistoryService).create((String) Mockito.any(), Mockito.any(),
        Mockito.any());
  }

}
