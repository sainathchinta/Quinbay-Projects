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

import com.gdn.mta.product.service.ProductService;
import com.gdn.partners.pbp.publisher.Publisher;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.x.productcategorybase.entity.Product;

@SuppressWarnings("unchecked")
public class CreateDirectWorkflowWorkerTest {

  private static final String NOTES = "notes";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_APPROVAL_STATUS = "APPROVED";

  @Mock
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Mock
  private ProductService productService;

  @Mock
  private Publisher publisher;

  @InjectMocks
  private CreateDirectWorkflowWorkerBean createDirectWorkflowWorkerBean;

  private Product generateProduct() throws Exception {
    Product product = new Product();
    return product;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    Mockito.doNothing().when(this.publisher).publish(Mockito.anyMap());
    Mockito.doNothing().when(this.productLevel1HistoryService)
        .create(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productService);
    Mockito.verifyNoMoreInteractions(this.publisher);
    Mockito.verifyNoMoreInteractions(this.productLevel1HistoryService);
  }

  @Test
  public void processTest() throws Exception {
    Map<String, Object> datas = new HashMap<String, Object>();
    datas.put("request", this.generateProduct());
    this.createDirectWorkflowWorkerBean.process(datas);
    /*Mockito.verify(this.productService).update(Mockito.any(Product.class));
    Mockito.verify(this.publisher).publish(Mockito.anyMap());*/
    Mockito.verify(this.productLevel1HistoryService).create((String) Mockito.any(), Mockito.any(), Mockito.any());
  }

}
