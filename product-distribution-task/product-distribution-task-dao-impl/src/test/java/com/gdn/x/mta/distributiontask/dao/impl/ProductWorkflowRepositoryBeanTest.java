package com.gdn.x.mta.distributiontask.dao.impl;

import java.util.ArrayList;
import java.util.HashMap;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.MDC;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.dto.workflow.product.ProductWorkflowStatusResponse;
import com.gdn.x.mta.distributiontask.dao.api.feign.PBPFeign;

@ExtendWith(MockitoExtension.class)
public class ProductWorkflowRepositoryBeanTest {

  private static final String PRODUCT_CODE = "product-code";

  private static final String REQUEST_ID = "requestID";

  private static final String USERNAME = "username";

  @InjectMocks
  private ProductWorkflowRepositoryBean instance;

  @Mock
  private PBPFeign pbpFeign;

  private ProductWorkflowStatusResponse productWorkflowStatusResponse;

  @BeforeEach
  public void setUp() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
    productWorkflowStatusResponse =
        new ProductWorkflowStatusResponse(PRODUCT_CODE, new ArrayList<String>(),
            new HashMap<>());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.pbpFeign);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, "");
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, "");
  }

  @Test
   void testGetWorkflowStatus() throws Exception {
    Mockito.when(this.pbpFeign.getProductWorkflowStatus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(
        new GdnRestSingleResponse<ProductWorkflowStatusResponse>(productWorkflowStatusResponse, REQUEST_ID));
    ProductWorkflowStatusResponse response = this.instance.getWorkflowStatus(PRODUCT_CODE);
    Assertions.assertNotNull(response);
    Mockito.verify(this.pbpFeign)
        .getProductWorkflowStatus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
  }

  @Test
   void testGetWorkflowStatus_exception() throws Exception {
    Mockito.when(this.pbpFeign.getProductWorkflowStatus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(
        new GdnRestSingleResponse<ProductWorkflowStatusResponse>("ERROR", ErrorCategory.UNSPECIFIED.name(), false,
            productWorkflowStatusResponse, REQUEST_ID));
      Assertions.assertThrows(Exception.class,()->this.instance.getWorkflowStatus(PRODUCT_CODE));
      Mockito.verify(this.pbpFeign)
          .getProductWorkflowStatus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString());
  }

  @Test
   void testGetWorkflowWithoutGdnMandatoryParameterStatus() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, "");
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, "");
    Mockito.when(this.pbpFeign.getProductWorkflowStatus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString())).thenReturn(
        new GdnRestSingleResponse<ProductWorkflowStatusResponse>(productWorkflowStatusResponse, REQUEST_ID));
    ProductWorkflowStatusResponse response = this.instance.getWorkflowStatus(PRODUCT_CODE);
    Assertions.assertNotNull(response);
    Mockito.verify(this.pbpFeign)
        .getProductWorkflowStatus(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString());
  }
}
