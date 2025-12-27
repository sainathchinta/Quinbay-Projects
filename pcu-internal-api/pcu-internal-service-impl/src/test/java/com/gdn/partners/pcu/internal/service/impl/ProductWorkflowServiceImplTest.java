package com.gdn.partners.pcu.internal.service.impl;

import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pbp.dto.workflow.product.ProductReturnForCorrectionRequest;
import com.gdn.partners.pcu.internal.client.feign.ProductWorkflowFeign;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

/**
 * Created by govind on 14/01/2019 AD.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ProductWorkflowServiceImplTest {


  @Mock
  private ProductWorkflowFeign productWorkflowFeign;

  @InjectMocks
  private ProductWorkflowServiceImpl productWorkflowService;

  private ProductReturnForCorrectionRequest productReturnForCorrectionRequest;

  @BeforeEach
  public void init(){
    productReturnForCorrectionRequest = new ProductReturnForCorrectionRequest();
  }

  @AfterEach
  public void tearDown(){
    verifyNoMoreInteractions(productWorkflowFeign);
  }

  @Test
  public void returnForCorrectionTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, null);
    Mockito.when(productWorkflowFeign.returnForCorrection(productReturnForCorrectionRequest))
        .thenReturn(response);
    productWorkflowService.returnForCorrection(productReturnForCorrectionRequest);
    Mockito.verify(productWorkflowFeign).returnForCorrection(productReturnForCorrectionRequest);
  }

  @Test
  public void returnForCorrection_WhenClientExceptionTest() {
    boolean isResponseInvalid = true;
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, false, null);
    Mockito.when(productWorkflowFeign.returnForCorrection(productReturnForCorrectionRequest))
        .thenReturn(response);
    try {
      this.productWorkflowService.returnForCorrection(productReturnForCorrectionRequest);
    } catch (ClientException ex) {
      isResponseInvalid = false;
    } finally {
      Mockito.verify(productWorkflowFeign).returnForCorrection(productReturnForCorrectionRequest);
      Assertions.assertFalse(isResponseInvalid);
    }
  }
}
