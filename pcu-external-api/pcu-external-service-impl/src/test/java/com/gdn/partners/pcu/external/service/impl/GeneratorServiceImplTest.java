package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.x.productcategorybase.dto.response.CategoryShippingWeightResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class GeneratorServiceImplTest {
  private static final String PRODUCT_CODE = "productCode";
  private static final String REQUEST_ID = "requestId";
  private static final String BAR_CODE = "15449519286397394";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final double LENGTH = 10;
  private static final double WIDTH = 10;
  private static final double HEIGHT = 10;
  private static final double WEIGHT = 10;

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private PCBFeign pcbFeign;

  @InjectMocks
  private GeneratorServiceImpl generatorService;


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pbpFeign);
    Mockito.verifyNoMoreInteractions(pcbFeign);
  }

  @Test
  public void generateProductCode_Test() {
    Mockito.when(this.pbpFeign.generateProductCode())
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, PRODUCT_CODE));
    String response = generatorService.generateProductCode();
    Mockito.verify(this.pbpFeign).generateProductCode();
    Assertions.assertEquals(PRODUCT_CODE, response);
  }

  @Test
  public void generateBarCode_Test() {
    Mockito.when(this.pbpFeign.generateBarCode())
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, BAR_CODE));
    String response = generatorService.generateBarCode();
    Mockito.verify(this.pbpFeign).generateBarCode();
    Assertions.assertEquals(BAR_CODE, response);
  }

  @Test
  public void generateShippingWeightTest() {
    Mockito.when(this.pcbFeign.generateShippingWeight(CATEGORY_CODE, LENGTH, WIDTH, HEIGHT, WEIGHT))
        .thenReturn(new GdnRestSingleResponse<>(new CategoryShippingWeightResponse(10.0), REQUEST_ID));
    Double response = generatorService.generateShippingWeight(CATEGORY_CODE, LENGTH, WIDTH, HEIGHT, WEIGHT);
    Mockito.verify(this.pcbFeign).generateShippingWeight(CATEGORY_CODE, LENGTH, WIDTH, HEIGHT, WEIGHT);
    Assertions.assertEquals(10.0, response, 0);
  }

  @Test
  public void generateShippingWeightExceptionTest() {
    Mockito.when(this.pcbFeign.generateShippingWeight(CATEGORY_CODE, LENGTH, WIDTH, HEIGHT, WEIGHT)).thenReturn(null);
    try {
      generatorService.generateShippingWeight(CATEGORY_CODE, LENGTH, WIDTH, HEIGHT, WEIGHT);
    } catch (ClientException e) {
    } finally {
      Mockito.verify(this.pcbFeign).generateShippingWeight(CATEGORY_CODE, LENGTH, WIDTH, HEIGHT, WEIGHT);
    }
  }
}