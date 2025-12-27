package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3AttributeWipResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3ItemWipResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipDetailResponse;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3WipDetailWebResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;


public class ProductLevel3WipServiceImplTest {

  @InjectMocks
  private ProductLevel3WipServiceImpl productLevel3WipService;

  @Mock
  private PBPFeign pbpFeign;

  private static final String GDN_PRODUCT_SKU = "gdn-product-sku";
  private static final String REQUEST_ID = "requestId";
  private GdnRestSingleResponse<ProductLevel3WipDetailResponse> response;
  private ProductLevel3WipDetailResponse productLevel3WipDetailResponse;
  private List<ProductLevel3ItemWipResponse> productLevel3ItemWipResponses;
  private List<ProductLevel3AttributeWipResponse> productLevel3AttributeWipResponses;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ProductLevel3ItemWipResponse productLevel3ItemWipResponse1 = new ProductLevel3ItemWipResponse();
    ProductLevel3ItemWipResponse productLevel3ItemWipResponse2 = new ProductLevel3ItemWipResponse();
    productLevel3ItemWipResponses = new ArrayList<>();
    productLevel3ItemWipResponses.add(productLevel3ItemWipResponse1);
    productLevel3ItemWipResponses.add(productLevel3ItemWipResponse2);

    ProductLevel3AttributeWipResponse productLevel3AttributeWipResponse1 = new ProductLevel3AttributeWipResponse();
    ProductLevel3AttributeWipResponse productLevel3AttributeWipResponse2 = new ProductLevel3AttributeWipResponse();
    productLevel3AttributeWipResponses = new ArrayList<>();
    productLevel3AttributeWipResponses.add(productLevel3AttributeWipResponse1);
    productLevel3AttributeWipResponses.add(productLevel3AttributeWipResponse2);

    productLevel3WipDetailResponse = new ProductLevel3WipDetailResponse();
    productLevel3WipDetailResponse.setItems(productLevel3ItemWipResponses);
    productLevel3WipDetailResponse.setAttributes(productLevel3AttributeWipResponses);
    productLevel3WipDetailResponse.setProductSku(GDN_PRODUCT_SKU);

    response = new GdnRestSingleResponse<>(productLevel3WipDetailResponse, REQUEST_ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pbpFeign);
  }

  @Test
  public void findProductDetailByProductSkuTest() {
    Mockito.when(pbpFeign.getProductDetailByProductSku(GDN_PRODUCT_SKU, false)).thenReturn(response);
    ProductLevel3WipDetailWebResponse productLevel3WipDetailWebResponse =
        productLevel3WipService.findProductDetailByProductSku(GDN_PRODUCT_SKU, false);
    Mockito.verify(pbpFeign).getProductDetailByProductSku(GDN_PRODUCT_SKU, false);
    assertNotNull(productLevel3WipDetailWebResponse);
    assertEquals(productLevel3WipDetailWebResponse.getProductSku(), GDN_PRODUCT_SKU);
    assertEquals(productLevel3WipDetailWebResponse.getAttributes().size(), 2);
    assertEquals(productLevel3WipDetailWebResponse.getItems().size(), 2);
  }

  @Test
  public void findProductDetailByProductSkuTest_whenPBPClientExceptionTest() {
    ProductLevel3WipDetailWebResponse productLevel3WipDetailWebResponse = null;
    Mockito.when(pbpFeign.getProductDetailByProductSku(GDN_PRODUCT_SKU, true)).thenReturn(null);
    try {
      productLevel3WipDetailWebResponse = productLevel3WipService.findProductDetailByProductSku(GDN_PRODUCT_SKU, true);
    } catch (ClientException ex) {

    } finally {
      Mockito.verify(pbpFeign).getProductDetailByProductSku(GDN_PRODUCT_SKU, true);
      assertNull(productLevel3WipDetailWebResponse);
    }
  }
}