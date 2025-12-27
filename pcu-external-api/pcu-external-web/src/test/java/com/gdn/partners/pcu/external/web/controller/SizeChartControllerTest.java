package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.SizeChartApiPath;
import com.gdn.partners.pcu.external.service.SizeChartService;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import com.gdn.partners.pcu.external.web.model.request.ProductSizeChartUpdateWebRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.Set;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;


public class SizeChartControllerTest extends TestHelper {
  private static final String SIZE_CHART_CODE = "SIZE_CHART_CODE";
  private static final String PRODUCT_SKU_1 = "PRODUCT_SKU_1";
  private static final String PRODUCT_SKU_2 = "PRODUCT_SKU_2";

  private ProductSizeChartUpdateWebRequest productSizeChartUpdateWebRequest;

  @Mock
  private SizeChartService sizeChartService;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @InjectMocks
  private SizeChartController sizeChartController;

  @BeforeEach
  public void init() {
    MockitoAnnotations.openMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(sizeChartController).build();
    productSizeChartUpdateWebRequest =
        new ProductSizeChartUpdateWebRequest(Set.of(PRODUCT_SKU_1), Set.of(PRODUCT_SKU_2));
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(sizeChartService, mandatoryParameterHelper);
  }

  @Test
  public void updateProductSizeChartCodeTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    doNothing().when(sizeChartService).updateProductSizeChart(SIZE_CHART_CODE, productSizeChartUpdateWebRequest);

    MockHttpServletRequestBuilder requestBuilder =
        post(SizeChartApiPath.BASE_PATH + SizeChartApiPath.UPDATE_PRODUCT_SIZE_CHART, SIZE_CHART_CODE).accept(
                MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(productSizeChartUpdateWebRequest)).param("storeId", Constants.STORE_ID)
            .param("channelId", Constants.CHANNEL_ID).param("clientId", Constants.CLIENT_ID)
            .param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());

    verify(mandatoryParameterHelper, times(2)).getRequestId();
    verify(sizeChartService).updateProductSizeChart(SIZE_CHART_CODE, productSizeChartUpdateWebRequest);
  }
}