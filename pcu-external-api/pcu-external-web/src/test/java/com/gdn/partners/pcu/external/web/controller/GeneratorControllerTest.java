package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.GeneratorApiPath;
import com.gdn.partners.pcu.external.service.GeneratorService;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;


public class GeneratorControllerTest extends TestHelper {

  private static final String PRODUCT_CODE = "productCode";
  private static final String BAR_CODE = "15449519286397394";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final double LENGTH = 10;
  private static final double WIDTH = 10;
  private static final double HEIGHT = 10;
  private static final double WEIGHT = 10;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private GeneratorService generatorService;

  @InjectMocks
  private GeneratorController generatorController;


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.generatorController).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(mandatoryParameterHelper);
    verifyNoMoreInteractions(generatorService);
  }

  @Test
  public void generateProductCode_Test() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(generatorService.generateProductCode()).thenReturn(PRODUCT_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        get(GeneratorApiPath.BASE_PATH + GeneratorApiPath.GENERATE_PRODUCT_CODE).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
    .andExpect(jsonPath("$.value",equalTo(PRODUCT_CODE)));
    verify(generatorService).generateProductCode();
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void generateBarCode_Test() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(generatorService.generateBarCode()).thenReturn(BAR_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        get(GeneratorApiPath.BASE_PATH + GeneratorApiPath.GENERATE_BAR_CODE).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", equalTo(BAR_CODE)));
    verify(generatorService).generateBarCode();
    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void generateShippingWeightTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(generatorService.generateShippingWeight(CATEGORY_CODE, LENGTH, WIDTH, HEIGHT, HEIGHT)).thenReturn(10.0);
    MockHttpServletRequestBuilder requestBuilder =
        get(GeneratorApiPath.BASE_PATH + GeneratorApiPath.GENERATE_SHIPPING_WEIGHT, CATEGORY_CODE)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession())
            .param("length", String.valueOf(LENGTH)).param("width", String.valueOf(WIDTH))
            .param("height", String.valueOf(HEIGHT)).param("weight", String.valueOf(WEIGHT));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", equalTo(10.0)));
    verify(generatorService).generateShippingWeight(CATEGORY_CODE, LENGTH, WIDTH, HEIGHT, WEIGHT);
    verify(mandatoryParameterHelper).getRequestId();
  }
}
