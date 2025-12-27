package com.gdn.partners.pcu.internal.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.DistributionListControllerPath;
import com.gdn.partners.pcu.internal.service.DistributionListService;
import com.gdn.partners.pcu.internal.web.TestApplication;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.model.request.DistributionFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductVendorWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.DistributionProductWebResponse;

@AutoConfigureMockMvc
public class DistributionListControllerTest extends TestHelper {

  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String VENDOR_CODE = "vendorCode";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";

  private DistributionFilterWebRequest distributionFilterWebRequest;
  private ProductVendorWebRequest productVendorWebRequest;

  @Mock
  private DistributionListService distributionListService;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @InjectMocks
  private DistributionListController distributionListController;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(this.distributionListController).build();

    distributionFilterWebRequest = new DistributionFilterWebRequest();
    productVendorWebRequest = new ProductVendorWebRequest();
    productVendorWebRequest.setVendorCode(VENDOR_CODE);
    productVendorWebRequest.setProductCodes(Arrays.asList(PRODUCT_CODE));
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(distributionListService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
  }


  @Test
  public void getDistributionProductListTest() throws Exception {
    Mockito.when(this.distributionListService.getSummaryByMultipleFilter(PAGE, SIZE, distributionFilterWebRequest))
        .thenReturn(new PageImpl<DistributionProductWebResponse>(Arrays.asList(new DistributionProductWebResponse())));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(DistributionListControllerPath.BASE_PATH + DistributionListControllerPath.PRODUCT_LIST)
            .param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE))
            .content(toJson(distributionListController)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.distributionListService).getSummaryByMultipleFilter(PAGE, SIZE, distributionFilterWebRequest);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void saveProductVendorMappingTest() throws Exception {
    Mockito.doNothing().when(this.distributionListService).saveProductVendorMapping(productVendorWebRequest);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(DistributionListControllerPath.BASE_PATH + DistributionListControllerPath.PRODUCT_VENDOR_MAPPING)
            .content(toJson(productVendorWebRequest)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.distributionListService).saveProductVendorMapping(productVendorWebRequest);
    verify(clientParameterHelper).getRequestId();
  }
}