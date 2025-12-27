package com.gdn.partners.pcu.external.web.controller.util;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.BrandAuthApipath;
import com.gdn.partners.pcu.external.service.BrandAuthService;
import com.gdn.partners.pcu.external.web.model.request.BulkBrandData;
import com.gdn.partners.pcu.external.web.model.request.BulkBrandDataRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import jakarta.servlet.http.HttpServletResponse;
import java.util.Arrays;
import java.util.List;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

public class BrandAuthControllerTest {

  @InjectMocks
  BrandAuthController brandAuthController;

  @Mock
  BrandAuthService brandAuthService;

  @Mock
  MandatoryParameterHelper mandatoryParameterHelper;

  private MockMvc mockMvc;

  private static final String REQUEST_ID = "requestId";
  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_NAME = "brandName";
  private static final String SELLER_CODE = "sellerCode";
  private static final String SELLER_NAME = "sellerName";
  private static final Long BRAND_AUTH_START_DATE = 202314L;
  private static final Long BRAND_AUTH_END_DATE = 202314L;

  private BulkBrandData bulkBrandData;
  private BulkBrandDataRequest bulkBrandDataRequest;
  private ObjectMapper objectMapper = new ObjectMapper();

  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
    mockMvc = standaloneSetup(this.brandAuthController).build();
    bulkBrandData =
        new BulkBrandData(BRAND_CODE, BRAND_NAME, SELLER_CODE, SELLER_NAME, BRAND_AUTH_START_DATE, BRAND_AUTH_END_DATE);
    List<BulkBrandData> bulkBrandDataList = Arrays.asList(bulkBrandData);
    bulkBrandDataRequest = new BulkBrandDataRequest();
    bulkBrandDataRequest.setDownloadRequest(bulkBrandDataList);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(brandAuthService);
    Mockito.verifyNoMoreInteractions(mandatoryParameterHelper);
  }

  @Test
  public void selectedBrandBulkDownloadTest() throws Exception {
    String requestBody = objectMapper.writeValueAsString(bulkBrandDataRequest);
    MockHttpServletRequestBuilder requestBuilder =
        post(BrandAuthApipath.BASE_PATH + BrandAuthApipath.SELECTED_BRAND_BULK_DOWNLOAD)
            .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(requestBody);

    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    Mockito.verify(brandAuthService)
        .selectedBrandBulkDownload(Mockito.any(HttpServletResponse.class), Mockito.any(BulkBrandDataRequest.class));
    Mockito.verify(mandatoryParameterHelper).getUsername();
  }
}