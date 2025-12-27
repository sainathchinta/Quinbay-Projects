package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.SellerLogisticsApiPath;
import com.gdn.partners.pcu.external.service.SellerLogisticsService;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import com.gdn.partners.pcu.external.web.model.response.ExcelSkuUpdateStatusResponse;
import com.gdn.partners.pcu.external.web.model.response.LogisticsDownloadTemplateResponse;
import com.gdn.partners.pcu.external.web.model.response.LogisticsExcelSkuUploadResponse;
import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;

import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;


public class SellerLogisticsControllerTest extends TestHelper {

  private static final String BUSINESS_PARTNER_ID = "business-partner-id";
  private static final String MERCHANT_DELIEVERY_TYPE = "merchantDeliveryType";
  private static final String PICKUP = "PICKUP";
  private static final String DUMMY_FILE_NAME = "dummy-excel.xls";
  private static final String FILE_FOLDER = "Product";

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private SellerLogisticsService sellerLogisticsService;

  @InjectMocks
  private SellerLogisticsController sellerLogisticsController;



  private MockMultipartFile multipartFile;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.sellerLogisticsController).build();
    multipartFile = new MockMultipartFile("request", generateDummyExcelMultipartFile().getBytes());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(BUSINESS_PARTNER_ID);
    when(sellerLogisticsService.getSellerLogisticsProduct(BUSINESS_PARTNER_ID, PICKUP))
        .thenReturn(new ArrayList<>());
    when(sellerLogisticsService.getTemplateData(BUSINESS_PARTNER_ID, PICKUP))
        .thenReturn(new LogisticsDownloadTemplateResponse());
    when(sellerLogisticsService.excelUpload(multipartFile, BUSINESS_PARTNER_ID, PICKUP))
        .thenReturn(new LogisticsExcelSkuUploadResponse());
    when(sellerLogisticsService.excelUploadStatus(BUSINESS_PARTNER_ID))
        .thenReturn(new ExcelSkuUpdateStatusResponse());

  }

  @Test
  public void getSellerLogisticsProductTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(SellerLogisticsApiPath.BASE_PATH + SellerLogisticsApiPath.GET_SELER_LOGISTICS_PRODUCT)
            .param(MERCHANT_DELIEVERY_TYPE, PICKUP).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(sellerLogisticsService).getSellerLogisticsProduct(BUSINESS_PARTNER_ID, PICKUP);
  }

  @Test
  public void getTemplateDataTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(SellerLogisticsApiPath.BASE_PATH + SellerLogisticsApiPath.GET_TEMPLATE_DATA)
            .param(MERCHANT_DELIEVERY_TYPE, PICKUP).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(sellerLogisticsService).getTemplateData(BUSINESS_PARTNER_ID, PICKUP);
  }

  @Test
  public void excelUploadTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder = multipart(SellerLogisticsApiPath.BASE_PATH
        + SellerLogisticsApiPath.UPLOAD_LOGISTICS_TEMPLATE_UPDATE).file(multipartFile)
        .param(MERCHANT_DELIEVERY_TYPE, PICKUP).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(sellerLogisticsService).excelUpload(multipartFile, BUSINESS_PARTNER_ID, PICKUP);
  }

  private MultipartFile generateDummyExcelMultipartFile() throws Exception {
    File file = generateDummyExcelFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile =
        new MockMultipartFile("dummy-excel", DUMMY_FILE_NAME, null, fileData);
    return multipartFile;
  }

  private File generateDummyExcelFile() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file =
        new File(classLoader.getResource(FILE_FOLDER + File.separator + DUMMY_FILE_NAME).getFile());
    return file;
  }

  @Test
  public void excelUploadStatusTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(SellerLogisticsApiPath.BASE_PATH + SellerLogisticsApiPath.GET_UPLOAD_STATUS)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", is(true)));
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(sellerLogisticsService).excelUploadStatus(BUSINESS_PARTNER_ID);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(sellerLogisticsService);
    verifyNoMoreInteractions(mandatoryParameterHelper);
  }
}
