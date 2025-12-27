package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.QRApiPath;
import com.gdn.partners.pcu.external.service.QRService;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import com.gdn.partners.pcu.external.web.model.enums.TemplateSize;
import com.gdn.partners.pcu.external.web.model.request.QRDownloadWebRequest;
import com.gdn.partners.pcu.external.web.model.request.QRGenerateRequest;
import com.gdn.partners.pcu.external.web.model.request.QRProductWebRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import java.util.Arrays;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

public class QRControllerTest extends TestHelper {

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @InjectMocks
  private QRController qrController;

  @Mock
  private QRService qrService;

  private static final String PATH = "path";
  private static final String MERCHANT_CODE = "MTA-Dummy-1200";
  private static final String MERCHANT_NAME = "Dummy Merchant Name";

  private QRGenerateRequest qrGenerateRequest;
  private QRDownloadWebRequest qrDownloadWebRequest;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.qrController).build();
    qrGenerateRequest = new QRGenerateRequest();
    qrGenerateRequest.setMerchantCode(MERCHANT_CODE);
    qrGenerateRequest.setMerchantName(MERCHANT_NAME);
    qrGenerateRequest.setTemplateSize(TemplateSize.A1);

    qrDownloadWebRequest = new QRDownloadWebRequest();
    qrDownloadWebRequest.setMerchantCode(MERCHANT_CODE);
    qrDownloadWebRequest.setType("pdf");
    qrDownloadWebRequest.setProducts(Arrays.asList(new QRProductWebRequest()));
  }

  @Test
  public void generateQRTest() throws Exception {
    Mockito.when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(qrService.getPathOfImage(qrGenerateRequest)).thenReturn(PATH);
    MockHttpServletRequestBuilder requestBuilder =
        put(QRApiPath.BASE_PATH + QRApiPath.GENERATE).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession())
            .content(toJson(qrGenerateRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.qrService).getPathOfImage(qrGenerateRequest);
    verify(this.mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void merchantTemplateDownloadTest() throws Exception {
    Mockito.when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(qrService.merchantTemplateDownload(qrGenerateRequest)).thenReturn(PATH);
    MockHttpServletRequestBuilder requestBuilder =
        put(QRApiPath.BASE_PATH + QRApiPath.MERCHANT_TEMPLATE_DOWNLOAD).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession())
            .content(toJson(qrGenerateRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.qrService).merchantTemplateDownload(qrGenerateRequest);
    verify(this.mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void deleteQRCodesTest() throws Exception {
    Mockito.when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.doNothing().when(qrService).deleteQRCodes(7);
    MockHttpServletRequestBuilder requestBuilder =
        delete(QRApiPath.BASE_PATH + QRApiPath.DELETE).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession()).param("days", "7");
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.qrService).deleteQRCodes(7);
    verify(this.mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void productsQRDownloadTest() throws Exception {
    Mockito.when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.doNothing().when(qrService).downloadQRCodesForProducts(qrDownloadWebRequest);
    MockHttpServletRequestBuilder requestBuilder =
        put(QRApiPath.BASE_PATH + QRApiPath.DOWNLOAD).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession())
            .content(toJson(qrDownloadWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.qrService).downloadQRCodesForProducts(qrDownloadWebRequest);
    verify(this.mandatoryParameterHelper).getRequestId();
  }
  
  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(qrService);
    verifyNoMoreInteractions(mandatoryParameterHelper);
  }
}
