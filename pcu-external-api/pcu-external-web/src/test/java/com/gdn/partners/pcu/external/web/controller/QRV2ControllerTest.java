package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.QRV2ApiPath;
import com.gdn.partners.pcu.external.service.QRV2Service;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import com.gdn.partners.pcu.external.web.model.enums.TemplateSize;
import com.gdn.partners.pcu.external.web.model.request.ManualQRCodeRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductDetailsRequest;
import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.List;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;


public class QRV2ControllerTest extends TestHelper {

  private static final String PRODUCT_SKU = "product-sku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String ITEM_SKU = "itemSku";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String DUMMY_FILE_NAME = "dummy-excel.xls";
  private static final String FILE_FOLDER = "Product";

  private ManualQRCodeRequest manualQRCodeRequest;
  private ProductDetailsRequest productDetailsRequest;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;
  @Mock
  private QRV2Service qrv2Service;


  @InjectMocks
  private QRV2Controller qrController;

  private MockMultipartFile multipartFile;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.qrController).build();
    productDetailsRequest = new ProductDetailsRequest();
    productDetailsRequest.setProductSku(PRODUCT_SKU);
    productDetailsRequest.setPickupPointCode(PICKUP_POINT_CODE);
    productDetailsRequest.setItemSku(ITEM_SKU);

    manualQRCodeRequest = new ManualQRCodeRequest();
    manualQRCodeRequest.setAllStores(false);
    manualQRCodeRequest.setIsDarkTheme(true);
    manualQRCodeRequest.setQrPerPage(1);
    manualQRCodeRequest.setTemplateSize(TemplateSize.A5);
    manualQRCodeRequest.setCncActivated(true);
  }

  private MultipartFile generateDummyExcelMultipartFile() throws Exception {
    File file = generateDummyExcelFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    return new MockMultipartFile("dummy-excel", DUMMY_FILE_NAME, null, fileData);
  }

  private File generateDummyExcelFile() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    return new File(classLoader.getResource(FILE_FOLDER + File.separator + DUMMY_FILE_NAME).getFile());
  }

  @Test
  public void checkQrGenerationAccessibleTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
        Constants.BUSINESS_PARTNER_CODE);
    when(qrv2Service.qrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE)).thenReturn(
        Boolean.FALSE);
    MockHttpServletRequestBuilder requestBuilder =
        get(QRV2ApiPath.BASE_PATH + QRV2ApiPath.ACCESSIBLE).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession());
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", equalTo(false)));
    verify(this.qrv2Service).qrGenerationAccessible(Constants.BUSINESS_PARTNER_CODE);
    verify(this.mandatoryParameterHelper).getRequestId();
    verify(this.mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(qrv2Service);
    verifyNoMoreInteractions(mandatoryParameterHelper);
  }

  @Test
  public void downloadQRCodeTest() throws Exception {
    List<ProductDetailsRequest> productDetailsRequestList = new ArrayList<>();
    productDetailsRequestList.add(productDetailsRequest);
    this.manualQRCodeRequest.setProductDetailsRequestList(productDetailsRequestList);
    Mockito.doNothing().when(qrv2Service)
        .downloadQRCodes(Constants.STORE_ID, Constants.REQUEST_ID, Constants.BUSINESS_PARTNER_CODE,
            BUSINESS_PARTNER_NAME, manualQRCodeRequest);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
        Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(mandatoryParameterHelper.getBusinessPartnerName()).thenReturn(BUSINESS_PARTNER_NAME);

    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(QRV2ApiPath.BASE_PATH + QRV2ApiPath.DOWNLOAD_QR_CODES)
            .content(toJson(manualQRCodeRequest)).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

    verify(qrv2Service).downloadQRCodes(Constants.STORE_ID, Constants.REQUEST_ID,
        Constants.BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, manualQRCodeRequest);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getBusinessPartnerName();
  }

  @Test
  public void uploadExcelToGenerateQrTest() throws Exception {
    multipartFile = new MockMultipartFile("multipartFile", generateDummyExcelMultipartFile().getBytes());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(
      Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(mandatoryParameterHelper.getBusinessPartnerName()).thenReturn(BUSINESS_PARTNER_NAME);
    this.mockMvc.perform(
      MockMvcRequestBuilders.multipart(QRV2ApiPath.BASE_PATH + QRV2ApiPath.UPLOAD_FOR_QR)
        .file(multipartFile)
        .file(new MockMultipartFile("manualQRCodeRequest", "",
          MediaType.APPLICATION_JSON_VALUE, toJson(manualQRCodeRequest).getBytes()))
        .accept(MediaType.APPLICATION_JSON)).andExpect(status().isOk());

    verify(qrv2Service).uploadExcelAndPublishRequest(Constants.STORE_ID, Constants.REQUEST_ID,
      Constants.BUSINESS_PARTNER_CODE, BUSINESS_PARTNER_NAME, manualQRCodeRequest, multipartFile);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mandatoryParameterHelper).getStoreId();
    verify(mandatoryParameterHelper).getUsername();
    verify(mandatoryParameterHelper).getBusinessPartnerName();
  }
}
