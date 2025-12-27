package com.gdn.partners.pcu.internal.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.imageio.ImageIO;

import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.BrandWipApiPath;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.service.BrandWipService;
import com.gdn.partners.pcu.internal.web.TestApplication;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.model.request.ApproveBrandWipWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BrandApprovalResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandRejectionWebResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistoryResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistorySummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipSummaryRequest;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;


@AutoConfigureMockMvc
@ExtendWith(MockitoExtension.class)
public class BrandWipControllerTest extends TestHelper {

  private static final String DEFAULT_BRAND_REQUEST_CODE = "BR-0001-0001";
  private static final String DEFAULT_BRAND_NAME = "BliBli";
  private static final String DEFAULT_DESCRIPTION = "description blibli";
  private static final String DRAFT = "DRAFT";
  private static final String ID = "id";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final int TOTAL_RECORDS = 1;
  private static final String  DEFAULT_BRAND_DESCRIPTION_STRING = "Description";
  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private static final String INTERNAL = "INTERNAL";
  private static final byte[] DEFAULT_BRAND_DESCRIPTION = DEFAULT_BRAND_DESCRIPTION_STRING.getBytes();
  private static final String DEFAULT_BRAND_LOGO_PATH = "blibli-com-logo.jpg";

  private static MockMultipartFile multipartFile;
  private static ClassLoader classLoader;
  private static File file;
  private static int height;
  private static int width;
  private static BufferedImage img;

  private ApproveBrandWipWebRequest request;
  private List<BrandWipResponse> brandWipResponseList = new ArrayList<>();
  private GdnRestListResponse<BrandWipResponse> brandWipResponseGdnRestListResponse = new GdnRestListResponse<>();
  private BrandWipSummaryRequest brandWipSummaryRequest = new BrandWipSummaryRequest();
  private BrandWipResponse brandWipResponse = new BrandWipResponse();
  private GdnRestSingleResponse<BrandWipResponse> response = new GdnRestSingleResponse<>();
  private GdnRestListResponse<BrandWipHistoryResponse> historyResponse;
  private List<BrandWipHistoryResponse> historyList = new ArrayList<>();
  private BrandWipHistoryResponse brandWipHistoryResponse = new BrandWipHistoryResponse();
  private BrandWipHistorySummaryRequest brandWipHistorySummaryRequest = new BrandWipHistorySummaryRequest();
  private BrandRejectionWebResponse brandRejectionWebResponse;

  @Mock
  private BrandWipService brandWipService;

  @InjectMocks
  private BrandWipController brandWipController;

  @Mock
  private ClientParameterHelper clientParameterHelper;


  @BeforeEach
  public void init() {
    mockMvc = MockMvcBuilders.standaloneSetup(this.brandWipController).build();
    brandWipResponse.setBrandName(DEFAULT_BRAND_NAME);
    brandWipResponse.setBrandDescription(DEFAULT_DESCRIPTION);
    response.setValue(brandWipResponse);
    response.setSuccess(true);
    brandWipSummaryRequest.setBrandName(DEFAULT_BRAND_NAME);
    brandWipSummaryRequest.setState(DRAFT);
    brandWipResponseList.add(brandWipResponse);
    brandWipResponseGdnRestListResponse.setContent(brandWipResponseList);
    brandWipResponseGdnRestListResponse.setSuccess(true);
    brandWipHistoryResponse.setState(BrandWipState.DRAFT.getDescription());
    brandWipHistoryResponse.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWipHistoryResponse.setDescription(DEFAULT_DESCRIPTION);
    historyList.add(brandWipHistoryResponse);
    historyResponse = new GdnRestListResponse<>(historyList, new PageMetaData(SIZE, PAGE, TOTAL_RECORDS), null);
    historyResponse.setSuccess(true);
    brandWipHistorySummaryRequest.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);

    request = new ApproveBrandWipWebRequest();
    request.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);

    brandWipResponse.setBrandName(DEFAULT_BRAND_NAME);
    brandWipResponse.setBrandDescription(DEFAULT_DESCRIPTION);
    response.setValue(brandWipResponse);
    response.setSuccess(true);
    brandWipSummaryRequest.setBrandName(DEFAULT_BRAND_NAME);
    brandWipSummaryRequest.setState(DRAFT);
    brandWipResponseList.add(brandWipResponse);
    brandWipResponseGdnRestListResponse.setContent(brandWipResponseList);
    brandWipResponseGdnRestListResponse.setSuccess(true);
    brandRejectionWebResponse =
        BrandRejectionWebResponse.builder().id(ID).rejectionReason(DEFAULT_BRAND_DESCRIPTION_STRING)
            .brandRequestCode(DEFAULT_BRAND_REQUEST_CODE).brandName(DEFAULT_BRAND_NAME).build();
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.brandWipService, clientParameterHelper);
  }

  @Test
  public void getBrandWipDetail() throws Exception {
    Mockito.when(this.brandWipService.getBrandWipDetail(DEFAULT_BRAND_REQUEST_CODE)).thenReturn(response);
    MockHttpServletRequestBuilder requestBuilder = get(BrandWipApiPath.BASE_PATH + BrandWipApiPath.DETAIL, DEFAULT_BRAND_REQUEST_CODE)
        .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandWipService).getBrandWipDetail(DEFAULT_BRAND_REQUEST_CODE);
  }

  @Test
  public void getBrandRejectionReason() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(this.brandWipService.getBrandRejectionReasonByBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE))
        .thenReturn(brandRejectionWebResponse);
    MockHttpServletRequestBuilder requestBuilder =
        get(BrandWipApiPath.BASE_PATH + BrandWipApiPath.REJECTION_REASON, DEFAULT_BRAND_REQUEST_CODE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandWipService).getBrandRejectionReasonByBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    Mockito.verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void getBrandWipListTest() throws Exception {
    Mockito.when(this.brandWipService.getBrandWipList(brandWipSummaryRequest, PAGE, SIZE))
        .thenReturn(brandWipResponseGdnRestListResponse);
    String requestString = BrandWipControllerTest.OBJECT_MAPPER.writeValueAsString(brandWipSummaryRequest);
    this.mockMvc.perform(
        post(BrandWipApiPath.BASE_PATH + BrandWipApiPath.FILTER_SUMMARY).contentType(MediaType.APPLICATION_JSON)
            .content(requestString)).andExpect(status().isOk());
    Mockito.verify(this.brandWipService).getBrandWipList(brandWipSummaryRequest, PAGE, SIZE);
  }

  @Test
  public void getBrandWipHistoryTest() throws Exception {
    Mockito.when(this.brandWipService.getBrandWipHistory(brandWipHistorySummaryRequest, PAGE, SIZE))
        .thenReturn(historyResponse);
    this.mockMvc.perform(
        post(BrandWipApiPath.BASE_PATH + BrandWipApiPath.HISTORY_SUMMARY).contentType(MediaType.APPLICATION_JSON)
            .content(toJson(brandWipHistorySummaryRequest)).param("page", toJson(PAGE)).param("size", toJson(SIZE)))
        .andExpect(status().isOk());
    Mockito.verify(this.brandWipService).getBrandWipHistory(brandWipHistorySummaryRequest, PAGE, SIZE);
  }

  @Test
  public void approveBrandTest() throws Exception {
    BrandApprovalResponse brandApprovalResponse =
        new BrandApprovalResponse(DEFAULT_BRAND_REQUEST_CODE, DEFAULT_BRAND_NAME);
    Mockito.when(brandWipService.approveBrand(Mockito.eq(request), Mockito.any(), Mockito.any()))
        .thenReturn(brandApprovalResponse);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    multipartFile = this.generateMultipartFile();
    this.mockMvc.perform(MockMvcRequestBuilders
        .multipart(BrandWipApiPath.BASE_PATH + BrandWipApiPath.APPROVE, DEFAULT_BRAND_REQUEST_CODE)
        .file(new MockMultipartFile("request", "", MediaType.APPLICATION_JSON_VALUE, toJson(request).getBytes()))
        .file(multipartFile).accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession()))
        .andExpect(status().isOk());
    Mockito.verify(brandWipService).approveBrand(Mockito.eq(request), Mockito.any(), Mockito.any());
    Mockito.verify(clientParameterHelper).getRequestId();  }

  @Test
  public void rejectBrandTest() throws Exception {
    Mockito.when(brandWipService.rejectBrand(new BrandRejectRequest())).thenReturn(DEFAULT_BRAND_REQUEST_CODE);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(BrandWipApiPath.BASE_PATH + BrandWipApiPath.REJECT, DEFAULT_BRAND_REQUEST_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(toJson(new BrandRejectRequest()));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(brandWipService).rejectBrand(new BrandRejectRequest());
    Mockito.verify(clientParameterHelper).getRequestId();  }

  @Test
  public void updateTest() throws Exception {
    Mockito.when(brandWipService.update(Mockito.eq(request), Mockito.any(), Mockito.any()))
        .thenReturn(new GdnBaseRestResponse(true));
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    multipartFile = this.generateMultipartFile();
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(BrandWipApiPath.BASE_PATH + BrandWipApiPath.UPDATE)
        .file(new MockMultipartFile("request", "", MediaType.APPLICATION_JSON_VALUE, toJson(request).getBytes()))
        .file(multipartFile).accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession()))
        .andExpect(status().isOk());
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(brandWipService).update(Mockito.eq(request), Mockito.any(), Mockito.any());
  }

  private MockMultipartFile generateMultipartFile() throws Exception {
    classLoader = this.getClass().getClassLoader();
    mockFile("target/test-classes/Brand/" + DEFAULT_BRAND_LOGO_PATH);
    file = new File(classLoader.getResource("Brand/" + BrandWipControllerTest.DEFAULT_BRAND_LOGO_PATH).getFile());
    multipartFile = new MockMultipartFile("brandLogo", BrandWipControllerTest.DEFAULT_BRAND_LOGO_PATH, "image/jpg",
        IOUtils.toByteArray(new FileInputStream(file)));
    return multipartFile;
  }

  private void mockFile(String filePath) throws IOException {
    file = new File(filePath);
    file.mkdirs();
    width = 640;
    height = 320;
    img = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
    ImageIO.write(img, "jpg", file);
  }
}