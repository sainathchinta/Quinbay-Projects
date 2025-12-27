package com.gdn.partners.pcu.internal.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import javax.imageio.ImageIO;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.BrandApiPath;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.service.BrandService;
import com.gdn.partners.pcu.internal.web.TestApplication;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.model.request.BrandDeleteWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.UpdateBrandWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BrandLogo;
import com.gdn.partners.pcu.internal.web.model.response.BrandWebResponse;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandRequest;


@AutoConfigureMockMvc
@ExtendWith(MockitoExtension.class)
public class BrandControllerTest extends TestHelper {

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Autowired
  public void setMockMvc(MockMvc mockMvc) {
    this.mockMvc = mockMvc;
    super.setMockMvc(mockMvc);
  }

  @Mock
  private BrandService brandService;

  @InjectMocks
  private BrandController brandController;

  @Captor
  private ArgumentCaptor<UpdateBrandRequest> updateBrandRequestArgumentCaptor;

  private static final String BRAND_CODE = "brandCode";
  private static final String BRAND_NAME = "brandName";
  private static final String BRAND_DELETE_REASON = "brandDeleteReason";
  private static final String BRAND_DESCRIPTION = "brandDescription";
  private static final String DEFAULT_BRAND_LOGO_PATH = "blibli-com-logo.jpg";
  private int height;
  private int width;
  private static final byte[] DEFAULT_BRAND_LOGO_DATA = {1,2,3,4,5,6,7,8,9};
  private BrandDeleteWebRequest brandDeleteWebRequest;
  private UpdateBrandWebRequest updateBrandWebRequest;
  private File file;
  private BufferedImage img;
  private static ClassLoader classLoader;
  private static MockMultipartFile multipartFile;

  @BeforeEach
  public void setUp() {
    mockMvc = MockMvcBuilders.standaloneSetup(brandController).build();
    brandDeleteWebRequest = BrandDeleteWebRequest.builder().brandName(BRAND_NAME)
        .brandDeletedReason(BRAND_DELETE_REASON).build();
    updateBrandWebRequest =
        UpdateBrandWebRequest.builder().brandCode(BRAND_CODE).brandName(BRAND_NAME).brandDescription(BRAND_DESCRIPTION)
            .build();
  }

  @Test
  public void deleteBrandTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        delete(BrandApiPath.BASE_PATH + BrandApiPath.DELETE, BRAND_CODE)
            .content(toJson(brandDeleteWebRequest))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(brandService).deleteBrand(BRAND_CODE, brandDeleteWebRequest);
  }

  @Test
  public void updateBrandTest() throws Exception {
    updateBrandWebRequest.setValidBrand(true);
    updateBrandWebRequest.setProtectedBrand(true);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    multipartFile = generateMultipartFile();
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(BrandApiPath.BASE_PATH + BrandApiPath.UPDATE).file(
        new MockMultipartFile("requestBody", "", MediaType.APPLICATION_JSON_VALUE,
            toJson(updateBrandWebRequest).getBytes())).file(multipartFile).accept(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
    verify(clientParameterHelper).getRequestId();
    Mockito.verify(brandService)
        .updateBrand(updateBrandRequestArgumentCaptor.capture(), Mockito.eq(multipartFile), Mockito.eq(null));
    Assertions.assertTrue(updateBrandRequestArgumentCaptor.getValue().isValidBrand());
    Assertions.assertTrue(updateBrandRequestArgumentCaptor.getValue().isProtectedBrand());
    deleteFile("target/test-classes/Brand");
  }

  private MockMultipartFile generateMultipartFile() throws Exception {
    classLoader = this.getClass().getClassLoader();
    mockFile("target/test-classes/Brand/" + DEFAULT_BRAND_LOGO_PATH);
    file = new File(classLoader.getResource("Brand/" + BrandControllerTest.DEFAULT_BRAND_LOGO_PATH).getFile());
    multipartFile = new MockMultipartFile("brandLogo", BrandControllerTest.DEFAULT_BRAND_LOGO_PATH, "image/jpg",
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

  public void deleteFile(String destination) throws Exception {
    FileUtils.deleteDirectory(new File(destination));
  }

  @Test
  public void filterBrandLogoByBrandCodeTest() throws Exception {
    Mockito.when(this.brandService.filterBrandLogoByBrandCode(BRAND_CODE, true))
        .thenReturn(new BrandLogo(DEFAULT_BRAND_LOGO_DATA, null, null));
    MockHttpServletRequestBuilder requestBuilder =
        get(BrandApiPath.BASE_PATH + BrandApiPath.IMAGE)
            .contentType(MediaType.APPLICATION_JSON)
            .param("brandCode", BRAND_CODE)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    Mockito.verify(this.brandService).filterBrandLogoByBrandCode(BRAND_CODE, true);
  }

  @Test
  public void filterBrandLogoByBrandRequestCodeTest() throws Exception {
    Mockito.when(this.brandService.filterBrandLogoByBrandRequestCode(BRAND_CODE, true))
        .thenReturn(new BrandLogo(DEFAULT_BRAND_LOGO_DATA, null, null));
    MockHttpServletRequestBuilder requestBuilder =
        get(BrandApiPath.BASE_PATH + BrandApiPath.IMAGE)
            .contentType(MediaType.APPLICATION_JSON)
            .param("brandRequestCode", BRAND_CODE)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    Mockito.verify(this.brandService).filterBrandLogoByBrandRequestCode(BRAND_CODE, true);
  }

  @Test
  public void getBrandDetailTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(brandService.getBrandDetail(BRAND_CODE)).thenReturn(new BrandWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(BrandApiPath.BASE_PATH + BrandApiPath.DETAIL, BRAND_CODE)
            .contentType(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(brandService).getBrandDetail(BRAND_CODE);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(brandService, clientParameterHelper);
  }
}