package com.gdn.partners.pcu.external.web.controller;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.model.BrandWipApiPath;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.service.impl.BrandWipServiceBean;
import com.gdn.partners.pcu.external.service.impl.exception.DuplicateEntryException;
import com.gdn.partners.pcu.external.service.model.request.UploadImageRequest;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipResponse;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import static org.hamcrest.CoreMatchers.nullValue;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

public class BrandWipControllerTest extends TestHelper {

  private static final String DEFAULT_BRAND_NAME = "Blibli.com";
  private static final String DEFAULT_BRAND_REQUEST_CODE = "BR-0001-0001";
  private static final String DEFAULT_BRAND_DESCRIPTION_STRING = "Description";
  private static final String BRAND_EXISTS_ERROR_MESSAGE = "Brand already exists";
  private static final String INTERNAL = "INTERNAL";
  private static final byte[]  DEFAULT_BRAND_DESCRIPTION = DEFAULT_BRAND_DESCRIPTION_STRING.getBytes();
  private static final String DEFAULT_BRAND_LOGO_PATH = "blibli-com-logo.jpg";
  private static MockMultipartFile multipartFile;
  private static ClassLoader classLoader;
  private static File file;
  private static int height;
  private static int width;
  private static BufferedImage img;

  CreateBrandWipRequest request = new CreateBrandWipRequest();
  CreateBrandWipResponse response = new CreateBrandWipResponse();
  GdnRestSingleResponse<CreateBrandWipResponse> restSingleResponse = new GdnRestSingleResponse<>();
  BrandResponse brandResponse = new BrandResponse();
  private UploadImageRequest uploadImageRequest;


  @Mock
  private BrandWipServiceBean brandWipServiceBean;

  @InjectMocks
  private BrandWipController brandWipController;


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    mockMvc = standaloneSetup(this.brandWipController).build();
    request.setBrandName(DEFAULT_BRAND_NAME);
    request.setBrandDescription(DEFAULT_BRAND_DESCRIPTION_STRING);
    response.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    restSingleResponse.setValue(response);
    uploadImageRequest = new UploadImageRequest();
    uploadImageRequest.setImageFileName("Brand/" + DEFAULT_BRAND_LOGO_PATH);
    brandResponse.setBrandName(DEFAULT_BRAND_NAME);
  }

  @AfterEach
  public void teardown() throws Exception {
    verifyNoMoreInteractions(this.brandWipServiceBean);
    deleteFolder("target/test-classes/Brand/");
    deleteFolder("Brand");
    deleteFolder("null");
  }

  private void deleteFolder(String folderPath) throws IOException {
    FileUtils.deleteDirectory(new File(folderPath));
  }


  @Test
  public void createTest() throws Exception {
    Mockito.when(this.brandWipServiceBean.create(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(restSingleResponse);
    multipartFile = this.generateMultipartFile();
    this.mockMvc
        .perform(MockMvcRequestBuilders.multipart(BrandWipApiPath.BASE_PATH + BrandWipApiPath.CREATE)
            .file(new MockMultipartFile("request", "",
                MediaType.APPLICATION_JSON_VALUE, toJson(request).getBytes()))
            .file(multipartFile)
            .accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession()))
        .andExpect(status().isOk());
    Mockito.verify(this.brandWipServiceBean).create(Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void createTest_expectDuplicateEntryException() throws Exception {
    Mockito.when(this.brandWipServiceBean.create(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenThrow(new DuplicateEntryException(BRAND_EXISTS_ERROR_MESSAGE));
    this.mockMvc
        .perform(MockMvcRequestBuilders.multipart(BrandWipApiPath.BASE_PATH + BrandWipApiPath.CREATE)
            .file(new MockMultipartFile("request", "",
                MediaType.APPLICATION_JSON_VALUE, toJson(request).getBytes()))
            .accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession()))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", CoreMatchers.equalTo(BRAND_EXISTS_ERROR_MESSAGE)));
    Mockito.verify(this.brandWipServiceBean).create(Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void createTest_expectException() throws Exception {
    Mockito.when(this.brandWipServiceBean.create(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenThrow(IOException.class);
    this.mockMvc
        .perform(MockMvcRequestBuilders.multipart(BrandWipApiPath.BASE_PATH + BrandWipApiPath.CREATE)
            .file(new MockMultipartFile("request", "",
                MediaType.APPLICATION_JSON_VALUE, toJson(request).getBytes()))
            .accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession()))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", nullValue()));
    Mockito.verify(this.brandWipServiceBean).create(Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  public void createTest_emptyBrandName() throws Exception {
    request.setBrandName(null);
    Mockito.when(this.brandWipServiceBean.create(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(restSingleResponse);
    multipartFile = this.generateMultipartFile();
    this.mockMvc
        .perform(MockMvcRequestBuilders.multipart(BrandWipApiPath.BASE_PATH + BrandWipApiPath.CREATE)
            .file(new MockMultipartFile("request", "",
                MediaType.APPLICATION_JSON_VALUE, toJson(request).getBytes()))
            .file(multipartFile)
            .accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession()))
        .andExpect(status().isOk());
    Mockito.verify(this.brandWipServiceBean).create(Mockito.any(), Mockito.any(), Mockito.any());
  }

  private MockMultipartFile generateMultipartFile() throws Exception {
    classLoader = this.getClass().getClassLoader();
    mockFile("target/test-classes/Brand/" + DEFAULT_BRAND_LOGO_PATH);
    file = new File(classLoader.getResource("Brand/" + BrandWipControllerTest.DEFAULT_BRAND_LOGO_PATH).getFile());
    multipartFile = new MockMultipartFile("brandLogo",
        BrandWipControllerTest.DEFAULT_BRAND_LOGO_PATH, "image/jpg", IOUtils.toByteArray(new FileInputStream(file)));
    return multipartFile;
  }

  private void mockFile(String filePath) throws IOException {
    file = new File(filePath);
    file.mkdirs();
    width = 640;
    height = 320;
    img = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
    ImageIO.write(img, "jpg", file);
    uploadImageRequest.setBytes(FileUtils.readFileToByteArray(file));
  }
}